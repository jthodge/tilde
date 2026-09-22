"""Test same-target link adoption without Nix or private user state."""
import os
from pathlib import Path
import runpy
import tempfile
import unittest
from unittest.mock import patch

API = runpy.run_path(str(Path(__file__).resolve().parents[1] / "adopt-home-links"))
plan = API["plan"]
apply = API["apply"]


class AdoptHomeLinksTest(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name).resolve()
        self.home = self.root / "home"
        self.source = self.root / "checkout"
        self.generation = self.root / "generation"
        for path in (self.home, self.source, self.generation / "home-files"):
            path.mkdir(parents=True)

    def link(self, name="config", directory=False):
        source = self.source / name
        source.parent.mkdir(parents=True, exist_ok=True)
        if directory:
            source.mkdir()
        else:
            source.write_text("unchanged\n")
        target = self.home / name
        target.parent.mkdir(parents=True, exist_ok=True)
        target.symlink_to(source)
        candidate = self.generation / "home-files" / name
        candidate.parent.mkdir(parents=True, exist_ok=True)
        candidate.symlink_to(source)
        return source, target, candidate

    def test_preview_changes_nothing(self):
        source, target, candidate = self.link()
        before = os.readlink(target)
        records = plan(self.home, self.generation, ["config"])
        self.assertEqual(os.readlink(target), before)
        self.assertEqual(records[0]["after"], str(candidate))
        self.assertEqual(sorted(p.name for p in self.home.iterdir()), ["config"])

    def test_directory_adoption_preserves_mutable_state_and_identity(self):
        source, target, candidate = self.link(directory=True)
        source.chmod(0o700)
        private = source / "local-state"
        private.write_text("old fixture\n")
        records = plan(self.home, self.generation, ["config"])
        # Model a running application replacing its state after the preview.
        replacement = source / "next-state"
        replacement.write_text("private fixture\n")
        replacement.chmod(0o600)
        os.replace(replacement, private)
        before = (source.stat().st_ino, private.stat().st_ino)
        apply(records)
        self.assertEqual(os.readlink(target), str(candidate))
        self.assertEqual((target.stat().st_ino, (target / "local-state").stat().st_ino), before)
        self.assertEqual(private.read_text(), "private fixture\n")
        self.assertEqual(private.stat().st_mode & 0o777, 0o600)
        self.assertEqual(source.stat().st_mode & 0o777, 0o700)

    def test_file_adoption_preserves_executable_source(self):
        source, target, candidate = self.link()
        source.chmod(0o755)
        apply(plan(self.home, self.generation, ["config"]))
        self.assertEqual(os.readlink(target), str(candidate))
        self.assertTrue(os.access(target, os.X_OK))
        self.assertEqual(source.read_text(), "unchanged\n")

    def test_identical_regular_file_is_refused(self):
        source, target, candidate = self.link()
        target.unlink()
        target.write_bytes(source.read_bytes())
        with self.assertRaises(ValueError):
            plan(self.home, self.generation, ["config"])
        self.assertFalse(target.is_symlink())

    def test_different_source_is_refused_even_with_identical_bytes(self):
        source, target, candidate = self.link()
        other = self.root / "other"
        other.write_bytes(source.read_bytes())
        candidate.unlink()
        candidate.symlink_to(other)
        with self.assertRaises(ValueError):
            plan(self.home, self.generation, ["config"])

    def test_symlinked_parent_is_refused(self):
        source, target, candidate = self.link(directory=True)
        (source / "child").write_text("unchanged\n")
        with self.assertRaises(ValueError):
            plan(self.home, self.generation, ["config/child"])

    def test_invalid_or_overlapping_targets_are_refused(self):
        for names in ([], ["config", "config"], ["../config"], ["/config"],
                      ["."], ["config", "config/child"]):
            with self.subTest(names=names), self.assertRaises(ValueError):
                plan(self.home, self.generation, names)

    def test_all_targets_are_checked_before_writing(self):
        source, target, candidate = self.link()
        before = os.readlink(target)
        with self.assertRaises(ValueError):
            plan(self.home, self.generation, ["config", "missing"])
        self.assertEqual(os.readlink(target), before)

    def test_changed_target_after_plan_is_refused(self):
        source, target, candidate = self.link()
        records = plan(self.home, self.generation, ["config"])
        target.unlink()
        target.write_text("new local state\n")
        with self.assertRaises(ValueError):
            apply(records)
        self.assertEqual(target.read_text(), "new local state\n")

    def test_replace_failure_preserves_link_and_cleans_temporary_directory(self):
        source, target, candidate = self.link()
        before = os.readlink(target)
        with patch("os.replace", side_effect=OSError("fixture failure")):
            with self.assertRaises(OSError):
                apply(plan(self.home, self.generation, ["config"]))
        self.assertEqual(os.readlink(target), before)
        self.assertEqual(sorted(p.name for p in self.home.iterdir()), ["config"])


if __name__ == "__main__":
    unittest.main()
