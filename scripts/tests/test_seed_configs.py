"""Prove seed-only deployment preserves local files and legacy link contents."""
import importlib.machinery
import importlib.util
import json
import os
from pathlib import Path
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[2]
loader = importlib.machinery.SourceFileLoader("seed_configs", str(ROOT / "scripts/seed-configs"))
spec = importlib.util.spec_from_loader(loader.name, loader)
assert spec is not None
module = importlib.util.module_from_spec(spec)
loader.exec_module(module)


class SeedTest(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        self.source = self.root / "template.json"
        self.source.write_text('{"model":"default"}\n')
        self.target = self.root / "home/.claude/settings.json"
        self.state = self.root / "state"

    def seed(self, apply=True):
        return module.seed(self.source, self.target, self.state, apply)

    def test_missing_file_is_private_and_repeat_is_noop(self):
        self.assertEqual(self.seed(), "seed")
        self.assertEqual(self.target.read_bytes(), self.source.read_bytes())
        self.assertEqual(self.target.stat().st_mode & 0o777, 0o600)
        before = self.target.stat()
        self.assertEqual(self.seed(), "preserve")
        self.assertEqual(before, self.target.stat())

    def test_dry_run_does_not_create_directories(self):
        self.assertEqual(self.seed(False), "seed")
        self.assertFalse(self.target.parent.exists())
        self.assertFalse(self.state.exists())

    def test_existing_local_bytes_and_permissions_are_preserved(self):
        self.target.parent.mkdir(parents=True)
        self.target.write_text('{"local":"keep"}\n')
        self.target.chmod(0o400)
        before = self.target.stat()
        self.seed()
        self.assertEqual(self.target.read_text(), '{"local":"keep"}\n')
        self.assertEqual(before, self.target.stat())

    def test_detach_link_and_preserve_source_with_backup(self):
        self.target.parent.mkdir(parents=True)
        self.target.symlink_to(self.source)
        original = self.source.read_bytes()
        self.assertEqual(self.seed(), "detach")
        self.assertFalse(self.target.is_symlink())
        self.assertEqual(self.target.read_bytes(), original)
        self.assertEqual(self.source.read_bytes(), original)
        backups = list((self.state / "backups").glob("*/settings.json"))
        self.assertEqual(len(backups), 1)
        self.assertEqual(backups[0].read_bytes(), original)
        self.assertEqual(backups[0].stat().st_mode & 0o777, 0o600)
        self.assertEqual(backups[0].parent.stat().st_mode & 0o777, 0o700)

    def test_broken_link_is_preserved(self):
        self.target.parent.mkdir(parents=True)
        self.target.symlink_to(self.root / "absent")
        with self.assertRaises(ValueError):
            self.seed()
        self.assertTrue(self.target.is_symlink())

    def test_directory_is_preserved(self):
        self.target.mkdir(parents=True)
        with self.assertRaises(ValueError):
            self.seed()
        self.assertTrue(self.target.is_dir())

    def test_invalid_template_is_not_installed(self):
        self.source.write_text("invalid JSON")
        with self.assertRaises(ValueError):
            self.seed()
        self.assertFalse(self.target.exists())

    def test_stow_template_is_excluded(self):
        ignore = (ROOT / "claude/.stow-local-ignore").read_text()
        self.assertIn(r"^/\.claude/settings\.json$", ignore)


if __name__ == "__main__":
    unittest.main()
