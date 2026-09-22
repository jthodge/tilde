"""Exercise legacy Make targets with empty and nonempty Stow manifests."""
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest

REPO = Path(__file__).resolve().parents[2]


class MakeDeploymentTest(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        self.home = self.root / "home"
        self.home.mkdir()
        self.bin = self.root / "bin"
        self.bin.mkdir()
        (self.root / "scripts").mkdir()
        (self.root / "claude/.claude").mkdir(parents=True)
        (self.root / "claude/.claude/settings.json").write_text('{"model":"template"}\n')
        shutil.copyfile(REPO / "Makefile", self.root / "Makefile")
        shutil.copyfile(REPO / "scripts/seed-configs", self.root / "scripts/seed-configs")
        stow = self.bin / "stow"
        stow.write_text('#!/bin/sh\nprintf "%s\\n" "$*" >> "$STOW_LOG"\nexit "$STOW_STATUS"\n')
        stow.chmod(0o755)
        self.log = self.root / "stow.log"
        self.env = dict(os.environ, HOME=str(self.home),
                        PATH=str(self.bin) + os.pathsep + os.environ["PATH"],
                        STOW_LOG=str(self.log), STOW_STATUS="0")
        self.env.pop("MAKEFLAGS", None)
        self.env.pop("MFLAGS", None)

    def make(self, target):
        return subprocess.run(["make", "--no-print-directory", target], cwd=self.root,
                              env=self.env, text=True, capture_output=True)

    def test_empty_manifest_skips_stow_but_preserves_seed_workflow(self):
        for contents in ("", "# All packages have transferred.\n"):
            with self.subTest(contents=contents):
                (self.root / ".stow-packages").write_text(contents)
                self.env["STOW_STATUS"] = "99"
                target = self.home / ".claude/settings.json"
                if target.exists():
                    target.unlink()
                result = self.make("dry-run")
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertFalse(target.exists())
                result = self.make("switch")
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertTrue(target.is_file())
                self.assertFalse(target.is_symlink())
                self.assertEqual(target.stat().st_mode & 0o777, 0o600)
                target.write_text('{"model":"local"}\n')
                for action in ("switch", "unstow"):
                    result = self.make(action)
                    self.assertEqual(result.returncode, 0, result.stderr)
                    self.assertEqual(target.read_text(), '{"model":"local"}\n')
                self.assertFalse(self.log.exists())

    def test_nonempty_manifest_keeps_scoped_stow_commands(self):
        (self.root / ".stow-packages").write_text("# remaining\nfish\n")
        for action, flag in (("dry-run", "--simulate"), ("switch", "--restow"),
                             ("unstow", "--delete")):
            with self.subTest(action=action):
                result = self.make(action)
                self.assertEqual(result.returncode, 0, result.stderr)
                command = self.log.read_text().splitlines()[-1]
                self.assertIn(flag, command)
                self.assertTrue(command.endswith(" fish"))

    def test_stow_failure_stops_before_seeding(self):
        (self.root / ".stow-packages").write_text("fish\n")
        self.env["STOW_STATUS"] = "7"
        result = self.make("switch")
        self.assertNotEqual(result.returncode, 0)
        self.assertFalse((self.home / ".claude/settings.json").exists())


if __name__ == "__main__":
    unittest.main()
