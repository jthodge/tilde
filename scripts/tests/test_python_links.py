"""Exercise the Python link helper in a temporary home, without real uv."""
import os
from pathlib import Path
import subprocess
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "bin/.local/bin/uv-python-simlink"


class PythonLinksTest(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.home = Path(self.temp.name).resolve()
        self.bin = self.home / ".local/bin"
        self.bin.mkdir(parents=True)
        self.managed = self.home / "managed"
        self.python = self.managed / "cpython-3.13.2-macos-aarch64-none/bin/python3.13"
        self.python.parent.mkdir(parents=True)
        self.python.write_text("#!/bin/sh\nexit 0\n")
        self.python.chmod(0o755)
        uv = self.bin / "uv"
        uv.write_text('#!/bin/sh\ncase "$*" in\n"python dir") printf "%s\\n" "$MANAGED";;\n"python find --managed-python") printf "%s\\n" "$PYTHON";;\n*) exit 1;;\nesac\n')
        uv.chmod(0o755)
        self.env = dict(os.environ, HOME=str(self.home),
                        PATH=str(self.bin) + ":/usr/bin:/bin",
                        MANAGED=str(self.managed), PYTHON=str(self.python))

    def run_script(self):
        return subprocess.run(["/bin/bash", str(SCRIPT)], env=self.env,
                              text=True, capture_output=True)

    def test_create_and_repeat_without_changes(self):
        first = self.run_script()
        self.assertEqual(first.returncode, 0, first.stderr)
        target = self.bin / "python"
        self.assertEqual(target.resolve(), self.python)
        before = target.lstat()
        second = self.run_script()
        self.assertEqual(second.returncode, 0, second.stderr)
        self.assertEqual(second.stdout, "")
        self.assertEqual(before.st_ino, target.lstat().st_ino)

    def test_refuse_real_file_before_any_changes(self):
        target = self.bin / "python"
        target.write_text("keep me")
        self.assertNotEqual(self.run_script().returncode, 0)
        self.assertEqual(target.read_text(), "keep me")
        self.assertFalse((self.bin / "python3.13").exists())

    def test_refuse_directory(self):
        target = self.bin / "python"
        target.mkdir()
        (target / "keep").write_text("data")
        self.assertNotEqual(self.run_script().returncode, 0)
        self.assertEqual((target / "keep").read_text(), "data")

    def test_refuse_foreign_link(self):
        target = self.bin / "python"
        target.symlink_to("/some/other/python")
        self.assertNotEqual(self.run_script().returncode, 0)
        self.assertEqual(os.readlink(target), "/some/other/python")

    def test_repair_owned_broken_link(self):
        target = self.bin / "python"
        target.symlink_to(self.managed / "old/bin/python")
        result = self.run_script()
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(target.resolve(), self.python)

    def test_refuse_empty_installation(self):
        self.python.unlink()
        self.assertNotEqual(self.run_script().returncode, 0)
        self.assertFalse((self.bin / "python").exists())

    def test_refuse_failed_uv(self):
        (self.bin / "uv").write_text("#!/bin/sh\nexit 2\n")
        self.assertNotEqual(self.run_script().returncode, 0)
        self.assertFalse((self.bin / "python").exists())

    def test_refuse_unmanaged_default(self):
        self.env["PYTHON"] = "/usr/bin/python3"
        self.assertNotEqual(self.run_script().returncode, 0)
        self.assertFalse((self.bin / "python3.13").exists())


if __name__ == "__main__":
    unittest.main()
