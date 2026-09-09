"""Run the documented ELPA snapshot/restore blocks against a synthetic HOME.

The blocks in docs/upgrades-and-recovery.md are the authority. This
suite extracts them by stable HTML comment markers and executes them
verbatim in a temporary HOME. It never installs, upgrades, or fetches
any package, and it never touches the real `~/.emacs.d/`.
"""
import os
from pathlib import Path
import re
import stat
import subprocess
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[2]
DOC = ROOT / "docs/upgrades-and-recovery.md"
BLOCK_RE = re.compile(
    r"<!--\s*(?P<name>[a-z0-9-]+):begin\s*-->\s*```sh\n(?P<body>.*?)```\s*<!--\s*(?P=name):end\s*-->",
    re.DOTALL,
)


def _extract_blocks():
    text = DOC.read_text()
    return {match.group("name"): match.group("body") for match in BLOCK_RE.finditer(text)}


class UpgradesRecoveryDocTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.blocks = _extract_blocks()
        for name in ("snapshot-elpa", "restore-elpa"):
            if name not in cls.blocks:
                raise AssertionError("missing documented block: " + name)

    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory(prefix="tilde-recovery-")
        self.addCleanup(self.tmp.cleanup)
        self.home = Path(self.tmp.name)
        self.elpa = self.home / ".emacs.d/elpa"

    def _seed_elpa(self):
        pkg = self.elpa / "demo-pkg-1.0"
        pkg.mkdir(parents=True)
        (pkg / "demo.el").write_bytes(b";;; demo.el --- fixture\n(provide 'demo)\n")
        script = pkg / "run.sh"
        script.write_bytes(b"#!/bin/sh\necho ok\n")
        script.chmod(0o755)
        target = pkg / "target.txt"
        target.write_bytes(b"real\n")
        os.symlink("target.txt", pkg / "link.txt")

    def _run(self, block, extra_env=None):
        # Keep even a faulty temporary-path implementation inside the fixture;
        # assertions below require the persistent location. Never source BASH_ENV.
        tmp = self.home / "tmp"
        tmp.mkdir(exist_ok=True)
        env = {"HOME": str(self.home), "TMPDIR": str(tmp),
               "PATH": os.defpath, "LC_ALL": "C"}
        if extra_env:
            env.update(extra_env)
        return subprocess.run(
            ["bash", "-c", block],
            capture_output=True, text=True, env=env,
        )

    def _parse_kv(self, stdout, key):
        for line in stdout.splitlines():
            if line.startswith(key + "="):
                return line.split("=", 1)[1]
        return None

    # --- snapshot ---------------------------------------------------------

    def test_snapshot_lives_under_persistent_home_state(self):
        self._seed_elpa()
        run = self._run(self.blocks["snapshot-elpa"])
        self.assertEqual(run.returncode, 0, run.stderr)
        snap = self._parse_kv(run.stdout, "snapshot")
        assert snap is not None
        snap_root = self._parse_kv(run.stdout, "snap_root")
        assert snap_root is not None
        expected_parent = self.home / ".local/state/tilde/upgrades"
        self.assertEqual(Path(snap_root).parent, expected_parent)
        # A second snapshot in the same second must also get its own directory.
        again = self._run(self.blocks["snapshot-elpa"])
        self.assertEqual(again.returncode, 0, again.stderr)
        self.assertNotEqual(snap_root, self._parse_kv(again.stdout, "snap_root"))

    def test_snapshot_kv_stdout_and_errors_stderr(self):
        # Guard: no human error text is ever written to stdout by the
        # snapshot block, and the KV lines only exist on stdout.
        run = self._run(self.blocks["snapshot-elpa"])  # missing elpa
        self.assertNotEqual(run.returncode, 0)
        self.assertEqual(run.stdout, "")
        self.assertIn("no elpa directory", run.stderr)

    def test_snapshot_then_mutate_then_restore(self):
        self._seed_elpa()
        snap_run = self._run(self.blocks["snapshot-elpa"])
        self.assertEqual(snap_run.returncode, 0, snap_run.stderr)
        snap = self._parse_kv(snap_run.stdout, "snapshot")
        snap_root = self._parse_kv(snap_run.stdout, "snap_root")
        assert snap is not None and snap_root is not None
        snap_path = Path(snap)
        self.assertTrue(snap_path.is_dir())
        # Private backup directory (0700) at both root and snap.
        self.assertEqual(Path(snap_root).stat().st_mode & 0o777, 0o700)
        # Symlink preserved with cp -Rp.
        self.assertTrue((snap_path / "demo-pkg-1.0/link.txt").is_symlink())
        # Executable bit preserved with cp -Rp.
        self.assertTrue(os.access(str(snap_path / "demo-pkg-1.0/run.sh"), os.X_OK))
        # Snapshot is a private directory tree, not the .emacs.d
        # hierarchy and not any agent auth or session directory.
        self.assertNotIn(self.elpa, snap_path.parents)
        self.assertNotIn(self.home / ".emacs.d", snap_path.parents)
        for forbidden in (".claude", ".pi", ".config/pi"):
            self.assertNotIn(self.home / forbidden, snap_path.parents)

        # Simulate a failed upgrade that mutates the live elpa.
        (self.elpa / "demo-pkg-1.0/demo.el").write_bytes(b";; broken by upgrade\n")
        (self.elpa / "brand-new").mkdir()
        (self.elpa / "brand-new/junk.el").write_bytes(b"; leftover\n")

        restore_run = self._run(self.blocks["restore-elpa"], extra_env={"snap": str(snap_path)})
        self.assertEqual(restore_run.returncode, 0, restore_run.stderr)
        failed = self._parse_kv(restore_run.stdout, "failed_elpa")
        assert failed is not None
        failed_path = Path(failed)
        # Failed slot is a fixed name inside the snapshot's own parent
        # directory (no second-granularity timestamp in the name).
        self.assertEqual(failed_path, Path(snap_root) / "elpa.failed")
        # Failed state retained, not destroyed.
        self.assertTrue(failed_path.is_dir())
        self.assertEqual((failed_path / "demo-pkg-1.0/demo.el").read_bytes(),
                         b";; broken by upgrade\n")
        self.assertTrue((failed_path / "brand-new").is_dir())
        # Restored state matches snapshot.
        self.assertEqual((self.elpa / "demo-pkg-1.0/demo.el").read_bytes(),
                         b";;; demo.el --- fixture\n(provide 'demo)\n")
        self.assertTrue((self.elpa / "demo-pkg-1.0/link.txt").is_symlink())
        self.assertTrue(os.access(str(self.elpa / "demo-pkg-1.0/run.sh"), os.X_OK))
        self.assertFalse((self.elpa / "brand-new").exists())

    def test_snapshot_refuses_symlinked_elpa(self):
        self.elpa.parent.mkdir(parents=True)
        real = self.home / "other-elpa"
        real.mkdir()
        os.symlink(real, self.elpa)
        run = self._run(self.blocks["snapshot-elpa"])
        self.assertNotEqual(run.returncode, 0)
        self.assertIn("symlink", run.stderr)
        self.assertNotIn("symlink", run.stdout)

    def test_snapshot_refuses_missing_elpa(self):
        run = self._run(self.blocks["snapshot-elpa"])
        self.assertNotEqual(run.returncode, 0)
        self.assertIn("no elpa directory", run.stderr)

    def test_snapshot_refuses_nondirectory_live_elpa(self):
        self.elpa.parent.mkdir(parents=True)
        self.elpa.write_bytes(b"not a directory\n")
        run = self._run(self.blocks["snapshot-elpa"])
        self.assertNotEqual(run.returncode, 0)
        self.assertIn("not a directory", run.stderr)

    def test_documented_blocks_pass_shellcheck(self):
        source = self.blocks["snapshot-elpa"] + "\n" + self.blocks["restore-elpa"]
        run = subprocess.run(["shellcheck", "--shell=sh", "-"], input=source,
                             text=True, capture_output=True, check=False)
        self.assertEqual(run.returncode, 0, run.stdout + run.stderr)

    # --- restore ----------------------------------------------------------

    def test_restore_refuses_nondirectory_live_elpa(self):
        self._seed_elpa()
        run = self._run(self.blocks["snapshot-elpa"])
        snap = self._parse_kv(run.stdout, "snapshot")
        os.rename(self.elpa, self.home / "held-elpa")
        self.elpa.write_bytes(b"unrelated file")
        result = self._run(self.blocks["restore-elpa"], {"snap": snap})
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual(self.elpa.read_bytes(), b"unrelated file")

    def test_restore_refuses_existing_failed_snapshot(self):
        self._seed_elpa()
        snap_run = self._run(self.blocks["snapshot-elpa"])
        self.assertEqual(snap_run.returncode, 0, snap_run.stderr)
        snap = self._parse_kv(snap_run.stdout, "snapshot")
        snap_root = self._parse_kv(snap_run.stdout, "snap_root")
        assert snap is not None and snap_root is not None
        # Pre-create the exact fixed failed slot; no date-boundary race
        # because the slot's name is fixed, not timestamped.
        prior = Path(snap_root) / "elpa.failed"
        prior.mkdir()
        (prior / "sentinel").write_bytes(b"do-not-clobber\n")
        run = self._run(self.blocks["restore-elpa"], extra_env={"snap": snap})
        self.assertNotEqual(run.returncode, 0)
        self.assertIn("already exists", run.stderr)
        self.assertEqual((prior / "sentinel").read_bytes(), b"do-not-clobber\n")

    def test_restore_refuses_broken_symlink_failed_slot(self):
        # A broken symlink at the failed slot fails `[ -e ]` but must
        # still be caught by the `-L` guard.
        self._seed_elpa()
        snap_run = self._run(self.blocks["snapshot-elpa"])
        snap = self._parse_kv(snap_run.stdout, "snapshot")
        snap_root = self._parse_kv(snap_run.stdout, "snap_root")
        assert snap is not None and snap_root is not None
        broken = Path(snap_root) / "elpa.failed"
        os.symlink(self.home / "nonexistent", broken)
        run = self._run(self.blocks["restore-elpa"], extra_env={"snap": snap})
        self.assertNotEqual(run.returncode, 0)
        self.assertIn("already exists", run.stderr)
        self.assertTrue(broken.is_symlink())

    def test_restore_refuses_symlink_snapshot(self):
        self._seed_elpa()
        snap_run = self._run(self.blocks["snapshot-elpa"])
        snap = self._parse_kv(snap_run.stdout, "snapshot")
        snap_root = self._parse_kv(snap_run.stdout, "snap_root")
        assert snap is not None and snap_root is not None
        # Replace the snapshot directory with a symlink; restore must
        # refuse rather than dereferencing it.
        real = Path(snap_root) / "elpa-real"
        os.rename(snap, real)
        os.symlink(real, snap)
        run = self._run(self.blocks["restore-elpa"], extra_env={"snap": snap})
        self.assertNotEqual(run.returncode, 0)
        self.assertIn("snapshot missing", run.stderr)

    def test_restore_missing_snapshot_refused(self):
        self._seed_elpa()
        run = self._run(self.blocks["restore-elpa"],
                        extra_env={"snap": str(self.home / "nonexistent")})
        self.assertNotEqual(run.returncode, 0)
        self.assertIn("snapshot missing", run.stderr)

    def test_restore_preserves_snapshot_and_failed_when_cp_fails(self):
        # Simulate a copy failure at restore time by injecting a stub
        # `cp` on PATH that always exits non-zero. Both the snapshot
        # and the quarantined failed state must survive.
        self._seed_elpa()
        snap_run = self._run(self.blocks["snapshot-elpa"])
        self.assertEqual(snap_run.returncode, 0, snap_run.stderr)
        snap = self._parse_kv(snap_run.stdout, "snapshot")
        snap_root = self._parse_kv(snap_run.stdout, "snap_root")
        assert snap is not None and snap_root is not None

        # Mutate the live elpa so the "failed" content is distinctive.
        (self.elpa / "demo-pkg-1.0/demo.el").write_bytes(b";; broken\n")
        snapshot_demo = Path(snap) / "demo-pkg-1.0/demo.el"
        original_snapshot_bytes = snapshot_demo.read_bytes()

        stub_dir = self.home / "stub-bin"
        stub_dir.mkdir()
        stub_cp = stub_dir / "cp"
        stub_cp.write_text('#!/bin/sh\necho "simulated cp failure" >&2\nexit 42\n')
        stub_cp.chmod(0o755)

        run = self._run(
            self.blocks["restore-elpa"],
            extra_env={"snap": snap, "PATH": str(stub_dir) + os.pathsep + os.environ["PATH"]},
        )
        self.assertNotEqual(run.returncode, 0)
        # Snapshot is untouched.
        self.assertTrue(Path(snap).is_dir())
        self.assertEqual(snapshot_demo.read_bytes(), original_snapshot_bytes)
        # Failed state was quarantined into the fixed slot and preserved.
        failed = Path(snap_root) / "elpa.failed"
        self.assertTrue(failed.is_dir())
        self.assertEqual((failed / "demo-pkg-1.0/demo.el").read_bytes(),
                         b";; broken\n")
        # Live elpa is missing (it was moved to `failed`); the operator
        # sees the failure and can retry with a good cp.
        self.assertFalse(self.elpa.exists())


if __name__ == "__main__":
    unittest.main()
