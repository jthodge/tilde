"""Regression tests for scripts/check.

The deployment checker is exercised inside a disposable Git checkout
with a synthetic `.stow-packages` and a synthetic `$HOME`. The real
repo, real `$HOME`, and real Git config are never touched.

The failure modes covered are the ones the checker previously
mishandled:

- two failed `realpath` values compared as equal (false OK on a
  broken symlink or a missing `realpath` binary),
- `git ls-files` errors swallowed by process substitution,
- a missing manifest silently reporting zero problems,
- a malformed manifest passing shell metacharacters into `git ls-files`
  and `realpath`.

Seed-only ownership for `claude/.claude/settings.json` is preserved.
"""
from __future__ import annotations

import os
import shutil
import stat
import subprocess
import tempfile
import unittest
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]
CHECK = REPO / "scripts/check"


def _git(cwd: Path, *args: str) -> None:
    env = dict(os.environ, GIT_CONFIG_GLOBAL="/dev/null",
               GIT_CONFIG_SYSTEM="/dev/null",
               GIT_AUTHOR_NAME="t", GIT_AUTHOR_EMAIL="t@example.invalid",
               GIT_COMMITTER_NAME="t", GIT_COMMITTER_EMAIL="t@example.invalid")
    subprocess.run(["git", *args], cwd=str(cwd), env=env, check=True,
                   capture_output=True, text=True)


class CheckTest(unittest.TestCase):
    def setUp(self) -> None:
        self.tmp = Path(tempfile.mkdtemp(prefix="tilde-check-"))
        self.addCleanup(shutil.rmtree, self.tmp, ignore_errors=True)

        # scripts/check resolves REPO via `dirname $BASH_SOURCE/..`, so
        # we mirror that layout: `<root>/scripts/check`.
        self.root = self.tmp / "repo"
        (self.root / "scripts").mkdir(parents=True)
        shutil.copy2(CHECK, self.root / "scripts/check")
        (self.root / "scripts/check").chmod(0o755)

        self.home = self.tmp / "home"
        self.home.mkdir()

        _git(self.root, "init", "-q", "-b", "main")

    # -- fixture helpers ----------------------------------------------

    def _write(self, rel: str, content: str = "") -> Path:
        path = self.root / rel
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(content)
        return path

    def _seed(self, files: dict[str, str]) -> None:
        for rel, content in files.items():
            self._write(rel, content)
        _git(self.root, "add", "-A")

    def _run(self, *args: str, path: str | None = None) -> subprocess.CompletedProcess:
        env = dict(os.environ, HOME=str(self.home))
        if path is not None:
            env["PATH"] = path
        return subprocess.run(
            ["bash", str(self.root / "scripts/check"), *args],
            cwd=str(self.root), env=env, capture_output=True, text=True,
        )

    def _link(self, rel_in_home: str, source: Path) -> None:
        target = self.home / rel_in_home
        target.parent.mkdir(parents=True, exist_ok=True)
        os.symlink(source, target)

    # -- tests --------------------------------------------------------

    def test_ok_when_every_tracked_file_is_correctly_symlinked(self) -> None:
        self._seed({
            ".stow-packages": "fish\n",
            "fish/.config/fish/config.fish": "# ok\n",
        })
        self._link(".config/fish/config.fish",
                   self.root / "fish/.config/fish/config.fish")
        result = self._run()
        self.assertEqual(result.returncode, 0, msg=result.stderr + result.stdout)
        self.assertIn("OK: 1 tracked deployment entries", result.stdout)

    def test_missing_target_is_reported_and_exits_nonzero(self) -> None:
        self._seed({
            ".stow-packages": "fish\n",
            "fish/.config/fish/config.fish": "# ok\n",
        })
        result = self._run()
        self.assertEqual(result.returncode, 1)
        self.assertIn("MISSING", result.stdout)
        self.assertIn("fish/.config/fish/config.fish", result.stdout)

    def test_regular_file_shadowing_a_stow_link_is_drift(self) -> None:
        self._seed({
            ".stow-packages": "fish\n",
            "fish/.config/fish/config.fish": "# ok\n",
        })
        # An app replaced the link with a real file. `realpath` on both
        # sides succeeds but resolves to different paths.
        target = self.home / ".config/fish/config.fish"
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_text("# app rewrote this\n")
        result = self._run()
        self.assertEqual(result.returncode, 1)
        self.assertIn("DRIFT", result.stdout)
        self.assertIn("fish/.config/fish/config.fish", result.stdout)

    def test_broken_symlink_is_drift_not_ok(self) -> None:
        """Previously two failed `realpath` calls compared as equal."""
        self._seed({
            ".stow-packages": "fish\n",
            "fish/.config/fish/config.fish": "# ok\n",
        })
        target = self.home / ".config/fish/config.fish"
        target.parent.mkdir(parents=True, exist_ok=True)
        os.symlink(self.tmp / "does-not-exist", target)
        result = self._run()
        self.assertEqual(result.returncode, 1)
        self.assertIn("DRIFT", result.stdout)
        self.assertIn("unresolvable", result.stdout)

    def test_declared_package_missing_from_repo_is_drift(self) -> None:
        # A package listed in the manifest but never tracked.
        self._seed({".stow-packages": "phantom\n"})
        result = self._run()
        self.assertEqual(result.returncode, 1)
        self.assertIn("DRIFT", result.stdout)
        self.assertIn("phantom", result.stdout)

    def test_undeclared_top_level_package_is_reported(self) -> None:
        self._seed({
            ".stow-packages": "fish\n",
            "fish/.config/fish/config.fish": "# ok\n",
            # A tracked top-level dir that no one declared.
            "orphan/marker": "x\n",
        })
        self._link(".config/fish/config.fish",
                   self.root / "fish/.config/fish/config.fish")
        result = self._run()
        self.assertEqual(result.returncode, 1)
        self.assertIn("UNDECLARED", result.stdout)
        self.assertIn("orphan", result.stdout)

    def test_claude_settings_as_symlink_is_drift(self) -> None:
        """Seed-only ownership: settings.json must be an app-owned file."""
        self._seed({
            ".stow-packages": "claude\n",
            "claude/.claude/settings.json": '{"model":"default"}\n',
        })
        self._link(".claude/settings.json",
                   self.root / "claude/.claude/settings.json")
        result = self._run()
        self.assertEqual(result.returncode, 1)
        self.assertIn("DRIFT", result.stdout)
        self.assertIn("expected an app-owned file", result.stdout)

    def test_claude_settings_as_regular_file_is_ok(self) -> None:
        self._seed({
            ".stow-packages": "claude\n",
            "claude/.claude/settings.json": '{"model":"default"}\n',
        })
        deployed = self.home / ".claude/settings.json"
        deployed.parent.mkdir(parents=True)
        deployed.write_text('{"model":"local"}\n')
        result = self._run()
        self.assertEqual(result.returncode, 0, msg=result.stderr + result.stdout)
        self.assertIn("OK: 1 tracked deployment entries", result.stdout)

    def test_missing_manifest_fails_closed(self) -> None:
        # Stage only a package, without a .stow-packages manifest.
        self._seed({"fish/.config/fish/config.fish": "# ok\n"})
        result = self._run()
        self.assertNotEqual(result.returncode, 0)
        self.assertIn(".stow-packages", result.stderr)
        self.assertIn("aborting", result.stderr)

    def test_malformed_manifest_entry_fails_closed(self) -> None:
        # A shell metacharacter must never reach `git ls-files --` or
        # `realpath`. The previous script would have gone on to run
        # `git ls-files -- "; rm -rf /"` and print an odd DRIFT line.
        self._seed({".stow-packages": "; rm -rf /\n"})
        result = self._run()
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("malformed package name", result.stderr)

    def test_path_separator_in_manifest_is_rejected(self) -> None:
        self._seed({".stow-packages": "foo/bar\n"})
        result = self._run()
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("malformed package name", result.stderr)

    def test_missing_dependency_fails_closed(self) -> None:
        """Without realpath, empty-vs-empty comparison used to pass."""
        self._seed({
            ".stow-packages": "fish\n",
            "fish/.config/fish/config.fish": "# ok\n",
        })
        # A PATH containing only bash keeps the shell working but hides
        # every other tool the checker depends on.
        bin_only = self.tmp / "bin-only"
        bin_only.mkdir()
        # Symlink rather than copy: /bin/bash on macOS carries file
        # flags that copystat() cannot preserve from a Python test.
        os.symlink(shutil.which("bash") or "/bin/bash", bin_only / "bash")
        result = self._run(path=str(bin_only))
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("required tool not on PATH", result.stderr)

    def test_inspection_failures_never_report_ok(self) -> None:
        self._seed({".stow-packages": "fish\n", "fish/.config/fish/config.fish": "x"})
        self._link(".config/fish/config.fish", self.root / "fish/.config/fish/config.fish")
        for tool in ("git", "realpath"):
            with self.subTest(tool=tool):
                bin_dir = self.tmp / ("broken-" + tool)
                bin_dir.mkdir()
                stub = bin_dir / tool
                stub.write_text("#!/bin/sh\nexit 1\n")
                stub.chmod(0o755)
                result = self._run(path=str(bin_dir) + os.pathsep + os.environ["PATH"])
                self.assertNotEqual(result.returncode, 0)
                self.assertNotIn("OK:", result.stdout)

    def test_manifest_without_final_newline_is_read(self) -> None:
        self._seed({".stow-packages": "fish", "fish/.config/fish/config.fish": "x"})
        result = self._run()
        self.assertEqual(result.returncode, 1)
        self.assertIn("MISSING", result.stdout)

    def test_explicit_package_argument_is_validated(self) -> None:
        self._seed({
            ".stow-packages": "fish\n",
            "fish/.config/fish/config.fish": "# ok\n",
        })
        result = self._run("../etc")
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("malformed package name", result.stderr)


if __name__ == "__main__":
    unittest.main()
