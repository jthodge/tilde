"""Regression tests for git/.config/git/hooks/pre-commit.

The hook is exercised inside disposable Git repositories with synthetic
tokens only. No real credentials are ever staged, no commits are ever
recorded (the hook is invoked directly on the index), and no live
`.git/hooks` on this checkout are touched.
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
HOOK = REPO / "git/.config/git/hooks/pre-commit"

# Synthetic tokens that match the hook's patterns but are obviously not
# real credentials. Regenerating them requires a matching pattern edit.
SYNTHETIC_AWS = "AKIA" + "A" * 16
SYNTHETIC_GH_PAT = "ghp_" + "a" * 36
SYNTHETIC_OPENAI = "sk-" + "A1" * 20  # 43 chars, satisfies {40,}


def _git(cwd: Path, *args: str, env: dict | None = None,
         check: bool = True) -> subprocess.CompletedProcess:
    full_env = dict(os.environ)
    # Isolate every git invocation from the developer's real config.
    full_env.update({
        "GIT_CONFIG_GLOBAL": "/dev/null",
        "GIT_CONFIG_SYSTEM": "/dev/null",
        "GIT_AUTHOR_NAME": "t",
        "GIT_AUTHOR_EMAIL": "t@example.invalid",
        "GIT_COMMITTER_NAME": "t",
        "GIT_COMMITTER_EMAIL": "t@example.invalid",
        # Fixtures use index/tree objects only: never create commits.
        "GIT_ALLOW_PROTOCOL": "file",
    })
    if env:
        full_env.update(env)
    return subprocess.run(
        ["git", *args], cwd=str(cwd), env=full_env,
        capture_output=True, text=True, check=check,
    )


class SecretHookTest(unittest.TestCase):
    def setUp(self) -> None:
        self.tmp = Path(tempfile.mkdtemp(prefix="tilde-hook-"))
        self.addCleanup(shutil.rmtree, self.tmp, ignore_errors=True)
        self.repo = self.tmp / "repo"
        self.repo.mkdir()
        _git(self.repo, "init", "-q", "-b", "main")
        self.base_tree = ""
        self.adapter = self.tmp / "adapter"
        self.adapter.mkdir()
        wrapper = self.adapter / "git"
        wrapper.write_text('''#!/bin/sh
if [ "$1" = diff ] && [ -n "$TEST_BASE_TREE" ]; then
    shift
    exec "$TEST_GIT" diff "$TEST_BASE_TREE" "$@"
fi
exec "$TEST_GIT" "$@"
''')
        wrapper.chmod(0o755)

    # -- helpers -------------------------------------------------------

    def _write_and_stage(self, rel: str, content: str) -> None:
        path = self.repo / rel
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(content)
        _git(self.repo, "add", "--", rel)

    def _seed_tree(self, files: dict[str, str]) -> None:
        """Create a baseline tree; the adapter compares the index to it."""
        for rel, content in files.items():
            self._write_and_stage(rel, content)
        self.base_tree = _git(self.repo, "write-tree").stdout.strip()

    def _run_hook(self, extra_path: str | None = None) -> subprocess.CompletedProcess:
        env = dict(os.environ, TEST_BASE_TREE=self.base_tree,
                   TEST_GIT=shutil.which("git") or "/usr/bin/git",
                   GIT_CONFIG_GLOBAL="/dev/null", GIT_CONFIG_SYSTEM="/dev/null")
        env["PATH"] = str(self.adapter) + os.pathsep + os.environ["PATH"]
        if extra_path is not None:
            env["PATH"] = extra_path
        return subprocess.run(
            ["bash", str(HOOK)], cwd=str(self.repo), env=env,
            capture_output=True, text=True,
        )

    # -- tests ---------------------------------------------------------

    def test_clean_addition_is_allowed(self) -> None:
        self._write_and_stage("notes.txt", "hello world\n")
        result = self._run_hook()
        self.assertEqual(result.returncode, 0, msg=result.stderr)
        self.assertEqual(result.stdout, "")
        self.assertEqual(result.stderr, "")

    def test_added_secret_is_blocked_without_echoing_the_token(self) -> None:
        self._write_and_stage("keys.env", f"AWS_KEY={SYNTHETIC_AWS}\n")
        result = self._run_hook()
        self.assertEqual(result.returncode, 1, msg=result.stderr)
        self.assertIn("AWS access key ID", result.stderr)
        self.assertIn("keys.env", result.stderr)
        # The literal secret must never appear in either stream.
        self.assertNotIn(SYNTHETIC_AWS, result.stderr)
        self.assertNotIn(SYNTHETIC_AWS, result.stdout)
        # And we must not advertise `--no-verify` as a routine escape.
        self.assertNotIn("--no-verify", result.stderr)

    def test_deletion_of_a_flagged_file_is_allowed(self) -> None:
        # History carries a synthetic secret; the staged change removes it.
        self._seed_tree({"keys.env": f"AWS_KEY={SYNTHETIC_AWS}\n"})
        _git(self.repo, "update-index", "--force-remove", "--", "keys.env")
        result = self._run_hook()
        self.assertEqual(result.returncode, 0, msg=result.stderr)
        self.assertEqual(result.stderr, "")

    def test_rename_with_clean_destination_is_allowed(self) -> None:
        self._seed_tree({"old.txt": "harmless\n"})
        _git(self.repo, "mv", "old.txt", "new.txt")
        result = self._run_hook()
        self.assertEqual(result.returncode, 0, msg=result.stderr)

    def test_rename_that_introduces_a_secret_is_blocked(self) -> None:
        self._seed_tree({"old.txt": "harmless\n"})
        _git(self.repo, "mv", "old.txt", "new.txt")
        (self.repo / "new.txt").write_text(f"token={SYNTHETIC_GH_PAT}\n")
        _git(self.repo, "add", "--", "new.txt")
        result = self._run_hook()
        self.assertEqual(result.returncode, 1, msg=result.stderr)
        self.assertIn("GitHub personal access token", result.stderr)
        self.assertIn("new.txt", result.stderr)
        self.assertNotIn(SYNTHETIC_GH_PAT, result.stderr)

    def test_unstaged_secret_in_a_partially_staged_file_is_ignored(self) -> None:
        """The hook must scan the staged version, not the worktree."""
        self._seed_tree({"cfg.txt": "line one\n"})
        # Stage a benign edit, then dirty the worktree with a synthetic
        # secret that never enters the index.
        (self.repo / "cfg.txt").write_text("line one\nline two\n")
        _git(self.repo, "add", "--", "cfg.txt")
        (self.repo / "cfg.txt").write_text(
            f"line one\nline two\nleak={SYNTHETIC_OPENAI}\n")
        result = self._run_hook()
        self.assertEqual(result.returncode, 0, msg=result.stderr)
        self.assertEqual(result.stderr, "")

    def test_filenames_with_spaces_are_handled(self) -> None:
        self._write_and_stage("a name with spaces.txt",
                              f"token={SYNTHETIC_GH_PAT}\n")
        result = self._run_hook()
        self.assertEqual(result.returncode, 1, msg=result.stderr)
        self.assertIn(r"a\ name\ with\ spaces.txt", result.stderr)
        self.assertNotIn(SYNTHETIC_GH_PAT, result.stderr)

    def test_type_change_and_binary_content_are_scanned(self) -> None:
        link = self.repo / "changed"
        link.symlink_to("absent")
        _git(self.repo, "add", "changed")
        self.base_tree = _git(self.repo, "write-tree").stdout.strip()
        link.unlink()
        link.write_bytes(b"\0" + SYNTHETIC_GH_PAT.encode() + b"\0")
        _git(self.repo, "add", "changed")
        result = self._run_hook()
        self.assertEqual(result.returncode, 1)
        self.assertIn("GitHub personal access token", result.stderr)
        self.assertNotIn(SYNTHETIC_GH_PAT, result.stderr)

    def test_large_blob_is_scanned_without_sigpipe(self) -> None:
        self._write_and_stage("large", SYNTHETIC_GH_PAT + "\n" + "x" * 1000000)
        result = self._run_hook()
        self.assertEqual(result.returncode, 1)
        self.assertIn("GitHub personal access token", result.stderr)
        self.assertNotIn("grep failed", result.stderr)

    def test_gitlink_is_not_read_as_a_blob(self) -> None:
        _git(self.repo, "update-index", "--add", "--cacheinfo", "160000," + "1" * 40 + ",module")
        result = self._run_hook()
        self.assertEqual(result.returncode, 0, result.stderr)

    def test_grep_and_blob_failures_abort(self) -> None:
        self._write_and_stage("text", "safe")
        for tool in ("grep", "git"):
            with self.subTest(tool=tool):
                folder = self.tmp / ("fail-" + tool)
                folder.mkdir()
                program = folder / tool
                if tool == "git":
                    program.write_text('#!/bin/sh\nif [ "$1" = cat-file ]; then exit 1; fi\nexec "$TEST_GIT" "$@"\n')
                else:
                    program.write_text('#!/bin/sh\nexit 2\n')
                program.chmod(0o755)
                result = self._run_hook(str(folder) + os.pathsep + os.environ["PATH"])
                self.assertNotEqual(result.returncode, 0)
                self.assertIn("aborting", result.stderr)

    def test_hook_fails_closed_when_git_is_missing(self) -> None:
        """A missing/broken `git` must abort, not silently pass."""
        fake_bin = self.tmp / "bin"
        fake_bin.mkdir()
        stub = fake_bin / "git"
        stub.write_text("#!/usr/bin/env bash\nexit 127\n")
        stub.chmod(stub.stat().st_mode | stat.S_IXUSR | stat.S_IXGRP | stat.S_IXOTH)
        result = self._run_hook(extra_path=f"{fake_bin}:/usr/bin:/bin")
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("aborting", result.stderr)


if __name__ == "__main__":
    unittest.main()
