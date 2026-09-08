"""Tests for scripts/setup-tools.

Every test invokes the script as a subprocess with a sandboxed PATH and
HOME. --check mode must not mutate the working tree; --install mode is
exercised with mock `volta`/`uv` executables that log their invocations
to a file, so no real toolchain call is made.
"""
from __future__ import annotations

import os
import shutil
import stat
import subprocess
import textwrap
import tempfile
import unittest
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]
SCRIPT = REPO / "scripts" / "setup-tools"
MANIFEST_SRC = REPO / "scripts" / "runtime-versions.env"


def _make_bin(dir_path: Path, name: str, script: str) -> Path:
    path = dir_path / name
    path.write_text("#!/usr/bin/env bash\n" + script + "\n")
    path.chmod(path.stat().st_mode | stat.S_IXUSR | stat.S_IXGRP | stat.S_IXOTH)
    return path


class SetupToolsCheckMode(unittest.TestCase):
    def setUp(self) -> None:
        self.tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self.tmp.cleanup)
        self.bindir = Path(self.tmp.name) / "bin"
        self.bindir.mkdir()

    def _run(self, extra_args: list[str], env_overrides: dict) -> subprocess.CompletedProcess:
        env = os.environ.copy()
        # Keep /usr/bin and /bin so `env bash`, sed, and grep resolve;
        # the mock bindir goes first so it wins for shadowed tools.
        env["PATH"] = f"{self.bindir}:/usr/bin:/bin"
        env.update(env_overrides)
        return subprocess.run(
            [str(SCRIPT), *extra_args],
            capture_output=True,
            text=True,
            env=env,
            check=False,
        )

    def test_check_reports_missing_volta_and_exits_nonzero(self) -> None:
        proc = self._run(["--check"], {})
        self.assertEqual(proc.returncode, 1, msg=proc.stderr)
        self.assertIn("MISSING volta", proc.stderr)

    def test_check_with_volta_default_keeps_existing(self) -> None:
        # Mock volta reports every asked-for tool as already default.
        _make_bin(
            self.bindir,
            "volta",
            textwrap.dedent(
                """
                if [ "$1" = "list" ]; then
                  case "$2" in
                    node) echo "runtime node@22.14.0 (default)" ;;
                    pnpm) echo "package-manager pnpm@10.15.0 (default)" ;;
                    yarn) echo "package-manager yarn@4.4.0 (default)" ;;
                  esac
                fi
                """
            ).strip(),
        )
        proc = self._run(["--check"], {})
        self.assertEqual(proc.returncode, 0, msg=proc.stderr)
        self.assertIn("keep node default", proc.stderr)
        self.assertIn("keep pnpm default", proc.stderr)
        self.assertIn("keep yarn default", proc.stderr)

    def test_check_would_install_when_volta_default_absent(self) -> None:
        # Volta present but reports no defaults.
        _make_bin(self.bindir, "volta", 'exit 0')
        proc = self._run(["--check"], {})
        self.assertEqual(proc.returncode, 0, msg=proc.stderr)
        self.assertIn("would install node@22.14.0 via Volta", proc.stderr)
        self.assertIn("would install pnpm@10.15.0 via Volta", proc.stderr)
        self.assertIn("would install yarn@4.4.0 via Volta", proc.stderr)

    def test_check_does_not_mutate_repo(self) -> None:
        # Capture the mtimes of every tracked scripts/ file, then confirm
        # they are unchanged after a --check run.
        watched = [p for p in (REPO / "scripts").iterdir() if p.is_file()]
        before = {p: p.stat().st_mtime_ns for p in watched}
        proc = self._run(["--check"], {})
        self.assertIn(proc.returncode, (0, 1))
        after = {p: p.stat().st_mtime_ns for p in watched}
        self.assertEqual(before, after)

    def test_invalid_flag_returns_two(self) -> None:
        proc = self._run(["--wat"], {})
        self.assertEqual(proc.returncode, 2)
        self.assertIn("unknown argument", proc.stderr)


class SetupToolsInstallMode(unittest.TestCase):
    """--install must call `volta install` only for absent defaults."""

    def setUp(self) -> None:
        self.tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self.tmp.cleanup)
        self.bindir = Path(self.tmp.name) / "bin"
        self.bindir.mkdir()
        self.log = Path(self.tmp.name) / "calls.log"

    def _run(self) -> subprocess.CompletedProcess:
        env = os.environ.copy()
        env["PATH"] = f"{self.bindir}:/usr/bin:/bin"
        env["SETUP_TOOLS_LOG"] = str(self.log)
        env["HOME"] = self.tmp.name
        return subprocess.run(
            [str(SCRIPT), "--install"],
            capture_output=True,
            text=True,
            env=env,
            check=False,
        )

    def test_install_skips_when_defaults_exist(self) -> None:
        _make_bin(
            self.bindir,
            "volta",
            textwrap.dedent(
                """
                echo "invoked $@" >> "$SETUP_TOOLS_LOG"
                if [ "$1" = "list" ]; then
                  case "$2" in
                    node) echo "runtime node@22.14.0 (default)" ;;
                    pnpm) echo "package-manager pnpm@10.15.0 (default)" ;;
                    yarn) echo "package-manager yarn@4.4.0 (default)" ;;
                  esac
                fi
                """
            ).strip(),
        )
        # Mock uv so the base-venv branch is exercised.
        _make_bin(self.bindir, "uv", 'echo "uv $@" >> "$SETUP_TOOLS_LOG"')
        proc = self._run()
        self.assertEqual(proc.returncode, 0, msg=proc.stderr)
        calls = self.log.read_text().splitlines() if self.log.exists() else []
        # Only `volta list` calls should appear; no `volta install`.
        self.assertTrue(any(c.startswith("invoked list ") for c in calls))
        self.assertFalse(any("install" in c and c.startswith("invoked") for c in calls))
        self.assertIn(f"uv venv {self.tmp.name}/.venv/base", calls)

    def test_install_invokes_volta_install_when_missing(self) -> None:
        _make_bin(
            self.bindir,
            "volta",
            textwrap.dedent(
                """
                echo "invoked $@" >> "$SETUP_TOOLS_LOG"
                # `list` prints nothing (nothing is default).
                exit 0
                """
            ).strip(),
        )
        _make_bin(self.bindir, "uv", 'echo "uv $@" >> "$SETUP_TOOLS_LOG"')
        proc = self._run()
        self.assertEqual(proc.returncode, 0, msg=proc.stderr)
        calls = self.log.read_text().splitlines() if self.log.exists() else []
        install_calls = [c for c in calls if c.startswith("invoked install")]
        self.assertEqual(len(install_calls), 3)
        for token in ("node@22.14.0", "pnpm@10.15.0", "yarn@4.4.0"):
            self.assertTrue(
                any(token in c for c in install_calls),
                msg=f"missing install call for {token}: {install_calls}",
            )


class ManifestShape(unittest.TestCase):
    """The manifest is the pinned interface — assert its shape."""

    def test_manifest_declares_all_three_versions(self) -> None:
        import re
        text = MANIFEST_SRC.read_text()
        for pattern in (
            r"^NODE_VERSION=22\.14\.0$",
            r"^PNPM_VERSION=10\.15\.0$",
            r"^YARN_VERSION=4\.4\.0$",
        ):
            self.assertRegex(text, re.compile(pattern, re.MULTILINE))

    def test_manifest_is_kv_lines_only(self) -> None:
        for raw in MANIFEST_SRC.read_text().splitlines():
            line = raw.strip()
            if not line or line.startswith("#"):
                continue
            self.assertIn("=", line, msg=f"non-kv line: {line!r}")


if __name__ == "__main__":
    unittest.main()
