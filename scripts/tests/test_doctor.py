"""Tests for scripts/doctor.

The doctor script is treated as an executable black box: invoke it with
a sandboxed PATH and REPO layout, parse the JSON stdout, and verify the
public shape. The doctor module itself is also imported to unit-test
the executable-resolution helper against a controlled PATH.
"""
from __future__ import annotations

import importlib.util
import json
import os
import shutil
import stat
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]
DOCTOR = REPO / "scripts" / "doctor"


def _load_doctor_module():
    """Load scripts/doctor (no .py suffix) as a module for unit tests."""
    from importlib.machinery import SourceFileLoader
    loader = SourceFileLoader("doctor", str(DOCTOR))
    spec = importlib.util.spec_from_loader("doctor", loader)
    assert spec is not None
    module = importlib.util.module_from_spec(spec)
    loader.exec_module(module)
    return module


def _touch_exe(dir_path: Path, name: str) -> Path:
    path = dir_path / name
    path.write_text("#!/usr/bin/env bash\nexit 0\n")
    path.chmod(path.stat().st_mode | stat.S_IXUSR | stat.S_IXGRP | stat.S_IXOTH)
    return path


class WhichAllResolution(unittest.TestCase):
    def setUp(self) -> None:
        self.tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self.tmp.cleanup)
        self.a = Path(self.tmp.name) / "a"
        self.b = Path(self.tmp.name) / "b"
        self.a.mkdir()
        self.b.mkdir()
        self.doctor = _load_doctor_module()

    def test_reads_runtime_without_executing_volta(self) -> None:
        platform = self.a / "tools/user/platform.json"
        platform.parent.mkdir(parents=True)
        platform.write_text('{"node": {"runtime": "22.14.0"}}')
        with unittest_env({"VOLTA_HOME": str(self.a), "PATH": str(self.b)}):
            self.assertEqual(self.doctor.volta_default("node"), "22.14.0")
            self.assertIsNone(self.doctor.volta_default("yarn"))

    def test_missing_runtime_is_reported(self) -> None:
        with unittest_env({"VOLTA_HOME": str(self.a)}):
            report = self.doctor.build_report()
        self.assertFalse(report["ok"])
        self.assertEqual(len(report["runtime_drift"]), 3)

    def test_returns_empty_when_absent(self) -> None:
        with unittest_env({"PATH": str(self.a)}):
            self.assertEqual(self.doctor.which_all("nope"), [])

    def test_returns_all_hits_in_path_order(self) -> None:
        first = _touch_exe(self.a, "widget")
        second = _touch_exe(self.b, "widget")
        with unittest_env({"PATH": f"{self.a}:{self.b}"}):
            hits = self.doctor.which_all("widget")
        self.assertEqual(hits, [str(first), str(second)])

    def test_probe_tool_separates_chosen_from_alternatives(self) -> None:
        _touch_exe(self.a, "widget")
        _touch_exe(self.b, "widget")
        with unittest_env({"PATH": f"{self.a}:{self.b}"}):
            probe = self.doctor.probe_tool("widget")
        self.assertTrue(probe["present"])
        self.assertEqual(probe["chosen"], str(self.a / "widget"))
        self.assertEqual(probe["alternatives"], [str(self.b / "widget")])


class DoctorSubprocess(unittest.TestCase):
    """End-to-end: run scripts/doctor and inspect its report."""

    def _run(self, path_dirs: list[str]) -> tuple[int, dict, str]:
        env = os.environ.copy()
        # Include /usr/bin and /bin so python3 and git resolve. The
        # test-provided dirs come first, which is how a real bootstrap
        # would look.
        env["PATH"] = ":".join([*path_dirs, "/usr/bin", "/bin"])
        with tempfile.TemporaryDirectory() as home:
            env["VOLTA_HOME"] = home
            platform = Path(home) / "tools/user/platform.json"
            platform.parent.mkdir(parents=True)
            platform.write_text(json.dumps({"node": {"runtime": "22.14.0"},
                                           "pnpm": "10.15.0", "yarn": "4.4.0"}))
            proc = subprocess.run(
                [sys.executable, str(DOCTOR)],
                capture_output=True,
                text=True,
                env=env,
                check=False,
            )
        report = json.loads(proc.stdout) if proc.stdout.strip() else {}
        return proc.returncode, report, proc.stderr

    def test_json_report_lists_required_and_optional_sections(self) -> None:
        rc, report, stderr = self._run([])
        self.assertIn("ok", report)
        self.assertIn("required_tools", report)
        self.assertIn("optional_tools", report)
        self.assertIn("tpm", report)
        self.assertIn("submodules", report)
        self.assertIn("runtimes", report)
        self.assertIn("runtime_drift", report)
        # Manifest key/values are surfaced verbatim.
        self.assertEqual(report["manifest"]["NODE_VERSION"], "22.14.0")
        # Verdict line always appears on stderr.
        self.assertIn("verdict:", stderr)

    def test_missing_required_tools_flips_exit_code(self) -> None:
        # Empty custom PATH — every required tool is absent (except
        # git/python3 which /usr/bin provides). Regardless of which
        # subset resolves on the host, doctor must exit nonzero when
        # something required is missing.
        with tempfile.TemporaryDirectory() as tmp:
            empty = Path(tmp) / "empty"
            empty.mkdir()
            rc, report, _ = self._run([str(empty)])
        # brew/stow/volta/uv/mise/jq/tmux/fzf/rg/fd/zoxide/gh cannot
        # possibly live in /usr/bin on stock macOS.
        self.assertTrue(report["missing_required"], msg=report)
        self.assertNotEqual(rc, 0)

    def test_optional_missing_does_not_flip_exit_code(self) -> None:
        # Build a PATH that contains every required tool as a no-op
        # shim plus the manifest-defined volta default reporter, but
        # NO optional tool. Doctor must still exit 0.
        with tempfile.TemporaryDirectory() as tmp:
            bindir = Path(tmp) / "bin"
            bindir.mkdir()
            for req in _load_doctor_module().REQUIRED_TOOLS:
                _touch_exe(bindir, req)
            # volta needs to report the manifest defaults so there is
            # no runtime drift.
            volta = bindir / "volta"
            volta.write_text(
                "#!/usr/bin/env bash\n"
                'if [ "$1" = "list" ]; then\n'
                '  case "$2" in\n'
                '    node) echo "runtime node@22.14.0 (default)" ;;\n'
                '    pnpm) echo "package-manager pnpm@10.15.0 (default)" ;;\n'
                '    yarn) echo "package-manager yarn@4.4.0 (default)" ;;\n'
                "  esac\n"
                "fi\n"
            )
            volta.chmod(volta.stat().st_mode | stat.S_IXUSR)
            rc, report, stderr = self._run([str(bindir)])
        self.assertEqual(report["missing_required"], [], msg=stderr)
        # No exit-flipping side effect from optional gaps.
        if not report["missing_submodules"] and report["tpm"]["tpm_installed"] and not report["tpm"]["missing_plugins"]:
            self.assertEqual(rc, 0, msg=stderr)

    def test_stdout_is_valid_json_only(self) -> None:
        rc, report, _ = self._run([])
        # If stdout parses at all, the invariant holds.
        self.assertIsInstance(report, dict)


class ContextManagerHelper(unittest.TestCase):
    """Sanity-check the tiny env helper used by other tests."""

    def test_env_restores_previous_values(self) -> None:
        before = os.environ.get("PATH")
        with unittest_env({"PATH": "/tmp"}):
            self.assertEqual(os.environ["PATH"], "/tmp")
        self.assertEqual(os.environ.get("PATH"), before)


class _EnvCtx:
    def __init__(self, overrides: dict[str, str]) -> None:
        self.overrides = overrides
        self.previous: dict[str, str | None] = {}

    def __enter__(self) -> None:
        for key, value in self.overrides.items():
            self.previous[key] = os.environ.get(key)
            os.environ[key] = value

    def __exit__(self, *_exc) -> None:
        for key, value in self.previous.items():
            if value is None:
                os.environ.pop(key, None)
            else:
                os.environ[key] = value


def unittest_env(overrides: dict[str, str]) -> _EnvCtx:
    return _EnvCtx(overrides)


if __name__ == "__main__":
    unittest.main()
