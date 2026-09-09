"""Tests for fish shell startup.

The tracked config is copied into a sandboxed `$HOME` and every tool it
integrates with (uv, op, brew, mise, zoxide, fzf, ngrok) is replaced by
a fake executable that logs its invocation. Fish is then invoked with
`--no-config` so it does not autoload from the developer's real
`~/.config/fish`, and the sandbox config is sourced explicitly.

These tests prove the invariants documented in docs/shell-startup.md:

- Non-interactive startup does not call uv, op, or brew, and does not
  emit any completion subprocess.
- __uv_autovenv never creates a base venv; it only sources an existing
  activation script.
- local.fish runs before activation and can retune UV_DEFAULT_VENV.
- An already-active VIRTUAL_ENV is preserved verbatim.

Real integrations (mise, zoxide, fzf, uv completions, ngrok) are gated
so they never execute during tests.
"""
from __future__ import annotations

import os
import shutil
import stat
import subprocess
import sys
import tempfile
import textwrap
import unittest
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]
FISH_SRC = REPO / "fish" / ".config" / "fish"
FISH_BIN = shutil.which("fish")

# Tools the fish config touches. Every one gets a fake in the sandbox
# PATH so an accidental invocation is both harmless and observable.
TRACKED_TOOLS = ("uv", "op", "brew", "mise", "zoxide", "fzf", "ngrok")


def _write_fake_exe(directory: Path, name: str, log_path: Path) -> None:
    """Write a shell stub that appends its argv to log_path and exits 0."""
    script = directory / name
    script.write_text(
        "#!/usr/bin/env bash\n"
        f'printf "%s\\n" "{name} $*" >> "{log_path}"\n'
        "exit 0\n"
    )
    script.chmod(script.stat().st_mode | stat.S_IXUSR | stat.S_IXGRP | stat.S_IXOTH)


class FishStartupTests(unittest.TestCase):
    """Sandboxed fish startup invariants."""

    @classmethod
    def setUpClass(cls) -> None:
        if FISH_BIN is None:
            raise unittest.SkipTest("fish not installed")

    def setUp(self) -> None:
        self.tmp = Path(tempfile.mkdtemp(prefix="tilde-shell-"))
        self.addCleanup(shutil.rmtree, self.tmp, ignore_errors=True)

        # Sandboxed HOME + fish config dir.
        self.home = self.tmp / "home"
        self.fish_cfg = self.home / ".config" / "fish"
        self.fish_cfg.mkdir(parents=True)
        # Copy tracked sources only, never local.fish or universal state.
        names = subprocess.check_output(
            ["git", "ls-files", "-z", "--", "fish/.config/fish"], cwd=str(REPO)
        ).decode().split("\0")
        for name in filter(None, names):
            item = REPO / name
            dst = self.fish_cfg / item.relative_to(FISH_SRC)
            dst.parent.mkdir(parents=True, exist_ok=True)
            shutil.copy2(item, dst)

        # Fake-executable PATH with a log for every invocation.
        self.bin = self.tmp / "bin"
        self.bin.mkdir()
        self.log = self.tmp / "invocations.log"
        self.log.touch()
        for tool in TRACKED_TOOLS:
            _write_fake_exe(self.bin, tool, self.log)

        # Baseline env: cleared of anything the developer's real login
        # already exported so tests are hermetic.
        self.env = {
            "HOME": str(self.home),
            "PATH": f"{self.bin}:/usr/bin:/bin",
            "TERM": "dumb",
            # Prevent fish 4.x from creating XDG dirs outside the sandbox.
            "XDG_CONFIG_HOME": str(self.home / ".config"),
            "XDG_DATA_HOME": str(self.home / ".local" / "share"),
            "XDG_CACHE_HOME": str(self.home / ".cache"),
        }

    # ---- helpers -----------------------------------------------------

    def _run_fish(self, snippet: str, extra_env: dict | None = None,
                  interactive: bool = False) -> subprocess.CompletedProcess:
        """Invoke fish --no-config and source the sandbox config, then run snippet."""
        env = dict(self.env)
        if extra_env:
            env.update(extra_env)
        # Point fish at the sandbox config dir and source config.fish
        # explicitly. --no-config prevents fish from touching the
        # developer's real ~/.config/fish.
        # `--no-config` fixes fish_function_path before we can rewrite
        # __fish_config_dir, so prepend the sandbox functions dir
        # explicitly for autoload.
        prelude = (
            f"set -gx __fish_config_dir {self.fish_cfg}\n"
            f"set -p fish_function_path {self.fish_cfg}/functions\n"
            f"source {self.fish_cfg}/config.fish\n"
        )
        args = [FISH_BIN, "--no-config"]
        if interactive:
            args.append("-i")
        args.extend(["-c", prelude + snippet])
        return subprocess.run(
            args, env=env, capture_output=True, text=True, timeout=15,
        )

    def _log_lines(self) -> list[str]:
        return [ln for ln in self.log.read_text().splitlines() if ln]

    # ---- tests -------------------------------------------------------

    def test_noninteractive_touches_no_tracked_tools(self) -> None:
        """Sourcing config non-interactively must not call uv/op/brew/mise/etc."""
        result = self._run_fish("true")
        self.assertEqual(result.returncode, 0, msg=result.stderr)
        self.assertEqual(
            self._log_lines(),
            [],
            msg=("non-interactive startup invoked tracked tools:\n"
                 + "\n".join(self._log_lines())),
        )

    def test_noninteractive_does_not_activate_existing_base(self) -> None:
        venv = self.home / ".venv/base/bin"
        venv.mkdir(parents=True)
        (venv / "activate.fish").write_text("set -gx VIRTUAL_ENV unexpected\n")
        result = self._run_fish('echo "venv=<$VIRTUAL_ENV>"')
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("venv=<>", result.stdout)

    def test_interactive_uses_only_stub_integrations(self) -> None:
        # The local phase restores fake PATH after the Homebrew phase.
        (self.fish_cfg / "local.fish").write_text(
            f"set -gx PATH {self.bin} /usr/bin /bin\n")
        venv = self.home / ".venv/base/bin"
        venv.mkdir(parents=True)
        (venv / "activate.fish").write_text("set -gx VIRTUAL_ENV interactive-marker\n")
        result = self._run_fish('echo "venv=$VIRTUAL_ENV"', interactive=True)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("venv=interactive-marker", result.stdout)
        self.assertFalse(any(line.startswith(("op ", "brew ", "uv venv")) for line in self._log_lines()))

    def test_noninteractive_prepends_homebrew_bin(self) -> None:
        """PATH gains /opt/homebrew/bin on macOS without spawning brew."""
        if sys.platform != "darwin" or not Path("/opt/homebrew/bin").exists():
            self.skipTest("Homebrew prefix not present")
        result = self._run_fish("echo $PATH")
        self.assertEqual(result.returncode, 0, msg=result.stderr)
        self.assertIn("/opt/homebrew/bin", result.stdout)
        # And still no brew subprocess.
        self.assertNotIn("brew", "\n".join(self._log_lines()))

    def test_autovenv_does_not_create_missing_venv(self) -> None:
        """__uv_autovenv must never invoke uv to create a base venv."""
        result = self._run_fish("__uv_autovenv; echo done:$status")
        self.assertEqual(result.returncode, 0, msg=result.stderr)
        self.assertIn("done:0", result.stdout)
        self.assertFalse(
            (self.home / ".venv").exists(),
            msg="__uv_autovenv created ~/.venv",
        )
        self.assertNotIn("uv", "\n".join(self._log_lines()))

    def test_autovenv_sources_existing_activation(self) -> None:
        """When the activation script exists, __uv_autovenv sources it."""
        venv = self.home / ".venv" / "base" / "bin"
        venv.mkdir(parents=True)
        (venv / "activate.fish").write_text(
            "set -gx VIRTUAL_ENV sourced-marker\n"
        )
        result = self._run_fish("__uv_autovenv; echo venv=$VIRTUAL_ENV")
        self.assertEqual(result.returncode, 0, msg=result.stderr)
        self.assertIn("venv=sourced-marker", result.stdout)
        # Still no uv invocation.
        self.assertNotIn("uv", "\n".join(self._log_lines()))

    def test_autovenv_does_not_wrap_native_prompt(self) -> None:
        """Prompt suppression is tracked, not dependent on Tide's old universal."""
        venv = self.home / ".venv/base/bin"
        venv.mkdir(parents=True)
        (venv / "activate.fish").write_text(
            'if test -z "$VIRTUAL_ENV_DISABLE_PROMPT"\n'
            '    function fish_prompt; printf wrapped; end\n'
            'end\n'
        )
        result = self._run_fish(
            "function fish_prompt; printf native; end\n"
            "__uv_autovenv\nfish_prompt"
        )
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout, "native")

    def test_autovenv_preserves_active_virtualenv(self) -> None:
        """An inherited VIRTUAL_ENV must not be overwritten."""
        # Provision an activation script that would replace VIRTUAL_ENV
        # if it ran. If preservation works, the marker stays intact.
        venv = self.home / ".venv" / "base" / "bin"
        venv.mkdir(parents=True)
        (venv / "activate.fish").write_text(
            "set -gx VIRTUAL_ENV should-not-happen\n"
        )
        result = self._run_fish(
            "echo venv=$VIRTUAL_ENV",
            extra_env={"VIRTUAL_ENV": "/existing/env"},
        )
        self.assertEqual(result.returncode, 0, msg=result.stderr)
        self.assertIn("venv=/existing/env", result.stdout)

    def test_autovenv_opt_out(self) -> None:
        """TILDE_AUTO_VENV=0 skips activation even when the venv exists."""
        venv = self.home / ".venv" / "base" / "bin"
        venv.mkdir(parents=True)
        (venv / "activate.fish").write_text(
            "set -gx VIRTUAL_ENV should-not-happen\n"
        )
        result = self._run_fish(
            "echo \"venv=<$VIRTUAL_ENV>\"",
            extra_env={"TILDE_AUTO_VENV": "0"},
        )
        self.assertEqual(result.returncode, 0, msg=result.stderr)
        self.assertIn("venv=<>", result.stdout)

    def test_local_fish_runs_before_activation(self) -> None:
        """local.fish must run before __uv_autovenv so it can retune the target venv."""
        # Provision a 'custom' venv but NOT 'base'. If local.fish ran
        # after activation (or not at all), autovenv would look for
        # 'base' and find nothing.
        (self.fish_cfg / "local.fish").write_text(
            "set -gx UV_DEFAULT_VENV custom\n"
        )
        custom = self.home / ".venv" / "custom" / "bin"
        custom.mkdir(parents=True)
        (custom / "activate.fish").write_text(
            "set -gx VIRTUAL_ENV custom-marker\n"
        )
        result = self._run_fish("__uv_autovenv; echo venv=$VIRTUAL_ENV")
        self.assertEqual(result.returncode, 0, msg=result.stderr)
        self.assertIn("venv=custom-marker", result.stdout)


if __name__ == "__main__":
    unittest.main()
