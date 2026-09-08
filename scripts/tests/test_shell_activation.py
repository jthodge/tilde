"""Robustness tests for zsh, bash, and fish activation helpers.

The tests use a sandbox `$HOME` that contains only copies of the tracked
config files: no cargo env, no git-completion, no git-prompt, no
NVM script, no 1Password credentials, no installed integrations. A
fresh checkout can lack these integrations. Copied files redirect hardcoded
Homebrew prefixes to a missing sandbox path, so installed host integrations
cannot become reachable through PATH or absolute source paths. Startup must
succeed, and the `activate` helper in each shell must validate its
argument *before* deactivating the currently-active virtualenv.

These tests never source the developer's real HOME. Each subprocess
runs with an empty environment plus a hermetic PATH pointing at
`/usr/bin:/bin`, so the only login state is what the tracked file sets.
"""
from __future__ import annotations

import os
import shutil
import subprocess
import tempfile
import textwrap
import unittest
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]

ZSHENV = REPO / "zsh" / ".zshenv"
ZPROFILE = REPO / "zsh" / ".zprofile"
ZSHRC = REPO / "zsh" / ".zshrc"
BASH_PROFILE = REPO / "bash" / ".bash_profile"
FISH_ACTIVATE = REPO / "fish" / ".config" / "fish" / "functions" / "activate.fish"

ZSH_BIN = shutil.which("zsh")
BASH_BIN = shutil.which("bash")
FISH_BIN = shutil.which("fish")


class ShellSandbox:
    """A hermetic HOME plus a minimal PATH. No real integrations reachable."""

    def __init__(self) -> None:
        self.tmp = Path(tempfile.mkdtemp(prefix="tilde-shell-round2-"))
        self.home = self.tmp / "home"
        self.home.mkdir()
        # Baseline PATH: system dirs only, so tracked configs cannot
        # accidentally shell out to Homebrew, uv, cargo, or op.
        self.env = {
            "HOME": str(self.home),
            "PATH": "/usr/bin:/bin",
            "TERM": "dumb",
            "SHELL": "/bin/sh",
            "XDG_CONFIG_HOME": str(self.home / ".config"),
            "XDG_CACHE_HOME": str(self.home / ".cache"),
            "XDG_DATA_HOME": str(self.home / ".local" / "share"),
        }

    def cleanup(self) -> None:
        shutil.rmtree(self.tmp, ignore_errors=True)


class ZshFreshHomeTests(unittest.TestCase):
    """`zsh -l` in a HOME without cargo, brew, or op still boots."""

    @classmethod
    def setUpClass(cls) -> None:
        if ZSH_BIN is None:
            raise unittest.SkipTest("zsh not installed")

    def setUp(self) -> None:
        self.sandbox = ShellSandbox()
        self.addCleanup(self.sandbox.cleanup)
        # Copy the tracked zsh files into $ZDOTDIR so login files load
        # from the sandbox and never the developer's real ~/.zshrc.
        self.zdotdir = self.sandbox.home / ".zsh-dot"
        self.zdotdir.mkdir()
        for src, name in (
            (ZSHENV, ".zshenv"),
            (ZPROFILE, ".zprofile"),
            (ZSHRC, ".zshrc"),
        ):
            (self.zdotdir / name).write_text(src.read_text().replace(
                "/opt/homebrew", str(self.sandbox.home / "absent-brew")))
        self.env = dict(self.sandbox.env, ZDOTDIR=str(self.zdotdir))

    def _run(self, snippet: str, extra_env: dict[str, str] | None = None
             ) -> subprocess.CompletedProcess[str]:
        env = dict(self.env)
        if extra_env:
            env.update(extra_env)
        # `-i` in addition to `-l` so .zshrc is sourced too (that is where
        # `activate` is defined). `--no-globalrcs` skips /etc so we don't
        # inherit a host-wide zsh config that could pollute the sandbox.
        assert ZSH_BIN is not None
        return subprocess.run(
            [ZSH_BIN, "--no-globalrcs", "-l", "-i", "-c", snippet],
            env=env, capture_output=True, text=True, timeout=15,
        )

    def test_login_succeeds_without_cargo_env(self) -> None:
        """.zshenv sourcing must not fail when ~/.cargo/env is absent."""
        self.assertFalse((self.sandbox.home / ".cargo" / "env").exists())
        result = self._run("print done")
        self.assertEqual(result.returncode, 0, msg=result.stderr)
        self.assertIn("done", result.stdout)
        self.assertEqual(result.stderr, "")

    def test_zprofile_never_spawns_brew(self) -> None:
        """.zprofile mirrors brew shellenv without invoking brew."""
        # A fake `brew` that would log if called. Because PATH does not
        # include this fake in the login, .zprofile must not resolve brew
        # at all; but even if it did, the log stays empty.
        fake_bin = self.sandbox.home / "fake-bin"
        fake_bin.mkdir()
        log = self.sandbox.home / "brew-invocations.log"
        brew_stub = fake_bin / "brew"
        brew_stub.write_text(
            f'#!/usr/bin/env bash\nprintf "brew %s\\n" "$*" >> "{log}"\nexit 0\n'
        )
        brew_stub.chmod(0o755)
        # Prepend the fake path so brew *would* be reachable if login shells
        # tried to run it.
        env = dict(self.env, PATH=f"{fake_bin}:{self.env['PATH']}")
        assert ZSH_BIN is not None
        result = subprocess.run(
            [ZSH_BIN, "--no-globalrcs", "-l", "-i", "-c", "print PATH=$PATH"],
            env=env, capture_output=True, text=True, timeout=15,
        )
        self.assertEqual(result.returncode, 0, msg=result.stderr)
        self.assertFalse(
            log.exists() and log.read_text(),
            msg=f"zsh login invoked brew: {log.read_text() if log.exists() else ''!r}",
        )

    def test_activate_missing_arg_is_a_typed_error(self) -> None:
        result = self._run("activate")
        # Bash-style exit for usage errors: non-zero, message on stderr,
        # no attempt to touch VIRTUAL_ENV.
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("usage: activate", result.stderr)

    def test_activate_missing_target_preserves_virtualenv(self) -> None:
        """Bad path must not deactivate an already-active venv."""
        # Pretend an existing venv is active. zsh's `deactivate` is a
        # function defined by the venv's activate script; we substitute a
        # sentinel that flips a variable so we can detect a spurious call.
        result = self._run(
            textwrap.dedent(
                """\
                deactivate() { print "SPURIOUS_DEACTIVATE"; }
                activate /nowhere-tilde-round2 || true
                print venv=$VIRTUAL_ENV
                """
            ),
            extra_env={"VIRTUAL_ENV": "/existing/env"},
        )
        self.assertEqual(result.returncode, 0, msg=result.stderr)
        self.assertNotIn("SPURIOUS_DEACTIVATE", result.stdout,
                         msg="activate deactivated before validating the target")
        self.assertIn("venv=/existing/env", result.stdout)
        self.assertIn("no activation script", result.stderr)

    def test_activate_sources_readable_target(self) -> None:
        """A readable activate script is sourced and marks the shell."""
        venv = self.sandbox.home / ".venv" / "sample" / "bin"
        venv.mkdir(parents=True)
        (venv / "activate").write_text("export VIRTUAL_ENV=activated-marker\n")
        result = self._run(f'activate "{venv.parent}" && print venv=$VIRTUAL_ENV')
        self.assertEqual(result.returncode, 0, msg=result.stderr)
        self.assertIn("venv=activated-marker", result.stdout)


class BashFreshHomeTests(unittest.TestCase):
    """`bash -l` in a HOME without git-prompt or NVM still boots."""

    @classmethod
    def setUpClass(cls) -> None:
        if BASH_BIN is None:
            raise unittest.SkipTest("bash not installed")

    def setUp(self) -> None:
        self.sandbox = ShellSandbox()
        self.addCleanup(self.sandbox.cleanup)
        # bash reads ~/.bash_profile on login. Drop the tracked one in.
        (self.sandbox.home / ".bash_profile").write_text(
            BASH_PROFILE.read_text().replace("/opt/homebrew", str(self.sandbox.home / "absent-brew")))
        self.env = self.sandbox.env

    def _run(self, snippet: str, extra_env: dict[str, str] | None = None
             ) -> subprocess.CompletedProcess[str]:
        env = dict(self.env)
        if extra_env:
            env.update(extra_env)
        # Source only the copied login profile, not /etc/profile or other rc files.
        assert BASH_BIN is not None
        return subprocess.run(
            [BASH_BIN, "--noprofile", "--norc", "-c", '. "$HOME/.bash_profile";\n' + snippet],
            env=env, capture_output=True, text=True, timeout=15,
        )

    def test_login_succeeds_without_git_helpers(self) -> None:
        """Missing ~/.git-completion.bash and ~/.git-prompt.sh must not fail login."""
        self.assertFalse((self.sandbox.home / ".git-completion.bash").exists())
        self.assertFalse((self.sandbox.home / ".git-prompt.sh").exists())
        result = self._run("printf DONE\\\\n")
        self.assertEqual(result.returncode, 0, msg=result.stderr)
        self.assertIn("DONE", result.stdout)
        self.assertEqual(result.stderr, "")
        # And no unbound-function error surfaced for __git_ps1.
        self.assertNotIn("__git_ps1", result.stderr)
        self.assertNotIn("command not found", result.stderr)

    def test_prompt_falls_back_without_git_ps1(self) -> None:
        """PS1 must not embed __git_ps1 when git-prompt.sh is absent."""
        result = self._run('printf "PS1=%s\\n" "$PS1"')
        self.assertEqual(result.returncode, 0, msg=result.stderr)
        self.assertNotIn("__git_ps1", result.stdout)

    def test_prompt_uses_git_ps1_when_available(self) -> None:
        """Drop in a stub git-prompt.sh; PS1 gains __git_ps1."""
        stub = self.sandbox.home / ".git-prompt.sh"
        stub.write_text('__git_ps1() { printf " (branch)"; }\n')
        result = self._run('printf "PS1=%s\\n" "$PS1"')
        self.assertEqual(result.returncode, 0, msg=result.stderr)
        self.assertIn("__git_ps1", result.stdout)

    def test_nvm_bootstrap_is_gone(self) -> None:
        """The stale NVM bootstrap must not resurface. Volta owns Node."""
        body = BASH_PROFILE.read_text()
        self.assertNotIn("NVM_DIR", body)
        self.assertNotIn("nvm.sh", body)

    def test_login_without_homebrew_still_prepends_bin(self) -> None:
        """~/bin stays highest even when /opt/homebrew is absent."""
        result = self._run('printf "PATH=%s\\n" "$PATH"')
        self.assertEqual(result.returncode, 0, msg=result.stderr)
        # $HOME/bin must appear before /usr/bin. Use the resolved HOME
        # so a macOS realpath (/private/var/...) still matches.
        home_bin = str(Path(self.env["HOME"]).resolve() / "bin")
        path_value = next(
            line[len("PATH="):]
            for line in result.stdout.splitlines()
            if line.startswith("PATH=")
        )
        parts = path_value.split(":")
        # Normalize because on macOS realpath prepends /private.
        idx_home = next((i for i, p in enumerate(parts) if Path(p).resolve() == Path(home_bin)),
                        None)
        idx_usr = parts.index("/usr/bin") if "/usr/bin" in parts else None
        self.assertIsNotNone(idx_home, msg=f"$HOME/bin missing from PATH: {parts}")
        self.assertIsNotNone(idx_usr, msg=f"/usr/bin missing from PATH: {parts}")
        # mypy: both indices are ints now.
        assert idx_home is not None and idx_usr is not None
        self.assertLess(idx_home, idx_usr,
                        msg=f"$HOME/bin should precede /usr/bin: {parts}")


class FishActivateTests(unittest.TestCase):
    """`activate` in fish must validate before deactivating."""

    @classmethod
    def setUpClass(cls) -> None:
        if FISH_BIN is None:
            raise unittest.SkipTest("fish not installed")

    def setUp(self) -> None:
        self.sandbox = ShellSandbox()
        self.addCleanup(self.sandbox.cleanup)
        self.fn_dir = self.sandbox.home / ".config" / "fish" / "functions"
        self.fn_dir.mkdir(parents=True)
        shutil.copy2(FISH_ACTIVATE, self.fn_dir / "activate.fish")
        self.env = self.sandbox.env

    def _run(self, snippet: str, extra_env: dict[str, str] | None = None
             ) -> subprocess.CompletedProcess[str]:
        env = dict(self.env)
        if extra_env:
            env.update(extra_env)
        # --no-config so fish does not source the developer's real
        # ~/.config/fish. `set -p fish_function_path` makes the sandbox
        # functions directory autoloadable.
        prelude = f"set -p fish_function_path {self.fn_dir}\n"
        assert FISH_BIN is not None
        return subprocess.run(
            [FISH_BIN, "--no-config", "-c", prelude + snippet],
            env=env, capture_output=True, text=True, timeout=15,
        )

    def test_missing_arg_returns_typed_error(self) -> None:
        result = self._run("activate; echo status=$status")
        self.assertIn("usage: activate", result.stderr)
        self.assertIn("status=2", result.stdout)

    def test_missing_target_preserves_virtualenv(self) -> None:
        result = self._run(
            "function deactivate; echo SPURIOUS_DEACTIVATE; end\n"
            "activate /nowhere-tilde-round2; echo venv=$VIRTUAL_ENV",
            extra_env={"VIRTUAL_ENV": "/existing/env"},
        )
        self.assertNotIn("SPURIOUS_DEACTIVATE", result.stdout)
        self.assertIn("venv=/existing/env", result.stdout)
        self.assertIn("no activation script", result.stderr)

    def test_readable_target_is_sourced(self) -> None:
        venv = self.sandbox.home / ".venv" / "sample" / "bin"
        venv.mkdir(parents=True)
        (venv / "activate.fish").write_text(
            "set -gx VIRTUAL_ENV activated-marker\n"
        )
        result = self._run(
            f'activate "{venv.parent}"; echo venv=$VIRTUAL_ENV'
        )
        self.assertEqual(result.returncode, 0, msg=result.stderr)
        self.assertIn("venv=activated-marker", result.stdout)


if __name__ == "__main__":
    unittest.main()
