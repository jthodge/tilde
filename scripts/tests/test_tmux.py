"""Check tmux config on a private socket, with a stub plugin and no user shell."""
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[2]
TMUX = shutil.which("tmux")


@unittest.skipUnless(TMUX and Path("/opt/homebrew/bin/fish").exists(), "macOS tmux and fish required")
class TmuxTest(unittest.TestCase):
    def test_plugins_cannot_override_shell_and_clipboard_is_scoped(self):
        with tempfile.TemporaryDirectory(prefix="tilde-tmux-") as name:
            home = Path(name)
            socket = home / "socket"
            config = home / ".tmux.conf"
            config.write_bytes((ROOT / "tmux/.tmux.conf").read_bytes())
            plugin = home / ".tmux/plugins/tpm/tpm"
            plugin.parent.mkdir(parents=True)
            plugin.write_text('#!/bin/sh\n"$TMUX_BINARY" set-option -g default-command "legacy-shell"\n')
            plugin.chmod(0o755)
            env = dict(os.environ, HOME=name, SHELL="/bin/sh", TMUX_BINARY=TMUX)
            env.pop("TMUX", None)
            env["XDG_CONFIG_HOME"] = str(home / ".config")
            command = [TMUX, "-S", str(socket)]
            def run(*args):
                return subprocess.run(command + list(args), env=env, text=True,
                                      capture_output=True, timeout=10, check=True).stdout.strip()
            try:
                run("-f", str(config), "new-session", "-d", "-s", "test", "/bin/sleep 60")
                self.assertEqual(run("show-options", "-gv", "default-shell"), "/opt/homebrew/bin/fish")
                self.assertEqual(run("show-options", "-gv", "default-command"), "")
                self.assertEqual(run("show-environment", "-g", "XDG_CONFIG_HOME"),
                                 "XDG_CONFIG_HOME=" + str(home / ".config"))
                features = run("show-options", "-gv", "terminal-features")
                self.assertIn("tmux*:clipboard", features)
                self.assertIn("alacritty*:clipboard", features)
                self.assertNotIn(",*:clipboard", features)
                run("source-file", str(config))
                self.assertEqual(run("show-options", "-gv", "terminal-features"), features)
                self.assertEqual(run("show-options", "-gv", "default-command"), "")
            finally:
                subprocess.run(command + ["kill-server"], env=env, capture_output=True, timeout=10)


if __name__ == "__main__":
    unittest.main()
