"""Exercise the Fish-native prompt with temporary HOME and Git state only."""
import os
from pathlib import Path
import re
import shutil
import subprocess
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[2]
SOURCE = ROOT / "fish/.config/fish"
FISH = shutil.which("fish")
GIT = shutil.which("git")


@unittest.skipUnless(FISH and GIT, "fish and git required")
class FishPromptTest(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(prefix="tilde-prompt-")
        self.addCleanup(self.temp.cleanup)
        self.home = Path(self.temp.name).resolve()
        self.functions = self.home / ".config/fish/functions"
        self.functions.mkdir(parents=True)
        for name in ("fish_prompt.fish", "set_pwd_color.fish"):
            shutil.copy2(SOURCE / "functions" / name, self.functions / name)
        self.colors = self.home / ".config/fish/colors.fish"
        shutil.copy2(SOURCE / "colors.fish", self.colors)
        assert FISH is not None and GIT is not None
        self.env = {
            "HOME": str(self.home),
            "XDG_CONFIG_HOME": str(self.home / ".config"),
            "XDG_DATA_HOME": str(self.home / ".local/share"),
            "XDG_CACHE_HOME": str(self.home / ".cache"),
            "PATH": os.pathsep.join((str(Path(GIT).parent), "/usr/bin", "/bin")),
            "TERM": "xterm-256color",
            "GIT_CONFIG_NOSYSTEM": "1",
            "GIT_CONFIG_GLOBAL": os.devnull,
            "GIT_TERMINAL_PROMPT": "0",
        }

    def fish(self, snippet, extra_env=None):
        assert FISH is not None
        result = subprocess.run(
            [FISH, "--no-config", "-c",
             "set -p fish_function_path $argv[1]\nsource $argv[2]\n" + snippet,
             str(self.functions), str(self.colors)],
            cwd=self.home, env=dict(self.env, **(extra_env or {})),
            capture_output=True, text=True, timeout=15, check=True,
        )
        self.assertEqual(result.stderr, "")
        return result.stdout

    def plain_prompt(self):
        return re.sub(r"\x1b\[[0-9;]*m", "", self.fish("fish_prompt"))

    def git(self, *args):
        assert GIT is not None
        return subprocess.run(
            [GIT, *args], cwd=self.home, env=self.env,
            capture_output=True, text=True, timeout=15, check=True,
        )

    def test_non_repo_prompt_is_one_line_with_no_extra_character(self):
        self.assertEqual(self.plain_prompt(), "~ ")

    def test_local_and_ssh_directory_colors(self):
        # Inspect color intent without depending on ANSI palette encoding.
        stub = 'function set_color; printf "<%s>" $argv; end\nset_pwd_color'
        self.assertEqual(self.fish(stub), "<magenta>")
        self.assertEqual(self.fish(stub, {"SSH_CLIENT": "192.0.2.1 1234 22"}), "<blue>")
        self.assertEqual(self.fish(stub, {"SSH_CLIENT": ""}), "<magenta>")

    def test_directory_abbreviation_uses_fish_defaults(self):
        (self.home / "source/project").mkdir(parents=True)
        output = self.fish("cd source/project; fish_prompt")
        self.assertEqual(re.sub(r"\x1b\[[0-9;]*m", "", output), "~/s/project ")

    def test_git_branch_dirty_marker_and_untracked_files(self):
        # An unborn branch suffices: no commits, signing bypass, hooks or network.
        self.git("init", "-q")
        self.git("symbolic-ref", "HEAD", "refs/heads/prompt-test")
        baseline = self.plain_prompt()
        self.assertIn("prompt-test", baseline)
        self.assertNotIn("±", baseline)
        tracked = self.home / "tracked.txt"
        tracked.write_text("initial\n")
        self.assertEqual(self.plain_prompt(), baseline)  # untracked files are not shown
        self.git("add", "tracked.txt")
        tracked.write_text("modified\n")
        dirty = self.plain_prompt()
        self.assertIn("±", dirty)
        self.assertNotIn("\n", dirty)
        self.assertTrue(dirty.endswith(") "))

    def test_no_framework_or_right_prompt_is_loaded(self):
        self.assertEqual(self.fish(
            "fish_prompt >/dev/null\n"
            "functions --all | string match --entire --regex '^_?tide'; true"), "")
        self.assertEqual(self.fish(
            "functions --query fish_right_prompt; printf '%s' $status"), "1")

    def test_git_prompt_settings_match_mark(self):
        self.assertEqual(self.fish(
            "printf '%s\\n' $__fish_git_prompt_char_dirtystate "
            "$__fish_git_prompt_color_branch $__fish_git_prompt_showdirtystate"
        ), "±\nyellow\nyes\n")


class FishPromptSourceTest(unittest.TestCase):
    def test_tide_payload_and_registration_are_removed(self):
        self.assertEqual((SOURCE / "fish_plugins").read_text(), "")
        self.assertFalse((SOURCE / "functions/fish_mode_prompt.fish").exists())
        self.assertEqual([str(path) for path in SOURCE.rglob("*")
                          if "tide" in path.name.lower()], [])


if __name__ == "__main__":
    unittest.main()
