"""Tests for scripts/check-home-generation."""
from __future__ import annotations

import json
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest

REPO = Path(__file__).resolve().parents[2]
CHECK_GENERATION = REPO / "scripts" / "check-home-generation"


def _git(cwd: Path, *args: str) -> None:
    env = dict(os.environ, GIT_CONFIG_GLOBAL="/dev/null",
               GIT_CONFIG_SYSTEM="/dev/null",
               GIT_AUTHOR_NAME="t", GIT_AUTHOR_EMAIL="t@example.invalid",
               GIT_COMMITTER_NAME="t", GIT_COMMITTER_EMAIL="t@example.invalid")
    subprocess.run(["git", *args], cwd=str(cwd), env=env, check=True,
                   capture_output=True, text=True)


class HomeGenerationCheckTest(unittest.TestCase):
    def setUp(self) -> None:
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name) / "repo"
        self.root.mkdir()
        (self.root / "scripts").mkdir()
        shutil.copy2(CHECK_GENERATION, self.root / "scripts/check-home-generation")
        (self.root / "scripts/check-home-generation").chmod(0o755)
        _git(self.root, "init", "-q", "-b", "main")
        self.generation = Path(self.temp.name) / "generation"
        self.home_files = self.generation / "home-files"
        self.home_files.mkdir(parents=True)

    def _write(self, rel: str, text: str = "x\n") -> Path:
        path = self.root / rel
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(text)
        return path

    def _seed(self, files: dict[str, str]) -> None:
        for rel, text in files.items():
            self._write(rel, text)
        _git(self.root, "add", "-A")

    def _link(self, rel: str, source: Path) -> None:
        target = self.home_files / rel
        target.parent.mkdir(parents=True, exist_ok=True)
        os.symlink(source, target)

    def _run(self) -> subprocess.CompletedProcess:
        return subprocess.run(
            [str(self.root / "scripts/check-home-generation"), str(self.generation)],
            cwd=self.root,
            text=True,
            capture_output=True,
        )

    def _report(self, result: subprocess.CompletedProcess) -> dict:
        return json.loads(result.stdout)

    def test_generation_covers_direct_files_and_directory_bridges(self) -> None:
        self._seed({
            ".stow-packages": "# none\n",
            ".home-manager-packages": "fish\nagents\nclaude\n",
            "fish/.config/fish/config.fish": "# fish\n",
            "agents/.agents/skills/example/SKILL.md": "# skill\n",
            "claude/.claude/CLAUDE.md": "# claude\n",
            "claude/.claude/settings.json": "{\"model\":\"template\"}\n",
        })
        self._link(".config/fish", self.root / "fish/.config/fish")
        self._link(".agents", self.root / "agents/.agents")
        self._link(".claude/CLAUDE.md", self.root / "claude/.claude/CLAUDE.md")
        result = self._run()
        self.assertEqual(result.returncode, 0, msg=result.stderr + result.stdout)
        report = self._report(result)
        self.assertTrue(report["ok"])
        # claude/.claude/settings.json is seed-only, so it is not a HM file.
        self.assertEqual(report["checked"], 3)

    def test_missing_generation_entry_is_reported(self) -> None:
        self._seed({
            ".stow-packages": "# none\n",
            ".home-manager-packages": "fish\n",
            "fish/.config/fish/config.fish": "# fish\n",
        })
        result = self._run()
        self.assertEqual(result.returncode, 1)
        report = self._report(result)
        self.assertEqual(report["problems"][0]["kind"], "missing")
        self.assertEqual(report["problems"][0]["path"], "fish/.config/fish/config.fish")

    def test_active_stow_manifest_and_undeclared_package_fail(self) -> None:
        self._seed({
            ".stow-packages": "fish\n",
            ".home-manager-packages": "agents\n",
            "fish/.config/fish/config.fish": "# fish\n",
            "agents/.agents/README.md": "# agents\n",
            "orphan/file": "x\n",
        })
        self._link(".agents", self.root / "agents/.agents")
        result = self._run()
        self.assertEqual(result.returncode, 1)
        kinds = {problem["kind"] for problem in self._report(result)["problems"]}
        self.assertIn("stow-active", kinds)
        self.assertIn("undeclared", kinds)


if __name__ == "__main__":
    unittest.main()
