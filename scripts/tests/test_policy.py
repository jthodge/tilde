"""Regression checks for previously contradictory command instructions."""
from pathlib import Path
import unittest

ROOT = Path(__file__).resolve().parents[2]
COMMANDS = ROOT / "claude/.claude/commands"


class PolicyTest(unittest.TestCase):
    def test_repository_tool_access_is_not_publication_authority(self):
        text = (ROOT / "AGENTS.md").read_text()
        self.assertNotIn("using the command IS", text)
        self.assertIn("Tool access is not publishing authorization", text)

    def test_pr_ceremony_stops_on_signing_failure(self):
        text = (COMMANDS / "pr-ceremony.md").read_text()
        self.assertNotIn("Local commits may need", text)
        self.assertIn("If signing fails, stop", text)
        self.assertIn("Never use `--no-gpg-sign`", text)

    def test_review_is_not_publish_permission(self):
        text = (COMMANDS / "pr-ceremony.md").read_text()
        self.assertNotIn("implies `--auto`", text)
        self.assertNotIn("CI IS the approval", text)
        self.assertIn("explicit force-push authorization", text)
        self.assertIn("readiness, not permission", text)

    def test_commit_push_requires_verification_and_branch_check(self):
        text = (COMMANDS / "commit-push.md").read_text()
        for required in ("default branch", "Stop on detached HEAD", "Stop on failure",
                         "lint, typecheck, and tests", "1Password SSH agent",
                         "Leave unrelated", "without force"):
            self.assertIn(required, text)
        self.assertNotIn("Do not use any other tools", text)


if __name__ == "__main__":
    unittest.main()
