"""Tests for scripts/ddr and scripts/dwr.

The scripts under test are safety-wrapped wrappers around `gh`, `jq`, and
`fzf`. To avoid any real network I/O these tests shim `gh` and `fzf` with
tiny shell scripts placed on a temporary PATH; `jq` is still needed and
must be installed on the host. If `jq` is absent, the affected tests are
skipped.

The shims are driven by environment variables:

    FAKE_GH_LIST_FIXTURE  path to a JSON file returned for the list call
                          (`gh api --paginate /repos/.../{deployments,
                          actions/runs}`)
    FAKE_GH_LIST_EXIT     override exit code for the list call (default 0)
    FAKE_GH_LOG           append every gh invocation (one line per call)
    FAKE_GH_MUTATE_EXIT   override exit code for POST/DELETE calls
    FAKE_FZF_OUTPUT       what `fzf` writes to stdout (empty = cancel)
    FAKE_FZF_EXIT         override exit code for fzf (default 0 unless
                          FAKE_FZF_OUTPUT is empty, in which case 130)

Nothing here talks to the network. If a test ever tried to, PATH would
not resolve to a real gh anyway.
"""

from __future__ import annotations

import json
import os
import shutil
import stat
import subprocess
import tempfile
import textwrap
import unittest
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[2]
DDR = REPO_ROOT / "scripts" / "ddr"
DWR = REPO_ROOT / "scripts" / "dwr"

JQ_AVAILABLE = shutil.which("jq") is not None


def _write_exec(path: Path, body: str) -> None:
    path.write_text(body)
    mode = path.stat().st_mode
    path.chmod(mode | stat.S_IXUSR | stat.S_IXGRP | stat.S_IXOTH)


class Harness:
    """Per-test scratch dir with shimmed gh + fzf on PATH."""

    def __init__(self) -> None:
        self.tmp = Path(tempfile.mkdtemp(prefix="cleanup-test-"))
        self.bin = self.tmp / "bin"
        self.bin.mkdir()
        self.gh_log = self.tmp / "gh.log"
        self.list_fixture = self.tmp / "list.json"
        self.fzf_output = self.tmp / "fzf.out"
        self.list_fixture.write_text("[]")
        self.fzf_output.write_text("")

        # gh shim: distinguishes list (no -X flag) from mutate (-X POST/DELETE).
        _write_exec(
            self.bin / "gh",
            textwrap.dedent(
                r"""#!/usr/bin/env bash
                # Log every invocation, one line per call, args joined by \t.
                {
                  printf '%s' "gh"
                  for a in "$@"; do printf '\t%s' "$a"; done
                  printf '\n'
                } >> "${FAKE_GH_LOG:-/dev/null}"

                # Is this a mutation?
                is_mutate=0
                for a in "$@"; do
                  if [ "$a" = "-X" ]; then
                    is_mutate=1
                    break
                  fi
                done

                if [ "$is_mutate" = "1" ]; then
                  exit "${FAKE_GH_MUTATE_EXIT:-0}"
                fi

                # List call: emit the fixture then honor the exit override.
                if [ -n "${FAKE_GH_LIST_FIXTURE:-}" ] && [ -f "$FAKE_GH_LIST_FIXTURE" ]; then
                  cat "$FAKE_GH_LIST_FIXTURE"
                fi
                exit "${FAKE_GH_LIST_EXIT:-0}"
                """
            ).lstrip(),
        )

        # fzf shim: reads stdin (must consume to avoid SIGPIPE upstream)
        # then emits FAKE_FZF_OUTPUT and exits with FAKE_FZF_EXIT.
        _write_exec(
            self.bin / "fzf",
            textwrap.dedent(
                r"""#!/usr/bin/env bash
                cat >/dev/null
                out="${FAKE_FZF_OUTPUT:-}"
                if [ -n "$out" ] && [ -f "$out" ]; then
                  cat "$out"
                fi
                exit "${FAKE_FZF_EXIT:-0}"
                """
            ).lstrip(),
        )

    def cleanup(self) -> None:
        shutil.rmtree(self.tmp, ignore_errors=True)

    def env(self, **overrides: str) -> dict:
        base = os.environ.copy()
        # Keep jq reachable but drop anything that could leak to a real gh.
        for k in ("GITHUB_TOKEN", "GH_TOKEN", "GH_CONFIG_DIR"):
            base.pop(k, None)
        base["PATH"] = f"{self.bin}:{base.get('PATH', '')}"
        base["FAKE_GH_LOG"] = str(self.gh_log)
        base["FAKE_GH_LIST_FIXTURE"] = str(self.list_fixture)
        base["FAKE_FZF_OUTPUT"] = str(self.fzf_output)
        base.update(overrides)
        return base

    def set_list(self, obj) -> None:
        self.list_fixture.write_text(json.dumps(obj))

    def set_list_raw(self, raw: str) -> None:
        self.list_fixture.write_text(raw)

    def set_fzf(self, text: str) -> None:
        self.fzf_output.write_text(text)

    def run(self, script: Path, *args: str, env_extra=None, stdin: str = ""):
        env = self.env(**(env_extra or {}))
        return subprocess.run(
            [str(script), *args],
            input=stdin,
            capture_output=True,
            text=True,
            env=env,
            timeout=30,
        )

    def gh_calls(self) -> list[str]:
        if not self.gh_log.exists():
            return []
        return [ln for ln in self.gh_log.read_text().splitlines() if ln]

    def mutating_calls(self) -> list[str]:
        return [ln for ln in self.gh_calls() if "\t-X\t" in ln]


class CleanupTestBase(unittest.TestCase):
    def setUp(self) -> None:
        self.h = Harness()

    def tearDown(self) -> None:
        self.h.cleanup()


class SyntaxTests(unittest.TestCase):
    """Every script parses under bash -n."""

    def _check(self, path: Path) -> None:
        r = subprocess.run(["bash", "-n", str(path)], capture_output=True, text=True)
        self.assertEqual(r.returncode, 0, msg=r.stderr)

    def test_ddr_parses(self) -> None:
        self._check(DDR)

    def test_dwr_parses(self) -> None:
        self._check(DWR)


class HelpTests(CleanupTestBase):
    def test_ddr_help_exits_zero_and_prints_usage(self) -> None:
        r = self.h.run(DDR, "--help")
        self.assertEqual(r.returncode, 0)
        self.assertIn("Usage:", r.stdout)
        self.assertEqual(self.h.gh_calls(), [])  # no network

    def test_dwr_help_short_flag(self) -> None:
        r = self.h.run(DWR, "-h")
        self.assertEqual(r.returncode, 0)
        self.assertIn("Usage:", r.stdout)
        self.assertEqual(self.h.gh_calls(), [])


class RepoValidationTests(CleanupTestBase):
    def test_ddr_missing_repo_no_tty_errors(self) -> None:
        r = self.h.run(DDR)  # stdin is a pipe (not tty)
        self.assertEqual(r.returncode, 2)
        self.assertIn("repo required", r.stderr)
        self.assertEqual(self.h.gh_calls(), [])

    def test_ddr_invalid_repo(self) -> None:
        r = self.h.run(DDR, "not-a-repo")
        self.assertEqual(r.returncode, 2)
        self.assertIn("invalid repo", r.stderr)
        self.assertEqual(self.h.gh_calls(), [])

    def test_dwr_unknown_flag(self) -> None:
        r = self.h.run(DWR, "--bogus")
        self.assertEqual(r.returncode, 2)
        self.assertIn("unknown option", r.stderr)
        self.assertEqual(self.h.gh_calls(), [])


@unittest.skipUnless(JQ_AVAILABLE, "jq not installed")
class DryRunTests(CleanupTestBase):
    def test_ddr_dry_run_makes_no_mutations(self) -> None:
        self.h.set_list([
            {
                "id": 111,
                "created_at": "2024-01-02T03:04:05Z",
                "environment": "prod",
                "description": "d1",
                "ref": "main",
            },
            {
                "id": 222,
                "created_at": "2024-02-02T03:04:05Z",
                "environment": "prod",
                "description": None,
                "ref": "main",
            },
        ])
        # fzf "selects" both rows in TSV form (id\tcreated\tenv\tdesc\tref)
        self.h.set_fzf(
            "111\t2024-01-02 03:04:05 \tprod\td1\tmain\n"
            "222\t2024-02-02 03:04:05 \tprod\tno description\tmain\n"
        )
        r = self.h.run(DDR, "--dry-run", "octo/hello")
        self.assertEqual(r.returncode, 0, msg=r.stderr)
        self.assertIn("SKIP\t111", r.stdout)
        self.assertIn("SKIP\t222", r.stdout)
        self.assertEqual(self.h.mutating_calls(), [],
                         msg=f"mutations leaked: {self.h.mutating_calls()}")

    def test_dwr_dry_run_makes_no_mutations(self) -> None:
        self.h.set_list({
            "workflow_runs": [
                {
                    "id": 900,
                    "conclusion": "success",
                    "status": "completed",
                    "created_at": "2024-05-01T00:00:00Z",
                    "event": "push",
                    "name": "CI",
                },
            ]
        })
        self.h.set_fzf("GOOD\t2024-05-01 00:00:00 \t900\tpush\tCI\n")
        r = self.h.run(DWR, "--dry-run", "--yes", "octo/hello")
        self.assertEqual(r.returncode, 0, msg=r.stderr)
        self.assertIn("SKIP\t900", r.stdout)
        self.assertEqual(self.h.mutating_calls(), [])


@unittest.skipUnless(JQ_AVAILABLE, "jq not installed")
class YesMutationTests(CleanupTestBase):
    def test_ddr_yes_marks_inactive_then_deletes(self) -> None:
        self.h.set_list([
            {
                "id": 111,
                "created_at": "2024-01-02T03:04:05Z",
                "environment": "prod",
                "description": "d1",
                "ref": "main",
            },
        ])
        self.h.set_fzf("111\t2024-01-02 03:04:05 \tprod\td1\tmain\n")
        r = self.h.run(DDR, "--yes", "octo/hello")
        self.assertEqual(r.returncode, 0, msg=r.stderr)
        self.assertIn("OK\t111", r.stdout)

        mutates = self.h.mutating_calls()
        self.assertEqual(
            len(mutates), 2, msg=f"expected 2 mutations (POST then DELETE): {mutates}"
        )
        # First mutation is POST inactive status.
        self.assertIn("POST", mutates[0])
        self.assertIn("/repos/octo/hello/deployments/111/statuses", mutates[0])
        self.assertIn("state=inactive", mutates[0])
        # Second is DELETE of the deployment record itself.
        self.assertIn("DELETE", mutates[1])
        self.assertIn("/repos/octo/hello/deployments/111", mutates[1])
        # And the DELETE URL must not be the statuses subpath.
        self.assertNotIn("/statuses", mutates[1].split("/repos/octo/hello/deployments/111")[1])

    def test_dwr_yes_deletes_run(self) -> None:
        self.h.set_list({
            "workflow_runs": [
                {
                    "id": 900,
                    "conclusion": "failure",
                    "status": "completed",
                    "created_at": "2024-05-01T00:00:00Z",
                    "event": "push",
                    "name": "CI",
                }
            ]
        })
        self.h.set_fzf("FAIL\t2024-05-01 00:00:00 \t900\tpush\tCI\n")
        r = self.h.run(DWR, "--yes", "octo/hello")
        self.assertEqual(r.returncode, 0, msg=r.stderr)
        self.assertIn("OK\t900", r.stdout)
        mutates = self.h.mutating_calls()
        self.assertEqual(len(mutates), 1)
        self.assertIn("DELETE", mutates[0])
        self.assertIn("/repos/octo/hello/actions/runs/900", mutates[0])


@unittest.skipUnless(JQ_AVAILABLE, "jq not installed")
class NullConclusionTests(CleanupTestBase):
    """A run whose conclusion is null must not blow up jq; status is used."""

    def test_dwr_null_conclusion_falls_back_to_status(self) -> None:
        self.h.set_list({
            "workflow_runs": [
                {
                    "id": 42,
                    "conclusion": None,
                    "status": "in_progress",
                    "created_at": "2024-05-01T00:00:00Z",
                    "event": "push",
                    "name": "CI",
                }
            ]
        })
        # Simulate the user selecting the parsed line. fzf shim doesn't
        # actually parse; we just need SOME selection to force delete.
        self.h.set_fzf("PROG\t2024-05-01 00:00:00 \t42\tpush\tCI\n")
        r = self.h.run(DWR, "--dry-run", "--yes", "octo/hello")
        self.assertEqual(r.returncode, 0, msg=r.stderr)
        # No parse error surfaced.
        self.assertNotIn("failed to parse", r.stderr)
        self.assertIn("SKIP\t42", r.stdout)


@unittest.skipUnless(JQ_AVAILABLE, "jq not installed")
class FailureModeTests(CleanupTestBase):
    def prepare_run(self):
        self.h.set_list({"workflow_runs": [{"id": 900, "conclusion": "success",
            "created_at": "2024-05-01T00:00:00Z", "event": "push", "name": "CI"}]})
        self.h.set_fzf("GOOD\t2024-05-01 00:00:00 \t900\tpush\tCI\n")

    def test_delete_failure_returns_failure(self):
        self.prepare_run()
        r = self.h.run(DWR, "--yes", "octo/hello",
                       env_extra={"FAKE_GH_MUTATE_EXIT": "22"})
        self.assertNotEqual(r.returncode, 0)
        self.assertIn("BAD\t900", r.stdout)

    def test_selection_cannot_add_another_id(self):
        self.prepare_run()
        self.h.set_fzf("GOOD\t2024-05-01 00:00:00 \t901\tpush\tCI\n")
        r = self.h.run(DWR, "--yes", "octo/hello")
        self.assertNotEqual(r.returncode, 0)
        self.assertEqual(self.h.mutating_calls(), [])

    def test_double_dash_rejects_extra_arguments(self):
        r = self.h.run(DWR, "--", "octo/hello", "other/repo")
        self.assertEqual(r.returncode, 2)
        self.assertEqual(self.h.gh_calls(), [])

    def test_ddr_list_failure_aborts_before_mutation(self) -> None:
        r = self.h.run(
            DDR, "--yes", "octo/hello",
            env_extra={"FAKE_GH_LIST_EXIT": "22"},
        )
        self.assertNotEqual(r.returncode, 0)
        self.assertIn("failed to list", r.stderr)
        self.assertEqual(self.h.mutating_calls(), [])

    def test_dwr_parse_failure_aborts_before_mutation(self) -> None:
        # Feed jq garbage.
        self.h.set_list_raw("not-json-at-all")
        # Even with a "selection" queued up, no mutation must fire.
        self.h.set_fzf("GOOD\t2024\t900\tpush\tCI\n")
        r = self.h.run(DWR, "--yes", "octo/hello")
        self.assertNotEqual(r.returncode, 0)
        self.assertIn("failed to parse", r.stderr)
        self.assertEqual(self.h.mutating_calls(), [])

    def test_dwr_empty_selection_is_noop(self) -> None:
        self.h.set_list({
            "workflow_runs": [
                {
                    "id": 900,
                    "conclusion": "success",
                    "status": "completed",
                    "created_at": "2024-05-01T00:00:00Z",
                    "event": "push",
                    "name": "CI",
                }
            ]
        })
        self.h.set_fzf("")  # user selected nothing
        r = self.h.run(
            DWR, "--yes", "octo/hello",
            env_extra={"FAKE_FZF_EXIT": "130"},  # fzf's cancel code
        )
        self.assertEqual(r.returncode, 0, msg=r.stderr)
        self.assertIn("no selection", r.stderr)
        self.assertEqual(self.h.mutating_calls(), [])

    def test_ddr_empty_list_is_noop(self) -> None:
        self.h.set_list([])
        r = self.h.run(DDR, "--yes", "octo/hello")
        self.assertEqual(r.returncode, 0, msg=r.stderr)
        self.assertIn("no deployment records found", r.stderr)
        self.assertEqual(self.h.mutating_calls(), [])


@unittest.skipUnless(JQ_AVAILABLE, "jq not installed")
class ConfirmationTests(CleanupTestBase):
    def test_dwr_without_yes_and_no_tty_refuses(self) -> None:
        """Without --yes and with no /dev/tty available for read the script
        must refuse. We can't easily hide /dev/tty on macOS, so this asserts
        the softer contract: default confirmation MUST NOT be silently
        skipped. If a tty exists the script will block on read from
        /dev/tty; we detect that by imposing a short timeout via
        subprocess.
        """
        self.h.set_list({
            "workflow_runs": [
                {
                    "id": 900,
                    "conclusion": "success",
                    "status": "completed",
                    "created_at": "2024-05-01T00:00:00Z",
                    "event": "push",
                    "name": "CI",
                }
            ]
        })
        self.h.set_fzf("GOOD\t2024-05-01 00:00:00 \t900\tpush\tCI\n")

        env = self.h.env()
        try:
            r = subprocess.run(
                [str(DWR), "octo/hello"],
                capture_output=True,
                text=True,
                env=env,
                stdin=subprocess.DEVNULL,
                start_new_session=True,
                timeout=3,
            )
        except subprocess.TimeoutExpired:
            # Blocked on /dev/tty read - acceptable: it did NOT silently
            # proceed to delete. Nothing should have mutated yet.
            self.assertEqual(self.h.mutating_calls(), [])
            return

        # If the process returned, it must have refused (no tty case) or
        # asked and been declined. Either way: no mutations.
        self.assertNotEqual(r.returncode, 0)
        self.assertEqual(self.h.mutating_calls(), [])


if __name__ == "__main__":  # pragma: no cover
    unittest.main()
