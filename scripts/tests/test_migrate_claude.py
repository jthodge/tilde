"""Reviewable Claude settings migration: preview/apply/restore invariants.

Every case runs against a temporary HOME. The live ~/.claude/settings.json
is never read, written, or stat()ed. Backups live under the same temporary
HOME. No auth or session material exists in the fixture.
"""
import contextlib
import errno
import hashlib
import importlib.machinery
import importlib.util
import io
import json
import os
from pathlib import Path
import stat
import subprocess
import sys
import tempfile
import unittest
from unittest.mock import patch

ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts/migrate-claude-settings"
loader = importlib.machinery.SourceFileLoader("migrate_claude", str(SCRIPT))
spec = importlib.util.spec_from_loader(loader.name, loader)
assert spec is not None
module = importlib.util.module_from_spec(spec)
loader.exec_module(module)


def _sha(data):
    return hashlib.sha256(data).hexdigest()


class MigrateClaudeTest(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory(prefix="tilde-migrate-")
        self.addCleanup(self.tmp.cleanup)
        self.home = Path(self.tmp.name)
        self.target = self.home / ".claude/settings.json"
        self.state = self.home / ".local/state/tilde/claude-migrations"

    def _seed(self, payload_bytes, mode=0o600):
        self.target.parent.mkdir(parents=True, exist_ok=True, mode=0o700)
        self.target.write_bytes(payload_bytes)
        self.target.chmod(mode)

    def _seed_json(self, obj):
        self._seed((json.dumps(obj) + "\n").encode("utf-8"))

    def _apply(self, expected):
        return module.apply(self.home, expected)

    def _preview(self):
        return module.preview(self.home)

    def _run_cli(self, *argv):
        return subprocess.run(
            [sys.executable, str(SCRIPT), "--home", str(self.home), *argv],
            capture_output=True, text=True)

    def test_migration_never_publishes_output_larger_than_its_read_limit(self):
        payload = b'{"model":"opus[1m]"}'
        self._seed(payload)
        with patch.object(module, "_MAX_BYTES", 64):
            with self.assertRaises(module.MigrationError):
                self._apply(_sha(payload))
        self.assertEqual(self.target.read_bytes(), payload)
        self.assertFalse(self.state.exists())

    def test_empty_or_relative_home_never_falls_back_to_live_home(self):
        for argument in ("", ".", "/"):
            with patch.object(module.Path, "home", side_effect=AssertionError("live HOME")):
                with self.assertRaises(module.MigrationError):
                    module._resolve_home(argument)

    def test_exponent_overflow_and_deep_json_fail_without_traceback(self):
        for payload in (b'{"private":1e99999}',
                        b'{"private":' + b'[' * 1500 + b'0' + b']' * 1500 + b'}'):
            self._seed(payload)
            run = self._run_cli()
            self.assertEqual(run.returncode, 2)
            self.assertEqual(run.stdout, "")
            self.assertNotIn("Traceback", run.stderr)
            self.assertNotIn("private", run.stderr)

    def test_metadata_actions_must_match_computed_plan(self):
        _, report = self._prime_backup()
        path = self.state / report["backup_id"] / "metadata.json"
        metadata = json.loads(path.read_bytes())
        for value in (None, 123, {}, [], ["effort:retain"]):
            metadata["actions"] = value
            path.write_text(json.dumps(metadata))
            run = self._run_cli("--restore", report["backup_id"])
            self.assertEqual(run.returncode, 2)
            self.assertNotIn("Traceback", run.stderr)
            self.assertIn("restore:metadata-mismatch", run.stderr)

    # --- planning ---------------------------------------------------------

    def test_model_missing_sets_sonnet_and_adds_effort(self):
        self._seed_json({})
        report = self._preview()
        self.assertIn("model:set-sonnet", report["actions"])
        self.assertIn("effort:add-high", report["actions"])
        self.assertTrue(report["changed"])

    def test_model_opus_1m_replaced(self):
        self._seed_json({"model": "opus[1m]", "modelSettings": {"claude-sonnet-5": {"effortLevel": "medium"}}})
        report = self._preview()
        self.assertEqual(report["actions"], ["model:set-sonnet", "effort:retain"])

    def test_bare_opus_and_custom_model_retained(self):
        for value in ("opus", "sonnet", "my-team/custom"):
            self._seed_json({"model": value,
                             "modelSettings": {"claude-sonnet-5": {"effortLevel": "high"}}})
            report = self._preview()
            self.assertIn("model:retain", report["actions"])
            self.assertFalse(report["changed"])

    def test_custom_effort_and_unknown_fields_preserved(self):
        payload = {
            "model": "sonnet",
            "customTop": {"keep": True},
            "modelSettings": {
                "claude-sonnet-5": {"effortLevel": "medium", "customNested": [1, 2]},
                "claude-opus-5": {"effortLevel": "high"},
            },
        }
        self._seed_json(payload)
        report = self._preview()
        self.assertFalse(report["changed"])
        self.assertEqual(report["current_sha256"], report["post_sha256"])
        # Apply is a semantic no-op and must not change bytes.
        before = self.target.read_bytes()
        result = self._apply(_sha(before))
        self.assertFalse(result["changed"])
        self.assertEqual(self.target.read_bytes(), before)

    def test_missing_effort_added_but_existing_kept(self):
        self._seed_json({"model": "sonnet",
                         "modelSettings": {"claude-sonnet-5": {"customNested": True}}})
        report = self._preview()
        self.assertIn("effort:add-high", report["actions"])

    def test_expected_sha256_matches_current_in_preview(self):
        # The apply gate documents pasting `expected_sha256` from the
        # preview, so it must equal `current_sha256`.
        self._seed_json({"model": "opus[1m]"})
        report = self._preview()
        self.assertEqual(report["expected_sha256"], report["current_sha256"])

    # --- rejections -------------------------------------------------------

    def test_duplicate_keys_rejected(self):
        self._seed(b'{"model":"sonnet","model":"opus"}')
        with self.assertRaises(module.MigrationError) as ctx:
            self._preview()
        self.assertEqual(str(ctx.exception), "settings:duplicate-key")

    def test_explicit_null_model_settings_rejected_not_overwritten(self):
        self._seed_json({"model": "sonnet", "modelSettings": None})
        with self.assertRaises(module.MigrationError) as ctx:
            self._preview()
        self.assertEqual(str(ctx.exception), "settings:modelSettings-null")

    def test_explicit_null_sonnet_rejected_not_overwritten(self):
        self._seed_json({"model": "sonnet",
                         "modelSettings": {"claude-sonnet-5": None}})
        with self.assertRaises(module.MigrationError) as ctx:
            self._preview()
        self.assertEqual(str(ctx.exception), "settings:sonnet-null")

    def test_explicit_null_effort_level_rejected(self):
        self._seed_json({"model": "sonnet",
                         "modelSettings": {"claude-sonnet-5": {"effortLevel": None}}})
        with self.assertRaises(module.MigrationError) as ctx:
            self._preview()
        self.assertEqual(str(ctx.exception), "settings:effortLevel-null")

    def test_explicit_null_model_rejected(self):
        self._seed_json({"model": None})
        with self.assertRaises(module.MigrationError) as ctx:
            self._preview()
        self.assertEqual(str(ctx.exception), "settings:model-null")

    def test_non_object_model_settings_rejected(self):
        self._seed_json({"model": "sonnet", "modelSettings": []})
        with self.assertRaises(module.MigrationError) as ctx:
            self._preview()
        self.assertEqual(str(ctx.exception), "settings:modelSettings-not-object")

    def test_non_object_sonnet_rejected(self):
        self._seed_json({"model": "sonnet", "modelSettings": {"claude-sonnet-5": "high"}})
        with self.assertRaises(module.MigrationError) as ctx:
            self._preview()
        self.assertEqual(str(ctx.exception), "settings:sonnet-not-object")

    def test_nonfinite_numbers_rejected(self):
        for constant in (b'{"model":"sonnet","x":NaN}',
                         b'{"model":"sonnet","x":Infinity}',
                         b'{"model":"sonnet","x":-Infinity}'):
            self._seed(constant)
            with self.assertRaises(module.MigrationError) as ctx:
                self._preview()
            self.assertEqual(str(ctx.exception), "settings:nonfinite-number")

    def test_malformed_json_error_has_no_content_leak(self):
        secret = b'{"model":"__NEVER_LEAK__" invalid'
        self._seed(secret)
        with self.assertRaises(module.MigrationError) as ctx:
            self._preview()
        self.assertEqual(str(ctx.exception), "settings:malformed-json")
        self.assertNotIn("NEVER_LEAK", str(ctx.exception))

    def test_symlink_target_rejected(self):
        real = self.home / "elsewhere.json"
        real.write_bytes(b'{"model":"sonnet"}\n')
        self.target.parent.mkdir(parents=True, exist_ok=True)
        os.symlink(real, self.target)
        with self.assertRaises(module.MigrationError) as ctx:
            self._preview()
        # O_NOFOLLOW returns ELOOP -> "is-symlink".
        self.assertEqual(str(ctx.exception), "settings:is-symlink")

    def test_directory_target_rejected(self):
        self.target.parent.mkdir(parents=True, exist_ok=True)
        self.target.mkdir()
        with self.assertRaises(module.MigrationError) as ctx:
            self._preview()
        self.assertEqual(str(ctx.exception), "settings:not-regular-file")

    def test_oversize_target_rejected(self):
        self._seed(b"{" + b" " * (module._MAX_BYTES + 16) + b"}")
        with self.assertRaises(module.MigrationError) as ctx:
            self._preview()
        self.assertEqual(str(ctx.exception), "settings:too-large")

    def test_semantic_preserve_not_verbatim(self):
        # Unusual whitespace in the input is normalized by re-render.
        # The doc claim is *semantic* preservation, not byte-identity.
        self._seed(b'{"model":"opus[1m]","extra":\t\t"keep"}\n')
        report = self._preview()
        self.assertTrue(report["changed"])
        # After apply, the extra key is preserved semantically but the
        # bytes are re-rendered with a fixed indent.
        result = self._apply(report["expected_sha256"])
        rendered = self.target.read_bytes()
        parsed = json.loads(rendered)
        self.assertEqual(parsed["extra"], "keep")
        self.assertNotIn(b"\t", rendered)

    # --- preview does not write ------------------------------------------

    def test_preview_writes_nothing(self):
        payload = {"model": "opus[1m]"}
        self._seed_json(payload)
        before = self.target.read_bytes()
        stat_before = self.target.stat()
        self._preview()
        self.assertEqual(self.target.read_bytes(), before)
        self.assertEqual(self.target.stat().st_ino, stat_before.st_ino)
        self.assertFalse(self.state.exists())

    # --- apply gate -------------------------------------------------------

    def test_apply_requires_matching_expected_hash(self):
        self._seed_json({"model": "opus[1m]"})
        with self.assertRaises(module.MigrationError) as ctx:
            self._apply("0" * 64)
        self.assertEqual(str(ctx.exception), "apply:stale-preview")
        # File untouched.
        self.assertIn(b"opus[1m]", self.target.read_bytes())

    def test_apply_missing_expected_hash_via_cli(self):
        self._seed_json({"model": "opus[1m]"})
        completed = self._run_cli("--apply")
        self.assertEqual(completed.returncode, 2)
        self.assertIn("apply:missing-expected-sha256", completed.stderr)

    def test_apply_creates_private_backup_with_integrity_metadata(self):
        payload = json.dumps({"model": "opus[1m]"}).encode("utf-8") + b"\n"
        self._seed(payload)
        report = self._apply(_sha(payload))
        backup_dir = self.state / report["backup_id"]
        self.assertTrue(backup_dir.is_dir())
        self.assertEqual(backup_dir.stat().st_mode & 0o777, 0o700)
        for name in ("original-bytes", "metadata.json"):
            entry = backup_dir / name
            self.assertEqual(entry.stat().st_mode & 0o777, 0o600)
            self.assertFalse(entry.is_symlink())
        original = (backup_dir / "original-bytes").read_bytes()
        self.assertEqual(original, payload)
        metadata = json.loads((backup_dir / "metadata.json").read_text())
        self.assertEqual(metadata["migration"], "sonnet-defaults-v1")
        self.assertEqual(metadata["original_sha256"], _sha(payload))
        self.assertEqual(metadata["post_sha256"], _sha(self.target.read_bytes()))

    def test_apply_publishes_expected_bytes(self):
        payload = json.dumps({"model": "opus[1m]"}).encode("utf-8") + b"\n"
        self._seed(payload)
        report = self._apply(_sha(payload))
        result = json.loads(self.target.read_bytes())
        self.assertEqual(result["model"], "sonnet")
        self.assertEqual(result["modelSettings"]["claude-sonnet-5"]["effortLevel"], "high")
        self.assertEqual(_sha(self.target.read_bytes()), report["post_sha256"])
        self.assertEqual(self.target.stat().st_mode & 0o777, 0o600)

    def test_apply_target_changed_between_hash_and_replace(self):
        payload = json.dumps({"model": "opus[1m]"}).encode("utf-8") + b"\n"
        self._seed(payload)
        expected = _sha(payload)
        real_read = module._read_regular_bounded
        calls = {"n": 0}

        def racing(path, label):
            calls["n"] += 1
            if calls["n"] == 2:  # inside publish recheck, race the file.
                self.target.write_bytes(payload + b"\n")
            return real_read(path, label)

        with patch.object(module, "_read_regular_bounded", racing):
            with self.assertRaises(module.MigrationError) as ctx:
                self._apply(expected)
        self.assertEqual(str(ctx.exception), "publish:target-changed")

    def test_publication_failure_leaves_original_and_backup_intact(self):
        payload = json.dumps({"model": "opus[1m]"}).encode("utf-8") + b"\n"
        self._seed(payload)

        def raiser(*args, **kwargs):
            raise OSError("simulated publication failure")

        with patch.object(module.os, "replace", raiser):
            with self.assertRaises(module.MigrationError) as ctx:
                self._apply(_sha(payload))
        self.assertEqual(str(ctx.exception), "publish:replace-failed")
        self.assertEqual(self.target.read_bytes(), payload)
        backups = list(self.state.iterdir())
        self.assertEqual(len(backups), 1)
        self.assertEqual((backups[0] / "original-bytes").read_bytes(), payload)

    def test_publication_failure_via_cli_reports_fixed_label(self):
        # Simulate an OSError inside apply through the CLI entrypoint so
        # `main`'s catch-all is exercised end-to-end.
        payload = json.dumps({"model": "opus[1m]"}).encode("utf-8") + b"\n"
        self._seed(payload)
        expected = _sha(payload)
        # Force an OSError deep in publish by overriding chmod inside a
        # small helper module; the runner injects it via a stub script.
        helper = self.home / "run_stub.py"
        helper.write_text(
            "import importlib.machinery, importlib.util, sys\n"
            "loader = importlib.machinery.SourceFileLoader('m', %r)\n"
            "spec = importlib.util.spec_from_loader('m', loader)\n"
            "m = importlib.util.module_from_spec(spec); loader.exec_module(m)\n"
            "orig = m.os.replace\n"
            "def boom(*a, **k):\n"
            "    raise OSError('simulated')\n"
            "m.os.replace = boom\n"
            "sys.exit(m.main([%r, %r, %r, %r, %r]))\n"
            % (str(SCRIPT), "--home", str(self.home),
               "--apply", "--expected-sha256", expected)
        )
        completed = subprocess.run(
            [sys.executable, str(helper)], capture_output=True, text=True)
        self.assertEqual(completed.returncode, 2)
        self.assertIn("migrate-claude-settings: publish:replace-failed",
                      completed.stderr)
        # No raw traceback marker, no file path leak.
        self.assertNotIn("Traceback", completed.stderr)
        self.assertNotIn(str(self.target), completed.stderr)

    def test_chmod_failure_fails_closed(self):
        payload = json.dumps({"model": "opus[1m]"}).encode("utf-8") + b"\n"
        self._seed(payload)
        real_chmod = module.os.chmod
        calls = {"n": 0}

        def flaky(path, mode):
            calls["n"] += 1
            if calls["n"] == 1:  # first chmod is on state root
                raise OSError("simulated chmod failure")
            return real_chmod(path, mode)

        with patch.object(module.os, "chmod", flaky):
            with self.assertRaises(module.MigrationError) as ctx:
                self._apply(_sha(payload))
        self.assertEqual(str(ctx.exception), "backup:chmod-failed")
        # Target untouched.
        self.assertEqual(self.target.read_bytes(), payload)

    # --- restore ----------------------------------------------------------

    def _prime_backup(self):
        payload = json.dumps({"model": "opus[1m]"}).encode("utf-8") + b"\n"
        self._seed(payload)
        report = self._apply(_sha(payload))
        return payload, report

    def test_restore_ok_when_target_unchanged(self):
        original, report = self._prime_backup()
        current = self.target.read_bytes()
        preview = module.restore_preview(self.home, report["backup_id"])
        self.assertEqual(preview["current_sha256"], _sha(current))
        self.assertEqual(preview["expected_sha256"], _sha(current))
        result = module.restore_apply(self.home, report["backup_id"],
                                      preview["expected_sha256"])
        self.assertTrue(result["restored"])
        self.assertEqual(self.target.read_bytes(), original)

    def test_restore_refuses_when_current_diverged(self):
        _, report = self._prime_backup()
        self.target.write_bytes(b'{"model":"someone-else"}\n')
        with self.assertRaises(module.MigrationError) as ctx:
            module.restore_preview(self.home, report["backup_id"])
        self.assertEqual(str(ctx.exception), "restore:target-diverged")

    def test_restore_stale_hash_refused(self):
        _, report = self._prime_backup()
        with self.assertRaises(module.MigrationError) as ctx:
            module.restore_apply(self.home, report["backup_id"], "0" * 64)
        self.assertEqual(str(ctx.exception), "apply:stale-preview")

    def test_restore_invalid_id_does_not_echo(self):
        completed = self._run_cli("--restore", "../../etc/passwd")
        self.assertEqual(completed.returncode, 2)
        self.assertNotIn("passwd", completed.stderr)
        self.assertNotIn("../", completed.stderr)
        self.assertIn("restore:invalid-id", completed.stderr)

    def test_restore_id_with_trailing_newline_refused(self):
        # `re.match` used to allow trailing input; `fullmatch` refuses.
        _, report = self._prime_backup()
        with self.assertRaises(module.MigrationError) as ctx:
            module.restore_preview(self.home, report["backup_id"] + "\n")
        self.assertEqual(str(ctx.exception), "restore:invalid-id")

    def test_restore_state_root_symlink_refused(self):
        _, report = self._prime_backup()
        # Replace the state root with a symlink pointing at itself's
        # real content; the loader must refuse.
        real = self.home / "elsewhere-state"
        os.rename(self.state, real)
        os.symlink(real, self.state)
        with self.assertRaises(module.MigrationError) as ctx:
            module.restore_preview(self.home, report["backup_id"])
        self.assertEqual(str(ctx.exception), "state:is-symlink")

    def test_restore_symlink_backup_files_refused(self):
        _, report = self._prime_backup()
        backup_dir = self.state / report["backup_id"]
        real = backup_dir / "original-bytes"
        detour = self.home / "detour"
        detour.write_bytes(b"junk")
        real.unlink()
        os.symlink(detour, real)
        with self.assertRaises(module.MigrationError) as ctx:
            module.restore_preview(self.home, report["backup_id"])
        self.assertEqual(str(ctx.exception), "backup:is-symlink")

    def test_restore_backup_corruption_detected(self):
        _, report = self._prime_backup()
        (self.state / report["backup_id"] / "original-bytes").write_bytes(b"tampered\n")
        with self.assertRaises(module.MigrationError) as ctx:
            module.restore_preview(self.home, report["backup_id"])
        self.assertEqual(str(ctx.exception), "restore:backup-corrupted")

    def test_restore_metadata_backup_id_mismatch_detected(self):
        _, report = self._prime_backup()
        metadata_path = self.state / report["backup_id"] / "metadata.json"
        metadata = json.loads(metadata_path.read_text())
        metadata["backup_id"] = "sonnet-defaults-v1-0000000000000"
        metadata_path.write_text(json.dumps(metadata))
        with self.assertRaises(module.MigrationError) as ctx:
            module.restore_preview(self.home, report["backup_id"])
        self.assertEqual(str(ctx.exception), "restore:metadata-mismatch")

    def test_restore_never_echoes_metadata_secrets(self):
        # A doctored metadata file with a bogus post_sha256 must fail
        # rather than being trusted, and the returned report must
        # contain only recomputed hashes.
        _, report = self._prime_backup()
        metadata_path = self.state / report["backup_id"] / "metadata.json"
        metadata = json.loads(metadata_path.read_text())
        secret_value = "SECRET_VALUE_THAT_MUST_NOT_ECHO"
        metadata["post_sha256"] = "0" * 64
        metadata["extra"] = secret_value
        metadata_path.write_text(json.dumps(metadata))
        with self.assertRaises(module.MigrationError) as ctx:
            module.restore_preview(self.home, report["backup_id"])
        self.assertEqual(str(ctx.exception), "restore:backup-corrupted")
        # Restore a good metadata but inject an extra field; ensure the
        # returned report never surfaces it.
        metadata["post_sha256"] = report["post_sha256"]
        metadata_path.write_text(json.dumps(metadata))
        preview = module.restore_preview(self.home, report["backup_id"])
        self.assertNotIn("extra", preview)
        self.assertNotIn(secret_value, json.dumps(preview))

    def test_restore_metadata_bad_action_rejected(self):
        _, report = self._prime_backup()
        metadata_path = self.state / report["backup_id"] / "metadata.json"
        metadata = json.loads(metadata_path.read_text())
        metadata["actions"] = ["evil:do-bad-thing"]
        metadata_path.write_text(json.dumps(metadata))
        with self.assertRaises(module.MigrationError) as ctx:
            module.restore_preview(self.home, report["backup_id"])
        self.assertEqual(str(ctx.exception), "restore:metadata-mismatch")

    def test_restore_missing_target_ok(self):
        _, report = self._prime_backup()
        self.target.unlink()
        preview = module.restore_preview(self.home, report["backup_id"])
        self.assertIsNone(preview["current_sha256"])
        self.assertFalse(preview["target_present"])
        # The dedicated `expected_sha256` field is what the docs tell
        # the operator to paste back, so it must be a real hex digest.
        self.assertEqual(preview["expected_sha256"], preview["original_sha256"])
        self.assertEqual(len(preview["expected_sha256"]), 64)
        result = module.restore_apply(self.home, report["backup_id"],
                                      preview["expected_sha256"])
        self.assertTrue(result["restored"])

    def test_restore_missing_uses_link_never_clobbers_raced_in_file(self):
        # If Claude Code races back to life between the preview and the
        # apply and creates the file with unrelated content, the
        # missing-target flow must refuse and preserve that content.
        _, report = self._prime_backup()
        self.target.unlink()
        preview = module.restore_preview(self.home, report["backup_id"])
        raced_bytes = b'{"model":"raced-in-value"}\n'
        real_link = module.os.link

        def racing_link(src, dst):
            # Simulate the app writing the file between the temp write
            # and the atomic publish.
            self.target.write_bytes(raced_bytes)
            return real_link(src, dst)

        with patch.object(module.os, "link", racing_link):
            with self.assertRaises(module.MigrationError) as ctx:
                module.restore_apply(self.home, report["backup_id"],
                                     preview["expected_sha256"])
        self.assertEqual(str(ctx.exception), "publish:target-appeared")
        # The raced-in file is preserved byte-for-byte.
        self.assertEqual(self.target.read_bytes(), raced_bytes)

    def test_restore_existing_target_rechecks_before_replace(self):
        # Race the file after the temp write but before os.replace so
        # the publication boundary observes different bytes.
        original, report = self._prime_backup()
        current = self.target.read_bytes()
        real_read = module._read_regular_bounded
        calls = {"n": 0}
        raced_bytes = current + b"//raced\n"

        def racing(path, label):
            calls["n"] += 1
            # The first read is the top-level target read in
            # restore_apply; the second happens inside _publish_atomic.
            if calls["n"] == 2:
                self.target.write_bytes(raced_bytes)
            return real_read(path, label)

        with patch.object(module, "_read_regular_bounded", racing):
            with self.assertRaises(module.MigrationError) as ctx:
                module.restore_apply(self.home, report["backup_id"], _sha(current))
        # Race is detected as target-changed OR target-diverged
        # depending on which invariant the recheck catches first.
        self.assertIn(str(ctx.exception),
                      ("publish:target-changed", "restore:target-diverged"))
        # The raced-in bytes are preserved.
        self.assertEqual(self.target.read_bytes(), raced_bytes)

    # --- no-force ---------------------------------------------------------

    def test_no_force_flag_exists(self):
        completed = subprocess.run(
            [sys.executable, str(SCRIPT), "--force"], capture_output=True, text=True)
        self.assertNotEqual(completed.returncode, 0)
        self.assertIn("unrecognized arguments", completed.stderr)
        source = SCRIPT.read_text()
        self.assertNotIn("--force", source)


if __name__ == "__main__":
    unittest.main()
