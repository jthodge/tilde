"""Focused unit tests for scripts/check-capabilities.

The script is loaded as a module so we can exercise its helpers with
crafted subprocess fakes. Only the failure paths that the real
end-to-end probes cannot reliably trigger are covered here: malformed
LSP framing, timeouts, missing binaries, wrong formatter output,
graceful shutdown, and result-set aggregation.

Real probes against installed compilers/formatters/servers happen when
the developer or CI runs `python3 scripts/check-capabilities`; those
are intentionally not duplicated in this suite so tests stay cheap and
network-free.
"""
from __future__ import annotations

import importlib.util
import io
import json
import os
import stat
import subprocess
import sys
import tempfile
import textwrap
import time
import unittest
from unittest.mock import patch
from importlib.machinery import SourceFileLoader
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]
SCRIPT = REPO / "scripts" / "check-capabilities"


def load_module():
    loader = SourceFileLoader("check_capabilities", str(SCRIPT))
    spec = importlib.util.spec_from_loader("check_capabilities", loader)
    assert spec is not None
    module = importlib.util.module_from_spec(spec)
    loader.exec_module(module)
    return module


CC = load_module()


def _write_exe(path: Path, body: str) -> Path:
    path.write_text(body)
    path.chmod(path.stat().st_mode | stat.S_IXUSR | stat.S_IXGRP | stat.S_IXOTH)
    return path


class CleanEnvTests(unittest.TestCase):
    def test_scrubs_untrusted_variables_and_sets_temp_home(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            env = CC.clean_env(tmp)
        self.assertEqual(env["HOME"], tmp)
        self.assertEqual(env["TMPDIR"], tmp)
        self.assertEqual(env["NO_COLOR"], "1")
        self.assertEqual(env["LC_ALL"], "C")
        self.assertTrue(env["XDG_CONFIG_HOME"].startswith(tmp))
        self.assertNotIn("PYTHONPATH", env)
        self.assertNotIn("NODE_OPTIONS", env)
        self.assertNotIn("GOPATH", env)

    def test_extra_env_overrides_defaults(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            env = CC.clean_env(tmp, extra={"HOME": "/somewhere/else", "X": "y"})
        self.assertEqual(env["HOME"], "/somewhere/else")
        self.assertEqual(env["X"], "y")

    def test_preserves_volta_home_when_present(self) -> None:
        with tempfile.TemporaryDirectory() as fake_volta, \
                tempfile.TemporaryDirectory() as tmp:
            previous = os.environ.get("VOLTA_HOME")
            os.environ["VOLTA_HOME"] = fake_volta
            try:
                env = CC.clean_env(tmp)
            finally:
                if previous is None:
                    os.environ.pop("VOLTA_HOME", None)
                else:
                    os.environ["VOLTA_HOME"] = previous
        self.assertEqual(env["VOLTA_HOME"], fake_volta)

    def test_omits_volta_home_when_directory_absent(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            previous_home = os.environ.get("HOME")
            previous_volta = os.environ.get("VOLTA_HOME")
            os.environ["HOME"] = os.path.join(tmp, "does-not-exist")
            os.environ.pop("VOLTA_HOME", None)
            try:
                env = CC.clean_env(tmp)
            finally:
                if previous_home is None:
                    os.environ.pop("HOME", None)
                else:
                    os.environ["HOME"] = previous_home
                if previous_volta is not None:
                    os.environ["VOLTA_HOME"] = previous_volta
        self.assertNotIn("VOLTA_HOME", env)


class RunProbeTests(unittest.TestCase):
    def test_spawn_failure_reports_error(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            missing = os.path.join(tmp, "does-not-exist")
            result = CC.run_probe([missing], env=CC.clean_env(tmp), cwd=tmp, timeout=2.0)
        self.assertFalse(result["ok"])
        self.assertIsNone(result["returncode"])
        self.assertTrue(result["error"].startswith("spawn failed"))

    def test_timeout_kills_child_and_flags_timed_out(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            started = time.monotonic()
            result = CC.run_probe(
                ["/bin/sh", "-c", "sleep 5"],
                env=CC.clean_env(tmp), cwd=tmp, timeout=0.3,
            )
            elapsed = time.monotonic() - started
        self.assertTrue(result["timed_out"])
        self.assertFalse(result["ok"])
        self.assertEqual(result["error"], "timed out")
        # Timeout + kill grace + join grace stays well under 10 s.
        self.assertLess(elapsed, 10.0, msg=f"took {elapsed:.2f}s")

    def test_stdout_and_stderr_are_captured_and_bounded(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            result = CC.run_probe(
                ["/bin/sh", "-c", "printf hello; printf bye 1>&2; exit 0"],
                env=CC.clean_env(tmp), cwd=tmp, timeout=5.0,
            )
        self.assertTrue(result["ok"])
        self.assertEqual(result["stdout"], "hello")
        self.assertEqual(result["stderr"], "bye")

    def test_large_output_is_truncated_with_marker(self) -> None:
        # Produce roughly 512 KiB — the CMD_STDOUT_LIMIT is 256 KiB.
        with tempfile.TemporaryDirectory() as tmp:
            result = CC.run_probe(
                ["/bin/sh", "-c", "yes x | head -c 524288"],
                env=CC.clean_env(tmp), cwd=tmp, timeout=10.0,
            )
        self.assertTrue(result["ok"])
        self.assertIn("bytes elided", result["stdout"])

    def test_output_flood_stays_bounded_and_does_not_deadlock(self) -> None:
        # 4 MiB burst: without drain-during-execution, either memory grows
        # unbounded or the child deadlocks on pipe backpressure. The
        # drainer must keep the pipe empty and cap the retained bytes.
        with tempfile.TemporaryDirectory() as tmp:
            result = CC.run_probe(
                ["/bin/sh", "-c", "yes AAAA | head -c 4194304"],
                env=CC.clean_env(tmp), cwd=tmp, timeout=10.0,
            )
        self.assertTrue(result["ok"], msg=result["error"])
        self.assertIn("bytes elided", result["stdout"])
        # Cap + marker suffix (~50 bytes). Certainly not the full 4 MiB.
        self.assertLess(len(result["stdout"]), CC.CMD_STDOUT_LIMIT + 200)


class LspHandshakeTests(unittest.TestCase):
    """Cover LSP failure paths using a Python-scripted fake server."""

    def setUp(self) -> None:
        self.tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self.tmp.cleanup)
        self.root = Path(self.tmp.name)
        (self.root / "ws").mkdir()
        self.ws = str(self.root / "ws")

    def _fake_server(self, body: str) -> list[str]:
        """Write a Python script that impersonates an LSP server."""
        path = self.root / "server.py"
        path.write_text(textwrap.dedent(body).lstrip())
        return [sys.executable, str(path)]

    def _run(self, cmd: list[str], *, timeout: float = 3.0,
             init_options: dict | None = None) -> dict:
        env = CC.clean_env(self.tmp.name)
        return CC.lsp_initialize(
            cmd, env=env, cwd=self.ws,
            root_uri=Path(self.ws).as_uri(),
            init_options=init_options, timeout=timeout,
        )

    def test_spawn_failure_returns_error(self) -> None:
        result = self._run([os.path.join(self.tmp.name, "no-such-server")])
        self.assertFalse(result["ok"])
        self.assertTrue(result["error"].startswith("spawn failed"))
        self.assertEqual(result["capability_keys"], [])

    def test_valid_initialize_returns_capability_keys(self) -> None:
        cmd = self._fake_server("""
            import json, sys
            data = sys.stdin.buffer
            # Read one framed message (we ignore its contents).
            header = b''
            while b'\\r\\n\\r\\n' not in header:
                header += data.read(1)
            length = 0
            for line in header.decode().split('\\r\\n'):
                if line.lower().startswith('content-length:'):
                    length = int(line.split(':', 1)[1].strip())
            data.read(length)
            body = json.dumps({
                'jsonrpc': '2.0', 'id': 1,
                'result': {'capabilities': {'hoverProvider': True,
                                            'completionProvider': {}}},
            }).encode()
            sys.stdout.buffer.write(b'Content-Length: %d\\r\\n\\r\\n%s' % (len(body), body))
            sys.stdout.buffer.flush()
            # Wait for shutdown/exit then quit.
            try:
                sys.stdin.buffer.read()
            except Exception:
                pass
        """)
        result = self._run(cmd)
        self.assertTrue(result["ok"], msg=result)
        self.assertIn("hoverProvider", result["capability_keys"])
        self.assertEqual(result["capability_count"], 2)

    def test_malformed_content_length_reports_parse_error(self) -> None:
        cmd = self._fake_server("""
            import sys, time
            sys.stdin.buffer.read(0)
            sys.stdout.buffer.write(b'Content-Length: not-a-number\\r\\n\\r\\n{}')
            sys.stdout.buffer.flush()
            time.sleep(5)
        """)
        result = self._run(cmd, timeout=2.5)
        self.assertFalse(result["ok"])
        self.assertIn("Content-Length", result["error"])

    def test_invalid_json_body_reports_parse_error(self) -> None:
        cmd = self._fake_server("""
            import sys, time
            sys.stdout.buffer.write(b'Content-Length: 5\\r\\n\\r\\n{not}')
            sys.stdout.buffer.flush()
            time.sleep(5)
        """)
        result = self._run(cmd, timeout=2.5)
        self.assertFalse(result["ok"])
        self.assertIn("invalid JSON", result["error"])

    def test_capabilities_missing_from_result_fails(self) -> None:
        cmd = self._fake_server("""
            import json, sys, time
            body = json.dumps({'jsonrpc':'2.0','id':1,'result':{}}).encode()
            sys.stdout.buffer.write(b'Content-Length: %d\\r\\n\\r\\n%s' % (len(body), body))
            sys.stdout.buffer.flush()
            time.sleep(5)
        """)
        result = self._run(cmd, timeout=2.5)
        self.assertFalse(result["ok"])
        self.assertIn("capabilities", result["error"])

    def test_server_error_response_is_propagated(self) -> None:
        cmd = self._fake_server("""
            import json, sys, time
            body = json.dumps({
                'jsonrpc':'2.0','id':1,
                'error':{'code':-32601,'message':'method not found'},
            }).encode()
            sys.stdout.buffer.write(b'Content-Length: %d\\r\\n\\r\\n%s' % (len(body), body))
            sys.stdout.buffer.flush()
            time.sleep(5)
        """)
        result = self._run(cmd, timeout=2.5)
        self.assertFalse(result["ok"])
        self.assertIn("initialize returned error", result["error"])

    def test_server_exits_before_reply_is_reported(self) -> None:
        cmd = self._fake_server("""
            import sys
            sys.exit(3)
        """)
        result = self._run(cmd, timeout=3.0)
        self.assertFalse(result["ok"])
        self.assertIn("server exited", result["error"])

    def test_silent_server_times_out_and_is_killed(self) -> None:
        cmd = self._fake_server("""
            import sys, time
            # Read the initialize request then hang without replying.
            data = sys.stdin.buffer
            header = b''
            while b'\\r\\n\\r\\n' not in header:
                header += data.read(1)
            length = 0
            for line in header.decode().split('\\r\\n'):
                if line.lower().startswith('content-length:'):
                    length = int(line.split(':', 1)[1].strip())
            data.read(length)
            time.sleep(30)
        """)
        started = time.monotonic()
        result = self._run(cmd, timeout=0.5)
        elapsed = time.monotonic() - started
        self.assertFalse(result["ok"])
        self.assertIn("no initialize response", result["error"])
        # Timeout budget + termination grace (~4s in the worst case).
        self.assertLess(elapsed, 8.0, msg=f"handshake took {elapsed:.2f}s")


class InterpreterResolutionTests(unittest.TestCase):
    def test_reports_base_venv_when_present(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            venv = Path(tmp) / ".venv" / "base" / "bin"
            venv.mkdir(parents=True)
            _write_exe(venv / "python3", "#!/usr/bin/env bash\nexit 0\n")
            previous = os.environ.get("HOME")
            os.environ["HOME"] = tmp
            try:
                path, source = CC.resolve_python_interpreter()
            finally:
                if previous is None:
                    os.environ.pop("HOME", None)
                else:
                    os.environ["HOME"] = previous
        self.assertEqual(path, str(venv / "python3"))
        self.assertIn("base venv", source)

    def test_falls_back_to_sys_executable_and_names_the_source(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            previous = os.environ.get("HOME")
            os.environ["HOME"] = tmp
            try:
                path, source = CC.resolve_python_interpreter()
            finally:
                if previous is None:
                    os.environ.pop("HOME", None)
                else:
                    os.environ["HOME"] = previous
        self.assertEqual(path, sys.executable)
        self.assertIn("sys.executable", source)


class ReportAndCliTests(unittest.TestCase):
    def test_report_aggregates_required_and_optional_failures(self) -> None:
        probes = [
            {"name": "a", "required": True, "ok": True, "detail": {}},
            {"name": "b", "required": True, "ok": False, "detail": {}},
            {"name": "c", "required": False, "ok": False, "detail": {}},
        ]
        report = CC.build_report(probes)
        self.assertFalse(report["ok"])
        self.assertEqual(report["missing_required"], ["b"])
        self.assertEqual(report["missing_optional"], ["c"])

    def test_summary_marks_probe_states_distinctly(self) -> None:
        probes = [
            {"name": "a", "required": True, "ok": True, "detail": {}},
            {"name": "b", "required": True, "ok": False, "detail": {}},
            {"name": "c", "required": False, "ok": False, "detail": {}},
        ]
        report = CC.build_report(probes)
        summary = CC.render_summary(report)
        self.assertIn("OK   a", summary)
        self.assertIn("FAIL b", summary)
        self.assertIn("MISS c (optional)", summary)
        self.assertIn("verdict: PROBLEMS FOUND", summary)

    def test_summary_flag_suppresses_json(self) -> None:
        # Drive main() via a synthetic argv that runs no probes but
        # still exits 0 / emits summary lines to stderr.
        stdout_bak, stderr_bak = sys.stdout, sys.stderr
        sys.stdout = io.StringIO()
        sys.stderr = io.StringIO()
        try:
            with patch.object(CC, "probe_python", return_value=[]):
                rc = CC.main(["--only", "python", "--summary"])
        finally:
            captured_out = sys.stdout.getvalue()
            captured_err = sys.stderr.getvalue()
            sys.stdout, sys.stderr = stdout_bak, stderr_bak
        # Regardless of pass/fail, --summary must not emit JSON on stdout.
        self.assertEqual(captured_out, "")
        self.assertIn("verdict:", captured_err)
        # rc mirrors the boolean verdict of whatever ran.
        self.assertIn(rc, (0, 1))


class ProcessTreeCleanupTests(unittest.TestCase):
    """Verify start_new_session + process-group kill actually work.

    These are synthetic: a Python parent forks a background writer
    that inherits stdout and outlives the parent. Without
    ``start_new_session=True`` + ``os.killpg`` in ``_kill_tree``, the
    drainer thread would block reading a still-live pipe from the
    grandchild and ``run_probe`` would hang until the grandchild died.
    """

    def _writer_script(self, tmp: str) -> Path:
        script = textwrap.dedent(r"""
            import os, sys, time
            # Fork a background writer that inherits stdout and lives
            # for several seconds after the parent has exited.
            pid = os.fork()
            if pid == 0:
                # Stay silent while holding the pipes. Periodic output would
                # wake a blocked reader and conceal broken cleanup.
                time.sleep(30)
                os._exit(0)
            # Parent: exit immediately without waiting on the grandchild.
            os._exit(0)
        """).lstrip()
        path = Path(tmp) / "holder.py"
        path.write_text(script)
        return path

    @unittest.skipUnless(hasattr(os, "fork"), "POSIX fork required")
    def test_grandchild_holding_stdout_does_not_hang_after_parent_exit(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            script = self._writer_script(tmp)
            started = time.monotonic()
            result = CC.run_probe(
                [sys.executable, str(script)],
                env=CC.clean_env(tmp), cwd=tmp, timeout=1.0,
            )
            elapsed = time.monotonic() - started
        # Because we spawn with start_new_session=True and kill by
        # process group, the run must return quickly regardless of the
        # grandchild's schedule. Give a generous ceiling to tolerate
        # slow CI: timeout (1s) + 2 * KILL_GRACE + JOIN_GRACE ≈ 7s.
        self.assertLess(elapsed, 3.0, msg=f"took {elapsed:.2f}s")

    @unittest.skipUnless(hasattr(os, "fork"), "POSIX fork required")
    def test_grandchild_holding_stdout_on_lsp_timeout(self) -> None:
        # LSP path uses the same _spawn/_cleanup helpers; assert the
        # same cleanup budget applies.
        with tempfile.TemporaryDirectory() as tmp:
            script = self._writer_script(tmp)
            (Path(tmp) / "ws").mkdir()
            started = time.monotonic()
            result = CC.lsp_initialize(
                [sys.executable, str(script)],
                env=CC.clean_env(tmp), cwd=str(Path(tmp) / "ws"),
                root_uri=Path(tmp, "ws").as_uri(), timeout=0.5,
            )
            elapsed = time.monotonic() - started
        self.assertFalse(result["ok"])
        self.assertLess(elapsed, 8.0, msg=f"took {elapsed:.2f}s")


class LspFramingValidationTests(unittest.TestCase):
    """Frame-level rejections that must not silently pass initialize."""

    def _run_with_stdout(self, header_and_body: bytes, *, timeout: float = 2.0) -> dict:
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / "server.py"
            # Repr the bytes so any special char embeds cleanly.
            body_literal = repr(header_and_body)
            path.write_text(textwrap.dedent(f"""
                import sys, time
                sys.stdout.buffer.write({body_literal})
                sys.stdout.buffer.flush()
                time.sleep(5)
            """).lstrip())
            (Path(tmp) / "ws").mkdir()
            return CC.lsp_initialize(
                [sys.executable, str(path)],
                env=CC.clean_env(tmp), cwd=str(Path(tmp) / "ws"),
                root_uri=Path(tmp, "ws").as_uri(), timeout=timeout,
            )

    def test_zero_content_length_rejected(self) -> None:
        r = self._run_with_stdout(b"Content-Length: 0\r\n\r\n")
        self.assertFalse(r["ok"])
        self.assertIn("non-positive Content-Length", r["error"])

    def test_negative_content_length_rejected(self) -> None:
        r = self._run_with_stdout(b"Content-Length: -1\r\n\r\n")
        self.assertFalse(r["ok"])
        self.assertIn("non-positive Content-Length", r["error"])

    def test_oversized_content_length_rejected(self) -> None:
        oversized = CC.LSP_MAX_CONTENT_LENGTH + 1
        r = self._run_with_stdout(f"Content-Length: {oversized}\r\n\r\n".encode())
        self.assertFalse(r["ok"])
        self.assertIn("exceeds cap", r["error"])

    def test_missing_content_length_rejected(self) -> None:
        r = self._run_with_stdout(b"X-Foo: bar\r\n\r\n")
        self.assertFalse(r["ok"])
        self.assertIn("missing Content-Length", r["error"])

    def test_wrong_jsonrpc_version_rejected(self) -> None:
        body = json.dumps({
            "jsonrpc": "1.0", "id": 1,
            "result": {"capabilities": {"hoverProvider": True}},
        }).encode()
        r = self._run_with_stdout(b"Content-Length: %d\r\n\r\n%s" % (len(body), body))
        self.assertFalse(r["ok"])
        self.assertIn("jsonrpc", r["error"])

    def test_response_for_wrong_id_is_ignored_then_times_out(self) -> None:
        # id != 1 must not be accepted as our initialize reply.
        body = json.dumps({
            "jsonrpc": "2.0", "id": 999,
            "result": {"capabilities": {"hoverProvider": True}},
        }).encode()
        r = self._run_with_stdout(
            b"Content-Length: %d\r\n\r\n%s" % (len(body), body),
            timeout=0.5,
        )
        self.assertFalse(r["ok"])
        self.assertIn("no initialize response", r["error"])


class PythonOverrideTests(unittest.TestCase):
    def test_override_selects_named_interpreter(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            venv = Path(tmp) / "custom" / "bin"
            venv.mkdir(parents=True)
            exe = _write_exe(venv / "python3", "#!/usr/bin/env bash\nexit 0\n")
            path, source = CC.resolve_python_interpreter(str(exe))
        self.assertEqual(path, str(exe))
        self.assertEqual(source, "--python override")

    def test_override_missing_file_is_reported(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            missing = os.path.join(tmp, "nope", "python3")
            path, source = CC.resolve_python_interpreter(missing)
        self.assertEqual(path, missing)
        self.assertIn("missing", source)

    def test_override_not_executable_is_reported(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            f = Path(tmp) / "python3"
            f.write_text("")  # regular file, no +x
            path, source = CC.resolve_python_interpreter(str(f))
        self.assertEqual(path, str(f))
        self.assertIn("not executable", source)

    def test_override_wins_over_base_venv(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            # Ensure a base venv exists so we can prove override wins.
            venv = Path(tmp) / ".venv" / "base" / "bin"
            venv.mkdir(parents=True)
            _write_exe(venv / "python3", "#!/usr/bin/env bash\nexit 0\n")
            custom = _write_exe(Path(tmp) / "custom-python",
                                "#!/usr/bin/env bash\nexit 0\n")
            previous = os.environ.get("HOME")
            os.environ["HOME"] = tmp
            try:
                path, source = CC.resolve_python_interpreter(str(custom))
            finally:
                if previous is None:
                    os.environ.pop("HOME", None)
                else:
                    os.environ["HOME"] = previous
        self.assertEqual(path, str(custom))
        self.assertEqual(source, "--python override")


class PytestEnvHardeningTests(unittest.TestCase):
    """probe_python's pytest env must disable plugin autoload."""

    def test_pytest_env_disables_plugin_autoload(self) -> None:
        captured: dict = {}

        def fake_run_probe(cmd, *, env, cwd, stdin=None, timeout=CC.DEFAULT_TIMEOUT):
            # Record the *first* pytest invocation's env; downstream
            # probes never run because we stub which() to return None.
            if any("pytest" in part for part in cmd):
                captured["env"] = env
            return {"ok": True, "returncode": 0, "stdout": "Python 3.13.0",
                    "stderr": "", "timed_out": False, "error": None}

        with tempfile.TemporaryDirectory() as tmp:
            original_run_probe = CC.run_probe
            original_which = CC.which
            CC.run_probe = fake_run_probe  # type: ignore[assignment]
            CC.which = lambda name: None  # type: ignore[assignment]
            try:
                CC.probe_python(tmp, python_override=None)
            finally:
                CC.run_probe = original_run_probe  # type: ignore[assignment]
                CC.which = original_which  # type: ignore[assignment]
        self.assertIn("env", captured, msg="pytest invocation not observed")
        self.assertEqual(captured["env"].get("PYTEST_DISABLE_PLUGIN_AUTOLOAD"), "1")
        self.assertEqual(captured["env"].get("PIP_NO_INDEX"), "1")


class RuntimeVersionReportingTests(unittest.TestCase):
    """Runtime probes must report an actual version string, not just a path."""

    def test_version_probe_captures_first_stdout_line(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            r = CC._version_probe(
                ["/bin/sh", "-c", "printf 'v9.9.9\\nignored\\n'"], tmp,
            )
        self.assertTrue(r["ok"])
        self.assertEqual(r["version"], "v9.9.9")

    def test_version_probe_falls_back_to_stderr(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            r = CC._version_probe(
                ["/bin/sh", "-c", "printf 'v1.2.3 stderr' 1>&2; exit 0"], tmp,
            )
        self.assertTrue(r["ok"])
        self.assertEqual(r["version"], "v1.2.3 stderr")

    def test_version_probe_reports_failure(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            r = CC._version_probe(
                [os.path.join(tmp, "no-such-runtime")], tmp,
            )
        self.assertFalse(r["ok"])
        self.assertIsNone(r["version"])


class BoundedBufferTests(unittest.TestCase):
    def test_appends_up_to_limit_and_tracks_overflow(self) -> None:
        buf = CC._BoundedBuffer(4)
        buf.append(b"ab")
        buf.append(b"cdefg")
        self.assertEqual(buf.raw(), b"abcd")
        self.assertEqual(buf.total, 7)
        text = buf.decode()
        self.assertIn("abcd", text)
        self.assertIn("3 bytes elided", text)

    def test_under_limit_no_marker(self) -> None:
        buf = CC._BoundedBuffer(64)
        buf.append(b"hello")
        self.assertEqual(buf.decode(), "hello")


class WrongFormatterOutputTests(unittest.TestCase):
    """The formatter probe must reject an incorrect formatter output.

    We simulate this by pointing ``run_probe`` at ``/bin/cat``, which
    echoes its stdin unchanged. That is intentionally the *wrong*
    output, and the aggregation logic in probe_python/probe_node/probe_go
    must reject it. We assert on the underlying invariant that
    ``run_probe`` returns the untransformed bytes so an outer 'matched'
    comparison can distinguish success from a no-op passthrough.
    """

    def test_passthrough_does_not_match_expected_bytes(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            r = CC.run_probe(["/bin/cat"], env=CC.clean_env(tmp), cwd=tmp,
                             stdin=b"def f(x,y ):\n    return  x+y\n", timeout=3.0)
        self.assertTrue(r["ok"])
        self.assertEqual(r["stdout"], "def f(x,y ):\n    return  x+y\n")
        # The Python formatter expects "def f(x, y):" — passthrough must not match.
        expected = "def f(x, y):\n    return x + y\n"
        self.assertNotEqual(r["stdout"], expected)


class WorkspaceCleanupTests(unittest.TestCase):
    """main() must destroy every temp workspace it creates."""

    def test_temp_directory_removed_after_run(self) -> None:
        created: list[str] = []
        original = tempfile.TemporaryDirectory

        class _Recorder(original):
            def __init__(self, *args, **kwargs):
                super().__init__(*args, **kwargs)
                created.append(self.name)

        tempfile.TemporaryDirectory = _Recorder  # type: ignore[assignment]
        stdout_bak, stderr_bak = sys.stdout, sys.stderr
        sys.stdout = io.StringIO()
        sys.stderr = io.StringIO()
        try:
            # This test checks lifecycle, not installed language tools.
            with patch.object(CC, "probe_python", return_value=[]):
                CC.main(["--only", "python", "--summary"])
        finally:
            tempfile.TemporaryDirectory = original  # type: ignore[assignment]
            sys.stdout, sys.stderr = stdout_bak, stderr_bak
        # At least one temp workspace was created and every one is gone.
        self.assertTrue(created)
        for path in created:
            self.assertFalse(os.path.exists(path), msg=path)


if __name__ == "__main__":
    unittest.main()
