"""Verification reports fail closed without mixing narration into JSON."""
import contextlib
import importlib.machinery
import importlib.util
import io
import json
from pathlib import Path
import unittest
from unittest.mock import patch

loader = importlib.machinery.SourceFileLoader("verification", str(Path(__file__).resolve().parents[1] / "verify"))
spec = importlib.util.spec_from_loader(loader.name, loader)
assert spec is not None
module = importlib.util.module_from_spec(spec)
loader.exec_module(module)


class VerificationTest(unittest.TestCase):
    def test_missing_tool_is_an_error(self):
        with patch.object(module.shutil, "which", return_value=None):
            with self.assertRaisesRegex(RuntimeError, "Missing verification tools: fish"):
                module.require("fish")

    def test_typechecker_uses_the_test_interpreter(self):
        with patch.object(module, "require"), patch.object(module, "run") as run:
            module.typecheck()
        args = run.call_args.args
        self.assertEqual(args[args.index("--pythonpath") + 1], module.sys.executable)
        self.assertEqual(args[args.index("--pythonversion") + 1], "3.9")

    def invoke(self, choice):
        out, err = io.StringIO(), io.StringIO()
        with patch.object(module.sys, "argv", ["verify", choice]), contextlib.redirect_stdout(out), contextlib.redirect_stderr(err):
            code = module.main()
        return code, json.loads(out.getvalue()), err.getvalue()

    def test_stops_on_failure_and_reports_it(self):
        with patch.object(module, "lint", side_effect=RuntimeError("missing tool")), patch.object(module, "test") as tests:
            code, report, error = self.invoke("all")
        self.assertEqual(code, 1)
        self.assertEqual(report, [{"check": "lint", "ok": False}])
        self.assertIn("missing tool", error)
        tests.assert_not_called()

    def test_syntax_errors_are_reported(self):
        with patch.object(module, "lint", side_effect=SyntaxError("broken source")):
            code, report, error = self.invoke("lint")
        self.assertEqual(code, 1)
        self.assertFalse(report[0]["ok"])
        self.assertIn("broken source", error)

    def test_all_excludes_host_specific_smoke(self):
        with patch.object(module, "lint"), patch.object(module, "typecheck"), patch.object(module, "test"), patch.object(module, "smoke") as smoke:
            code, report, error = self.invoke("all")
        self.assertEqual(code, 0)
        self.assertTrue(all(result["ok"] for result in report))
        self.assertEqual([result["check"] for result in report], ["lint", "typecheck", "test"])
        self.assertEqual(error, "")
        smoke.assert_not_called()


if __name__ == "__main__":
    unittest.main()
