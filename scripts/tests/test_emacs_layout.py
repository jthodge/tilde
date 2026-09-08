"""Static checks for the explicitly ordered Emacs modules."""
from pathlib import Path
import re
import unittest

ROOT = Path(__file__).resolve().parents[2] / "emacs/.emacs.d"


class EmacsLayoutTest(unittest.TestCase):
    def test_loader_names_each_module_once(self):
        init = (ROOT / "init.el").read_text()
        names = re.findall(r'^\s*(?:\(dolist \(module \'\()?"([a-z-]+)"', init, re.M)
        self.assertTrue(names)
        self.assertEqual(len(names), len(set(names)))
        self.assertEqual(set(names), {path.stem for path in (ROOT / "modules").glob("*.el")})

    def test_modules_use_lexical_binding(self):
        for path in (ROOT / "modules").glob("*.el"):
            self.assertIn("lexical-binding: t", path.read_text().splitlines()[0], str(path))


if __name__ == "__main__":
    unittest.main()
