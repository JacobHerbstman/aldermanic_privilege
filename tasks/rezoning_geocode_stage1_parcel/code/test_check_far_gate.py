import unittest

from check_far_gate import evaluate_parseable_non_structural_gate


class TestCheckFarGate(unittest.TestCase):
    def test_passes_when_missing_zero(self) -> None:
        summary = {"parseable_non_structural_missing_count": 0}
        passed, message = evaluate_parseable_non_structural_gate(summary)
        self.assertTrue(passed)
        self.assertIn("passed", message.lower())

    def test_fails_when_missing_positive(self) -> None:
        summary = {"parseable_non_structural_missing_count": 3}
        passed, message = evaluate_parseable_non_structural_gate(summary)
        self.assertFalse(passed)
        self.assertIn("failed", message.lower())

    def test_fails_when_key_missing(self) -> None:
        summary = {"rows_total": 10}
        passed, message = evaluate_parseable_non_structural_gate(summary)
        self.assertFalse(passed)
        self.assertIn("missing", message.lower())


if __name__ == "__main__":
    unittest.main()
