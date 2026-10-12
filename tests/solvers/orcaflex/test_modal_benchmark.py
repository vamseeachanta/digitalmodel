# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Offline tests; run this file directly without package or pytest setup.

All computed inputs are synthetic. No temporary folders or solver imports.
"""

import csv
import importlib.util
import io
import re
import sys
import unittest
from contextlib import redirect_stdout
from pathlib import Path


ROOT = Path(__file__).resolve().parents[3]
HARNESS = ROOT / "src/digitalmodel/solvers/orcaflex/modal_benchmark.py"
TARGETS = ROOT / (
    "docs/domains/orcaflex/subsea/jumper/m_shaped/modal_benchmark_targets.yaml"
)
FIXTURE = Path(__file__).parent / "fixtures/modal_benchmark/tiny_modal_result.json"


def load_harness():
    """Load the supported standalone entry point, never its parent package."""
    spec = importlib.util.spec_from_file_location("modal_benchmark", HARNESS)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def target(mode, frequency, direction=None):
    """Construct an explicitly synthetic target for pairing tests."""
    return {"mode": mode, "frequency_hz": frequency, "direction": direction}


class TestModalBenchmark(unittest.TestCase):
    def setUp(self):
        self.harness = load_harness()

    def test_loader_and_mj01_header_values(self):
        table = self.harness.load_targets(TARGETS)
        model = table["models"]["mj01"]
        text = (ROOT / model["spec_file"]).read_text(encoding="utf-8")
        expected = [float(v) for v in re.findall(r"f\d=([\d.]+) Hz", text)]
        self.assertEqual(len(expected), 4)
        self.assertEqual([row["frequency_hz"] for row in model["modes"]], expected)
        for item in table["models"].values():
            for row in item["modes"]:
                self.assertTrue(row["citation"])
                for source in row["sources"]:
                    lines = (
                        (ROOT / source["file"]).read_text(encoding="utf-8").splitlines()
                    )
                    self.assertLessEqual(source["line_end"], len(lines))
                    self.assertGreaterEqual(source["line_start"], 1)

    def test_fixture_arithmetic(self):
        table = self.harness.load_targets(TARGETS)
        result = self.harness.load_result(FIXTURE)
        self.assertTrue(result["synthetic"])
        rows = self.harness.compare_modes(
            table["models"]["mj01"]["modes"], result["modes"]
        )
        published = table["models"]["mj01"]["modes"][0]["frequency_hz"]
        computed = min(row["frequency_hz"] for row in result["modes"])
        self.assertAlmostEqual(
            rows[0]["difference_percent"], (computed - published) / published * 100
        )
        self.assertLess(rows[1]["difference_percent"], 0)

    def test_unordered_modes_pair_by_frequency_not_number(self):
        targets = [target(2, 4.0), target(1, 2.0)]
        modes = [{"mode": 1, "frequency_hz": 4.4}, {"mode": 2, "frequency_hz": 2.2}]
        rows = self.harness.compare_modes(targets, modes)
        self.assertEqual([r["computed_mode"] for r in rows], [2, 1])
        self.assertAlmostEqual(rows[0]["difference_percent"], (2.2 - 2) / 2 * 100)

    def test_short_result_marks_not_computed(self):
        rows = self.harness.compare_modes(
            [target(1, 2), target(2, 4)], [{"mode": 1, "frequency_hz": 2}]
        )
        self.assertEqual(rows[1]["status"], "not computed")
        self.assertIsNone(rows[1]["difference_percent"])

    def test_empty_result(self):
        rows = self.harness.compare_modes([target(1, 2)], [])
        self.assertEqual(rows[0]["status"], "not computed")

    def test_direction_classes_precede_unclassified_slots(self):
        targets = [target(1, 2, "out-of-plane"), target(2, 3, "in-plane"), target(3, 4)]
        modes = [target(1, 1, "in-plane"), target(2, 5, "out-of-plane"), target(3, 6)]
        rows = self.harness.compare_modes(targets, modes)
        self.assertEqual([r["computed_mode"] for r in rows], [2, 1, 3])
        self.assertEqual(len({r["computed_mode"] for r in rows}), 3)

    def test_missing_direction_is_not_guessed(self):
        rows = self.harness.compare_modes(
            [target(1, 2, "out-of-plane")], [target(1, 2)]
        )
        self.assertEqual(rows[0]["status"], "not computed")
        self.assertIsNone(rows[0]["computed_mode"])

    def test_sparse_targets_preserve_rank_slots(self):
        targets = [target(1, 2), target(2, None), target(3, 6)]
        modes = [target(1, 2.2), target(2, 4.4), target(3, 6.6)]
        rows = self.harness.compare_modes(targets, modes)
        self.assertEqual(rows[1]["status"], "not held in the repository")
        self.assertIsNone(rows[1]["difference_percent"])
        self.assertEqual(rows[2]["computed_mode"], 3)
        self.assertAlmostEqual(rows[2]["difference_percent"], (6.6 - 6) / 6 * 100)

    def test_mj03_does_not_pair_mode13_with_fifth_result(self):
        targets = self.harness.load_targets(TARGETS)["models"]["mj03"]["modes"]
        modes = [
            target(i, float(i), {1: "out-of-plane", 2: "in-plane"}.get(i))
            for i in range(1, 6)
        ]
        rows = self.harness.compare_modes(targets, modes)
        self.assertEqual(len(rows), 13)
        self.assertEqual(rows[-1]["status"], "not computed")

    def test_invalid_frequency_rejected(self):
        for frequency in (0, -1, float("nan"), float("inf"), True, "2"):
            with self.subTest(frequency=frequency), self.assertRaises(ValueError):
                self.harness.compare_modes([target(1, 2)], [target(1, frequency)])

    def test_invalid_published_frequency_rejected(self):
        for frequency in (0, -1, float("nan")):
            with self.subTest(frequency=frequency), self.assertRaises(ValueError):
                self.harness.compare_modes([target(1, frequency)], [])

    def test_duplicate_mode_rejected(self):
        with self.assertRaises(ValueError):
            self.harness.compare_modes([target(1, 2)], [target(1, 2), target(1, 3)])

    def test_invalid_mode_and_direction_rejected(self):
        for mode in (0, -1, True, 1.5):
            with self.subTest(mode=mode), self.assertRaises(ValueError):
                self.harness.compare_modes([target(1, 2)], [target(mode, 2)])
        with self.assertRaises(ValueError):
            self.harness.compare_modes([target(1, 2)], [target(1, 2, "sideways")])

    def test_missing_target_slots_rejected(self):
        with self.assertRaises(ValueError):
            self.harness.compare_modes([target(1, 2), target(3, 6)], [])

    def test_equal_frequencies_have_deterministic_tie_break(self):
        rows = self.harness.compare_modes(
            [target(1, 2), target(2, 2)], [target(2, 2), target(1, 2)]
        )
        self.assertEqual([r["computed_mode"] for r in rows], [1, 2])

    def test_csv_output_and_rule(self):
        rows = self.harness.compare_modes([target(1, 2), target(2, 4)], [target(1, 2)])
        stream = io.StringIO()
        self.harness.write_comparison(rows, stream, model="synthetic", synthetic=True)
        parsed = list(csv.DictReader(io.StringIO(stream.getvalue())))
        self.assertEqual(parsed[0]["pairing_rule"], self.harness.PAIRING_RULE)
        self.assertEqual(parsed[0]["synthetic"], "True")
        self.assertEqual(parsed[1]["difference_percent"], "")
        self.assertEqual(parsed[1]["status"], "not computed")

    def test_cli_stdout_is_a_comparison_table(self):
        output = io.StringIO()
        with redirect_stdout(output):
            self.harness.main([str(TARGETS), str(FIXTURE)])
        rows = list(csv.DictReader(io.StringIO(output.getvalue())))
        self.assertEqual(len(rows), 4)
        self.assertTrue(all(row["model"] == "mj01" for row in rows))
        self.assertEqual(rows[-1]["status"], "not computed")

    def test_import_does_not_load_solver_interface(self):
        self.assertNotIn("OrcFxAPI", sys.modules)
        load_harness()
        self.assertNotIn("OrcFxAPI", sys.modules)
        self.assertNotIn(
            "digitalmodel.solvers.orcaflex.orcaflex_modal_analysis", sys.modules
        )


if __name__ == "__main__":
    unittest.main()
