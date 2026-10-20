# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Offline tests; run this file directly without package or pytest setup.

All computed inputs are synthetic. No temporary folders or solver imports.
"""

import csv
import importlib.util
import io
import os
import re
import subprocess
import sys
import types
import unittest
from contextlib import redirect_stdout
from pathlib import Path
from unittest.mock import patch


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

    def test_computed_rank_gaps_rejected(self):
        """Ranks 1 and 3 cannot establish the second frequency in a spectrum."""
        with self.assertRaisesRegex(ValueError, "every rank"):
            self.harness.compare_modes(
                [target(1, 2), target(2, 4)], [target(1, 2), target(3, 6)]
            )

    def test_summary_csv_reciprocal_period_and_duplicate_objects(self):
        """Hand calculation: T=2 s gives 1/2 Hz; T=0.5 s gives 2 Hz.

        Two objects sharing one global mode must yield one frequency, not two.
        These are synthetic inputs, not published or native results.
        """
        stream = io.StringIO(
            "modeIndex,period,name,abs_max_dof\n1,0.5,pipe,x\n0,2,pipe,z\n0,2,hub,z\n"
        )
        result = self.harness.read_summary_csv(
            stream, model="mj01", synthetic=True, period_unit="s"
        )
        self.assertEqual(result["model"], "mj01")
        self.assertTrue(result["synthetic"])
        self.assertEqual(result["modes"], [target(1, 0.5), target(2, 2.0)])

    def test_summary_csv_invalid_data_rejected(self):
        for body in (
            "modeIndex,period\n0,0\n",
            "modeIndex,period\n0,nan\n",
            "modeIndex,period\n0,-2\n",
            "modeIndex,period\n-1,2\n",
            "modeIndex,period\n0,2\n0,3\n",
            "modeIndex,period\n1,2\n",
            "modeIndex,period\n0,2\n2,1\n",
            "modeIndex,period\n0.0,2\n",
            "modeIndex,period\n",
            "modeIndex,frequency\n0,2\n",
        ):
            with self.subTest(body=body), self.assertRaises(ValueError):
                self.harness.read_summary_csv(
                    io.StringIO(body), model="mj01", synthetic=True, period_unit="s"
                )

    def test_summary_csv_requires_seconds_and_origin(self):
        for unit, origin in (("ms", True), ("s", "false")):
            with self.subTest(unit=unit), self.assertRaises(ValueError):
                self.harness.read_summary_csv(
                    io.StringIO("modeIndex,period\n0,2\n"),
                    model="mj01",
                    synthetic=origin,
                    period_unit=unit,
                )

    def test_summary_csv_cli(self):
        """Synthetic T=2 s -> 0.5 Hz; against 2 Hz gives -75.0 percent."""
        table = {"models": {"synthetic": {"modes": [target(1, 2)]}}}
        output = io.StringIO()
        with (
            patch.object(self.harness, "load_targets", return_value=table),
            patch.object(
                Path, "open", return_value=io.StringIO("modeIndex,period\n0,2\n")
            ),
            redirect_stdout(output),
        ):
            self.harness.main(
                [
                    "targets.yaml",
                    "summary.csv",
                    "--summary-csv",
                    "--model",
                    "synthetic",
                    "--period-unit",
                    "s",
                    "--synthetic",
                    "true",
                ]
            )
        row = next(csv.DictReader(io.StringIO(output.getvalue())))
        self.assertEqual(row["difference_percent"], "-75.0")
        self.assertEqual(row["synthetic"], "True")

    def test_summary_csv_cli_requires_metadata(self):
        with (
            patch.object(self.harness, "load_targets", return_value={"models": {}}),
            patch("sys.stderr", new_callable=io.StringIO),
            self.assertRaises(SystemExit) as error,
        ):
            self.harness.main(["targets.yaml", "summary.csv", "--summary-csv"])
        self.assertEqual(error.exception.code, 2)

    def test_csv_difference_uses_one_decimal(self):
        """Synthetic 3 versus 7 Hz: 100*(3-7)/7 = -400/7 -> -57.1%."""
        rows = self.harness.compare_modes([target(1, 7)], [target(1, 3)])
        stream = io.StringIO()
        self.harness.write_comparison(rows, stream, model="synthetic", synthetic=True)
        parsed = next(csv.DictReader(io.StringIO(stream.getvalue())))
        self.assertEqual(parsed["difference_percent"], "-57.1")
        self.assertAlmostEqual(rows[0]["difference_percent"], -400 / 7)

    def test_fixture_arithmetic(self):
        """Hand calculation in millihertz: (900-863)*100/863 = 3700/863 %.

        The source frequency is in the mj01 specification header, lines 15-16;
        900 millihertz is the explicitly synthetic stored fixture input.
        """
        table = self.harness.load_targets(TARGETS)
        result = self.harness.load_result(FIXTURE)
        self.assertTrue(result["synthetic"])
        rows = self.harness.compare_modes(
            table["models"]["mj01"]["modes"], result["modes"]
        )
        self.assertAlmostEqual(rows[0]["difference_percent"], 3700 / 863)
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

    def test_missing_class_does_not_shift_unclassified_ranks(self):
        """An unclassified first mode cannot silently become published mode two."""
        rows = self.harness.compare_modes(
            [target(1, 2, "out-of-plane"), target(2, 4)],
            [target(1, 2), target(2, 4)],
        )
        self.assertTrue(all(row["status"] == "not computed" for row in rows))

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

    def test_phase_b_rank_pairing_caveat(self):
        table = self.harness.load_targets(TARGETS)
        pairs = []
        for model, data in table["models"].items():
            known = [row for row in data["modes"] if row["frequency_hz"] is not None]
            for lower, upper in zip(known, known[1:]):
                spacing = (
                    (upper["frequency_hz"] - lower["frequency_hz"])
                    / lower["frequency_hz"]
                    * 100
                )
                pairs.append((spacing, model, lower["mode"], upper["mode"]))
        spacing, model, lower, upper = min(pairs)
        skeleton = TARGETS.with_name("modal-benchmark-report-skeleton.html").read_text(
            encoding="utf-8"
        )
        self.assertIn("Rank-pairing caveat", skeleton)
        self.assertIn(f"{model} modes {lower} and {upper}", skeleton)
        self.assertIn(f"{spacing:.1f}%", skeleton)

    def test_import_does_not_load_solver_interface(self):
        code = """
import importlib.util
import sys
spec = importlib.util.spec_from_file_location("modal_benchmark", sys.argv[1])
module = importlib.util.module_from_spec(spec)
spec.loader.exec_module(module)
print("OrcFxAPI" in sys.modules)
print("digitalmodel.solvers.orcaflex.orcaflex_modal_analysis" in sys.modules)
"""
        env = dict(os.environ, PYTHONDONTWRITEBYTECODE="1")
        result = subprocess.run(
            [sys.executable, "-c", code, str(HARNESS)],
            env=env,
            capture_output=True,
            text=True,
            check=False,
        )
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout.splitlines(), ["False", "False"])


def test_import_check_with_modules_already_loaded(monkeypatch):
    """Check helper isolation when another test has registered both names."""
    monkeypatch.setitem(sys.modules, "OrcFxAPI", types.ModuleType("OrcFxAPI"))
    name = "digitalmodel.solvers.orcaflex.orcaflex_modal_analysis"
    monkeypatch.setitem(sys.modules, name, types.ModuleType(name))
    case = TestModalBenchmark("test_import_does_not_load_solver_interface")
    case.test_import_does_not_load_solver_interface()


if __name__ == "__main__":
    unittest.main()
