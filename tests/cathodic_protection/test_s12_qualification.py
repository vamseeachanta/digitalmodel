# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Exercise the S12 applicability checklist without importing a CP engine.

Expected outcomes follow the unresolved decisions in benchmarks/S12.json:
TSA thickness, TSA initial/final treatment and bracelet depletion each remain
NOT_EVALUATED. There are no engineering numerical expectations or tolerances;
source locators are checked against that independently authored specification.
These tests cannot qualify the standard's cells or their engineering use.
"""

import copy
import importlib.util
import json
from pathlib import Path
import subprocess
import sys
import unittest


ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts/reporting/build_cp_s12_qualification.py"
SPEC = ROOT / "docs/domains/cathodic_protection/benchmarks/S12.json"


def load_builder():
    """Load the standalone checklist, without importing the package engines."""
    module_spec = importlib.util.spec_from_file_location("s12_checklist", SCRIPT)
    module = importlib.util.module_from_spec(module_spec)
    module_spec.loader.exec_module(module)
    return module


class TestS12Qualification(unittest.TestCase):
    def setUp(self):
        self.spec = json.loads(SPEC.read_text(encoding="utf-8"))
        self.builder = load_builder()

    def test_open_applicability_decisions_have_sources_and_next_actions(self):
        report = self.builder.build_report(self.spec)
        self.assertEqual(report["status"], "NOT_EVALUATED")
        self.assertFalse(report["executable"])
        self.assertIsNone(report["numerical_results"])
        checks = {row["check_id"]: row for row in report["checks"]}
        expected = {
            "tsa_thickness_basis": ["tsa"],
            "tsa_initial_final_treatment": ["tsa", "mass_output"],
            "bracelet_depletion_applicability": [
                "resistance",
                "depletion",
                "utilisation",
            ],
        }
        self.assertEqual(set(checks), set(expected))
        for key, references in expected.items():
            with self.subTest(check=key):
                row = checks[key]
                self.assertEqual(row["status"], "NOT_EVALUATED")
                self.assertIsNone(row["selected_treatment"])
                self.assertTrue(row["reason"])
                self.assertTrue(row["required_evidence"])
                self.assertTrue(row["next_action"])
                self.assertEqual(
                    row["citations"],
                    {ref: self.spec["citations"][ref] for ref in references},
                )
        self.assertEqual(
            report["source_document"]["sha256"],
            self.spec["source_document"]["sha256"],
        )
        self.assertIn("not verified", report["limits"])

    def test_report_cannot_upgrade_a_supplied_qualification_claim(self):
        self.spec["qualification_status"] = "PASS"
        self.spec["selected_treatment"] = "zero TSA demand"
        report = self.builder.build_report(self.spec)
        self.assertEqual(report["status"], "NOT_EVALUATED")
        self.assertTrue(
            all(row["selected_treatment"] is None for row in report["checks"])
        )

    def test_source_identity_and_planning_boundary_are_required(self):
        mutations = [
            ("case_id", "another_case"),
            ("status", "qualified"),
            ("executable", True),
            ("numerical_inputs", {}),
            ("expected_results", {}),
        ]
        for key, value in mutations:
            with self.subTest(field=key):
                changed = copy.deepcopy(self.spec)
                changed[key] = value
                with self.assertRaises(ValueError):
                    self.builder.build_report(changed)
        for section in ("proposed_route", "source_document"):
            with self.subTest(section=section):
                changed = copy.deepcopy(self.spec)
                changed[section]["edition"] = "2021"
                with self.assertRaises(ValueError):
                    self.builder.build_report(changed)
        changed = copy.deepcopy(self.spec)
        changed["source_document"]["sha256"] = "unbound source"
        with self.assertRaises(ValueError):
            self.builder.build_report(changed)

    def test_missing_citation_or_locator_is_rejected(self):
        for key in ("tsa", "mass_output", "resistance", "depletion", "utilisation"):
            with self.subTest(citation=key):
                changed = copy.deepcopy(self.spec)
                del changed["citations"][key]
                with self.assertRaises(ValueError):
                    self.builder.build_report(changed)
        self.spec["citations"]["tsa"]["locator"] = ""
        with self.assertRaises(ValueError):
            self.builder.build_report(self.spec)

    def test_report_is_detached_from_the_specification(self):
        original = copy.deepcopy(self.spec)
        report = self.builder.build_report(self.spec)
        report["checks"][0]["citations"]["tsa"]["locator"] = "changed"
        report["source_document"]["edition"] = "changed"
        self.assertEqual(self.spec, original)

    def test_command_emits_an_unqualified_json_report(self):
        result = subprocess.run(
            [sys.executable, str(SCRIPT)],
            cwd=ROOT,
            capture_output=True,
            text=True,
            check=True,
        )
        report = json.loads(result.stdout)
        self.assertEqual(report, self.builder.build_report(self.spec))
        self.assertEqual(result.stderr, "")


if __name__ == "__main__":
    unittest.main()
