# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Check the planning-only S12 specification without importing a CP engine.

Source pages and engineering applicability are not verified offline. These tests
check source-reference structure and the live edition/registry claims only.
"""

import ast
import importlib.util
import json
from pathlib import Path
import unittest


ROOT = Path(__file__).resolve().parents[2]
SPEC = ROOT / "docs/domains/cathodic_protection/benchmarks/S12.json"


class TestS12CaseSpec(unittest.TestCase):
    def test_spec_loads_with_resolvable_source_references(self):
        spec = json.loads(SPEC.read_text(encoding="utf-8"))
        self.assertEqual(spec["case_id"], "S12")
        self.assertEqual(spec["status"], "specification_only")
        self.assertFalse(spec["executable"])
        self.assertEqual(spec["proposed_route"]["edition"], "1993")
        self.assertIsNone(spec["numerical_inputs"])
        self.assertIsNone(spec["expected_results"])
        self.assertTrue(spec["missing"])

        document = spec["source_document"]
        self.assertTrue(document["url"].startswith("https://rules.dnv.com/"))
        self.assertRegex(document["sha256"], r"^[0-9a-f]{64}$")
        citations = spec["citations"]
        for key, citation in citations.items():
            with self.subTest(citation=key):
                self.assertTrue(citation["locator"])
                self.assertGreaterEqual(citation["printed_page"], 1)
                self.assertLessEqual(citation["pdf_page"], document["pdf_pages"])
                self.assertEqual(citation["pdf_page"], citation["printed_page"] + 1)
                end = citation.get("printed_page_end", citation["printed_page"])
                pdf_end = citation.get("pdf_page_end", citation["pdf_page"])
                self.assertGreaterEqual(end, citation["printed_page"])
                self.assertEqual(pdf_end, end + 1)
                self.assertLessEqual(pdf_end, document["pdf_pages"])

        referenced = set(spec["acceptance_citations"])
        for group in spec["required_inputs"]:
            with self.subTest(input_group=group["group"]):
                self.assertTrue(group["fields"])
                self.assertTrue(group["citations"])
                for reference in group["citations"]:
                    self.assertIn(reference, citations)
                referenced.update(group["citations"])
        self.assertEqual(
            {zone["basis"] for zone in spec["zones"]},
            {"bare_steel", "thermally_sprayed_aluminium"},
        )
        for zone in spec["zones"]:
            for reference in zone["citations"]:
                self.assertIn(reference, citations)
            referenced.update(zone["citations"])
        self.assertEqual(referenced, set(citations))

    def test_1993_is_rejected_by_the_repository_edition_api(self):
        # Load only the helper file; package __init__ imports the CP engines.
        path = ROOT / "src/digitalmodel/cathodic_protection/_edition.py"
        module_spec = importlib.util.spec_from_file_location("s12_edition", path)
        module = importlib.util.module_from_spec(module_spec)
        module_spec.loader.exec_module(module)
        with self.assertRaisesRegex(ValueError, "Unsupported.*1993"):
            module.normalize_edition("1993")

    def test_s12_is_absent_from_the_executable_case_registry(self):
        # Inspect the actual CASES literal without running the pack builder.
        path = ROOT / "scripts/reporting/build_cp_review_portfolio.py"
        tree = ast.parse(path.read_text(encoding="utf-8"))
        assignments = [
            node
            for node in tree.body
            if isinstance(node, ast.Assign)
            and any(
                isinstance(target, ast.Name) and target.id == "CASES"
                for target in node.targets
            )
        ]
        self.assertEqual(len(assignments), 1)
        cases = ast.literal_eval(assignments[0].value)
        self.assertTrue(cases)
        self.assertNotIn("S12", [case[0] for case in cases])


if __name__ == "__main__":
    unittest.main()
