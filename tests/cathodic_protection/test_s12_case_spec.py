# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Load the planning-only S12 specification without importing a CP engine."""

import json
from pathlib import Path
import unittest


SPEC = (
    Path(__file__).resolve().parents[2]
    / "docs/domains/cathodic_protection/benchmarks/S12.json"
)


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

        for group in spec["required_inputs"]:
            with self.subTest(input_group=group["group"]):
                self.assertTrue(group["fields"])
                self.assertTrue(group["citations"])
                for reference in group["citations"]:
                    self.assertIn(reference, citations)
        self.assertEqual(
            {zone["basis"] for zone in spec["zones"]},
            {"bare_steel", "thermally_sprayed_aluminium"},
        )
        for zone in spec["zones"]:
            for reference in zone["citations"]:
                self.assertIn(reference, citations)


if __name__ == "__main__":
    unittest.main()
