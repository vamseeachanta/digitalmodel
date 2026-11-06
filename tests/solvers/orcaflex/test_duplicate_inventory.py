# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Independent hand-counted examples for the offline duplicate report."""

import csv
import contextlib
import io
import json
import runpy
import tempfile
import unittest
from collections import defaultdict
from pathlib import Path
from unittest.mock import patch

ROOT = Path(__file__).resolve().parents[3]
PREFIX = "docs/domains/orcaflex/"


class TestCompactInventory(unittest.TestCase):
    def setUp(self):
        self.tool = runpy.run_path(
            str(ROOT / PREFIX / "library/duplicate_inventory.py")
        )

    def test_join_preserves_all_reference_candidates(self):
        """Hand count: two equal copies, one surplus, two reference locations.

        Each copy joins on sample.yml to both locations: four candidate pairs.
        Storing two reference rows preserves all four without repeating them.
        """
        paths = [PREFIX + "examples/raw/sample.yml", PREFIX + "examples/yml/sample.yml"]
        references = defaultdict(set, {"sample.yml": {"tests/a.py:3", "docs/b.md:7"}})
        with tempfile.TemporaryDirectory() as directory:
            output = Path(directory)
            counts = self.tool["write_table"](
                output / "copies.csv", [("digest", paths)], references
            )
            self.tool["write_reference_table"](output / "references.csv", references)
            with (output / "copies.csv").open(newline="") as stream:
                copies = list(csv.DictReader(stream))
            with (output / "references.csv").open(newline="") as stream:
                refs = list(csv.DictReader(stream))
        self.assertEqual(
            counts["examples_and_model_library"],
            {"sets": 1, "files": 2, "redundant": 1},
        )
        self.assertEqual(
            [row["path"] for row in copies if row["proposed_canonical"] == "true"],
            [paths[0]],
        )
        joined = {
            (copy["path"], ref["location"])
            for copy in copies
            for ref in refs
            if copy["reference_basename"] == ref["basename"]
        }
        self.assertEqual(
            joined,
            {
                (path, location)
                for path in paths
                for location in references["sample.yml"]
            },
        )
        self.assertEqual(len(refs), 2)

    def test_absent_references_are_explicit(self):
        """Hand count: one duplicate set, no documents mentioning its basename."""
        paths = [PREFIX + "reference/a.yml", PREFIX + "regional/a.yml"]
        with tempfile.TemporaryDirectory() as directory:
            output = Path(directory) / "copies.csv"
            self.tool["write_table"](output, [("digest", paths)], {})
            with output.open(newline="") as stream:
                rows = list(csv.DictReader(stream))
        self.assertEqual([row["reference_count"] for row in rows], ["0", "0"])

    def test_full_report_readback(self):
        """Hand calculation: three inputs, two equal after comment removal.

        The unequal value remains outside the only set: two files minus one
        canonical equals one redundant copy. One document mentions the basename
        twice on the same line, so there is one distinct reference location.
        """
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory) / "source"
            output = Path(directory) / "output"
            names = [
                PREFIX + "examples/raw/a.yml",
                PREFIX + "examples/yml/a.yml",
                PREFIX + "examples/raw/b.yml",
                "docs/usage.md",
            ]
            texts = [
                "# first\nGeneral: {}\n",
                "# second\nGeneral: {}\n",
                "General: {UnitsSystem: SI}\n",
                "a.yml and a.yml\n",
            ]
            for name, text in zip(names, texts):
                path = root / name
                path.parent.mkdir(parents=True, exist_ok=True)
                path.write_text(text)
            main = self.tool["main"]
            args = ["inventory", "--root", str(root), "--output-dir", str(output)]
            with (
                patch("sys.argv", args),
                patch.dict(main.__globals__, {"tracked_paths": lambda _: names}),
            ):
                with contextlib.redirect_stdout(io.StringIO()):
                    main()
            summary = json.loads((output / "duplicate_counts.json").read_text())
            self.assertEqual(summary["schema_version"], "2")
            self.assertEqual(
                [
                    summary[key]
                    for key in (
                        "scanned",
                        "sets",
                        "files",
                        "redundant",
                        "reference_rows",
                    )
                ],
                [3, 1, 2, 1, 1],
            )
            for key, name in (
                ("table_bytes", "duplicate_sets.csv"),
                ("reference_table_bytes", "duplicate_references.csv"),
            ):
                self.assertEqual(summary[key], len((output / name).read_bytes()))

    def test_reference_rows_are_sorted_and_empty_table_has_header(self):
        """Lexical ordering is explicit; no findings still emits a usable schema."""
        with tempfile.TemporaryDirectory() as directory:
            output = Path(directory) / "references.csv"
            write = self.tool["write_reference_table"]
            write(output, {"b.yml": {"tests/z.py:2", "docs/a.md:1"}, "a.yml": set()})
            self.assertEqual(
                output.read_text().splitlines(),
                ["basename,location", "b.yml,docs/a.md:1", "b.yml,tests/z.py:2"],
            )
            write(output, {})
            self.assertEqual(output.read_text(), "basename,location\n")

    def test_mooring_iteration_group_has_provenance(self):
        """An existing README does not establish origin or redistribution terms."""
        text = (
            ROOT / "tests/solvers/orcaflex/mooring-tension-iteration/PROVENANCE.md"
        ).read_text()
        for field in (
            "Origin class",
            "Source statement",
            "Date",
            "Basis",
            "Permitted use",
        ):
            self.assertIn("## " + field, text)
        self.assertIn("not stated; redistribution terms to be checked", text)

    def test_example_counts_and_template_include(self):
        """Counts are direct enumeration; include resolution is filesystem arithmetic."""
        examples = ROOT / PREFIX / "examples"
        readme = (examples / "README.md").read_text()
        for folder in ("raw", "yml", "modular"):
            files = len(list((examples / folder).rglob("*.yml")))
            directories = sum(path.is_dir() for path in (examples / folder).iterdir())
            self.assertIn(
                f"{files} YAML files in {directories} immediate folders", readme
            )
        template = ROOT / PREFIX / "templates/platforms/fpso_hybrid/base/fpso_base.yml"
        includes = [
            line.split("IncludeFile:", 1)[1].strip()
            for line in template.read_text().splitlines()
            if line.strip().startswith("IncludeFile:")
        ]
        chain = next(value for value in includes if value.endswith("chain_84mm_r4.yml"))
        self.assertEqual(
            (template.parent / chain).resolve(),
            ROOT / PREFIX / "library/line_types/chain_84mm_r4.yml",
        )
        self.assertTrue((template.parent / chain).is_file())


if __name__ == "__main__":
    unittest.main()
