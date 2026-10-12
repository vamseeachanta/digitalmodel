"""Synthetic text-in/text-out checks; also runnable directly with unittest.

Direct loading avoids executing the solver package initializer. No temporary
directories, repository pytest setup, solver bindings or network are needed.
"""

import importlib.util
from pathlib import Path
import unittest
import xml.etree.ElementTree as ET

import yaml

ROOT = Path(__file__).resolve().parents[3]
SOURCE = ROOT / "src/digitalmodel/solvers/orcaflex/model_catalogue.py"
FIXTURES = Path(__file__).parent / "fixtures/model_catalogue"


def load_module():
    spec = importlib.util.spec_from_file_location("model_catalogue", SOURCE)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def fixture(name):
    return (FIXTURES / f"{name}.yml").read_text(encoding="utf-8")


class TestModelCatalogue(unittest.TestCase):
    def setUp(self):
        self.module = load_module()

    def card(self, text, **kwargs):
        return self.module.render_card("synthetic/model.yml", text, **kwargs)

    def test_counts_and_global_chord(self):
        text = fixture("coordinates")
        card = self.card(text)
        self.assertIn("Lines: 1", card)
        self.assertIn("Vessels: 1", card)
        self.assertIn("Water depth: 100.000", card)
        self.assertIn("chords", card)
        line = yaml.safe_load(text)["Lines"][0]
        # SVG uses actual model coordinates, with screen Y inverted by a group.
        for axis, label in (("Y", "Plan"), ("Z", "Elevation")):
            start = card.index("<svg", card.index(f"<h4>{label}"))
            end = card.index("</svg>", start) + len("</svg>")
            svg = ET.fromstring(card[start:end])
            chord = next(e for e in svg.iter() if e.get("class") == "chord")
            self.assertEqual(float(chord.get("x1")), line["EndAX"])
            self.assertEqual(float(chord.get("x2")), line["EndBX"])
            self.assertEqual(float(chord.get("y1")), line[f"EndA{axis}"])
            self.assertEqual(float(chord.get("y2")), line[f"EndB{axis}"])
        self.assertIn("still water", card)
        self.assertIn("seabed", card)
        self.assertIn('y1="-100.000"', card)  # surface (0) minus depth (100)

    def test_specification_without_geometry(self):
        card = self.card(fixture("specification"))
        self.assertIn("geometry cannot be drawn without the solver", card)
        self.assertIn("System type: riser", card)
        self.assertIn("Water depth: 50.000", card)

    def test_missing_depth_and_includes(self):
        card = self.card(fixture("no_depth"))
        self.assertIn("Water depth: not stated", card)
        self.assertIn("BaseFile: 1", card)
        self.assertNotIn("synthetic_base.yml", card)
        self.assertNotIn('class="seabed"', card)
        self.assertIn("6DBuoys: 1", card)

    def test_deterministic_sorted_build(self):
        entries = [
            (f"synthetic/{n}.yml", fixture(n), "not stated")
            for n in ("specification", "coordinates", "no_depth")
        ]
        first = self.module.render_catalogue(entries)
        self.assertEqual(first, self.module.render_catalogue(reversed(entries)))
        self.assertNotIn("<script", first)
        self.assertNotIn("<link", first)

    def test_project_group_excluded_before_parsing(self):
        entries = [
            (
                "synthetic/project-derived/model.yml",
                "sensitive: [",
                "project-derived sanitised",
            )
        ]
        page = self.module.render_catalogue(entries)
        self.assertNotIn("<article", page)
        self.assertNotIn("sensitive", page)
        self.assertIn("excluded: project-derived", page)

    def test_pending_inventory_is_not_claim(self):
        registry = "models: [{name: attested, highest_validated_level: L1}]"
        inventory = "inventory: [{name: pending, highest_validated_level: L3}]"
        claims = self.module.claim_levels(registry, inventory)
        self.assertEqual(claims["attested"], "L1 (registry)")
        self.assertEqual(claims["pending"], "no claim (pending inventory)")
        self.assertIn("Claim: no claim", self.card(fixture("no_depth")))

    def test_parse_failure_does_not_echo_yaml(self):
        card = self.card("hidden_identifier: [\n")
        self.assertIn("YAML parse failure", card)
        self.assertNotIn("hidden_identifier", card)
        self.assertIn("Water depth: not stated", card)
        self.assertIn("Claim: no claim", card)

    def test_global_connection_table(self):
        data = yaml.safe_load(fixture("coordinates"))
        data["Lines"] = [
            {
                "Connection, ConnectionX, ConnectionY, ConnectionZ": [
                    ["Free", 10, 20, 0],
                    ["Fixed", 30, 40, -80],
                ]
            }
        ]
        card = self.card(yaml.safe_dump(data))
        self.assertIn('class="chord"', card)
        self.assertIn('x2="30.000"', card)

    def test_project_path_pattern_is_excluded(self):
        page = self.module.render_catalogue(
            [
                (
                    "synthetic/project-derived/model.yml",
                    fixture("coordinates"),
                    "not stated",
                )
            ]
        )
        self.assertNotIn("<article", page)

    def test_invalid_encoding_is_not_silent_data(self):
        card = self.card("# synthetic \ufffd\nEnvironment: {WaterDepth: 10}\n")
        self.assertIn("YAML parse failure", card)
        self.assertIn("Water depth: not stated", card)

    def test_generic_object_sections_and_wave_trains(self):
        text = fixture("specification") + (
            "generic:\n  constraints: [{name: synthetic}]\n"
            "  vessel_types: [{name: synthetic_type}]\n"
        )
        card = self.card(text)
        self.assertIn("generic.constraints: 1", card)
        self.assertIn("generic.vessel_types: 1", card)
        text = "environment: {waves: {trains: [{height: 2, type: airy}]}}"
        self.assertIn("height=2.000", self.card(text))
        self.assertIn("type=airy", self.card(text))

    def test_vendor_metadata_only(self):
        card = self.card(
            fixture("coordinates") + "Secret: hidden_identifier\n",
            provenance="vendor example",
        )
        self.assertIn("vendor example models", card)
        self.assertNotIn("synthetic_body", card)
        self.assertNotIn("hidden_identifier", card)
        self.assertIn(
            "redistribution terms to be checked before any model file "
            "is shared outside",
            card,
        )

    def test_local_frame_is_not_drawn_as_global(self):
        text = fixture("coordinates").replace(
            "EndAConnection: Free", "EndAConnection: synthetic_body"
        )
        card = self.card(text)
        self.assertNotIn('class="chord"', card)
        self.assertIn("unresolved", card)

    def test_nonfinite_or_missing_coordinate_is_not_zero(self):
        text = fixture("coordinates").replace("EndAX: 10", "EndAX: .nan")
        self.assertNotIn('class="chord"', self.card(text))
        text = fixture("coordinates").replace("    EndAY: 20\n", "")
        self.assertNotIn('class="chord"', self.card(text))

    def test_provenance_requires_evidence(self):
        classify = self.module.classify_provenance
        self.assertEqual(classify("a01_test", "", ""), "not stated")
        readme = "a* to e* prefixes follow the Orcina example lettering"
        self.assertEqual(classify("a01_test", readme, ""), "vendor example")
        self.assertEqual(classify("f01_test", readme, ""), "not stated")
        self.assertEqual(classify("synthetic", "", "# Provenance: generic"), "generic")
        self.assertEqual(
            classify("synthetic", "project-derived sanitised", ""),
            "project-derived sanitised",
        )

    def test_html_escaping_and_usage_paths(self):
        card = self.card(fixture("no_depth"), usage=["tests/<synthetic>.py"])
        self.assertIn("tests/&lt;synthetic&gt;.py", card)
        self.assertNotIn("tests/<synthetic>", card)


if __name__ == "__main__":
    unittest.main(verbosity=2)
