# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Synthetic text-in/text-out checks; also runnable directly with unittest.

Direct loading avoids executing the solver package initializer. Pytest fixtures
use isolated temporary directories; no solver bindings or network are needed.
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


def test_structural_owners_and_path_references(tmp_path):
    """Include fragments belong to the model and ignore ordinary prose."""
    module = load_module()
    owner = "docs/domains/orcaflex/templates/synthetic_model"
    leaves = ("includes/line.yml", "inputs/parameters.yml", "includes/nested/a.yml")
    paths = [f"{owner}/{leaf}" for leaf in leaves]
    for relative in paths:
        target = tmp_path / relative
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_text("General: {}\n", encoding="utf-8")
        assert module.model_name(relative) == "synthetic_model"
    tests = tmp_path / "tests"
    tests.mkdir()
    (tests / "prose.py").write_text("includes inputs synthetic_model", encoding="utf-8")
    (tests / "reference.py").write_text(
        f"'{owner}/includes/line.yml'", encoding="utf-8"
    )
    groups = [(tmp_path / owner, [tmp_path / p for p in paths])]
    usage = module.find_usage(tmp_path, groups)
    assert usage[owner] == ["tests/reference.py"]
    page = module.render_catalogue(
        [(p, "General: {}", "not stated") for p in paths], usage=usage
    )
    assert page.count("tests/reference.py") == 3
    assert "tests/prose.py" not in page


def test_package_word_requires_path_context(tmp_path):
    """An ordinary package word alone cannot constitute a model reference."""
    module = load_module()
    path = "docs/domains/orcaflex/library/templates/geometry/model.yml"
    tests = tmp_path / "tests"
    tests.mkdir()
    (tests / "prose.py").write_text("geometry", encoding="utf-8")
    (tests / "reference.py").write_text(
        "'templates/geometry/model.yml'", encoding="utf-8"
    )
    (tests / "windows.py").write_text(
        r"'templates\geometry\model.yml'", encoding="utf-8"
    )
    (tests / "suffix.py").write_text("'other_geometry/model.yml'", encoding="utf-8")
    owner = path.rsplit("/", 1)[0]
    usage = module.find_usage(tmp_path, [(tmp_path / owner, [tmp_path / path])])
    assert usage[owner] == ["tests/reference.py", "tests/windows.py"]


def test_unknown_origin_requires_caution():
    """Unstated origin retains its class and receives redistribution caution."""
    card = load_module().render_card("synthetic/model.yml", "General: {}")
    assert "Provenance: not stated" in card
    assert "redistribution terms to be checked" in card


def test_extracted_description_requires_caution():
    """Extraction requires caution even when another provenance class is stated."""
    card = load_module().render_card(
        "synthetic/spec.yml",
        "metadata: {description: Extracted from synthetic.yml}",
        provenance="generic",
    )
    assert "Provenance: generic" in card
    assert "redistribution terms to be checked" in card


def test_monolithic_sibling_requires_caution():
    """A reference output requires caution on every variant of its model."""
    page = load_module().render_catalogue(
        [("synthetic/model/includes/line.yml", "General: {}", "generic")],
        extracted_models={"synthetic/model"},
    )
    assert "redistribution terms to be checked" in page


def test_stated_generic_origin_without_extraction():
    """Affirmative generic origin without extraction needs no redistribution caution."""
    card = load_module().render_card(
        "synthetic/model.yml", "General: {}", provenance="generic"
    )
    assert "redistribution terms to be checked" not in card


def test_fixture_base_exists():
    """The committed include fixture references a committed synthetic base."""
    data = yaml.safe_load(fixture("no_depth"))
    target = FIXTURES / data["BaseFile"]
    assert target.is_file()
    assert "Synthetic" in target.read_text(encoding="utf-8")


def test_generated_header_and_master_count():
    """The page declares its command, inputs, freshness check and master failures."""
    page = load_module().render_catalogue(
        [("synthetic/model/modular/master.yml", "General: {}\n- invalid", "generic")]
    )
    assert "Generated by" in page
    assert (
        "python src/digitalmodel/solvers/orcaflex/model_catalogue.py --output" in page
    )
    assert "Inputs:" in page
    assert "test_committed_page_is_fresh" in page
    assert "master-file form: 1" in page
    committed = ROOT / "docs/domains/orcaflex/library/model_catalogue.html"
    assert "Generated by" in committed.read_text(encoding="utf-8")


def test_inherited_claim_is_model_level():
    """An include fragment labels its inherited registry claim as model-level."""
    page = load_module().render_catalogue(
        [("synthetic/model/includes/line.yml", "General: {}", "generic")],
        claims={"model": "L1 (registry)"},
    )
    assert "model-level claim" in page
    assert "L1 (registry)" in page


def test_renderer_loads_without_solver_initializer():
    """Both modules load by path in a fresh interpreter without solver imports."""
    import subprocess
    import sys

    probe = """
import importlib.util
from pathlib import Path
import sys
before = set(sys.modules)
for filename in ("model_catalogue.py", "model_catalogue_render.py"):
    spec = importlib.util.spec_from_file_location("probe", Path(sys.argv[1]) / filename)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
added = set(sys.modules) - before
assert not any(name.startswith("digitalmodel.solvers.orcaflex") for name in added)
assert "OrcFxAPI" not in added
"""
    result = subprocess.run(
        [sys.executable, "-c", probe, str(SOURCE.parent)],
        capture_output=True,
        text=True,
        check=False,
    )
    assert result.returncode == 0, result.stderr


def test_python_modules_meet_size_rules():
    """Catalogue modules and tests remain below 400 lines and within 88 columns."""
    for path in (SOURCE, SOURCE.with_name("model_catalogue_render.py"), Path(__file__)):
        lines = path.read_text(encoding="utf-8").splitlines()
        assert len(lines) < 400, (path.name, len(lines))
        assert max(map(len, lines)) <= 88, path.name


def test_committed_page_is_fresh():
    """Two consecutive builds equal each other and the committed UTF-8 bytes."""
    module = load_module()
    first = module.build(ROOT).encode("utf-8")
    assert first == module.build(ROOT).encode("utf-8")
    assert (
        first
        == (ROOT / "docs/domains/orcaflex/library/model_catalogue.html").read_bytes()
    )


def test_extracted_specification_cautions_siblings(tmp_path):
    """Extraction recorded by the model applies to its include variants."""
    module = load_module()
    model = tmp_path / "synthetic/model"
    model.mkdir(parents=True)
    spec = model / "spec.yml"
    spec.write_text("metadata: {description: Extracted from synthetic.yml}")
    fragment = model / "includes/line.yml"
    groups = [(model, [spec, fragment])]
    extracted = module.extracted_models(tmp_path, groups)
    page = module.render_catalogue(
        [("synthetic/model/includes/line.yml", "General: {}", "generic")],
        extracted_models=extracted,
    )
    assert "redistribution terms to be checked" in page


if __name__ == "__main__":
    unittest.main(verbosity=2)
