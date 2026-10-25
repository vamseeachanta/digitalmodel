# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Front-page contracts and independent arithmetic on saved public examples."""

import importlib.util
import json
import math
import re
from pathlib import Path
from xml.etree import ElementTree as ET

import pytest

ROOT = Path(__file__).resolve().parents[2]
SPEC = importlib.util.spec_from_file_location(
    "front_page_builder", ROOT / "scripts/cathodic_protection/build_front_page_data.py"
)
builder = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(builder)
from digitalmodel.cathodic_protection import report_schematics as drawings


@pytest.fixture(scope="module")
def data():
    return builder.build_data(ROOT)


def active(data):
    return [card for card in data["structures"] if card["status"] == "ready"]


def card(data, key):
    return next(item for item in data["structures"] if item["id"] == key)


def test_step_table_contract(data):
    """Acceptance 3: six ordered steps and complete seven-column rows."""
    for item in active(data):
        assert [step["title"] for step in item["steps"]] == [
            "Inputs", "Criteria", "Anode sizing", "Distribution", "Check", "Monitoring plan"
        ]
        for step in item["steps"]:
            assert step["columns"] == [
                "Quantity", "Symbol", "Value", "Unit", "Input class", "Basis", "Source"
            ]
            assert step["rows"]
            assert step["report"].endswith("#" + step["id"])
            for row in step["rows"]:
                assert all(row[key] not in (None, "", []) for key in (
                    "quantity", "symbol", "value", "unit", "input_class", "basis", "source"
                ))
                assert row["input_class"] in {
                    "published", "synthetic", "ASSUMED - to be confirmed"
                }


def test_every_numeric_row_has_resolvable_evidence(data):
    """Acceptance 4: values and printed precision match independently read files."""
    for item in active(data):
        for step in item["steps"]:
            for row in step["rows"]:
                if "raw_value" not in row:
                    continue
                evidence = row["evidence"]
                operands = []
                for operand in evidence["operands"]:
                    path = ROOT / operand["path"]
                    if path.suffix == ".json":
                        source = json.loads(path.read_text(encoding="utf-8"))
                    else:
                        import yaml
                        source = yaml.safe_load(path.read_text(encoding="utf-8"))
                    value = source
                    for key in operand["keys"]:
                        value = value[key]
                    operands.append(value)
                operation = evidence["operation"]
                if operation == "identity":
                    expected = operands[0]
                elif operation == "product":
                    expected = math.prod(operands)
                elif operation == "difference":
                    expected = operands[0] - operands[1]
                elif operation == "quotient":
                    expected = operands[0] / operands[1]
                elif operation == "difference_product":
                    expected = (operands[0] - operands[1]) * operands[2]
                elif operation == "product_margin_percent":
                    expected = 100 * (math.prod(operands[:-1]) / operands[-1] - 1)
                elif operation == "margin_percent":
                    expected = 100 * (operands[0] / operands[1] - 1)
                else:
                    pytest.fail("unrecognised evidence operation")
                assert row["raw_value"] == pytest.approx(expected, rel=1e-12)
                assert row["value"] == format(expected, f'.{row["precision"]}f').replace("-", "−")


def test_standard_editions_and_existing_locators(data):
    """Acceptance 5: standards use case-pack editions and existing locators."""
    for item in active(data):
        assert re.search(r"20\d{2}", item["standard"])
        assert item["standard"] in {"DNV-RP-B401 (May 2021)",
            "DNVGL-RP-F103 (September 2019, amended May 2021)"}
        for step in item["steps"]:
            for row in step["rows"]:
                if "DNV" in row["basis"]:
                    assert item["standard"] in row["basis"]
                    assert row["locator"] in item["citation_locators"]


@pytest.mark.parametrize("key", ["hull", "riser"])
def test_pending_cards_have_no_numbers(data, key):
    """Acceptance 11 includes all nested content, not just a status label."""
    item = card(data, key)
    assert item["status"] == "in preparation"
    assert not re.search(r"\d", json.dumps(item))
    assert "steps" not in item


def test_default_does_not_read_pending_case(monkeypatch):
    original = Path.read_text
    def read(path, *args, **kwargs):
        assert path.parent.name != "S08"
        return original(path, *args, **kwargs)
    monkeypatch.setattr(Path, "read_text", read)
    builder.build_data(ROOT)


def test_opt_in_riser_is_explicit():
    assert card(builder.build_data(ROOT, include_riser=True), "riser")["status"] == "preview"


@pytest.mark.parametrize("key,quantity,expected", [
    # S02 input: 1200 m² × 0.20 A/m² × 0.02 initial coating factor.
    ("monopile", "Initial current demand", 4.8),
    # S03: 850 m² × 0.12 A/m² × (0.02 + 0.012 × 25 / 2).
    ("subsea-structure", "Mean current demand", 17.34),
    # S06: ceil(846.119 kg / 25 kg net per bracelet).
    ("pipeline", "Anodes needed for mass", 34),
    # Reference sidecar: 252 trial anodes × π × 0.05² × 1 × 2750 kg/m³.
    # Density basis is the fixture's cp_input_sources alloy-density entry.
    ("fixed-jacket", "Mass provided", 252 * math.pi * 0.05**2 * 2750),
])
def test_hand_calculation_anchors(data, key, quantity, expected):
    rows = [row for step in card(data, key)["steps"] for row in step["rows"]]
    values = [row["raw_value"] for row in rows if row["quantity"] == quantity]
    assert values
    assert all(value == pytest.approx(expected, rel=1e-6) for value in values)


def test_result_line_values_are_bound_to_rows(data):
    for item in active(data):
        rows = {row["key"]: row for step in item["steps"] for row in step["rows"]}
        for key, value in item["result_values"].items():
            assert rows[key]["value"] == value
            assert value in item["result_line"]


def test_monitoring_and_three_phase_response(data):
    for item in active(data):
        plan = item["monitoring"]
        assert plan["status"] == "NOT_EVALUATED"
        assert plan["positions"] and plan["required_inputs"]
        for field in ["frequency", "trigger", "retrofit_quantity", "intervention_date"]:
            assert plan[field] == "NOT_EVALUATED"
        if item["verdict"] == "does not meet the criterion":
            assert set(item["recommendation"]) == {"design", "monitoring", "retrofit"}
            assert "NOT_EVALUATED" in item["recommendation"]["retrofit"]


def test_no_unsupported_surface_scope(data):
    for item in active(data):
        text = json.dumps([item["title"], item["result_line"],
            [[row[field] for field in ("quantity", "value", "basis")]
             for step in item["steps"] for row in step["rows"]]]).lower()
        assert not any(term in text for term in ["iccp", "terminal anode", "buried", "attenuation"])
    assert "outside" in card(data, "fixed-jacket")["limits"].lower()


def test_pinned_reconciled_basis(data):
    basis = data["published_jacket_basis"]
    assert basis["values"]["required_count"] == 547
    assert basis["values"]["provided_count"] == 569
    assert basis["values"]["initial_output_A"] == 5.0119
    assert basis["values"]["final_output_A"] == 3.5798
    assert basis["governing_check"] == "mass"
    assert basis["assessment"] == "declared assumed reproduction"
    assert re.fullmatch(r"[a-f0-9]{40}", basis["commit"])
    assert "retrofit_quantity" not in basis


def test_rebuild_outputs_and_report_anchors(tmp_path, data):
    builder.write_outputs(data, tmp_path)
    saved = json.loads((tmp_path / "front-page.json").read_text(encoding="utf-8"))
    assert saved == data
    for item in active(data):
        html = (tmp_path / item["report"]).read_text(encoding="utf-8")
        assert html.index('id="summary"') < html.index('id="revision-history"') < html.index('id="inputs"')
        for step in item["steps"]:
            assert f'id="{step["id"]}"' in html
        assert item["result_line"] in html
        assert "NOT_EVALUATED" in html
        assert not re.search(r"[\w.+-]+@[\w.-]+\.[A-Za-z]{2,}", html)
    before = {p.relative_to(tmp_path): p.read_bytes() for p in tmp_path.rglob("*") if p.is_file()}
    builder.write_outputs(builder.build_data(ROOT), tmp_path)
    assert before == {p.relative_to(tmp_path): p.read_bytes() for p in tmp_path.rglob("*") if p.is_file()}


@pytest.mark.parametrize("kind,geometry", [
    ("monopile", {"diameter_m": 6, "height_m": 30}),
    ("manifold", {"width_m": 8, "height_m": 4}),
    ("pipeline", {"diameter_m": 0.5, "length_m": 100, "spacing_m": 20, "bracelet_length_m": 0.2}),
    ("riser", {"diameter_m": 1, "length_m": 40, "family_positions_m": {"upper": 5, "lower": 35}}),
])
def test_schematic_scale_and_dimension_leaders(kind, geometry):
    svg = drawings.render_schematic(kind, geometry)
    root = ET.fromstring(svg)
    assert float(root.attrib["data-scale-x"]) == float(root.attrib["data-scale-y"])
    assert 'class="dimension"' in svg and 'class="leader"' in svg
    assert 'fill="black"' not in svg
    assert root.find("{http://www.w3.org/2000/svg}title") is not None


@pytest.mark.parametrize("kind", ["monopile", "manifold", "pipeline", "riser"])
def test_missing_dimensions_are_not_invented(kind):
    assert "NOT_EVALUATED" in drawings.render_schematic(kind, {})


@pytest.mark.parametrize("bad", [0, -1, float("nan"), float("inf"), True])
def test_invalid_geometry_rejected(bad):
    with pytest.raises(ValueError):
        drawings.render_schematic("monopile", {"diameter_m": bad, "height_m": 30})


def test_saved_artifacts_match_current_builder(tmp_path, data):
    builder.write_outputs(data, tmp_path)
    for path in tmp_path.rglob("*"):
        if path.is_file():
            assert path.read_bytes() == (ROOT / "docs/api/cathodic-protection" / path.relative_to(tmp_path)).read_bytes()


def test_source_digests_match_bytes(data):
    import hashlib
    for source in data["references"]:
        assert source["sha256"] == hashlib.sha256((ROOT / source["path"]).read_bytes()).hexdigest()
        assert source["commit"] == data["source_commit"]


def test_changed_result_is_read_instead_of_a_typed_number(monkeypatch):
    import copy
    original = builder.Sources.read
    def read(sources, path):
        result = original(sources, path)
        if path.endswith("S02/results.json"):
            result = copy.deepcopy(result)
            result["current_demand_A"]["total_initial_A"] = 5.7
        return result
    monkeypatch.setattr(builder.Sources, "read", read)
    changed = card(builder.build_data(ROOT), "monopile")
    row = next(row for step in changed["steps"] for row in step["rows"] if row["key"] == "demand-initial")
    assert row["value"] == "5.7"


def test_nonfinite_saved_result_stops_build(monkeypatch):
    import copy
    original = builder.Sources.read
    def read(sources, path):
        result = original(sources, path)
        if path.endswith("S03/results.json"):
            result = copy.deepcopy(result)
            result["current_demand_A"]["total_mean_A"] = float("nan")
        return result
    monkeypatch.setattr(builder.Sources, "read", read)
    with pytest.raises(ValueError, match="finite"):
        builder.build_data(ROOT)


def test_pipeline_geometry_matches_source_inputs(data):
    import yaml
    cfg = yaml.safe_load((ROOT / builder.REPORTS / "cases/S06/input.yml").read_text(encoding="utf-8"))["inputs"]
    result = json.loads((ROOT / builder.REPORTS / "cases/S06/results.json").read_text(encoding="utf-8"))
    geometry = card(data, "pipeline")["schematic"]["geometry"]
    assert geometry["diameter_m"] == cfg["pipeline"]["outer_diameter_m"]
    assert geometry["length_m"] == cfg["pipeline"]["length_m"]
    assert geometry["spacing_m"] == result["anode_spacing_m"]["spacing_m"]
    assert geometry["bracelet_length_m"] == cfg["anode"]["length_m"]
    assert geometry["bracelet_thickness_m"] == cfg["anode"]["thickness_m"]


def test_dimensionless_surface_area_does_not_create_structure_dimensions(data):
    for key in ("monopile", "subsea-structure"):
        schematic = card(data, key)["schematic"]
        assert schematic["status"] == "partial"
        assert set(schematic["geometry"]) == {"anode_detail"}


def test_source_report_links_and_svg_assets_resolve(tmp_path, data):
    from html.parser import HTMLParser
    from urllib.parse import urlsplit
    class Links(HTMLParser):
        def __init__(self):
            super().__init__()
            self.targets = []
            self.ids = set()
        def handle_starttag(self, tag, attributes):
            attrs = dict(attributes)
            if "id" in attrs:
                self.ids.add(attrs["id"])
            for attribute in ("href", "src"):
                if attribute in attrs:
                    self.targets.append(attrs[attribute])
    builder.write_outputs(data, tmp_path)
    for item in active(data):
        path = tmp_path / item["report"]
        parser = Links()
        parser.feed(path.read_text(encoding="utf-8"))
        for target in parser.targets:
            parts = urlsplit(target)
            if parts.path:
                assert (path.parent / parts.path).is_file()
            if parts.fragment:
                assert parts.fragment in parser.ids


def test_schematic_length_ratio_is_physical():
    svg = ET.fromstring(drawings.render_monopile({"diameter_m": 6, "height_m": 30}))
    rectangle = svg.find("{http://www.w3.org/2000/svg}rect")
    assert float(rectangle.attrib["height"]) / float(rectangle.attrib["width"]) == pytest.approx(5)
