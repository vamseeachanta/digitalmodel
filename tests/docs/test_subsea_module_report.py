# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Rebuild the client report and reconcile its numbers to the public API."""

import importlib.util
import json
import math
from pathlib import Path

import pytest
from bs4 import BeautifulSoup

ROOT = Path(__file__).resolve().parents[2]
BUILDER = ROOT / "scripts/generate_subsea_module_report.py"
REPORT = ROOT / "docs/reports/modules/subsea/subsea-module-report.html"
COATED_INPUTS = {
    "outer_diameter": 0.508, "wall_thickness": 0.025,
    "coating_thickness": 0.050, "density_steel": 7850,
    "coating_density": 700, "density_contents": 850, "density_water": 1025,
    "gravity": 9.81, "water_depth": 1000.0,
    "horizontal_offset": 500.0, "length": 1500.0,
}


def builder():
    spec = importlib.util.spec_from_file_location("subsea_report_builder", BUILDER)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_report_rebuild_and_every_example_number(tmp_path):
    module = builder()
    target = tmp_path / "subsea-module-report.html"
    module.build_report(target)
    assert target.read_bytes() == REPORT.read_bytes()
    soup = BeautifulSoup(target.read_text(encoding="utf-8"), "html.parser")
    inputs, riser, result, weight = module.calculate_example()
    # Re-evaluate through public APIs, independently of template substitutions.
    from digitalmodel.subsea.catenary_riser import (
        EffectiveWeightCalculator, SimpleCatenaryAnalyzer,
    )
    direct = SimpleCatenaryAnalyzer().analyze_riser(
        riser, water_depth=inputs["water_depth"],
        horizontal_offset=inputs["horizontal_offset"],
    )
    direct_weight = EffectiveWeightCalculator().calculate(riser)
    expected = module.example_numbers(inputs, riser, direct, direct_weight)
    cells = soup.select("[data-number]")
    assert {cell["data-number"] for cell in cells} == set(expected)
    for cell in cells:
        key = cell["data-number"]
        assert float(cell["data-value"]) == expected[key]
        assert cell.get_text() == module.format_number(key, expected[key])
    # No hand-entered numerical literals can enter worked-example prose.
    worked = soup.select_one("#worked-example")
    for cell in worked.select("[data-number]"):
        cell.decompose()
    assert not any(char.isdigit() for char in worked.get_text())
    assert result.converged and weight.effective_weight > 0


def test_public_reference_and_curve_arrays_match_module():
    module = builder()
    reference = module.public_reference()
    assert reference["offset_m"] == pytest.approx(reference["expected_offset_m"], abs=1e-7)
    assert reference["length_m"] == pytest.approx(reference["expected_length_m"], abs=1e-7)
    maximum_difference = max(abs(reference["offset_difference_pct"]), abs(reference["length_difference_pct"]))
    assert reference["difference_bound_pct"] == math.ceil(maximum_difference * 10) / 10
    assert maximum_difference <= reference["difference_bound_pct"]
    data = module.chart_data()
    inputs, riser, result, _ = module.calculate_example()
    from digitalmodel.subsea.catenary_riser import SimpleCatenaryAnalyzer
    analyzer = SimpleCatenaryAnalyzer()
    arc, tension = analyzer.calculate_tension_along_riser(result, len(data["arc_m"]))
    assert data["arc_m"] == list(arc)
    assert data["tension_kN"] == list(tension / 1000)
    assert data["length_limit_m"] == riser.length
    assert max(data["required_length_m"]) == pytest.approx(riser.length)
    for offset, required in zip(data["offset_m"], data["required_length_m"], strict=True):
        checked = analyzer.analyze_riser(riser, water_depth=inputs["water_depth"], horizontal_offset=offset)
        assert required == checked.arc_length
    assert math.isfinite(data["offset_limit_m"])


def test_house_style_sections_sources_and_offline_assets():
    module = builder()
    html = REPORT.read_text(encoding="utf-8")
    assert all(line.rstrip() == line for line in html.splitlines())
    soup = BeautifulSoup(html, "html.parser")
    assert "hs-doc" in soup.body["class"]
    assert len(soup.select("main > section")) == 10
    assert not soup.select("h4, h5, h6, b, strong, script[src], link[rel=stylesheet]")
    assert soup.select_one("#revision-history")
    assert soup.select_one("#comment-text") and soup.select_one("#save-comments")
    assert soup.select_one("#changes-panel")
    assert "showDirectoryPicker" in soup.get_text() or "showDirectoryPicker" in str(soup)
    for link in soup.select('a[href^="#"]'):
        assert soup.find(id=link["href"][1:]), link
    exemplar = (ROOT / module.EXEMPLAR).read_text(encoding="utf-8")
    import re
    style = re.search(r"<style>(.*?)</style>", exemplar, re.S).group(1)
    assert style.strip() in str(soup)
    changes = json.loads((REPORT.parent / "changes.json").read_text())
    assert changes["revisions"][0]["revision"] == "00"


def test_visible_client_text_uses_engineering_register():
    soup = BeautifulSoup(REPORT.read_text(encoding="utf-8"), "html.parser")
    for node in soup.select("script, style"):
        node.decompose()
    text = soup.get_text(" ", strip=True)
    import re
    assert not re.search(r"\b(owner|agent|lane|job|could|should|would|certified|compliant)\b", text, re.I)
    assert not re.search(r"(?:/(?:home|mnt)/|[\w.+-]+@[\w.-]+\.[A-Za-z]{2,})", text)


def test_missing_design_limits_never_appear_as_acceptance():
    soup = BeautifulSoup(REPORT.read_text(encoding="utf-8"), "html.parser")
    assert "Strength and fatigue acceptance are not established" in soup.get_text()
    assert "NOT_EVALUATED" in soup.get_text()
    assert "ASSUMED - to be confirmed" in soup.get_text()


@pytest.mark.parametrize("replacement", ["OTHER_MATERIAL", "factory()"])
def test_changed_documented_material_is_rejected(tmp_path, monkeypatch, replacement):
    module = builder()
    original = (ROOT / module.README).read_text()
    example = tmp_path / "example.md"
    example.write_text(original.replace("material=STEEL_API_5L_X65,", f"material={replacement},"))
    monkeypatch.setattr(module, "README", example)
    with pytest.raises(ValueError, match="documented input"):
        module.example_inputs()


def test_missing_house_style_is_reported(tmp_path, monkeypatch):
    module = builder()
    exemplar = tmp_path / "exemplar.html"
    exemplar.write_text("<html><body>No stylesheet.</body></html>")
    monkeypatch.setattr(module, "EXEMPLAR", str(exemplar))
    with pytest.raises(ValueError, match="stylesheet"):
        module.build_report(tmp_path / "report.html")


def test_generator_registry_rebuilds_report_in_isolation(tmp_path):
    from scripts import check_generated_html as checker

    matches = [entry for entry in checker.GENERATORS
               if entry.script == BUILDER.relative_to(ROOT).as_posix()]
    assert len(matches) == 1, "The report generator must have registered ownership"
    entry = matches[0]
    checker.prepare_shadow(ROOT, tmp_path)
    checker.clear_generated_html(tmp_path, entry)
    assert checker.run_generator(tmp_path, entry) is None
    assert (tmp_path / REPORT.relative_to(ROOT)).read_bytes() == REPORT.read_bytes()
    # A new inventory file must change regenerated bytes, without touching
    # either the retained calculation evidence or the original report.
    extra = tmp_path / "tests/subsea/catenary_riser/test_inventory_probe.py"
    extra.write_text("# Inventory probe.\n", encoding="utf-8")
    assert checker.run_generator(tmp_path, entry) is None
    assert (tmp_path / REPORT.relative_to(ROOT)).read_bytes() != REPORT.read_bytes()
    extra.unlink()
    mutations = {
        "scripts/templates/subsea_module_report.html": ("7.2 Independent", "7.2 Separate"),
        "docs/api/_assets/brand.css": ("#0b3d91", "#ffffff"),
        "docs/reports/modules/subsea/changes.json": ("First issue:", "Initial issue:"),
    }
    for relative, (old, new) in mutations.items():
        source = tmp_path / relative
        original = source.read_text(encoding="utf-8")
        assert old in original
        source.write_text(original.replace(old, new), encoding="utf-8")
        assert checker.run_generator(tmp_path, entry) is None
        assert (tmp_path / REPORT.relative_to(ROOT)).read_bytes() != REPORT.read_bytes()
        source.write_text(original, encoding="utf-8")
    source = tmp_path / "docs/reports/modules/subsea/validation.json"
    record = json.loads(source.read_text(encoding="utf-8"))
    record["failed"] = 1
    source.write_text(json.dumps(record), encoding="utf-8")
    assert "Selected validation tests did not pass" in checker.run_generator(tmp_path, entry)


def coated_worksheet(offset=500.0):
    """Arithmetic worksheet inputs fixed at revision 01 of the report.

    Basis: README Python example; models.py densities/gravity.

    Annular mass/displacement balance and Catenary Curve (2019), equations
    (1), (3), (4), (9), (10), (34):
    https://www.mygeodesy.id.au/documents/Catenary%20Curve.pdf
    No production analyzer, model property, input parser or formatter is used.
    """
    inputs = COATED_INPUTS
    outside = inputs["outer_diameter"]
    bore = outside - 2 * inputs["wall_thickness"]
    coated = outside + 2 * inputs["coating_thickness"]
    gravity = inputs["gravity"]
    steel = math.pi / 4 * (outside**2 - bore**2) * inputs["density_steel"] * gravity
    coating = math.pi / 4 * (coated**2 - outside**2) * inputs["coating_density"] * gravity
    contents = math.pi / 4 * bore**2 * inputs["density_contents"] * gravity
    buoyancy = math.pi / 4 * coated**2 * inputs["density_water"] * gravity
    submerged = steel + coating + contents - buoyancy
    # Solve the dimensionless geometry equation for u=x/a, independently
    # of the production horizontal-force solver and its force bracket.
    lower, upper = 0.1, 10.0
    depth, length = inputs["water_depth"], inputs["length"]
    assert (math.cosh(lower) - 1) / lower < depth / offset < (math.cosh(upper) - 1) / upper
    for _ in range(100):
        u = (lower + upper) / 2
        if (math.cosh(u) - 1) / u > depth / offset:
            upper = u
        else:
            lower = u
    a = offset / ((lower + upper) / 2)
    arc = a * math.sinh(offset / a)
    horizontal = submerged * a
    boundary_a = (length**2 - depth**2) / (2 * depth)
    return {
        "steel_weight": steel, "coating_weight": coating,
        "contents_weight": contents, "buoyancy": buoyancy,
        "effective_weight": submerged, "catenary_parameter": a,
        "arc_length": arc, "grounded_length": max(0, length - arc),
        "horizontal_tension_kN": horizontal / 1000,
        "top_tension_kN": math.hypot(horizontal, submerged * arc) / 1000,
        "offset_limit_m": boundary_a * math.acosh(1 + depth / boundary_a),
    }


def assert_in_offset_bounds(actual, key, offset=500.0):
    """Propagate the solver's offset residual through independent equations."""
    bounds = [coated_worksheet(offset * factor)[key] for factor in (1 - 1e-6, 1 + 1e-6)]
    assert min(bounds) - 1e-8 <= actual <= max(bounds) + 1e-8, (key, actual, bounds)


def test_report_matches_independent_coated_worksheet():
    soup = BeautifulSoup(REPORT.read_text(encoding="utf-8"), "html.parser")
    worksheet = soup.select_one("#coated-worksheet")
    assert worksheet is not None, "A cited independent coated worksheet is required"
    assert worksheet.select_one('a[href="#source-curve"]')
    expected = coated_worksheet()
    inputs = dict(COATED_INPUTS, bore_diameter=0.458, coated_diameter=0.608)
    displayed_inputs = worksheet.select("[data-worksheet-input]")
    assert {cell["data-worksheet-input"] for cell in displayed_inputs} == set(inputs)
    for cell in displayed_inputs:
        assert float(cell.get_text()) == pytest.approx(inputs[cell["data-worksheet-input"]], abs=1e-12)
    rows = worksheet.select("[data-worksheet]")
    assert {row["data-worksheet"] for row in rows} == set(expected)
    for row in rows:
        key = row["data-worksheet"]
        assert float(row.get_text().replace(",", "")) == round(expected[key], 1)
    cells = soup.select("[data-number]")
    assert set(expected) | set(COATED_INPUTS) <= {cell["data-number"] for cell in cells}
    for cell in cells:
        key = cell["data-number"]
        if key in expected:
            assert_in_offset_bounds(float(cell["data-value"]), key)
        elif key in COATED_INPUTS:
            assert float(cell["data-value"]) == pytest.approx(COATED_INPUTS[key], abs=1e-12)
    chart = json.loads(soup.select_one("#chart-data").get_text())
    assert chart["offset_limit_m"] == pytest.approx(expected["offset_limit_m"], abs=1e-8)
    assert_in_offset_bounds(chart["arc_m"][-1], "arc_length")
    horizontal_bounds = [coated_worksheet(500 * factor)["horizontal_tension_kN"] * 1000
                         for factor in (1 - 1e-6, 1 + 1e-6)]
    for arc, tension in zip(chart["arc_m"], chart["tension_kN"], strict=True):
        bounds = [math.hypot(force, expected["effective_weight"] * arc) / 1000
                  for force in horizontal_bounds]
        assert min(bounds) - 1e-8 <= tension <= max(bounds) + 1e-8
    for offset, length in zip(chart["offset_m"], chart["required_length_m"], strict=True):
        assert_in_offset_bounds(length, "arc_length", offset)


@pytest.mark.parametrize("factor", [1 - 1e-6, 1 + 1e-6])
def test_available_length_boundary_behavior(factor):
    from digitalmodel.subsea.catenary_riser import SimpleCatenaryAnalyzer

    _, riser, _, _ = builder().calculate_example()
    offset = coated_worksheet()["offset_limit_m"] * factor
    if factor < 1:
        result = SimpleCatenaryAnalyzer().analyze_riser(riser, horizontal_offset=offset)
        assert result.converged and result.arc_length <= COATED_INPUTS["length"]
    else:
        with pytest.raises(ValueError, match="available riser length"):
            SimpleCatenaryAnalyzer().analyze_riser(riser, horizontal_offset=offset)


def test_shadow_copies_registered_inventory_directory(tmp_path, monkeypatch):
    from scripts import check_generated_html as checker

    repo, shadow = tmp_path / "repo", tmp_path / "shadow"
    for relative in ("scripts", "docs/api", "tests/subsea/example"):
        (repo / relative).mkdir(parents=True)
    inventory = repo / "tests/subsea/example/test_geometry.py"
    inventory.write_text("# Inventory input.\n", encoding="utf-8")
    (inventory.parent / "stale.pyc").write_bytes(b"bytecode")
    (inventory.parent / "__pycache__").mkdir()
    (inventory.parent / "__pycache__/stale.pyc").write_bytes(b"bytecode")
    (shadow / "tests/subsea/example").mkdir(parents=True)
    entry = checker.Generator("scripts/build.py", inputs=("tests/subsea",))
    monkeypatch.setattr(checker, "GENERATORS", (entry,))
    checker.prepare_shadow(repo, shadow)
    copied = shadow / inventory.relative_to(repo)
    assert copied.is_file(), "Registered inventory directories must be copied"
    assert copied.read_bytes() == inventory.read_bytes()
    assert not copied.is_symlink()
    assert not (copied.parent / "stale.pyc").exists()
    assert not (copied.parent / "__pycache__").exists()


def test_registry_rejects_missing_declared_input(tmp_path, monkeypatch):
    from scripts import check_generated_html as checker

    entry = checker.Generator("scripts/build.py", inputs=("missing-input.json",))
    monkeypatch.setattr(checker, "GENERATORS", (entry,))
    assert any("missing generator input: missing-input.json" in error
               for error in checker.validate_registry(tmp_path))
