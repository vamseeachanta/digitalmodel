# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Public wording, unchanged calculations and release integrity for four examples."""

import importlib
import json
import math
import stat
import sys
from html.parser import HTMLParser
from pathlib import Path
from types import SimpleNamespace

import pytest

ROOT = Path(__file__).resolve().parents[2]
REPORTS = ROOT / "docs/domains/cathodic_protection/reports"
sys.path.insert(0, str(ROOT / "scripts/reporting"))


def builder():
    return importlib.import_module("build_cp_public_examples")


def presentation_spec(project):
    from digitalmodel.reporting.spec import DocumentMeta, ReportSpec, Section, TextBlock

    spec = ReportSpec(
        document=DocumentMeta(number="B0000-RPT-001-00", revision="00",
                              title="Cathodic protection analysis", project=project,
                              client="Not applicable"),
        sections=[Section(key="summary", title="Summary", blocks=[TextBlock(markdown="Example")])],
        verification_presentation=True,
    )
    spec.provenance.add("fixture", "synthetic presentation inputs")
    return spec


@pytest.mark.parametrize("project", ["Worked example on synthetic data", "Example basis"])
def test_presentation_requires_explicit_flag_independent_of_project(project):
    from digitalmodel.reporting.engine import render_html

    spec = presentation_spec(project)
    assert "Draft for review" in render_html(spec)
    spec.worked_example_presentation = True
    page = render_html(spec)
    assert 'class="example-basis"' in page
    assert "Revision 00 — Issued for information" in page
    assert "Draft for review" not in page


def test_child_template_preserves_status_and_body_end(monkeypatch):
    from jinja2 import ChoiceLoader, DictLoader
    from digitalmodel.reporting import engine

    env = engine._environment()
    parent = env.loader.get_source(env, "verification_report.html.j2")[0]
    env.loader = ChoiceLoader([DictLoader({
        "base_verification.html.j2": parent,
        "verification_report.html.j2": (
            "{% extends 'base_verification.html.j2' %}"
            "{% set report_status = 'Issued child status' %}"
            "{% block body_end %}<aside>Child controls</aside>{% endblock %}"
        ),
    }), env.loader])
    monkeypatch.setattr(engine, "_environment", lambda: env)
    page = engine.render_html(presentation_spec("Example basis"))
    assert "Issued child status" in page
    assert "Draft for review" not in page
    assert "<aside>Child controls</aside>" in page
    assert page.index("Child controls") < page.index("</body>")


class VisibleText(HTMLParser):
    """Read rendered strings while excluding bundled scripts and styles."""

    def __init__(self):
        super().__init__()
        self.hidden = 0
        self.parts = []

    def handle_starttag(self, tag, attrs):
        if tag in {"script", "style"}:
            self.hidden += 1

    def handle_endtag(self, tag):
        if tag in {"script", "style"}:
            self.hidden -= 1

    def handle_data(self, data):
        if not self.hidden:
            self.parts.append(data)


def test_repository_citation_target_is_metadata_only():
    from digitalmodel.citations import Citation, validate_citation

    records = json.loads((REPORTS / "cases/S02/report_citations.json").read_text(encoding="utf-8"))
    citation = Citation(**records["citations"][0])
    validate_citation(citation, repo_root=ROOT)
    page = (ROOT / "knowledge" / citation.wiki_path).read_text(encoding="utf-8")
    assert "status:" in page and "https://www.dnv.com/" in page
    assert "Table" not in page and "A/m" not in page


@pytest.mark.parametrize("case_id", ["S02", "S03", "S06"])
def test_rebuilt_examples_keep_results_and_bind_release(case_id, tmp_path):
    module = builder()
    folder = module.build_case(case_id, tmp_path)
    saved = json.loads((REPORTS / "cases" / case_id / "results.json").read_text(encoding="utf-8"))
    rebuilt = json.loads((folder / "results.json").read_text(encoding="utf-8"))
    module.compare_results(saved, rebuilt)
    source_folder = REPORTS / "cases" / case_id
    assert module.read_json(folder / "verification.json") == module.read_json(source_folder / "verification.json")
    release = module.read_json(folder / "release.json")
    assert release["input_fixture"] == module.CASES[case_id][1]
    assert release["data_basis"] == "synthetic data"
    module.verify_pack(folder)
    page = (folder / "report.html").read_text(encoding="utf-8")
    reader = VisibleText()
    reader.feed(page)
    text = " ".join(reader.parts)
    first_screen = page.split('<main class="main">')[0]
    assert "Worked example on synthetic data" in first_screen
    assert "internal review draft" not in first_screen.casefold()
    for phrase in ("client", "approved for", "Draft for review"):
        assert phrase.casefold() not in text.casefold()
    # The preceding issue row is retained verbatim as history, below the new summary.
    assert "Internal review draft" in text
    assert text.index("Executive Summary") < text.index("Revision history")
    assert "Criterion" in text and "NOT_EVALUATED" in text
    assert (folder / "changes.json").is_file()
    product = module.PIPELINE_PRODUCT if case_id == "S06" else module.PRODUCT
    assert product in page
    if case_id in {"S02", "S03"}:
        assert "FAIL" in text and "Final current output" in text
        assert "[4.8.1]" in text and "May 2021" in text
        assert "increase the arrangement" in text and "Required input" in text


def test_result_comparison_rejects_numeric_and_structural_changes():
    module = builder()
    module.compare_results({"x": [1, 2.0]}, {"x": [1.0, 2]})
    for changed in ({"x": [1, 2.01]}, {"x": [1]}, {"y": [1, 2]}, {"x": [True, 2]}):
        with pytest.raises(ValueError, match="results changed"):
            module.compare_results({"x": [1, 2]}, changed)


@pytest.mark.parametrize("missing", [None, float("nan"), float("inf"), "not applicable"])
def test_advice_fails_closed_on_invalid_final_demand(missing):
    module = builder()
    results = json.loads((REPORTS / "cases/S02/results.json").read_text(encoding="utf-8"))
    results["current_output_verification"]["final_current_demand_A"] = missing
    with pytest.raises(ValueError, match="NOT_EVALUATED"):
        module.b401_checks(results)


@pytest.mark.parametrize("key", ["anode_requirements", "current_output_verification", "status"])
def test_advice_fails_closed_on_missing_record(key):
    module = builder()
    results = json.loads((REPORTS / "cases/S02/results.json").read_text(encoding="utf-8"))
    results[key] = None
    with pytest.raises(ValueError, match="NOT_EVALUATED"):
        module.b401_checks(results)


@pytest.mark.parametrize("case_id, expected, added", [("S02", 48, 23), ("S03", 64, 36)])
def test_three_hand_calculation_anchors(case_id, expected, added):
    results = json.loads((REPORTS / "cases" / case_id / "results.json").read_text(encoding="utf-8"))
    assert_hand_calculation_anchors(case_id, results, expected, added)


def assert_hand_calculation_anchors(case_id, results, expected, added):
    module = builder()
    checks = module.b401_checks(results)
    # Hand calculation: 25 * 150 = 3750 kg; 28 * 80 = 2240 kg.
    assert checks[0]["provided"] == (3750.0 if case_id == "S02" else 2240.0)
    # Independent basis: workflow_inputs/monopile.yml and manifold.yml select
    # Category III, aluminium and seawater at 8 or 6 C, respectively.
    # Public regression evidence: test_b401_tables.py spot-value and driving-
    # voltage tests; test_b401_zone_families.py Arctic current-density anchors.
    # DNV-RP-B401 (May 2021), Tables 8-1 and 8-4: density and coating basis;
    # Table 8-6 with [2.4.2]: driving voltage. These case assumptions are pinned
    # independently of saved outputs; standard selections are not qualified here.
    inputs = module.load_fixture(module.CASES[case_id][1])["inputs"]
    submerged = next(z for z in inputs["structure"]["zones"] if z["zone"] == "submerged")
    assert submerged["coating_category"] == "III"
    assert inputs["anode"]["material"] == "aluminium"
    assert inputs["environment"]["seawater_temperature_C"] == (8.0 if case_id == "S02" else 6.0)
    a, b = 0.02, 0.012
    density = 0.13 if case_id == "S02" else 0.17
    driving_voltage = 0.25
    coating = results["coating_breakdown"]["submerged"]
    assert coating["a"] == pytest.approx(a)
    assert coating["b_per_yr"] == pytest.approx(b)
    assert results["current_densities_A_m2"]["submerged"]["i_final_A_m2"] == pytest.approx(density)
    breakdown = min(1.0, a + b * inputs["design_data"]["design_life"])
    demand = submerged["area_m2"] * density * breakdown
    # Hand demand: 1200 * 0.13 * (0.02 + 0.012 * 30) = 59.28 A;
    # 850 * 0.17 * (0.02 + 0.012 * 25) = 46.24 A (Tables 8-1 and 8-4).
    assert demand == pytest.approx(59.28 if case_id == "S02" else 46.24)
    verification = results["current_output_verification"]
    assert verification["driving_voltage_V"] == pytest.approx(driving_voltage)
    assert verification["final_current_demand_A"] == pytest.approx(demand)
    # DNV-RP-B401 (May 2021), Table 8-7: depleted long stand-off resistance;
    # [4.8.1]-[4.8.5], Eq. (4.3): output = driving voltage / resistance.
    anode = inputs["anode"]
    radius = anode["radius_m"] * math.sqrt(1.0 - anode["utilization_factor"])
    length = anode["length_m"]
    assert length >= 4.0 * radius
    resistance = (inputs["environment"]["seawater_resistivity_ohm_m"] / (2.0 * math.pi * length)
                  * (math.log(4.0 * length / radius) - 1.0))
    output = driving_voltage / resistance
    # Hand output: 0.25 / 0.200184 = 1.2489 A; 0.25 / 0.343586 = 0.7276 A.
    assert output == pytest.approx(1.2489 if case_id == "S02" else 0.7276, abs=0.00005)
    assert verification["anode_resistance_final_ohm"] == pytest.approx(resistance, abs=0.0000005)
    assert verification["anode_current_output_final_A"] == pytest.approx(output, abs=0.00005)
    assert math.ceil(demand / output) == expected
    assert module.design_count(results) == expected
    # Hand calculation: 48 - 25 = 23; 64 - 28 = 36 additional anodes.
    assert module.design_count(results) - results["current_output_verification"]["anode_count"] == added


@pytest.mark.parametrize("case_id, expected, added", [("S02", 48, 23), ("S03", 64, 36)])
@pytest.mark.parametrize("field", ["a", "b_per_yr", "density", "voltage"])
def test_hand_anchors_reject_self_consistent_saved_basis_change(case_id, expected, added, field):
    module = builder()
    results = module.read_json(REPORTS / "cases" / case_id / "results.json")
    coating = results["coating_breakdown"]["submerged"]
    density = results["current_densities_A_m2"]["submerged"]
    verification = results["current_output_verification"]
    if field in {"a", "b_per_yr"}:
        coating[field] *= 1.001
    elif field == "density":
        density["i_final_A_m2"] *= 1.001
    else:
        verification["driving_voltage_V"] *= 1.001
    inputs = module.load_fixture(module.CASES[case_id][1])["inputs"]
    area = next(z["area_m2"] for z in inputs["structure"]["zones"] if z["zone"] == "submerged")
    breakdown = coating["a"] + coating["b_per_yr"] * inputs["design_data"]["design_life"]
    verification["final_current_demand_A"] = area * density["i_final_A_m2"] * breakdown
    anode = inputs["anode"]
    radius = anode["radius_m"] * math.sqrt(1.0 - anode["utilization_factor"])
    resistance = (inputs["environment"]["seawater_resistivity_ohm_m"] / (2.0 * math.pi * anode["length_m"])
                  * (math.log(4.0 * anode["length_m"] / radius) - 1.0))
    verification["anode_current_output_final_A"] = round(verification["driving_voltage_V"] / resistance, 4)
    verification["total_anode_current_output_final_A"] = (
        verification["anode_count"] * verification["anode_current_output_final_A"])
    with pytest.raises(AssertionError):
        assert_hand_calculation_anchors(case_id, results, expected, added)


def test_reference_rebuild_keeps_all_saved_numerical_evidence(tmp_path):
    module = builder()
    folder = module.build_reference(tmp_path)
    old = json.loads((REPORTS / "reference-jacket/cathodic-protection-analysis.citations.json").read_text(encoding="utf-8"))
    new = json.loads((folder / "results.json").read_text(encoding="utf-8"))
    module.compare_results(module.reference_results(old), new)
    module.verify_pack(folder)
    page = (folder / "cathodic-protection-analysis.html").read_text(encoding="utf-8")
    assert "Worked example on published geometry" in page.split('<main class="main">')[0]
    assert "synthetic" in page and "CC BY 4.0" in page


def test_unrelated_case_is_rejected_before_writing(tmp_path):
    with pytest.raises(ValueError, match="scope"):
        builder().build_case("S01", tmp_path)
    assert not list(tmp_path.iterdir())


@pytest.fixture
def install_trees(tmp_path, monkeypatch):
    module = builder()
    destination, candidate = tmp_path / "reports", tmp_path / "candidate"
    relative_folders = [Path("cases") / key for key in module.CASES] + [Path("reference-jacket")]
    for relative in relative_folders:
        for parent in (candidate, destination):
            folder = parent / relative
            folder.mkdir(parents=True)
            module.write_json(folder / "results.json", {"quantity": 1})
            content = "candidate\n" if parent == candidate else "existing\n"
            (folder / "report.html").write_text(content, encoding="utf-8", newline="\n")
            module.write_receipt(folder)
    module.write_json(destination / "portfolio-receipt.json", {"unrelated/report.html": "retained"})
    monkeypatch.setattr(module, "REPORTS", destination)
    return module, candidate, destination


def tree_bytes(folder):
    return {path.relative_to(folder).as_posix(): path.read_bytes()
            for path in folder.rglob("*") if path.is_file()}


def test_install_rejects_symlink_before_any_output_changes(install_trees, tmp_path):
    module, candidate, destination = install_trees
    external = tmp_path / "preserved.html"
    external.write_text("preserved\n", encoding="utf-8", newline="\n")
    target = destination / "cases/S02/report.html"
    target.unlink()
    target.symlink_to(external)
    module.write_receipt(target.parent)
    before = tree_bytes(destination)
    with pytest.raises(ValueError, match="unsafe"):
        module.install(candidate)
    assert external.read_text(encoding="utf-8") == "preserved\n"
    assert tree_bytes(destination) == before


@pytest.mark.parametrize("location", ["external", "internal", "missing"])
def test_install_rejects_receipt_link_before_any_output_changes(install_trees, tmp_path, location):
    module, candidate, destination = install_trees
    parent = destination if location == "internal" else tmp_path
    external = parent / "preserved.json"
    if location != "missing":
        module.write_json(external, {"unrelated/report.html": "retained"})
    receipt = destination / "portfolio-receipt.json"
    receipt.unlink()
    receipt.symlink_to(external)
    before = tree_bytes(destination)
    original = external.read_bytes() if external.exists() else None
    with pytest.raises(ValueError, match="unsafe"):
        module.install(candidate)
    assert tree_bytes(destination) == before
    assert (external.read_bytes() if external.exists() else None) == original


@pytest.mark.parametrize("root_name", ["candidate", "destination"])
def test_install_rejects_intermediate_directory_link(install_trees, root_name):
    module, candidate, destination = install_trees
    root = candidate if root_name == "candidate" else destination
    preserved = root / "preserved-cases"
    (root / "cases").rename(preserved)
    (root / "cases").symlink_to(preserved, target_is_directory=True)
    before = tree_bytes(destination)
    original = tree_bytes(preserved)
    with pytest.raises(ValueError, match="unsafe"):
        module.install(candidate)
    assert tree_bytes(destination) == before
    assert tree_bytes(preserved) == original


@pytest.mark.parametrize("location", ["candidate", "candidate/cases", "destination",
                                      "destination/cases", "destination/portfolio-receipt.json",
                                      "candidate/cases/S02/report.html", "destination/cases/S02/report.html"])
def test_install_rejects_windows_reparse_points(install_trees, monkeypatch, location):
    module, candidate, destination = install_trees
    root_name, _, relative = location.partition("/")
    root = candidate if root_name == "candidate" else destination
    target = root / relative
    original_lstat = Path.lstat

    def reparse_lstat(path, *args, **kwargs):
        details = original_lstat(path, *args, **kwargs)
        if path == target:
            tag = getattr(stat, "IO_REPARSE_TAG_MOUNT_POINT", 0xA0000003)
            return SimpleNamespace(st_mode=details.st_mode, st_reparse_tag=tag)
        return details

    before = tree_bytes(destination)
    monkeypatch.setattr(Path, "lstat", reparse_lstat)
    with pytest.raises(ValueError, match="unsafe"):
        module.install(candidate)
    assert tree_bytes(destination) == before


def test_install_updates_selected_packs_and_preserves_unrelated_receipt(install_trees):
    module, candidate, destination = install_trees
    module.install(candidate)
    receipt = module.read_json(destination / "portfolio-receipt.json")
    assert receipt.pop("unrelated/report.html") == "retained"
    assert receipt == {name: module.digest(destination / name) for name in tree_bytes(candidate)}, (
        'Receipt differs; rebuild with python scripts/reporting/build_cp_public_examples.py '
        '--output "$CANDIDATE" --install')
    for relative in [Path("cases") / key for key in module.CASES] + [Path("reference-jacket")]:
        assert tree_bytes(destination / relative) == tree_bytes(candidate / relative)
        module.verify_pack(destination / relative)
