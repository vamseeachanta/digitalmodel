# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Independent arithmetic and publication contract for the synthetic hull."""

from __future__ import annotations

import copy
import hashlib
import importlib.util
import json
import math
from pathlib import Path

import pytest

from digitalmodel.citations import CitationResolutionError

ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts/reporting/build_synthetic_hull_analysis.py"
FIXTURE = ROOT / "tests/fixtures/cathodic_protection/workflow_inputs/synthetic_hull.json"
REPORT = ROOT / "docs/domains/cathodic_protection/reports/synthetic-hull/cathodic-protection-analysis.md"
RESULTS = REPORT.with_suffix(".citations.json")
CITATION_ROOT = ROOT / "tests/citations/fixtures"
REBUILD = "PYTHONPATH=src uv run --no-project --with pydantic --with loguru --with scipy python scripts/reporting/build_synthetic_hull_analysis.py"


def builder():
    spec = importlib.util.spec_from_file_location("synthetic_hull_builder", SCRIPT)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def evidence():
    return builder().calculate(citation_repo_root=CITATION_ROOT)


def test_hand_calculated_hull_demand_and_mass():
    # ASSUMED rectangular open-top hull: L=50, B=12, T=6 m.
    # External bottom + sides + ends; waterplane is excluded.
    area = 50 * 12 + 2 * 50 * 6 + 2 * 12 * 6
    assert area == 1344
    # ASSUMED coating covers 90%, with 2%/6% initial/final breakdown.
    # ASSUMED bare densities 300/100 mA/m2, dynamic fraction 1/4.
    # ABS GN Ships, December 2017, Current demand: I=A*j*f/1000.
    factors = {"initial": 0.02, "mean": 0.04, "final": 0.06}
    density = {"initial": 300, "mean": 0.25 * 300 + 0.75 * 100, "final": 300}
    expected = {
        f"I_{phase}_A": area * (0.9 * factors[phase] + 0.1) * density[phase] / 1000
        for phase in factors
    }
    result = evidence()
    assert result["zone_demand_A"]["external_hull"] == pytest.approx(expected)
    # Hand products: 1344 * (0.9*f + 0.1) * j / 1000, for each phase.
    assert result["zone_demand_A"]["external_hull"] == pytest.approx(
        {"I_initial_A": 47.5776, "I_mean_A": 27.4176, "I_final_A": 62.0928}
    )
    # ABS GN Ships, December 2017, Minimum net anode mass.
    # Capacity 2500 Ah/kg is an ASSUMED performance basis, matched by the
    # existing alloy lookup; utilisation 0.85 and life 5 yr are ASSUMED.
    # 8760 = 365 days/year * 24 hours/day (non-leap design year).
    mass = expected["I_mean_A"] * 5 * (365 * 24) / (2500 * 0.85)
    checks = result["arrangements"]["stated"]["checks"]
    assert checks[0]["required"] == pytest.approx(mass)
    # ASSUMED rectangular anode: net volume subtracts a through core.
    individual_mass = (1 * 0.2 * 0.1 - 1 * 0.002) * 2700
    assert individual_mass == pytest.approx(48.6)
    assert checks[0]["provided"] == pytest.approx(48 * individual_mass)
    assert result["arrangements"]["stated"]["count_by_check"]["mass"] == math.ceil(mass / individual_mass)


def test_independent_depleted_output_and_governing_count():
    # ABS GN Ships, December 2017, Long flush anodes / Final dimensions.
    # Arithmetic uses only the declared ASSUMED geometry and performance.
    initial_r = 0.25 / (1 + 0.2)
    remaining_mass = 48.6 * (1 - 0.85)
    depleted_length = 1 * (1 - 0.1 * 0.85)
    section = math.pi * remaining_mass / (depleted_length * 2700) + 0.002
    radius = math.sqrt(2 * section / math.pi)
    final_r = 0.25 / (depleted_length + 2 * radius)
    driving = -0.80 - (-1.09)  # ASSUMED V against Ag/AgCl seawater.
    initial_output, final_output = driving / initial_r, driving / final_r
    result = evidence()
    stated = result["arrangements"]["stated"]
    checks = {row["quantity"]: row for row in stated["checks"]}
    assert checks["Initial current output"]["provided"] == pytest.approx(48 * initial_output)
    assert checks["Final current output"]["provided"] == pytest.approx(48 * final_output)
    # Direct hand demand = 1344*(0.9*0.06+0.1)*300/1000 A.
    required = math.ceil((1344 * (0.9 * 0.06 + 0.1) * 300 / 1000) / final_output)
    assert stated["recommended_anode_count"] == required
    assert stated["governing_case"] == "final_current_output"
    assert stated["status"] == "FAIL"
    assert result["arrangements"]["rechecked"]["status"] == "PASS"
    assert result["arrangements"]["rechecked"]["anode_count"] == 2 * math.ceil(required / 2)
    for row in stated["checks"]:
        assert row["margin_percent"] == pytest.approx(100 * (row["provided"] / row["required"] - 1))
        assert row["passes"] == (row["provided"] >= row["required"])


def test_every_input_has_assumption_basis():
    cfg = json.loads(FIXTURE.read_text(encoding="utf-8"))

    def leaves(value, prefix=""):
        if isinstance(value, dict):
            for key, item in value.items():
                yield from leaves(item, f"{prefix}.{key}" if prefix else key)
        else:
            yield prefix

    paths = set(leaves({"inputs": cfg["inputs"], "hull_geometry": cfg["hull_geometry"]}))
    covered = []
    for basis in cfg["input_basis"]:
        assert basis["input_class"] == "ASSUMED"
        assert basis["basis"] and basis["unit"]
        covered.extend(basis["paths"])
    assert set(covered) == paths
    assert len(covered) == len(paths)
    assert cfg["inputs"]["structure"]["steel_total_area"] == 1344


def test_result_file_matches_fresh_run_and_reference_contract():
    result = evidence()
    stored = json.loads(RESULTS.read_text(encoding="utf-8"))
    assert_evidence_matches(stored, result)
    reference = json.loads((REPORT.parent.parent / "reference-jacket/cathodic-protection-analysis.citations.json").read_text(encoding="utf-8"))
    assert set(reference) <= set(result)
    for arrangement in result["arrangements"].values():
        for row in arrangement["checks"]:
            assert set(row) == {"quantity", "required", "provided", "unit", "margin_percent", "passes"}
    assert result["input_sha256"] == hashlib.sha256(FIXTURE.read_bytes()).hexdigest(), REBUILD
    assert result["report_sha256"] == hashlib.sha256(REPORT.read_bytes()).hexdigest(), REBUILD
    assert result["monitoring"]["status"] == "NOT_EVALUATED"
    assert result["local_potential_assessment"] == "NOT_EVALUATED"
    assert result["marketing_cleared"] is False


def test_reproduction_is_deterministic(tmp_path):
    output = builder().build(tmp_path, citation_repo_root=CITATION_ROOT)
    first = json.loads(output.read_text(encoding="utf-8"))
    assert_evidence_matches(first, json.loads(RESULTS.read_text(encoding="utf-8")))
    assert (tmp_path / REPORT.name).read_bytes() == REPORT.read_bytes(), REBUILD
    second = builder().build(tmp_path, citation_repo_root=CITATION_ROOT)
    assert_evidence_matches(first, json.loads(second.read_text(encoding="utf-8")))


def assert_evidence_matches(actual, expected):
    if isinstance(expected, dict):
        assert actual.keys() == expected.keys()
        for key, value in expected.items():
            assert_evidence_matches(actual[key], value)
    elif isinstance(expected, list):
        assert len(actual) == len(expected)
        for actual_item, expected_item in zip(actual, expected):
            assert_evidence_matches(actual_item, expected_item)
    elif isinstance(expected, float):
        assert actual == pytest.approx(expected)
    else:
        assert actual == expected, "Rebuild with: " + REBUILD


def test_missing_citation_fails_closed(tmp_path):
    with pytest.raises(CitationResolutionError, match="page_missing"):
        builder().calculate(citation_repo_root=tmp_path)


def test_area_mismatch_is_rejected():
    cfg = copy.deepcopy(json.loads(FIXTURE.read_text(encoding="utf-8")))
    cfg["inputs"]["structure"]["steel_total_area"] += 1
    with pytest.raises(ValueError, match="hull area"):
        builder().calculate(cfg, citation_repo_root=CITATION_ROOT)


def test_note_reports_mass_governed_requirement():
    cfg = json.loads(FIXTURE.read_text(encoding="utf-8"))
    cfg["inputs"]["design_data"]["design_life"] = 50
    module = builder()
    result = module.calculate(cfg, citation_repo_root=CITATION_ROOT)
    # 27.4176 A * 50 yr * 8760 h/yr / (2500 Ah/kg * 0.85) / 48.6 kg;
    # ceil gives 117 anodes; paired locations require 118.
    assert result["arrangements"]["stated"]["recommended_anode_count"] == 117
    assert result["arrangements"]["rechecked"]["anode_count"] == 118
    note = module.render_note(result)
    assert "Mean-current lifetime mass governs the aggregate requirement of 117 anodes" in note
    assert "Final current governs" not in note


@pytest.mark.parametrize("spacing,status", [(6, "PASS"), (9, "FAIL")])
def test_note_reports_spacing_against_its_limit(spacing, status):
    cfg = json.loads(FIXTURE.read_text(encoding="utf-8"))
    cfg["inputs"]["layout"]["actual_max_spacing_m"] = spacing
    module = builder()
    result = module.calculate(cfg, citation_repo_root=CITATION_ROOT)
    assert result["arrangements"]["rechecked"]["status"] == status
    note = module.render_note(result)
    assert f"rechecked arrangement has aggregate status {status}" in note
    assert f"maximum spacing {spacing:.3f} m against a limit of 8.000 m" in note
    assert f"| rechecked | spacing | True | {'True' if status == 'PASS' else 'False'} | {status} |" in note
    if status == "FAIL":
        assert "Resolve the failed general-layout checks before acceptance" in note
        assert "General spacing and layout flags pass" not in note


@pytest.mark.parametrize("flag", ["uniform_distribution_confirmed", "bilge_damage_avoided"])
def test_note_reports_failed_layout_declarations(flag):
    cfg = json.loads(FIXTURE.read_text(encoding="utf-8"))
    cfg["inputs"]["layout"][flag] = False
    module = builder()
    result = module.calculate(cfg, citation_repo_root=CITATION_ROOT)
    note = module.render_note(result)
    check = "uniform_distribution" if flag == "uniform_distribution_confirmed" else flag
    assert f"| rechecked | {check} | True | False | FAIL |" in note
    assert "rechecked arrangement has aggregate status FAIL" in note


def test_passing_trial_is_retained_without_negative_additions():
    cfg = json.loads(FIXTURE.read_text(encoding="utf-8"))
    cfg["inputs"]["layout"]["selected_locations"] = 30
    module = builder()
    result = module.calculate(cfg, citation_repo_root=CITATION_ROOT)
    assert result["arrangements"]["stated"]["status"] == "PASS"
    assert result["arrangements"]["rechecked"]["anode_count"] == 60
    assert result["recommendations"]["design_additional_anodes"] == 0
    note = module.render_note(result)
    assert "60-anode trial has aggregate status PASS" in note
    assert "Retain the 60-anode trial arrangement" in note
    assert "trial does not meet" not in note
