# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""YAML-to-report coverage for the optional offshore design supplements."""

from copy import deepcopy
from pathlib import Path

import pytest
import yaml

from digitalmodel.cathodic_protection.engine_adapter import run_cathodic_protection
from digitalmodel.cathodic_protection.report_adapters import anode_design_report
from digitalmodel.reporting import TableBlock, TextBlock, render_html


ROOT = Path(__file__).resolve().parents[2]


def base_cfg():
    return yaml.safe_load((ROOT / "tests/fixtures/cathodic_protection/workflow_inputs/jacket.yml").read_text())


def options(cfg):
    cfg["inputs"]["current_distribution"] = {"reach_m": 5, "zones": {"submerged": {
        "anode_positions_m": [[0, 0, 0], [10, 0, 0]],
        "governing_points_m": [[5, 0, 0], [0, 0, 6]],
    }}}
    cfg["inputs"]["calcareous"] = {"phase_factors": {"initial": 1, "mean": .5, "final": .8}}
    return cfg


def test_optional_results_preserve_existing_results_and_inputs():
    cfg = options(base_cfg())
    inputs = deepcopy(cfg["inputs"])
    baseline = run_cathodic_protection(base_cfg())["results"]
    result = run_cathodic_protection(cfg)["results"]
    assert cfg["inputs"] == inputs
    extras = {"current_distribution", "calcareous"}
    assert {k: v for k, v in result.items() if k not in extras} == baseline
    assert result["current_distribution"]["zones"]["submerged"]["status"] == "FLAG"
    assert set(result["current_distribution"]["zones"]) == {"submerged"}
    assert result["calcareous"]["with_calcareous"]["total_mass_kg"] == pytest.approx(
        baseline["anode_requirements"]["total_mass_kg"] / 2, abs=.01)


def test_no_options_add_no_results_or_report_sections():
    cfg = run_cathodic_protection(base_cfg())
    assert "current_distribution" not in cfg["results"]
    assert "calcareous" not in cfg["results"]
    assert [s.key for s in anode_design_report(cfg).sections] == [
        "design-basis", "current-demand", "anode-requirements", "adequacy", "references",
    ]


def test_report_contains_screen_margins_comparison_and_manual_checklist():
    cfg = run_cathodic_protection(options(base_cfg()))
    spec = anode_design_report(cfg)
    sections = {s.key: s for s in spec.sections}
    assert {"current-distribution", "electrical-continuity", "calcareous-demand"} <= sections.keys()
    checklist = sections["electrical-continuity"]
    tables = [b for b in checklist.blocks if isinstance(b, TableBlock)]
    assert len(tables[0].rows) == 3
    assert all(row[-1] == "Engineer to complete" for row in tables[0].rows)
    text = " ".join(b.markdown for s in spec.sections for b in s.blocks if isinstance(b, TextBlock))
    assert "shielding" in text
    assert "double counting" in text
    html = render_html(spec)
    for phrase in ("Reach margin", "Spacing margin", "FLAG", "Bonding of appurtenances",
                   "Anode connections", "Isolation joints", "Without calcareous", "With calcareous"):
        assert phrase in html


@pytest.mark.parametrize("key", ["calcareous", "current_distribution"])
@pytest.mark.parametrize("value", [None, False, [], "bad"])
def test_non_mapping_options_are_rejected(key, value):
    cfg = base_cfg()
    cfg["inputs"][key] = value
    with pytest.raises(ValueError, match=key):
        run_cathodic_protection(cfg)


@pytest.mark.parametrize("mode", ["components", "anode_families", "riser_base_assessment"])
def test_unsupported_modes_reject_options_instead_of_ignoring_them(mode):
    cfg = options(base_cfg())
    cfg["inputs"][mode] = [] if mode != "riser_base_assessment" else {}
    with pytest.raises(ValueError, match="single.*anode"):
        run_cathodic_protection(cfg)


def test_other_routes_reject_options():
    cfg = options(base_cfg())
    cfg["inputs"]["calculation_type"] = "DNV_RP_F103"
    with pytest.raises(ValueError, match="offshore"):
        run_cathodic_protection(cfg)


def test_worked_example_runs_to_html():
    cfg = yaml.safe_load((ROOT / "examples/cathodic_protection/screening_and_calcareous.yml").read_text())
    result = run_cathodic_protection(cfg)["results"]
    assert result["current_distribution"]["zones"]["submerged"]["status"] == "PASS"
    assert result["calcareous"]["with_calcareous"]["total_mass_kg"] == pytest.approx(547.5)
    assert result["calcareous"]["without_calcareous"]["total_mass_kg"] == pytest.approx(1095)
    assert result["status"]["result"] == "FAIL"
    assert result["current_output_verification"]["recommended_anode_count"] == 18
    assert "Electrical continuity" in render_html(anode_design_report(cfg))


def test_buried_zone_has_no_additional_deposition_credit():
    cfg = base_cfg()
    cfg["inputs"]["structure"]["zones"].append({
        "zone": "buried", "area_m2": 100, "coating_category": "bare",
    })
    cfg["inputs"]["calcareous"] = {"factor": .5}
    result = run_cathodic_protection(cfg)["results"]["calcareous"]
    zone = result["zones"]["buried"]
    assert zone["with_calcareous"] == zone["without_calcareous"]


def test_tiny_positive_demand_is_still_screened():
    cfg = base_cfg()
    cfg["inputs"]["structure"]["zones"][0]["area_m2"] = 1e-6
    cfg["inputs"]["current_distribution"] = {}
    result = run_cathodic_protection(cfg)["results"]
    assert result["current_distribution"]["zones"]["submerged"]["status"] == "FLAG"
