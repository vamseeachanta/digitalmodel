"""Synthetic CP portfolio modes retain explicit phase and zone evidence."""

from pathlib import Path
from typing import Any, cast

import pytest
import yaml  # type: ignore[import-untyped]

from digitalmodel.cathodic_protection.engine_adapter import run_cathodic_protection

FIXTURES = Path(__file__).parents[1] / "fixtures/cathodic_protection/workflow_inputs"


def _load(name: str) -> dict[str, Any]:
    return cast(dict[str, Any], yaml.safe_load((FIXTURES / name).read_text("utf-8")))


def test_temporary_service_has_explicit_half_year_phase_and_mass() -> None:
    cfg = _load("temporary_service.yml")
    phases = cfg["inputs"]["riser_base_assessment"]["phases"]
    assert len(phases) == 1
    assert phases[0]["type"] == "temporary"
    assert phases[0]["life_years"] == 0.5
    result = run_cathodic_protection(cfg)["results"]
    assessment = result["riser_base_assessment"]
    assert set(assessment["components"]) == {"base"}
    assert "base::frame" in result["current_demand_A"]
    assert len(assessment["phases"]) == 1
    assert set(assessment["phases"][0]["families"]) == {"sea"}
    family = assessment["phases"][0]["families"]["sea"]
    # B401:2021 Sec. 4.7.1: 2 A * 0.5 y * 8760 h/y / 2000 Ah/kg.
    assert family["consumed_mass_kg"] == pytest.approx(4.38)


def test_mixed_family_has_separate_seawater_buried_and_reinforcement_evidence() -> None:
    cfg = _load("mixed_family.yml")
    assert {zone["base_zone"] for zone in cfg["inputs"]["structure"]["zones"]} == {
        "submerged", "buried", "concrete_embedded",
    }
    result = run_cathodic_protection(cfg)["results"]
    demand = result["current_demand_A"]
    expected = {"clamp_steel": 1.1, "buried_frame": 0.4, "mattress_reinforcement": 0.06}
    # B401:2021 Tables 8-2/8-3 and buried density: 10*.11, 20*.02, 100*.0006.
    for zone, current in expected.items():
        assert demand[zone]["I_mean_A"] == pytest.approx(current)
    assert demand["mattress_reinforcement"]["area_basis"] == "reinforcement_steel"
    families = result["anode_families"]
    assert set(families) == {"sea", "mud"}
    assert families["sea"]["current_demand_A"]["mean"] == pytest.approx(1.1)
    assert families["mud"]["current_demand_A"]["mean"] == pytest.approx(0.46)
