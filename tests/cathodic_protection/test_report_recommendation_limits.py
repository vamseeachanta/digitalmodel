"""FAIL advice distinguishes sizing evidence from monitoring evidence."""

from copy import deepcopy

import pytest

from digitalmodel.cathodic_protection.report_recommendations import (
    conclusions_block,
    recommendation_block,
)


def sizing_result():
    """Independent hand example; all quantities are arbitrary test inputs.

    DNV-RP-B401 (2021), [4.7.1] Eq. (4.2):
    M = 8 A * 10 h / (0.8 * 100 Ah/kg) = 1 kg.
    [4.8.1]-[4.8.5]: counts are ceil(1/1) = 1 by mass,
    ceil(8/2) = 4 initially, ceil(12/2) = 6 finally; max = 6.
    With 2 installed units, the addition is 6 - 2 = 4 units or 4 kg.
    Final output difference = 100 * (4 - 12) / 12 = -66.7% rounded.
    No test input is a prescribed material property or design limit.
    """
    return {
        "edition": "2021",
        "anode_requirements": {
            "individual_mass_kg": 1,
            "total_mass_kg": 1,
            "electrochemical_capacity_Ah_kg": 100,
            "utilization_factor": 0.8,
            "design_life_hours": 10,
        },
        "current_output_verification": {
            "anode_count": 2,
            "recommended_anode_count": 6,
            "mean_current_demand_A": 8,
            "initial_current_demand_A": 8,
            "final_current_demand_A": 12,
            "anode_current_output_initial_A": 2,
            "anode_current_output_final_A": 2,
            "total_anode_current_output_initial_A": 4,
            "total_anode_current_output_final_A": 4,
            "count_by_mass": 1,
            "count_by_initial_current": 4,
            "count_by_final_current": 6,
        },
    }


def test_hand_calculation_and_comparison_precision():
    result = sizing_result()
    before = deepcopy(result)
    text = recommendation_block(result, "final_current_output").markdown
    assert "additional count: 4 anodes" in text
    assert "additional net mass: 4.000 kg" in text
    assert "-66.7% relative to" in text
    assert result == before


def test_missing_sizing_does_not_suppress_monitoring():
    result = sizing_result()
    del result["anode_requirements"]["total_mass_kg"]
    text = recommendation_block(result, "final_current_output").markdown
    initial = text.split("**Initial design:**")[1].split("**Installed structure:**")[0]
    installed = text.split("**Installed structure:**")[1].split("**Retrofit:**")[0]
    assert "NOT_EVALUATED" in initial
    assert "potential survey" in installed
    assert "remote or shielded" in installed
    assert "[2.4.2]" in installed
    assert "additional count:" not in initial


@pytest.mark.parametrize("key", ["spatial_protection", "continuity"])
def test_unsupported_failure_explicitly_names_missing_method(key):
    text = recommendation_block(sizing_result(), key).markdown
    for phase in ("Initial design", "Installed structure", "Retrofit"):
        detail = text.split(f"**{phase}:**")[1].split("\n\n")[0]
        assert "NOT_EVALUATED" in detail
        assert "method" in detail
        assert "citation" in detail


def test_missing_edition_does_not_select_a_default_criterion():
    result = sizing_result()
    del result["edition"]
    text = recommendation_block(result, "final_current_output").markdown
    assert "potential survey" not in text
    assert text.count("NOT_EVALUATED") == 3


def test_supported_retrofit_names_missing_condition_data():
    text = recommendation_block(sizing_result(), "mass").markdown
    retrofit = text.split("**Retrofit:**")[1]
    assert "NOT_EVALUATED" in retrofit
    for required in ("remaining mass", "elapsed service", "current demand"):
        assert required in retrofit


@pytest.mark.parametrize("key", ["mass", "spatial_protection"])
def test_conclusions_preserve_unevaluated_disposition(key):
    result = sizing_result()
    result["status"] = {"result": "FAIL", "checks": {key: False}}
    del result["anode_requirements"]["total_mass_kg"]
    text = conclusions_block(result).markdown
    assert "NOT_EVALUATED" in text
    assert "should use the sizing quantities" not in text


def test_missing_edition_conclusion_agrees_with_detailed_recommendation():
    result = sizing_result()
    result["status"] = {"result": "FAIL", "checks": {"mass": False}}
    del result["edition"]
    assert "initial design quantity is NOT_EVALUATED" in conclusions_block(result).markdown
    assert "additional anodes" not in conclusions_block(result).markdown
