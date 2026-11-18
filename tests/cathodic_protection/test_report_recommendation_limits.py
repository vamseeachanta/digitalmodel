"""FAIL advice distinguishes sizing evidence from monitoring evidence."""

from copy import deepcopy
import json
from pathlib import Path

import pytest

from digitalmodel.cathodic_protection.report_adapters import anode_design_report
from digitalmodel.cathodic_protection.report_recommendations import (
    conclusions_block,
    recommendation_block,
    recommendation_caveats,
)
from digitalmodel.reporting import render_html


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
    assert "remote or shielded" not in installed
    assert "locations distant from anodes" not in installed
    assert "[2.4.2]" in installed
    assert "additional count:" not in initial


@pytest.mark.parametrize("key", ["spatial_protection", "continuity"])
def test_unsupported_failure_explicitly_names_missing_method(key):
    text = recommendation_block(sizing_result(), key).markdown
    for phase in ("Initial design", "Retrofit"):
        detail = text.split(f"**{phase}:**")[1].split("\n\n")[0]
        assert "NOT_EVALUATED" in detail
        assert "method" in detail
        assert "citation" in detail
    installed = text.split("**Installed structure:**")[1].split("**Retrofit:**")[0]
    assert "potential survey" in installed
    assert "[2.4.2]" in installed
    assert "NOT_EVALUATED" not in installed


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


@pytest.mark.parametrize("key", ["mass", "spatial_protection"])
def test_missing_edition_conclusion_agrees_with_detailed_recommendation(key):
    result = sizing_result()
    result["status"] = {"result": "FAIL", "checks": {key: False}}
    del result["edition"]
    text = conclusions_block(result).markdown
    assert "initial design quantity is NOT_EVALUATED" in text
    assert "additional anodes" not in text
    assert "potential survey" not in text
    assert (
        "installed-structure monitoring is NOT_EVALUATED (edition required)" in text
    )


@pytest.mark.parametrize("edition", ["2005", "2010"])
def test_older_edition_monitoring_explains_citation_source(edition):
    result = sizing_result()
    result["edition"] = edition
    text = recommendation_block(result, "mass").markdown
    assert "dnv-rp-b401 2011 Sec. 5" in text
    assert "October 2010 protective-potential source (printed 2011)" in text
    if edition == "2005":
        assert "2005 with 2008 amendments uses the" in text


def test_spatial_failure_conclusion_retains_cited_monitoring():
    result = sizing_result()
    result["status"] = {"result": "FAIL", "checks": {"spatial_protection": False}}
    text = conclusions_block(result).markdown.split(
        "Recommendation for spatial protection:"
    )[1]
    assert "potential survey" in text
    assert "initial design quantity is NOT_EVALUATED" in text
    assert "Retrofit quantity and timing: NOT_EVALUATED" in text


@pytest.mark.parametrize("missing_sizing", [False, True])
@pytest.mark.parametrize("missing_routed_citations", [False, True])
def test_rendered_monitoring_caveats_agree_with_available_criterion(
    missing_sizing, missing_routed_citations
):
    fixture = (
        Path(__file__).resolve().parents[1]
        / "fixtures/reporting/cp_anode_design_result.json"
    )
    cfg = json.loads(fixture.read_text(encoding="utf-8"))
    results = cfg["results"]
    if missing_sizing:
        del results["anode_requirements"]["total_mass_kg"]
    if missing_routed_citations:
        results["current_output_verification"].pop("citations", None)
    before = deepcopy(cfg)

    html = render_html(anode_design_report(cfg))

    assert "potential survey" in html
    assert "monitoring applicability are not evaluated" not in html
    assert "Monitoring citation not held" not in html
    assert "Monitoring interval and depletion action trigger are not evaluated" in html
    if missing_sizing:
        assert "Initial-design quantities are not evaluated" in html
        assert "additional count:" not in html
    assert cfg == before


@pytest.mark.parametrize("edition", [None, "unsupported"])
def test_monitoring_caveats_require_a_resolvable_edition_even_with_routed_citation(
    edition,
):
    results = sizing_result()
    results["edition"] = edition
    results["status"] = {"result": "FAIL", "checks": {"mass": False}}
    results["current_output_verification"]["citations"] = ["protective potential"]

    caveats = " ".join(recommendation_caveats(results))

    assert "Installed-structure monitoring is not evaluated" in caveats
    assert "edition" in caveats
    assert "Initial-design quantities are not evaluated" in caveats


def test_unsupported_check_caveat_limits_only_initial_design_and_retrofit():
    results = sizing_result()
    results["status"] = {"result": "FAIL", "checks": {"spatial_protection": False}}

    caveats = " ".join(recommendation_caveats(results))

    assert "Initial-design and retrofit recommendations for additional check types" in caveats
    assert "Recommendations for additional check types are not evaluated" not in caveats
    assert "Installed-structure monitoring is not evaluated" not in caveats
