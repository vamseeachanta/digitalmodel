# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Recommendations retain routed B401 quantities and citation evidence."""

from copy import deepcopy
import json
import re
import warnings
from pathlib import Path

import pytest

from digitalmodel.cathodic_protection import _kernels as kernel
from digitalmodel.cathodic_protection.b401_structures_phases_schema import (
    rule_citations,
)
from digitalmodel.cathodic_protection.engine_adapter import run_cathodic_protection
from digitalmodel.cathodic_protection.report_adapters import anode_design_report
from digitalmodel.reporting import TextBlock

FIXTURE = (
    Path(__file__).resolve().parents[1]
    / "fixtures/reporting/cp_anode_design_result.json"
)


def cfg():
    return json.loads(FIXTURE.read_text())


def with_count(count):
    routed = deepcopy(cfg())
    routed["inputs"]["anode"]["count"] = count
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        return run_cathodic_protection(routed)


def body(spec):
    return "\n".join(
        b.markdown for s in spec.sections for b in s.blocks if isinstance(b, TextBlock)
    )


def appendix(spec):
    return "\n".join(
        b.markdown
        for s in spec.appendices
        for b in s.blocks
        if isinstance(b, TextBlock)
    )


@pytest.mark.parametrize(
    "check", ["final_current_output", "mass", "initial_current_output"]
)
def test_every_fail_has_a_recommendation_block(check):
    routed = cfg() if check == "final_current_output" else with_count(1)
    routed["results"]["status"]["checks"] = {check: False}
    text = body(anode_design_report(routed))
    assert f"Recommendation for {check.replace('_', ' ')}" in text
    assert "Recommendation for overall status" in text
    assert text.count("**Initial design:**") == 2
    assert text.count("**Installed structure:**") == 2
    assert text.count("**Retrofit:**") == 2


def test_no_recommendation_block_when_all_pass():
    routed = with_count(173)
    assert routed["results"]["status"]["result"] == "PASS"
    text = body(anode_design_report(routed))
    assert "Recommendation for" not in text
    assert "No check fails." in text


def test_initial_design_quantity_equals_route_method():
    routed = cfg()
    result = routed["results"]
    ver, req = result["current_output_verification"], result["anode_requirements"]
    counts = [kernel.anode_count(req["total_mass_kg"], req["individual_mass_kg"])]
    for phase in ["initial", "final"]:
        counts.append(
            kernel.anodes_for_current(
                ver[f"{phase}_current_demand_A"], ver[f"anode_current_output_{phase}_A"]
            )
        )
    needed = max(counts)
    assert needed == ver["recommended_anode_count"]
    assert needed * ver["anode_current_output_final_A"] >= ver["final_current_demand_A"]
    extra = needed - ver["anode_count"]
    text = body(anode_design_report(routed))
    assert f"additional count: {extra} anodes" in text
    assert f"additional net mass: {extra * req['individual_mass_kg']:.3f} kg" in text
    assert "ceil(208.000 A / 1.2091 A per anode) = 173 anodes" in text


def test_retrofit_is_not_evaluated_without_required_data():
    text = appendix(anode_design_report(cfg()))
    assert "retrofit timing not evaluated" in text
    assert "retrofit quantity not evaluated" in text
    for item in [
        "installed count",
        "installation date",
        "remaining mass",
        "measured potentials",
        "current demand",
    ]:
        assert item in text


def test_recommendation_citations_exist_in_repository():
    routed = cfg()
    text = body(anode_design_report(routed))
    held = routed["results"]["citations"] + rule_citations("2021")
    labels = re.findall(r"`(dnv-rp-b401[^`]+)`", text)
    assert labels
    assert set(labels) <= set(held)
    assert "[2.4.2]" in text
    assert "[4.8.1]" in text


def test_conclusions_summarise_each_fail():
    routed = cfg()
    routed["results"]["status"]["checks"]["mass"] = False
    text = body(anode_design_report(routed)).split("Conclusions and recommendations")[
        -1
    ]
    assert "Conclusions and recommendations" in body(anode_design_report(routed))
    for name in ["overall status", "mass", "final current output"]:
        assert f"Recommendation for {name}" in text
    assert "118 additional anodes" in text
    assert "potential survey" in text
    assert "Retrofit" in text


def test_comparisons_are_percentage_differences():
    spec = anode_design_report(cfg())
    added = "\n".join(
        b.markdown
        for s in spec.sections
        for b in s.blocks
        if isinstance(b, TextBlock) and "Recommendation for" in b.markdown
    )
    assert "% relative to" in added
    assert not re.search(r"\btimes\b|\bfactor of\b|\d\s*x\s*\d", added)


def test_added_caveats_sit_in_the_appendix():
    spec = anode_design_report(cfg())
    for statement in [
        "the recommended arrangement is not re-checked by this calculation",
        "retrofit timing not evaluated",
        "citation not held in the repository",
    ]:
        assert statement in appendix(spec)
        assert statement not in body(spec)
    assert "Appendix A" in body(spec)


@pytest.mark.parametrize("edition", ["2005", "2010", "2017", "2021"])
def test_recommendation_sources_are_selected_by_key(edition, monkeypatch):
    """Reordering a citation register cannot change its engineering meaning."""
    from digitalmodel.cathodic_protection import report_recommendations as recommendations

    routed = cfg()
    routed["results"]["edition"] = edition
    expected = rule_citations(edition)
    monkeypatch.setattr(recommendations, "rule_citations", lambda selected: list(reversed(rule_citations(selected))))
    text = recommendations.recommendation_block(routed["results"], "final_current_output").markdown
    mass = next(c for c in expected if c.endswith("(anode mass)"))
    output = next(c for c in expected if c.endswith("(output adequacy)"))
    assert f"anodes (`{mass}`)" in text
    assert f"anodes (`{output}`)" in text


def test_verified_options_recheck_count_and_consistent_geometry() -> None:
    from digitalmodel.cathodic_protection.report_recommendations import arrangement_options
    from digitalmodel.reporting import TableBlock

    routed = cfg()
    blocks = arrangement_options(routed["inputs"], routed["results"])
    tables = [b for b in blocks if isinstance(b, TableBlock)]
    assert len(tables) == 2
    count_rows = tables[0].rows
    assert count_rows[0][1:3] == [55, 173]
    assert count_rows[-1][1:3] == ["FAIL", "PASS"]
    assert count_rows[3][2] == pytest.approx(173 * routed["results"]["current_output_verification"]["anode_current_output_final_A"], abs=.01)
    geometry = tables[1].rows
    candidate = deepcopy(routed["inputs"])
    candidate["anode"]["count"] = 55
    candidate["anode"]["length_m"] = geometry[1][2]
    candidate["anode"]["individual_anode_mass_kg"] = geometry[2][2]
    candidate["anode"].pop("radius_m", None)
    checked = run_cathodic_protection({"inputs": candidate})["results"]
    assert checked["status"]["result"] == "PASS"
    assert geometry[3][2] == pytest.approx(kernel.equivalent_radius_from_mass(geometry[2][2], geometry[1][2]), abs=.000001)
    text = " ".join(b.markdown for b in blocks if isinstance(b, TextBlock))
    assert "remote or shielded" in text
    assert "more positive than -0.800 V" in text
    assert "[b401-retrofit]" in text
    assert "118" in text and "23600 kg" in text


@pytest.mark.parametrize("edition", ["2005", "2010", "2017", "2021"])
def test_verified_options_use_edition_specific_retrofit_subject(edition):
    from digitalmodel.cathodic_protection.report_recommendations import arrangement_options

    routed = cfg()
    routed["results"]["edition"] = edition
    routed["inputs"]["design_data"]["edition"] = edition
    text = " ".join(b.markdown for b in arrangement_options(routed["inputs"], routed["results"])
                    if isinstance(b, TextBlock))
    assert ("retrofit evidence" if edition == "2021" else "retrofit documentation") in text
    assert "published-jacket-paper" not in text


def test_verified_options_preserve_passing_count_and_selected_source(monkeypatch):
    from digitalmodel.cathodic_protection import report_recommendations as recommendations
    from digitalmodel.reporting import TableBlock

    routed = with_count(200)
    held = rule_citations("2021")
    reordered = list(reversed(held))
    monkeypatch.setattr(recommendations, "rule_citations", lambda edition: reordered)
    blocks = recommendations.arrangement_options(routed["inputs"], routed["results"])
    assert blocks == []
    text = " ".join(b.markdown for b in blocks if isinstance(b, TextBlock))
    for phrase in ["adds 0 units", "allowance of 0 units", "Anode geometry alternative"]:
        assert phrase not in text
