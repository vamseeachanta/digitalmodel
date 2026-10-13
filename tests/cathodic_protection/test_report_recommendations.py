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
