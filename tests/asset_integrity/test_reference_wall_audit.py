# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Discovery regressions for #2345; numeric acceptance is not qualification.

Inputs: issue #2345 (2026), synthetic counterexample: nominal wall 0.500,
required wall 0.470, pit wall 0.440, background 0.460 inches, pitch 1 inch.
OD 24 inches comes from pitting-validation-2026-10-09.md, Formula anchor.
The extra FCA case is a hand construction: 0.500 - 0.040 = 0.460 < 0.470.
Equality control: 0.470 - 0 = 0.470. Missing background has no minimum.

Independent expected-value bound: Rt = 44/47 > 9/10 (440 > 423).
For 0 < Rt < 1 and Mt >= 1, Rt <= Rt/(1-(1-Rt)/Mt) <= 1.
Thus every unadjusted local RSF must exceed the input RSFa, regardless of
Folias coefficients. This is algebra on the existing relation documented in
level2_engine.py, LML, citing API 579-1/ASME FFS-1 (2021), Part 5 §5.4.
It is not independent qualification of that relation or of equivalent pitting.
No licensed coefficient or table is reproduced. Background expectations use
pitting-validation-2026-10-09.md, Published cases and applicability.

The assertions deliberately record current gaps for a discovery-only issue.
When a caller gains a reference-wall gate, revise its witness and audit row;
a passing numeric verdict here must never be used for component acceptance.
"""

import numpy as np
import pandas as pd
import pytest

from digitalmodel.asset_integrity.assessment import (
    FFSComponent,
    Level1Screener,
    Level2Engine,
    assess_component,
    assess_pitting,
    assess_pitting_level2_equivalent_lta,
    characterize_pit_field,
)
from digitalmodel.asset_integrity.assessment.pitting_workflow import _assess

# Synthetic inputs and source are specified in the module docstring.
NOMINAL = 0.500
REQUIRED = 0.470
PIT = 0.440
OD = 24.0
RSFA = 0.90
PITCH = 1.0


@pytest.fixture(params=[
    pytest.param((None, 0.0, "NOT_EVALUATED", "pitting.no_background"), id="missing"),
    pytest.param((0.460, 0.0, "FAIL", "pitting.background_below_tmin"), id="below"),
    pytest.param((0.500, 0.040, "FAIL", "pitting.background_below_tmin"), id="after-fca"),
    pytest.param((0.470, 0.0, "PASS", None), id="equal"),
])
def reference_case(request):
    """Hand oracle: minimum non-pitted wall minus FCA versus required wall.

    A selected synthetic three-by-three lattice has separated corner pits.
    With no background, all cells are pits; no nominal wall is substituted.
    Background selection uses the characterization's existing population mask.
    """
    background, fca, expected, flag = request.param
    grid = pd.DataFrame(
        np.full((3, 3), PIT if background is None else background),
        index=np.arange(3) * PITCH,
    )
    grid.iloc[0, 0] = grid.iloc[2, 2] = PIT
    char = characterize_pit_field(grid, NOMINAL)
    outside = grid.to_numpy()[grid.to_numpy() >= char.pit_threshold_in]
    observed = (
        "NOT_EVALUATED" if not outside.size
        else "PASS" if outside.min() - fca >= REQUIRED else "FAIL"
    )
    assert observed == expected
    return grid, char, fca, expected, flag


def test_direct_lml_formula_has_no_reference_wall_gate(reference_case):
    """Rt bound in module docstring predicts local ACCEPT for every case."""
    grid, _, _, _, _ = reference_case
    result = Level2Engine("LML", OD, NOMINAL, REQUIRED, RSFA).evaluate(grid)
    assert result["rsf"] >= PIT / REQUIRED > RSFA
    assert result["verdict"] == "ACCEPT"
    assert result["applicability"].ok


def test_equivalent_lta_helper_does_not_establish_background(reference_case):
    """The helper receives a summary without background minimum or FCA."""
    _, char, _, _, _ = reference_case
    result = assess_pitting_level2_equivalent_lta(
        char, nominal_od_in=OD, nominal_wt_in=NOMINAL,
        t_min_in=REQUIRED, rsf_a=RSFA,
    )
    assert result["rsf"] >= PIT / REQUIRED > RSFA
    assert result["verdict"] == "ACCEPT"
    assert result["applicability"].ok


def test_pitting_convenience_fca_does_not_reach_level2(reference_case):
    """Unadjusted LTA wall remains 0.440 even with the constructed FCA."""
    grid, _, fca, _, _ = reference_case
    result = assess_pitting(
        grid, nominal_od_in=OD, nominal_wt_in=NOMINAL,
        t_min_in=REQUIRED, rsf_a=RSFA, fca_in=fca,
    )
    assert result["level1"]["fca_in"] == fca
    assert result["level2"]["equivalent_lta"]["effective_thickness_in"] == PIT
    assert result["level2"]["verdict"] == "ACCEPT"
    assert result["level2"]["applicability"].ok


@pytest.mark.parametrize("assessment_type", ["LML", "PITTING"])
def test_coordinator_numeric_verdict_is_not_reference_qualification(
    reference_case, assessment_type, monkeypatch,
):
    """Supply required wall directly; no pressure-code coefficient is tested.

    Unit pressure is a placeholder: the required-wall method is replaced with
    the issue's explicit input. Both actual orchestration branches still run.
    """
    grid, _, fca, _, _ = reference_case
    monkeypatch.setattr(Level1Screener, "t_min", lambda self: REQUIRED)
    component = FFSComponent(
        component_id="synthetic-reference-wall", design_code="B31.8",
        nominal_od_in=OD, nominal_wt_in=NOMINAL, design_pressure_psi=1.0,
        fca_in=fca, rsf_a=RSFA,
    )
    result = assess_component(component, grid, force_type=assessment_type)
    assert result.t_min_in == REQUIRED
    assert result.level1["verdict"] == "FAIL_LEVEL_1"
    assert result.level2["verdict"] == "ACCEPT"
    assert result.rsf >= PIT / REQUIRED > RSFA
    assert result.applicability.ok
    # The existing Level 1 minimum-wall failure already forces repair;
    # an all-clear applicability record still does not qualify the reference.
    assert result.verdict == "REPAIR"
    assert not result.passes
    assert result.level2["t_mm_in"] == PIT


def test_dedicated_workflow_gates_reference_independently_of_rsf(reference_case):
    """Hand background oracle governs escalation, including the equality case.

    For FCA, the local bound is (0.440-0.040)/0.470. No strength value is
    imported from a run of the implementation to predict the background flag.
    """
    grid, _, fca, expected, flag = reference_case
    _, _, l2, decision = _assess({
        "nominal_od_in": OD, "nominal_wt_in": NOMINAL, "t_min_in": REQUIRED,
        "row_spacing_in": PITCH, "fca_in": fca, "rsf_a": RSFA,
        "grid": grid.to_numpy().tolist(),
    })
    assert l2["rsf"] >= (PIT - fca) / REQUIRED
    flags = decision["applicability"]["flags"]
    if expected == "PASS":
        assert decision["verdict"] in {"ACCEPT", "MONITOR"}
        assert "pitting.no_background" not in flags
        assert "pitting.background_below_tmin" not in flags
    else:
        assert decision["verdict"] == "ESCALATE"
        assert flag in flags


def test_lml_coordinator_fca_can_leave_a_false_passing_disposition(monkeypatch):
    """Hand construction extends the issue's synthetic inputs, not a design.

    Required wall input 0.400 < measured pit 0.440 allows raw Level 1 to pass.
    FCA input 0.100 leaves background 0.460-0.100 = 0.360 < 0.400 and
    pit 0.440-0.100 = 0.340 < 0.400. Because unadjusted Rt >= 1 caps at
    unity, the existing LML engine must return RSF = 1, independent of Mt.
    No source coefficient is required for this identity. The observed ACCEPT
    records a caller gap; it does not qualify component acceptance.
    """
    required, fca, background = 0.400, 0.100, 0.460
    assert background - fca < required
    assert PIT - fca < required < PIT
    monkeypatch.setattr(Level1Screener, "t_min", lambda self: required)
    component = FFSComponent(
        component_id="synthetic-reference-wall", design_code="B31.8",
        nominal_od_in=OD, nominal_wt_in=NOMINAL, design_pressure_psi=1.0,
        fca_in=fca, rsf_a=RSFA,
    )
    grid = pd.DataFrame([[PIT, background], [background, PIT]])
    result = assess_component(component, grid, force_type="LML")
    assert result.level1["verdict"] == "ACCEPT"
    assert result.level2["verdict"] == "ACCEPT"
    assert result.rsf == 1.0
    assert result.verdict == "ACCEPT"
    assert result.applicability.ok
