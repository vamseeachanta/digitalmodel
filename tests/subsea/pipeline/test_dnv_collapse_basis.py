# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Collapse input-basis and consumer regressions.

Equation source: EPRG, Deeper, Longer and Thicker: Challenges for the Design
of Subsea Pipelines (2022), section 3.1, equation (3). Component definitions
are retained from pipeline_pressure_dnv.py, which cites TU Delft, Local
Buckling and Collapse of Thick Walled pipe (2016), equations (3.9) and (3.10).
That institutional copy timed out during this change; its content is not newly
qualified here. Inputs are synthetic arithmetic examples, not code limits.
The relative tolerance 1e-10 tests arithmetic only: it is looser than binary64
rounding and does not define engineering acceptance.
"""

import runpy
from pathlib import Path

import pytest

from digitalmodel.subsea.pipeline.pipeline_pressure import (
    PipeMaterial,
    WallThicknessSizingWorkflow,
    dnv_combined_loading_check,
    dnv_external_collapse_check,
)


def test_combined_loading_accepts_explicit_elastic_properties():
    """Independent substitution gives Pc=28 MPa and pressure ratio=-0.5.

    D=1 m, t=0.05 m, fy=350 MPa, E=144000 MPa, nu=0, f0=0.005.
    Pel=2*144000*0.05**3=36 MPa; Pp=2*350*0.05=35 MPa.
    (28-36)*(28**2-35**2)=3528=28*36*35*0.005/0.05.
    With zero mechanical loads, pi=0 and pe=14 MPa, U=abs(-14/28)=0.5.
    The unity comparison is the existing screen, not a qualified code check.
    """
    result = dnv_combined_loading_check(
        D=1.0, t=0.05, mat=PipeMaterial("synthetic", 350.0, 400.0),
        M_Sd=0.0, S_Sd=0.0, p_li=0.0, p_e=14.0,
        f_0=0.005, E=144000.0, nu=0.0,
    )
    assert result["p_c"] == pytest.approx(28.0, rel=1e-10)
    assert result["pressure_ratio"] == pytest.approx(-0.5, rel=1e-10)
    assert result["utilization"] == pytest.approx(0.5, rel=1e-10)
    assert result["pass"] is True


@pytest.mark.parametrize("check", ["external", "combined"])
@pytest.mark.parametrize("demand", [0.0, 100000.0])
def test_screen_does_not_claim_standard_compliance(check, demand):
    """Neither zero demand nor overwhelming demand qualifies undocumented factors.

    Chosen depth [m] or external pressure [MPa] is zero or 100000: the latter
    exceeds the synthetic pipe's capacity by either route. No acceptance value
    is inferred from the resulting legacy pass flag.
    """
    inputs = dict(D=1.0, t=0.05, mat=PipeMaterial("synthetic", 350.0, 400.0))
    if check == "external":
        result = dnv_external_collapse_check(depth=demand, **inputs)
    else:
        result = dnv_combined_loading_check(
            M_Sd=0.0, S_Sd=0.0, p_li=0.0, p_e=demand, **inputs,
        )
    assert result["pass"] is (demand == 0.0)
    assert result["standard_compliance"] == "NOT_EVALUATED"
    assert "fabrication_factor" in result["not_evaluated"]
    assert "strength_derating" in result["not_evaluated"]
    assert "ovality_and_thickness_basis" in result["not_evaluated"]
    assert "design_criterion_and_factors" in result["not_evaluated"]


@pytest.mark.parametrize("overrides", [{"E": 0.0}, {"nu": 1.0}])
def test_combined_loading_validates_explicit_collapse_inputs(overrides):
    """Elastic-domain validation is shared with the collapse solver."""
    with pytest.raises(ValueError):
        dnv_combined_loading_check(
            D=1.0, t=0.05, mat=PipeMaterial("synthetic", 350.0, 400.0),
            M_Sd=0.0, S_Sd=0.0, p_li=0.0, p_e=0.0, **overrides,
        )


def test_executable_worked_example():
    """The shipped example must reproduce the independent 28 MPa hand root.

    See test_combined_loading_accepts_explicit_elastic_properties for the
    independent substitution; expected utilization is abs(-14/28)=0.5.
    """
    example = Path(__file__).resolve().parents[3] / "examples/pipeline_collapse.py"
    result = runpy.run_path(str(example))["run_example"]()
    assert result["p_c"] == pytest.approx(28.0, rel=1e-10)
    assert result["utilization"] == pytest.approx(0.5, rel=1e-10)
    assert result["standard_compliance"] == "NOT_EVALUATED"


def test_sizing_workflow_preserves_collapse_assessment_limits():
    """Synthetic sizing demand must retain the consumer's qualification status.

    Diameter, pressure and depth are chosen inputs, not reference capacities.
    This test checks propagation only; it does not qualify library properties
    or sizing equations.
    """
    result = WallThicknessSizingWorkflow({
        "pipeline": {
            "outer_diameter_m": 1.0,
            "design_pressure_MPa": 10.0,
            "depth_m": 100.0,
            "material": "X65",
        },
    }).run()
    collapse = result["dnv_external_collapse"]
    assert collapse["standard_compliance"] == "NOT_EVALUATED"
    assert "design_criterion_and_factors" in collapse["not_evaluated"]
