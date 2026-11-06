# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Numerical regressions for the existing collapse interaction equation.

Sources (public, opened for this regression):
* EPRG, Deeper, Longer and Thicker: Challenges for the Design of Subsea
  Pipelines, June 2022, section 3.1, equation (3): interaction equation.
  https://www.eprg.net/fileadmin/EPRG_Dokumente/00222_Position_Paper_-_Final__EPRG__49__2_29_.pdf
* TU Delft, Local Buckling and Collapse of Thick Walled pipe, 2016,
  equations (3.9), (3.10): elastic and plastic component pressures.
  https://repository.tudelft.nl/file/File_09e16762-ff32-49eb-bff8-85cffa889063

Inputs below are chosen synthetic values, not standard material properties or
engineering acceptance limits. Relative tolerance 1e-10 is a numerical regression
criterion: substantially looser than binary64 rounding, tighter than the former
solver's relative bracket tolerance. These tests do not qualify design factors.
"""

import pytest

from digitalmodel.subsea.pipeline.pipeline_pressure import (
    PipeMaterial,
    dnv_collapse_pressure,
    dnv_combined_loading_check,
    dnv_external_collapse_check,
)


@pytest.fixture
def synthetic_material():
    """Chosen yield and tensile strengths [MPa], not a standard grade."""
    return PipeMaterial(name="synthetic", SMYS=350.0, SMTS=400.0)


def test_default_ovality_exact_hand_root(synthetic_material):
    """Hand calculation using the module sources, not the implementation.

    Choose D=1 m, t=0.05 m, E=144000 MPa, nu=0, fy=350 MPa.
    Pel=2*144000*(0.05)^3=36 MPa; Pp=2*350*0.05=35 MPa.
    The existing default f0=0.005 gives C=36*35*0.005/0.05=126 MPa^2.
    At Pc=28 MPa: (28-36)*(28^2-35^2)=3528=28*126.
    On (0,35), (36-p)*(35^2-p^2)/p decreases strictly from infinity
    to zero, so 28 is the unique physical root in this interval.
    """
    actual = dnv_collapse_pressure(
        D=1.0,
        t=0.05,
        mat=synthetic_material,
        E=144000.0,
        nu=0.0,
    )
    assert actual == pytest.approx(28.0, rel=1e-10)


@pytest.mark.parametrize(
    "modulus, expected", [(144000.0, 35.0), (140000.0, 35.0), (80000.0, 20.0)]
)
def test_zero_ovality_lower_component(modulus, expected, synthetic_material):
    """At f0=0 the cubic factors, so the first positive root is min(Pel,Pp).

    D=1 m, t=0.05 m, nu=0 and fy=350 MPa give Pp=35 MPa.
    Pel=2*E*(0.05)^3: E=144000 gives 36 MPa (plastic governs);
    E=140000 gives 35 MPa (coincident roots);
    E=80000 gives 20 MPa (elastic governs). Expected roots: 35, 35 and 20.
    """
    actual = dnv_collapse_pressure(
        D=1.0,
        t=0.05,
        mat=synthetic_material,
        E=modulus,
        nu=0.0,
        f_0=0.0,
    )
    assert actual == pytest.approx(expected, rel=1e-10)


def test_issue_case_independent_substitution_bracket():
    """Independent hand substitution for the synthetic case in the pipeline pack.

    D=0.3239 m, t=0.0159 m, fy=450 MPa, E=207000 MPa, nu=0.3,
    f0=0.005. With the component definitions cited above:
    Pel=2*207000*(159/3239)^3/(1-0.3^2)=53.81675131438836 MPa;
    Pp=2*450*159/3239=44.18030256251930 MPa.
    C=Pel*Pp*0.005*3239/159. Direct decimal substitution into
    g(p)=(p-Pel)*(p^2-Pp^2)-p*C yields:
    g(37.4115634)=+0.00009265298 MPa^3,
    g(37.4115635)=-0.00010954071 MPa^3.
    Hence the physical root lies strictly between these pressure bounds.
    Bounds are independent decimal arithmetic, not calls to the routine.
    """
    mat = PipeMaterial(name="synthetic", SMYS=450.0, SMTS=450.0)
    actual = dnv_collapse_pressure(D=0.3239, t=0.0159, mat=mat)
    assert 37.4115634 < actual < 37.4115635


@pytest.mark.parametrize("ovality", [0.0, 0.005, 0.02])
def test_direct_consumers_share_corrected_root(ovality, synthetic_material):
    """Consumers must return the same root, without qualifying their other checks.

    The expected pressure is the independently established 28 MPa hand root
    for the external check's explicit inputs; other ovalities compare only
    consumer wiring. Depth/loads are synthetic zero-demand inputs.
    """
    external = dnv_external_collapse_check(
        depth=0.0,
        D=1.0,
        t=0.05,
        mat=synthetic_material,
        E=144000.0,
        nu=0.0,
        f_0=ovality,
    )
    if ovality == 0.005:
        assert external["p_c"] == pytest.approx(28.0, rel=1e-10)
    assert external["p_c"] == dnv_collapse_pressure(
        D=1.0,
        t=0.05,
        mat=synthetic_material,
        E=144000.0,
        nu=0.0,
        f_0=ovality,
    )
    combined = dnv_combined_loading_check(
        D=1.0,
        t=0.05,
        mat=synthetic_material,
        M_Sd=0.0,
        S_Sd=0.0,
        p_li=0.0,
        p_e=0.0,
        f_0=ovality,
    )
    assert combined["p_c"] == dnv_collapse_pressure(
        D=1.0,
        t=0.05,
        mat=synthetic_material,
        f_0=ovality,
    )


@pytest.mark.parametrize(
    "overrides",
    [
        {"D": 0.0},
        {"D": -1.0},
        {"D": float("inf")},
        {"t": 0.0},
        {"t": -0.05},
        {"t": 0.5},
        {"t": float("nan")},
        {"E": 0.0},
        {"E": float("inf")},
        {"nu": -1.0},
        {"nu": 1.0},
        {"nu": float("nan")},
        {"f_0": -0.005},
        {"f_0": float("nan")},
        {"f_0": float("inf")},
    ],
)
def test_invalid_inputs_cannot_define_physical_bracket(overrides, synthetic_material):
    """Finite positive components and nonnegative ovality define the root bracket.

    The geometry requires a positive bore (2*t<D); |nu|<1 keeps the elastic
    denominator positive. These are mathematical domain checks, not code limits.
    """
    inputs = dict(D=1.0, t=0.05, mat=synthetic_material, E=144000.0, nu=0.0)
    inputs.update(overrides)
    with pytest.raises(ValueError):
        dnv_collapse_pressure(**inputs)


@pytest.mark.parametrize("strength", [float("nan"), float("inf")])
def test_nonfinite_strength_rejected(strength):
    """Nonfinite component pressures cannot define a collapse root."""
    mat = PipeMaterial(name="synthetic", SMYS=strength, SMTS=strength)
    with pytest.raises(ValueError):
        dnv_collapse_pressure(D=1.0, t=0.05, mat=mat)
