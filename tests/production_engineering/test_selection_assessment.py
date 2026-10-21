"""Selection evidence and independent hand calculations for existing IPR models.

Synthetic cases are explicitly chosen here, not taken from a field dataset.
Equation sources read: Nass (2010), *Inflow Performance Relationships (IPR)
for Solution Gas Drive Reservoirs — a Semi-Analytical Approach*, Texas A&M
MS thesis, Eqs. (1.7), (1.12), (1.16); public URL in the literature survey.
The composite extension is derived in that survey's Composite join derivation.
These arithmetic checks do not qualify a correlation against measured data.
"""

from dataclasses import FrozenInstanceError

import pytest

from digitalmodel.production_engineering.ipr_models import (
    CompositeIpr,
    FetkovichIpr,
    LinearIpr,
    ReservoirConditions,
    VogelIpr,
)
from digitalmodel.production_engineering.method_selector import (
    assess_ipr_selection,
    assess_vlp_selection,
    select_ipr_model,
)


def test_selected_linear_hand_calculation():
    """Nass (2010) Eq. (1.7): J=2, Pr=3000, Pb=2000, Pwf=2500.

    Entire interval is above Pb, hence linear. q=2*(3000-2500)=1000 bbl/d.
    All inputs are synthetic; integer arithmetic is exactly representable.
    """
    # Synthetic inputs and exact expected value: calculation in this docstring.
    reservoir = ReservoirConditions(3000, 2000, 2)
    state = dict(
        reservoir_pressure_psi=3000, bubble_point_psi=2000, minimum_pwf_psi=2500
    )
    decision = assess_ipr_selection(state, {"phase": "oil"})
    assert decision.method == "linear"
    assert "(1.7)" in decision.sources[-1]
    # Eq. (1.7), independently evaluated in this test's docstring.
    assert LinearIpr(reservoir, 2).flow_rate(2500) == 1000
    assert decision.qualification_status == "NOT_EVALUATED"


def test_selected_vogel_hand_calculation():
    """Nass (2010) Eq. (1.12): Pr=Pb=2000, Pwf=1000, qmax=1000.

    r=1000/2000=1/2; q/qmax=1-0.2/2-0.8/4=0.7; q=700 bbl/d.
    Synthetic equality Pr=Pb selects Vogel with confirmed solution-gas drive.
    """
    # Synthetic inputs: calculation in this docstring; coefficients: Eq. (1.12).
    reservoir = ReservoirConditions(2000, 2000, 1)
    state = dict(
        reservoir_pressure_psi=2000, bubble_point_psi=2000, minimum_pwf_psi=1000
    )
    decision = assess_ipr_selection(state, {"phase": "oil", "solution_gas_drive": True})
    assert decision.method == "vogel"
    assert "(1.12)" in decision.sources[-1]
    # Exact synthetic result derived above; no engineering acceptance band.
    assert VogelIpr(reservoir, 1000).flow_rate(1000) == 700


def test_selected_fetkovich_hand_calculation():
    """Nass (2010) Eq. (1.16), with C=qmax/Pr^(2*n).

    Choose synthetic Pr=2000, Pb=1800, Pwf=1000, C=0.001, n=1.
    q=0.001*(2000^2-1000^2)=0.001*3000000=3000 bbl/d.
    Declared multirate availability takes precedence over the composite choice.
    n is a caller input, not a default copied from an observed exponent range.
    """
    # Synthetic inputs and exact expected value: calculation in this docstring.
    reservoir = ReservoirConditions(2000, 1800, 1)
    state = dict(
        reservoir_pressure_psi=2000,
        bubble_point_psi=1800,
        minimum_pwf_psi=1000,
        multirate_test_available=True,
    )
    decision = assess_ipr_selection(state, {"phase": "oil"})
    assert decision.method == "fetkovich"
    assert "(1.16)" in decision.sources[-1]
    # Eq. (1.16), independently evaluated above; supplied C and n are synthetic.
    assert FetkovichIpr(reservoir, 0.001, 1).flow_rate(1000) == 3000
    assert "Parameter fit and field calibration: NOT_EVALUATED." in decision.open_checks


def test_selected_composite_hand_calculation():
    """Synthetic Pr=3000, Pb=1800, J=1, Pwf=900; q=1900 bbl/d.

    Join derivation in survey, from Nass (2010) Eqs. (1.7), (1.12):
    qb=1*(3000-1800)=1200; extension=1*1800/(0.2+2*0.8)=1000.
    At r=900/1800=1/2, factor=1-0.1-0.2=0.7.
    Total q=1200+1000*0.7=1900; at Pwf=Pb, factor=0 so q=1200.
    """
    # Synthetic inputs and exact expected values: calculation in this docstring.
    reservoir = ReservoirConditions(3000, 1800, 1)
    state = dict(
        reservoir_pressure_psi=3000, bubble_point_psi=1800, minimum_pwf_psi=900
    )
    decision = assess_ipr_selection(state, {"phase": "oil", "solution_gas_drive": True})
    assert decision.method == "composite"
    assert "Composite join derivation" in decision.sources[-1]
    # Exact hand results above; no copied model outputs or implicit tolerance.
    assert CompositeIpr(reservoir).flow_rate(900) == 1900
    assert CompositeIpr(reservoir).flow_rate(1800) == 1200
    # Same synthetic case at Pb: linear interval, as documented in the policy.
    state["minimum_pwf_psi"] = 1800
    assert assess_ipr_selection(state, {"phase": "oil"}).method == "linear"


@pytest.mark.parametrize(
    "fluid",
    [
        {"phase": "gas"},
        {"phase": "oil", "significant_skin": True},
        {"phase": "oil"},
    ],
)
def test_unsupported_ipr_has_no_qualified_fallback(fluid):
    """Pr=Pb=2000>Pwf=1000: gas, skin or unconfirmed drive prevents selection.

    Synthetic pressures specify the inequalities only. Expected no-method
    follows the documented policy, Vogel (1968) and Standing (1970) scope.
    """
    # Synthetic pressure ordering derived in this test's docstring.
    state = dict(
        reservoir_pressure_psi=2000, bubble_point_psi=2000, minimum_pwf_psi=1000
    )
    decision = assess_ipr_selection(state, fluid)
    assert decision.method is None
    assert decision.qualification_status == "NOT_EVALUATED"
    assert decision.rationale
    assert decision.sources
    assert all("NOT_EVALUATED" in check for check in decision.open_checks)
    with pytest.raises(NotImplementedError):
        select_ipr_model(state, fluid)


@pytest.mark.parametrize(
    "state",
    [
        {},
        # Synthetic invalid pressure domain: zero reference pressure, thesis Eq. (1.12).
        dict(reservoir_pressure_psi=0, bubble_point_psi=2000, minimum_pwf_psi=1000),
        # Synthetic injection ordering is outside production domain, thesis Eq. (1.7).
        dict(reservoir_pressure_psi=2000, bubble_point_psi=2000, minimum_pwf_psi=3000),
        dict(
            reservoir_pressure_psi=float("nan"),
            bubble_point_psi=True,
            minimum_pwf_psi=False,
        ),
    ],
)
def test_assessment_preserves_invalid_input_errors(state):
    with pytest.raises(ValueError):
        assess_ipr_selection(state, {"phase": "oil"})


@pytest.mark.parametrize(
    "regime,phase",
    [
        ("vertical_oil", "oil"),
        ("inclined_oil", "oil"),
        ("gas_condensate", "gas"),
        ("screening", "gas"),
    ],
)
def test_vlp_unavailable_remains_not_evaluated(regime, phase):
    decision = assess_vlp_selection(regime, {"phase": phase})
    assert decision.method is None
    assert decision.qualification_status == "NOT_EVALUATED"
    assert decision.sources
    assert all("NOT_EVALUATED" in check for check in decision.open_checks)


def test_screening_has_explicit_evidence_gaps_and_immutable_result():
    decision = assess_vlp_selection("screening", {"phase": "oil"})
    assert decision.method == "simplified_homogeneous"
    assert "exploratory" in decision.rationale
    assert decision.qualification_status == "NOT_EVALUATED"
    assert (
        "Measured pressure-survey calibration: NOT_EVALUATED." in decision.open_checks
    )
    assert isinstance(decision.sources, tuple)
    assert isinstance(decision.open_checks, tuple)
    with pytest.raises(FrozenInstanceError):
        decision.method = "beggs_brill"


def test_unknown_vlp_regime_remains_an_input_error():
    with pytest.raises(ValueError, match="Unknown VLP regime"):
        assess_vlp_selection("unknown", {"phase": "oil"})
