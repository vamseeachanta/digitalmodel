"""Slice 2: synthetic hand calculations, independent of implementation constants.

Basis: public Morison fixed-body equation (1950), as transcribed in the public
theory documentation linked in validation-claims.md; lift uses the explicitly
defined coefficient FL/(rho D U²/2). All densities, coefficients and dimensions
below are synthetic arithmetic assumptions, not values from a standard table.
The factors 1/2 and pi/4 are the dynamic-pressure and circle-area identities.
"""

import math

import pytest

from digitalmodel.subsea.on_bottom_stability import dnv_rp_f109 as f109
from digitalmodel.subsea.on_bottom_stability import simplified
from digitalmodel.subsea.on_bottom_stability.e305_simplified import (
    Pipe,
    weight_build_up,
)


def test_morison_and_lift_hand_calculation():
    """rho=1000, D=.4, U=2, a=.25; CD=2, CM=3, CL=.5 (assumed).

    FD=1000*2*.4*4/2=1600 N/m;
    FI=1000*3*pi*.16*.25/4=30*pi N/m;
    FL=1000*.5*.4*4/2=400 N/m. Reversing U and a reverses FH.
    """
    expected = 1600 + 30 * math.pi
    assert f109.hydrodynamic_force_per_meter(1000, 0.4, 2, 0.25, 2, 3) == pytest.approx(
        expected
    )
    assert f109.hydrodynamic_force_per_meter(
        1000, 0.4, -2, -0.25, 2, 3
    ) == pytest.approx(-expected)
    assert f109.lift_force_per_meter(1000, 0.4, -2, 0.5) == pytest.approx(400)
    loads = simplified.hydrodynamic_loads(0.4, 2, 0.25, 1000, 2, 3, 0.5)
    assert loads.drag_n_per_m == pytest.approx(1600)
    assert loads.inertia_n_per_m == pytest.approx(30 * math.pi)
    assert loads.lift_n_per_m == pytest.approx(400)
    assert loads.total_horizontal_n_per_m == pytest.approx(expected)
    assert simplified.drag_force_per_meter(2, 0.4, 1000, 2) == pytest.approx(1600)
    assert simplified.inertia_force_per_meter(0.25, 0.4, 1000, 3) == pytest.approx(
        30 * math.pi
    )
    assert simplified.lift_force_per_meter(-2, 0.4, 1000, 0.5) == pytest.approx(400)


def test_layered_weight_including_concrete_and_growth():
    """Independent annular ledger; every input is a synthetic assumption.

    Steel OD=1, bore=.8, corrosion OD=1.1, concrete OD=1.3, growth OD=1.4.
    Areas/pi: steel=.09; corrosion=.0525; concrete=.12; growth=.0675;
    contents=.16; displaced=.49. Densities: 8,2,3,1.5,1,water=1.
    Mass/pi=.72+.105+.36+.10125+.16=1.44625 kg/m/pi.
    At supplied g=10, Ws/pi=10*(1.44625-.49)=9.5625 N/m/pi.
    No field joints, absorption, corrosion allowance or extra mass.
    """
    pipe = Pipe(1, 0.1, 0, 8, 0.05, 2, 0.1, 3, 0, 0.05, 1.5, 0, 0, 0, 1, 0, 1)
    result = weight_build_up(pipe, g=10)
    assert dict(result.component_mass_kg_m) == pytest.approx(
        {
            "steel": 0.72 * math.pi,
            "corrosion_coating": 0.105 * math.pi,
            "concrete": 0.36 * math.pi,
            "marine_growth": 0.10125 * math.pi,
            "contents": 0.16 * math.pi,
            "field_joint": 0,
            "added": 0,
        }
    )
    assert result.submerged_weight_n_m == pytest.approx(9.5625 * math.pi)
    assert result.hydrodynamic_diameter_m == pytest.approx(1.4)


def test_single_coat_weight_both_surfaces():
    """Assumed D=1, bore=.8, coated OD=1.2, rho steel=8, coat=3,
    contents=water=1. Net mass/pi=.09*8+.11*3+.16-.36=.85.
    Expected weight=.85*pi*9.80665 using the documented fixed legacy gravity
    (knowledge pack §2, canonical weight); no empirical coefficient is used.
    """
    expected = 0.85 * math.pi * 9.80665
    assert f109.submerged_weight_per_meter(1, 0.1, 8, 3, 0.1, 1, 1) == pytest.approx(
        expected
    )
    assert simplified.submerged_weight(
        1, 0.1, 0.1, 8, 3, 1, 1
    ).ws_n_per_m == pytest.approx(expected)


@pytest.mark.parametrize("horizontal", [40, -40])
def test_coulomb_and_supplied_passive_resistance(horizontal):
    """Assumed W=200, H=+/-40, L=10, mu=.5, FR=10, gamma=2.

    Coulomb contact capacity=.5*(200-10)=95 N/m; with supplied passive
    resistance, capacity=105. Demand=2*40=80, utilization=80/105.
    This checks balance with an input resistance, not a soil calibration law.
    """
    actual = f109.generalized_stability_check(200, horizontal, 10, 0.5, 10, 2)
    assert actual.details["capacity"] == pytest.approx(105)
    assert actual.utilisation == pytest.approx(80 / 105)
    assert actual.is_stable


def test_negative_control_cannot_hide_uplift_behind_passive_resistance():
    """Assumed W=10<L=20: contact is lost even if supplied FR=1000 N/m."""
    assert not f109.generalized_stability_check(10, 1, 20, 0.5, 1000, 1).is_stable
