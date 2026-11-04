"""Productivity index from tests, skin and pre-stimulation triage (issue #3830).

Hand checks (independent of the implementation; no expected value below is
obtained by calling the module):

1. Pwf sensitivity. q = 300 STB/d, Pr = 2,000 psi, Pwf = 1,700 psi, so the
   drawdown is 300 psi and J = 300/300 = 1.000 STB/d/psi. A Pwf error of
   +/-100 psi gives drawdowns of 400 and 200 psi, so J ranges from
   300/400 = 0.750 to 300/200 = 1.500 STB/d/psi: -25 % and +50 % on J.

2. Darcy radial PI, pseudo-steady state, oil, field units:
   J = k*h / (141.2*mu*Bo*(ln(re/rw) - 0.75 + S)).
   The geometry is chosen so that ln(re/rw) = 7.75 exactly
   (rw = 0.5 ft, re = 0.5*exp(7.75) ft), giving ln(re/rw) - 0.75 = 7.
   With mu = 1 cp, Bo = 1 rb/STB, k = 98.84 md and h = 10 ft,
   k*h = 988.4 md-ft = 141.2*7, so J(S=0) = 1.000 STB/d/psi and
   J(S=7) = 988.4/(141.2*14) = 0.500 STB/d/psi.

3. Skin from the PI ratio: S = (ln(re/rw) - 0.75)*(J_ideal/J_test - 1)
   = 7*(1.0/0.5 - 1) = 7.

4. Skin pressure drop: dP_skin = 141.2*q*mu*Bo*S/(k*h). At q = 150 STB/d
   and S = 7: 141.2*150*1*1*7/988.4 = 150.0 psi.

5. Flow efficiency: FE = (Pr - Pwf - dP_skin)/(Pr - Pwf). With a 300 psi
   drawdown and dP_skin = 150 psi, FE = 150/300 = 0.500, equal to
   J(S=7)/J(S=0) = 0.5/1.0.

6. PI ratio J2/J1 = (ln(re/rw) - 0.75 + S1)/(ln(re/rw) - 0.75 + S2).
   From S1 = 7 to S2 = 0: 14/7 = 2.000. From S1 = 7 to S2 = -2: 14/5 = 2.800.

Equation basis: pseudo-steady-state radial inflow for a well centred in a
closed circular drainage area, oil, field units (STB/d, md, ft, psi, cp,
rb/STB), 141.2 = 1/0.00708. The equation is the standard textbook form; a
published worked example with author, title, edition and page is not
established in this test file (no published example could be read for this
change). These are reproduction checks of the arithmetic, not field accuracy
limits.

Tolerance: 1e-9 absolute in output units, for floating-point roundoff only.
"""

from __future__ import annotations

import math

import pytest

from digitalmodel.production_engineering.pi_diagnostics import (
    DARCY_OIL_FIELD_CONSTANT,
    PwfSource,
    ReservoirRockFluid,
    TriageCause,
    WellTestPoint,
    darcy_radial_pi,
    flow_efficiency,
    pi_from_test,
    pi_ratio,
    pre_stimulation_triage,
    skin_from_pi,
    skin_pressure_drop_psi,
)

ABS = 1e-9

RW_FT = 0.5
RE_FT = 0.5 * math.exp(7.75)  # ln(re/rw) - 0.75 = 7 exactly


def rock_fluid(**overrides) -> ReservoirRockFluid:
    values = dict(
        permeability_md=98.84,
        net_pay_ft=10.0,
        oil_viscosity_cp=1.0,
        oil_fvf_rb_stb=1.0,
        drainage_radius_ft=RE_FT,
        wellbore_radius_ft=RW_FT,
    )
    values.update(overrides)
    return ReservoirRockFluid(**values)


def well_test(**overrides) -> WellTestPoint:
    values = dict(
        q_oil_stb_d=300.0,
        pr_psi=2000.0,
        pwf_psi=1700.0,
        pwf_source=PwfSource.MEASURED,
        pwf_error_psi=0.0,
        water_cut_fraction=0.10,
        gor_scf_stb=500.0,
        stabilised=True,
        flow_regime="pseudo_steady_state",
        choke_64ths=32.0,
        lift_setting="natural_flow",
    )
    values.update(overrides)
    return WellTestPoint(**values)


# ---------------------------------------------------------------------------
# 1. PI from a test with a Pwf band
# ---------------------------------------------------------------------------


def test_constant_is_141_2():
    assert DARCY_OIL_FIELD_CONSTANT == 141.2


def test_pi_from_test_point_value():
    result = pi_from_test(300.0, 2000.0, 1700.0, PwfSource.MEASURED, pwf_error_psi=0.0)
    assert result.drawdown_psi == pytest.approx(300.0, rel=0, abs=ABS)
    assert result.pi_stb_d_psi == pytest.approx(1.0, rel=0, abs=ABS)
    assert result.pi_low_stb_d_psi == pytest.approx(1.0, rel=0, abs=ABS)
    assert result.pi_high_stb_d_psi == pytest.approx(1.0, rel=0, abs=ABS)


def test_100_psi_error_at_300_psi_drawdown_gives_minus_25_plus_50_percent():
    result = pi_from_test(300.0, 2000.0, 1700.0, PwfSource.CALCULATED, pwf_error_psi=100.0)
    assert result.pi_stb_d_psi == pytest.approx(1.0, rel=0, abs=ABS)
    assert result.pi_low_stb_d_psi == pytest.approx(0.75, rel=0, abs=ABS)
    assert result.pi_high_stb_d_psi == pytest.approx(1.5, rel=0, abs=ABS)
    assert result.low_change_fraction == pytest.approx(-0.25, rel=0, abs=ABS)
    assert result.high_change_fraction == pytest.approx(0.50, rel=0, abs=ABS)
    assert result.pwf_source is PwfSource.CALCULATED
    assert result.band_bounded is True


def test_calculated_pwf_requires_a_stated_error():
    with pytest.raises(ValueError, match="pwf_error_psi"):
        pi_from_test(300.0, 2000.0, 1700.0, PwfSource.CALCULATED)


def test_measured_pwf_without_error_has_zero_band():
    result = pi_from_test(300.0, 2000.0, 1700.0, PwfSource.MEASURED)
    assert result.pwf_error_psi == 0.0
    assert result.pi_low_stb_d_psi == pytest.approx(1.0, rel=0, abs=ABS)
    assert result.pi_high_stb_d_psi == pytest.approx(1.0, rel=0, abs=ABS)


def test_error_not_less_than_drawdown_leaves_upper_band_unbounded():
    # Drawdown 300 psi, error 300 psi: the low-drawdown end reaches zero.
    result = pi_from_test(300.0, 2000.0, 1700.0, PwfSource.CALCULATED, pwf_error_psi=300.0)
    assert result.band_bounded is False
    assert math.isinf(result.pi_high_stb_d_psi)
    assert result.pi_low_stb_d_psi == pytest.approx(0.5, rel=0, abs=ABS)  # 300/600


@pytest.mark.parametrize(
    "q, pr, pwf, err",
    [
        (-1.0, 2000.0, 1700.0, 0.0),  # negative rate
        (300.0, 2000.0, 2000.0, 0.0),  # zero drawdown
        (300.0, 1700.0, 2000.0, 0.0),  # negative drawdown
        (300.0, 2000.0, 1700.0, -5.0),  # negative error
        (math.nan, 2000.0, 1700.0, 0.0),  # non-finite input
    ],
)
def test_pi_from_test_rejects_invalid_input(q, pr, pwf, err):
    with pytest.raises(ValueError):
        pi_from_test(q, pr, pwf, PwfSource.MEASURED, pwf_error_psi=err)


def test_pwf_source_accepts_string_values():
    result = pi_from_test(300.0, 2000.0, 1700.0, "measured")
    assert result.pwf_source is PwfSource.MEASURED
    with pytest.raises(ValueError):
        pi_from_test(300.0, 2000.0, 1700.0, "guessed")


# ---------------------------------------------------------------------------
# 2. Darcy radial PI, skin, dP_skin, flow efficiency, PI ratio
# ---------------------------------------------------------------------------


def test_darcy_radial_pi_zero_skin():
    assert darcy_radial_pi(rock_fluid()) == pytest.approx(1.0, rel=0, abs=ABS)


def test_darcy_radial_pi_with_skin_7():
    assert darcy_radial_pi(rock_fluid(), skin=7.0) == pytest.approx(0.5, rel=0, abs=ABS)


def test_darcy_radial_pi_rejects_non_positive_denominator():
    # ln(re/rw) - 0.75 = 7; S = -7.5 makes the denominator -0.5.
    with pytest.raises(ValueError):
        darcy_radial_pi(rock_fluid(), skin=-7.5)


@pytest.mark.parametrize(
    "field, value",
    [
        ("permeability_md", 0.0),
        ("net_pay_ft", -1.0),
        ("oil_viscosity_cp", 0.0),
        ("oil_fvf_rb_stb", 0.0),
        ("wellbore_radius_ft", 0.0),
        ("drainage_radius_ft", 0.4),  # re < rw
    ],
)
def test_rock_fluid_rejects_invalid_values(field, value):
    with pytest.raises(ValueError):
        darcy_radial_pi(rock_fluid(**{field: value}))


def test_skin_from_pi_ratio():
    assert skin_from_pi(0.5, 1.0, RE_FT, RW_FT) == pytest.approx(7.0, rel=0, abs=ABS)


def test_skin_from_pi_negative_for_stimulated_well():
    # J_test = 1.4 against J_ideal = 1.0: S = 7*(1/1.4 - 1) = -2.
    assert skin_from_pi(1.4, 1.0, RE_FT, RW_FT) == pytest.approx(-2.0, rel=0, abs=ABS)


def test_skin_pressure_drop():
    dp = skin_pressure_drop_psi(150.0, rock_fluid(), skin=7.0)
    assert dp == pytest.approx(150.0, rel=0, abs=ABS)


def test_flow_efficiency_from_pressures():
    assert flow_efficiency(2000.0, 1700.0, 150.0) == pytest.approx(0.5, rel=0, abs=ABS)


def test_flow_efficiency_matches_pi_ratio_to_ideal():
    j_actual = darcy_radial_pi(rock_fluid(), skin=7.0)
    j_ideal = darcy_radial_pi(rock_fluid(), skin=0.0)
    dp = skin_pressure_drop_psi(j_actual * 300.0, rock_fluid(), skin=7.0)
    assert flow_efficiency(2000.0, 1700.0, dp) == pytest.approx(j_actual / j_ideal, rel=0, abs=ABS)


def test_pi_ratio_remove_skin_7():
    assert pi_ratio(RE_FT, RW_FT, skin_before=7.0, skin_after=0.0) == pytest.approx(2.0, rel=0, abs=ABS)


def test_pi_ratio_skin_7_to_minus_2():
    assert pi_ratio(RE_FT, RW_FT, skin_before=7.0, skin_after=-2.0) == pytest.approx(2.8, rel=0, abs=ABS)


# ---------------------------------------------------------------------------
# 3. Ordered pre-stimulation triage
# ---------------------------------------------------------------------------


def test_triage_stops_at_pwf_source_when_change_is_inside_band():
    previous = well_test(q_oil_stb_d=300.0, pwf_psi=1700.0)  # J = 1.0 measured
    # Current: J = 270/300 = 0.9 from a calculated Pwf +/-100 psi: band 0.675..1.35
    current = well_test(
        q_oil_stb_d=270.0, pwf_source=PwfSource.CALCULATED, pwf_error_psi=100.0
    )
    result = pre_stimulation_triage(current, previous, rock_fluid())
    assert result.cause is TriageCause.PWF_UNCERTAINTY
    assert result.step == 1
    assert "calculated" in result.reason.lower()
    assert [f.step for f in result.findings] == [1]


def test_triage_passes_pwf_step_when_both_measured_then_finds_depletion():
    previous = well_test(q_oil_stb_d=300.0, pr_psi=2000.0, pwf_psi=1700.0)  # J = 1.0
    # Pr fell 100 psi; same J = 1.0 at 200 psi drawdown; rate fell to 200.
    current = well_test(q_oil_stb_d=200.0, pr_psi=1900.0, pwf_psi=1700.0)
    result = pre_stimulation_triage(current, previous, rock_fluid())
    assert result.cause is TriageCause.DEPLETION
    assert result.step == 2
    assert [f.step for f in result.findings] == [1, 2]
    assert result.findings[0].cause_found is False


def test_triage_no_decline_when_rate_and_pi_hold():
    result = pre_stimulation_triage(well_test(), well_test(), rock_fluid())
    assert result.cause is TriageCause.NO_DECLINE
    assert result.step == 2


def test_triage_mobility_on_water_cut_change():
    previous = well_test()
    current = well_test(q_oil_stb_d=150.0, water_cut_fraction=0.40)  # J = 0.5
    result = pre_stimulation_triage(current, previous, rock_fluid())
    assert result.cause is TriageCause.MOBILITY_CHANGE
    assert result.step == 3
    assert "water cut" in result.reason.lower()


def test_triage_mobility_on_gor_change():
    previous = well_test()
    current = well_test(q_oil_stb_d=150.0, gor_scf_stb=900.0)
    result = pre_stimulation_triage(current, previous, rock_fluid())
    assert result.cause is TriageCause.MOBILITY_CHANGE
    assert "gor" in result.reason.lower()


@pytest.mark.parametrize(
    "overrides",
    [
        {"stabilised": False},
        {"flow_regime": "transient"},
        {"choke_64ths": 40.0},
        {"lift_setting": "gas_lift_1.0_mmscfd"},
    ],
)
def test_triage_not_comparable(overrides):
    previous = well_test()
    current = well_test(q_oil_stb_d=150.0, **overrides)
    result = pre_stimulation_triage(current, previous, rock_fluid())
    assert result.cause is TriageCause.NOT_COMPARABLE
    assert result.step == 4


def test_triage_reaches_skin_and_reports_skin_values():
    previous = well_test()  # J = 1.0 = ideal
    current = well_test(q_oil_stb_d=150.0)  # J = 0.5 -> S = 7
    result = pre_stimulation_triage(current, previous, rock_fluid())
    assert result.cause is TriageCause.SKIN_DAMAGE
    assert result.step == 5
    assert result.skin == pytest.approx(7.0, rel=0, abs=ABS)
    assert result.skin_pressure_drop_psi == pytest.approx(150.0, rel=0, abs=ABS)
    assert result.flow_efficiency == pytest.approx(0.5, rel=0, abs=ABS)
    assert result.pi_ratio_if_skin_removed == pytest.approx(2.0, rel=0, abs=ABS)
    assert [f.step for f in result.findings] == [1, 2, 3, 4, 5]


def test_triage_skin_step_without_reservoir_data_is_insufficient():
    result = pre_stimulation_triage(well_test(q_oil_stb_d=150.0), well_test(), None)
    assert result.cause is TriageCause.INSUFFICIENT_DATA
    assert result.step == 5
    assert result.skin is None


def test_triage_pi_fall_without_positive_skin_is_unexplained():
    # h = 4 ft gives k*h = 395.36 md-ft, so ideal J = 395.36/(141.2*7) = 0.4.
    # Tested J fell from 1.0 to 150/300 = 0.5: S = 7*(0.4/0.5 - 1) = -1.4.
    # (S exactly 0 is avoided: roundoff decides its sign.)
    rf = rock_fluid(net_pay_ft=4.0)
    result = pre_stimulation_triage(well_test(q_oil_stb_d=150.0), well_test(), rf)
    assert result.cause is TriageCause.NO_SKIN_DAMAGE
    assert result.step == 5
    assert result.skin == pytest.approx(-1.4, rel=0, abs=ABS)


def test_triage_no_decline_when_pi_rose():
    previous = well_test()  # J = 1.0
    current = well_test(q_oil_stb_d=450.0)  # J = 1.5
    result = pre_stimulation_triage(current, previous, rock_fluid())
    assert result.cause is TriageCause.NO_DECLINE
    assert result.step == 2
