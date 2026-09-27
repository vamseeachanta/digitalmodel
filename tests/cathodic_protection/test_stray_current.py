"""Tests for the re-modelled stray-current module (issue #2247).

Expected values are hand-derived from the cited formulas:

* DC limit bands (Lynch 2016, reproducing EN 50162 Table 1): 20 mV,
  1.5 * rho mV, 300 mV including IR drop; 20 mV excluding IR drop.
* Point source + infinite leaky line: at the closest point
  dE(0) = (rho I / 2 pi d) [a (pi/2)(H0(a) - Y0(a)) - 1], a = alpha d,
  from int_0^inf e^{-a t}/sqrt(1+t^2) dt = (pi/2)(H0(a) - Y0(a))
  (Abramowitz & Stegun 12.1.8, Struve function).
* Coupon density i_ac = 8 V / (rho pi d) (Brenna et al. 2020; Newman 1966).
* Drainage bond R = V/I - R_circuit, P = I^2 R.
"""

from __future__ import annotations

import math
from numbers import Real

import pytest
from pydantic import ValidationError
from scipy.special import struve, y0

from digitalmodel.cathodic_protection import stray_current as sc
from digitalmodel.cathodic_protection._experimental import ExperimentalModelError
from digitalmodel.cathodic_protection._provisional import (
    ProvisionalValue,
    ProvisionalValueError,
    render_provisional,
    render_provisional_table,
)
from digitalmodel.cathodic_protection.stray_current import (
    InterferenceType,
    ShiftBasis,
    StrayCurrentInput,
    assess_stray_current,
    design_drainage_bond,
)

# Pipe used by the source-model tests: D = 0.5 m, t = 12.5 mm, R_c = 1e4 ohm-m2,
# rho_steel = 2e-7 ohm-m  ->  alpha^2 = rho_s D / (t (D - t) R_c) = 1e-7 / 60.9375.
PIPE = dict(pipeline_od_m=0.5, wall_thickness_m=0.0125, coating_resistance_ohm_m2=1.0e4)
ALPHA = math.sqrt(1.0e-7 / 60.9375)


def _dc_source(**kw: float) -> StrayCurrentInput:
    base: dict = dict(
        interference_type=InterferenceType.DC_TRANSIT,
        soil_resistivity_ohm_m=100.0,
        leakage_current_A=10.0,
        separation_distance_m=100.0,
        **PIPE,
    )
    base.update(kw)
    return StrayCurrentInput(**base)


def _closed_form_shift_at_closest_point_mV(rho: float, i: float, d: float, alpha: float) -> float:
    a = alpha * d
    return 1000.0 * rho * i / (2.0 * math.pi * d) * (a * math.pi / 2.0 * (struve(0, a) - y0(a)) - 1.0)


# ---------------------------------------------------------------------------
# Experimental gate
# ---------------------------------------------------------------------------


@pytest.mark.parametrize(
    "call",
    [
        lambda: assess_stray_current(StrayCurrentInput(
            interference_type=InterferenceType.DC_TRANSIT, soil_resistivity_ohm_m=50.0,
            measured_shift_mV=10.0)),
        lambda: assess_stray_current(StrayCurrentInput(
            interference_type=InterferenceType.AC_POWERLINE, soil_resistivity_ohm_m=50.0,
            ac_voltage_V=5.0)),
        lambda: design_drainage_bond(5.0, 2.0),
        lambda: sc.dc_shift_limit_mV(50.0),
        lambda: sc.pipe_attenuation_constant(0.5, 0.0125, 1.0e4),
        lambda: sc.point_source_shift_mV(0.0, 10.0, 100.0, 100.0, ALPHA),
        lambda: sc.ac_current_density_A_m2(10.0, 100.0, 0.01),
    ],
)
def test_public_calculations_require_experimental_flag(call):
    with pytest.raises(ExperimentalModelError, match="EN 50162 Table 1"):
        call()


def test_experimental_error_carries_model_reason_standard():
    with pytest.raises(ExperimentalModelError) as excinfo:
        design_drainage_bond(5.0, 2.0)
    err = excinfo.value
    assert err.model == "stray_current.design_drainage_bond"
    assert err.standard == "EN 50162 Table 1"
    assert "provisional" in err.reason
    assert "experimental=True" in str(err)


# ---------------------------------------------------------------------------
# Provisional constants
# ---------------------------------------------------------------------------


def test_every_default_constant_is_a_provisional_value_with_source():
    assert sc.PROVISIONAL_VALUES
    for name, pv in sc.PROVISIONAL_VALUES.items():
        assert isinstance(pv, ProvisionalValue), name
        assert pv.source.strip(), name
        assert pv.provisional is True, name
        assert pv.pending_standard.strip(), name
        assert getattr(sc, name) is pv


def test_no_bare_numeric_module_constants():
    """Upper-case numeric module attributes would be uncited defaults."""
    bare = [
        n for n, v in vars(sc).items()
        if n.isupper() and isinstance(v, Real) and not isinstance(v, bool)
    ]
    assert bare == []


def test_steel_resistivity_default_is_the_provisional_value():
    field = StrayCurrentInput.model_fields["steel_resistivity_ohm_m"]
    assert field.default == sc.STEEL_RESISTIVITY_OHM_M.value


def test_thresholds_cite_literature_not_a_standard_read():
    for key in ("DC_SHIFT_LIMIT_HIGH_RHO_MV", "DC_SHIFT_SLOPE_MV_PER_OHM_M"):
        assert "Lynch" in sc.PROVISIONAL_VALUES[key].source
        assert "EN 50162" in sc.PROVISIONAL_VALUES[key].pending_standard
    for key in ("AC_CURRENT_DENSITY_LIMIT_A_M2", "AC_DC_RATIO_LIMIT", "AC_VOLTAGE_TARGET_V"):
        assert "10.3390/ma13092158" in sc.PROVISIONAL_VALUES[key].source
        assert "ISO 18086" in sc.PROVISIONAL_VALUES[key].pending_standard


def test_provisional_value_validation_and_render():
    with pytest.raises(ProvisionalValueError):
        ProvisionalValue(1.0, "mV", "", pending_standard="X")
    with pytest.raises(ProvisionalValueError):
        ProvisionalValue(1.0, "mV", "somebody 2020")  # no pending standard
    with pytest.raises(ProvisionalValueError):
        ProvisionalValue(float("nan"), "mV", "somebody 2020", pending_standard="X")
    confirmed = ProvisionalValue(1.0, "mV", "somebody 2020", provisional=False)
    assert "confirmed" in render_provisional(confirmed)
    text = render_provisional(sc.AC_CURRENT_DENSITY_LIMIT_A_M2, "i_ac")
    assert text.startswith("i_ac = 30 A/m2 [PROVISIONAL; source: A. Brenna")
    assert "ISO 18086" in text
    assert float(sc.AC_DC_RATIO_LIMIT) == 3.0
    table = render_provisional_table(sc.PROVISIONAL_VALUES)
    assert table.count("\n") == len(sc.PROVISIONAL_VALUES) + 1


# ---------------------------------------------------------------------------
# DC limit table
# ---------------------------------------------------------------------------


@pytest.mark.parametrize(
    "rho, basis, expected",
    [
        (10.0, ShiftBasis.INCLUDING_IR, 20.0),
        (15.0, ShiftBasis.INCLUDING_IR, 22.5),
        (100.0, ShiftBasis.INCLUDING_IR, 150.0),
        (199.0, ShiftBasis.INCLUDING_IR, 298.5),
        (200.0, ShiftBasis.INCLUDING_IR, 300.0),
        (1000.0, ShiftBasis.INCLUDING_IR, 300.0),
        (10.0, ShiftBasis.EXCLUDING_IR, 20.0),
        (1000.0, ShiftBasis.EXCLUDING_IR, 20.0),
    ],
)
def test_dc_shift_limit_bands(rho, basis, expected):
    assert sc.dc_shift_limit_mV(rho, basis, experimental=True) == pytest.approx(expected)


def test_dc_shift_limit_rejects_non_positive_resistivity():
    with pytest.raises(ValueError):
        sc.dc_shift_limit_mV(0.0, experimental=True)


# ---------------------------------------------------------------------------
# DC measured route
# ---------------------------------------------------------------------------


def test_dc_measured_shift_fails_banded_limit():
    p = StrayCurrentInput(interference_type=InterferenceType.DC_TRANSIT,
                          soil_resistivity_ohm_m=50.0, measured_shift_mV=100.0)
    r = assess_stray_current(p, experimental=True)
    assert r.method == "measured_shift"
    assert r.shift_limit_mV == pytest.approx(75.0)  # 1.5 * 50
    assert r.shift_ratio == pytest.approx(100.0 / 75.0)
    assert r.passes is False
    assert r.recommended_mitigation
    assert "DC_SHIFT_SLOPE_MV_PER_OHM_M" in r.provenance
    assert "PROVISIONAL" in r.provenance["DC_SHIFT_SLOPE_MV_PER_OHM_M"]


def test_dc_measured_ir_free_shift_uses_20_mV():
    p = StrayCurrentInput(interference_type=InterferenceType.DC_HVDC, soil_resistivity_ohm_m=500.0,
                          measured_shift_mV=15.0, shift_basis=ShiftBasis.EXCLUDING_IR)
    r = assess_stray_current(p, experimental=True)
    assert r.shift_limit_mV == 20.0
    assert r.passes is True
    assert r.shift_ratio == pytest.approx(0.75)


def test_cathodically_protected_structure_requires_allowable_shift():
    p = StrayCurrentInput(interference_type=InterferenceType.DC_TRANSIT, soil_resistivity_ohm_m=50.0,
                          measured_shift_mV=10.0, cathodically_protected=True)
    with pytest.raises(ValueError, match="allowable_shift_mV is required"):
        assess_stray_current(p, experimental=True)
    p2 = p.model_copy(update={"allowable_shift_mV": 40.0})
    r = assess_stray_current(p2, experimental=True)
    assert r.shift_limit_mV == 40.0
    assert r.provenance["shift_limit"] == "user input allowable_shift_mV"


def test_telluric_needs_measured_shift():
    p = StrayCurrentInput(interference_type=InterferenceType.TELLURIC, soil_resistivity_ohm_m=50.0)
    with pytest.raises(ValueError, match="telluric"):
        assess_stray_current(p, experimental=True)


# ---------------------------------------------------------------------------
# DC source model
# ---------------------------------------------------------------------------


def test_attenuation_constant_hand_value():
    alpha = sc.pipe_attenuation_constant(0.5, 0.0125, 1.0e4, 2.0e-7, experimental=True)
    assert alpha == pytest.approx(ALPHA, rel=1e-12)
    assert alpha == pytest.approx(4.05096e-5, rel=1e-5)


@pytest.mark.parametrize("alpha", [1.0e-6, ALPHA, 1.0e-3, 1.0e-2, 0.1])
def test_shift_at_closest_point_matches_struve_closed_form(alpha):
    got = sc.point_source_shift_mV(0.0, 10.0, 100.0, 100.0, alpha, experimental=True)
    assert got == pytest.approx(_closed_form_shift_at_closest_point_mV(100.0, 10.0, 100.0, alpha),
                                rel=1e-8)


def test_shift_is_not_the_remote_earth_potential():
    """B10: the old model reported rho I / (2 pi d) itself as the pipe shift."""
    remote_earth_mV = 1000.0 * 100.0 * 10.0 / (2.0 * math.pi * 100.0)  # 1591.5 mV
    # Well-coated pipe (a -> 0): the shift at the closest point tends to -V_e ...
    near = sc.point_source_shift_mV(0.0, 10.0, 100.0, 100.0, 1.0e-7, experimental=True)
    assert near == pytest.approx(-remote_earth_mV, rel=5e-3)
    # ... a leaky pipe (a = 1) follows the earth potential and carries ~25 % of it,
    shift_a1 = sc.point_source_shift_mV(0.0, 10.0, 100.0, 100.0, 0.01, experimental=True)
    assert abs(shift_a1) == pytest.approx(0.24539 * remote_earth_mV, rel=1e-4)
    # ... and the governing (anodic) shift is far below the remote-earth potential.
    r = assess_stray_current(_dc_source(), experimental=True)
    assert 0.0 < r.potential_shift_mV < 0.02 * remote_earth_mV


def test_source_model_polarity():
    """Source (I > 0): pickup at the closest point, discharge (anodic) away from it.
    Sink (I < 0): the anodic peak is at the closest point and equals -dE_source(0)."""
    src = assess_stray_current(_dc_source(), experimental=True)
    assert src.method == "point_source_transmission_line"
    assert src.max_cathodic_shift_mV == pytest.approx(
        _closed_form_shift_at_closest_point_mV(100.0, 10.0, 100.0, ALPHA), rel=1e-6)
    assert src.anodic_peak_offset_m > 1000.0
    assert src.potential_shift_mV > 0.0

    sink = assess_stray_current(_dc_source(leakage_current_A=-10.0), experimental=True)
    assert sink.anodic_peak_offset_m == pytest.approx(0.0, abs=1e-3)
    assert sink.potential_shift_mV == pytest.approx(-src.max_cathodic_shift_mV, rel=1e-6)
    assert sink.max_cathodic_shift_mV == pytest.approx(-src.potential_shift_mV, rel=1e-6)
    assert sink.passes is False  # ~1556 mV against 150 mV


def test_shift_grows_linearly_with_leakage_current():
    shifts = [assess_stray_current(_dc_source(leakage_current_A=i), experimental=True).potential_shift_mV
              for i in (1.0, 5.0, 10.0, 50.0)]
    assert shifts == sorted(shifts)
    assert shifts[3] == pytest.approx(50.0 * shifts[0], rel=1e-6)


def test_shift_falls_with_distance():
    anodic, cathodic = [], []
    for d in (20.0, 50.0, 100.0, 200.0, 500.0, 1000.0):
        r = assess_stray_current(_dc_source(separation_distance_m=d), experimental=True)
        anodic.append(r.potential_shift_mV)
        cathodic.append(abs(r.max_cathodic_shift_mV))
    assert all(a > b for a, b in zip(anodic, anodic[1:]))
    assert all(a > b for a, b in zip(cathodic, cathodic[1:]))


def test_source_model_validity_and_missing_inputs():
    with pytest.raises(ValueError, match="must exceed the pipe OD"):
        assess_stray_current(_dc_source(separation_distance_m=0.4), experimental=True)
    with pytest.raises(ValueError, match="missing: coating_resistance_ohm_m2"):
        assess_stray_current(_dc_source(coating_resistance_ohm_m2=None), experimental=True)
    with pytest.raises(ValueError, match="including the coating IR drop"):
        assess_stray_current(_dc_source(shift_basis=ShiftBasis.EXCLUDING_IR), experimental=True)
    with pytest.raises(ValueError, match="less than half the OD"):
        sc.pipe_attenuation_constant(0.5, 0.3, 1.0e4, experimental=True)


# ---------------------------------------------------------------------------
# AC coupon density and criteria
# ---------------------------------------------------------------------------


def test_coupon_diameter_of_1_cm2():
    assert sc.coupon_diameter_m() == pytest.approx(0.0112838, rel=1e-5)


def test_ac_current_density_hand_value():
    # 8 * 10 / (100 * pi * 0.01) = 25.4648 A/m2
    got = sc.ac_current_density_A_m2(10.0, 100.0, 0.01, experimental=True)
    assert got == pytest.approx(80.0 / (math.pi), rel=1e-12)
    assert sc.ac_current_density_A_m2(0.0, 100.0, 0.01, experimental=True) == 0.0


@pytest.mark.parametrize(
    "kwargs, match",
    [
        (dict(ac_voltage_V=-1.0, soil_resistivity_ohm_m=100.0, holiday_diameter_m=0.01), "ac_voltage_V"),
        (dict(ac_voltage_V=1.0, soil_resistivity_ohm_m=0.0, holiday_diameter_m=0.01), "soil"),
        (dict(ac_voltage_V=1.0, soil_resistivity_ohm_m=100.0, holiday_diameter_m=0.0), "holiday"),
        (dict(ac_voltage_V=1.0, soil_resistivity_ohm_m=100.0, holiday_diameter_m=0.5,
              pipeline_od_m=0.3), "smaller than the pipe OD"),
    ],
)
def test_ac_current_density_validity_range(kwargs, match):
    with pytest.raises(ValueError, match=match):
        sc.ac_current_density_A_m2(**kwargs, experimental=True)


def test_negative_ac_voltage_rejected_at_input():
    with pytest.raises(ValidationError):
        StrayCurrentInput(interference_type=InterferenceType.AC_POWERLINE,
                          soil_resistivity_ohm_m=100.0, ac_voltage_V=-5.0)


def _ac(**kw: float) -> StrayCurrentInput:
    return StrayCurrentInput(interference_type=InterferenceType.AC_POWERLINE, **kw)


def test_ac_low_density_passes():
    r = assess_stray_current(_ac(soil_resistivity_ohm_m=100.0, ac_voltage_V=10.0), experimental=True)
    # 80 / (100 pi 0.0112838) = 22.568 A/m2 < 30
    assert r.ac_current_density_A_m2 == pytest.approx(22.568, rel=1e-4)
    assert r.ac_voltage_ok is True
    assert r.criteria_met == ["i_ac < limit"]
    assert r.passes is True
    assert "COUPON_AREA_M2" in r.provenance
    assert any("i_dc not given" in n for n in r.notes)


def test_ac_voltage_above_target_fails():
    r = assess_stray_current(_ac(soil_resistivity_ohm_m=1000.0, ac_voltage_V=20.0), experimental=True)
    assert r.ac_current_density_A_m2 < 30.0
    assert r.ac_voltage_ok is False
    assert r.passes is False


@pytest.mark.parametrize(
    "i_dc, passes, met",
    [
        (None, False, []),
        (0.5, True, ["i_dc < limit"]),  # ratio 210 > 3
        (50.0, True, ["i_ac/i_dc < limit"]),  # ratio 2.106 < 3
        (20.0, False, []),  # ratio 5.27, i_dc >= 1
    ],
)
def test_ac_alternative_criteria(i_dc, passes, met):
    # i_ac = 8 * 14 / (30 pi 0.0112838) = 105.31 A/m2 > 30
    r = assess_stray_current(_ac(soil_resistivity_ohm_m=30.0, ac_voltage_V=14.0,
                                 dc_current_density_A_m2=i_dc), experimental=True)
    assert r.ac_current_density_A_m2 == pytest.approx(105.31, rel=1e-4)
    assert r.passes is passes
    assert r.criteria_met == met
    if i_dc:
        assert r.ac_dc_ratio == pytest.approx(105.31 / i_dc, rel=1e-4)


def test_ac_non_coupon_holiday_is_flagged():
    r = assess_stray_current(_ac(soil_resistivity_ohm_m=100.0, ac_voltage_V=5.0,
                                 holiday_diameter_m=0.05, pipeline_od_m=0.3), experimental=True)
    assert any("1 cm2 coupon" in n for n in r.notes)


def test_ac_requires_voltage():
    with pytest.raises(ValueError, match="ac_voltage_V"):
        assess_stray_current(_ac(soil_resistivity_ohm_m=100.0), experimental=True)


# ---------------------------------------------------------------------------
# Drainage bond
# ---------------------------------------------------------------------------


def test_drainage_bond_ohms_law():
    r = design_drainage_bond(5.0, 2.0, 0.1, experimental=True)
    assert r.mitigation_type == "drainage_bond"
    assert r.bond_resistance_ohm == pytest.approx(0.3)  # 2/5 - 0.1
    assert r.bond_current_A == 5.0
    assert r.bond_power_W == pytest.approx(7.5)  # 25 * 0.3
    assert "Peabody" in r.provenance["bond"]


@pytest.mark.parametrize(
    "args, match",
    [
        ((0.0, 2.0), "required_drainage_current_A"),
        ((5.0, -1.0), "driving_voltage_V"),
        ((5.0, 2.0, -0.1), "circuit_resistance_ohm"),
        ((5.0, 0.4, 0.1), "cannot pass"),
    ],
)
def test_drainage_bond_input_validation(args, match):
    with pytest.raises(ValueError, match=match):
        design_drainage_bond(*args, experimental=True)
