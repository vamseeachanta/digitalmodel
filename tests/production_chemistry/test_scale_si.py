# ABOUTME: Tests for production_chemistry.scale_si — ionic-strength hand-check,
# ABOUTME: qualitative Oddo-Tomson behavior anchors, profile/mixing contracts.

"""Tests for the mineral-scale saturation-index workflow (issue #1295).

Each test is labeled with its anchoring:

* derivation-anchored — checks arithmetic that follows from a stated
  formula (ionic strength definition, coefficient-override algebra,
  mixing-endpoint identity);
* behavior-anchored — checks well-known qualitative scale behaviors that
  any physically consistent coefficient set must reproduce (the module
  ships configurable defaults, not verified paper coefficients).
"""

import math

import pytest

from digitalmodel.production_chemistry.scale_si import (
    SCALE_FAMILIES,
    BrineComposition,
    CorrelationCoefficients,
    default_coefficients,
    ionic_strength,
    mix_brines,
    mixing_sweep,
    saturation_indices,
    si_profile,
)


# ---------------------------------------------------------------------------
# Fixtures: synthetic demo brines (no client data)
# ---------------------------------------------------------------------------


def formation_water(**overrides) -> BrineComposition:
    """Synthetic barium-rich, sulfate-poor formation water."""
    defaults = dict(
        na=31_000.0,
        k=400.0,
        mg=500.0,
        ca=2_000.0,
        sr=400.0,
        ba=250.0,
        fe=5.0,
        cl=53_000.0,
        so4=10.0,
        hco3=1_500.0,
        tds=89_000.0,
        ph=6.6,
        co2_mole_fraction_gas=0.02,
    )
    defaults.update(overrides)
    return BrineComposition(**defaults)


def seawater(**overrides) -> BrineComposition:
    """Standard-seawater major-ion composition (Millero et al. 2008)."""
    defaults = dict(
        na=10_770.0,
        k=399.0,
        mg=1_290.0,
        ca=412.0,
        sr=7.9,
        ba=0.01,
        fe=0.002,
        cl=19_350.0,
        so4=2_712.0,
        hco3=142.0,
        tds=35_170.0,
        ph=8.1,
    )
    defaults.update(overrides)
    return BrineComposition(**defaults)


# ---------------------------------------------------------------------------
# Ionic strength (derivation-anchored)
# ---------------------------------------------------------------------------


def test_ionic_strength_hand_check():
    """derivation-anchored: I = 1/2 sum(c_i z_i^2) for a 4-ion brine.

    0.1 M Na+ (2299 mg/L / 22.99), 0.1 M Cl-, 0.01 M Ca2+, 0.01 M SO4^2-:
    I = 0.5 * (0.1*1 + 0.1*1 + 0.01*4 + 0.01*4) = 0.14 mol/L.
    """
    brine = BrineComposition(
        na=2_299.0, cl=3_545.3, ca=400.78, so4=960.60, ph=7.0
    )
    assert brine.molarity("na") == pytest.approx(0.1, rel=1e-3)
    assert brine.molarity("ca") == pytest.approx(0.01, rel=1e-3)
    assert ionic_strength(brine) == pytest.approx(0.14, rel=1e-3)


def test_negative_concentration_rejected():
    """derivation-anchored: composition validation."""
    with pytest.raises(ValueError):
        BrineComposition(na=-1.0)


# ---------------------------------------------------------------------------
# saturation_indices contract
# ---------------------------------------------------------------------------


def test_all_families_returned_with_basis():
    res = saturation_indices(formation_water(), t_f=150.0, p_psia=2_000.0)
    assert set(res) == set(SCALE_FAMILIES)
    for family, r in res.items():
        assert r.family == family
        assert math.isfinite(r.si)
        assert "Oddo-Tomson" in r.basis
        # honesty statement must travel with every number
        assert "NOT verified paper values" in r.basis


def test_calcite_requires_carbonate_route():
    brine = formation_water(ph=None, co2_mole_fraction_gas=None)
    with pytest.raises(ValueError, match="calcite"):
        saturation_indices(brine, t_f=150.0, p_psia=2_000.0)


def test_calcite_gas_route_preferred_and_reports_estimated_ph():
    res = saturation_indices(formation_water(), t_f=150.0, p_psia=2_000.0)
    assert "gas-CO2 route" in res["calcite"].basis
    assert 3.0 < res["calcite"].details["estimated_ph"] < 9.0


def test_coefficient_override_shifts_si_by_a0_delta():
    """derivation-anchored: SI = log10(IAP) + pK_cond, so raising the barite
    a0 by exactly 1.0 must raise the barite SI by exactly 1.0."""
    brine = formation_water()
    base = saturation_indices(brine, 150.0, 2_000.0)["barite"].si
    coeffs = default_coefficients()
    c = coeffs["barite"]
    coeffs["barite"] = CorrelationCoefficients(
        a0=c.a0 + 1.0, a_t=c.a_t, a_t2=c.a_t2, a_t3=c.a_t3,
        a_p=c.a_p, a_sqrt_i=c.a_sqrt_i, a_i=c.a_i,
    )
    shifted = saturation_indices(brine, 150.0, 2_000.0, coefficients=coeffs)
    assert shifted["barite"].si == pytest.approx(base + 1.0, abs=1e-12)


# ---------------------------------------------------------------------------
# Qualitative behavior anchors (behavior-anchored)
# ---------------------------------------------------------------------------


def test_calcite_si_increases_with_temperature():
    """behavior-anchored: calcite is retrograde-soluble — heating a fixed
    composition raises the calcite SI (both carbonate routes)."""
    gas = formation_water()
    si_cold = saturation_indices(gas, 100.0, 3_000.0)["calcite"].si
    si_hot = saturation_indices(gas, 250.0, 3_000.0)["calcite"].si
    assert si_hot > si_cold

    ph_route = formation_water(co2_mole_fraction_gas=None)  # falls back to pH
    si_cold_ph = saturation_indices(ph_route, 100.0, 3_000.0)["calcite"].si
    si_hot_ph = saturation_indices(ph_route, 250.0, 3_000.0)["calcite"].si
    assert si_hot_ph > si_cold_ph


def test_pressure_drop_increases_calcite_si():
    """behavior-anchored: depressurization strips CO2 (raises in-situ pH),
    so calcite SI rises as pressure falls at fixed T — the classic reason
    calcite scale forms across chokes and up the tubing."""
    brine = formation_water()
    si_hi_p = saturation_indices(brine, 180.0, 6_000.0)["calcite"].si
    si_lo_p = saturation_indices(brine, 180.0, 500.0)["calcite"].si
    assert si_lo_p > si_hi_p


def test_barite_si_decreases_with_temperature_at_low_t():
    """behavior-anchored: barite solubility increases with temperature in
    the low-T range, so barite SI falls as the brine heats up."""
    brine = formation_water()
    si_cold = saturation_indices(brine, 80.0, 2_000.0)["barite"].si
    si_warm = saturation_indices(brine, 200.0, 2_000.0)["barite"].si
    assert si_warm < si_cold


def test_anhydrite_overtakes_gypsum_at_high_temperature():
    """behavior-anchored: gypsum is the stable CaSO4 phase cold, anhydrite
    hot (crossover ~40 degC) — so the anhydrite SI must exceed the gypsum
    SI at high T and sit below it at low T."""
    brine = formation_water()
    cold = saturation_indices(brine, 70.0, 1_000.0)
    hot = saturation_indices(brine, 250.0, 1_000.0)
    assert cold["anhydrite"].si < cold["gypsum"].si
    assert hot["anhydrite"].si > hot["gypsum"].si


def test_halite_only_near_saturation_tds():
    """behavior-anchored: halite SI is strongly negative for a typical
    ~90 g/L brine and approaches 0 only for a near-saturated NaCl brine."""
    typical = saturation_indices(formation_water(), 100.0, 1_000.0)["halite"].si
    assert typical < -0.5

    near_sat = formation_water(
        na=140_000.0, cl=216_000.0, tds=360_000.0
    )  # ~6.1 mol/L Na and Cl = saturated NaCl at ambient
    si_sat = saturation_indices(near_sat, 77.0, 1_000.0)["halite"].si
    assert si_sat > typical
    assert abs(si_sat) < 0.4


# ---------------------------------------------------------------------------
# si_profile (bottomhole -> wellhead trending)
# ---------------------------------------------------------------------------


def _linear_profile(n=10, t=(250.0, 120.0), p=(9_000.0, 250.0)):
    return [
        (t[0] + (t[1] - t[0]) * i / (n - 1), p[0] + (p[1] - p[0]) * i / (n - 1))
        for i in range(n)
    ]


def test_si_profile_shape_order_and_dsi():
    brine = formation_water()
    profile = _linear_profile()
    df = si_profile(brine, profile)
    assert len(df) == len(profile)
    # rows returned in input (bottomhole -> wellhead) order
    assert list(df["t_f"]) == [tp[0] for tp in profile]
    assert list(df["p_psia"]) == [tp[1] for tp in profile]
    for family in SCALE_FAMILIES:
        assert f"si_{family}" in df.columns
        assert df[f"dsi_{family}"].iloc[0] == pytest.approx(0.0, abs=1e-12)


def test_si_profile_pure_depressurization_is_monotonic_for_calcite():
    """behavior-anchored: along an isothermal monotonic pressure drawdown
    the calcite SI must rise monotonically step over step."""
    brine = formation_water()
    profile = [(180.0, p) for p in (8_000.0, 6_000.0, 4_000.0, 2_000.0, 500.0)]
    si = si_profile(brine, profile)["si_calcite"].to_list()
    assert all(b > a for a, b in zip(si, si[1:]))


def test_si_profile_empty_rejected():
    with pytest.raises(ValueError, match="tp_profile"):
        si_profile(formation_water(), [])


# ---------------------------------------------------------------------------
# mixing_sweep (waterflood compatibility)
# ---------------------------------------------------------------------------


def test_mixing_endpoints_equal_pure_brines():
    """derivation-anchored: fraction 0 and 1 must reproduce the pure-brine
    saturation indices exactly."""
    fw, sw = formation_water(), seawater()
    t_f, p = 100.0, 150.0
    df = mixing_sweep(fw, sw, [0.0, 0.5, 1.0], t_f=t_f, p_psia=p)
    pure_a = saturation_indices(fw, t_f, p)
    pure_b = saturation_indices(sw, t_f, p)
    for family in SCALE_FAMILIES:
        assert df[f"si_{family}"].iloc[0] == pytest.approx(
            pure_a[family].si, abs=1e-9
        )
        assert df[f"si_{family}"].iloc[-1] == pytest.approx(
            pure_b[family].si, abs=1e-9
        )


def test_seawater_mixing_drives_barite_si_up():
    """behavior-anchored: blending sulfate-rich seawater into barium-rich
    formation water must push the barite SI above both endpoints — the
    classic waterflood incompatibility."""
    fractions = [i / 20 for i in range(21)]
    df = mixing_sweep(formation_water(), seawater(), fractions, t_f=100.0,
                      p_psia=150.0)
    si = df["si_barite"]
    assert si.max() > si.iloc[0]
    assert si.max() > si.iloc[-1]
    # and the peak sits at an interior fraction
    peak_fraction = df.loc[si.idxmax(), "fraction_b"]
    assert 0.0 < peak_fraction < 1.0


def test_mix_brines_linear_concentrations_and_h_ion_mixing():
    """derivation-anchored: concentrations mix linearly with volume
    fraction; hydrogen-ion concentration (not pH) mixes linearly."""
    fw, sw = formation_water(), seawater()
    mixed = mix_brines(fw, sw, 0.25)
    assert mixed.ba == pytest.approx(0.75 * fw.ba + 0.25 * sw.ba, rel=1e-12)
    assert mixed.so4 == pytest.approx(0.75 * fw.so4 + 0.25 * sw.so4, rel=1e-12)
    h_expected = 0.75 * 10.0 ** (-fw.ph) + 0.25 * 10.0 ** (-sw.ph)
    assert mixed.ph == pytest.approx(-math.log10(h_expected), rel=1e-12)
    # CO2 fraction defined on only one brine -> dropped at interior fractions
    assert mixed.co2_mole_fraction_gas is None


def test_mixing_fraction_out_of_range_rejected():
    with pytest.raises(ValueError, match="fraction_b"):
        mix_brines(formation_water(), seawater(), 1.5)
