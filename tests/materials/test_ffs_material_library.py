# ABOUTME: Tests for the FFS material library (#2171) — lower-bound toughness
# ABOUTME: estimates, true stress-strain curve for FEA, and grade lookup.
"""Tests for :mod:`digitalmodel.materials.ffs_material_library`.

Published anchors used below (numbers hand-entered, sources named inline):

* ASTM E1921 defines the Master Curve reference temperature ``T0`` as the
  temperature at which the median 1T fracture toughness is 100 MPa*sqrt(m);
  Wallin (Eng. Fract. Mech. 69, 2002) gives the 5 % lower-tolerance curve
  ``25.2 + 36.6 exp[0.019 (T - T0)]``, i.e. 61.8 MPa*sqrt(m) at ``T = T0``.
* ASME BPVC Section XI, Nonmandatory Appendix A, A-4200 lower-bound curves
  (reproduced in EPRI NP-719-SR, Marston 1978, and NUREG/CR-6609): at
  ``T = RT_NDT`` the K_Ic curve gives 33.2 + 20.734 = 53.93 ksi*sqrt(in) and
  the K_Ia/K_IR curve gives 26.78 + 1.223 exp(0.0145 x 160) = 39.22
  ksi*sqrt(in); 1 ksi*sqrt(in) = 1.0988 MPa*sqrt(m).
* Considere criterion (Dieter, Mechanical Metallurgy, 3rd ed., Ch. 8): for
  power-law hardening the true strain at the engineering ultimate equals the
  hardening exponent ``n``, and true ultimate = engineering ultimate x exp(n).
"""
from __future__ import annotations

import math

import pytest

from digitalmodel.asset_integrity.assessment import crack_fad
from digitalmodel.materials import get as get_grade
from digitalmodel.materials.ffs_material_library import (
    FFSMaterialInputs,
    ToughnessEstimate,
    TrueStressStrainCurve,
    kmat_from_charpy,
    lookup_grade,
    lower_bound_toughness,
)
from digitalmodel.units import Q_

KSI_SQRT_IN_TO_MPA_SQRT_M = 1.0988


# ---------------------------------------------------------------------------
# Toughness — single implementation, monotonic behaviour
# ---------------------------------------------------------------------------
def test_charpy_fallback_is_the_crack_fad_implementation():
    """One BS 7910 Annex J implementation: re-exported, not duplicated."""
    assert kmat_from_charpy is crack_fad.kmat_from_charpy
    est = lower_bound_toughness(cvn_j=40.0, thickness_mm=25.0, method="charpy")
    assert isinstance(est, ToughnessEstimate)
    assert math.isclose(
        est.k_mat_mpa_sqrt_m, crack_fad.kmat_from_charpy(40.0, 25.0), rel_tol=1e-12
    )
    assert float(est) == est.k_mat_mpa_sqrt_m


@pytest.mark.parametrize("thickness_mm", [12.0, 25.0, 50.0])
def test_toughness_monotonic_in_charpy_energy(thickness_mm):
    energies = [10.0, 20.0, 27.0, 40.0, 60.0, 80.0, 120.0]
    ks = [
        float(lower_bound_toughness(cvn_j=e, thickness_mm=thickness_mm, method="charpy"))
        for e in energies
    ]
    assert all(b > a for a, b in zip(ks, ks[1:]))


@pytest.mark.parametrize("method", ["asme_kic", "asme_kir"])
def test_toughness_monotonic_in_temperature_asme(method):
    temps = [-80.0, -60.0, -40.0, -20.0, 0.0, 20.0, 40.0]
    ks = [
        float(lower_bound_toughness(temperature_degc=t, t_ref_degc=-20.0,
                                    sigma_y_mpa=345.0, method=method))
        for t in temps
    ]
    assert all(b > a for a, b in zip(ks, ks[1:]))


def test_toughness_monotonic_in_temperature_master_curve():
    temps = [-60.0, -40.0, -20.0, 0.0, 20.0, 40.0]
    ks = [
        float(lower_bound_toughness(temperature_degc=t, t0_degc=-10.0,
                                    sigma_y_mpa=345.0, method="master_curve"))
        for t in temps
    ]
    assert all(b > a for a, b in zip(ks, ks[1:]))


def test_charpy_transition_route_monotonic_in_temperature():
    """T27J -> T0 (Wallin, T0 = T27J - 18 C) -> Master Curve lower bound."""
    temps = [-40.0, -20.0, 0.0, 10.0]  # T0 = -38 C, all within +/-50 C
    ks = [
        float(lower_bound_toughness(temperature_degc=t, t27j_degc=-20.0,
                                    sigma_y_mpa=355.0, method="charpy_transition"))
        for t in temps
    ]
    assert all(b > a for a, b in zip(ks, ks[1:]))
    # Same answer as calling the Master Curve with T0 = T27J - 18 directly.
    direct = lower_bound_toughness(temperature_degc=0.0, t0_degc=-38.0,
                                   sigma_y_mpa=355.0, method="master_curve")
    assert math.isclose(ks[2], float(direct), rel_tol=1e-12)


# ---------------------------------------------------------------------------
# Toughness — published anchors (hand-entered)
# ---------------------------------------------------------------------------
def test_master_curve_median_is_100_at_t0():
    """ASTM E1921: median 1T K_Jc at T = T0 is 100 MPa*sqrt(m) by definition."""
    est = lower_bound_toughness(temperature_degc=-15.0, t0_degc=-15.0,
                                sigma_y_mpa=400.0, method="master_curve",
                                fractile=0.5, thickness_mm=25.4)
    assert math.isclose(est.k_mat_mpa_sqrt_m, 100.0, rel_tol=0.01)


def test_master_curve_5pct_lower_bound_at_t0():
    """Wallin (2002): K_Jc(0.05) = 25.2 + 36.6 exp[0.019 (T - T0)] -> 61.8 at T0."""
    est = lower_bound_toughness(temperature_degc=10.0, t0_degc=10.0,
                                sigma_y_mpa=400.0, method="master_curve",
                                thickness_mm=25.4)
    assert math.isclose(est.k_mat_mpa_sqrt_m, 61.8, rel_tol=0.01)


def test_asme_kic_at_reference_temperature():
    """ASME XI App. A, A-4200 K_Ic at T = RT_NDT: 53.93 ksi*sqrt(in)."""
    est = lower_bound_toughness(temperature_degc=-10.0, t_ref_degc=-10.0,
                                sigma_y_mpa=345.0, method="asme_kic")
    expected = 53.934 * KSI_SQRT_IN_TO_MPA_SQRT_M
    assert math.isclose(est.k_mat_mpa_sqrt_m, expected, rel_tol=0.01)


def test_asme_kir_at_reference_temperature():
    """ASME XI App. A, A-4200 K_Ia (K_IR) at T = RT_NDT: 39.22 ksi*sqrt(in)."""
    est = lower_bound_toughness(temperature_degc=0.0, t_ref_degc=0.0,
                                sigma_y_mpa=345.0, method="asme_kir")
    expected = 39.22 * KSI_SQRT_IN_TO_MPA_SQRT_M
    assert math.isclose(est.k_mat_mpa_sqrt_m, expected, rel_tol=0.01)


def test_asme_curves_capped_at_upper_shelf():
    """ASME XI caps the lower-bound curves at 200 ksi*sqrt(in) (~220 MPa*sqrt(m))."""
    est = lower_bound_toughness(temperature_degc=200.0, t_ref_degc=-40.0,
                                sigma_y_mpa=345.0, method="asme_kic")
    assert math.isclose(est.k_mat_mpa_sqrt_m, 200.0 * KSI_SQRT_IN_TO_MPA_SQRT_M,
                        rel_tol=0.01)
    assert any("cap" in n.lower() for n in est.notes)


# ---------------------------------------------------------------------------
# Toughness — material-class limits and input validation
# ---------------------------------------------------------------------------
def test_toughness_rejects_non_ferritic_material_class():
    with pytest.raises(ValueError, match="ferritic"):
        lower_bound_toughness(cvn_j=40.0, method="charpy", material_class="austenitic")
    with pytest.raises(ValueError, match="ferritic"):
        lower_bound_toughness(temperature_degc=20.0, t0_degc=-20.0, sigma_y_mpa=205.0,
                              method="master_curve", material_class="austenitic")


def test_master_curve_yield_strength_scope():
    """ASTM E1921 scope: 275 <= sigma_y <= 825 MPa."""
    with pytest.raises(ValueError, match="275"):
        lower_bound_toughness(temperature_degc=0.0, t0_degc=-20.0, sigma_y_mpa=200.0,
                              method="master_curve")
    with pytest.raises(ValueError):
        lower_bound_toughness(temperature_degc=0.0, t0_degc=-20.0, sigma_y_mpa=900.0,
                              method="master_curve")


def test_master_curve_temperature_window_is_enforced():
    with pytest.raises(ValueError, match="50"):
        lower_bound_toughness(temperature_degc=80.0, t0_degc=-20.0, sigma_y_mpa=345.0,
                              method="master_curve")
    est = lower_bound_toughness(temperature_degc=80.0, t0_degc=-20.0, sigma_y_mpa=345.0,
                                method="master_curve", allow_extrapolation=True)
    assert est.k_mat_mpa_sqrt_m > 0
    assert any("extrapolat" in n.lower() for n in est.notes)


def test_asme_curves_yield_strength_limit():
    with pytest.raises(ValueError, match="620"):
        lower_bound_toughness(temperature_degc=0.0, t_ref_degc=-20.0, sigma_y_mpa=700.0,
                              method="asme_kic")


def test_toughness_requires_the_inputs_of_the_chosen_method():
    with pytest.raises(ValueError, match="cvn_j"):
        lower_bound_toughness(method="charpy")
    with pytest.raises(ValueError, match="t_ref_degc"):
        lower_bound_toughness(temperature_degc=0.0, sigma_y_mpa=345.0, method="asme_kic")
    with pytest.raises(ValueError, match="t0_degc"):
        lower_bound_toughness(temperature_degc=0.0, sigma_y_mpa=345.0, method="master_curve")
    with pytest.raises(ValueError, match="method"):
        lower_bound_toughness(cvn_j=40.0, method="not_a_method")


def test_toughness_auto_method_selection():
    a = lower_bound_toughness(cvn_j=40.0, thickness_mm=25.0)
    assert a.method == "charpy"
    b = lower_bound_toughness(temperature_degc=0.0, t0_degc=-20.0, sigma_y_mpa=345.0)
    assert b.method == "master_curve"
    c = lower_bound_toughness(temperature_degc=0.0, t_ref_degc=-20.0, sigma_y_mpa=345.0)
    assert c.method == "asme_kic"


def test_toughness_untagged_positional_input_rejected():
    with pytest.raises(TypeError):
        lower_bound_toughness(40.0)  # type: ignore[misc]


def test_toughness_wrong_unit_quantity_rejected():
    with pytest.raises(ValueError, match="cvn_j"):
        lower_bound_toughness(cvn_j=Q_(40.0, "MPa"), method="charpy")
    with pytest.raises(ValueError, match="temperature_degc"):
        lower_bound_toughness(temperature_degc=Q_(20.0, "mm"), t0_degc=-20.0,
                              sigma_y_mpa=345.0, method="master_curve")
    with pytest.raises(TypeError, match="cvn_j"):
        lower_bound_toughness(cvn_j="40 J", method="charpy")  # type: ignore[arg-type]


def test_toughness_tagged_quantities_are_converted():
    """Pint quantities in other units of the right dimension are accepted."""
    a = lower_bound_toughness(cvn_j=Q_(40.0, "J"), thickness_mm=Q_(1.0, "inch"),
                              method="charpy")
    b = lower_bound_toughness(cvn_j=40.0, thickness_mm=25.4, method="charpy")
    assert math.isclose(float(a), float(b), rel_tol=1e-12)
    c = lower_bound_toughness(temperature_degc=Q_(293.15, "kelvin"),
                              t0_degc=Q_(-4.0, "degF"),
                              sigma_y_mpa=Q_(50.0, "ksi"), method="master_curve")
    d = lower_bound_toughness(temperature_degc=20.0, t0_degc=-20.0,
                              sigma_y_mpa=344.74, method="master_curve")
    assert math.isclose(float(c), float(d), rel_tol=1e-6)


# ---------------------------------------------------------------------------
# True stress-strain curve
# ---------------------------------------------------------------------------
@pytest.fixture
def x65_curve() -> TrueStressStrainCurve:
    return TrueStressStrainCurve(E_mpa=207_000.0, sigma_y_mpa=450.0, sigma_u_mpa=535.0,
                                 n=0.12, temperature_degc=20.0)


def test_curve_passes_through_yield(x65_curve):
    eps_y = 450.0 / 207_000.0
    assert math.isclose(x65_curve.yield_strain, eps_y, rel_tol=1e-12)
    assert math.isclose(x65_curve.stress(eps_y), 450.0, rel_tol=1e-9)
    # Elastic below yield.
    assert math.isclose(x65_curve.stress(0.5 * eps_y), 225.0, rel_tol=1e-9)
    assert x65_curve.stress(0.0) == 0.0


def test_curve_reaches_true_ultimate_and_is_flat_beyond(x65_curve):
    """Considere: true strain at ultimate = n; true ultimate = sigma_u exp(n)."""
    eps_u = 0.12
    sigma_u_true = 535.0 * math.exp(0.12)
    assert math.isclose(x65_curve.ultimate_true_strain, eps_u, rel_tol=1e-12)
    assert math.isclose(x65_curve.sigma_u_true_mpa, sigma_u_true, rel_tol=1e-9)
    assert math.isclose(x65_curve.stress(eps_u), sigma_u_true, rel_tol=1e-9)
    for eps in (0.13, 0.2, 0.5, 1.0):
        assert math.isclose(x65_curve.stress(eps), sigma_u_true, rel_tol=1e-12)


def test_curve_hardening_branch_is_monotonic_and_bounded(x65_curve):
    eps = [x65_curve.yield_strain + k * (0.12 - x65_curve.yield_strain) / 50
           for k in range(51)]
    s = [x65_curve.stress(e) for e in eps]
    assert all(b >= a for a, b in zip(s, s[1:]))
    assert all(450.0 - 1e-9 <= v <= x65_curve.sigma_u_true_mpa + 1e-9 for v in s)


def test_curve_explicit_ultimate_strain_and_true_ultimate_input():
    c = TrueStressStrainCurve(E_mpa=200_000.0, sigma_y_mpa=300.0, sigma_u_mpa=600.0,
                              n=0.2, ultimate_true_strain=0.15, sigma_u_is_true=True)
    assert math.isclose(c.sigma_u_true_mpa, 600.0)
    assert math.isclose(c.stress(0.15), 600.0, rel_tol=1e-9)
    assert math.isclose(c.stress(0.3), 600.0, rel_tol=1e-12)


def test_curve_table_for_solver_decks(x65_curve):
    tbl = x65_curve.table(n_points=25)
    assert len(tbl) == 25
    strains = [p[0] for p in tbl]
    stresses = [p[1] for p in tbl]
    assert strains[0] == 0.0 and stresses[0] == 0.0
    assert all(b > a for a, b in zip(strains, strains[1:]))
    assert all(b >= a for a, b in zip(stresses, stresses[1:]))
    # Yield and ultimate points are present exactly.
    assert any(math.isclose(e, x65_curve.yield_strain, rel_tol=1e-12) for e in strains)
    assert any(math.isclose(e, x65_curve.ultimate_true_strain, rel_tol=1e-12)
               for e in strains)
    assert math.isclose(stresses[-1], x65_curve.sigma_u_true_mpa, rel_tol=1e-12)
    assert strains[-1] > x65_curve.ultimate_true_strain  # plateau point included

    plastic = x65_curve.plastic_table(n_points=10)
    assert plastic[0][0] == pytest.approx(450.0)
    assert plastic[0][1] == 0.0
    assert all(b[1] > a[1] for a, b in zip(plastic, plastic[1:]))
    assert all(b[0] >= a[0] for a, b in zip(plastic, plastic[1:]))


def test_curve_rejects_bad_inputs():
    with pytest.raises(ValueError):
        TrueStressStrainCurve(E_mpa=207_000.0, sigma_y_mpa=600.0, sigma_u_mpa=535.0, n=0.1)
    with pytest.raises(ValueError):
        TrueStressStrainCurve(E_mpa=207_000.0, sigma_y_mpa=450.0, sigma_u_mpa=535.0, n=1.5)
    with pytest.raises(ValueError):
        TrueStressStrainCurve(E_mpa=-1.0, sigma_y_mpa=450.0, sigma_u_mpa=535.0, n=0.1)
    with pytest.raises(ValueError):
        TrueStressStrainCurve(E_mpa=207_000.0, sigma_y_mpa=450.0, sigma_u_mpa=535.0, n=0.1,
                              ultimate_true_strain=0.001)  # below yield strain
    c = TrueStressStrainCurve(E_mpa=207_000.0, sigma_y_mpa=450.0, sigma_u_mpa=535.0, n=0.1)
    with pytest.raises(ValueError):
        c.stress(-0.01)
    with pytest.raises(ValueError):
        c.table(n_points=2)


def test_curve_untagged_positional_rejected_and_units_converted():
    with pytest.raises(TypeError):
        TrueStressStrainCurve(207_000.0, 450.0, 535.0, 0.12)  # type: ignore[misc]
    with pytest.raises(ValueError, match="sigma_y_mpa"):
        TrueStressStrainCurve(E_mpa=207_000.0, sigma_y_mpa=Q_(450.0, "mm"),
                              sigma_u_mpa=535.0, n=0.12)
    with pytest.raises(TypeError, match="E_mpa"):
        TrueStressStrainCurve(E_mpa="207 GPa", sigma_y_mpa=450.0,  # type: ignore[arg-type]
                              sigma_u_mpa=535.0, n=0.12)
    c = TrueStressStrainCurve(E_mpa=Q_(207.0, "GPa"), sigma_y_mpa=Q_(450.0, "MPa"),
                              sigma_u_mpa=Q_(535e6, "Pa"), n=0.12,
                              temperature_degc=Q_(68.0, "degF"))
    assert math.isclose(c.E_mpa, 207_000.0)
    assert math.isclose(c.sigma_u_mpa, 535.0)
    assert math.isclose(c.temperature_degc, 20.0, abs_tol=1e-9)


# ---------------------------------------------------------------------------
# Grade lookup
# ---------------------------------------------------------------------------
def test_lookup_carbon_steel_grade():
    m = lookup_grade("X65")
    assert isinstance(m, FFSMaterialInputs)
    g = get_grade("X65")
    assert m.sigma_y_mpa == g.smys_mpa == 450.0
    assert m.sigma_u_mpa == g.smts_mpa == 535.0
    assert m.E_mpa == g.E_mpa
    assert m.material_class == "ferritic"
    assert m.temperature_degc == 20.0
    assert m.hardening_exponent is None  # user input, no public grade-wise default
    curve = m.true_stress_strain_curve(n=0.12)
    assert math.isclose(curve.stress(curve.yield_strain), 450.0, rel_tol=1e-9)
    est = m.lower_bound_toughness(cvn_j=40.0, thickness_mm=20.0)
    assert est.method == "charpy"


def test_lookup_stainless_grade():
    m = lookup_grade("TP316L")
    assert m.material_class == "austenitic"
    assert m.sigma_y_mpa == 170.0     # ASTM A312/A312M, TP316L min yield (25 ksi)
    assert m.sigma_u_mpa == 485.0     # ASTM A312/A312M, TP316L min tensile (70 ksi)
    assert m.E_mpa == 193_000.0
    # The ferritic toughness correlations refuse austenitic material.
    with pytest.raises(ValueError, match="ferritic"):
        m.lower_bound_toughness(cvn_j=100.0)
    # The stress-strain curve does not care about the class.
    curve = m.true_stress_strain_curve(n=0.3)
    assert math.isclose(curve.stress(curve.yield_strain), 170.0, rel_tol=1e-9)
    # Alias forms.
    assert lookup_grade("316L").sigma_y_mpa == 170.0
    assert lookup_grade("tp316l").sigma_y_mpa == 170.0


def test_lookup_hull_steel_carries_charpy_test_temperature():
    m = lookup_grade("DH36")
    assert m.charpy_test_temperature_degc == -20.0
    assert lookup_grade("X65").charpy_test_temperature_degc is None


def test_lookup_unknown_grade():
    with pytest.raises(KeyError):
        lookup_grade("Unobtainium-9000")
