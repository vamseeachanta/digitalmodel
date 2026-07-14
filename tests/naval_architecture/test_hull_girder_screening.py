# ABOUTME: Core tests for hull-girder longitudinal strength screening —
# ABOUTME: golden box-barge SF/BM fixture, Bonjean waterline solve, allowables, SM.
"""Tests for :mod:`digitalmodel.naval_architecture.hull_girder_screening`.

GOLDEN FIXTURE (synthetic, published-textbook form)
---------------------------------------------------
The still-water SF/BM fixture is the classical *box barge with uniform
buoyancy and block cargo* worked-example form used throughout the standard
texts — E. V. Lewis (ed.), *Principles of Naval Architecture*, Vol. I, SNAME
(longitudinal strength: load curve and double integration); O. F. Hughes &
J. K. Paik, *Ship Structural Analysis and Design*, SNAME, ch. 3 (hull-girder
response of a box barge); C. B. Barrass & D. R. Derrett, *Ship Stability for
Masters and Mates* (box-barge shear/bending examples). No client-derived
values are used; the numbers below are the closed-form hand calculation.

Sagging case — box barge, length L = 100 m, total weight W = 10 000 t
spread uniformly over the middle half [L/4, 3L/4]; uniform buoyancy
b = W/L = 100 t/m (LCG = LCB = midship):

* load q = w - b: q = -100 t/m on the end quarters, +100 t/m on the middle
* SF(L/4) = -W/4 = -2 500 t (minimum), SF(3L/4) = +2 500 t, SF(L/2) = 0
* BM(L/4) = -(W/4)(L/4)/2 = -W L/32 = -31 250 t·m
* BM(L/2) = -W L/16 = -62 500 t·m  (sagging, negative by convention)
* SF and BM close to zero at both free ends (equilibrium closure).

Hogging case — the mirror condition (W/2 on each end quarter) gives
BM(L/2) = +W L/16 = +62 500 t·m.
"""

import numpy as np
import pytest

from digitalmodel.naval_architecture.hull_girder_screening import (
    AllowableCurve,
    DistributedWeight,
    HydroStation,
    PointWeight,
    SectionElement,
    build_weight_curve,
    buoyancy_box,
    buoyancy_direct,
    buoyancy_hydrostatics_table,
    condition_totals,
    section_modulus_screen,
    section_properties,
    solve_waterline,
    still_water_sf_bm,
    utilization_at_frames,
)

L = 100.0
W = 10_000.0
N = 401  # dx = 0.25 m; stations align with the L/4 block edges


def _sagging_case():
    x = np.linspace(0.0, L, N)
    cargo = [DistributedWeight("cargo", W, x_start_m=L / 4, x_end_m=3 * L / 4)]
    w = build_weight_curve(x, cargo)
    b, _ = buoyancy_box(x, w)
    return x, w, b


# --------------------------------------------------------------------------- #
# Golden fixture: SF/BM closes at the ends and matches the hand calc
# --------------------------------------------------------------------------- #
def test_sagging_barge_matches_hand_calc():
    x, w, b = _sagging_case()
    result = still_water_sf_bm(x, w, b)
    sf = np.asarray(result.shear_force_t)
    bm = np.asarray(result.bending_moment_t_m)

    # closure at the free ends
    assert abs(sf[-1]) < 1e-6
    assert abs(bm[-1]) < 1e-6
    assert result.closure_ok

    # hand-calc values (see module docstring); SF at x=20 m (inside the end
    # quarter, away from the block-edge smear) is exactly -b*x = -2 000 t
    i_20 = int(np.argmin(np.abs(x - 20.0)))
    i_q1 = int(np.argmin(np.abs(x - 25.0)))
    i_mid = int(np.argmin(np.abs(x - 50.0)))
    assert sf[i_20] == pytest.approx(-2_000.0, rel=1e-6)
    assert sf[i_q1] == pytest.approx(-W / 4, rel=1e-2)
    assert bm[i_q1] == pytest.approx(-W * L / 32, rel=1e-3)
    assert bm[i_mid] == pytest.approx(-W * L / 16, rel=1e-3)
    assert result.max_sagging_t_m == pytest.approx(-62_500.0, rel=1e-3)
    assert result.x_max_sagging_m == pytest.approx(50.0, abs=0.5)
    assert result.max_hogging_t_m == 0.0
    assert abs(result.max_shear_t) == pytest.approx(2_500.0, rel=1e-2)


def test_hogging_barge_sign_convention():
    x = np.linspace(0.0, L, N)
    cargo = [
        DistributedWeight("aft", W / 2, x_start_m=0.0, x_end_m=L / 4),
        DistributedWeight("fwd", W / 2, x_start_m=3 * L / 4, x_end_m=L),
    ]
    w = build_weight_curve(x, cargo)
    b, _ = buoyancy_box(x, w)
    result = still_water_sf_bm(x, w, b)
    # weight at the ends, buoyancy excess amidships -> hogging positive
    assert result.max_hogging_t_m == pytest.approx(+W * L / 16, rel=1e-3)
    assert result.max_sagging_t_m == 0.0


def test_shear_and_moment_are_antisymmetric_and_symmetric():
    x, w, b = _sagging_case()
    result = still_water_sf_bm(x, w, b)
    sf = np.asarray(result.shear_force_t)
    bm = np.asarray(result.bending_moment_t_m)
    assert sf == pytest.approx(-sf[::-1], abs=1e-6)
    assert bm == pytest.approx(bm[::-1], abs=1e-4)


# --------------------------------------------------------------------------- #
# Weight distribution builder
# --------------------------------------------------------------------------- #
def test_distributed_weight_trapezoid_centroid():
    item = DistributedWeight("tank", 300.0, x_start_m=10.0, x_end_m=40.0, lcg_m=28.0)
    xs = np.linspace(10.0, 40.0, 30001)
    dens = np.array([item.density_at(float(xi)) for xi in xs])
    total = np.trapz(dens, xs)
    lcg = np.trapz(dens * xs, xs) / total
    assert total == pytest.approx(300.0, rel=1e-6)
    assert lcg == pytest.approx(28.0, rel=1e-6)
    assert dens.min() >= 0.0


def test_distributed_weight_rejects_lcg_outside_middle_third():
    with pytest.raises(ValueError, match="middle third"):
        DistributedWeight("bad", 100.0, x_start_m=0.0, x_end_m=30.0, lcg_m=25.0)


def test_condition_totals_and_point_weights():
    items = [DistributedWeight("ls", 1000.0, 0.0, 100.0)]
    points = [PointWeight("crane", 100.0, x_m=80.0, extent_m=4.0)]
    total, lcg = condition_totals(items, points)
    assert total == pytest.approx(1100.0)
    assert lcg == pytest.approx((1000.0 * 50.0 + 100.0 * 80.0) / 1100.0)

    x = np.linspace(0.0, 100.0, 401)
    w = build_weight_curve(x, items, points)
    assert np.trapz(w, x) == pytest.approx(1100.0, rel=1e-6)


def test_weight_outside_hull_rejected():
    x = np.linspace(0.0, 100.0, 101)
    with pytest.raises(ValueError, match="beyond the hull"):
        build_weight_curve(x, [DistributedWeight("bad", 10.0, 90.0, 110.0)])


# --------------------------------------------------------------------------- #
# Buoyancy: Bonjean hydrostatics-table waterline solve
# --------------------------------------------------------------------------- #
def _box_stations(length=60.0, beam=8.0, depth=10.0):
    # A box hull expressed as a Bonjean table: A(T) = B * T at every station.
    table = {"drafts_m": (0.0, depth), "areas_m2": (0.0, beam * depth)}
    return [HydroStation(x_m=0.0, **table), HydroStation(x_m=length, **table)]


def test_waterline_solve_recovers_box_drafts():
    """Box barge, L=60 m, B=8 m, rho=1.025: W = 1968 t floats at T = 4.0 m
    even keel; shifting the LCG to 30.5 m trims it. Closed form for a box:
    mean draft T = W/(rho*B*L) = 4.0 m and end-draft difference
    s = 12*W*(lcg - L/2)/(rho*B*L^2) = 0.4 m -> T_aft = 3.8 m, T_fwd = 4.2 m.
    """
    length, beam, rho = 60.0, 8.0, 1.025
    x = np.linspace(0.0, length, 241)
    items = [DistributedWeight("ls", 1968.0, 0.0, length, lcg_m=30.5)]
    w = build_weight_curve(x, items)
    b, eq = buoyancy_hydrostatics_table(x, w, _box_stations(length, beam), rho)
    assert eq["draft_aft_m"] == pytest.approx(3.8, rel=1e-3)
    assert eq["draft_fwd_m"] == pytest.approx(4.2, rel=1e-3)
    assert eq["trim_m"] == pytest.approx(0.4, rel=1e-2)

    result = still_water_sf_bm(x, w, b, equilibrium=eq)
    assert result.closure_ok
    assert abs(result.closure_shear_fraction) < 1e-6
    assert abs(np.asarray(result.shear_force_t)[-1]) < 1e-6
    assert abs(np.asarray(result.bending_moment_t_m)[-1]) < 1e-6


def test_waterline_solve_rejects_overload():
    length = 60.0
    x = np.linspace(0.0, length, 121)
    with pytest.raises(ValueError, match="exceeds the buoyancy"):
        solve_waterline(_box_stations(length), 99_000.0, 30.0, x)


def test_direct_buoyancy_reports_bad_closure():
    x, w, _ = _sagging_case()
    # deliberately 2% light on buoyancy, no closure correction
    b_bad = np.full_like(x, 0.98 * W / L)
    b, _ = buoyancy_direct(x, x, b_bad)
    result = still_water_sf_bm(x, w, b, closure_correction=False)
    assert not result.closure_ok
    assert result.closure_shear_fraction == pytest.approx(0.02, rel=1e-3)


# --------------------------------------------------------------------------- #
# Allowable curves and frame utilisation
# --------------------------------------------------------------------------- #
def _allowables():
    sf = AllowableCurve(
        x_m=(0.0, 25.0, 75.0, 100.0),
        positive=(1000.0, 5000.0, 5000.0, 1000.0),
        negative=(1000.0, 5000.0, 5000.0, 1000.0),
    )
    bm = AllowableCurve(
        x_m=(0.0, 50.0, 100.0),
        positive=(20_000.0, 125_000.0, 20_000.0),
        negative=(20_000.0, 125_000.0, 20_000.0),
    )
    return sf, bm


def test_utilization_at_frames_pass():
    x, w, b = _sagging_case()
    result = still_water_sf_bm(x, w, b)
    sf_allow, bm_allow = _allowables()
    rows = utilization_at_frames(
        result,
        [("Fr 25", 25.0), ("Midship", 50.0)],
        sf_allow,
        bm_allow,
    )
    fr25, mid = rows
    assert fr25.shear_utilization == pytest.approx(2500.0 / 5000.0, rel=1e-2)
    assert mid.bending_utilization == pytest.approx(62_500.0 / 125_000.0, rel=1e-2)
    assert all(r.status == "pass" for r in rows)


def test_utilization_flags_exceedance():
    x, w, b = _sagging_case()
    result = still_water_sf_bm(x, w, b)
    bm_allow = AllowableCurve(
        x_m=(0.0, 100.0), positive=(50_000.0, 50_000.0),
        negative=(50_000.0, 50_000.0),
    )
    rows = utilization_at_frames(result, [("Midship", 50.0)], None, bm_allow)
    assert rows[0].status == "fail"
    assert rows[0].bending_utilization == pytest.approx(1.25, rel=1e-2)
    assert rows[0].shear_allowable_t is None


def test_frame_outside_hull_rejected():
    x, w, b = _sagging_case()
    result = still_water_sf_bm(x, w, b)
    with pytest.raises(ValueError, match="outside the hull"):
        utilization_at_frames(result, [("bad", 120.0)], None, None)


def test_allowable_curve_validation():
    with pytest.raises(ValueError, match="ascend"):
        AllowableCurve(x_m=(0.0, 0.0), positive=(1.0, 1.0), negative=(1.0, 1.0))
    with pytest.raises(ValueError, match="positive magnitudes"):
        AllowableCurve(x_m=(0.0, 1.0), positive=(1.0, -1.0), negative=(1.0, 1.0))


# --------------------------------------------------------------------------- #
# Section modulus
# --------------------------------------------------------------------------- #
def test_section_properties_box_girder_hand_calc():
    """Box girder B=10 m, D=5 m: deck 10x0.02 at z=5, bottom 10x0.03 at z=0,
    two sides 5x0.01 at z=2.5. Hand calc: A=0.6 m^2, NA = 25/12 m,
    I = 0.2*(5-z)^2 + 0.3*z^2 + 2*(0.01*5^3/12) + 0.1*(2.5-z)^2 (+ tiny plate
    self-inertia terms) = 3.2292 m^4.
    """
    elements = [
        SectionElement.horizontal("deck", 10.0, 0.02, z_m=5.0),
        SectionElement.horizontal("bottom", 10.0, 0.03, z_m=0.0),
        SectionElement.vertical("side_p", 5.0, 0.01, z_m=2.5),
        SectionElement.vertical("side_s", 5.0, 0.01, z_m=2.5),
    ]
    props = section_properties(elements, depth_m=5.0)
    z_na = 25.0 / 12.0
    assert props["area_m2"] == pytest.approx(0.6, rel=1e-9)
    assert props["neutral_axis_m"] == pytest.approx(z_na, rel=1e-9)
    inertia = (
        0.2 * (5.0 - z_na) ** 2
        + 0.3 * z_na**2
        + 2 * (0.01 * 5.0**3 / 12.0)
        + 0.1 * (2.5 - z_na) ** 2
    )
    assert props["inertia_m4"] == pytest.approx(inertia, rel=1e-4)
    assert props["sm_deck_m3"] == pytest.approx(inertia / (5.0 - z_na), rel=1e-4)
    assert props["sm_keel_m3"] == pytest.approx(inertia / z_na, rel=1e-4)


def test_section_modulus_screen_stress_and_utilization():
    x, w, b = _sagging_case()
    result = still_water_sf_bm(x, w, b)
    # Sagging extreme 62 500 t·m = 613 125 kN·m; Z = 5 m^3 -> 122.6 MPa;
    # permissible 175/k = 175 MPa for 235 grade -> utilisation 0.7007.
    screens = section_modulus_screen(
        result, sm_deck_m3=5.0, sm_keel_m3=None, yield_mpa=235.0,
        sm_source="approved: test",
    )
    sag_deck = [s for s in screens if s.condition == "sagging"][0]
    assert sag_deck.moment_kn_m == pytest.approx(-613_125.0, rel=1e-3)
    assert sag_deck.check.stress_mpa == pytest.approx(122.625, rel=1e-3)
    assert sag_deck.check.utilization == pytest.approx(0.7007, rel=1e-3)
    assert sag_deck.check.passes


def test_section_modulus_screen_with_s11_wave():
    x, w, b = _sagging_case()
    result = still_water_sf_bm(x, w, b)
    screens = section_modulus_screen(
        result, sm_deck_m3=5.0, sm_keel_m3=5.5, yield_mpa=235.0,
        sm_source="approved: test",
        wave={"beam_m": 20.0, "block_coefficient": 0.9},
    )
    sag = [s for s in screens if s.condition == "sagging" and s.location == "deck"][0]
    assert sag.wave_kn_m < 0.0  # S11 sagging wave moment is negative
    assert abs(sag.moment_kn_m) > abs(sag.still_water_kn_m)
    assert len(screens) == 4  # deck/keel x hog/sag


def test_section_properties_validation():
    with pytest.raises(ValueError, match="at least one element"):
        section_properties([])
    with pytest.raises(ValueError, match="neutral axis"):
        section_properties([SectionElement("deck", z_m=5.0, area_m2=1.0)], depth_m=5.0)
