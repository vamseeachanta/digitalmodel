# ABOUTME: Tests for the box-hull loading computer / loadicator.
# ABOUTME: Equilibrium, intact + damage stability, longitudinal SF/BM, router.
"""Tests for ``digitalmodel.naval_architecture.loading_computer``.

Covers: equilibrium balances weight = buoyancy; intact GM is positive and the
IMO check passes for a stable load; damage flooding reduces GM; the SF/BM load
curve integrates correctly (BM is the integral of SF, both ~0 at the free ends);
and ``router(cfg)`` runs end-to-end and writes a JSON summary.
"""

import json
import math

import pytest

from digitalmodel.naval_architecture.loading_computer import (
    BoxHull,
    DamageCase,
    WeightItem,
    _cumulative_trapz,
    damage_stability,
    intact_stability,
    longitudinal_strength,
    router,
    run_loading_computer,
    solve_equilibrium,
)

RHO = 1.025


def _barge_hull() -> BoxHull:
    return BoxHull(length_m=90.0, beam_m=27.0, depth_m=6.0)


def _balanced_items() -> list[WeightItem]:
    # Symmetric about midship (x=45) -> even keel.
    return [
        WeightItem("lightship", 2500.0, 45.0, 3.0, length_m=90.0),
        WeightItem("cargo_fwd", 1500.0, 63.0, 4.5, length_m=24.0),
        WeightItem("cargo_aft", 1500.0, 27.0, 4.5, length_m=24.0),
        WeightItem("ballast", 500.0, 45.0, 1.0, length_m=30.0),
    ]


# --------------------------------------------------------------------------- #
# Equilibrium
# --------------------------------------------------------------------------- #
def test_equilibrium_balances_weight_and_buoyancy():
    hull = _barge_hull()
    items = _balanced_items()
    eq = solve_equilibrium(hull, items, RHO)

    total_weight = sum(it.weight_t for it in items)
    assert eq.displacement_t == pytest.approx(total_weight)
    # Buoyancy of the displaced box volume equals the total weight.
    buoyancy = eq.volume_m3 * RHO
    assert buoyancy == pytest.approx(total_weight, rel=1e-12)
    # Closed-form box draft T = V / (L*B).
    assert eq.draft_m == pytest.approx(
        eq.volume_m3 / (hull.length_m * hull.beam_m)
    )
    # KB = T/2, BM_T = B^2/(12T), KM = KB + BM_T.
    assert eq.kb_m == pytest.approx(eq.draft_m / 2.0)
    assert eq.bm_t_m == pytest.approx(hull.beam_m**2 / (12.0 * eq.draft_m))
    assert eq.km_m == pytest.approx(eq.kb_m + eq.bm_t_m)


def test_symmetric_load_is_even_keel_and_asymmetric_trims():
    hull = _barge_hull()
    eq_sym = solve_equilibrium(hull, _balanced_items(), RHO)
    assert eq_sym.trim_m == pytest.approx(0.0, abs=1e-9)
    assert eq_sym.trim_direction == "even keel"

    # Move weight forward -> trims by the bow.
    fwd = [
        WeightItem("lightship", 2500.0, 45.0, 3.0, length_m=90.0),
        WeightItem("cargo", 3000.0, 70.0, 4.5, length_m=24.0),
    ]
    eq_fwd = solve_equilibrium(hull, fwd, RHO)
    assert eq_fwd.lcg_m > hull.length_m / 2.0
    assert eq_fwd.trim_m > 0.0
    assert eq_fwd.trim_direction == "by the bow"


def test_overload_sinks_the_box_raises():
    hull = BoxHull(length_m=50.0, beam_m=10.0, depth_m=2.0)
    # Draft would exceed depth.
    heavy = [WeightItem("brick", 5000.0, 25.0, 1.0, length_m=50.0)]
    with pytest.raises(ValueError):
        solve_equilibrium(hull, heavy, RHO)


# --------------------------------------------------------------------------- #
# Intact stability
# --------------------------------------------------------------------------- #
def test_intact_stability_positive_gm_and_passes_imo():
    hull = _barge_hull()
    eq = solve_equilibrium(hull, _balanced_items(), RHO)
    intact = intact_stability(eq)

    assert eq.gm_t_m > 0.0
    assert intact.gm_m == pytest.approx(eq.gm_t_m)
    assert intact.gz_m[0] == pytest.approx(0.0)  # GZ(0) = 0
    assert intact.max_gz_m > 0.0
    assert intact.imo_check["overall_pass"] is True
    assert intact.screening_status == "pass"


def test_high_kg_reduces_gm_and_can_fail():
    hull = _barge_hull()
    low = solve_equilibrium(hull, _balanced_items(), RHO)
    # Same weights but very high VCG -> smaller GM.
    high_items = [
        WeightItem("lightship", 2500.0, 45.0, 5.8, length_m=90.0),
        WeightItem("cargo_fwd", 1500.0, 63.0, 5.8, length_m=24.0),
        WeightItem("cargo_aft", 1500.0, 27.0, 5.8, length_m=24.0),
        WeightItem("ballast", 500.0, 45.0, 5.8, length_m=30.0),
    ]
    high = solve_equilibrium(hull, high_items, RHO)
    assert high.gm_t_m < low.gm_t_m


# --------------------------------------------------------------------------- #
# Damage stability
# --------------------------------------------------------------------------- #
def test_damage_reduces_gm_and_increases_draft():
    hull = _barge_hull()
    eq = solve_equilibrium(hull, _balanced_items(), RHO)
    dmg = DamageCase("amidships", length_m=15.0, x_center_m=45.0, permeability=0.95)
    ds = damage_stability(hull, eq, dmg, RHO)

    assert ds.intact_gm_m == pytest.approx(eq.gm_t_m)
    assert ds.damaged_gm_m < eq.gm_t_m  # flooding reduces GM
    assert ds.gm_reduction_m > 0.0
    assert ds.sinkage_m > 0.0
    assert ds.damaged_draft_m > eq.draft_m  # parallel sinkage
    assert ds.lost_waterplane_inertia_m4 > 0.0


def test_bigger_damage_loses_more_gm():
    hull = _barge_hull()
    eq = solve_equilibrium(hull, _balanced_items(), RHO)
    small = damage_stability(
        hull, eq, DamageCase("s", 10.0, 45.0, 0.95), RHO
    )
    big = damage_stability(
        hull, eq, DamageCase("b", 30.0, 45.0, 0.95), RHO
    )
    assert big.gm_reduction_m > small.gm_reduction_m
    assert big.sinkage_m > small.sinkage_m


# --------------------------------------------------------------------------- #
# Longitudinal strength
# --------------------------------------------------------------------------- #
def test_sf_bm_zero_at_free_ends():
    hull = _barge_hull()
    items = _balanced_items()
    eq = solve_equilibrium(hull, items, RHO)
    ls = longitudinal_strength(hull, items, eq, n_stations=201)

    # Free-free beam in equilibrium: SF and BM vanish at both ends.
    assert ls.shear_force_t[0] == pytest.approx(0.0, abs=1e-9)
    assert ls.bending_moment_t_m[0] == pytest.approx(0.0, abs=1e-9)
    assert ls.shear_force_t[-1] == pytest.approx(0.0, abs=1e-6)
    assert ls.bending_moment_t_m[-1] == pytest.approx(0.0, abs=1e-4)
    assert ls.shear_end_residual_t == pytest.approx(0.0, abs=1e-6)
    assert ls.moment_end_residual_t_m == pytest.approx(0.0, abs=1e-4)


def test_bm_is_the_integral_of_sf():
    hull = _barge_hull()
    items = _balanced_items()
    eq = solve_equilibrium(hull, items, RHO)
    ls = longitudinal_strength(hull, items, eq, n_stations=151)

    recomputed_bm = _cumulative_trapz(ls.shear_force_t, ls.x_m)
    for got, expect in zip(ls.bending_moment_t_m, recomputed_bm):
        assert got == pytest.approx(expect, abs=1e-9)


def test_buoyancy_integrates_to_displacement():
    hull = _barge_hull()
    items = _balanced_items()
    eq = solve_equilibrium(hull, items, RHO)
    ls = longitudinal_strength(hull, items, eq, n_stations=201)

    total_buoyancy = _cumulative_trapz(ls.buoyancy_per_m_t, ls.x_m)[-1]
    total_weight = _cumulative_trapz(ls.weight_per_m_t, ls.x_m)[-1]
    # Buoyancy is pinned to the discrete weight so the ends balance exactly.
    assert total_buoyancy == pytest.approx(total_weight, rel=1e-9)
    # Both are within discretization error of the true displacement.
    assert total_buoyancy == pytest.approx(eq.displacement_t, rel=1e-2)


def test_bending_allowable_governs_status():
    hull = _barge_hull()
    items = _balanced_items()
    eq = solve_equilibrium(hull, items, RHO)
    ls_loose = longitudinal_strength(
        hull, items, eq, n_stations=201, allowable_bending_t_m=1e9
    )
    ls_tight = longitudinal_strength(
        hull, items, eq, n_stations=201, allowable_bending_t_m=1.0
    )
    assert ls_loose.screening_status == "pass"
    assert ls_tight.screening_status == "fail"
    assert ls_tight.max_bending_kn_m == pytest.approx(
        ls_tight.max_bending_t_m * 9.81
    )


# --------------------------------------------------------------------------- #
# Router
# --------------------------------------------------------------------------- #
def _settings() -> dict:
    return {
        "hull": {"length_m": 90.0, "beam_m": 27.0, "depth_m": 6.0},
        "water_density_t_m3": 1.025,
        "weight_items": [
            {"name": "lightship", "weight_t": 2500.0, "lcg_m": 45.0, "vcg_m": 3.0,
             "length_m": 90.0},
            {"name": "cargo_fwd", "weight_t": 1500.0, "lcg_m": 63.0, "vcg_m": 4.5,
             "length_m": 24.0},
            {"name": "cargo_aft", "weight_t": 1500.0, "lcg_m": 27.0, "vcg_m": 4.5,
             "length_m": 24.0},
            {"name": "ballast", "weight_t": 500.0, "lcg_m": 45.0, "vcg_m": 1.0,
             "length_m": 30.0},
        ],
        "longitudinal_strength": {"n_stations": 201,
                                  "allowable_bending_t_m": 60000.0},
        "damage_cases": [
            {"name": "amidships", "length_m": 15.0, "x_center_m": 45.0,
             "permeability": 0.95},
        ],
    }


def test_run_loading_computer_returns_full_result():
    result = run_loading_computer(_settings())
    assert set(result) >= {
        "equilibrium",
        "intact_stability",
        "damage_stability",
        "longitudinal_strength",
        "screening_status",
    }
    assert result["equilibrium"]["displacement_t"] == pytest.approx(6000.0)
    assert len(result["damage_stability"]) == 1
    assert result["screening_status"] in {"pass", "fail"}


def test_router_writes_summary_and_stamps_status(tmp_path):
    cfg = {
        "_config_dir_path": str(tmp_path),
        "loading_computer": {
            **_settings(),
            "outputs": {"directory": "results/loading_computer",
                        "summary_json": "summary.json"},
        },
    }
    out = router(cfg)

    assert out["screening_status"] in {"pass", "fail"}
    assert out["loading_computer"]["screening_status"] == out["screening_status"]
    summary_path = tmp_path / "results" / "loading_computer" / "summary.json"
    assert summary_path.exists()
    data = json.loads(summary_path.read_text())
    assert data["result"]["equilibrium"]["displacement_t"] == pytest.approx(6000.0)
    assert not math.isnan(data["result"]["intact_stability"]["gm_m"])
