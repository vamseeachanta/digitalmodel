"""Drilling-riser joint FFS — envelopes, collapse depth, placement, rollup (#1292).

Physics legs are compositions of golden-tested engines; these tests pin the
composition behavior and run the real-inspection fixtures end to end.
"""

import math
from pathlib import Path

import pandas as pd
import pytest

from digitalmodel.asset_integrity.corroded_pipe import SMYS_PSI, modified_b31g
from digitalmodel.asset_integrity.riser_joint_ffs import (
    COLLAPSE_DESIGN_FACTOR,
    DEPTH_CAP_BASE_METAL,
    DEPTH_CAP_WELD,
    SEAWATER_PSI_PER_FT,
    acceptable_water_depth_ft,
    collapse_pressure_psi,
    fleet_rollup,
    level1_flaw_envelope,
    place_joint,
)

DATA = Path(__file__).parent / "test_data" / "real_inspection"

# The real fleet's main-tube geometry.
OD, WT, GRADE, P_DESIGN = 21.25, 0.875, "X80", 500.0


# ---------------------------------------------------------------------------
# Level-1 envelope
# ---------------------------------------------------------------------------
def test_envelope_monotone_and_capped():
    env = level1_flaw_envelope(OD, WT, GRADE, P_DESIGN, region="base")
    lengths = env["max_acceptable_length_in"]
    nonzero = [v for v in lengths if v > 0]
    # deeper flaw -> shorter acceptable length
    assert nonzero == sorted(nonzero, reverse=True)
    # depths beyond the base-metal cap are excluded
    for frac, length in zip(env["depth_frac"], lengths):
        if frac > DEPTH_CAP_BASE_METAL:
            assert length == 0.0


def test_weld_region_stricter_than_base():
    base = level1_flaw_envelope(OD, WT, GRADE, P_DESIGN, region="base")
    weld = level1_flaw_envelope(OD, WT, GRADE, P_DESIGN, region="weld")
    assert weld["depth_cap_frac"] == DEPTH_CAP_WELD < DEPTH_CAP_BASE_METAL
    for frac, b, w in zip(base["depth_frac"], base["max_acceptable_length_in"],
                          weld["max_acceptable_length_in"]):
        if frac > DEPTH_CAP_WELD:
            assert w == 0.0
        else:
            assert w == b  # same strength physics inside the cap


def test_campaign_end_envelope_is_stricter():
    start = level1_flaw_envelope(OD, WT, GRADE, P_DESIGN)
    end = level1_flaw_envelope(
        OD, WT, GRADE, P_DESIGN,
        corrosion_rate_in_per_yr=0.25 / 25.4, campaign_years=3.0,
    )
    assert any(e < s for s, e in zip(start["max_acceptable_length_in"],
                                     end["max_acceptable_length_in"]))
    assert all(e <= s for s, e in zip(start["max_acceptable_length_in"],
                                      end["max_acceptable_length_in"]))


def test_envelope_boundary_matches_direct_method_check():
    """A flaw on the envelope passes the direct Modified B31G check; one just
    beyond it fails — the chart is exactly the validated method inverted.
    Uses a pressure high enough that strength (not the chart's length cap)
    governs the boundary."""
    p_gov = 3000.0
    env = level1_flaw_envelope(OD, WT, GRADE, p_gov, region="base")
    frac, d, L = None, None, None
    for frac, d, L in zip(env["depth_frac"], env["depth_in"],
                          env["max_acceptable_length_in"]):
        if 0 < L < 39.0:
            break
    assert L and 0 < L < 39.0
    on = modified_b31g(OD, WT, d, L, SMYS_PSI[GRADE]).safe_pressure_psi
    beyond = modified_b31g(OD, WT, d, L * 1.10, SMYS_PSI[GRADE]).safe_pressure_psi
    assert on >= p_gov > beyond


# ---------------------------------------------------------------------------
# Collapse-limited water depth
# ---------------------------------------------------------------------------
def test_collapse_pressure_interaction_hand_value():
    r = collapse_pressure_psi(OD, WT, SMYS_PSI["X80"])
    t_over_d = WT / OD
    p_el = 2 * 30e6 * t_over_d**3 / (1 - 0.3**2)
    p_y = 2 * 80_000 * t_over_d
    assert r["p_el_psi"] == pytest.approx(p_el)
    assert r["p_y_psi"] == pytest.approx(p_y)
    assert r["p_c_psi"] == pytest.approx(p_el * p_y / math.hypot(p_el, p_y))
    assert r["p_c_psi"] < min(p_el, p_y)


def test_acceptable_depth_monotone_in_wall():
    d_nominal = acceptable_water_depth_ft(OD, WT, GRADE)
    d_corroded = acceptable_water_depth_ft(OD, 15.02 / 25.4, GRADE)
    assert d_corroded < d_nominal
    # fully-evacuated criterion qualifies the nominal wall to ~5,900 ft; a
    # mud-filled basis (lower differential head) reaches farther
    assert 5_500 < d_nominal < 6_500
    assert acceptable_water_depth_ft(
        OD, WT, GRADE, differential_head_fraction=0.5) == pytest.approx(2 * d_nominal)
    p_c = collapse_pressure_psi(OD, WT, SMYS_PSI["X80"])["p_c_psi"]
    assert d_nominal == pytest.approx(
        COLLAPSE_DESIGN_FACTOR * p_c / SEAWATER_PSI_PER_FT)


# ---------------------------------------------------------------------------
# Placement + fleet rollup on the real register
# ---------------------------------------------------------------------------
def _register():
    return pd.read_csv(DATA / "gml_results_register.csv")


def test_place_severe_vs_mild_joint():
    reg = _register()
    main = reg[reg["component"] == "Main"].groupby("joint_id")["min_life_years"].min()
    kw = dict(od_in=OD, grade=GRADE, campaign_water_depth_ft=5_000.0,
              campaign_years=3.0)
    severe = place_joint("RJ-101", main["RJ-101"], 15.02 / 25.4, **kw)
    mild = place_joint("RJ-103", main["RJ-103"], 21.0 / 25.4, **kw)
    # severe joint: life fits only mid, but its thinnest wall is not
    # collapse-qualified even at 50% campaign depth -> repair
    assert severe.eligible_zones == []
    assert severe.verdict == "REPAIR"
    assert "50% campaign depth" in severe.reason
    # mild joint (31.5 yr) qualifies everywhere including high-fatigue bands
    assert mild.verdict == "ACCEPT"
    assert mild.eligible_zones == ["bottom", "mid", "top"]


def test_degraded_joint_restricted_to_mid_string():
    p = place_joint("RJ-x", min_life_years=4.0, wt_min_in=WT,
                    od_in=OD, grade=GRADE,
                    campaign_water_depth_ft=5_000.0, campaign_years=3.0)
    assert p.verdict == "ACCEPT"
    assert p.eligible_zones == ["mid"]  # low-fatigue band only


def test_fleet_rollup_on_real_register():
    roll = fleet_rollup(_register(), component="Main",
                        campaign_years=3.0, n_campaigns=4)
    assert roll["n_joints"] == 26
    c = roll["campaigns"]
    assert [x["campaign"] for x in c] == [1, 2, 3, 4]
    # fitness can only shrink with horizon
    fits = [x["fit_joints"] for x in c]
    assert fits == sorted(fits, reverse=True)
    assert all(x["fit_joints"] + x["repair_joints"] == 26 for x in c)
    assert all(x["high_fatigue_qualified"] <= x["fit_joints"] for x in c)
    # the real fleet has repairs by campaign 1 (worst joint: 3.46 yr < 3x margin)
    assert c[0]["high_fatigue_qualified"] < 26
