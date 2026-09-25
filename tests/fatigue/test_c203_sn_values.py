"""DNV-RP-C203 S-N values against the October 2011 edition (#2165, owner C14).

Reference values are read from DNV-RP-C203 (October 2011) Table 2-1 (air),
Table 2-2 (seawater with cathodic protection) and Table 2-3 (seawater, free
corrosion), as cross-checked three ways in the verified 2011 dataset. The
library keeps its "2021" edition label (#2161); these tests pin the values
verified against the 2011 tables, not the label.

Defects covered:

* free corrosion reused the seawater-CP intercepts and kept m = 4 for B1/B2;
  Table 2-3 gives its own intercepts and m = 3 for every class, single slope,
  no knee and no fatigue limit;
* seawater with CP placed the knee at 1e7 cycles; Table 2-2 places it at 1e6,
  and the fatigue limit at 1e7 lies on the m2 = 5 segment;
* W2 log a2 was 13.855; Tables 2-1 and 2-2 give 13.845;
* one thickness exponent (0.25) for every class; the tables give k per class.
"""

from __future__ import annotations

import math

import numpy as np
import pytest

from digitalmodel.fatigue import c203_editions as ce
from digitalmodel.fatigue import sn_curves
from digitalmodel.fatigue.sn_library import get_catalog, get_library_curve
from digitalmodel.fatigue.sn_library_api import calculate_endurance, get_curve

CLASSES = ["B1", "B2", "C", "C1", "C2", "D", "E", "F", "F1", "F3", "G", "W1", "W2", "W3"]

# DNV-RP-C203 (2011) Table 2-1 / 2-2: m1, log a1 (air), log a1 (CP), log a2,
# fatigue limit at 1e7 cycles (MPa), thickness exponent k.
TABLE_2_1_2_2 = {
    "B1": (4.0, 15.117, 14.917, 17.146, 106.97, 0.0),
    "B2": (4.0, 14.885, 14.685, 16.856, 93.59, 0.0),
    "C": (3.0, 12.592, 12.192, 16.320, 73.10, 0.15),
    "C1": (3.0, 12.449, 12.049, 16.081, 65.50, 0.15),
    "C2": (3.0, 12.301, 11.901, 15.835, 58.48, 0.15),
    "D": (3.0, 12.164, 11.764, 15.606, 52.63, 0.20),
    "E": (3.0, 12.010, 11.610, 15.350, 46.78, 0.20),
    "F": (3.0, 11.855, 11.455, 15.091, 41.52, 0.25),
    "F1": (3.0, 11.699, 11.299, 14.832, 36.84, 0.25),
    "F3": (3.0, 11.546, 11.146, 14.576, 32.75, 0.25),
    "G": (3.0, 11.398, 10.998, 14.330, 29.24, 0.25),
    "W1": (3.0, 11.261, 10.861, 14.101, 26.32, 0.25),
    "W2": (3.0, 11.107, 10.707, 13.845, 23.39, 0.25),
    "W3": (3.0, 10.970, 10.570, 13.617, 21.05, 0.25),
}

# DNV-RP-C203 (2011) Table 2-3: log a (m = 3.0 for all cycles), k.
TABLE_2_3 = {
    "B1": (12.436, 0.0),
    "B2": (12.262, 0.0),
    "C": (12.115, 0.15),
    "C1": (11.972, 0.15),
    "C2": (11.824, 0.15),
    "D": (11.687, 0.20),
    "E": (11.533, 0.20),
    "F": (11.378, 0.25),
    "F1": (11.222, 0.25),
    "F3": (11.068, 0.25),
    "G": (10.921, 0.25),
    "W1": (10.784, 0.25),
    "W2": (10.630, 0.25),
    "W3": (10.493, 0.25),
}


def _rec(cls: str, env: str):
    return get_library_curve(f"DNV-RP-C203:{cls}:{env}")


# -- sn_library: free corrosion (Table 2-3) -----------------------------------


@pytest.mark.parametrize("cls", CLASSES)
def test_free_corrosion_matches_table_2_3(cls):
    log_a, k = TABLE_2_3[cls]
    r = _rec(cls, "free_corrosion")
    assert r.m1 == 3.0
    assert r.log_a1 == pytest.approx(log_a, abs=5e-4)
    assert r.m2 is None and r.log_a2 is None
    assert r.n_transition is None  # single slope: no knee
    assert r.endurance_limit is None  # no fatigue limit
    assert r.thickness_exponent == pytest.approx(k)


def test_free_corrosion_b1_hand_calc():
    """B1 at 100 MPa: N = 10^12.436 * 100^-3 = 10^6.436 = 2.729e6 cycles.

    Before #2165 the library used the CP curve (m = 4, log a = 14.917):
    10^14.917 * 100^-4 = 10^6.917 = 8.26e6 (non-conservative by 3.0x).
    """
    r = _rec("B1", "free_corrosion")
    assert r.cycles(100.0) == pytest.approx(10**6.436, rel=1e-9)
    assert r.cycles(100.0) == pytest.approx(2.729e6, rel=1e-3)


def test_free_corrosion_single_slope_at_low_stress():
    """D at 20 MPa: N = 10^11.687 * 20^-3 = 10^(11.687 - 3.90309) = 6.080e7."""
    r = _rec("D", "free_corrosion")
    assert r.cycles(20.0) == pytest.approx(10 ** (11.687 - 3 * math.log10(20.0)), rel=1e-9)
    assert r.cycles(20.0) == pytest.approx(6.080e7, rel=1e-3)


# -- sn_library: seawater with CP (Table 2-2) ----------------------------------


@pytest.mark.parametrize("cls", CLASSES)
def test_seawater_cp_matches_table_2_2(cls):
    m1, _, log_a1_cp, log_a2, fl, k = TABLE_2_1_2_2[cls]
    r = _rec(cls, "seawater_cp")
    assert r.m1 == m1
    assert r.log_a1 == pytest.approx(log_a1_cp, abs=5e-4)
    assert r.m2 == 5.0
    assert r.log_a2 == pytest.approx(log_a2, abs=5e-4)
    assert r.n_transition == 1e6
    assert r.endurance_limit == pytest.approx(fl, abs=5e-3)
    assert r.thickness_exponent == pytest.approx(k)


@pytest.mark.parametrize("cls", CLASSES)
def test_seawater_cp_fatigue_limit_on_m2_segment(cls):
    """Table 2-2 fatigue limit = 10^((log a2 - 7) / 5), within table rounding."""
    r = _rec(cls, "seawater_cp")
    on_m2 = 10 ** ((r.log_a2 - 7.0) / 5.0)
    assert r.endurance_limit == pytest.approx(on_m2, abs=0.01)


def test_seawater_cp_d_between_fatigue_limit_and_knee_uses_m2():
    """D (CP) at 60 MPa lies between the fatigue limit (52.63) and the 1e6 knee
    stress 10^((11.764 - 6) / 3) = 83.43 MPa, so the m2 = 5 segment governs:
    N = 10^15.606 * 60^-5 = 10^(15.606 - 8.89076) = 5.191e6 cycles.

    Before #2165 (knee at 1e7) the m1 segment was used:
    10^11.764 * 60^-3 = 2.688e6 (half the life).
    """
    r = _rec("D", "seawater_cp")
    assert r.cycles(60.0) == pytest.approx(10 ** (15.606 - 5 * math.log10(60.0)), rel=1e-9)
    assert r.cycles(60.0) == pytest.approx(5.191e6, rel=1e-3)


def test_seawater_cp_d_above_knee_uses_m1():
    """D (CP) at 100 MPa > 83.43 MPa knee: N = 10^11.764 / 1e6 = 5.808e5."""
    r = _rec("D", "seawater_cp")
    assert r.cycles(100.0) == pytest.approx(10 ** (11.764 - 6.0), rel=1e-9)


# -- sn_library: air (Table 2-1) -----------------------------------------------


@pytest.mark.parametrize("cls", CLASSES)
def test_air_matches_table_2_1(cls):
    m1, log_a1, _, log_a2, fl, k = TABLE_2_1_2_2[cls]
    r = _rec(cls, "air")
    assert r.m1 == m1
    assert r.log_a1 == pytest.approx(log_a1, abs=5e-4)
    assert r.m2 == 5.0
    assert r.log_a2 == pytest.approx(log_a2, abs=5e-4)
    assert r.n_transition == 1e7
    assert r.endurance_limit == pytest.approx(fl, abs=5e-3)
    assert r.thickness_exponent == pytest.approx(k)


@pytest.mark.parametrize("env", ["air", "seawater_cp"])
@pytest.mark.parametrize("cls", CLASSES)
def test_bilinear_segments_meet_at_knee(cls, env):
    """log N from both segments at the knee stress agree within table rounding.

    W2 with log a2 = 13.855 failed this by 0.010 in log N; 13.845 passes.
    """
    r = _rec(cls, env)
    s_knee = 10 ** ((r.log_a1 - math.log10(r.n_transition)) / r.m1)
    log_n_m2 = r.log_a2 - r.m2 * math.log10(s_knee)
    assert log_n_m2 == pytest.approx(math.log10(r.n_transition), abs=2e-3)


def test_w2_log_a2_is_13_845():
    assert _rec("W2", "air").log_a2 == pytest.approx(13.845)
    assert _rec("W2", "seawater_cp").log_a2 == pytest.approx(13.845)


def test_w2_air_below_knee_hand_calc():
    """W2 air at 15 MPa (< 23.39 fatigue limit): N = 10^13.845 * 15^-5
    = 10^(13.845 - 5.88046) = 9.216e7 (13.855 gave 9.431e7)."""
    r = _rec("W2", "air")
    assert r.cycles(15.0) == pytest.approx(9.216e7, rel=1e-3)


# -- sn_library_api follows the same knee ----------------------------------------


def test_api_calculate_endurance_uses_cp_knee():
    c = get_curve("DNV-RP-C203:D:seawater_cp")
    assert c.knee_point == 1e6
    assert calculate_endurance(c, 60.0) == pytest.approx(5.191e6, rel=1e-3)


def test_api_free_corrosion_has_no_knee():
    c = get_curve("DNV-RP-C203:B1:free_corrosion")
    assert c.knee_point is None
    assert c.m1 == 3.0
    assert calculate_endurance(c, 100.0) == pytest.approx(2.729e6, rel=1e-3)


# -- sn_curves (pyLife WoehlerCurve) ---------------------------------------------


@pytest.mark.parametrize("cls", CLASSES)
def test_sn_curves_free_corrosion_single_slope_m3(cls):
    log_a, _ = TABLE_2_3[cls]
    wc = sn_curves.get_sn_curve(cls, "free_corrosion")
    assert wc.k_1 == 3.0 and wc.k_2 == 3.0
    for s in (10.0, 50.0, 200.0):
        assert wc.cycles(s)[0] == pytest.approx(10**log_a * s**-3.0, rel=1e-9)


@pytest.mark.parametrize("cls", CLASSES)
def test_sn_curves_seawater_cp_knee_at_1e6(cls):
    m1, _, log_a1_cp, log_a2, _, _ = TABLE_2_1_2_2[cls]
    wc = sn_curves.get_sn_curve(cls, "seawater_cp")
    assert wc.ND == 1e6
    assert wc.SD == pytest.approx(10 ** ((log_a1_cp - 6.0) / m1), rel=1e-9)
    assert wc.k_2 == 5.0
    # below the knee the curve follows the tabulated m2 segment
    s = 0.8 * wc.SD
    assert wc.cycles(s)[0] == pytest.approx(10**log_a2 * s**-5.0, rel=2e-3)


def test_sn_curves_d_cp_hand_calc():
    """D (CP) at 60 MPa. pyLife anchors the m2 segment at the knee stress from
    log a1 (SD = 83.432 MPa at ND = 1e6): N = 1e6 * (60 / 83.432)^-5 = 5.199e6,
    within 0.2 % of the tabulated m2 segment (5.191e6; table rounding)."""
    wc = sn_curves.get_sn_curve("D", "seawater_cp")
    assert wc.cycles(60.0)[0] == pytest.approx(5.199e6, rel=1e-3)
    assert wc.cycles(60.0)[0] == pytest.approx(5.191e6, rel=2e-3)


def test_sn_curves_w2_log_a2():
    assert sn_curves.DNV_CURVES["W2"]["log_a2"] == pytest.approx(13.845)


# -- notes and edition labels -----------------------------------------------------


def test_notes_state_2011_verification_through_c203_editions():
    verified = ce.DNV_RP_C203_VERIFIED_EDITION
    assert verified == "2011"
    for r in get_catalog().curves:
        if r.standard != "DNV-RP-C203":
            continue
        table = ce.c203_sn_table(r.environment, verified)
        assert f"verified against {ce.c203_label(verified)} {table}" in r.note, r.note
        assert "not verified" not in r.note


def test_edition_label_unchanged():
    """The label question is out of scope for #2165: values are verified against
    2011 and the records keep the implemented-edition label."""
    records = [r for r in get_catalog().curves if r.standard == "DNV-RP-C203"]
    assert {r.standard_edition for r in records} == {ce.DNV_RP_C203_IMPLEMENTED_EDITION}


def test_catalog_count_unchanged():
    assert get_catalog().total_count == 221


def test_non_dnv_bilinear_curves_unchanged_by_knee_switch():
    """Switching segments at the knee stress (not the rounded endurance limit)
    leaves other standards unchanged away from the 0.01 MPa rounding band."""
    r = get_library_curve("IIW:FAT90:air")
    s = np.array([30.0, 100.0])
    expected = np.array(
        [10**r.log_a2 * 30.0 ** (-r.m2), 10**r.log_a1 * 100.0 ** (-r.m1)]
    )
    assert np.allclose(r.cycles(s), expected, rtol=1e-12)
