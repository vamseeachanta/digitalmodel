"""DNV-RP-C203 curves in the structural, signal-processing and free-span paths
come from fatigue.c203_sn_tables (#2165 review r1 findings 1 and 2, owner C14).

Reference values: DNV-RP-C203 (October 2011) Table 2-1 (air) and Table 2-2
(seawater with CP), as verified for #2165. Every changed expectation carries a
hand calculation.

Representation per path (unchanged API shape):

* structural.fatigue.sn_curves (and its re-export digitalmodel.fatigue
  .get_dnv_curve): PowerLawSNCurve, in air. The m1 segment with the tabulated
  fatigue limit at 1e7 cycles as the cut-off; in air the knee is also at 1e7.
* signal_processing...fatigue.curves.SNCurve('standard', DNV): bilinear, in
  air, knee at 1e7, m2 = 5 from continuity, cut-off at the tabulated limit.
* subsea.pipeline.free_span._bilinear_sn: bilinear with its knee per
  environment (1e7 air, 1e6 CP) and the tabulated log a2.
"""

from __future__ import annotations

import math
from dataclasses import replace

import pytest

CLASSES = ["B1", "B2", "C", "C1", "C2", "D", "E", "F", "F1", "F3", "G", "W1", "W2", "W3"]

# DNV-RP-C203 (2011) Tables 2-1 / 2-2: m1, log a1 (air), log a1 (CP), m2,
# log a2, fatigue limit at 1e7 cycles (MPa), k.
TABLE = {
    "B1": (4.0, 15.117, 14.917, 5.0, 17.146, 106.97, 0.0),
    "B2": (4.0, 14.885, 14.685, 5.0, 16.856, 93.59, 0.0),
    "C": (3.0, 12.592, 12.192, 5.0, 16.320, 73.10, 0.15),
    "C1": (3.0, 12.449, 12.049, 5.0, 16.081, 65.50, 0.15),
    "C2": (3.0, 12.301, 11.901, 5.0, 15.835, 58.48, 0.15),
    "D": (3.0, 12.164, 11.764, 5.0, 15.606, 52.63, 0.20),
    "E": (3.0, 12.010, 11.610, 5.0, 15.350, 46.78, 0.20),
    "F": (3.0, 11.855, 11.455, 5.0, 15.091, 41.52, 0.25),
    "F1": (3.0, 11.699, 11.299, 5.0, 14.832, 36.84, 0.25),
    "F3": (3.0, 11.546, 11.146, 5.0, 14.576, 32.75, 0.25),
    "G": (3.0, 11.398, 10.998, 5.0, 14.330, 29.24, 0.25),
    "W1": (3.0, 11.261, 10.861, 5.0, 14.101, 26.32, 0.25),
    "W2": (3.0, 11.107, 10.707, 5.0, 13.845, 23.39, 0.25),
    "W3": (3.0, 10.970, 10.570, 5.0, 13.617, 21.05, 0.25),
}


def _above_limit(cls: str) -> float:
    """A stress range on the m1 segment in air: 1.5 x the fatigue limit."""
    return 1.5 * TABLE[cls][5]


# -- structural.fatigue.sn_curves (finding 1) -----------------------------------


@pytest.mark.parametrize("cls", CLASSES)
def test_structural_dnv_curve_matches_table_2_1(cls):
    from digitalmodel.structural.fatigue.sn_curves import StandardSNCurves

    m1, log_a1, _, _, _, fl, _ = TABLE[cls]
    c = StandardSNCurves.get_curve("DNV", cls)
    assert c.m == m1
    assert math.log10(c.A) == pytest.approx(log_a1, abs=1e-9)
    assert c.fatigue_limit == pytest.approx(fl)


@pytest.mark.parametrize("cls", CLASSES)
def test_structural_agrees_with_the_verified_library_above_the_limit(cls):
    """Above the fatigue limit the structural helper gives the same N as the
    verified library (sn_library, DNV-RP-C203 in air)."""
    from digitalmodel.fatigue.sn_library import get_library_curve
    from digitalmodel.structural.fatigue.sn_curves import get_dnv_curve

    s = _above_limit(cls)
    lib = get_library_curve(f"DNV-RP-C203:{cls}:air")
    assert get_dnv_curve(cls).get_allowable_cycles(s) == pytest.approx(
        lib.cycles(s), rel=1e-12
    )


def test_structural_b2_air_200_mpa_hand_calc():
    """B2 air at 200 MPa: N = 10^14.885 * 200^-4 = 7.6736e14 / 1.6e9 = 479,601.

    Before #2165 (A = 1.01e15, m = 3.5): 1.01e15 * 200^-3.5 = 8.927e6 (18.6x).
    """
    from digitalmodel import fatigue as dmf
    from digitalmodel.structural.fatigue.sn_curves import get_dnv_curve

    assert get_dnv_curve("B2").get_allowable_cycles(200.0) == pytest.approx(
        479_601, rel=1e-5
    )
    # the package re-export is the same helper
    assert dmf.get_dnv_curve("B2").get_allowable_cycles(200.0) == pytest.approx(
        479_601, rel=1e-5
    )


def test_structural_d_air_100_mpa_hand_calc():
    """D air at 100 MPa: N = 10^12.164 / 100^3 = 1.45881e12 / 1e6 = 1,458,814.

    Before #2165 (A = 5.73e11): 573,000 (the old A was 10^11.758)."""
    from digitalmodel.structural.fatigue.sn_curves import get_dnv_curve

    assert get_dnv_curve("D").get_allowable_cycles(100.0) == pytest.approx(
        1_458_814, rel=1e-6
    )


def test_structural_below_the_tabulated_limit_is_infinite():
    """E air: the tabulated limit is 46.78 MPa (the old 45.54 was not in the
    table), so 46.0 MPa is now below the cut-off."""
    from digitalmodel.structural.fatigue.sn_curves import get_dnv_curve

    assert math.isinf(get_dnv_curve("E").get_allowable_cycles(46.0))
    assert get_dnv_curve("E").fatigue_limit == pytest.approx(46.78)


def test_yaml_holds_no_dnv_curve_copy():
    """The DNV-RP-C203 curves have one source; the YAML keeps the other
    standards and the DNV multislope entries only."""
    from pathlib import Path

    import yaml

    data = yaml.safe_load(
        (Path(__file__).parents[2] / "data" / "fatigue" / "sn_curves.yaml").read_text()
    )
    assert "curves" not in data["standards"]["DNV"]
    assert "multislope" in data["standards"]["DNV"]


def test_yaml_dnv_curves_are_ignored_if_present(tmp_path, monkeypatch):
    """A data-dir YAML that still carries stale DNV curves cannot override the
    verified tables."""
    from digitalmodel.structural.fatigue.sn_curves import StandardSNCurves

    d = tmp_path / "fatigue"
    d.mkdir()
    (d / "sn_curves.yaml").write_text(
        "standards:\n  DNV:\n    curves:\n      D: {A: 5.73e11, m: 3.0, fatigue_limit: 52.63}\n"
    )
    monkeypatch.setenv("DIGITALMODEL_DATA_DIR", str(tmp_path))
    monkeypatch.setattr(StandardSNCurves, "_loaded_from_yaml", False)
    monkeypatch.setattr(StandardSNCurves, "DNV_CURVES", dict(StandardSNCurves.DNV_CURVES))
    c = StandardSNCurves.get_curve("DNV", "D")
    assert math.log10(c.A) == pytest.approx(12.164, abs=1e-9)


# -- signal_processing ... fatigue.curves (finding 1) ---------------------------


@pytest.mark.parametrize("cls", CLASSES)
def test_signal_dnv_curve_matches_table_2_1(cls):
    from digitalmodel.signal_processing.signal_analysis.fatigue import SNCurve

    m1, log_a1, _, m2, _, fl, _ = TABLE[cls]
    c = SNCurve(curve_type="standard", standard="DNV", **{"class": cls})
    assert c.curve_type == "bilinear"
    assert c.params["m"] == m1
    assert c.params["m2"] == m2
    assert math.log10(c.params["A"]) == pytest.approx(log_a1, abs=1e-9)
    assert c.params["fatigue_limit"] == pytest.approx(fl)
    assert c.params["N_transition"] == 1e7
    s = _above_limit(cls)
    assert float(c.get_allowable_cycles(s)) == pytest.approx(
        10**log_a1 * s**-m1, rel=1e-12
    )


def test_signal_b2_air_200_mpa_hand_calc():
    """B2 air at 200 MPa: 479,601 (see the structural hand calculation).
    Before #2165 (A = 1.01e15, m = 3.5): 8.927e6."""
    from digitalmodel.signal_processing.signal_analysis.fatigue import SNCurve

    c = SNCurve(curve_type="standard", standard="DNV", **{"class": "B2"})
    assert float(c.get_allowable_cycles(200.0)) == pytest.approx(479_601, rel=1e-5)


# -- subsea.pipeline.free_span._bilinear_sn (finding 2) -------------------------


@pytest.mark.parametrize("cls", CLASSES)
def test_free_span_cp_curve_matches_table_2_2(cls):
    from digitalmodel.subsea.pipeline.free_span._bilinear_sn import get_sn_curve

    m1, _, log_a1_cp, m2, log_a2, _, _ = TABLE[cls]
    p = get_sn_curve(cls, "seawater_cp").params
    assert p.N_transition == 1e6
    assert (p.m1, p.m2) == (m1, m2)
    assert math.log10(p.A1) == pytest.approx(log_a1_cp, abs=1e-9)
    assert math.log10(p.A2) == pytest.approx(log_a2, abs=1e-9)
    assert p.fatigue_limit == 0.0  # the curve object keeps no CP cut-off


@pytest.mark.parametrize("cls", CLASSES)
def test_free_span_air_curve_matches_table_2_1(cls):
    from digitalmodel.subsea.pipeline.free_span._bilinear_sn import get_sn_curve

    m1, log_a1, _, m2, log_a2, fl, _ = TABLE[cls]
    p = get_sn_curve(cls, "air").params
    assert p.N_transition == 1e7
    assert (p.m1, p.m2) == (m1, m2)
    assert math.log10(p.A1) == pytest.approx(log_a1, abs=1e-9)
    assert math.log10(p.A2) == pytest.approx(log_a2, abs=1e-9)
    assert p.fatigue_limit == pytest.approx(fl)


def test_free_span_b1_cp_200_mpa_hand_calc():
    """B1 CP at 200 MPa (above the 1e6 knee stress 10^((14.917 - 6)/4) =
    169.53 MPa): N = 10^14.917 * 200^-4 = 8.2604e14 / 1.6e9 = 516,274.
    Before #2165 (single slope, A = 2.3431e15): 1.464e6 (2.84x)."""
    from digitalmodel.subsea.pipeline.free_span._bilinear_sn import get_sn_curve

    assert get_sn_curve("B1", "seawater_cp").get_allowable_cycles(200.0) == pytest.approx(
        516_274, rel=1e-5
    )


def test_free_span_d_cp_60_mpa_uses_m2_below_the_1e6_knee():
    """D CP knee stress = 10^((11.764 - 6)/3) = 83.43 MPa; 60 MPa is below it:
    N = 10^(15.606 - 5 log10 60) = 10^(15.606 - 8.890756) = 5.191e6.
    Before #2165 (knee at 1e7, A1 = 2.83e11): 2.83e11 / 60^3 = 1.310e6."""
    from digitalmodel.subsea.pipeline.free_span._bilinear_sn import get_sn_curve

    c = get_sn_curve("D", "seawater_cp")
    assert c.params.transition_stress == pytest.approx(83.43, abs=5e-3)
    assert c.get_allowable_cycles(60.0) == pytest.approx(5.191e6, rel=1e-3)
    # above the knee: m1 with the CP intercept, 10^11.764 / 100^3 = 580,764
    assert c.get_allowable_cycles(100.0) == pytest.approx(580_764, rel=1e-5)


def test_free_span_thickness_exponent_defaults_from_the_class():
    """D (k = 0.20), 40 mm: A1 / (40/25)^(0.20 * 3); the old default k = 0.25
    applied to every class."""
    from digitalmodel.subsea.pipeline.free_span._bilinear_sn import get_sn_curve

    base = get_sn_curve("D", "air").params.A1
    thick = get_sn_curve("D", "air", thickness_mm=40.0).params.A1
    assert thick == pytest.approx(base / 1.6 ** (0.20 * 3.0), rel=1e-12)
    over = get_sn_curve("D", "air", thickness_mm=40.0, thickness_exponent=0.25)
    assert over.params.A1 == pytest.approx(base / 1.6 ** (0.25 * 3.0), rel=1e-12)


def _span_input(**kw):
    from digitalmodel.subsea.pipeline.free_span.models import (
        BoundaryConditionF105,
        EnvironmentType,
        PipeSpanInput,
    )

    inp = PipeSpanInput(
        od_m=0.2731,
        wt_m=0.0127,
        span_length_m=34.0,
        e_modulus_pa=207e9,
        steel_density_kgm3=7850.0,
        content_density_kgm3=900.0,
        water_density_kgm3=1025.0,
        current_velocity_ms=0.8,
        wave_velocity_ms=0.0,
        seabed_gap_m=0.5,
        bc=BoundaryConditionF105("pinned-pinned"),
        sag_m=0.0,
        structural_damping=0.005,
        hydrodynamic_damping=0.010,
        sn_curve_class="F",
        environment=EnvironmentType("seawater_cp"),
        gamma_on_IL=1.1,
        gamma_on_CF=1.3,
        gamma_k=1.15,
    )
    return replace(inp, **kw)


def test_span_fatigue_damage_uses_the_verified_cp_curve():
    """SpanFatigueDamage reaches _bilinear_sn directly. D CP at 60 MPa is above
    the tabulated fatigue limit (52.63 MPa; the same value in Tables 2-1 and
    2-2), so the cut-off does not apply and N = 5.191e6 (m2 below the 1e6 knee).
    B1 CP at 200 MPa gives 516,274 (m1)."""
    from digitalmodel.subsea.pipeline.free_span import SpanFatigueDamage

    d = SpanFatigueDamage(_span_input(sn_curve_class="D"), 0.5, 60.0)
    assert d.allowable_cycles(60.0) == pytest.approx(5.191e6, rel=1e-3)
    assert math.isinf(d.allowable_cycles(52.0))  # below the 52.63 MPa limit
    b1 = SpanFatigueDamage(_span_input(sn_curve_class="B1"), 0.5, 200.0)
    assert b1.allowable_cycles(200.0) == pytest.approx(516_274, rel=1e-5)
