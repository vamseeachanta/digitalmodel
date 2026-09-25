"""DNV-RP-C203 curves count variable-amplitude damage below the old cut-off
(#2165, PR #2195 review r1 findings 2-4, owner C14).

DNV-RP-C203 (2011) section 2.4: the in-air and seawater-with-CP curves are
bilinear, with m2 = 5 below the knee, and the standard has no cut-off for
variable-amplitude loading. Reference values: Tables 2-1 (air), 2-2 (seawater
with CP) and 2-3 (free corrosion), as held in fatigue.c203_sn_tables.

Every expectation carries its hand calculation.
"""

from __future__ import annotations

import math
from dataclasses import replace

import numpy as np
import pandas as pd
import pytest

# 10^15.350 / 46^5: log10(46) = 1.6627578, x5 = 8.3137892,
# 15.350 - 8.3137892 = 7.0362108 -> N = 1.08695e7 cycles.
N_E_AIR_46 = 10**15.350 / 46.0**5
# Miner damage for 1e7 cycles at 46 MPa: 1e7 / 1.08695e7 = 0.920003.
D_E_AIR_46 = 1e7 / N_E_AIR_46


def test_hand_values():
    assert N_E_AIR_46 == pytest.approx(10_869_532, rel=1e-6)
    assert D_E_AIR_46 == pytest.approx(0.920003, abs=1e-6)


def _hist46():
    return pd.DataFrame({"range": [46.0], "mean": [0.0], "count": [1e7]})


# -- structural.fatigue (finding 2) --------------------------------------------


def test_structural_e_air_46_mpa_uses_m2_below_the_knee():
    """E air knee stress = (10^12.010 / 1e7)^(1/3) = 10^1.67 = 46.77 MPa;
    46 MPa lies on the m2 = 5 segment: N = 1.08695e7 (was infinite)."""
    from digitalmodel.structural.fatigue.sn_curves import get_dnv_curve

    c = get_dnv_curve("E")
    assert c.knee_stress == pytest.approx(10 ** (5.010 / 3.0), rel=1e-12)
    assert c.get_allowable_cycles(46.0) == pytest.approx(N_E_AIR_46, rel=1e-12)
    # above the knee: unchanged m1 segment, D 100 MPa = 1,458,814
    assert get_dnv_curve("D").get_allowable_cycles(100.0) == pytest.approx(
        1_458_814, rel=1e-6
    )
    # the tabulated fatigue limit is kept for reporting, not as a cut-off
    assert c.fatigue_limit == pytest.approx(46.78)


def test_structural_no_cut_off_far_below_the_knee():
    """E air at 10 MPa: 10^15.350 / 10^5 = 10^10.350 = 2.2387e10 (finite)."""
    from digitalmodel.structural.fatigue.sn_curves import get_dnv_curve

    n = get_dnv_curve("E").get_allowable_cycles(np.array([10.0, 46.0]))
    assert n[0] == pytest.approx(10**10.350, rel=1e-12)
    assert n[1] == pytest.approx(N_E_AIR_46, rel=1e-12)


def test_structural_miner_damage_below_the_old_cut_off():
    """LinearDamageAccumulation, 1e7 cycles at 46 MPa on E air: D = 0.920003
    (was 0)."""
    from digitalmodel.structural.fatigue.damage_accumulation import (
        LinearDamageAccumulation,
    )
    from digitalmodel.structural.fatigue.sn_curves import get_dnv_curve

    res = LinearDamageAccumulation().calculate_damage(_hist46(), get_dnv_curve("E"))
    assert res["total_damage"] == pytest.approx(D_E_AIR_46, rel=1e-9)


def test_structural_stress_range_inverse_on_both_segments():
    """Inverse: N = 1.08695e7 > 1e7 returns 46 MPa (m2); N = 1e6 on D returns
    (10^12.164 / 1e6)^(1/3) = 10^(6.164/3) = 113.34 MPa (m1)."""
    from digitalmodel.structural.fatigue.sn_curves import get_dnv_curve

    assert get_dnv_curve("E").get_stress_range(N_E_AIR_46) == pytest.approx(
        46.0, rel=1e-12
    )
    assert get_dnv_curve("D").get_stress_range(1e6) == pytest.approx(
        10 ** (6.164 / 3.0), rel=1e-12
    )


def test_structural_thickness_correction_keeps_the_second_segment():
    """D, 40 mm, k = 0.20: t-factor 1.6^0.20 = 1.098560, so A2 / 1.6^(0.20*5)
    = 10^15.606 / 1.6. At 40 MPa (below the corrected knee 52.63 / 1.09856 =
    47.91 MPa): N = 10^15.606 / 1.6 / 40^5 = 4.0365e15 / 1.6 / 1.024e8 =
    2.4637e7 (was infinite)."""
    from digitalmodel.structural.fatigue.sn_curves import get_dnv_curve

    c = get_dnv_curve("D", thickness=40.0)
    assert c.A2 == pytest.approx(10**15.606 / 1.6, rel=1e-12)
    assert c.get_allowable_cycles(40.0) == pytest.approx(
        10**15.606 / 1.6 / 40.0**5, rel=1e-12
    )
    assert c.get_allowable_cycles(40.0) == pytest.approx(2.4637e7, rel=1e-4)


def test_structural_narrow_band_damage_below_the_old_cut_off():
    """Frequency-domain narrow band, 2 sigma = 46 MPa on E air: damage rate =
    nu0 / N(46) with N = 1.08695e7 (was 0)."""
    from digitalmodel.structural.fatigue.frequency_domain import NarrowBandMethod
    from digitalmodel.structural.fatigue.sn_curves import get_dnv_curve

    f = np.linspace(0.05, 0.15, 201)
    psd = np.full_like(f, 23.0**2 / (f[-1] - f[0]))
    res = NarrowBandMethod().calculate_damage_rate(f, psd, get_dnv_curve("E"))
    assert res.damage_rate > 0.0
    n = get_dnv_curve("E").get_allowable_cycles(res.equivalent_stress)
    assert res.damage_rate == pytest.approx(
        res.expected_cycles_per_second / n, rel=1e-12
    )


def test_structural_cp_curve_from_table_2_2():
    """Seawater with CP, D: log a1 = 11.764, knee at 1e6, the same log a2 =
    15.606. Knee stress = 10^((11.764 - 6)/3) = 83.43 MPa.
    60 MPa: 10^(15.606 - 5 log10 60) = 5.191e6; 100 MPa: 10^11.764 / 1e6 =
    580,764."""
    from digitalmodel.structural.fatigue.sn_curves import StandardSNCurves

    c = StandardSNCurves.get_dnv_c203_curve("D", "seawater_cp")
    assert math.log10(c.A) == pytest.approx(11.764, abs=1e-9)
    assert math.log10(c.A2) == pytest.approx(15.606, abs=1e-9)
    assert c.knee_cycles == 1e6
    assert c.knee_stress == pytest.approx(83.43, abs=5e-3)
    assert c.get_allowable_cycles(60.0) == pytest.approx(5.191e6, rel=1e-3)
    assert c.get_allowable_cycles(100.0) == pytest.approx(580_764, rel=1e-5)


def test_structural_free_corrosion_curve_from_table_2_3():
    """Free corrosion, F1: log a = 11.222, m = 3, no knee, no limit.
    At 30 MPa: 10^11.222 / 27,000 = 1.6672e11 / 2.7e4 = 6.1749e6."""
    from digitalmodel.structural.fatigue.sn_curves import StandardSNCurves

    c = StandardSNCurves.get_dnv_c203_curve("F1", "free_corrosion")
    assert math.log10(c.A) == pytest.approx(11.222, abs=1e-9)
    assert c.m == 3.0
    assert c.m2 is None
    assert c.fatigue_limit == 0.0
    assert c.get_allowable_cycles(30.0) == pytest.approx(10**11.222 / 30.0**3)


def test_worked_example_cp_curve_is_the_table_2_2_curve():
    """The pipeline girth-weld example uses the Table 2-2 D curve (was the air
    curve x 0.87 with the air cut-off)."""
    from digitalmodel.structural.fatigue.worked_examples import (
        _dnv_seawater_cp,
        _dnv_seawater_free,
    )

    cp = _dnv_seawater_cp("D")
    assert math.log10(cp.A) == pytest.approx(11.764, abs=1e-9)
    assert cp.get_allowable_cycles(40.0) == pytest.approx(
        10**15.606 / 40.0**5, rel=1e-12
    )
    free = _dnv_seawater_free("F1")
    assert math.log10(free.A) == pytest.approx(11.222, abs=1e-9)


def test_non_dnv_curves_keep_their_cut_off():
    """API X keeps its 48 MPa cut-off; BS D keeps 50 MPa."""
    from digitalmodel.structural.fatigue.sn_curves import StandardSNCurves

    assert math.isinf(StandardSNCurves.get_curve("API", "X").get_allowable_cycles(47.0))
    assert math.isinf(StandardSNCurves.get_curve("BS", "D").get_allowable_cycles(49.0))
    assert StandardSNCurves.get_curve("API", "X").m2 is None


# -- signal_processing (finding 2) ---------------------------------------------


def _signal_e():
    from digitalmodel.signal_processing.signal_analysis.fatigue import SNCurve

    return SNCurve(curve_type="standard", standard="DNV", **{"class": "E"})


def test_signal_e_air_46_mpa_uses_m2_below_the_knee():
    c = _signal_e()
    assert float(c.get_allowable_cycles(46.0)) == pytest.approx(N_E_AIR_46, rel=1e-12)


def test_signal_damage_calculator_counts_damage_below_the_old_cut_off():
    """FatigueDamageCalculator with SNCurve.get_curve_parameters() for E air:
    1e7 cycles at 46 MPa -> 0.920003; 1e5 cycles at 100 MPa -> 1e5 /
    (10^12.010 / 1e6) = 1e5 / 1.02329e6 = 0.097724. The calculator used to
    fall back to its own default intercepts for a DNV curve (2.0596 and 0.1)."""
    from digitalmodel.signal_processing.signal_analysis.fatigue import (
        FatigueDamageCalculator,
    )

    params = _signal_e().get_curve_parameters()
    d46 = FatigueDamageCalculator("miners").calculate_damage(_hist46(), params)
    assert d46["total_damage"] == pytest.approx(D_E_AIR_46, rel=1e-9)
    d100 = FatigueDamageCalculator("miners").calculate_damage(
        pd.DataFrame({"range": [100.0], "mean": [0.0], "count": [1e5]}), params
    )
    assert d100["total_damage"] == pytest.approx(1e5 / (10**12.010 / 1e6), rel=1e-9)
    assert d100["total_damage"] == pytest.approx(0.097724, abs=1e-6)


def test_signal_user_bilinear_keeps_its_cut_off():
    """A user-built bilinear curve with a fatigue limit is unchanged."""
    from digitalmodel.signal_processing.signal_analysis.fatigue import SNCurve

    c = SNCurve(curve_type="bilinear", A=1e12, m=3.0, fatigue_limit=40.0)
    assert math.isinf(float(c.get_allowable_cycles(np.array([39.0]))[0]))


# -- signal_processing user-built bilinear curves (review r2) -------------------
#
# A = 1e12, m = 3, N_transition = 1e7: knee stress (1e12 / 1e7)^(1/3) =
# 46.416 MPa. Legacy (continuity) second intercept A2 = 1e7 x 46.416^5 =
# 2.15443e15, so 40 MPa -> 2.15443e15 / 40^5 = 2.15443e15 / 1.024e8 =
# 21,039,401 cycles. With the supplied A2 = 1e16 (opt-in): 1e16 / 1.024e8 =
# 97,656,250 cycles.

_USER = dict(A=1e12, m=3.0, A2=1e16, m2=5.0, N_transition=1e7)
N_USER_40_CONTINUITY = 21_039_401
N_USER_40_TABULATED = 97_656_250


def _user_curve(**kw):
    from digitalmodel.signal_processing.signal_analysis.fatigue import SNCurve

    return SNCurve("bilinear", **{**_USER, **kw})


def test_signal_user_bilinear_default_ignores_supplied_a2():
    """Default: the second intercept is derived for continuity at the knee and
    the supplied A2 is ignored, as before #2165."""
    c = _user_curve()
    n40 = float(c.get_allowable_cycles(np.array([40.0]))[0])
    assert n40 == pytest.approx(N_USER_40_CONTINUITY, rel=1e-7)
    a2_cont = 1e7 * (1e12 / 1e7) ** (5.0 / 3.0)
    assert n40 == pytest.approx(a2_cont / 40.0**5, rel=1e-12)


def test_signal_user_bilinear_default_is_continuous_at_the_knee():
    c = _user_curve()
    s_knee = (1e12 / 1e7) ** (1.0 / 3.0)
    n = c.get_allowable_cycles(np.array([s_knee * (1 - 1e-9), s_knee * (1 + 1e-9)]))
    assert n[0] == pytest.approx(1e7, rel=1e-7)
    assert n[1] == pytest.approx(1e7, rel=1e-7)


def test_signal_user_bilinear_opt_in_uses_supplied_a2():
    c = _user_curve(a2_mode="tabulated")
    n40 = float(c.get_allowable_cycles(np.array([40.0]))[0])
    assert n40 == pytest.approx(N_USER_40_TABULATED, rel=1e-12)


@pytest.mark.parametrize(
    "mode, n40",
    [("continuity", N_USER_40_CONTINUITY), ("tabulated", N_USER_40_TABULATED)],
)
def test_signal_user_bilinear_inverse_matches_forward(mode, n40):
    """get_stress_range is the inverse of get_allowable_cycles in both modes:
    the cycles at 40 MPa map back to 40 MPa, and 1e6 cycles (first segment)
    to (1e12 / 1e6)^(1/3) = 100 MPa."""
    c = _user_curve(a2_mode=mode)
    s = c.get_stress_range(np.array([float(n40), 1e6]))
    assert s[0] == pytest.approx(40.0, rel=1e-7)
    assert s[1] == pytest.approx(100.0, rel=1e-12)
    for stress in (20.0, 40.0, 46.0, 60.0, 150.0):
        n = c.get_allowable_cycles(np.array([stress]))
        assert c.get_stress_range(n)[0] == pytest.approx(stress, rel=1e-9)


def test_signal_user_bilinear_rejects_unknown_a2_mode():
    with pytest.raises(ValueError, match="a2_mode"):
        _user_curve(a2_mode="bogus").get_allowable_cycles(np.array([40.0]))


def test_signal_user_bilinear_tabulated_needs_a2():
    from digitalmodel.signal_processing.signal_analysis.fatigue import SNCurve

    c = SNCurve("bilinear", A=1e12, m=3.0, m2=5.0, a2_mode="tabulated")
    with pytest.raises(ValueError, match="A2"):
        c.get_allowable_cycles(np.array([40.0]))


def test_signal_dnv_curve_opts_in_to_the_tabulated_a2():
    """The DNV-RP-C203 path sets the opt-in: E air at 46 MPa stays at
    10^15.350 / 46^5 = 10,869,532 cycles, and the inverse maps it back."""
    c = _signal_e()
    assert c.params["a2_mode"] == "tabulated"
    assert float(c.get_allowable_cycles(46.0)) == pytest.approx(10_869_532, rel=1e-7)
    assert float(c.get_stress_range(np.array([N_E_AIR_46]))[0]) == pytest.approx(
        46.0, rel=1e-12
    )


# -- free span (finding 3) -----------------------------------------------------


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


def test_span_cp_40_mpa_is_finite():
    """F CP at 40 MPa, 0.5 Hz: N = 10^15.091 / 40^5 = 1.23310e15 / 1.024e8 =
    1.20420e7 cycles; D/yr = 0.5 x 3.15576e7 / 1.20420e7 = 1.31031; life =
    0.76318 years (was zero damage: the air cut-off 41.52 MPa was applied)."""
    from digitalmodel.subsea.pipeline.free_span import SpanFatigueDamage

    s = SpanFatigueDamage(_span_input(), 0.5, 40.0)
    assert s.allowable_cycles(40.0) == pytest.approx(10**15.091 / 40.0**5, rel=1e-12)
    assert s.allowable_cycles(40.0) == pytest.approx(12_042_039, rel=1e-6)
    assert s.damage_per_year() == pytest.approx(1.31031, abs=5e-6)
    assert s.fatigue_life_years() == pytest.approx(0.76318, abs=5e-6)


def test_span_cp_d_52_mpa_is_finite():
    """D CP at 52 MPa (below the old 52.63 MPa air cut-off): 10^15.606 / 52^5
    = 4.0365e15 / 3.8020e8 = 1.0617e7."""
    from digitalmodel.subsea.pipeline.free_span import SpanFatigueDamage

    s = SpanFatigueDamage(_span_input(sn_curve_class="D"), 0.5, 52.0)
    assert s.allowable_cycles(52.0) == pytest.approx(10**15.606 / 52.0**5, rel=1e-12)


def test_span_air_screening_cut_off_is_the_air_curve_own_limit():
    """In air the screening cut-off is the air curve's own tabulated limit
    (F: 41.52 MPa, equal to its knee stress 10^(4.855/3) = 41.52 MPa): 40 MPa
    gives zero damage; 42 MPa gives 10^11.855 / 42^3 = 9.666e6 cycles."""
    from digitalmodel.subsea.pipeline.free_span import SpanFatigueDamage
    from digitalmodel.subsea.pipeline.free_span.models import EnvironmentType

    air = _span_input(environment=EnvironmentType("in_air"))
    s = SpanFatigueDamage(air, 0.5, 40.0)
    assert s.screening_cut_off_mpa == pytest.approx(41.52)
    assert s.screening_cut_off_mpa == pytest.approx(s._curve.fatigue_limit)
    assert s.damage_per_year() == 0.0
    assert s.allowable_cycles(42.0) == pytest.approx(10**11.855 / 42.0**3, rel=1e-12)
    cp = SpanFatigueDamage(_span_input(), 0.5, 40.0)
    assert cp.screening_cut_off_mpa == 0.0


# -- user YAML (finding 4) -----------------------------------------------------


@pytest.fixture
def _fresh_standard_curves(monkeypatch):
    from digitalmodel.structural.fatigue.sn_curves import StandardSNCurves

    monkeypatch.setattr(StandardSNCurves, "_loaded_from_yaml", False)
    for attr in ("API_CURVES", "BS_CURVES", "AWS_CURVES", "CUSTOM_CURVES"):
        monkeypatch.setattr(
            StandardSNCurves, attr, dict(getattr(StandardSNCurves, attr))
        )
    monkeypatch.setattr(
        StandardSNCurves,
        "DNV_MULTISLOPE_CURVES",
        dict(StandardSNCurves.DNV_MULTISLOPE_CURVES),
    )
    return StandardSNCurves


def _data_dir(tmp_path, body):
    d = tmp_path / "fatigue"
    d.mkdir()
    (d / "sn_curves.yaml").write_text(body)
    return tmp_path


def test_yaml_dnv_curves_block_raises(tmp_path, monkeypatch, _fresh_standard_curves):
    """A deliberately reduced D curve (A = 1e11, m = 3) under standards.DNV.curves
    would give 1e11 / 100^3 = 100,000 cycles at 100 MPa; the loader used to
    replace it silently with 1,458,814 (14.59x). It now raises and names the
    block and the custom-curve route."""
    monkeypatch.setenv(
        "DIGITALMODEL_DATA_DIR",
        str(
            _data_dir(
                tmp_path,
                "standards:\n  DNV:\n    curves:\n      D: {A: 1.0e11, m: 3.0, fatigue_limit: 0.0}\n",
            )
        ),
    )
    with pytest.raises(ValueError) as exc:
        _fresh_standard_curves.get_curve("DNV", "D")
    msg = str(exc.value)
    assert "standards.DNV.curves" in msg
    assert "CUSTOM" in msg
    assert "register_custom_curve" in msg


def test_yaml_custom_curve_loads_and_is_used(
    tmp_path, monkeypatch, _fresh_standard_curves
):
    """The same reduced curve under its own name (standards.CUSTOM.curves) loads
    and gives 1e11 / 100^3 = 100,000 cycles at 100 MPa."""
    monkeypatch.setenv(
        "DIGITALMODEL_DATA_DIR",
        str(
            _data_dir(
                tmp_path,
                "standards:\n  CUSTOM:\n    curves:\n"
                "      D_reduced: {A: 1.0e11, m: 3.0, fatigue_limit: 0.0}\n",
            )
        ),
    )
    c = _fresh_standard_curves.get_curve("CUSTOM", "D_reduced")
    assert c.get_allowable_cycles(100.0) == pytest.approx(100_000, rel=1e-12)
    # the verified DNV curve is untouched
    assert _fresh_standard_curves.get_curve("DNV", "D").get_allowable_cycles(
        100.0
    ) == pytest.approx(1_458_814, rel=1e-6)
    # and the custom curve drives cumulative damage: 1e4 cycles -> 0.1
    from digitalmodel.structural.fatigue.damage_accumulation import (
        LinearDamageAccumulation,
    )

    res = LinearDamageAccumulation().calculate_damage(
        pd.DataFrame({"range": [100.0], "count": [1e4]}), c
    )
    assert res["total_damage"] == pytest.approx(0.1, rel=1e-12)


def test_register_custom_curve(_fresh_standard_curves):
    """The explicit API route: a bilinear user curve with its own name.
    A = 1e11, m = 3, A2 = 1e14, m2 = 5, knee at 1e7: knee stress
    (1e11 / 1e7)^(1/3) = 21.544 MPa; 10 MPa -> 1e14 / 1e5 = 1e9."""
    _fresh_standard_curves.register_custom_curve(
        "MY_D", A=1e11, m=3.0, A2=1e14, m2=5.0, knee_cycles=1e7
    )
    c = _fresh_standard_curves.get_curve("CUSTOM", "MY_D")
    assert c.get_allowable_cycles(100.0) == pytest.approx(1e5, rel=1e-12)
    assert c.get_allowable_cycles(10.0) == pytest.approx(1e9, rel=1e-12)
    with pytest.raises(ValueError):
        _fresh_standard_curves.register_custom_curve("BAD", A=1e11, m=3.0, m2=5.0)
