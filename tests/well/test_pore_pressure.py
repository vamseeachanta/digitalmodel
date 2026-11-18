"""Tests for the pore-pressure prediction module (issue #1034).

Closed-form / license-free. Covers the Eaton and Bowers predictors, the
P10/P50/P90 ensemble, calibration, the safe-drilling-window logic, and the
config-driven ``router`` (the engine basename wiring is applied by the
integrator, so the router is exercised directly).
"""

import json
from pathlib import Path

import pytest
import yaml

from digitalmodel.well.drilling.pore_pressure import (
    PorePressureDistribution,
    bowers_pore_pressure,
    calibrate,
    combine_methods,
    eaton_pore_pressure,
    router,
    safe_drilling_window,
)

REPO_ROOT = Path(__file__).resolve().parents[2]
INPUT_YML = REPO_ROOT / "examples" / "workflows" / "pore-pressure" / "input.yml"


# ---------------------------------------------------------------------------
# Eaton
# ---------------------------------------------------------------------------


def test_eaton_resistivity_normal_trend_gives_normal_pressure():
    # R_obs == R_normal -> ratio 1 -> Pp == Pn.
    pp = eaton_pore_pressure(
        overburden_emw_ppg=18.0,
        normal_pressure_emw_ppg=8.6,
        observed=2.0,
        normal=2.0,
    )
    assert pp == pytest.approx(8.6)


def test_eaton_resistivity_below_trend_is_overpressured():
    # R_obs < R_normal (undercompaction) -> pore pressure above normal.
    pp = eaton_pore_pressure(
        overburden_emw_ppg=18.0,
        normal_pressure_emw_ppg=8.6,
        observed=1.0,
        normal=2.0,
    )
    assert 8.6 < pp < 18.0


def test_eaton_pressure_bounded_by_overburden():
    # Very low resistivity ratio still cannot exceed the overburden.
    pp = eaton_pore_pressure(
        overburden_emw_ppg=18.0,
        normal_pressure_emw_ppg=8.6,
        observed=0.001,
        normal=2.0,
    )
    assert pp < 18.0


def test_eaton_sonic_uses_inverse_ratio():
    # Sonic: dt_obs > dt_normal (slower) -> overpressure.
    pp_over = eaton_pore_pressure(
        overburden_emw_ppg=18.0,
        normal_pressure_emw_ppg=8.6,
        observed=120.0,  # dt_obs us/ft, above trend
        normal=90.0,     # dt_normal
        log_type="sonic",
    )
    pp_normal = eaton_pore_pressure(
        overburden_emw_ppg=18.0,
        normal_pressure_emw_ppg=8.6,
        observed=90.0,
        normal=90.0,
        log_type="sonic",
    )
    assert pp_normal == pytest.approx(8.6)
    assert pp_over > pp_normal


def test_eaton_rejects_bad_inputs():
    with pytest.raises(ValueError):
        eaton_pore_pressure(
            overburden_emw_ppg=8.0,  # not above normal
            normal_pressure_emw_ppg=8.6,
            observed=1.0,
            normal=2.0,
        )
    with pytest.raises(ValueError):
        eaton_pore_pressure(
            overburden_emw_ppg=18.0,
            normal_pressure_emw_ppg=8.6,
            observed=-1.0,
            normal=2.0,
        )


# ---------------------------------------------------------------------------
# Bowers
# ---------------------------------------------------------------------------


def test_bowers_higher_velocity_gives_lower_pore_pressure():
    # Faster rock -> more effective stress -> lower pore pressure.
    fast = bowers_pore_pressure(
        overburden_emw_ppg=18.0, tvd_ft=12000.0, velocity_ft_s=10000.0
    )
    slow = bowers_pore_pressure(
        overburden_emw_ppg=18.0, tvd_ft=12000.0, velocity_ft_s=7000.0
    )
    assert slow > fast


def test_bowers_velocity_at_v0_is_full_overpressure():
    pp = bowers_pore_pressure(
        overburden_emw_ppg=18.0, tvd_ft=12000.0, velocity_ft_s=5000.0, v0_ft_s=5000.0
    )
    assert pp == pytest.approx(18.0)


def test_bowers_from_sonic_dt_matches_velocity():
    dt = 125.0  # us/ft -> 8000 ft/s
    by_dt = bowers_pore_pressure(
        overburden_emw_ppg=18.0, tvd_ft=12000.0, sonic_dt_us_ft=dt
    )
    by_v = bowers_pore_pressure(
        overburden_emw_ppg=18.0, tvd_ft=12000.0, velocity_ft_s=1.0e6 / dt
    )
    assert by_dt == pytest.approx(by_v)


def test_bowers_requires_exactly_one_velocity_input():
    with pytest.raises(ValueError):
        bowers_pore_pressure(overburden_emw_ppg=18.0, tvd_ft=12000.0)
    with pytest.raises(ValueError):
        bowers_pore_pressure(
            overburden_emw_ppg=18.0,
            tvd_ft=12000.0,
            velocity_ft_s=8000.0,
            sonic_dt_us_ft=125.0,
        )


# ---------------------------------------------------------------------------
# Ensemble distribution
# ---------------------------------------------------------------------------


def test_distribution_orders_p10_p50_p90():
    dist = combine_methods({"eaton": 12.0, "bowers": 13.0}, model_sigma_ppg=0.6)
    assert dist.p10 < dist.p50 < dist.p90
    assert dist.p50 == pytest.approx(12.5)  # mean of the two methods


def test_single_method_still_has_model_uncertainty():
    dist = combine_methods({"eaton": 12.0}, model_sigma_ppg=0.6)
    assert dist.between_method_sigma == pytest.approx(0.0)
    assert dist.sigma == pytest.approx(0.6)  # model uncertainty alone, not zero
    assert dist.p10 < dist.p50 < dist.p90


def test_disagreeing_methods_widen_the_band():
    tight = combine_methods({"eaton": 12.0, "bowers": 12.1}, model_sigma_ppg=0.5)
    wide = combine_methods({"eaton": 11.0, "bowers": 14.0}, model_sigma_ppg=0.5)
    assert wide.sigma > tight.sigma
    assert (wide.p90 - wide.p10) > (tight.p90 - tight.p10)


def test_combine_methods_rejects_empty():
    with pytest.raises(ValueError):
        combine_methods({})


# ---------------------------------------------------------------------------
# Calibration
# ---------------------------------------------------------------------------


def test_shift_calibration_matches_measured_p50_preserves_spread():
    dist = combine_methods({"eaton": 12.0, "bowers": 13.0}, model_sigma_ppg=0.6)
    cal = calibrate(dist, measured_emw_ppg=12.4, mode="shift")
    assert cal.p50 == pytest.approx(12.4)
    assert cal.sigma == pytest.approx(dist.sigma)  # shift does not change spread
    assert cal.calibration["mode"] == "shift"


def test_scale_calibration_hits_measured_and_scales_spread():
    dist = combine_methods({"eaton": 12.0, "bowers": 13.0}, model_sigma_ppg=0.6)
    cal = calibrate(dist, measured_emw_ppg=12.5, mode="scale")
    assert cal.p50 == pytest.approx(12.5)
    factor = 12.5 / dist.p50
    assert cal.sigma == pytest.approx(dist.sigma * factor)


def test_calibration_none_is_noop():
    dist = combine_methods({"eaton": 12.0}, model_sigma_ppg=0.6)
    assert calibrate(dist, measured_emw_ppg=99.0, mode="none") is dist


# ---------------------------------------------------------------------------
# Safe drilling window
# ---------------------------------------------------------------------------


def _pp_and_frac():
    pp = PorePressureDistribution(p50=12.0, sigma=0.6)
    frac = PorePressureDistribution(p50=15.5, sigma=0.5)
    return pp, frac


def test_window_uses_conservative_bounds():
    pp, frac = _pp_and_frac()
    w = safe_drilling_window(pp, frac)
    # Floor = pore P90 (high), ceiling = fracture P10 (low).
    assert w.static_low_emw_ppg == pytest.approx(pp.p90)
    assert w.static_high_emw_ppg == pytest.approx(frac.p10)
    assert w.static_feasible


def test_dynamic_window_is_narrower_by_ecd():
    pp, frac = _pp_and_frac()
    w = safe_drilling_window(pp, frac, ecd_delta_ppg=0.5)
    assert w.dynamic_high_emw_ppg == pytest.approx(w.static_high_emw_ppg - 0.5)
    assert w.dynamic_low_emw_ppg == pytest.approx(w.static_low_emw_ppg)
    assert w.dynamic_width_ppg < w.static_width_ppg


def test_margins_tighten_the_window():
    pp, frac = _pp_and_frac()
    base = safe_drilling_window(pp, frac)
    tight = safe_drilling_window(pp, frac, kick_margin_ppg=0.3, loss_margin_ppg=0.3)
    assert tight.static_low_emw_ppg == pytest.approx(base.static_low_emw_ppg + 0.3)
    assert tight.static_high_emw_ppg == pytest.approx(base.static_high_emw_ppg - 0.3)


def test_infeasible_window_when_pore_exceeds_fracture():
    pp = PorePressureDistribution(p50=15.0, sigma=0.6)
    frac = PorePressureDistribution(p50=15.2, sigma=0.5)
    w = safe_drilling_window(pp, frac, ecd_delta_ppg=0.5)
    assert not w.dynamic_feasible
    assert w.recommended_mud_weight_ppg() is None


def test_recommended_mud_weight_inside_dynamic_window():
    pp, frac = _pp_and_frac()
    w = safe_drilling_window(pp, frac, ecd_delta_ppg=0.5)
    rec = w.recommended_mud_weight_ppg()
    assert w.dynamic_low_emw_ppg <= rec <= w.dynamic_high_emw_ppg


# ---------------------------------------------------------------------------
# Router (config-driven; engine basename wiring applied by integrator)
# ---------------------------------------------------------------------------


def _example_cfg(tmp_path):
    cfg = yaml.safe_load(INPUT_YML.read_text())
    cfg["_config_dir_path"] = str(tmp_path)
    return cfg


def test_router_runs_example_and_writes_summary(tmp_path):
    cfg = _example_cfg(tmp_path)
    out = router(cfg)

    result = out["pore_pressure"]["result"]
    pp = result["pore_pressure"]
    assert pp["p10_emw_ppg"] < pp["p50_emw_ppg"] < pp["p90_emw_ppg"]
    # Two methods configured in the example.
    assert set(pp["methods"]) == {"eaton", "bowers"}
    # Calibrated to the offset-well measured point.
    assert pp["p50_emw_ppg"] == pytest.approx(12.4)
    assert pp["calibration"]["mode"] == "shift"

    window = result["drilling_window"]
    assert window["dynamic_high_emw_ppg"] < window["static_high_emw_ppg"]

    summary_json = Path(out["pore_pressure"]["summary"]["summary_json"])
    assert summary_json.is_file()
    assert list(tmp_path.rglob("*.json")) == [summary_json]
    saved = json.loads(summary_json.read_text(encoding="utf-8"))
    assert saved["calculation"] == "pore_pressure"
    assert saved["result"] == result

    distribution_fields = {
        "p10_emw_ppg", "p50_emw_ppg", "p90_emw_ppg", "sigma_ppg",
        "between_method_sigma_ppg", "model_sigma_ppg",
    }
    for name in ("pore_pressure", "fracture"):
        assert distribution_fields <= saved["result"][name].keys()
        assert all(saved["result"][name][key] is not None for key in distribution_fields)
    window_fields = {
        "static_low_emw_ppg", "static_high_emw_ppg", "static_width_ppg",
        "dynamic_low_emw_ppg", "dynamic_high_emw_ppg", "dynamic_width_ppg",
        "nominal_low_emw_ppg", "nominal_high_emw_ppg", "ecd_delta_ppg",
        "kick_margin_ppg", "loss_margin_ppg", "recommended_mud_weight_ppg",
    }
    saved_window = saved["result"]["drilling_window"]
    assert window_fields <= saved_window.keys()
    assert all(saved_window[key] is not None for key in window_fields)


def test_router_summary_lists_drilling_window_exclusions(tmp_path):
    out = router(_example_cfg(tmp_path))
    summary = out["pore_pressure"]["summary"]
    written_summary = json.loads(Path(summary["summary_json"]).read_text())

    assert written_summary["not_covered"] == [
        "collapse (shear failure)",
        "weak bedding planes",
        "natural fractures",
        "chemical or time-dependent effects",
    ]
    assert summary["not_covered"] == written_summary["not_covered"]


def test_router_without_fracture_skips_window(tmp_path):
    cfg = _example_cfg(tmp_path)
    cfg["pore_pressure"].pop("fracture", None)
    cfg["pore_pressure"].pop("window", None)
    out = router(cfg)
    result = out["pore_pressure"]["result"]
    assert "pore_pressure" in result
    assert "drilling_window" not in result


@pytest.mark.parametrize(
    "fracture, ecd, static_width, dynamic_width, static_ok, dynamic_ok, centre",
    [(14.0, 0.75, 2.5, 1.75, True, True, 11.375),
     (14.0, 2.5, 2.5, 0.0, True, False, None),
     (14.0, 3.0, 2.5, -0.5, True, False, None),
     (10.0, 0.75, -1.5, -2.25, False, False, None)],
)
def test_safe_window_widths_from_hand_interval_arithmetic(
    fracture, ecd, static_width, dynamic_width, static_ok, dynamic_ok, centre,
):
    """Specified interval bookkeeping, not an empirical uncertainty model.

    Definition: floor = pore bound + supplied kick margin; ceiling = fracture
    bound - supplied loss margin. With zero spread, both quantiles equal the
    supplied scalar; no normal-quantile coefficient is needed.
    Pore = 10 ppg, kick = .5 ppg, loss = 1 ppg: floor = 10.5.
    For fracture 14: ceiling = 13, static width = 2.5; ECD .75 gives
    dynamic ceiling 12.25, width 1.75, midpoint 11.375 ppg.
    ECD 2.5 closes the dynamic interval; ECD 3 gives width -.5.
    Fracture 10 gives ceiling 9, static width -1.5 and dynamic width -2.25.
    Relative/absolute 1e-12 checks arithmetic including zero-width closure;
    it is not a prescribed operational tolerance. All inputs are synthetic.
    """
    pore = PorePressureDistribution(p50=10.0, sigma=0.0, methods={"specified": 10.0})
    frac = PorePressureDistribution(
        p50=fracture, sigma=0.0, methods={"specified": fracture},
    )
    result = safe_drilling_window(
        pore, frac, ecd_delta_ppg=ecd, kick_margin_ppg=0.5, loss_margin_ppg=1.0,
    )
    assert result.static_low_emw_ppg == 10.5
    assert result.dynamic_low_emw_ppg == 10.5
    assert result.static_width_ppg == pytest.approx(static_width, rel=1e-12, abs=1e-12)
    assert result.dynamic_width_ppg == pytest.approx(dynamic_width, rel=1e-12, abs=1e-12)
    assert result.static_feasible is static_ok
    assert result.dynamic_feasible is dynamic_ok
    if centre is None:
        assert result.recommended_mud_weight_ppg() is None
    else:
        assert result.recommended_mud_weight_ppg() == pytest.approx(centre, rel=1e-12)
    saved = result.as_dict()
    assert saved["static_width_ppg"] == pytest.approx(static_width, abs=1e-12)
    assert saved["dynamic_width_ppg"] == pytest.approx(dynamic_width, abs=1e-12)
