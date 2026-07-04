"""Tests for the pore-pressure prediction module (issue #1034).

Closed-form / license-free. Covers the Eaton and Bowers predictors, the
P10/P50/P90 ensemble, calibration, the safe-drilling-window logic, and the
config-driven ``router`` (the engine basename wiring is applied by the
integrator, so the router is exercised directly).
"""

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


def test_router_without_fracture_skips_window(tmp_path):
    cfg = _example_cfg(tmp_path)
    cfg["pore_pressure"].pop("fracture", None)
    cfg["pore_pressure"].pop("window", None)
    out = router(cfg)
    result = out["pore_pressure"]["result"]
    assert "pore_pressure" in result
    assert "drilling_window" not in result
