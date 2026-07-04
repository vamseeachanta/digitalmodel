"""Forecast-skill tests (digitalmodel #1360)."""

import numpy as np
import pytest

from digitalmodel.motion_forecast.measured import MeasuredMotion
from digitalmodel.motion_forecast.models import DOF_NAMES, MotionForecast
from digitalmodel.motion_forecast.skill import (
    SkillRecord,
    aggregate_skill,
    error_vs_lead_time,
)


def _fc(t, **d):
    base = {k: np.zeros(len(t)) for k in DOF_NAMES}
    base.update(d)
    return MotionForecast(t=np.asarray(t, float), dof=base,
                          origin_time=float(t[0]), horizon=float(t[-1] - t[0]))


def _meas(t, **d):
    base = {k: np.zeros(len(t)) for k in DOF_NAMES}
    base.update(d)
    return MeasuredMotion(t=np.asarray(t, float), dof=base)


def test_record_requires_overlap():
    fc = _fc(np.linspace(0, 50, 51), heave=np.zeros(51))
    m = _meas(np.linspace(100, 150, 51), heave=np.zeros(51))
    with pytest.raises(ValueError, match="overlap"):
        SkillRecord(fc, m)


def test_error_vs_lead_time_recovers_injected_growth():
    t = np.linspace(0.0, 100.0, 201)
    fc = _fc(t, heave=np.zeros_like(t))          # forecast predicts 0
    m = _meas(t, heave=0.01 * t)                 # drift grows with lead (= t)
    curve = error_vs_lead_time([SkillRecord(fc, m)], "heave", n_bins=5)
    centers = sorted(curve)
    rmses = [curve[c] for c in centers]
    assert rmses == sorted(rmses)                # monotonic increase with lead
    assert rmses[-1] > rmses[0]


def test_aggregate_skill_pools_residuals_not_mean_of_rmse():
    # record A: 10 samples, residual 1 ; record B: 90 samples, residual 3
    tA = np.linspace(0.0, 9.0, 10)
    tB = np.linspace(0.0, 89.0, 90)
    recs = [
        SkillRecord(_fc(tA, heave=np.zeros_like(tA)), _meas(tA, heave=np.ones_like(tA))),
        SkillRecord(_fc(tB, heave=np.zeros_like(tB)), _meas(tB, heave=np.full_like(tB, 3.0))),
    ]
    agg = aggregate_skill(recs)["heave"]
    pooled = np.sqrt((10 * 1.0 + 90 * 9.0) / 100.0)   # 2.863...
    assert agg.rmse == pytest.approx(pooled, rel=1e-9)
    assert agg.rmse != pytest.approx(2.0)             # != mean-of-rmse (1+3)/2
    assert agg.n_samples == 100
