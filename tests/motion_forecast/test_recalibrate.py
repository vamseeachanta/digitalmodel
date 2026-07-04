"""Recalibration tests (digitalmodel #1360)."""

import numpy as np
import pytest

from digitalmodel.motion_forecast.measured import MeasuredMotion
from digitalmodel.motion_forecast.models import DOF_NAMES, MotionForecast
from digitalmodel.motion_forecast.recalibrate import fit_correction, holdout_report
from digitalmodel.motion_forecast.skill import SkillRecord


def _fc(t, **d):
    base = {k: np.zeros(len(t)) for k in DOF_NAMES}
    base.update(d)
    return MotionForecast(t=np.asarray(t, float), dof=base,
                          origin_time=float(t[0]), horizon=float(t[-1] - t[0]))


def _meas(t, **d):
    base = {k: np.zeros(len(t)) for k in DOF_NAMES}
    base.update(d)
    return MeasuredMotion(t=np.asarray(t, float), dof=base)


def _affine_record(t, gain, bias, seed=None, noise=0.0):
    fc_heave = np.sin(0.5 * t)
    m_heave = gain * fc_heave + bias
    if noise:
        m_heave = m_heave + np.random.default_rng(seed).normal(0, noise, size=t.shape)
    return SkillRecord(_fc(t, heave=fc_heave), _meas(t, heave=m_heave))


def test_fit_recovers_injected_gain_bias():
    t = np.linspace(0.0, 50.0, 501)
    rec = _affine_record(t, gain=2.0, bias=0.5)
    corr = fit_correction([rec])
    assert corr.gain["heave"] == pytest.approx(2.0, abs=1e-9)
    assert corr.bias["heave"] == pytest.approx(0.5, abs=1e-9)


def test_holdout_reduces_rmse_on_affine_structure():
    ts = [np.linspace(0.0, 50.0, 251) + 100 * i for i in range(4)]
    recs = [_affine_record(t, gain=2.0, bias=0.5) for t in ts]
    rep = holdout_report(recs)["heave"]
    assert rep.rmse_after < rep.rmse_before          # affine error is removable
    assert rep.rmse_after == pytest.approx(0.0, abs=1e-9)
    # correlation is invariant under a positive-gain affine correction
    assert rep.correlation_after == pytest.approx(rep.correlation_before, abs=1e-9)


def test_noise_only_is_near_identity_and_not_worsened():
    ts = [np.linspace(0.0, 50.0, 251) + 100 * i for i in range(4)]
    recs = [_affine_record(t, gain=1.0, bias=0.0, seed=i, noise=0.2)
            for i, t in enumerate(ts)]
    corr = fit_correction(recs)
    assert corr.gain["heave"] == pytest.approx(1.0, abs=0.05)   # ~identity
    rep = holdout_report(recs)["heave"]
    assert rep.rmse_after <= rep.rmse_before * 1.2             # not worsened


def test_zero_variance_dof_falls_back_to_bias_only():
    t = np.linspace(0.0, 50.0, 201)
    # sway forecast constant 0, measured constant 0.3 -> gain=1, bias=0.3
    rec = SkillRecord(_fc(t, sway=np.zeros_like(t)), _meas(t, sway=np.full_like(t, 0.3)))
    corr = fit_correction([rec])
    assert corr.gain["sway"] == 1.0
    assert corr.bias["sway"] == pytest.approx(0.3)
