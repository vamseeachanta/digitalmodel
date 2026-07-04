"""Reconciliation tests (digitalmodel #1367).

Covers the resampling path explicitly: forecast and measured on DIFFERENT grids,
scored against a closed-form value.
"""

import numpy as np
import pytest

from digitalmodel.marine_ops.installation.go_no_go import DecisionState
from digitalmodel.motion_forecast.criteria import Criterion
from digitalmodel.motion_forecast.decision import _classify
from digitalmodel.motion_forecast.measured import MeasuredMotion
from digitalmodel.motion_forecast.models import DOF_NAMES, MotionForecast
from digitalmodel.motion_forecast.reconcile import (
    measured_status,
    overlap_error,
    seam_offset,
)


def _forecast(t, **dofs):
    base = {d: np.zeros(len(t)) for d in DOF_NAMES}
    base.update(dofs)
    return MotionForecast(t=np.asarray(t, float), dof=base,
                          origin_time=float(t[0]), horizon=float(t[-1] - t[0]))


def _measured(t, **dofs):
    base = {d: np.zeros(len(t)) for d in DOF_NAMES}
    base.update(dofs)
    return MeasuredMotion(t=np.asarray(t, float), dof=base)


def test_seam_offset_zero_when_identical_at_now():
    t = np.linspace(100.0, 160.0, 61)
    fc = _forecast(t, heave=0.1 * (t - 100.0))
    m = _measured(t, heave=0.1 * (t - 100.0))
    off = seam_offset(m, fc)
    assert off["heave"] == pytest.approx(0.0, abs=1e-9)


def test_seam_offset_raises_when_now_outside_forecast():
    fc = _forecast(np.linspace(100.0, 160.0, 61), heave=np.zeros(61))
    m = _measured(np.linspace(100.0, 200.0, 51), heave=np.zeros(51))  # now=200
    with pytest.raises(ValueError, match="no extrapolation|outside"):
        seam_offset(m, fc)


def test_overlap_error_zero_on_identical():
    t = np.linspace(100.0, 160.0, 61)
    fc = _forecast(t, heave=np.sin(t))
    m = _measured(t, heave=np.sin(t))
    err = overlap_error(m, fc)
    assert err["heave"].rmse == pytest.approx(0.0, abs=1e-12)
    assert err["heave"].bias == pytest.approx(0.0, abs=1e-12)


def test_overlap_error_resamples_across_different_grids():
    """Forecast (coarse) vs measured (fine) linear signals offset by b:
    linear interp is exact, so err == b everywhere -> rmse=|b|, bias=b, corr=1."""
    tf = np.linspace(100.0, 160.0, 7)     # coarse forecast grid
    tm = np.linspace(100.0, 160.0, 121)   # fine measured grid
    b = 0.25

    def line(t):
        return 0.1 * (t - 100.0)

    fc = _forecast(tf, heave=line(tf))
    m = _measured(tm, heave=line(tm) + b)
    err = overlap_error(m, fc)
    assert err["heave"].rmse == pytest.approx(b, abs=1e-9)
    assert err["heave"].bias == pytest.approx(b, abs=1e-9)
    assert err["heave"].correlation == pytest.approx(1.0, abs=1e-9)


def test_overlap_error_correlation_none_on_zero_variance():
    t = np.linspace(100.0, 160.0, 61)
    fc = _forecast(t, heave=np.full_like(t, 2.0))  # constant -> zero variance
    m = _measured(t, heave=np.full_like(t, 2.0))
    err = overlap_error(m, fc)
    assert err["heave"].correlation is None
    assert err["heave"].rmse == pytest.approx(0.0)


_VEL = Criterion(key="hd", label="Helideck", governing="helideck_heave_velocity",
                 caution=0.30, limit=0.40, unit="m/s", alpha=1.0, basis="",
                 poi_offset=(0.0, 0.0, 0.0))


def test_measured_status_classifies_latest_and_matches_classify():
    t = np.linspace(0.0, 20.0, 201)
    m = _measured(t, heave=0.6 * t)  # |dz/dt|=0.6 > 0.40 -> NO_GO
    md = measured_status(m, _VEL)
    assert md.state is DecisionState.NO_GO and md.display == "NO-GO"
    assert md.state is _classify(md.current_value, _VEL.caution, _VEL.limit)


def test_measured_status_nan_is_fail_closed():
    t = np.linspace(0.0, 20.0, 201)
    h = 0.1 * t
    h[-1] = np.nan  # latest sample bad
    m = _measured(t, heave=h)
    md = measured_status(m, _VEL)
    assert md.state is DecisionState.NO_GO
