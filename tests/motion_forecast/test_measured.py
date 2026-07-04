"""MeasuredMotion representation tests (digitalmodel #1367)."""

import numpy as np
import pytest

from digitalmodel.motion_forecast.derived import compute_governing
from digitalmodel.motion_forecast.measured import MeasuredMotion


def _measured(t, **dofs):
    base = {d: np.zeros_like(t) for d in
            ("surge", "sway", "heave", "roll", "pitch", "yaw")}
    base.update(dofs)
    return MeasuredMotion(t=t, dof=base)


def test_now_is_last_sample():
    t = np.linspace(100.0, 160.0, 61)  # absolute seconds
    m = _measured(t, heave=np.sin(t))
    assert m.now == 160.0
    assert m.at_now()["heave"] == pytest.approx(np.sin(160.0))


def test_window_is_trailing_slice():
    t = np.linspace(100.0, 160.0, 61)
    m = _measured(t, heave=t)
    w = m.window(20.0)
    assert w.t[0] == pytest.approx(140.0) and w.now == 160.0
    assert w.dof["heave"][0] == pytest.approx(140.0)


def test_duck_types_through_derived():
    t = np.linspace(100.0, 160.0, 121)
    m = _measured(t, heave=0.5 * (t - 100.0))  # dz/dt = 0.5
    gs = compute_governing(m, "helideck_heave_velocity")
    assert gs.unit == "m/s"
    assert np.allclose(gs.values, 0.5, atol=1e-9)


def test_non_monotonic_t_raises():
    t = np.array([0.0, 3.0, 1.0, 2.0, 4.0])  # scrambled clock
    with pytest.raises(ValueError, match="ascending"):
        _measured(t, heave=t)


def test_validation_missing_dof_and_shape():
    t = np.linspace(0, 10, 11)
    with pytest.raises(ValueError, match="missing DOFs"):
        MeasuredMotion(t=t, dof={"heave": np.zeros_like(t)})
    bad = {d: np.zeros_like(t) for d in
           ("surge", "sway", "heave", "roll", "pitch", "yaw")}
    bad["heave"] = np.zeros(5)
    with pytest.raises(ValueError, match="shape"):
        MeasuredMotion(t=t, dof=bad)
