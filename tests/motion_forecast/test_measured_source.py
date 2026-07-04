"""Measured-motion ingest tests (digitalmodel #1367)."""

import numpy as np
import pytest

from digitalmodel.motion_forecast.measured import MeasuredMotion
from digitalmodel.motion_forecast.measured_source import (
    MeasuredMotionSource,
    SyntheticMMS,
    from_csv,
)
from digitalmodel.motion_forecast.models import DOF_NAMES


def _truth(t):
    return MeasuredMotion(
        t=t, dof={d: (np.sin(t) if d == "heave" else np.zeros_like(t))
                  for d in DOF_NAMES})


def test_synthetic_noiseless_reproduces_truth_and_is_source():
    t = np.linspace(0.0, 30.0, 121)
    src = SyntheticMMS(_truth(t), noise_std=0.0)
    assert isinstance(src, MeasuredMotionSource)  # runtime Protocol
    m = src.read()
    assert np.allclose(m.dof["heave"], np.sin(t))


def test_synthetic_noise_is_deterministic():
    t = np.linspace(0.0, 30.0, 121)
    a = SyntheticMMS(_truth(t), noise_std=0.05, seed=7).read()
    b = SyntheticMMS(_truth(t), noise_std=0.05, seed=7).read()
    assert np.array_equal(a.dof["heave"], b.dof["heave"])  # same seed -> identical
    assert not np.allclose(a.dof["heave"], np.sin(t))       # noise applied


def test_csv_round_trip(tmp_path):
    t = np.linspace(0.0, 5.0, 26)
    truth = _truth(t)
    p = tmp_path / "mms.csv"
    header = "t," + ",".join(DOF_NAMES)
    rows = [header]
    for i in range(t.size):
        rows.append(",".join(
            [f"{t[i]:.6f}"] + [f"{truth.dof[d][i]:.6f}" for d in DOF_NAMES]))
    p.write_text("\n".join(rows) + "\n")

    m = from_csv(str(p))
    assert np.allclose(m.t, t, atol=1e-6)
    assert np.allclose(m.dof["heave"], truth.dof["heave"], atol=1e-6)


def test_csv_missing_column_raises(tmp_path):
    p = tmp_path / "bad.csv"
    p.write_text("t,heave\n0,0\n1,1\n")
    with pytest.raises(ValueError, match="missing columns"):
        from_csv(str(p))
