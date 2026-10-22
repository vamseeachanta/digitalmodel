# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Numerical qualification with synthetic inputs and independent hand solutions."""

import importlib.util
import math
from pathlib import Path
from types import SimpleNamespace

import pandas as pd
import pytest

from digitalmodel.asset_integrity.assessment.level2_engine import Level2Engine


SPEC = importlib.util.spec_from_file_location(
    "local_loss_builder",
    Path(__file__).resolve().parents[2] / "scripts/reporting/build_pipeline_ffs_report.py",
)
builder = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(builder)


def component(pressure=800.0, od=12.0):
    """All values are synthetic test inputs, including the unit allowable RSF."""
    return SimpleNamespace(nominal_od_in=od, nominal_wt_in=1.0,
                           rsf_a=1.0, design_pressure_psi=pressure)


def grid(wall=0.6, spacing=1.0):
    """Two synthetic degraded rows with a full-wall background."""
    return pd.DataFrame([[1.0, wall], [1.0, wall]], index=[0.0, spacing])


def test_applicable_crossing_has_passing_and_failing_evidence():
    # A08: the former numerical root cannot qualify a missing width check.
    lam = 1.285 * 2 / math.sqrt(10)
    mt = math.sqrt(1 + 0.48 * lam**2)
    raw = Level2Engine("LML", 12, 1, 1, 1).evaluate(grid())
    assert raw["rsf"] == pytest.approx(.6 / (1 - .4 / mt))
    assert not raw["applicability"].ok
    result = builder.local_loss_limit_wall(grid(), component(), 1.0, 1000.0)
    assert result["status"] == "INAPPLICABLE"
    assert result["wall_in"] is None
    assert result["initial_bracket"] == result["final_bracket"] == []
    assert "circumferential" in result["reason"]


def test_crossing_outside_applicability_is_censored_without_invalid_evaluation(monkeypatch):
    """Long synthetic flaw: current Rt=1; any loss below reference is flagged.

    The existing engine's applicability exception at Rt=1 is the boundary.
    Demand=800 psi < the 1000 psi boundary rating, so no applicable root exists.
    """
    original = Level2Engine.evaluate
    evaluated = []

    def checked(engine, trial):
        result = original(engine, trial)
        assert result["applicability"].ok, "Unsupported wall was evaluated"
        evaluated.append(result["t_mm_in"])
        return result

    monkeypatch.setattr(Level2Engine, "evaluate", checked)
    result = builder.local_loss_limit_wall(grid(1.0, 100.0), component(), 1.0, 1000.0)
    assert result["status"] == "BOUNDED"
    assert result["wall_in"] == 1.0
    assert result["initial_bracket"][0]["wall_in"] == 1.0
    assert evaluated and min(evaluated) == 1.0


def test_current_geometry_outside_applicability_has_no_life_limit():
    """The synthetic long flaw is already outside the engine's supported range."""
    result = builder.local_loss_limit_wall(grid(0.6, 100.0), component(), 1.0, 1000.0)
    assert result["status"] == "INAPPLICABLE"
    assert result["wall_in"] is None
    assert result["initial_bracket"] == []


def test_target_above_current_capacity_has_no_future_crossing():
    # A08: no inferred crossing is allowed without a supported current wall.
    result = builder.local_loss_limit_wall(grid(), component(2000), 1.0, 1000.0)
    assert result["status"] == "INAPPLICABLE"
    assert result["wall_in"] is None


@pytest.mark.parametrize("pressure", [800.0, 1000.0])
def test_no_crossing_for_constant_short_flaw_rating(pressure):
    # A08: an axial short-flaw result does not supply the width criterion.
    result = builder.local_loss_limit_wall(grid(spacing=0.01), component(pressure), 1.0, 1000.0)
    assert result["status"] == "INAPPLICABLE"
    assert result["wall_in"] is None
    assert result["initial_bracket"] == []


def test_zero_wall_evidence_is_unqualified_for_nonunit_bulging():
    # A03/A08: zero measured wall cannot be used as a strength endpoint.
    frame = grid(1)
    frame.iloc[0, 1] = 0
    with pytest.raises(ValueError, match="finite positive"):
        builder.local_loss_limit_wall(frame, component(), 1.0, 1000.0)
    result = builder.local_loss_limit_wall(grid(), component(), 1.0, 1000.0)
    assert result["wall_in"] is None
    assert result["initial_bracket"] == []


def test_target_at_current_wall_is_boundary_censored():
    """At full wall with unit allowable RSF, synthetic P=P0=1000 psi."""
    result = builder.local_loss_limit_wall(grid(1.0, 100.0), component(1000.0), 1.0, 1000.0)
    assert result["status"] == "BOUNDARY"
    assert result["wall_in"] == 1.0


@pytest.mark.parametrize("pressure", [0.0, -1.0, float("nan"), float("inf")])
def test_nonpositive_or_nonfinite_target_rejected(pressure):
    """Numerical input contract: a positive finite pressure is required."""
    with pytest.raises(ValueError, match="positive finite"):
        builder.local_loss_limit_wall(grid(), component(pressure), 1.0, 1000.0)


def test_projection_domain_accounts_for_new_degraded_rows():
    """Uniform long grid stays supported only until the existing flaw threshold.

    The threshold is read from the existing engine definition; no independent
    acceptance limit is introduced. Just below it, every row becomes a flaw.
    """
    engine = Level2Engine("LML", 12.0, 1.0, 1.0, 1.0)
    # Synthetic spacing: one degraded row is supported, two exceed the cap.
    frame = grid(1.0, 30.0)
    lower, upper = engine.local_loss_projection_domain(frame)
    trial = frame.copy()
    trial[frame == 1.0] = lower
    assert engine.evaluate(trial)["applicability"].ok
    trial[frame == 1.0] = math.nextafter(lower, 0.0)
    assert not engine.evaluate(trial)["applicability"].ok
    assert upper == 1.0


def test_increasing_flaw_length_does_not_move_a_pressure_root_past_domain():
    """Pre-existing flaw rows fix geometry throughout the depletion interval."""
    engine = Level2Engine("LML", 12.0, 1.0, 1.0, 1.0)
    assert engine.local_loss_projection_domain(grid()) is None  # A08: circumferential check is missing.
    assert engine.local_loss_projection_domain(grid(0.6, 100.0)) is None


def test_flaw_extent_jump_is_reported_as_a_bound():
    # A08: crossing a segmentation threshold cannot supply missing applicability.
    result = builder.local_loss_limit_wall(grid(0.95), component(990), 1.0, 1000.0)
    assert result["status"] == "INAPPLICABLE"
    assert result["wall_in"] is None
    assert result["final_bracket"] == []


@pytest.mark.parametrize("where", ["endpoint", "interior"])
def test_pressure_ordering_failure_prevents_a_crossing(monkeypatch, where):
    # Retain independent numerical fault injection on a positive synthetic domain.
    # A08 prevents using an unqualified physical wall to exercise the bisection.
    def evaluate(wall):
        pressure = 1100 if where == "interior" else wall * 1000
        return {"wall_in": wall, "pressure_psi": pressure, "flaw_length_in": 1}
    low = {"wall_in": .1, "pressure_psi": 1100 if where == "endpoint" else 100}
    high = {"wall_in": 1, "pressure_psi": 1000}
    evidence = {"domain_wall_in": [.1, 1], "monotonic": True}
    def finish(status, wall, reason):
        return {**evidence, "status": status, "wall_in": wall, "reason": reason}
    result = builder._bisect_local_loss(evaluate, evidence, finish, low, high, 800)
    assert result["status"] == "NON_MONOTONIC"
    assert result["wall_in"] is None
    assert result["monotonic"] is False


def test_search_preserves_input_grid():
    """The projection changes a private trial grid, preserving received readings."""
    frame = grid()
    before = frame.copy(deep=True)
    builder.local_loss_limit_wall(frame, component(), 1.0, 1000.0)
    pd.testing.assert_frame_equal(frame, before)


@pytest.mark.parametrize("wall", [0.0, -1.0, float("inf"), float("nan")])
def test_invalid_wall_readings_rejected(wall):
    """Nonpositive readings or missing all readings cannot qualify a projection."""
    frame = pd.DataFrame([[wall]])
    with pytest.raises(ValueError, match="finite positive"):
        builder.local_loss_limit_wall(frame, component(), 1.0, 1000.0)


def test_missing_readings_do_not_create_false_flaw_rows():
    # A03: unmeasured background cannot be silently excluded.
    frame = grid()
    frame.iloc[0, 0] = math.nan
    with pytest.raises(ValueError, match="complete coverage"):
        builder.local_loss_limit_wall(frame, component(), 1.0, 1000.0)


@pytest.mark.parametrize("dtype", ["int64", "float32"])
def test_projection_supports_numeric_grid_dtypes(dtype):
    # A08: retain the full-wall boundary; do not search unqualified reduced wall.
    frame = grid(1).astype(dtype)
    result = builder.local_loss_limit_wall(frame, component(), 1.0, 1000.0)
    assert result["status"] == "BOUNDED"
    assert result["wall_in"] == 1
    assert str(frame.dtypes.iloc[0]) == dtype
