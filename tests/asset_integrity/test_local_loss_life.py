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
    """Independent inversion of the documented legacy simplified relation.

    The simplified form implemented by Level2Engine._folias_factor:
    D=12-2*1=10 in, L=2*1=2 in, lambda=1.285*2/sqrt(10),
    M=sqrt(1+0.48*lambda**2). Synthetic P0=1000 psi, demand=800 psi,
    allowable RSF=1, hence r=0.8 and t=r*(M-1)/(M-r) in.
    This algebraic inverse is independent of the bisection under test.
    """
    lam = 1.285 * 2 / math.sqrt(10)  # Source: Level2Engine._folias_factor.
    mt = math.sqrt(1 + 0.48 * lam**2)  # Same source; no new coefficient.
    expected = 0.8 * (mt - 1) / (mt - 0.8)
    result = builder.local_loss_limit_wall(grid(), component(), 1.0, 1000.0)
    assert result["status"] == "CROSSING"
    assert result["wall_in"] == pytest.approx(expected)
    assert result["monotonic"] is True
    assert result["qualification_scope"] == "lambda-range only"
    for bracket in (result["initial_bracket"], result["final_bracket"]):
        low, high = bracket
        assert high["applicability"]["ok"]
        assert low["pressure_psi"] < 800 <= high["pressure_psi"]
    assert result["final_bracket"][0]["applicability"]["ok"]
    assert math.nextafter(result["final_bracket"][0]["wall_in"], math.inf) == result["wall_in"]


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
    """Demand=2000 psi exceeds the synthetic uncorroded ceiling of 1000 psi."""
    result = builder.local_loss_limit_wall(grid(), component(2000.0), 1.0, 1000.0)
    assert result["status"] == "TARGET_UNREACHABLE"
    assert result["wall_in"] == 0.6  # Current demand already fails: zero life.


@pytest.mark.parametrize("pressure", [800.0, 1000.0])
def test_no_crossing_for_constant_short_flaw_rating(pressure):
    """M=1 short-flaw branch gives RSF=1 for every positive wall.

    The zero-wall numerical singularity must not manufacture a pressure root.
    Synthetic demand is at or below the constant 1000 psi rating.
    """
    result = builder.local_loss_limit_wall(grid(spacing=0.01), component(pressure), 1.0, 1000.0)
    assert result["status"] == "INAPPLICABLE"
    assert result["wall_in"] is None
    assert result["initial_bracket"][0]["pressure_psi"] == 1000.0
    assert not result["initial_bracket"][0]["applicability"]["ok"]
    assert result["initial_bracket"][0]["endpoint_kind"] == "right-hand limit"


def test_zero_wall_evidence_is_unqualified_for_nonunit_bulging():
    """An evaluated zero wall is not the short-flaw right-hand limit."""
    result = builder.local_loss_limit_wall(grid(), component(), 1.0, 1000.0)
    endpoint = result["initial_bracket"][0]
    assert endpoint["wall_in"] == 0.0
    assert not endpoint["applicability"]["ok"]
    assert endpoint["applicability"]["flags"] == ["ZERO_WALL_UNQUALIFIED"]
    assert endpoint["endpoint_kind"] == "evaluated"
    assert result["qualification_scope"] == "lambda-range only"
    assert all(point["wall_in"] > 0 for point in result["final_bracket"])


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
    assert engine.local_loss_projection_domain(grid()) == (0.0, 0.6)
    assert engine.local_loss_projection_domain(grid(0.6, 100.0)) is None


def test_flaw_extent_jump_is_reported_as_a_bound():
    """Synthetic target=990 psi lies between the two one-sided pressures.

    The existing flaw heuristic is t<0.9*nominal (not a code criterion).
    At t=0.9 in, one-row M=sqrt(1+0.48*(1.285/sqrt(10))**2);
    just below it, two-row M=sqrt(1+0.48*(1.285*2/sqrt(10))**2).
    P=1000*0.9/(1-0.1/M) brackets 990 psi. The source for M is the
    simplified implementation in Level2Engine._folias_factor.
    No continuous root exists at that geometry jump.
    """
    result = builder.local_loss_limit_wall(grid(0.95), component(990.0), 1.0, 1000.0)
    assert result["status"] == "BOUNDED"
    assert result["wall_in"] == 0.9
    assert result["final_bracket"][0]["pressure_psi"] < 990
    assert result["final_bracket"][1]["pressure_psi"] > 990


@pytest.mark.parametrize("where", ["endpoint", "interior"])
def test_pressure_ordering_failure_prevents_a_crossing(monkeypatch, where):
    """Fault injection: a unit-RSF bump violates the required pressure ordering.

    For the synthetic grid, P(0)=0 < P(0.3) < P(0.6) < P0=1000 psi.
    Replacing either lower/interior RSF by 1 makes its pressure exceed P(0.6).
    """
    original = Level2Engine.evaluate

    def broken(engine, frame):
        result = original(engine, frame)
        if result["t_mm_in"] == (0.0 if where == "endpoint" else 0.3):
            result["rsf"] = 1.0
        return result

    monkeypatch.setattr(Level2Engine, "evaluate", broken)
    result = builder.local_loss_limit_wall(grid(), component(), 1.0, 1000.0)
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
    with pytest.raises(ValueError, match="positive finite geometry"):
        builder.local_loss_limit_wall(frame, component(), 1.0, 1000.0)


def test_missing_readings_do_not_create_false_flaw_rows():
    """A missing synthetic background reading leaves the two-row hand case."""
    frame = grid()
    frame.iloc[0, 0] = math.nan
    actual = builder.local_loss_limit_wall(frame, component(), 1.0, 1000.0)
    expected = builder.local_loss_limit_wall(grid(), component(), 1.0, 1000.0)
    assert actual == expected


@pytest.mark.parametrize("dtype", ["int64", "float32"])
def test_projection_supports_numeric_grid_dtypes(dtype):
    """The two-row hand inverse above also applies after a uniform grid depletes.

    The target is below the flaw-extent jump; both rows are degraded at the
    continuous root. Integer or single-precision storage must not quantize trials.
    """
    frame = grid(1.0).astype(dtype)
    mt = math.sqrt(1 + 0.48 * (1.285 * 2 / math.sqrt(10))**2)  # Level2Engine._folias_factor; see hand case.
    expected = 0.8 * (mt - 1) / (mt - 0.8)
    result = builder.local_loss_limit_wall(frame, component(), 1.0, 1000.0)
    assert result["status"] == "CROSSING"
    assert result["wall_in"] == pytest.approx(expected)
    assert str(frame.dtypes.iloc[0]) == dtype
