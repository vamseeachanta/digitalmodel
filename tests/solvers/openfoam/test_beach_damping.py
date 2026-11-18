# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Hand calculations for the coefficient policy in issue #2046 (2026).

All rates, timesteps and caps below are synthetic arithmetic inputs, not
empirical tuning values. For each cell, s_eff = min(s, c*r), r = 1/dt.
The zero and unit bounds are the policy's nonnegative-rate and c < 1 domain.
"""

import importlib.util
from pathlib import Path

import pytest


@pytest.fixture(scope="module")
def beach():
    # Exercise the dependency-free policy without importing unrelated solvers.
    path = (
        Path(__file__).resolve().parents[3]
        / "src/digitalmodel/solvers/openfoam/beach_damping.py"
    )
    spec = importlib.util.spec_from_file_location("beach_damping", path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_local_steps_control_each_cell_despite_small_global_step(beach):
    """c=1/2, r=(1,4,20): caps=(1/2,2,10); min with s=(4,4,4).

    Thus s_eff=(1/2,2,4), two of three cells clip, max(s_eff/r)=1/2.
    The global dt=1/1000 would give cap=500 and must not be used for LTS.
    """
    result = beach.limit_beach_rates(
        [4, 4, 4], time_scheme="localEuler", max_damping_fraction=0.5,
        r_delta_t=[1, 4, 20], delta_t=0.001,
    )
    assert result.effective_rates == (0.5, 2, 4)
    assert result.clipped_cells == 2
    assert result.zone_cells == 3
    assert result.clip_fraction == 2 / 3
    assert result.max_effective_fraction == 0.5
    assert result.time_scheme == "localEuler"


def test_euler_small_step_leaves_rates_unchanged(beach):
    """dt=1/8, c=1/2: c/dt=4; s=(0,2,4) is entirely within the cap."""
    result = beach.limit_beach_rates(
        [0, 2, 4], time_scheme="Euler", max_damping_fraction=0.5, delta_t=0.125,
    )
    assert result.effective_rates == (0, 2, 4)
    assert result.clipped_cells == 0
    assert result.clip_fraction == 0
    assert result.max_effective_fraction == 0.5


def test_euler_large_step_clips_and_cap_is_required_input(beach):
    """dt=2, c=1/4: cap=1/8; min((0,1/8,1),1/8)=(0,1/8,1/8)."""
    result = beach.limit_beach_rates(
        [0, 0.125, 1], time_scheme="Euler", max_damping_fraction=0.25, delta_t=2,
    )
    assert result.effective_rates == (0, 0.125, 0.125)
    assert result.clipped_cells == 1  # Equality at the cap is not clipping.
    with pytest.raises(TypeError):
        beach.limit_beach_rates([1], time_scheme="Euler", delta_t=1)


def test_empty_partition_has_no_fraction_or_maximum(beach):
    """No zone cells means count=0; both ratios over that set are undefined."""
    result = beach.limit_beach_rates(
        [], time_scheme="localEuler", max_damping_fraction=0.5, r_delta_t=[],
    )
    assert result.effective_rates == ()
    assert result.clipped_cells == result.zone_cells == 0
    assert result.clip_fraction is None
    assert result.max_effective_fraction is None


@pytest.mark.parametrize("cap", [0, -1, 1, 2, float("nan"), float("inf")])
def test_invalid_policy_is_rejected(beach, cap):
    with pytest.raises(ValueError, match="max_damping_fraction"):
        beach.limit_beach_rates(
            [1], time_scheme="Euler", max_damping_fraction=cap, delta_t=1,
        )


@pytest.mark.parametrize("rate", [-1, float("nan"), float("inf")])
def test_invalid_raw_rate_is_rejected(beach, rate):
    with pytest.raises(ValueError, match="rates"):
        beach.limit_beach_rates(
            [rate], time_scheme="Euler", max_damping_fraction=0.5, delta_t=1,
        )


@pytest.mark.parametrize("reciprocal", [0, -1, float("nan"), float("inf")])
def test_invalid_local_step_is_rejected(beach, reciprocal):
    with pytest.raises(ValueError, match="r_delta_t"):
        beach.limit_beach_rates(
            [1], time_scheme="localEuler", max_damping_fraction=0.5,
            r_delta_t=[reciprocal],
        )


@pytest.mark.parametrize("step", [None, 0, -1, float("nan"), float("inf")])
def test_invalid_global_step_is_rejected(beach, step):
    with pytest.raises(ValueError, match="delta_t"):
        beach.limit_beach_rates(
            [1], time_scheme="Euler", max_damping_fraction=0.5, delta_t=step,
        )


def test_local_euler_never_falls_back_when_field_is_missing(beach):
    with pytest.raises(ValueError, match="r_delta_t"):
        beach.limit_beach_rates(
            [1], time_scheme="localEuler", max_damping_fraction=0.5, delta_t=1,
        )


def test_local_field_length_must_match_zone_ordered_rates(beach):
    with pytest.raises(ValueError, match="same length"):
        beach.limit_beach_rates(
            [1, 2], time_scheme="localEuler", max_damping_fraction=0.5,
            r_delta_t=[1],
        )


def test_euler_does_not_accept_a_stale_local_field(beach):
    with pytest.raises(ValueError, match="r_delta_t"):
        beach.limit_beach_rates(
            [1], time_scheme="Euler", max_damping_fraction=0.5,
            delta_t=1, r_delta_t=[100],
        )


def test_other_time_schemes_are_not_silently_treated_as_euler(beach):
    with pytest.raises(ValueError, match="time_scheme"):
        beach.limit_beach_rates(
            [1], time_scheme="steadyState", max_damping_fraction=0.5, delta_t=1,
        )


def test_renderer_requires_the_same_explicit_policy(beach):
    """The supplied c=1/4 must survive rendering as a C++ scalar literal."""
    text = beach.render_beach_limiter(max_damping_fraction=0.25)
    assert "const scalar beachCap = 0.25;" in text
    assert "@MAX_DAMPING_FRACTION@" not in text
    with pytest.raises(ValueError, match="max_damping_fraction"):
        beach.render_beach_limiter(max_damping_fraction=1)


def test_renderer_has_global_count_reductions_before_master_output(beach):
    """MPI clip fraction is sum(clipped)/sum(cells), not mean(rank ratios)."""
    text = beach.render_beach_limiter(max_damping_fraction=0.5)
    clipped = text.index("reduce(beachClipped, sumOp<label>());")
    count = text.index("reduce(beachCount, sumOp<label>());")
    maximum = text.index("reduce(beachMaxFraction, maxOp<scalar>());")
    master = text.index("if (Pstream::master())")
    assert max(clipped, count, maximum) < master
    assert "scalar(beachClipped)/scalar(beachCount)" in text
    assert "NOT_EVALUATED" in text
