# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent synthetic geometry checks; no survey accuracy limits are implied.

Geometry basis: NOAA Field Procedures Manual (2020), section 1.5.6.2,
pages 26–28. Fixture lengths, speeds and noise are deliberately chosen synthetic
inputs. Expected values are hand calculations below, independent of generation.
pytest.approx tolerances concern floating-point evaluation, not survey acceptance.
"""

import math

import pytest

from digitalmodel.subsea.survey_qa import (
    ReciprocalSwaths,
    correct_roll,
    estimate_latency,
    estimate_pitch,
    estimate_roll,
    estimate_yaw,
    synthetic_roll_swaths,
)


def test_roll_hand_calculation():
    """At y=(-10,0,10), dz=(-2,0,2): slope=.2=2*tan(r).

    Thus r=atan(.1), a=0 and fit RMS=0, without calling the generator.
    """
    pair = ReciprocalSwaths((-10, 0, 10), (19, 20, 21), (21, 20, 19))
    result = estimate_roll(pair)
    assert result.roll_deg == pytest.approx(math.degrees(math.atan(0.1)))
    assert result.vertical_offset_m == pytest.approx(0)
    assert result.fit_rmse_m == pytest.approx(0)
    assert result.sample_count == 3
    corrected = correct_roll(pair, result.roll_deg)
    assert corrected.forward_depth_m == pytest.approx((20, 20, 20))
    assert corrected.reverse_depth_m == pytest.approx((20, 20, 20))


def test_roll_fit_separates_vertical_offset_and_noise():
    """y=(-10,0,10), dz=(0,1.4,2): mean=1.133333..., slope=.1.

    Centered residuals=(-2/15,4/15,-2/15); RMS=sqrt(8)/15.
    The intercept must not be removed by an angular correction.
    """
    pair = ReciprocalSwaths((-10, 0, 10), (20, 21.4, 22), (20, 20, 20))
    result = estimate_roll(pair)
    assert result.roll_deg == pytest.approx(math.degrees(math.atan(0.05)))
    assert result.vertical_offset_m == pytest.approx(17 / 15)
    assert result.fit_rmse_m == pytest.approx(math.sqrt(8) / 15)
    corrected = correct_roll(pair, result.roll_deg)
    mismatch = [
        a - b
        for a, b in zip(
            corrected.forward_depth_m, corrected.reverse_depth_m, strict=True
        )
    ]
    assert mismatch == pytest.approx((1, 1.4, 1))


@pytest.mark.parametrize("roll_deg", [-0.5, 0, 0.5])
def test_synthetic_roll_recovery(roll_deg):
    """Synthetic +/-0.5 deg: z_f=20+y*tan(r), z_r=20-y*tan(r).

    Subtracting gives 2*y*tan(r), so the independently prescribed r is recovered.
    """
    pair = synthetic_roll_swaths((-10, -5, 0, 5, 10), depth_m=20, roll_deg=roll_deg)
    assert estimate_roll(pair).roll_deg == pytest.approx(roll_deg, abs=1e-12)


def test_generator_matches_hand_depths_with_supplied_noise():
    """tan(r)=.05: at y=(-10,10), noiseless z_f=(19.5,20.5).

    z_r=(20.5,19.5); adding supplied noise gives the literal arrays below.
    """
    pair = synthetic_roll_swaths(
        (-10, 10),
        depth_m=20,
        roll_deg=math.degrees(math.atan(0.05)),
        forward_noise_m=(0.1, -0.2),
        reverse_noise_m=(-0.1, 0.2),
    )
    assert pair.forward_depth_m == pytest.approx((19.6, 20.3))
    assert pair.reverse_depth_m == pytest.approx((20.4, 19.7))


@pytest.mark.parametrize("shift,expected", [(4, 1), (-4, -1), (0, 0)])
def test_latency_hand_calculation(shift, expected):
    """Synthetic speeds 6 and 2 m/s: latency = shift/(6-2) s."""
    assert estimate_latency(shift, first_speed_m_s=6, second_speed_m_s=2) == expected
    assert estimate_latency(-shift, first_speed_m_s=2, second_speed_m_s=6) == expected


def test_latency_issue_worked_value():
    """Synthetic .32 m shift / (6-2) m/s = .08 s (issue example input)."""
    assert estimate_latency(0.32, first_speed_m_s=6, second_speed_m_s=2) == 0.08


@pytest.mark.parametrize("shift", [-4, 0, 4])
def test_pitch_hand_calculation(shift):
    """Reciprocal shift +/-4 m at depth 20 m: p=atan(+/-4/40)."""
    assert estimate_pitch(shift, depth_m=20) == pytest.approx(
        math.degrees(math.atan(shift / 40))
    )


@pytest.mark.parametrize("shift", [-2, 0, 2])
def test_yaw_hand_calculation(shift):
    """Feature dx=+/-2 m; reported cross-track separation 20 m: atan(dx/20)."""
    assert estimate_yaw(
        shift, first_across_track_m=-10, second_across_track_m=10
    ) == pytest.approx(math.degrees(math.atan(shift / 20)))
    assert estimate_yaw(
        -shift, first_across_track_m=10, second_across_track_m=-10
    ) == pytest.approx(math.degrees(math.atan(shift / 20)))


@pytest.mark.parametrize(
    "pair",
    [
        ((), (), ()),
        ((0,), (20,), (20,)),
        ((1, 1), (20, 20), (20, 20)),
        ((-1, 1), (20,), (20, 20)),
        ((-1, 1), (20, 20), (20,)),
        ((-1, math.nan), (20, 20), (20, 20)),
        ((-1, 1), (math.inf, 20), (20, 20)),
        ((-1, 1), (20, 20), (20, math.nan)),
        ((-1, 1), (0, 20), (20, 20)),
        ((-1, 1), (20, 20), (20, -1)),
    ],
)
def test_invalid_swaths_are_refused(pair):
    """No empty, unidentifiable, nonfinite or nonpositive-depth fit is allowed."""
    with pytest.raises(ValueError):
        ReciprocalSwaths(*pair)


@pytest.mark.parametrize(
    "kwargs",
    [
        {"depth_m": 0, "roll_deg": 0},
        {"depth_m": math.nan, "roll_deg": 0},
        {"depth_m": 20, "roll_deg": 90},
        {"depth_m": 20, "roll_deg": -90},
        {"depth_m": 20, "roll_deg": math.inf},
        {"depth_m": 20, "roll_deg": 0, "forward_noise_m": (0,)},
        {"depth_m": 20, "roll_deg": 0, "reverse_noise_m": (0, math.nan)},
        {"depth_m": 20, "roll_deg": 0, "forward_noise_m": (-30, 0)},
    ],
)
def test_invalid_generator_inputs(kwargs):
    """Bounds are mathematical domain constraints, not empirical limits."""
    with pytest.raises(ValueError):
        synthetic_roll_swaths((-10, 10), **kwargs)


@pytest.mark.parametrize(
    "function,args,kwargs",
    [
        (estimate_latency, (1,), {"first_speed_m_s": 2, "second_speed_m_s": 2}),
        (estimate_latency, (1,), {"first_speed_m_s": -1, "second_speed_m_s": 2}),
        (estimate_latency, (1,), {"first_speed_m_s": 0, "second_speed_m_s": 2}),
        (estimate_latency, (math.nan,), {"first_speed_m_s": 4, "second_speed_m_s": 2}),
        (estimate_latency, (1,), {"first_speed_m_s": math.inf, "second_speed_m_s": 2}),
        (estimate_pitch, (1,), {"depth_m": 0}),
        (estimate_pitch, (1,), {"depth_m": -20}),
        (estimate_pitch, (math.inf,), {"depth_m": 20}),
        (estimate_pitch, (1,), {"depth_m": math.nan}),
        (estimate_yaw, (1,), {"first_across_track_m": 2, "second_across_track_m": 2}),
        (
            estimate_yaw,
            (math.nan,),
            {"first_across_track_m": -2, "second_across_track_m": 2},
        ),
        (
            estimate_yaw,
            (1,),
            {"first_across_track_m": -2, "second_across_track_m": math.inf},
        ),
    ],
)
def test_invalid_displacement_observations(function, args, kwargs):
    """An unidentifiable geometry or nonfinite observation is a clear error."""
    with pytest.raises(ValueError):
        function(*args, **kwargs)


def test_immutable_normalized_swaths():
    """Input lists cannot mutate the validated coordinates after construction."""
    positions = [-10, 10]
    pair = ReciprocalSwaths(positions, [20, 20], [20, 20])
    positions[0] = 10
    assert pair.across_track_m == (-10, 10)


def test_wrong_roll_correction_increases_mismatch():
    """For dz=(-2,0,2), opposite correction doubles each mismatch."""
    pair = ReciprocalSwaths((-10, 0, 10), (19, 20, 21), (21, 20, 19))
    corrected = correct_roll(pair, -math.degrees(math.atan(0.1)))
    assert [
        a - b
        for a, b in zip(
            corrected.forward_depth_m, corrected.reverse_depth_m, strict=True
        )
    ] == pytest.approx((-4, 0, 4))


def test_worked_example_runs_as_written():
    """Execute the documentation's Python block; independent oracle tests are above."""
    from pathlib import Path

    document = (
        Path(__file__).resolve().parents[3]
        / "docs/domains/subsea/survey_qa/patch_test_example.md"
    )
    code = document.read_text().split("```python\n", 1)[1].split("```", 1)[0]
    exec(compile(code, "patch_test_example", "exec"), {})
