"""Slice 1. Expectations follow the right-handed matrices in the module derivation.

All geometry and noise amplitudes here are chosen synthetic inputs, not standard
coefficients. Trigonometric identities give exact expected displacements.
"""

from math import cos, radians, sin

import pytest

from digitalmodel.subsea.survey_qa import Offsets, Seabed


@pytest.mark.parametrize(
    "bed", [Seabed(50, 0, 0), Seabed(50, 0.2, 0.1), Seabed(50, 0, 0, (0, 0, 5))]
)
@pytest.mark.parametrize("direction", [1, -1])
@pytest.mark.parametrize("speed", [2, 4])
def test_zero_offsets_exact(survey, bed, direction, speed):
    """Hand grid: X=start+direction*speed*t, Y=direction*b;
    Z=50+sx*X+sy*Y, except the point (0,0) is 5 m shallower.
    This expected grid is independent of the generator's truth field.
    """
    swath = survey(bed=bed, direction=direction, speed=speed)
    expected = []
    start = -20 if direction == 1 else 20
    for time in range(21):
        for beam in (-20, -10, 0, 10, 20):
            x, y = start + direction * speed * time, direction * beam
            z = 50 + bed.slope_x * x + bed.slope_y * y
            if bed.feature is not None and (x, y) == (0, 0):
                z -= 5
            expected.append((x, y, z))
    assert swath.xyz_m == tuple(expected)
    assert swath.xyz_m == swath.truth_xyz_m
    if bed.feature is not None:
        assert swath.feature_index is not None
        assert swath.xyz_m[swath.feature_index] == (0, 0, 45)


@pytest.mark.parametrize("direction", [1, -1])
@pytest.mark.parametrize("name", ["roll_deg", "pitch_deg", "yaw_deg", "latency_s"])
def test_offset_signs_and_magnitude(survey, direction, name):
    """At starboard b=20, d=50: Rx raises z by b sin(r)+d(cos(r)-1);
    Ry advances x by d sin(p); Rz moves x by -b sin(y); latency by v*tau.
    Reciprocal ground x/y signs reverse; positive depth stays down.
    """
    value = 0.08 if name == "latency_s" else 1.0
    base = survey(direction=direction)
    got = survey(direction=direction, offsets=Offsets(**{name: value}))
    x, y, z = base.xyz_m[-1]
    angle = radians(value)
    expected = {
        "roll_deg": (
            x,
            y + direction * (20 * (cos(angle) - 1) - 50 * sin(angle)),
            20 * sin(angle) + 50 * cos(angle),
        ),
        "pitch_deg": (x + direction * 50 * sin(angle), y, 50 * cos(angle)),
        "yaw_deg": (
            x - direction * 20 * sin(angle),
            y + direction * 20 * (cos(angle) - 1),
            z,
        ),
        "latency_s": (x + direction * 2 * value, y, z),
    }[name]
    assert got.xyz_m[-1] == pytest.approx(expected)


def test_seeded_depth_noise(survey):
    a = survey(noise=0.02)
    assert a.xyz_m == survey(noise=0.02).xyz_m
    assert a.xyz_m != survey(noise=0.02, seed=8).xyz_m
    assert a.truth_xyz_m == survey().truth_xyz_m
