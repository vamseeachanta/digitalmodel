"""Slice 3, NOAA FPM (2020), 1.5.6.2; signed analytical examples."""

from dataclasses import replace
from math import asin, degrees

import pytest

from digitalmodel.subsea.survey_qa import (
    Offsets,
    Seabed,
    estimate_latency,
    estimate_yaw,
)


@pytest.mark.parametrize("direction", [1, -1])
@pytest.mark.parametrize("latency", [-0.08, 0.0, 0.08])
def test_two_speed_latency(survey, direction, latency):
    """Chosen speeds 2 and 4 m/s: dx=(4-2)*0.08=0.16 m;
    dx/(4-2)=0.08 s. Direction multiplies both dx and velocity difference.
    """
    bed = Seabed(50, 0.2, 0)
    a = survey(bed=bed, direction=direction, offsets=Offsets(latency_s=latency))
    b = survey(
        bed=bed, direction=direction, speed=4, offsets=Offsets(latency_s=latency)
    )
    assert estimate_latency(b, a) == pytest.approx(latency, abs=1e-12)
    assert estimate_latency(a, b) == pytest.approx(latency, abs=1e-12)


def test_latency_independent_hand_shift(survey):
    """Profiles shifted by v*0.08: slow +0.16 m, fast +0.32 m.
    Their 0.16 m difference over 2 m/s gives 0.08 s, without injected rotations.
    """
    bed = Seabed(50, 0.2, 0)
    a, b = survey(bed=bed), survey(bed=bed, speed=4)
    a = replace(a, xyz_m=tuple((x + 0.16, y, z) for x, y, z in a.xyz_m), truth_xyz_m=())
    b = replace(b, xyz_m=tuple((x + 0.32, y, z) for x, y, z in b.xyz_m), truth_xyz_m=())
    assert estimate_latency(a, b) == pytest.approx(0.08)


@pytest.mark.parametrize("direction", [1, -1])
@pytest.mark.parametrize("angle", [-1.5, 0.0, 1.5])
def test_parallel_feature_yaw(survey, direction, angle):
    """Target at Y=0 from tracks +/-20 m: local b differ by 40 m.
    dx=-40*sin(yaw), dy=40*cos(yaw), so atan(-dx/dy)=yaw.
    """
    bed = Seabed(50, 0, 0, (0, 0, 5))
    a = survey(
        bed=bed, direction=direction, track_y=-20, offsets=Offsets(yaw_deg=angle)
    )
    b = survey(bed=bed, direction=direction, track_y=20, offsets=Offsets(yaw_deg=angle))
    assert estimate_yaw(a, b) == pytest.approx(angle, abs=1e-10)
    assert estimate_yaw(b, a) == pytest.approx(angle, abs=1e-10)


def test_yaw_independent_feature_displacement(survey):
    """Hand target returns: dx=-2 m, dy=sqrt(40^2-2^2) m.
    A 40 m beam separation rotated with sin(yaw)=2/40 gives asin(0.05).
    Only measured target positions supply the estimate, not synthetic truth.
    """
    bed = Seabed(50, 0, 0, (0, 0, 5))
    a = survey(bed=bed, track_y=-20)
    b = survey(bed=bed, track_y=20)
    transverse = (40**2 - 2**2) ** 0.5
    pa, pb = list(a.xyz_m), list(b.xyz_m)
    pa[a.feature_index] = (-1, -20 + transverse / 2, 45)
    pb[b.feature_index] = (1, 20 - transverse / 2, 45)
    a = replace(a, xyz_m=tuple(pa), truth_xyz_m=())
    b = replace(b, xyz_m=tuple(pb), truth_xyz_m=())
    assert estimate_yaw(a, b) == pytest.approx(degrees(asin(0.05)))
