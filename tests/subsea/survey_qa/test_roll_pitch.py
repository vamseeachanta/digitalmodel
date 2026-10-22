"""Slice 2: NOAA FPM (2020), 1.5.6.2 line geometries; independent derivations."""

from dataclasses import replace
from math import atan, degrees, tan, radians

import pytest

from digitalmodel.subsea.survey_qa import Offsets, Seabed, estimate_pitch, estimate_roll


@pytest.mark.parametrize("angle", [-0.5, 0.0, 0.5])
def test_reciprocal_roll(survey, angle):
    """At r=0.5 deg, m+ = tan(0.5 deg), m- = -tan(0.5 deg).
    Least-squares cross-track slope difference is 2*tan(r), hence atan(dm/2)=r.
    Angles are synthetic inputs specified by the worked example, issue #2955.
    """
    first = survey(offsets=Offsets(roll_deg=angle))
    second = survey(direction=-1, offsets=Offsets(roll_deg=angle))
    assert estimate_roll(first, second) == pytest.approx(angle, abs=1e-10)
    assert estimate_roll(second, first) == pytest.approx(angle, abs=1e-10)


def test_roll_independent_hand_plane(survey):
    """Hand plane z+=50+0.01*y, z-=50-0.01*y: dm=0.02,
    tan(r)=0.01 so r=atan(0.01) degrees. No generator rotation supplies data.
    """
    a, b = survey(), survey(direction=-1)
    a = replace(
        a, xyz_m=tuple((x, y, 50 + 0.01 * y) for x, y, z in a.xyz_m), truth_xyz_m=()
    )
    b = replace(
        b, xyz_m=tuple((x, y, 50 - 0.01 * y) for x, y, z in b.xyz_m), truth_xyz_m=()
    )
    assert estimate_roll(a, b) == pytest.approx(degrees(atan(0.01)))


@pytest.mark.parametrize("angle", [-1.0, 0.0, 1.0])
def test_reciprocal_pitch(survey, angle):
    """At common observed D=50 m, dx=2*50*tan(1 deg)=1.74550649 m;
    atan(dx/(2D)) recovers 1 deg. Test tolerance is numerical, not field accuracy.
    """
    bed = Seabed(50, 0.2, 0)
    a = survey(bed=bed, offsets=Offsets(pitch_deg=angle))
    b = survey(bed=bed, direction=-1, offsets=Offsets(pitch_deg=angle))
    assert estimate_pitch(a, b) == pytest.approx(angle, abs=1e-10)
    assert estimate_pitch(b, a) == pytest.approx(angle, abs=1e-10)


def test_pitch_independent_hand_shift(survey):
    """Construct x+=x+0.02*z, x-=x-0.02*z. At D=50, dx=2 m;
    pitch=atan(2/(2*50))=atan(0.02). This is a direct hand profile.
    """
    bed = Seabed(50, 0.2, 0)
    a, b = survey(bed=bed), survey(bed=bed, direction=-1)
    a = replace(
        a, xyz_m=tuple((x + 0.02 * z, y, z) for x, y, z in a.xyz_m), truth_xyz_m=()
    )
    b = replace(
        b, xyz_m=tuple((x - 0.02 * z, y, z) for x, y, z in b.xyz_m), truth_xyz_m=()
    )
    assert estimate_pitch(a, b) == pytest.approx(degrees(atan(0.02)))
    assert 2 * 50 * tan(radians(1)) == pytest.approx(1.74550649, abs=1e-8)
