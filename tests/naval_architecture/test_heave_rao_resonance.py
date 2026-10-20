"""Closed-form checks of the simplified oscillator, not a hull qualification.

Source: Mechanics Map, 2021 online edition, section 15.5, "Viscous Damped
Harmonic Forced Vibrations", magnification-factor equation and Figure 2.
https://eng.libretexts.org/Bookshelves/Mechanical_Engineering/Mechanics_Map_(Moore_et_al.)/15:_Vibrations_with_One_Degree_of_Freedom/15.5:_Viscous_Damped_Harmonic_Forced_Vibrations

All inputs below are synthetic mathematical cases, not measured properties.
"""

import pytest

from digitalmodel.naval_architecture.seakeeping import simple_heave_rao


@pytest.mark.parametrize("frequency", [0.5, 1.0, 2.0])
def test_undamped_resonance_raises(frequency):
    """At equal frequencies r=1 and zeta=0: (1-1)^2+(2*0*1)^2=0.

    The cited magnification equation has no finite value; Figure 2 identifies
    the unbounded response. Synthetic equal frequencies exercise scale changes.
    """
    with pytest.raises(ValueError, match="unbounded.*undamped resonance"):
        simple_heave_rao(frequency, frequency, damping_ratio=0.0)


@pytest.mark.parametrize(
    ("damping_ratio", "expected"),
    [(0.05, 10.0), (1e-12, 5e11), (1e-200, 5e199)],
)
def test_damped_resonance_is_finite(damping_ratio, expected):
    """Hand calculation using the cited equation at r=1: H=1/(2*zeta).

    Synthetic zeta=0.05 gives 1/0.1=10; 1e-12 gives 1/(2e-12)=5e11;
    1e-200 gives 1/(2e-200)=5e199. The last case detects squared-denominator
    underflow. Relative tolerance 1e-14 is a numerical test criterion.
    """
    assert simple_heave_rao(1.0, 1.0, damping_ratio) == pytest.approx(
        expected, rel=1e-14
    )


def test_undamped_off_resonance():
    """Hand calculation at synthetic r=2, zeta=0: H=1/abs(1-4)=1/3.

    Source: the cited magnification equation; relative tolerance 1e-14 is a
    numerical test criterion, not an engineering acceptance limit.
    """
    assert simple_heave_rao(2.0, 1.0, 0.0) == pytest.approx(1 / 3, rel=1e-14)


def test_undamped_near_resonance_is_not_capped():
    """Hand calculation at r=1+2^-40, zeta=0 gives H=2^80/(2^41+1).

    Here r^2=1+2^-39+2^-80; hence abs(1-r^2)=2^-39+2^-80.
    Inputs are synthetic. Relative tolerance 1e-12 allows cancellation from
    floating-point squaring; it is not an engineering acceptance limit.
    """
    expected = 2**80 / (2**41 + 1)
    assert simple_heave_rao(1.0 + 2**-40, 1.0, 0.0) == pytest.approx(
        expected, rel=1e-12
    )
