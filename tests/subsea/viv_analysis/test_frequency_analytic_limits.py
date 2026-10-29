# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent continuum oracles for uniform tubes, without calibration data.

All dimensional inputs are synthetic choices, not measured material properties.
The hand calculations in the test docstrings supply the expected values; no
production section properties or eigenvalue tables supply an oracle. Modes
1, 2, 3 and 6 sample integer harmonics through the supported upper limit.
The relative tolerance of 1e-12 allows floating-point roundoff only.

Minimal test dependencies: pytest, pytest-asyncio (repository configuration)
and numpy (VIV imports). The existing VIV suite needs its additional runtime
dependencies; these tests do not import the fatigue or legacy analysis paths.
"""

from dataclasses import replace
from math import pi, sqrt

import pytest

from digitalmodel.subsea.viv_analysis import (
    BoundaryCondition,
    FluidProperties,
    FrequencyCalculator,
    MaterialProperties,
    TubularMember,
)


REL_TOL = 1e-12


@pytest.fixture
def synthetic_tube():
    """Define the synthetic inputs used by the hand calculations below.

    Chosen inputs: outer diameter 0.10 m, wall 0.02 m, length 10 m,
    density 5000 kg/m^3 and modulus 20000 MPa. Thus the outer and inner
    radii are 0.05 m and 0.03 m. The prescribed tension 80000*pi N gives
    a wave speed of exactly 100 m/s for the derived mass of 8*pi kg/m.
    """
    return TubularMember(
        name="synthetic-tube",
        length=10.0,
        outer_diameter=0.10,
        wall_thickness=0.02,
        material=MaterialProperties(
            youngs_modulus=20_000.0,
            density=5000.0,
            name="synthetic-material",
        ),
        boundary_condition=BoundaryCondition.PINNED_PINNED,
        top_tension=80_000 * pi,
    )


@pytest.mark.parametrize("mode", [1, 2, 3, 6])
def test_pinned_beam_frequency_from_energy(synthetic_tube, mode):
    """Derive the dry pinned-beam frequency from exact modal energy.

    Independent hand calculation, with r=0.03 m and R=0.05 m:
    A = integral(s ds dtheta) = pi*(R^2-r^2) = pi/625 m^2.
    I = integral(s^3*sin(theta)^2 ds dtheta)
      = pi*(R^4-r^4)/4 = 17*pi/12500000 m^4.
    mu = rho*A = 5000*pi/625 = 8*pi kg/m.
    E = 20000 MPa = 20000000000 Pa, so EI/mu = 3400 m^4/s^2.

    For displacement q(t)*sin(n*pi*x/L), pinned ends have zero
    displacement and bending moment. Integrating the kinetic and bending
    energies gives modal mass M=mu*L/2 and stiffness
    K=EI*(n*pi/L)^4*L/2. Hence omega^2=K/M and, for L=10 m,
    f_n = omega/(2*pi) = n^2*pi*sqrt(34)/20 Hz.
    """
    assert synthetic_tube.cross_sectional_area == pytest.approx(pi / 625, rel=REL_TOL)
    assert synthetic_tube.second_moment_of_area == pytest.approx(
        17 * pi / 12_500_000, rel=REL_TOL, abs=0,
    )
    assert synthetic_tube.mass_per_length == pytest.approx(8 * pi, rel=REL_TOL)

    result = FrequencyCalculator().calculate_natural_frequency(
        synthetic_tube, mode=mode, include_added_mass=False,
    )

    expected_hz = mode**2 * pi * sqrt(34) / 20
    assert result.frequency == pytest.approx(expected_hz, rel=REL_TOL)
    assert result.includes_added_mass is False
    assert result.effective_mass_ratio == pytest.approx(1.0, rel=REL_TOL)


@pytest.mark.parametrize("mode", [1, 2, 3, 6])
def test_taut_string_frequency_from_wave_speed(synthetic_tube, mode):
    """Derive the tension-only limit from the transverse wave equation.

    Independent hand calculation for the same annulus:
    A=pi*(0.05^2-0.03^2)=pi/625 m^2,
    I=pi*(0.05^4-0.03^4)/4=17*pi/12500000 m^4,
    mu=5000*A=8*pi kg/m. Neglecting bending stiffness gives
    mu*y_tt=T*y_xx. With fixed displacements at x=0,L, a sine mode
    has k_n=n*pi/L and c=sqrt(T/mu). The prescribed T=80000*pi N
    gives c=100 m/s, so f_n=c*k_n/(2*pi)=5*n Hz at L=10 m.
    Quadrupling T gives c=200 m/s and f_n=10*n Hz.
    This is the calculator's ideal string branch, not a coupled beam solve.
    """
    member = replace(
        synthetic_tube, boundary_condition=BoundaryCondition.TENSION_CONTROLLED,
    )
    calculator = FrequencyCalculator()
    result = calculator.calculate_natural_frequency(
        member, mode=mode, include_added_mass=False,
    )
    increased_tension = calculator.calculate_natural_frequency(
        replace(member, top_tension=4 * member.top_tension),
        mode=mode, include_added_mass=False,
    )

    assert result.frequency == pytest.approx(5 * mode, rel=REL_TOL)
    assert increased_tension.frequency == pytest.approx(10 * mode, rel=REL_TOL)
    assert increased_tension.frequency / result.frequency == pytest.approx(
        sqrt(4), rel=REL_TOL,
    )
    assert result.includes_added_mass is False
    assert result.effective_mass_ratio == pytest.approx(1.0, rel=REL_TOL)


@pytest.mark.parametrize(
    "boundary,length_power,mode_power",
    [
        (BoundaryCondition.PINNED_PINNED, 2, 2),
        (BoundaryCondition.TENSION_CONTROLLED, 1, 1),
    ],
)
@pytest.mark.parametrize("mode", [1, 3, 6])
def test_frequency_scaling_and_explicit_added_mass(
    synthetic_tube, boundary, length_power, mode_power, mode,
):
    """Check ratios independently of the absolute frequency calculation.

    From the energy and wave-speed derivations above, beam f scales as
    n^2/L^2 and string f as n/L; both scale as mu^(-1/2).
    Doubling L gives frequency ratios 1/4 and 1/2 respectively.
    Quadrupling the chosen density gives mu_dense=32*pi kg/m and
    f_dense/f_dry=1/2, with geometry, modulus and tension held fixed.

    Prescribed synthetic fluid density 1000 kg/m^3 and coefficient 0.8
    define added mass per length explicitly as displaced-fluid inertia:
    mu_a = rho_f*C_a*integral(s ds dtheta, 0<=s<=0.05)
         = 1000*0.8*pi*0.05^2 = 2*pi kg/m.
    Thus mu_wet/mu_dry=(8*pi+2*pi)/(8*pi)=5/4 and
    f_wet/f_dry=sqrt(4/5). No empirical coefficient is inferred.
    """
    member = replace(synthetic_tube, boundary_condition=boundary)
    calculator = FrequencyCalculator(
        fluid=FluidProperties(density=1000.0, added_mass_coefficient=0.8),
    )
    dry = calculator.calculate_natural_frequency(
        member, mode=mode, include_added_mass=False,
    )
    fundamental = calculator.calculate_natural_frequency(
        member, mode=1, include_added_mass=False,
    )
    longer = calculator.calculate_natural_frequency(
        replace(member, length=2 * member.length),
        mode=mode, include_added_mass=False,
    )
    denser_member = replace(
        member, material=replace(member.material, density=4 * member.material.density),
    )
    denser = calculator.calculate_natural_frequency(
        denser_member, mode=mode, include_added_mass=False,
    )
    wet = calculator.calculate_natural_frequency(
        member, mode=mode, include_added_mass=True,
    )

    assert member.mass_per_length == pytest.approx(8 * pi, rel=REL_TOL)
    assert denser_member.mass_per_length == pytest.approx(32 * pi, rel=REL_TOL)
    assert dry.frequency / fundamental.frequency == pytest.approx(
        mode**mode_power, rel=REL_TOL,
    )
    assert longer.frequency / dry.frequency == pytest.approx(
        2 ** (-length_power), rel=REL_TOL,
    )
    assert denser.frequency / dry.frequency == pytest.approx(1 / 2, rel=REL_TOL)
    assert wet.frequency / dry.frequency == pytest.approx(sqrt(4 / 5), rel=REL_TOL)
    assert wet.effective_mass_ratio == pytest.approx(5 / 4, rel=REL_TOL)
    assert dry.effective_mass_ratio == pytest.approx(1.0, rel=REL_TOL)
    assert wet.includes_added_mass is True
    assert dry.includes_added_mass is False
