# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent annular geometry and axial tension scaling checks.

Basis: elementary circle area A = pi * diameter**2 / 4 and uniform axial
stress sigma = N / A. All dimensions, loads and material properties below
are synthetic assumptions, chosen for transparent hand arithmetic. They
are not standard examples or licensed acceptance criteria. These checks
do not qualify the full API RP 2A WSD implementation or its allowable factors.
"""

import math

import pytest

from digitalmodel.structural.jacket_topside.member_checks import (
    TubularSection,
    axial_tension_uc,
)


@pytest.fixture
def synthetic_tubular() -> TubularSection:
    """Declare every section/material input as a synthetic assumption.

    OD = 10 in, t = 1 in, Fy = 40 ksi, E = 28,000 ksi, kL = 120 in.
    These values represent no particular material grade or design standard.
    """
    return TubularSection(
        outer_diameter=10.0,
        wall_thickness=1.0,
        yield_strength=40.0,
        elastic_modulus=28_000.0,
        effective_length=120.0,
    )


def test_axial_stress_hand_annulus(synthetic_tubular: TubularSection) -> None:
    """Check area and stress against an independent hand calculation."""
    # Hand basis: ID = OD - 2*t = 10 - 2*1 = 8 in.
    # A = pi*(OD**2 - ID**2)/4 = pi*(100 - 64)/4 = 9*pi in².
    expected_area = math.pi * (10.0**2 - 8.0**2) / 4.0
    assert expected_area == pytest.approx(9.0 * math.pi)
    assert synthetic_tubular.inner_diameter == pytest.approx(8.0)
    assert synthetic_tubular.cross_sectional_area == pytest.approx(expected_area)

    # Synthetic force N = 45*pi kips gives sigma = (45*pi)/(9*pi) = 5 ksi.
    force_kips = 45.0 * math.pi
    stress_ksi = force_kips / synthetic_tubular.cross_sectional_area
    assert stress_ksi == pytest.approx(5.0)
    result = axial_tension_uc(synthetic_tubular, fa=stress_ksi)
    assert result.applied_stress == pytest.approx(5.0)


def test_tension_load_scaling(synthetic_tubular: TubularSection) -> None:
    """Double demand at fixed section/material; zero force gives zero demand."""
    # Synthetic forces: N = 45*pi kips, 2*N = 90*pi kips and unloaded N = 0.
    # With hand-derived A = 9*pi in², stresses are 5, 10 and 0 ksi.
    force_kips = 45.0 * math.pi
    base = axial_tension_uc(
        synthetic_tubular, fa=force_kips / synthetic_tubular.cross_sectional_area
    )
    doubled = axial_tension_uc(
        synthetic_tubular,
        fa=(2.0 * force_kips) / synthetic_tubular.cross_sectional_area,
    )
    unloaded = axial_tension_uc(synthetic_tubular, fa=0.0)

    assert base.applied_stress == pytest.approx(5.0)
    assert doubled.applied_stress == pytest.approx(10.0)
    # No numeric allowable is independently qualified here; only its invariance.
    assert math.isfinite(base.allowable_stress)
    assert base.allowable_stress > 0.0
    assert doubled.allowable_stress == base.allowable_stress
    assert unloaded.allowable_stress == base.allowable_stress
    assert base.unity_check > 0.0
    assert doubled.unity_check == pytest.approx(2.0 * base.unity_check)
    assert unloaded.applied_stress == 0.0
    assert unloaded.unity_check == 0.0
    assert unloaded.passes is True
