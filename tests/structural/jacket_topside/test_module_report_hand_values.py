# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent integrals and zero-demand arithmetic for synthetic geometry.

Basis: worked-tubular-section-geometry.md, Independent derivation. Joint
ratios follow division of the declared dimensions, without a capacity formula.
All material inputs are synthetic assumptions, not a specified steel grade.
"""

import math

import pytest

from digitalmodel.structural.jacket_topside import (
    JointGeometry,
    JointLoads,
    JointType,
    MemberLoads,
    TubularSection,
    classify_joint,
    member_checks,
    punching_shear_uc,
)


# Synthetic dimensions from the documented annulus; material/length choices
# follow test_tension_scaling.py's synthetic assumptions. No defaults are used.
def synthetic_section():
    """Return the explicitly specified, nondegenerate synthetic section."""
    return TubularSection(12.0, 0.5, 40.0, 28_000.0, 120.0)


def test_annulus_against_radial_integrals():
    """Integrals of rho and rho^3 sin²(theta) give A and I independently."""
    section = synthetic_section()
    expected = {
        "inner_diameter": 11.0,
        "cross_sectional_area": 23 * math.pi / 4,
        "moment_of_inertia": 6095 * math.pi / 64,
        "section_modulus": 6095 * math.pi / 384,
        "radius_of_gyration": math.sqrt(265) / 4,
    }
    for name, value in expected.items():
        # Same floating-point rounding policy as the documented geometry test.
        assert getattr(section, name) == pytest.approx(value, rel=1e-12, abs=0)


def test_joint_ratios_and_single_brace_classification():
    """Hand ratios: (12/2)/(1/2)=12; 6/12=(1/4)/(1/2)=1/2."""
    geometry = JointGeometry(12.0, 0.5, [90.0], [6.0], [0.25])
    assert geometry.gamma == 12.0
    assert geometry.beta == [0.5]
    assert geometry.tau == [0.5]
    assert geometry.num_braces == 1
    assert classify_joint(geometry) is JointType.T_Y


@pytest.mark.parametrize("is_tension", [False, True])
def test_member_zero_demand_is_arithmetic_only(is_tension):
    """Zero divided by finite positive capacity is zero; no capacity oracle."""
    results = member_checks(
        synthetic_section(), MemberLoads(fa=0.0, fb=0.0, Cm=1.0, is_tension=is_tension)
    )
    for result in results.values():
        assert math.isfinite(result.allowable_stress)
        assert result.allowable_stress > 0.0
        assert result.applied_stress == 0.0
        assert result.unity_check == 0.0


def test_joint_zero_demand_is_arithmetic_only():
    """Nonzero dimensions and positive synthetic strength exclude degeneracy."""
    result = punching_shear_uc(
        JointGeometry(12.0, 0.5, [90.0], [6.0], [0.25]),
        JointLoads(axial_load=0.0, ipb_moment=0.0, opb_moment=0.0),
        chord_fy=40.0, brace_index=0, chord_utilization=0.0,
    )
    assert math.isfinite(result.allowable_vp)
    assert result.allowable_vp > 0.0
    assert result.applied_vp == 0.0
    assert result.unity_check == 0.0
    assert result.joint_type is JointType.T_Y
