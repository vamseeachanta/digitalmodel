# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
# ABOUTME: Synthetic moment-equilibrium and displacement-scaling checks for girting
# ABOUTME: Independent hand calculations for the bounded slice of issue #1195
"""Check girting similarity against independent synthetic hand calculations.

Calculation basis (no external data): choose a transverse force F = 100 t-force,
a vertical lever l = 4 m and displacement W = 400 t. Moment equilibrium gives
W * HA = F * l * cos(phi), with the common gravity conversion cancelling, so
HA = cos(phi) metres. Elementary trigonometry gives cos(0) = 1,
cos(45 degrees) = sqrt(2)/2 and cos(60 degrees) = 1/2.

Specify synthetic GZ samples independently as (0 degrees, 0 m),
(45 degrees, sqrt(2)/2 m) and (60 degrees, 3/4 m). Thus the first intersection
is at 45 degrees, with a 15-degree margin to the chosen 60-degree deck edge.
The GZ/cos(phi) ratios at the nonzero samples are 1 and 3/2 m; the latter
sets the deck-edge limit F_crit = W * (3/4) / (l * 1/2) = 150 t-force.
Choosing the deck edge at 45 degrees instead gives F_crit = 100 t-force,
HA = sqrt(2)/2 m and zero angular margin at the boundary.

Scale factors 1/4, 1 and 4 are synthetic choices covering smaller, unchanged
and larger loads. Scaling F and W together preserves F/W, every heeling arm
and the equilibrium angle. Scaling W alone multiplies the critical force
because the limiting GZ, lever and cosine remain fixed. The default approx
precision accounts for floating-point trigonometry and the bisection search.
"""

import math

import pytest

from digitalmodel.tug.stability_girting import (
    assess_girting,
    critical_towline_force,
    heeling_arm_curve,
)


# All physical inputs and expected values follow the hand calculation above.
HEEL_ANGLES_DEG = [0.0, 45.0, 60.0]
GZ_SYNTHETIC = [(0.0, 0.0), (45.0, math.sqrt(2.0) / 2.0), (60.0, 0.75)]
FORCE_T = 100.0
LEVER_M = 4.0
DISPLACEMENT_T = 400.0
SCALE_FACTORS = (0.25, 1.0, 4.0)


@pytest.mark.parametrize("scale", SCALE_FACTORS)
def test_heeling_arm_preserves_force_displacement_ratio(scale):
    """The same force/displacement ratio preserves arms and stable heel."""
    curve = heeling_arm_curve(
        FORCE_T * scale, LEVER_M, DISPLACEMENT_T * scale, HEEL_ANGLES_DEG
    )
    assert [angle for angle, _ in curve] == HEEL_ANGLES_DEG
    assert [arm for _, arm in curve] == pytest.approx(
        [1.0, math.sqrt(2.0) / 2.0, 0.5]
    )

    result = assess_girting(
        GZ_SYNTHETIC, FORCE_T * scale, LEVER_M, DISPLACEMENT_T * scale, 60.0
    )
    assert result.equilibrium_heel_deg == pytest.approx(45.0)
    assert result.margin_deg == pytest.approx(15.0)
    assert result.girting_safe is True


@pytest.mark.parametrize("scale", SCALE_FACTORS)
def test_critical_towline_force_scales_with_displacement(scale):
    """Fixed righting arms require critical force proportional to displacement."""
    reference = critical_towline_force(GZ_SYNTHETIC, LEVER_M, DISPLACEMENT_T, 60.0)
    scaled = critical_towline_force(
        GZ_SYNTHETIC, LEVER_M, DISPLACEMENT_T * scale, 60.0
    )
    assert reference == pytest.approx(150.0)
    assert scaled == pytest.approx(150.0 * scale)
    assert scaled == pytest.approx(reference * scale)


@pytest.mark.parametrize("scale", SCALE_FACTORS)
def test_deck_edge_equilibrium_has_zero_margin(scale):
    """At the hand-calculated boundary, equilibrium has no positive margin."""
    curve = heeling_arm_curve(
        FORCE_T * scale, LEVER_M, DISPLACEMENT_T * scale, [45.0]
    )
    assert [arm for _, arm in curve] == pytest.approx([math.sqrt(2.0) / 2.0])

    result = assess_girting(
        GZ_SYNTHETIC, FORCE_T * scale, LEVER_M, DISPLACEMENT_T * scale, 45.0
    )
    assert result.equilibrium_heel_deg == pytest.approx(45.0)
    assert result.critical_towline_t == pytest.approx(100.0 * scale)
    assert result.margin_deg == pytest.approx(0.0)
    assert result.girting_safe is False
