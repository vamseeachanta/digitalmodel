# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent submerged-section geometry for one wall-sided righting lever.

All geometry is synthetic, stipulated here rather than physical design input.
The hand calculation below establishes this case only; it does not qualify an
unavailable textbook edition or establish compliance with a stability code.
The absolute tolerance of 1e-12 m, with zero relative tolerance, allows floating
point arithmetic rounding only, not physical model error.
"""

import math

import pytest

from digitalmodel.naval_architecture.loading_computer import gz_wall_sided


def test_nonzero_heel_gz_matches_submerged_polygon_moments():
    """Derive the lever by hand from submerged-area first moments.

    Stipulate a rectangular section: B=4 m, upright T=2 m, depth=4 m,
    KG=1 m, and tan(phi)=1/2. Body coordinates have x across the section
    and z above its bottom; G is on x=0. The waterline is h(x)=2+x/2
    for -2 <= x <= 2. Its extrema are 1 m and 3 m, strictly between
    the bottom and deck at 0 m and 4 m: no bilge exposure or deck-edge
    immersion. These bounds are also asserted below.

    Integrating over -2 <= x <= 2 gives, independently of production:
        A = integral (2+x/2) dx = 8 m2,
        x_B = integral x*(2+x/2) dx / A = (8/3)/8 = 1/3 m,
        z_B = integral (2+x/2)^2/2 dx / A = (26/3)/8 = 13/12 m.
    The upright waterplane moment per unit length is integral x^2 dx
    = B^3/12; hence BM = B^2/(12*T) = 2/3 m and
    GM = T/2 + BM - KG = 2/3 m. With cos(phi)=2/sqrt(5) and
    sin(phi)=1/sqrt(5), the horizontal lever from G to buoyancy is
        x_B*cos(phi) + (z_B-KG)*sin(phi)
        = (1/3)*2/sqrt(5) + (13/12-1)/sqrt(5) = 3/(4*sqrt(5)) m.
    This area-moment result is the numerical oracle, not the wall-sided
    expression under test. No production helper supplies either centroid.
    """
    beam_m, draft_m, depth_m = 4.0, 2.0, 4.0
    heel_tan = 1.0 / 2.0
    assert 0.0 < draft_m - beam_m / 2.0 * heel_tan
    assert draft_m + beam_m / 2.0 * heel_tan < depth_m

    bm_m = 2.0 / 3.0  # Upright waterplane moment / area, derived above.
    gm_m = 2.0 / 3.0  # Upright KB + BM - KG, derived above.
    heel_deg = math.degrees(math.atan(heel_tan))
    expected_gz_m = 3.0 / (4.0 * math.sqrt(5.0))

    assert gz_wall_sided(gm_m, bm_m, heel_deg) == pytest.approx(
        expected_gz_m, abs=1e-12, rel=0
    )


def test_geometric_gz_reflection_and_upright_limit():
    """Check reflection and upright geometry against independent levers.

    Use the preceding test's hand-integrated section and upright GM=BM=2/3 m.
    Reflecting its waterline gives h(x)=2-x/2, still between 1 m and 3 m:
    x_B=-1/3 m, z_B=13/12 m, and sin(-phi)=-1/sqrt(5).
    Thus the geometric lever is -3/(4*sqrt(5)) m, without relying on the
    positive production result. Upright, x_B=0 and G and B share the vertical
    centreline, so the horizontal lever is exactly zero.
    """
    bm_m = 2.0 / 3.0
    gm_m = 2.0 / 3.0
    heel_deg = math.degrees(math.atan(1.0 / 2.0))
    expected_gz_m = 3.0 / (4.0 * math.sqrt(5.0))

    assert gz_wall_sided(gm_m, bm_m, -heel_deg) == pytest.approx(
        -expected_gz_m, abs=1e-12, rel=0
    )
    assert gz_wall_sided(gm_m, bm_m, 0.0) == pytest.approx(
        0.0, abs=1e-12, rel=0
    )
