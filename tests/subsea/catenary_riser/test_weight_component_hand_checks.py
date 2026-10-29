# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Independent submerged-weight worksheet for synthetic concentric tubes.

Basis: circular area pi*r**2, annular volume pi*(R**2-r**2)*L,
mass=rho*volume and downward force=mass*g minus displaced-fluid mass*g.
The gravity convention is g=9.81 m/s**2, as documented in
src/digitalmodel/subsea/catenary_riser/docs/engineering-basis.md, E4 and
its unit-riser worksheet. This is arithmetic coverage, not an ISO 16389 check.

All dimensions and densities below are invented arithmetic inputs, not material
presets or design data. A 5 m tube has OD=0.40 m, wall=0.03 m and optional
0.02 m coating. Thus the bore radius is 0.17 m, metal radius 0.20 m and
coated radius 0.22 m. Independently expanding those squares gives volumes:
metal=0.0555*pi, bore=0.1445*pi, coating=0.0420*pi and displacement=0.2420*pi
in m**3. Without coating, displacement=0.2000*pi m**3.

The sinking tube uses densities 6400 (metal), 900 (coating), 720 (contents)
and 1100 (external fluid), all in kg/m**3. Its masses per metre are
71.04*pi, 7.56*pi, 20.808*pi and 53.24*pi kg/m respectively, giving
46.168*pi*g N/m downward. An empty uncoated tube has zero contents mass,
44*pi kg/m displaced fluid and 27.04*pi*g N/m downward. Empty means a sealed
zero-density bore here, rather than the nonzero-density air preset.

The buoyant tube uses metal/coating/contents densities 800/100/0 kg/m**3
and the same external fluid. Its mass per metre is (8.88+0.84)*pi kg/m,
so its signed submerged weight is -43.52*pi*g N/m. The unused elastic
inputs, modulus=200000 MPa and Poisson ratio=0.25, are synthetic placeholders.
Relative tolerance 1e-12 permits floating-point roundoff in this closed-form
arithmetic; absolute tolerance zero requires exact zero for absent components.
"""

import math

import pytest

from digitalmodel.subsea.catenary_riser.models import (
    FluidProperties,
    MaterialProperties,
    RiserConfiguration,
)


GRAVITY = 9.81  # Engineering basis E4; deliberately independent of the model.
REL_TOL = 1e-12  # Numerical comparison convention explained in the worksheet.


def _hand_case(case):
    """Return a configuration and a worksheet using inputs only."""
    # All literals and expanded volume coefficients are derived above.
    length = 5.0
    metal_density, contents_density = 6400.0, 720.0
    coating_thickness, coating_density = 0.02, 900.0
    coating_volume = 0.0420 * math.pi
    displaced_volume = 0.2420 * math.pi
    if case in ("empty-uncoated", "empty-zero-coating"):
        contents_density = 0.0
        coating_thickness = None if case == "empty-uncoated" else 0.0
        coating_density = None if case == "empty-uncoated" else 0.0
        coating_volume = 0.0
        displaced_volume = 0.2000 * math.pi
    elif case == "buoyant":
        metal_density, coating_density, contents_density = 800.0, 100.0, 0.0

    metal_volume = 0.0555 * math.pi
    bore_volume = 0.1445 * math.pi
    external_density = 1100.0
    metal_mass = metal_density * metal_volume
    coating_mass = (coating_density or 0.0) * coating_volume
    contents_mass = contents_density * bore_volume
    displaced_mass = external_density * displaced_volume
    expected = {
        "inner_diameter": 0.34,
        "pipe_cross_section": metal_volume / length,
        "internal_area": bore_volume / length,
        "external_area": displaced_volume / length,
        "displaced_volume_per_length": displaced_volume / length,
        "steel_weight_per_length": metal_mass * GRAVITY / length,
        "coating_weight_per_length": coating_mass * GRAVITY / length,
        "contents_weight_per_length": contents_mass * GRAVITY / length,
        "buoyancy_per_length": displaced_mass * GRAVITY / length,
        "effective_weight_per_length": (
            (metal_mass + coating_mass + contents_mass - displaced_mass)
            * GRAVITY / length
        ),
    }
    riser = RiserConfiguration(
        name="synthetic tube",
        outer_diameter=0.40,
        wall_thickness=0.03,
        length=length,
        material=MaterialProperties(
            name="synthetic metal", youngs_modulus=200000.0,
            poissons_ratio=0.25, density=metal_density,
        ),
        internal_fluid=FluidProperties("synthetic contents", contents_density),
        external_fluid=FluidProperties("synthetic exterior", external_density),
        coating_thickness=coating_thickness,
        coating_density=coating_density,
    )
    return riser, expected


@pytest.fixture(params=[
    "coated", "empty-uncoated", "empty-zero-coating", "buoyant",
])
def hand_case(request):
    """Exercise filled, absent/zero-coating and upward-force branches."""
    return _hand_case(request.param)


def test_riser_weight_components_match_independent_annuli(hand_case):
    """Check each public geometry and force contribution against the worksheet."""
    riser, expected = hand_case
    for property_name in (
        "inner_diameter", "pipe_cross_section", "internal_area", "external_area",
        "displaced_volume_per_length", "steel_weight_per_length",
        "coating_weight_per_length", "contents_weight_per_length",
        "buoyancy_per_length",
    ):
        assert getattr(riser, property_name) == pytest.approx(
            expected[property_name], rel=REL_TOL, abs=0.0,
        ), property_name


def test_riser_effective_weight_obeys_hand_force_balance(hand_case):
    """Compare net force with independently summed masses, never model outputs."""
    riser, expected = hand_case
    assert riser.effective_weight_per_length == pytest.approx(
        expected["effective_weight_per_length"], rel=REL_TOL, abs=0.0,
    )


def test_riser_buoyant_case_retains_negative_effective_weight():
    """Upward net force stays negative; it is neither clamped nor made absolute."""
    riser, _ = _hand_case("buoyant")
    # Expanded mass balance from the worksheet: 8.88+0.84-53.24=-43.52.
    assert riser.effective_weight_per_length == pytest.approx(
        -43.52 * math.pi * GRAVITY, rel=REL_TOL, abs=0.0,
    )
    assert riser.effective_weight_per_length < 0.0
