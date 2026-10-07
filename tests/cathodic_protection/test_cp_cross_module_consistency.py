"""Cross-module consistency: every CP entry point evaluates the same kernel.

Issue #2211 replaced seven current-demand, seven anode-mass, six coating
breakdown and four resistance implementations with one kernel. These tests
pin that the public wrappers in ``dnv_rp_b401``, ``iso_15589_2``,
``anode_sizing``, ``marine_structure_cp``, ``pipeline_cp`` and ``coating``
return bit-identical numbers for the same inputs.
"""

from __future__ import annotations

import math

import pytest

from digitalmodel.cathodic_protection import _kernels as kernel
from digitalmodel.cathodic_protection import anode_sizing, dnv_rp_b401, iso_15589_2
from digitalmodel.cathodic_protection.anode_sizing import AnodeType

EDITION = "2021"

# One input set: mean current 800 A, 25 yr, Al 2000 Ah/kg, u 0.90.
I_MEAN = 800.0
LIFE = 25.0
CAPACITY = 2000.0
UTIL = 0.90
EXPECTED_MASS = I_MEAN * LIFE * 8760.0 / (CAPACITY * UTIL)  # 97 333.33 kg


def test_anode_mass_entry_points_agree():
    ref = kernel.anode_mass(I_MEAN, LIFE, CAPACITY, UTIL)
    assert ref == pytest.approx(EXPECTED_MASS, rel=1e-12)
    assert iso_15589_2.anode_mass_requirement(I_MEAN, LIFE, CAPACITY, UTIL) == ref
    assert dnv_rp_b401.anode_mass_requirement(I_MEAN, LIFE, CAPACITY, UTIL, edition=EDITION) == ref
    assert anode_sizing.calculate_anode_mass(I_MEAN, LIFE, UTIL, CAPACITY) == ref


def test_current_demand_entry_points_agree():
    area, density, fc = 8000.0, 0.100, 1.0
    ref = kernel.current_demand(area, density, fc)
    assert ref == 800.0
    assert dnv_rp_b401.current_demand(area, density, fc, edition=EDITION) == ref
    assert anode_sizing.calculate_current_demand(area, fc, density * 1000.0) == pytest.approx(ref, rel=1e-12)
    # ISO takes the pipe geometry: pi D L = area
    D, L = 0.3239, area / (math.pi * 0.3239)
    assert iso_15589_2.pipeline_current_demand(D, L, fc, density * 1000.0) == pytest.approx(ref, rel=1e-12)


def test_coating_breakdown_entry_points_agree():
    a, b, t = 0.02, 0.012, 15.0
    ref = kernel.coating_breakdown_linear(a, b, t)
    assert dnv_rp_b401.coating_breakdown_factor(a, b, t, edition=EDITION) == ref
    # ISO parametrises by initial/final over the design life: fc_f = a + b T
    T = 25.0
    assert iso_15589_2.coating_breakdown_factor(a, a + b * T, t, T) == pytest.approx(ref, rel=1e-12)


def test_coating_module_uses_kernel_mean_and_final():
    from digitalmodel.cathodic_protection.coating import (
        CoatingCategory,
        coating_breakdown_factors,
        coating_constants,
    )

    constants = coating_constants(CoatingCategory.PAINT_III, 0.0, EDITION)
    result = coating_breakdown_factors(CoatingCategory.PAINT_III, 25.0, 0.0, edition=EDITION)
    assert result.mean_factor == kernel.coating_breakdown_mean(constants.a, constants.b, 25.0)
    assert result.final_factor == kernel.coating_breakdown_final(constants.a, constants.b, 25.0)


def test_resistance_entry_points_agree_long_slender():
    """Four stand-off entry points on one geometry (L = 2.0 m >= 4 r)."""
    rho, L, r = 0.30, 2.0, 0.1076
    ref = kernel.long_slender_standoff(rho, L, r)
    assert ref == pytest.approx((rho / (2 * math.pi * L)) * (math.log(4 * L / r) - 1), rel=1e-12)
    assert kernel.slender_standoff(rho, L, r) == ref
    assert dnv_rp_b401.anode_resistance_slender_standoff(rho, L, r, edition=EDITION) == ref
    assert iso_15589_2.anode_resistance(rho, L, r) == ref
    assert anode_sizing.calculate_anode_resistance(AnodeType.STAND_OFF, L, r, rho) == ref


def test_resistance_entry_points_agree_short_slender():
    """Same four entry points on a stubby geometry (L = 0.3 m < 4 r)."""
    rho, L, r = 0.30, 0.30, 0.10
    ref = kernel.short_slender_standoff(rho, L, r)
    assert kernel.slender_standoff(rho, L, r) == ref
    assert dnv_rp_b401.anode_resistance_slender_standoff(rho, L, r, edition=EDITION) == ref
    assert iso_15589_2.anode_resistance(rho, L, r) == ref
    assert anode_sizing.calculate_anode_resistance(AnodeType.STAND_OFF, L, r, rho) == ref


def test_bracelet_resistance_entry_points_agree():
    """F103 bracelet, anode_sizing bracelet and the deprecated flush wrapper."""
    rho, area = 0.30, 0.38067
    ref = kernel.short_flush_or_bracelet(rho, area)
    assert anode_sizing.calculate_anode_resistance(
        AnodeType.BRACELET, 0.3, 0.2, rho, exposed_area_m2=area
    ) == ref
    inch = 0.0254
    with pytest.warns(DeprecationWarning):
        wrapped = dnv_rp_b401.flush_anode_resistance(
            rho * 100.0, 1.0 / inch, area / inch, 1.0, 1.0, edition=EDITION
        )
    assert wrapped == pytest.approx(ref, rel=1e-12)


def test_anode_current_output_entry_points_agree():
    R_a = 0.0790
    ref = kernel.anode_current_output(0.25, R_a)
    assert iso_15589_2.anode_output_current(R_a, -1.050, -0.800) == ref
    assert dnv_rp_b401.anode_current_output(
        0.30, 2.0, 0.1076, delta_E=0.25, edition=EDITION
    ) == kernel.anode_current_output(0.25, kernel.long_slender_standoff(0.30, 2.0, 0.1076))


def test_pipeline_cp_demand_uses_kernel():
    from digitalmodel.cathodic_protection.pipeline_cp import (
        CURRENT_DENSITY_TABLE,
        PipelineCPInput,
        PipelineEnvironment,
        pipeline_current_demand,
    )

    inp = PipelineCPInput(
        outer_diameter_m=0.3239,
        wall_thickness_m=0.0127,
        length_m=10000.0,
        environment=PipelineEnvironment.SUBMERGED_SEAWATER,
        coating_breakdown_factor=0.03,
    )
    bare, _ = CURRENT_DENSITY_TABLE[PipelineEnvironment.SUBMERGED_SEAWATER]
    expected = kernel.current_demand(math.pi * 0.3239 * 10000.0, bare / 1000.0, 0.03)
    assert pipeline_current_demand(inp).current_demand_A == expected


def test_package_kernel_exports():
    import digitalmodel.cathodic_protection as cp

    assert cp.kernel_anode_mass is kernel.anode_mass
    assert cp.kernel_long_slender_standoff is kernel.long_slender_standoff
    assert cp.kernel_short_flush_or_bracelet is kernel.short_flush_or_bracelet
    assert cp.kernel_resistance_proximity_factor is kernel.resistance_proximity_factor
    assert cp.kernel_equivalent_radius_from_periphery is kernel.equivalent_radius_from_periphery
