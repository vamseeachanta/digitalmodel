"""Baseline tests for current B401 edition divergence.

These tests intentionally capture today's split behavior before the edition
merge changes source code. They should keep passing through the merge unless a
slice deliberately updates the expected cross-edition contract.
"""

from __future__ import annotations

import math

import pytest


def test_baseline_splash_zone_divergence():
    """2017 functional splash-zone demand is zero; 2021 router demand is not."""
    from digitalmodel.cathodic_protection.marine_structure_cp import (
        ClimateRegion,
        ExposureZone,
        ZONE_CURRENT_DENSITY,
    )
    from digitalmodel.infrastructure.base_solvers.hydrodynamics.cp_DNV_RP_B401_2021 import (
        _B401_2021_CURRENT_DENSITIES,
    )

    assert ZONE_CURRENT_DENSITY[
        (ExposureZone.SPLASH, ClimateRegion.TEMPERATE)
    ] == (0.0, 0.0, 0.0)
    assert _B401_2021_CURRENT_DENSITIES["splash"]["coated"] == 0.100
    assert _B401_2021_CURRENT_DENSITIES["splash"]["bare"] == 0.200


def test_baseline_flush_anode_resistance_ratio_geometry_dependent():
    """McCoy and Dwight flush formulas differ by about 1.8702 at L=2m, r=0.15m."""
    length_m = 2.0
    radius_m = 0.15

    r_mccoy = (1.0 / (math.pi * length_m)) * (
        math.log(2.0 * length_m / radius_m) - 0.5
    )
    r_dwight = (1.0 / (2.0 * math.pi * length_m)) * (
        math.log(4.0 * length_m / radius_m) - 1.0
    )

    assert r_mccoy / r_dwight == pytest.approx(1.8702, rel=1e-3)
    assert 1.5 < r_mccoy / r_dwight < 2.0


def test_baseline_internal_router_dwight_divergence():
    """Router B401 and sacrificial-anode helpers use different Dwight log arguments."""
    length_m = 2.0
    radius_m = 0.15

    r_b401 = (1.0 / (2.0 * math.pi * length_m)) * (
        math.log(4.0 * length_m / radius_m) - 1.0
    )
    r_sacrificial = (1.0 / (2.0 * math.pi * length_m)) * (
        math.log(2.0 * length_m / radius_m) - 1.0
    )

    assert r_b401 / r_sacrificial == pytest.approx(1.303, rel=5e-3)


def test_baseline_coating_category_schema_divergence():
    """Functional 2017 has 9 coating categories; router 2021 has 4."""
    from digitalmodel.cathodic_protection.coating import CoatingCategory
    from digitalmodel.infrastructure.base_solvers.hydrodynamics.cp_DNV_RP_B401_2021 import (
        _B401_2021_COATING_CATEGORIES,
    )

    assert len(list(CoatingCategory)) == 9
    assert len(_B401_2021_COATING_CATEGORIES) == 4


def test_baseline_coating_breakdown_a_b_constants():
    """FBE 2017 and Cat I 2021 use materially different breakdown constants."""
    from digitalmodel.cathodic_protection.coating import (
        COATING_CONSTANTS,
        CoatingCategory,
    )
    from digitalmodel.infrastructure.base_solvers.hydrodynamics.cp_DNV_RP_B401_2021 import (
        _B401_2021_COATING_CATEGORIES,
    )

    assert COATING_CONSTANTS[CoatingCategory.FBE] == (0.02, 0.003)
    assert _B401_2021_COATING_CATEGORIES["I"] == {"f_ci": 0.05, "k": 0.020}


@pytest.mark.parametrize("calculation_type", ["DNV_rp_b401_2011", "DNV_rp_b401_2021_05"])
def test_baseline_yaml_advertised_dnv_keys_raise_before_implementation(
    calculation_type,
):
    """YAML-advertised B401 keys currently raise before edition routing exists."""
    from digitalmodel.infrastructure.base_solvers.hydrodynamics.cathodic_protection import (
        CathodicProtection,
    )

    cfg = {"inputs": {"calculation_type": calculation_type}}

    with pytest.raises(ValueError, match="not IMPLEMENTED"):
        CathodicProtection().router(cfg)
