"""Baseline tests for B401 behaviour shared by the legacy solver and the new package.

Re-baselined for #2207: both code paths now draw their numbers from the cited
``b401_tables`` / ``f103_tables`` modules, so these tests pin the agreed
cross-path contract (splash 0 A/m2, Table 10-4 Cat I, F103 Table A.1 FBE) and
the resistance formulas that intentionally still differ.
"""

from __future__ import annotations

import math

import pytest

_EDITION = "2021"


def _legacy_densities(zone: str, temperature_c: float = 10.0) -> dict:
    from digitalmodel.infrastructure.base_solvers.hydrodynamics.cp_DNV_RP_B401_2021 import (
        _b401_current_densities,
    )

    inputs = {
        "design_data": {"edition": _EDITION},
        "structure": {"zones": [{"zone": zone, "area_m2": 100.0, "coating_category": "I"}]},
        "environment": {"seawater_temperature_C": temperature_c},
    }
    return _b401_current_densities(inputs)[zone]


def test_baseline_splash_zone_zero_in_both_paths():
    """Legacy router and functional package both give the splash zone 0 A/m2."""
    from digitalmodel.cathodic_protection.b401_tables import Climate, DesignPhase
    from digitalmodel.cathodic_protection.marine_structure_cp import (
        ExposureZone,
        zone_current_density,
    )

    for phase in DesignPhase:
        assert (
            zone_current_density(ExposureZone.SPLASH, Climate.TEMPERATE, 0.0, phase, _EDITION)
            is None
        )
    legacy = _legacy_densities("splash")
    assert legacy["i_initial_A_m2"] == 0.0
    assert legacy["i_mean_A_m2"] == 0.0
    assert legacy["i_final_A_m2"] == 0.0


def test_baseline_submerged_density_agrees_across_paths():
    """Legacy router submerged densities equal the cited Table 10-1 / 10-2 values."""
    from digitalmodel.cathodic_protection.b401_tables import Climate, DesignPhase
    from digitalmodel.cathodic_protection.marine_structure_cp import (
        ExposureZone,
        zone_current_density,
    )

    legacy = _legacy_densities("submerged", temperature_c=10.0)
    for phase, key in (
        (DesignPhase.INITIAL, "i_initial_A_m2"),
        (DesignPhase.MEAN, "i_mean_A_m2"),
        (DesignPhase.FINAL, "i_final_A_m2"),
    ):
        cited = zone_current_density(ExposureZone.SUBMERGED, Climate.TEMPERATE, 0.0, phase, _EDITION)
        assert cited is not None
        assert legacy[key] == pytest.approx(cited.value)
    # temperate 0-30 m: 0.200 / 0.100 / 0.130
    assert legacy["i_initial_A_m2"] == pytest.approx(0.200)
    assert legacy["i_mean_A_m2"] == pytest.approx(0.100)
    assert legacy["i_final_A_m2"] == pytest.approx(0.130)


def test_baseline_flush_anode_resistance_is_table_10_7_short_flush():
    """The flush wrapper is B401 Table 10-7 short flush (0.315 rho / sqrt(A)), not McCoy.

    Re-baselined for #2211: at L = 2 m, W = 0.3 m (A = 0.6 m2) and
    rho = 0.30 ohm-m, R_flush = 0.315 * 0.30 / sqrt(0.6) = 0.12200 ohm, and
    the stand-off Dwight value at r = 0.15 m is 0.07106 ohm (ratio 1.717).
    The former 1.8702 ratio was the half-space slender-body expression the
    review found in neither standard.
    """
    from digitalmodel.cathodic_protection.dnv_rp_b401 import (
        anode_resistance_slender_standoff,
        flush_anode_resistance,
    )

    rho_ohm_m = 0.30
    length_m = 2.0
    radius_m = 0.15
    width_m = 0.30
    inch = 0.0254

    r_dwight = anode_resistance_slender_standoff(rho_ohm_m, length_m, radius_m, edition=_EDITION)
    with pytest.warns(DeprecationWarning):
        r_flush = flush_anode_resistance(
            rho_ohm_m * 100.0,  # ohm-m -> ohm-cm
            length_m / inch,
            width_m / inch,
            1.0,  # height: not used by the Table 10-7 short flush form
            radius_m / inch,  # equivalent radius: not used
            edition=_EDITION,
        )

    # Dwight: rho/(2 pi L) * (ln(4L/r) - 1) = 0.023873 * 2.9766 = 0.07106 ohm
    assert r_dwight == pytest.approx(
        (rho_ohm_m / (2.0 * math.pi * length_m)) * (math.log(4.0 * length_m / radius_m) - 1.0)
    )
    # Table 10-7 short flush: 0.315 * 0.30 / sqrt(2.0 * 0.3) = 0.12200 ohm
    assert r_flush == pytest.approx(0.315 * rho_ohm_m / math.sqrt(length_m * width_m), rel=1e-9)
    assert r_flush / r_dwight == pytest.approx(1.717, rel=1e-3)


def test_baseline_internal_router_dwight_divergence():
    """Legacy router equals the package Dwight formula; the sacrificial helper still diverges."""
    from digitalmodel.cathodic_protection.dnv_rp_b401 import anode_resistance_slender_standoff
    from digitalmodel.infrastructure.base_solvers.hydrodynamics.cp_DNV_RP_B401_2021 import (
        _b401_anode_resistance,
    )
    from digitalmodel.infrastructure.base_solvers.hydrodynamics.cp_sacrificial_anode_b401 import (
        anode_resistance_flush,
    )

    rho_ohm_m = 0.30
    length_m = 2.0
    radius_m = 0.15

    r_package = anode_resistance_slender_standoff(rho_ohm_m, length_m, radius_m, edition=_EDITION)
    r_router = _b401_anode_resistance(
        {
            "environment": {"seawater_resistivity_ohm_m": rho_ohm_m},
            "anode": {"type": "stand_off", "length_m": length_m, "radius_m": radius_m},
        }
    )
    r_sacrificial = anode_resistance_flush(rho=rho_ohm_m, L=length_m, r=radius_m)

    # Same Table 10-7 long slender stand-off formula: ln(4L/r) - 1
    assert r_router == pytest.approx(r_package, rel=1e-12)
    # Sacrificial helper uses ln(2L/r) - 1: (ln 53.33 - 1) / (ln 26.67 - 1) = 1.3036
    assert r_router / r_sacrificial == pytest.approx(1.303, rel=5e-3)


def test_baseline_coating_category_schema_divergence():
    """Functional package has 12 coating categories; router accepts 4 keys."""
    from digitalmodel.cathodic_protection.coating import CoatingCategory
    from digitalmodel.infrastructure.base_solvers.hydrodynamics.cp_DNV_RP_B401_2021 import (
        _B401_2021_COATING_CATEGORIES,
    )

    assert len(list(CoatingCategory)) == 12
    assert sorted(_B401_2021_COATING_CATEGORIES) == ["I", "II", "III", "bare"]


def test_baseline_coating_breakdown_a_b_constants():
    """FBE (F103 Table A.1) and Cat I (B401 Table 10-4) constants in both paths."""
    from digitalmodel.cathodic_protection.coating import (
        COATING_CONSTANTS,
        CoatingCategory,
    )
    from digitalmodel.infrastructure.base_solvers.hydrodynamics.cp_DNV_RP_B401_2021 import (
        _b401_coating_breakdown,
    )

    # F103 Table A.1, CDS No. 1 single/dual layer FBE: a = 0.010, b = 0.0003
    assert COATING_CONSTANTS[CoatingCategory.FBE] == (0.01, 0.0003)
    # B401 Table 10-4 Cat I, row 0-30 m: a = 0.10, b = 0.10/yr in both paths
    assert COATING_CONSTANTS[CoatingCategory.PAINT_I] == (0.10, 0.10)
    legacy = _b401_coating_breakdown(
        {
            "design_data": {"edition": _EDITION},
            "structure": {"zones": [{"zone": "submerged", "area_m2": 100.0, "coating_category": "I"}]},
        },
        25.0,
    )["submerged"]
    assert (legacy["a"], legacy["b_per_yr"]) == (0.10, 0.10)


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
