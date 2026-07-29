from decimal import Decimal
from math import sqrt

import pytest

from digitalmodel.marine_ops.artificial_lift.reference_catalog import (
    AmbiguousCatalogKeyError,
    coupling_properties,
    find_couplings,
    load_catalog,
    rod_properties,
    surface_unit_geometry,
)


def test_rod_properties_preserve_catalog_values():
    rod = rod_properties(" ９７ ", "0.875")

    assert rod.grade == "97"
    assert rod.diameter_in == Decimal("0.875")
    assert rod.area_in2 == Decimal("0.601")
    assert rod.unit_weight_lbf_ft == Decimal("2.22")
    assert rod.modulus_psi == Decimal("30500000")
    assert rod.catalog_sonic_velocity_ft_s == Decimal("16000")
    assert rod.tensile_strength_psi == Decimal("140000")


def test_rod_sonic_velocity_matches_independent_physics():
    area_in2 = 0.601
    unit_weight_lbf_ft = 2.22
    modulus_psi = 30.5e6
    density_lbm_in3 = unit_weight_lbf_ft / (area_in2 * 12)
    computed_ft_s = sqrt(modulus_psi * 386.0886 / density_lbm_in3) / 12

    assert computed_ft_s == pytest.approx(16_299.1, abs=0.2)
    assert abs(computed_ft_s - 16_300) / 16_300 < 0.01
    assert float(
        rod_properties("97", Decimal("0.875")).weight_derived_velocity_ft_s
    ) == pytest.approx(computed_ft_s, rel=1e-9)


@pytest.mark.parametrize(
    "grade, diameter",
    [
        ("", "0.875"),
        (None, "0.875"),
        ("97", True),
        ("97", 0),
        ("97", -0.875),
        ("97", "nan"),
        ("97", "inf"),
        ("97", "0.8751"),
        ("97-0.875", "0.875"),
    ],
)
def test_rod_lookup_rejects_malformed_keys(grade, diameter):
    with pytest.raises((TypeError, ValueError)):
        rod_properties(grade, diameter)


def test_unknown_rod_key_raises_without_default():
    with pytest.raises(KeyError, match=r"UNKNOWN.*0\.875"):
        rod_properties("unknown", "0.875")


def test_coupling_lookup_never_silently_selects_a_diameter_match():
    matches = find_couplings("0.875")

    assert isinstance(matches, tuple)
    assert len(matches) > 1
    with pytest.raises(AmbiguousCatalogKeyError):
        coupling_properties("0.875")


def test_coupling_filters_return_literal_catalog_dimensions():
    coupling = coupling_properties(
        "0.875",
        manufacturer="Generic",
        size="Full Size",
        type="Spray Metal",
    )

    assert coupling.coupling_diameter_in == Decimal("1.8125")
    assert coupling.coupling_length_in == Decimal("4")
    assert coupling.tensile_strength_psi == Decimal("90000")
    assert coupling.friction_coefficient == Decimal("0.2")


def test_surface_unit_lookup_returns_source_qualified_raw_geometry():
    unit = surface_unit_geometry(
        "A",
        "T5F30-5B-D16B",
        source_catalog="surface_unit_catalog",
    )

    assert unit.geometry_code == "C"
    assert unit.gearbox_rating_raw == Decimal("16")
    assert unit.dimensional_a_raw == Decimal("45")
    assert unit.dimensional_c_raw == Decimal("45")
    assert unit.dimensional_i_raw == Decimal("45")
    assert unit.dimensional_k_raw == Decimal("61")
    assert unit.dimensional_p_raw == Decimal("60")


def test_packaged_manifest_counts_and_hashes_validate():
    catalog = load_catalog()

    assert catalog.version == "v1"
    assert len(catalog.rods) > 700
    assert len(catalog.couplings) == 116
    assert len(catalog.surface_units) == 7_173
