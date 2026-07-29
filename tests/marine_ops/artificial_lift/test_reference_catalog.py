from decimal import Decimal
from math import sqrt

import pytest

from digitalmodel.marine_ops.artificial_lift.reference_catalog import (
    AmbiguousCatalogKeyError,
    coupling_properties,
    find_couplings,
    find_surface_units,
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
        ("97 - .875", "0.875"),
        ("97 - 1", "0.875"),
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


def test_coupling_lookup_accepts_catalogued_five_place_diameter():
    matches = find_couplings("1.15625", type="Continuous Rod")

    assert len(matches) == 1
    assert matches[0].coupling_diameter_in == Decimal("1.15625")


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


def test_surface_search_skips_rows_with_blank_model_keys():
    assert find_surface_units(
        "B", "not a catalog model", source_catalog="surface_unit_catalog"
    ) == ()


def test_surface_unit_cross_source_ambiguity_raises():
    matches = find_surface_units("A", "A-C-114-133-54")

    assert len(matches) == 2
    with pytest.raises(AmbiguousCatalogKeyError):
        surface_unit_geometry("A", "A-C-114-133-54")


def test_packaged_manifest_counts_and_hashes_validate():
    catalog = load_catalog()

    assert catalog.version == "v1"
    assert len(catalog.rods) > 700
    assert len(catalog.couplings) == 116
    assert len(catalog.surface_units) == 7_173
    assert catalog.manifest["schema_version"] == "1.0"
    assert catalog.manifest["sources"]["rod_details"]["relative_workbook"] == (
        "REF/Rod Detail Table.xlsx"
    )
    assert catalog.manifest["outputs"]["rod_details.csv"]["units"]["modulus_psi"] == (
        "psi"
    )
    assert catalog.manifest["outputs"]["rod_details.csv"]["units"][
        "raw_sonic_velocity_kft_s"
    ] == "thousand_ft_per_s"
    assert catalog.manifest["outputs"]["rodpump_units.csv"]["units"][
        "source_identifier"
    ] == "text_or_identifier"
    surface_source = catalog.manifest["sources"]["surface_unit_catalog"]
    assert surface_source["source_rows"] == 3_593
    assert surface_source["emitted_rows"] == 3_572
    assert surface_source["lookup_eligible_rows"] == 3_572
    assert surface_source["quarantined_rows"] == 21
    falsification = catalog.manifest["physics_validation"]["independent_falsification"]
    assert Decimal(falsification["computed_ft_s"]) == pytest.approx(
        Decimal("16299.0966"), abs=Decimal("0.001")
    )
    assert Decimal(falsification["relative_difference"]) < Decimal("0.01")


def test_catalog_manifest_is_deeply_immutable():
    catalog = load_catalog()

    with pytest.raises(TypeError):
        catalog.manifest["outputs"]["rod_details.csv"]["row_count"] = 0
    with pytest.raises(TypeError):
        catalog.manifest["transformations"][0] = "tampered"
