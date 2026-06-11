"""Tests for DNV-RP-F106 factory-applied external pipeline coatings."""

from __future__ import annotations

from pathlib import Path

import pytest

import digitalmodel.cathodic_protection as cp
from digitalmodel.cathodic_protection.dnv_rp_f106 import (
    COATING_LIBRARY,
    CoatingType,
    holiday_detection_result,
    holiday_detection_voltage,
    select_coating,
    validate_thickness,
)
from digitalmodel.citations import validate_citation


def test_coating_library_covers_f106_coating_families():
    """F106 library covers the planned coating families."""
    assert set(COATING_LIBRARY) == {
        CoatingType.FBE,
        CoatingType.THREE_LAYER_PE,
        CoatingType.THREE_LAYER_PP,
        CoatingType.ASPHALT_ENAMEL,
        CoatingType.COAL_TAR_ENAMEL,
        CoatingType.POLYCHLOROPRENE,
    }
    assert COATING_LIBRARY[CoatingType.FBE].nominal_thickness_mm == pytest.approx(0.5)
    assert COATING_LIBRARY[
        CoatingType.THREE_LAYER_PE
    ].min_thickness_mm == pytest.approx(2.0)
    assert COATING_LIBRARY[
        CoatingType.THREE_LAYER_PP
    ].min_thickness_mm == pytest.approx(2.5)
    assert COATING_LIBRARY[
        CoatingType.ASPHALT_ENAMEL
    ].min_thickness_mm == pytest.approx(5.0)
    assert COATING_LIBRARY[
        CoatingType.COAL_TAR_ENAMEL
    ].min_thickness_mm == pytest.approx(5.0)


def test_project_specific_library_values_do_not_publish_zero_sentinels():
    for props in COATING_LIBRARY.values():
        if props.thickness_is_project_specific:
            assert props.min_thickness_mm is None
        else:
            assert props.min_thickness_mm is not None

        if (
            props.holiday_detection_is_project_specific
            or props.holiday_detection_is_thickness_dependent
        ):
            assert props.holiday_detection_voltage_v is None
        else:
            assert props.holiday_detection_voltage_v is not None


def test_select_coating_uses_temperature_and_mechanical_protection():
    """F106 selection returns conservative defaults for cold, warm, and hot service."""
    assert select_coating(40.0, False, 20.0).coating_type is CoatingType.FBE
    assert select_coating(80.0, True, 25.0).coating_type is CoatingType.THREE_LAYER_PE
    assert select_coating(105.0, False, 20.0).coating_type is CoatingType.THREE_LAYER_PE
    assert (
        select_coating(109.9995, False, 20.0).coating_type is CoatingType.THREE_LAYER_PE
    )
    assert select_coating(110.0, True, 25.0).coating_type is CoatingType.THREE_LAYER_PP
    assert select_coating(120.0, False, 20.0).coating_type is CoatingType.THREE_LAYER_PP


def test_select_coating_returns_f106_citation_sidecar():
    result = select_coating(105.0, False, 20.0)

    assert result.coating_type is CoatingType.THREE_LAYER_PE
    assert result.citation.code_id == "dnv-rp-f106"
    assert result.citation.section.startswith("§5")
    assert "temperature" in result.rationale


def test_public_helpers_accept_common_coating_aliases():
    assert (
        validate_thickness("FBE", 0.5, project_min_thickness_mm=0.4).coating_type
        is CoatingType.FBE
    )
    assert validate_thickness("3LPE", 2.0).coating_type is CoatingType.THREE_LAYER_PE
    assert validate_thickness("PE", 2.0).coating_type is CoatingType.THREE_LAYER_PE
    assert (
        validate_thickness("THREE_LAYER_PP", 2.5).coating_type
        is CoatingType.THREE_LAYER_PP
    )
    assert validate_thickness("3LPP", 2.5).coating_type is CoatingType.THREE_LAYER_PP
    assert (
        validate_thickness("COAL_TAR_ENAMEL", 5.0).coating_type
        is CoatingType.COAL_TAR_ENAMEL
    )


@pytest.mark.parametrize(
    "coating_type",
    ["COAL_TAR_EPOXY", "POLYURETHANE", "PUR", "EPOXY"],
)
def test_non_f106_or_ambiguous_coating_names_are_rejected(coating_type):
    with pytest.raises(ValueError, match="unknown coating_type"):
        validate_thickness(coating_type, 5.0)


@pytest.mark.parametrize("coating_type", list(CoatingType))
def test_each_coating_roundtrips_through_thickness_validation(coating_type):
    props = COATING_LIBRARY[coating_type]
    kwargs = {}
    measured_thickness = props.min_thickness_mm
    if props.thickness_is_project_specific:
        kwargs["project_min_thickness_mm"] = 1.5
        measured_thickness = 1.5

    result = validate_thickness(coating_type, measured_thickness, **kwargs)

    assert result.is_valid is True
    assert result.coating_type is coating_type


def test_select_coating_rejects_out_of_library_service_temperature():
    with pytest.raises(ValueError, match="service_temp_c"):
        select_coating(180.0, True, 25.0)


def test_validate_thickness_rejects_below_minimum_with_f106_citation():
    result = validate_thickness(CoatingType.THREE_LAYER_PE, 1.99)

    assert result.is_valid is False
    assert result.minimum_thickness_mm == pytest.approx(2.0)
    assert result.citation.code_id == "dnv-rp-f106"
    assert result.citation.publisher == "DNV"
    assert result.citation.revision == "2003"
    assert result.citation.section.startswith("§5.4")


def test_project_specific_thickness_requires_project_minimum():
    with pytest.raises(ValueError, match="project_min_thickness_mm"):
        validate_thickness(CoatingType.FBE, 0.5)

    with pytest.raises(ValueError, match="project_min_thickness_mm"):
        validate_thickness(CoatingType.POLYCHLOROPRENE, 2.5)

    fbe_result = validate_thickness(
        CoatingType.FBE,
        0.5,
        project_min_thickness_mm=0.4,
    )

    result = validate_thickness(
        CoatingType.POLYCHLOROPRENE,
        2.5,
        project_min_thickness_mm=3.0,
    )

    assert fbe_result.is_valid is True
    assert result.is_valid is False
    assert result.minimum_thickness_mm == pytest.approx(3.0)


def test_holiday_detection_voltage_matches_f106_sheet_rules():
    """F106 CDS sheets use 10 kV/mm for 3LPE/3LPP and 15 kV minimum for enamel."""
    assert holiday_detection_voltage(CoatingType.THREE_LAYER_PE, 2.0) == pytest.approx(
        20000.0
    )
    assert holiday_detection_voltage(CoatingType.THREE_LAYER_PP, 3.0) == pytest.approx(
        25000.0
    )
    assert holiday_detection_voltage(CoatingType.ASPHALT_ENAMEL, 5.0) == pytest.approx(
        15000.0
    )


def test_fbe_holiday_detection_voltage_is_project_specific():
    with pytest.raises(ValueError, match="project-specific"):
        holiday_detection_voltage(CoatingType.FBE, 0.5)


def test_holiday_detection_result_cites_f106_section_6_3():
    result = holiday_detection_result(CoatingType.THREE_LAYER_PE, 2.0)

    assert result.citation.code_id == "dnv-rp-f106"
    assert result.citation.section.startswith("§6.3")


def test_package_level_f106_aliases_are_available_and_cited():
    result = cp.f106_select_coating(105.0, False, 20.0)

    assert result.coating_type is cp.F106CoatingType.THREE_LAYER_PE
    assert result.citation.code_id == "dnv-rp-f106"
    assert cp.F106SelectionResult is type(result)
    assert cp.F106_COATING_LIBRARY[cp.F106CoatingType.THREE_LAYER_PE].min_thickness_mm
    assert cp.f106_holiday_detection_voltage(
        cp.F106CoatingType.THREE_LAYER_PE,
        2.0,
    ) == pytest.approx(20000.0)


def test_f106_citation_resolves_against_repo_overlay():
    result = validate_thickness(CoatingType.THREE_LAYER_PE, 2.0)

    validate_citation(result.citation, repo_root=Path.cwd())
