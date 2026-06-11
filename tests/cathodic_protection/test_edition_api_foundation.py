"""Edition API foundation tests for B401 merge work."""

from __future__ import annotations

import inspect
from pathlib import Path
import warnings

from pydantic import ValidationError
import pytest


def test_default_edition_exports_from_package_facade():
    from digitalmodel.cathodic_protection import DEFAULT_EDITION, Edition

    assert DEFAULT_EDITION == "2021"
    assert "2017" in Edition.__args__
    assert "2021" in Edition.__args__


@pytest.mark.parametrize(
    ("raw", "expected"),
    [
        ("2017", "2017"),
        ("dnv-rp-b401-2017", "2017"),
        ("DNV_RP_B401_2021", "2021"),
        ("2021-05", "2021"),
    ],
)
def test_normalize_edition_accepts_supported_aliases(raw, expected):
    from digitalmodel.cathodic_protection import normalize_edition

    assert normalize_edition(raw) == expected


def test_normalize_edition_warns_and_defaults_when_missing():
    from digitalmodel.cathodic_protection import normalize_edition

    with pytest.warns(UserWarning, match="defaulting to DNV-RP-B401 2021"):
        assert normalize_edition(None) == "2021"


def test_normalize_edition_rejects_unknown_values():
    from digitalmodel.cathodic_protection import normalize_edition

    with pytest.raises(ValueError, match="Unsupported DNV-RP-B401 edition"):
        normalize_edition("2011")


@pytest.mark.parametrize(
    "function_name",
    [
        "current_demand",
        "anode_mass_requirement",
        "coating_breakdown_factor",
        "anode_resistance_slender_standoff",
        "anode_current_output",
        "equivalent_radius_from_mass",
        "number_of_anodes",
        "protected_length",
        "flush_anode_resistance",
    ],
)
def test_dnv_b401_public_functions_accept_edition_kwarg(function_name):
    from digitalmodel.cathodic_protection import dnv_rp_b401

    signature = inspect.signature(getattr(dnv_rp_b401, function_name))

    assert "edition" in signature.parameters


def test_dnv_b401_explicit_edition_preserves_current_demand_numeric():
    from digitalmodel.cathodic_protection.dnv_rp_b401 import current_demand

    assert current_demand(100.0, 0.05, 0.2, edition="2017") == pytest.approx(1.0)
    assert current_demand(100.0, 0.05, 0.2, edition="2021") == pytest.approx(1.0)


def test_dnv_b401_missing_edition_warns_and_preserves_numeric():
    from digitalmodel.cathodic_protection.dnv_rp_b401 import current_demand

    with pytest.warns(UserWarning, match="defaulting to DNV-RP-B401 2021") as warnings:
        assert current_demand(100.0, 0.05, 0.2) == pytest.approx(1.0)

    assert Path(warnings[0].filename).name == "test_edition_api_foundation.py"


def _sample_anode_sizing_input():
    from digitalmodel.cathodic_protection.anode_sizing import (
        AnodeSizingInput,
        AnodeType,
    )

    return AnodeSizingInput(
        surface_area_m2=3000.0,
        coating_breakdown_factor=0.05,
        current_density_mA_m2=100.0,
        design_life_years=25.0,
        anode_type=AnodeType.STAND_OFF,
        anode_length_m=1.0,
        anode_radius_m=0.08,
        anode_net_mass_kg=200.0,
        resistivity_ohm_m=0.30,
        anode_capacity_Ah_kg=2000.0,
        utilization_factor=0.90,
    )


def test_design_cp_system_result_carries_explicit_edition_metadata():
    from digitalmodel.cathodic_protection.anode_sizing import design_cp_system

    result = design_cp_system(_sample_anode_sizing_input(), edition="2017")

    assert result.edition_used == "2017"
    assert result.standard == "DNV-RP-B401 (Oct 2017)"


def test_design_cp_system_missing_edition_warns_and_defaults_metadata():
    from digitalmodel.cathodic_protection.anode_sizing import design_cp_system

    with pytest.warns(UserWarning, match="defaulting to DNV-RP-B401 2021") as warnings:
        result = design_cp_system(_sample_anode_sizing_input())

    assert Path(warnings[0].filename).name == "test_edition_api_foundation.py"
    assert result.edition_used == "2021"
    assert result.standard == "DNV-RP-B401 (May 2021)"


def test_design_cp_system_explicit_edition_preserves_p1_numerics():
    from digitalmodel.cathodic_protection.anode_sizing import design_cp_system

    legacy = design_cp_system(_sample_anode_sizing_input(), edition="2017")
    current = design_cp_system(_sample_anode_sizing_input(), edition="2021")

    assert current.current_demand_A == pytest.approx(legacy.current_demand_A)
    assert current.total_anode_mass_kg == pytest.approx(legacy.total_anode_mass_kg)
    assert current.number_of_anodes == legacy.number_of_anodes
    assert current.anode_resistance_ohm == pytest.approx(legacy.anode_resistance_ohm)


def test_design_cp_system_explicit_edition_emits_no_missing_edition_warning():
    from digitalmodel.cathodic_protection.anode_sizing import design_cp_system

    with warnings.catch_warnings(record=True) as captured:
        warnings.simplefilter("always")
        result = design_cp_system(_sample_anode_sizing_input(), edition="2017")

    messages = [str(warning.message) for warning in captured]
    assert not [
        message
        for message in messages
        if "defaulting to DNV-RP-B401 2021" in message
    ]
    assert result.edition_used == "2017"


def _legacy_anode_sizing_result_data(**overrides):
    data = {
        "current_demand_A": 15.0,
        "total_anode_mass_kg": 1825.0,
        "number_of_anodes": 10,
        "anode_resistance_ohm": 0.139,
        "anode_current_output_A": 1.8,
        "driving_voltage_V": 0.25,
        "anode_type": "stand_off",
        "mass_check_ok": True,
    }
    data.update(overrides)
    return data


def test_anode_sizing_result_defaults_metadata_for_legacy_dicts():
    from digitalmodel.cathodic_protection.anode_sizing import AnodeSizingResult

    result = AnodeSizingResult(**_legacy_anode_sizing_result_data())

    assert result.edition_used == "2021"
    assert result.standard == "DNV-RP-B401 (May 2021)"


def test_anode_sizing_result_treats_none_metadata_as_legacy_missing():
    from digitalmodel.cathodic_protection.anode_sizing import AnodeSizingResult

    result = AnodeSizingResult(
        **_legacy_anode_sizing_result_data(edition_used=None, standard=None)
    )

    assert result.edition_used == "2021"
    assert result.standard == "DNV-RP-B401 (May 2021)"


def test_anode_sizing_result_rejects_invalid_metadata_with_validation_error():
    from digitalmodel.cathodic_protection.anode_sizing import AnodeSizingResult

    with pytest.raises(ValidationError, match="edition_used"):
        AnodeSizingResult(**_legacy_anode_sizing_result_data(edition_used="2025"))


def test_anode_sizing_result_schema_requires_metadata_fields():
    from digitalmodel.cathodic_protection.anode_sizing import AnodeSizingResult

    required = set(AnodeSizingResult.model_json_schema()["required"])

    assert "edition_used" in required
    assert "standard" in required


def test_coating_breakdown_factors_accepts_edition_kwarg():
    from digitalmodel.cathodic_protection.coating import coating_breakdown_factors

    signature = inspect.signature(coating_breakdown_factors)

    assert "edition" in signature.parameters


def test_coating_breakdown_result_carries_explicit_edition_metadata():
    from digitalmodel.cathodic_protection.coating import (
        CoatingCategory,
        coating_breakdown_factors,
    )

    result = coating_breakdown_factors(CoatingCategory.FBE, edition="2017")

    assert result.edition_used == "2017"
    assert result.standard == "DNV-RP-B401 (Oct 2017)"


def test_coating_breakdown_missing_edition_warns_and_defaults_metadata():
    from digitalmodel.cathodic_protection.coating import (
        CoatingCategory,
        coating_breakdown_factors,
    )

    with pytest.warns(UserWarning, match="defaulting to DNV-RP-B401 2021") as warnings:
        result = coating_breakdown_factors(CoatingCategory.FBE)

    assert Path(warnings[0].filename).name == "test_edition_api_foundation.py"
    assert result.edition_used == "2021"
    assert result.standard == "DNV-RP-B401 (May 2021)"


def test_coating_breakdown_explicit_edition_preserves_p1_numerics():
    from digitalmodel.cathodic_protection.coating import (
        CoatingCategory,
        coating_breakdown_factors,
    )

    legacy = coating_breakdown_factors(CoatingCategory.FBE, edition="2017")
    current = coating_breakdown_factors(CoatingCategory.FBE, edition="2021")

    assert current.initial_factor == pytest.approx(legacy.initial_factor)
    assert current.mean_factor == pytest.approx(legacy.mean_factor)
    assert current.final_factor == pytest.approx(legacy.final_factor)


def test_coating_breakdown_result_defaults_metadata_for_legacy_dicts():
    from digitalmodel.cathodic_protection.coating import CoatingBreakdownResult

    result = CoatingBreakdownResult(
        coating_type="fbe",
        initial_factor=0.02,
        mean_factor=0.0575,
        final_factor=0.095,
        design_life_years=25.0,
    )

    assert result.edition_used == "2021"
    assert result.standard == "DNV-RP-B401 (May 2021)"


def test_coating_breakdown_result_rejects_invalid_metadata_with_validation_error():
    from digitalmodel.cathodic_protection.coating import CoatingBreakdownResult

    with pytest.raises(ValidationError, match="edition_used"):
        CoatingBreakdownResult(
            coating_type="fbe",
            initial_factor=0.02,
            mean_factor=0.0575,
            final_factor=0.095,
            design_life_years=25.0,
            edition_used="2025",
        )


def test_coating_breakdown_result_schema_requires_metadata_fields():
    from digitalmodel.cathodic_protection.coating import CoatingBreakdownResult

    required = set(CoatingBreakdownResult.model_json_schema()["required"])

    assert "edition_used" in required
    assert "standard" in required


def _sample_marine_cp_input():
    from digitalmodel.cathodic_protection.marine_cp import (
        MarineCPInput,
        Zone,
        ZoneType,
    )

    return MarineCPInput(
        structure_name="Edition Test Jacket",
        zones=[
            Zone(
                name="submerged",
                zone_type=ZoneType.SUBMERGED,
                surface_area_m2=3000.0,
                coating_breakdown_factor=0.05,
            ),
            Zone(
                name="mudline",
                zone_type=ZoneType.MUDLINE,
                surface_area_m2=800.0,
                coating_breakdown_factor=1.0,
            ),
        ],
        water_temperature_c=8.0,
        water_depth_m=100.0,
        design_life_years=25.0,
        anode_net_mass_kg=250.0,
        anode_capacity_Ah_kg=2000.0,
        utilization_factor=0.90,
    )


def test_design_marine_cp_accepts_edition_kwarg():
    from digitalmodel.cathodic_protection.marine_cp import design_marine_cp

    signature = inspect.signature(design_marine_cp)

    assert "edition" in signature.parameters


def test_design_marine_cp_result_carries_explicit_edition_metadata():
    from digitalmodel.cathodic_protection.marine_cp import design_marine_cp

    result = design_marine_cp(_sample_marine_cp_input(), edition="2017")

    assert result.edition_used == "2017"
    assert result.standard == "DNV-RP-B401 (Oct 2017)"


def test_design_marine_cp_missing_edition_warns_and_defaults_metadata():
    from digitalmodel.cathodic_protection.marine_cp import design_marine_cp

    with pytest.warns(UserWarning, match="defaulting to DNV-RP-B401 2021") as warnings:
        result = design_marine_cp(_sample_marine_cp_input())

    assert Path(warnings[0].filename).name == "test_edition_api_foundation.py"
    assert result.edition_used == "2021"
    assert result.standard == "DNV-RP-B401 (May 2021)"


def test_design_marine_cp_explicit_edition_preserves_p1_numerics():
    from digitalmodel.cathodic_protection.marine_cp import design_marine_cp

    legacy = design_marine_cp(_sample_marine_cp_input(), edition="2017")
    current = design_marine_cp(_sample_marine_cp_input(), edition="2021")

    assert current.total_current_demand_A == pytest.approx(
        legacy.total_current_demand_A
    )
    assert current.total_anode_mass_kg == pytest.approx(legacy.total_anode_mass_kg)
    assert current.number_of_anodes == legacy.number_of_anodes
    assert current.zone_demands == legacy.zone_demands


def test_marine_cp_result_defaults_metadata_for_legacy_dicts():
    from digitalmodel.cathodic_protection.marine_cp import MarineCPResult

    result = MarineCPResult(
        structure_name="legacy",
        total_current_demand_A=31.0,
        total_anode_mass_kg=3771.0,
        number_of_anodes=16,
        zone_demands=[],
        design_life_years=25.0,
    )

    assert result.edition_used == "2021"
    assert result.standard == "DNV-RP-B401 (May 2021)"


def test_marine_cp_result_rejects_invalid_metadata_with_validation_error():
    from digitalmodel.cathodic_protection.marine_cp import MarineCPResult

    with pytest.raises(ValidationError, match="edition_used"):
        MarineCPResult(
            structure_name="invalid",
            total_current_demand_A=31.0,
            total_anode_mass_kg=3771.0,
            number_of_anodes=16,
            zone_demands=[],
            design_life_years=25.0,
            edition_used="2025",
        )


def test_marine_cp_result_schema_requires_metadata_fields():
    from digitalmodel.cathodic_protection.marine_cp import MarineCPResult

    required = set(MarineCPResult.model_json_schema()["required"])

    assert "edition_used" in required
    assert "standard" in required


def _sample_marine_structure_zones():
    from digitalmodel.cathodic_protection.marine_structure_cp import (
        ExposureZone,
        StructuralZone,
    )

    return [
        StructuralZone(
            zone_name="submerged_legs",
            exposure_zone=ExposureZone.SUBMERGED,
            surface_area_m2=2000.0,
            coating_breakdown_factor=0.05,
        ),
        StructuralZone(
            zone_name="buried_piles",
            exposure_zone=ExposureZone.BURIED_MUDLINE,
            surface_area_m2=500.0,
            coating_breakdown_factor=1.0,
        ),
    ]


def test_marine_structure_current_demand_accepts_edition_kwarg():
    from digitalmodel.cathodic_protection.marine_structure_cp import (
        marine_structure_current_demand,
    )

    signature = inspect.signature(marine_structure_current_demand)

    assert "edition" in signature.parameters


def test_marine_structure_result_carries_explicit_edition_metadata():
    from digitalmodel.cathodic_protection.marine_structure_cp import (
        marine_structure_current_demand,
    )

    result = marine_structure_current_demand(
        _sample_marine_structure_zones(),
        edition="2017",
    )

    assert result.edition_used == "2017"
    assert result.standard == "DNV-RP-B401 (Oct 2017)"


def test_marine_structure_missing_edition_warns_and_defaults_metadata():
    from digitalmodel.cathodic_protection.marine_structure_cp import (
        marine_structure_current_demand,
    )

    with pytest.warns(UserWarning, match="defaulting to DNV-RP-B401 2021") as warnings:
        result = marine_structure_current_demand(_sample_marine_structure_zones())

    assert Path(warnings[0].filename).name == "test_edition_api_foundation.py"
    assert result.edition_used == "2021"
    assert result.standard == "DNV-RP-B401 (May 2021)"


def test_marine_structure_explicit_edition_preserves_p1_numerics():
    from digitalmodel.cathodic_protection.marine_structure_cp import (
        marine_structure_current_demand,
    )

    legacy = marine_structure_current_demand(
        _sample_marine_structure_zones(), edition="2017"
    )
    current = marine_structure_current_demand(
        _sample_marine_structure_zones(), edition="2021"
    )

    assert current.total_initial_current_A == pytest.approx(
        legacy.total_initial_current_A
    )
    assert current.total_mean_current_A == pytest.approx(legacy.total_mean_current_A)
    assert current.total_final_current_A == pytest.approx(legacy.total_final_current_A)
    assert current.total_anode_mass_kg == pytest.approx(legacy.total_anode_mass_kg)
    assert current.zone_details == legacy.zone_details


def test_marine_structure_result_defaults_metadata_for_legacy_dicts():
    from digitalmodel.cathodic_protection.marine_structure_cp import MarineCPResult

    result = MarineCPResult(
        total_initial_current_A=40.0,
        total_mean_current_A=20.0,
        total_final_current_A=24.0,
        total_anode_mass_kg=2433.33,
        number_of_anodes=13,
        zone_details=[],
    )

    assert result.edition_used == "2021"
    assert result.standard == "DNV-RP-B401 (May 2021)"


def test_marine_structure_result_rejects_invalid_metadata_with_validation_error():
    from digitalmodel.cathodic_protection.marine_structure_cp import MarineCPResult

    with pytest.raises(ValidationError, match="edition_used"):
        MarineCPResult(
            total_initial_current_A=40.0,
            total_mean_current_A=20.0,
            total_final_current_A=24.0,
            total_anode_mass_kg=2433.33,
            number_of_anodes=13,
            zone_details=[],
            edition_used="2025",
        )


def test_marine_structure_result_schema_requires_metadata_fields():
    from digitalmodel.cathodic_protection.marine_structure_cp import MarineCPResult

    required = set(MarineCPResult.model_json_schema()["required"])

    assert "edition_used" in required
    assert "standard" in required
