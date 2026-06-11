"""Tests for API RP 2SM synthetic fiber rope mooring helpers."""

from __future__ import annotations

from pathlib import Path

import pytest
import yaml

import digitalmodel.orcaflex as ofx
import digitalmodel.orcaflex.synthetic_rope_design as synthetic_rope_design
from digitalmodel.citations import validate_citation
from digitalmodel.orcaflex import (
    MOORING_MATERIAL_LIBRARY,
    SegmentMaterial,
    SyntheticRopeProperties,
    SyntheticRopeStiffness,
)
from digitalmodel.orcaflex.synthetic_rope_design import (
    CompressionFatigueSeverity,
    LoadHistory,
    ManufacturerSpec,
    axial_compression_fatigue_check,
    axial_stiffness_for_load_history,
    creep_elongation,
    qa_program,
    select_rope_material,
    select_rope_material_result,
)


def _fixture_path() -> Path:
    return (
        Path(__file__).resolve().parent / "fixtures" / "synthetic_rope_load_history.csv"
    )


def _repo_root() -> Path:
    return Path(__file__).resolve().parents[2]


def _profile_config_path() -> Path:
    return (
        Path(synthetic_rope_design.__file__).resolve().parent
        / "data"
        / "synthetic_rope_profiles.yml"
    )


def test_synthetic_profile_constants_are_packaged_yaml_backed():
    payload = yaml.safe_load(_profile_config_path().read_text(encoding="utf-8"))

    assert payload["default_profile_keys"]["polyester"] == "160mm_polyester"
    assert payload["fatigue_event_thresholds"]["limited"]["critical"] == 1_000_000
    assert {"160mm_polyester", "220mm_polyester"} <= set(payload["profiles"])


def test_synthetic_stiffness_models_are_attached_to_polyester_library_entry():
    props = MOORING_MATERIAL_LIBRARY["160mm_polyester"]

    assert isinstance(props.synthetic_stiffness, SyntheticRopeStiffness)
    assert isinstance(props.synthetic_properties, SyntheticRopeProperties)
    assert props.synthetic_stiffness.static_stiffness_kn > 0
    assert props.synthetic_stiffness.dynamic_storm_stiffness_kn > (
        props.synthetic_stiffness.post_installation_stiffness_kn
    )
    assert props.synthetic_stiffness.mean_load_factor > 1.0
    assert props.synthetic_properties.creep_rate_pct_per_decade > 0
    assert props.synthetic_properties.bend_over_sheave_min_dia_ratio >= 8


def test_synthetic_property_library_covers_planned_fiber_families():
    synthetic_materials = {
        props.material
        for props in MOORING_MATERIAL_LIBRARY.values()
        if props.synthetic_stiffness is not None
    }

    assert {
        SegmentMaterial.POLYESTER,
        SegmentMaterial.HMPE,
        SegmentMaterial.NYLON,
        SegmentMaterial.ARAMID,
    } <= synthetic_materials


def test_high_tenacity_synthetic_strengths_exceed_polyester_default():
    polyester = MOORING_MATERIAL_LIBRARY["160mm_polyester"]
    hmpe = MOORING_MATERIAL_LIBRARY["180mm_hmpe"]
    aramid = MOORING_MATERIAL_LIBRARY["160mm_aramid"]

    assert hmpe.mbl > polyester.mbl
    assert aramid.mbl > polyester.mbl


def test_select_rope_material_returns_polyester_for_canonical_deepwater_fps():
    material = select_rope_material(
        service="fpso_taut_mooring",
        water_depth_m=2000.0,
        target_life_years=25.0,
        motion_envelope={"max_offset_pct": 8.0, "low_tension_event_count": 0},
    )

    assert material is SegmentMaterial.POLYESTER


def test_select_rope_material_result_emits_section_4_citation():
    result = select_rope_material_result(
        service="fpso_taut_mooring",
        water_depth_m=2000.0,
        target_life_years=25.0,
        motion_envelope={"max_offset_pct": 8.0},
    )

    assert result.material is SegmentMaterial.POLYESTER
    assert result.citation.code_id == "api-rp-2sm"
    assert result.citation.section.startswith("§4")
    validate_citation(result.citation, repo_root=_repo_root())


def test_deepwater_short_life_selection_does_not_fall_back_to_nylon():
    result = select_rope_material_result(
        service="permanent_mooring",
        water_depth_m=2000.0,
        target_life_years=10.0,
        motion_envelope={},
    )

    assert result.material is SegmentMaterial.POLYESTER
    assert "deepwater" in result.basis


def test_long_life_shallow_permanent_selection_avoids_nylon_fallback():
    result = select_rope_material_result(
        service="permanent_mooring",
        water_depth_m=800.0,
        target_life_years=35.0,
        motion_envelope={},
    )

    assert result.material is SegmentMaterial.POLYESTER
    assert "long-life" in result.basis


def test_select_rope_material_rejects_negative_low_tension_event_count():
    with pytest.raises(ValueError, match="low_tension_event_count"):
        select_rope_material(
            service="fpso_taut_mooring",
            water_depth_m=2000.0,
            target_life_years=25.0,
            motion_envelope={"low_tension_event_count": -1},
        )


@pytest.mark.parametrize("value", [1.2, None, "1.2"])
def test_select_rope_material_rejects_non_integer_low_tension_event_count(value):
    with pytest.raises(ValueError, match="low_tension_event_count"):
        select_rope_material(
            service="fpso_taut_mooring",
            water_depth_m=2000.0,
            target_life_years=25.0,
            motion_envelope={"low_tension_event_count": value},
        )


def test_load_history_csv_drives_mean_load_stiffness_selection():
    history = LoadHistory.from_csv(_fixture_path())
    stiffness = axial_stiffness_for_load_history(SegmentMaterial.POLYESTER, history)

    assert history.mean_load_pct_mbl == pytest.approx(20.0)
    assert stiffness.value == pytest.approx(270000.0)
    assert stiffness.units == "kN"
    assert stiffness.citation.code_id == "api-rp-2sm"
    assert stiffness.citation.section.startswith("§5.4")


def test_dynamic_tension_range_moves_stiffness_toward_storm_value():
    mild = LoadHistory(
        mean_load_pct_mbl=20.0,
        dynamic_tension_range_pct_mbl=5.0,
        low_tension_events=0,
    )
    storm = LoadHistory(
        mean_load_pct_mbl=20.0,
        dynamic_tension_range_pct_mbl=55.0,
        low_tension_events=0,
    )

    mild_stiffness = axial_stiffness_for_load_history(SegmentMaterial.POLYESTER, mild)
    storm_stiffness = axial_stiffness_for_load_history(SegmentMaterial.POLYESTER, storm)

    assert mild_stiffness.value == pytest.approx(270000.0)
    assert storm_stiffness.value == pytest.approx(320000.0)
    assert storm_stiffness.value > mild_stiffness.value


def test_profile_key_selects_diameter_specific_polyester_stiffness():
    history = LoadHistory.from_csv(_fixture_path())
    default = axial_stiffness_for_load_history(SegmentMaterial.POLYESTER, history)
    large_diameter = axial_stiffness_for_load_history("220mm_polyester", history)

    assert default.value == pytest.approx(270000.0)
    assert large_diameter.value == pytest.approx(540000.0)


def test_load_history_csv_reports_missing_columns(tmp_path):
    path = tmp_path / "bad_load_history.csv"
    path.write_text(
        "mean_load_pct_mbl,low_tension_event\n20.0,false\n",
        encoding="utf-8",
    )

    with pytest.raises(ValueError, match="dynamic_tension_range_pct_mbl"):
        LoadHistory.from_csv(path)


def test_load_history_csv_rejects_unknown_low_tension_tokens(tmp_path):
    path = tmp_path / "bad_low_tension_token.csv"
    path.write_text(
        "time_s,mean_load_pct_mbl,dynamic_tension_range_pct_mbl,low_tension_event\n"
        "0,20.0,10.0,maybe\n",
        encoding="utf-8",
    )

    with pytest.raises(ValueError, match="low_tension_event"):
        LoadHistory.from_csv(path)


def test_polyester_creep_at_20_pct_mbl_for_25_years_matches_local_example():
    result = creep_elongation(
        SegmentMaterial.POLYESTER,
        mean_load_pct_mbl=20.0,
        service_years=25.0,
    )

    assert result.value == pytest.approx(0.875, rel=0.10)
    assert result.units == "%"
    assert result.citation.code_id == "api-rp-2sm"
    assert result.citation.section.startswith("§5.5")


def test_axial_compression_fatigue_flags_million_low_tension_events_critical():
    verdict = axial_compression_fatigue_check(
        SegmentMaterial.POLYESTER,
        low_tension_events=1_000_000,
    )

    assert verdict.severity is CompressionFatigueSeverity.CRITICAL
    assert verdict.low_tension_events == 1_000_000
    assert verdict.citation.code_id == "api-rp-2sm"
    assert verdict.citation.section.startswith("§5.6")


def test_qa_program_covers_manufacturing_installation_and_citations_resolve():
    spec = ManufacturerSpec(
        manufacturer="Example Rope Works",
        material=SegmentMaterial.POLYESTER,
        has_prototype_test=True,
        has_splice_qualification=True,
        has_traceability=True,
        installation_tension_recorded=False,
    )

    items = qa_program(SegmentMaterial.POLYESTER, spec)

    assert {item.stage for item in items} >= {"manufacturing", "installation"}
    assert any(not item.is_satisfied for item in items)
    for item in items:
        assert item.citation.code_id == "api-rp-2sm"
        validate_citation(item.citation, repo_root=_repo_root())


def test_package_level_synthetic_rope_helpers_are_exported():
    assert (
        ofx.select_rope_material(
            "fpso_taut_mooring",
            water_depth_m=2000.0,
            target_life_years=25.0,
            motion_envelope={},
        )
        is ofx.SegmentMaterial.POLYESTER
    )
    assert (
        ofx.select_rope_material_result(
            "fpso_taut_mooring",
            water_depth_m=2000.0,
            target_life_years=25.0,
            motion_envelope={},
        ).material
        is ofx.SegmentMaterial.POLYESTER
    )
    assert ofx.CompressionFatigueSeverity.CRITICAL.value == "critical"
