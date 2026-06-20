"""Tests for the unified riser input schema (issue #809).

Covers: discriminated-union validation of all five riser types, loud failure
on invalid / missing input, legacy adapter mapping, and YAML round-trip /
example-file loading.
"""

from pathlib import Path

import pytest
import yaml
from pydantic import ValidationError

from digitalmodel.orcaflex.riser_config import (
    LazyWaveDesignInput,
    SCRDesignInput,
    TTRDesignInput,
)
from digitalmodel.orcaflex.riser_input_schema import (
    DrillingInput,
    FlexibleInput,
    SCRInput,
    SLWRInput,
    TTRInput,
    UnifiedRiserType,
    load_riser_input,
    validate_riser_input,
)

EXAMPLES_DIR = Path(__file__).resolve().parents[2] / "examples" / "riser_input"

ALL_EXAMPLES = {
    "SCR": (EXAMPLES_DIR / "riser_scr.yml", SCRInput),
    "SLWR": (EXAMPLES_DIR / "riser_slwr.yml", SLWRInput),
    "TTR": (EXAMPLES_DIR / "riser_ttr.yml", TTRInput),
    "drilling": (EXAMPLES_DIR / "riser_drilling.yml", DrillingInput),
    "flexible": (EXAMPLES_DIR / "riser_flexible.yml", FlexibleInput),
}


# ---------------------------------------------------------------------------
# Discriminated-union validation
# ---------------------------------------------------------------------------

class TestDiscriminatedUnion:
    """One schema validates all five riser types."""

    @pytest.mark.parametrize("rtype,expected_cls", [
        (UnifiedRiserType.SCR, SCRInput),
        (UnifiedRiserType.SLWR, SLWRInput),
        (UnifiedRiserType.TTR, TTRInput),
        (UnifiedRiserType.DRILLING, DrillingInput),
        (UnifiedRiserType.FLEXIBLE, FlexibleInput),
    ])
    def test_minimal_input_resolves_to_correct_variant(self, rtype, expected_cls):
        """Discriminator selects the right concrete model."""
        model = validate_riser_input({"riser_type": rtype.value})
        assert isinstance(model, expected_cls)
        assert model.riser_type == rtype

    def test_defaults_populate_shared_blocks(self):
        """Shared blocks get sensible defaults when omitted."""
        model = validate_riser_input({"riser_type": "SCR"})
        assert model.geometry.water_depth == 1500.0
        assert model.material.outer_diameter == pytest.approx(0.2731)
        assert model.analysis.run_statics is True


# ---------------------------------------------------------------------------
# Loud failure on invalid input
# ---------------------------------------------------------------------------

class TestInvalidInputFailsLoudly:
    def test_missing_discriminator_raises(self):
        with pytest.raises(ValidationError):
            validate_riser_input({"name": "no_type"})

    def test_unknown_riser_type_raises(self):
        with pytest.raises(ValidationError):
            validate_riser_input({"riser_type": "banana"})

    def test_bad_type_for_field_raises(self):
        with pytest.raises(ValidationError):
            validate_riser_input(
                {"riser_type": "SCR", "geometry": {"water_depth": "deep"}}
            )

    def test_out_of_range_value_raises(self):
        # SCR hang-off angle is bounded 0..30
        with pytest.raises(ValidationError):
            validate_riser_input(
                {"riser_type": "SCR", "hang_off_angle_from_vertical": 99.0}
            )

    def test_negative_water_depth_raises(self):
        with pytest.raises(ValidationError):
            validate_riser_input(
                {"riser_type": "TTR", "geometry": {"water_depth": -100.0}}
            )

    def test_unknown_extra_field_rejected(self):
        with pytest.raises(ValidationError):
            validate_riser_input({"riser_type": "SCR", "bogus_field": 1})


# ---------------------------------------------------------------------------
# Legacy adapters
# ---------------------------------------------------------------------------

class TestLegacyAdapters:
    def test_scr_maps_to_legacy_and_computes(self):
        model = SCRInput(hang_off_angle_from_vertical=12.0)
        legacy = model.to_legacy()
        assert isinstance(legacy, SCRDesignInput)
        assert legacy.water_depth == model.geometry.water_depth
        assert legacy.hang_off_angle_from_vertical == 12.0
        # Legacy calculator still runs through the adapter
        geom = legacy.calculate_geometry()
        assert geom["top_tension_kN"] > 0

    def test_slwr_maps_to_lazy_wave_legacy(self):
        model = SLWRInput(buoyancy_od=0.65)
        legacy = model.to_legacy()
        assert isinstance(legacy, LazyWaveDesignInput)
        assert legacy.buoyancy_od == 0.65
        sizing = legacy.size_buoyancy_section()
        assert sizing["total_riser_length_m"] > 0

    def test_flexible_maps_to_lazy_wave_legacy(self):
        model = FlexibleInput()
        legacy = model.to_legacy()
        assert isinstance(legacy, LazyWaveDesignInput)

    def test_ttr_maps_to_legacy_and_computes(self):
        model = TTRInput(overpull_pct=50.0)
        legacy = model.to_legacy()
        assert isinstance(legacy, TTRDesignInput)
        tension = legacy.calculate_top_tension()
        assert tension["overpull_factor"] == pytest.approx(1.5)

    def test_drilling_maps_to_ttr_legacy(self):
        model = DrillingInput(mud_density=1500.0, dynamic_factor=1.3)
        legacy = model.to_legacy()
        assert isinstance(legacy, TTRDesignInput)
        stroke = legacy.calculate_stroke()
        assert stroke["design_stroke_m"] > 0

    def test_material_block_maps_pipe_properties(self):
        model = SCRInput(material={"outer_diameter": 0.5, "wall_thickness": 0.03})
        pipe = model.material.to_pipe_properties()
        assert pipe.outer_diameter == 0.5
        assert pipe.inner_diameter == pytest.approx(0.44, abs=1e-9)


# ---------------------------------------------------------------------------
# YAML loading + round-trip
# ---------------------------------------------------------------------------

class TestYamlExamples:
    @pytest.mark.parametrize("key", list(ALL_EXAMPLES))
    def test_example_file_loads_and_validates(self, key):
        path, cls = ALL_EXAMPLES[key]
        assert path.exists(), f"missing example {path}"
        model = load_riser_input(path)
        assert isinstance(model, cls)

    @pytest.mark.parametrize("key", list(ALL_EXAMPLES))
    def test_yaml_round_trip(self, key, tmp_path):
        path, cls = ALL_EXAMPLES[key]
        model = load_riser_input(path)
        # dump -> reload -> identical model
        out = tmp_path / "roundtrip.yml"
        out.write_text(yaml.safe_dump(model.model_dump(mode="json")))
        reloaded = load_riser_input(out)
        assert reloaded == model

    def test_non_mapping_yaml_raises(self, tmp_path):
        bad = tmp_path / "bad.yml"
        bad.write_text("- just\n- a\n- list\n")
        with pytest.raises(ValueError):
            load_riser_input(bad)
