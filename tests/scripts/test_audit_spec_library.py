"""Tests for audit_spec_library.py -- spec library audit and classification."""
import sys
from pathlib import Path

import pytest

sys.path.insert(0, str(Path(__file__).resolve().parents[2] / "scripts"))
from audit_spec_library import (
    _compute_quality_score,
    _infer_category,
    _is_meaningful_description,
    _is_meaningful_name,
)


class TestQualityScoring:
    """Test quality score computation (0-100)."""

    def test_perfect_score_for_tier2_fast(self):
        """Tier2 fast specs should score high (>80)."""
        result = _compute_quality_score(
            schema_valid=True,
            has_comments=True,
            meaningful_name=True,
            meaningful_description=True,
            has_raw_properties=False,
            line_count=107,
        )
        assert result >= 80

    def test_low_score_for_extracted(self):
        """Auto-extracted specs should score lower (<50)."""
        result = _compute_quality_score(
            schema_valid=True,
            has_comments=False,
            meaningful_name=False,
            meaningful_description=False,
            has_raw_properties=True,
            line_count=5000,
        )
        assert result < 50

    def test_schema_invalid_caps_at_70(self):
        """Invalid schema loses 30 points."""
        result = _compute_quality_score(
            schema_valid=False,
            has_comments=True,
            meaningful_name=True,
            meaningful_description=True,
            has_raw_properties=False,
            line_count=100,
        )
        assert result <= 70

    def test_score_range(self):
        """Score must be 0-100."""
        for valid in [True, False]:
            for comments in [True, False]:
                score = _compute_quality_score(valid, comments, True, True, False, 100)
                assert 0 <= score <= 100


class TestCategoryInference:
    """Test structure category inference from path + content."""

    def test_tier2_fast_is_riser(self):
        assert _infer_category(Path("library/tier2_fast/a01_catenary_riser/spec.yml"), {"riser": {}}) == "riser"

    def test_pipeline_path(self):
        assert _infer_category(Path("pipeline/installation/floating/24in/spec.yml"), {"pipeline": {}}) == "pipeline"

    def test_c_series_is_mooring(self):
        assert _infer_category(Path("model_library/c07_metocean_buoy/spec.yml"), {"generic": {}}) == "mooring"

    def test_d_series_is_installation(self):
        assert _infer_category(Path("model_library/d02_pull_in/spec.yml"), {"generic": {}}) == "installation"

    def test_k_series_is_wind_turbine(self):
        assert _infer_category(Path("model_library/k02_turbine/spec.yml"), {"generic": {}}) == "wind_turbine"

    def test_e_series_is_installation(self):
        assert _infer_category(Path("model_library/e01_s_lay/spec.yml"), {"generic": {}}) == "installation"

    def test_h_series_is_heavy_lift(self):
        assert _infer_category(Path("model_library/h01_chinese_lantern/spec.yml"), {"generic": {}}) == "heavy_lift"

    def test_l_series_is_vessel(self):
        assert _infer_category(Path("model_library/l01_ship/spec.yml"), {"generic": {}}) == "vessel"

    def test_jumper_path(self):
        assert _infer_category(Path("jumper/sut_mm/spec.yml"), {"generic": {}}) == "jumper"

    def test_a_series_generic_is_riser(self):
        """A-series in model_library with generic schema should be riser."""
        assert _infer_category(Path("model_library/a01_catenary/spec.yml"), {"generic": {}}) == "riser"

    def test_unknown_defaults_to_other(self):
        assert _infer_category(Path("model_library/z99_unknown/spec.yml"), {"generic": {}}) == "other"


class TestDescriptionQuality:
    """Test whether description is meaningful vs auto-generated."""

    def test_extracted_description_is_not_meaningful(self):
        assert not _is_meaningful_description("Extracted from C07 Metocean buoy in deep water.yml")

    def test_good_description_is_meaningful(self):
        assert _is_meaningful_description("Simple catenary riser connected to FPSO")

    def test_empty_is_not_meaningful(self):
        assert not _is_meaningful_description("")

    def test_generic_is_not_meaningful(self):
        assert not _is_meaningful_description("generic")


class TestNameQuality:
    """Test whether name is meaningful vs auto-generated."""

    def test_temp_name_is_not_meaningful(self):
        assert not _is_meaningful_name("temp_model_001")

    def test_descriptive_name_is_meaningful(self):
        assert _is_meaningful_name("a01_catenary_riser")

    def test_single_char_is_not_meaningful(self):
        assert not _is_meaningful_name("x")
