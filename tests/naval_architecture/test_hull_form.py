# ABOUTME: Tests for hull form parametric design module
# ABOUTME: Form coefficients and Series 60 validated against textbook values
"""Tests for hull_form module — form coefficients and parametric design."""

import math

import pytest


class TestFormCoefficients:
    """Block, prismatic, midship, waterplane coefficient calculations."""

    def test_block_coefficient_tanker(self):
        from digitalmodel.naval_architecture.hull_form import (
            block_coefficient,
        )

        # Tanker: V=80000 m³, L=250, B=40, T=15
        cb = block_coefficient(80000.0, 250.0, 40.0, 15.0)
        assert abs(cb - 0.533) < 0.01

    def test_block_coefficient_destroyer(self):
        from digitalmodel.naval_architecture.hull_form import (
            block_coefficient,
        )

        # DDG-51: V≈4150 m³, L=142, B=18.0, T=6.3
        cb = block_coefficient(4150.0, 142.0, 18.0, 6.3)
        assert 0.25 < cb < 0.35  # slender hull

    def test_prismatic_from_cb_cm(self):
        from digitalmodel.naval_architecture.hull_form import (
            prismatic_coefficient,
        )

        cp = prismatic_coefficient(cb=0.65, cm=0.98)
        assert abs(cp - 0.663) < 0.01

    def test_midship_coefficient(self):
        from digitalmodel.naval_architecture.hull_form import (
            midship_coefficient,
        )

        # Am = 108 m², B=12, T=9.5 → Cm = 108/(12*9.5) = 0.947
        cm = midship_coefficient(108.0, 12.0, 9.5)
        assert abs(cm - 0.947) < 0.01

    def test_waterplane_coefficient(self):
        from digitalmodel.naval_architecture.hull_form import (
            waterplane_coefficient,
        )

        # Awp = 2100 m², L=150, B=20 → Cwp = 2100/3000 = 0.70
        cwp = waterplane_coefficient(2100.0, 150.0, 20.0)
        assert abs(cwp - 0.70) < 0.01

    def test_zero_dimension_raises(self):
        from digitalmodel.naval_architecture.hull_form import (
            block_coefficient,
        )

        with pytest.raises(ValueError):
            block_coefficient(1000.0, 0.0, 10.0, 5.0)


class TestDisplacement:
    """Displacement calculations from form coefficients."""

    def test_displacement_from_cb(self):
        from digitalmodel.naval_architecture.hull_form import (
            displacement_from_cb,
        )

        # Cb=0.80, L=205, B=32, T=12, rho=1025
        disp = displacement_from_cb(0.80, 205.0, 32.0, 12.0)
        expected = 0.80 * 205.0 * 32.0 * 12.0 * 1025.0 / 1000.0
        assert abs(disp - expected) < 1.0


class TestFroudeNumber:
    """Froude number calculations."""

    def test_froude_number(self):
        from digitalmodel.naval_architecture.hull_form import froude_number

        # V=7.72 m/s, L=121.92 m → Fn ≈ 0.223
        fn = froude_number(7.72, 121.92)
        assert abs(fn - 0.223) < 0.01


class TestSeries60:
    """Series 60 residuary resistance coefficient."""

    def test_series_60_cb060(self):
        from digitalmodel.naval_architecture.hull_form import series_60_cr

        cr = series_60_cr(cb=0.60, fn=0.20)
        assert cr > 0
        assert cr < 5.0  # reasonable range for Cr × 1000

    def test_series_60_cb080(self):
        from digitalmodel.naval_architecture.hull_form import series_60_cr

        cr_full = series_60_cr(cb=0.80, fn=0.20)
        cr_fine = series_60_cr(cb=0.60, fn=0.20)
        # Fuller hull has higher residuary resistance
        assert cr_full > cr_fine

    def test_out_of_range_cb(self):
        from digitalmodel.naval_architecture.hull_form import series_60_cr

        with pytest.raises(ValueError, match="Cb"):
            series_60_cr(cb=0.40, fn=0.20)


class TestLCBEstimate:
    """LCB position estimation from Cb."""

    def test_lcb_fine_hull(self):
        from digitalmodel.naval_architecture.hull_form import lcb_from_cb

        lcb = lcb_from_cb(0.55)
        # Fine hull: LCB slightly aft
        assert lcb < 0  # aft of midship

    def test_lcb_full_hull(self):
        from digitalmodel.naval_architecture.hull_form import lcb_from_cb

        lcb = lcb_from_cb(0.80)
        # Full hull: LCB forward of midship
        assert lcb > 0


class TestRigHullValidation:
    def test_rig_type_to_hull_form(self):
        from digitalmodel.naval_architecture.hull_form import rig_type_to_hull_form

        assert rig_type_to_hull_form("drillship") == "drillship"
        assert rig_type_to_hull_form("semi_submersible") == "semi_sub"
        assert rig_type_to_hull_form("jack_up") is None

    def test_estimate_rig_hull_dimensions_by_depth(self):
        from digitalmodel.naval_architecture.hull_form import estimate_rig_hull_dimensions

        drillship = estimate_rig_hull_dimensions("drillship", water_depth_ft=10000.0)
        semi_sub = estimate_rig_hull_dimensions("semi_submersible", water_depth_ft=5000.0)

        assert drillship is not None
        assert drillship.draft_m == pytest.approx(12.0)
        assert drillship.confidence == "estimated"
        assert semi_sub is not None
        assert semi_sub.draft_m == pytest.approx(21.0)

    def test_classify_rig_hull_geometry(self):
        from digitalmodel.naval_architecture.hull_form import classify_rig_hull_geometry

        assert classify_rig_hull_geometry(238.0, 42.0) == "ship_shape"
        assert classify_rig_hull_geometry(100.0, 80.0) == "semi_sub"

    def test_validate_drillship_hull_form(self):
        from digitalmodel.naval_architecture.hull_form import validate_drilling_rig_hull_form

        result = validate_drilling_rig_hull_form(
            {
                "RIG_NAME": "VALARIS DS-17",
                "RIG_TYPE": "drillship",
                "LOA_M": 238.0,
                "BEAM_M": 42.0,
                "WATER_DEPTH_RATING_FT": 12000.0,
            }
        )

        assert result is not None
        assert result.is_valid is True
        assert result.geometry_family == "ship_shape"
        assert result.matched_family == "ship_shape"
        assert result.dimension_confidence == "estimated"

    def test_validate_semi_sub_hull_form(self):
        from digitalmodel.naval_architecture.hull_form import validate_drilling_rig_hull_form

        result = validate_drilling_rig_hull_form(
            {
                "RIG_NAME": "DEEPWATER ATLAS",
                "RIG_TYPE": "semi_submersible",
                "LOA_M": 100.0,
                "BEAM_M": 78.0,
                "WATER_DEPTH_RATING_FT": 12000.0,
            }
        )

        assert result is not None
        assert result.is_valid is True
        assert result.matched_hull_id == "SEMI-100"

    def test_validate_outlier_geometry_marks_invalid(self):
        from digitalmodel.naval_architecture.hull_form import validate_drilling_rig_hull_form

        result = validate_drilling_rig_hull_form(
            {
                "RIG_NAME": "ABAN ABRAHAM",
                "RIG_TYPE": "drillship",
                "LOA_M": 148.74,
                "BEAM_M": 136.55,
                "WATER_DEPTH_RATING_FT": 10000.0,
            }
        )

        assert result is not None
        assert result.is_valid is False
        assert "expected ship_shape" in result.reason
