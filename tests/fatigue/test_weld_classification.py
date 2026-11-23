"""Tests for weld_classification — automated weld detail classification."""

import pytest

from digitalmodel.fatigue.weld_classification import (
    classify_weld_detail,
    list_dnv_detail_categories,
    WeldDetail,
)


class TestWeldClassification:
    """Test automated weld detail classification."""

    def test_butt_weld_ground_flush_inspected(self):
        """Ground flush, inspected butt weld → DNV Class C."""
        detail = WeldDetail(
            description="Full penetration butt weld, ground flush",
            joint_type="butt",
            ground_flush=True,
            inspected=True,
            full_penetration=True,
        )
        result = classify_weld_detail(detail)
        assert result.dnv_class == "C"
        assert result.aws_category == "B"
        assert result.confidence in ("high", "medium")

    def test_as_welded_butt(self):
        """As-welded full pen butt → DNV Class D."""
        detail = WeldDetail(
            description="Butt weld as-welded",
            joint_type="butt",
            ground_flush=False,
            full_penetration=True,
        )
        result = classify_weld_detail(detail)
        assert result.dnv_class == "D"

    def test_fillet_weld_partial_pen(self):
        """Fillet weld with root failure potential → DNV Class W3."""
        detail = WeldDetail(
            description="Fillet weld root failure",
            joint_type="fillet",
            full_penetration=False,
        )
        result = classify_weld_detail(detail)
        assert result.dnv_class == "W3"

    def test_tubular_joint(self):
        """Tubular joint → DNV T-curve."""
        detail = WeldDetail(
            description="Tubular T-joint brace-to-chord",
            joint_type="tubular",
        )
        result = classify_weld_detail(detail)
        assert result.dnv_class == "T"

    def test_cruciform_full_pen(self):
        """Cruciform with full pen → DNV F."""
        detail = WeldDetail(
            description="Cruciform joint",
            joint_type="cruciform",
            full_penetration=True,
        )
        result = classify_weld_detail(detail)
        assert result.dnv_class == "F"

    def test_unknown_defaults_gracefully(self):
        """Unknown description should still return a result."""
        detail = WeldDetail(description="Some unusual geometry")
        result = classify_weld_detail(detail)
        assert result.dnv_class  # not empty
        assert result.confidence in ("high", "medium", "low")


class TestDetailCategoryListing:
    """Test the category listing function."""

    def test_list_has_standard_categories(self):
        cats = list_dnv_detail_categories()
        assert "B1" in cats
        assert "D" in cats
        assert "W3" in cats
        assert "T" in cats

    def test_list_has_descriptions(self):
        cats = list_dnv_detail_categories()
        assert len(cats["D"]) > 10  # has a meaningful description

    def test_list_has_at_least_14_categories(self):
        cats = list_dnv_detail_categories()
        assert len(cats) >= 14
