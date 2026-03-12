"""Doc-verified tests for drilling riser tool passage calculations.

References:
- 144507 SpaceOut drawing — tool passage clearance
- API SPEC 16F — riser running equipment
- 2H-CAL-0015-8 — drilling riser stackup reference
"""

import pytest

from digitalmodel.drilling_riser.tool_passage import (
    annular_clearance_mm,
    minimum_riser_id_required,
    spacing_requirement_m,
)


class TestAnnularClearance:
    """Radial clearance between riser ID and tool OD."""

    def test_basic_clearance(self):
        """Riser ID 500mm, tool OD 200mm -> clearance = (500-200)/2 = 150mm."""
        result = annular_clearance_mm(riser_id_mm=500.0, tool_od_mm=200.0)
        assert result == pytest.approx(150.0, rel=0.01)

    def test_clearance_decreases_with_tool_size(self):
        """Larger tool reduces clearance."""
        c1 = annular_clearance_mm(500.0, 150.0)
        c2 = annular_clearance_mm(500.0, 300.0)
        assert c1 > c2

    def test_clearance_increases_with_riser_id(self):
        """Larger riser ID increases clearance."""
        c1 = annular_clearance_mm(400.0, 200.0)
        c2 = annular_clearance_mm(600.0, 200.0)
        assert c2 > c1

    def test_zero_clearance_when_tight_fit(self):
        """Tool OD equals riser ID -> zero clearance."""
        result = annular_clearance_mm(300.0, 300.0)
        assert result == pytest.approx(0.0, abs=0.01)


class TestMinimumRiserIdRequired:
    """Minimum riser ID for tool passage with required clearance."""

    def test_basic_minimum_id(self):
        """Tool OD 200mm + 50mm clearance -> min ID = 200 + 2*50 = 300mm."""
        result = minimum_riser_id_required(tool_od_mm=200.0, clearance_mm=50.0)
        assert result == pytest.approx(300.0, rel=0.01)

    def test_minimum_id_increases_with_tool_size(self):
        """Larger tool needs larger riser."""
        id1 = minimum_riser_id_required(150.0, 50.0)
        id2 = minimum_riser_id_required(250.0, 50.0)
        assert id2 > id1

    def test_minimum_id_increases_with_clearance(self):
        """More clearance required means larger riser."""
        id1 = minimum_riser_id_required(200.0, 25.0)
        id2 = minimum_riser_id_required(200.0, 75.0)
        assert id2 > id1

    def test_minimum_id_exceeds_tool_od(self):
        """Minimum ID must always exceed tool OD."""
        result = minimum_riser_id_required(200.0, 50.0)
        assert result > 200.0


class TestSpacingRequirement:
    """Connector spacing for tool passage."""

    def test_basic_spacing(self):
        """Connector 2.0m + tool 10.0m + margin 1.5 -> 2.0 + 10.0 + 1.5 = 13.5m."""
        result = spacing_requirement_m(
            connector_length_m=2.0, tool_length_m=10.0, margin_m=1.5
        )
        assert result == pytest.approx(13.5, rel=0.01)

    def test_spacing_increases_with_tool_length(self):
        """Longer tool requires more spacing."""
        s1 = spacing_requirement_m(2.0, 8.0)
        s2 = spacing_requirement_m(2.0, 12.0)
        assert s2 > s1

    def test_spacing_increases_with_connector(self):
        """Longer connector requires more spacing."""
        s1 = spacing_requirement_m(1.5, 10.0)
        s2 = spacing_requirement_m(3.0, 10.0)
        assert s2 > s1

    def test_spacing_exceeds_tool_length(self):
        """Total spacing must always exceed tool length alone."""
        result = spacing_requirement_m(2.0, 10.0)
        assert result > 10.0
