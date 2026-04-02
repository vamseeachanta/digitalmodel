"""Tests for digitalmodel.orcaflex.mooring_design module."""

import math

import pytest

from digitalmodel.orcaflex.mooring_design import (
    CatenaryResult,
    MOORING_MATERIAL_LIBRARY,
    MooringLineDesign,
    MooringLineSegment,
    SpreadMooringConfig,
    TurretMooringConfig,
    calculate_pretension,
    estimate_line_length,
    solve_catenary,
)


class TestCatenary:
    """Tests for catenary equation solver."""

    def test_basic_catenary_solution(self):
        """Catenary should produce valid tensions and geometry."""
        result = solve_catenary(
            water_depth=500.0,
            line_length=1200.0,
            submerged_weight_per_m=1000.0,
        )
        assert isinstance(result, CatenaryResult)
        assert result.horizontal_tension > 0
        assert result.top_tension > result.horizontal_tension
        assert result.top_angle > 0
        assert result.suspended_length > 0

    def test_catenary_grounded_length(self):
        """With long line, some should be grounded on seabed."""
        result = solve_catenary(
            water_depth=200.0,
            line_length=1000.0,
            submerged_weight_per_m=800.0,
        )
        assert result.grounded_length > 0

    def test_catenary_short_line_raises(self):
        """Line shorter than water depth should raise ValueError."""
        with pytest.raises(ValueError, match="must exceed"):
            solve_catenary(
                water_depth=1000.0,
                line_length=500.0,
                submerged_weight_per_m=500.0,
            )

    def test_catenary_with_pretension(self):
        """Solving with pretension should give consistent result."""
        result = solve_catenary(
            water_depth=500.0,
            line_length=1500.0,
            submerged_weight_per_m=1000.0,
            pretension=800.0,
        )
        assert result.horizontal_tension > 0
        assert result.top_tension > 0


class TestMooringLineDesign:
    """Tests for mooring line preliminary design."""

    def test_default_design_total_length(self):
        """Default design should have reasonable total length."""
        design = MooringLineDesign()
        assert design.total_length > 2000  # 150 + 1800 + 300 = 2250

    def test_estimate_catenary(self):
        """Catenary estimate should return valid result."""
        design = MooringLineDesign(
            water_depth=1500.0,
            target_pretension=1500.0,
        )
        result = design.estimate_catenary()
        assert result.horizontal_tension > 0

    def test_mbl_check(self):
        """MBL check should return utilisation ratios."""
        design = MooringLineDesign()
        utils = design.check_mbl(max_tension_kn=3000.0)
        for key, val in utils.items():
            assert 0 < val < 1.0  # should not exceed MBL at 3000 kN


class TestSpreadMooring:
    """Tests for spread mooring layout."""

    def test_layout_line_count(self):
        """Layout should generate correct number of lines."""
        config = SpreadMooringConfig(num_lines=12, num_groups=4)
        layout = config.generate_layout()
        assert len(layout) == 12

    def test_layout_azimuths_cover_360(self):
        """Lines should span approximately 360 degrees."""
        config = SpreadMooringConfig(num_lines=8, num_groups=4)
        layout = config.generate_layout()
        azimuths = [l["azimuth_deg"] for l in layout]
        assert min(azimuths) < 10
        assert max(azimuths) > 260

    def test_anchor_positions_nonzero(self):
        """Anchor positions should be at non-zero distances."""
        config = SpreadMooringConfig(num_lines=4, num_groups=4)
        layout = config.generate_layout()
        for line in layout:
            assert line["anchor_radius"] > 0


class TestTurretMooring:
    """Tests for turret mooring layout."""

    def test_turret_layout_count(self):
        """Turret layout should have correct number of lines."""
        config = TurretMooringConfig(num_lines=9)
        layout = config.generate_layout()
        assert len(layout) == 9

    def test_turret_even_spacing(self):
        """Lines should be evenly spaced around 360 degrees."""
        config = TurretMooringConfig(num_lines=6)
        layout = config.generate_layout()
        azimuths = sorted([l["azimuth_deg"] for l in layout])
        spacing = [azimuths[i + 1] - azimuths[i] for i in range(len(azimuths) - 1)]
        for s in spacing:
            assert s == pytest.approx(60.0, abs=0.1)


class TestUtilityFunctions:
    """Tests for utility functions."""

    def test_estimate_line_length(self):
        """Line length estimate should be water_depth * scope_ratio."""
        length = estimate_line_length(water_depth=1000.0, scope_ratio=3.0, catenary=False)
        assert length == pytest.approx(3000.0)

    def test_estimate_line_length_with_catenary(self):
        """With catenary flag, should add 10% extra."""
        length = estimate_line_length(water_depth=1000.0, scope_ratio=3.0, catenary=True)
        assert length == pytest.approx(3300.0)

    def test_material_library_has_properties(self):
        """Material library should have MBL and stiffness."""
        chain = MOORING_MATERIAL_LIBRARY["R4_84mm_chain"]
        assert chain.mbl > 0
        assert chain.axial_stiffness > 0
        assert chain.submerged_weight > 0
