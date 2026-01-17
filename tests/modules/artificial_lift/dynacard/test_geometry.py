# ABOUTME: Unit tests for card geometry analysis module.
# ABOUTME: Validates area, perimeter, centroid, and zoned area calculations.

import math
import pytest
import numpy as np
from pathlib import Path

from digitalmodel.modules.artificial_lift.dynacard.data_loader import (
    load_from_json_file,
)
from digitalmodel.modules.artificial_lift.dynacard.geometry import (
    CardGeometryCalculator,
    calculate_card_geometry,
    calculate_card_perimeter,
)
from digitalmodel.modules.artificial_lift.dynacard.models import (
    DynacardAnalysisContext,
    CardData,
    RodSection,
    PumpProperties,
    SurfaceUnit,
    CardGeometryAnalysis,
)
from digitalmodel.modules.artificial_lift.dynacard.exceptions import ValidationError


# Test data directory
TEST_DATA_DIR = Path(__file__).parent / "testdata"


class TestCardGeometryCalculator:
    """Tests for the CardGeometryCalculator class."""

    def test_calculator_initialization(self):
        """Test calculator can be initialized with valid context."""
        context = self._create_test_context()
        calculator = CardGeometryCalculator(context)
        assert calculator.ctx == context
        assert calculator.result is not None

    def test_calculate_rectangle_area(self):
        """Test area calculation for rectangular card (simple case)."""
        context = self._create_test_context()
        # Create a simple rectangle: 100 wide, 10000 tall
        # Vertices: (0,0), (100,0), (100,10000), (0,10000)
        context.surface_card = CardData(
            position=[0, 100, 100, 0],
            load=[0, 0, 10000, 10000],
        )
        calculator = CardGeometryCalculator(context)
        result = calculator.calculate()

        # Area = 100 * 10000 = 1,000,000 in-lbs
        assert abs(result.area - 1000000.0) < 1.0

    def test_calculate_triangle_area(self):
        """Test area calculation for triangular card."""
        context = self._create_test_context()
        # Create a triangle: base=100, height=10000
        # Vertices: (0,0), (100,0), (50,10000)
        context.surface_card = CardData(
            position=[0, 100, 50],
            load=[0, 0, 10000],
        )
        calculator = CardGeometryCalculator(context)
        result = calculator.calculate()

        # Area = 0.5 * base * height = 0.5 * 100 * 10000 = 500,000
        assert abs(result.area - 500000.0) < 1.0

    def test_calculate_perimeter_rectangle(self):
        """Test perimeter calculation for rectangle."""
        context = self._create_test_context()
        # Rectangle: 100 wide, 10000 tall
        context.surface_card = CardData(
            position=[0, 100, 100, 0],
            load=[0, 0, 10000, 10000],
        )
        calculator = CardGeometryCalculator(context)
        result = calculator.calculate()

        # Perimeter = 2*(100 + 10000) = 20200
        assert abs(result.perimeter - 20200.0) < 1.0

    def test_calculate_perimeter_square(self):
        """Test perimeter calculation for unit square."""
        context = self._create_test_context()
        # Unit square: 100x100
        context.surface_card = CardData(
            position=[0, 100, 100, 0],
            load=[0, 0, 100, 100],
        )
        calculator = CardGeometryCalculator(context)
        result = calculator.calculate()

        # Perimeter = 4 * 100 = 400
        assert abs(result.perimeter - 400.0) < 0.1

    def test_calculate_position_range(self):
        """Test position range calculation."""
        context = self._create_test_context()
        context.surface_card = CardData(
            position=[10, 110, 110, 10],
            load=[5000, 5000, 15000, 15000],
        )
        calculator = CardGeometryCalculator(context)
        result = calculator.calculate()

        assert result.position_range == 100.0

    def test_calculate_load_range(self):
        """Test load range calculation."""
        context = self._create_test_context()
        context.surface_card = CardData(
            position=[0, 100, 100, 0],
            load=[5000, 5000, 15000, 15000],
        )
        calculator = CardGeometryCalculator(context)
        result = calculator.calculate()

        assert result.load_range == 10000.0

    def test_calculate_centroid_rectangle(self):
        """Test centroid calculation for rectangle."""
        context = self._create_test_context()
        context.surface_card = CardData(
            position=[0, 100, 100, 0],
            load=[0, 0, 10000, 10000],
        )
        calculator = CardGeometryCalculator(context)
        result = calculator.calculate()

        # Centroid should be at center: (50, 5000)
        assert abs(result.centroid_position - 50.0) < 0.1
        assert abs(result.centroid_load - 5000.0) < 0.1

    def test_calculate_centroid_offset_rectangle(self):
        """Test centroid calculation for offset rectangle."""
        context = self._create_test_context()
        # Rectangle offset: position 10-110, load 5000-15000
        context.surface_card = CardData(
            position=[10, 110, 110, 10],
            load=[5000, 5000, 15000, 15000],
        )
        calculator = CardGeometryCalculator(context)
        result = calculator.calculate()

        # Centroid: (60, 10000)
        assert abs(result.centroid_position - 60.0) < 0.1
        assert abs(result.centroid_load - 10000.0) < 0.1

    def test_zoned_areas_symmetric(self):
        """Test zoned area distribution for symmetric card."""
        context = self._create_test_context()
        # Create card with points evenly distributed
        n = 100
        angles = np.linspace(0, 2 * np.pi, n, endpoint=False)
        position = 50 + 50 * np.cos(angles)
        load = 10000 + 5000 * np.sin(angles)

        context.surface_card = CardData(
            position=position.tolist(),
            load=load.tolist(),
        )
        calculator = CardGeometryCalculator(context)
        result = calculator.calculate()

        # For symmetric distribution, each quadrant should be ~25%
        assert len(result.zone_area_fractions) == 4
        for frac in result.zone_area_fractions:
            assert abs(frac - 0.25) < 0.1

    def test_zoned_areas_sum_to_total(self):
        """Test that zoned areas sum to total area."""
        context = self._create_test_context()
        context.surface_card = CardData(
            position=[0, 100, 100, 0],
            load=[0, 0, 10000, 10000],
        )
        calculator = CardGeometryCalculator(context)
        result = calculator.calculate()

        zone_sum = sum(result.zone_areas)
        # Zone areas should approximately sum to total area
        assert abs(zone_sum - result.area) < result.area * 0.1

    def test_zone_fractions_sum_to_one(self):
        """Test that zone fractions sum to 1.0."""
        context = self._create_test_context()
        context.surface_card = CardData(
            position=[0, 50, 100, 50],
            load=[5000, 10000, 15000, 10000],
        )
        calculator = CardGeometryCalculator(context)
        result = calculator.calculate()

        fraction_sum = sum(result.zone_area_fractions)
        assert abs(fraction_sum - 1.0) < 0.001

    def test_empty_card_raises_validation_error(self):
        """Test that empty card raises ValidationError."""
        context = self._create_test_context()
        context.surface_card = CardData(position=[], load=[])
        calculator = CardGeometryCalculator(context)

        with pytest.raises(ValidationError) as exc_info:
            calculator.calculate()

        assert "3" in exc_info.value.message or "data" in exc_info.value.message.lower()

    def test_insufficient_points_raises_validation_error(self):
        """Test that card with less than 3 points raises ValidationError."""
        context = self._create_test_context()
        context.surface_card = CardData(
            position=[0, 100],
            load=[5000, 15000],
        )
        calculator = CardGeometryCalculator(context)

        with pytest.raises(ValidationError) as exc_info:
            calculator.calculate()

        assert "3" in exc_info.value.message

    def test_mismatched_arrays_raises_validation_error(self):
        """Test that mismatched position/load arrays raises ValidationError."""
        context = self._create_test_context()
        context.surface_card = CardData(
            position=[0, 100, 100],
            load=[5000, 5000],  # Missing one value
        )
        calculator = CardGeometryCalculator(context)

        with pytest.raises(ValidationError) as exc_info:
            calculator.calculate()

        assert "length" in exc_info.value.message.lower()

    def test_convenience_function_handles_errors_gracefully(self):
        """Test that convenience function returns zeros on error."""
        context = self._create_test_context()
        context.surface_card = CardData(position=[], load=[])

        # Use convenience function with raise_on_error=False (default)
        result = calculate_card_geometry(context, raise_on_error=False)

        assert result.area == 0.0
        assert result.perimeter == 0.0

    def test_custom_surface_card(self):
        """Test calculation with custom surface card override."""
        context = self._create_test_context()
        # Context has default card
        context.surface_card = CardData(
            position=[0, 50, 50, 0],
            load=[0, 0, 5000, 5000],
        )
        # Provide custom card
        custom_card = CardData(
            position=[0, 100, 100, 0],
            load=[0, 0, 10000, 10000],
        )
        calculator = CardGeometryCalculator(context)
        result = calculator.calculate(surface_card=custom_card)

        # Should use custom card area = 1,000,000
        assert abs(result.area - 1000000.0) < 1.0

    def _create_test_context(self) -> DynacardAnalysisContext:
        """Create a test context with valid data."""
        return DynacardAnalysisContext(
            api14="TEST-GEO-001",
            surface_card=CardData(position=[0, 100], load=[5000, 15000]),
            rod_string=[RodSection(diameter=0.875, length=3000.0)],
            pump=PumpProperties(diameter=1.75, depth=5000.0),
            surface_unit=SurfaceUnit(),
            spm=6.0,
            runtime=24.0,
        )


class TestConvenienceFunction:
    """Tests for calculate_card_geometry convenience function."""

    def test_returns_card_geometry_analysis(self):
        """Test that convenience function returns CardGeometryAnalysis."""
        context = self._create_test_context()
        context.surface_card = CardData(
            position=[0, 100, 100, 0],
            load=[0, 0, 10000, 10000],
        )
        result = calculate_card_geometry(context)

        assert isinstance(result, CardGeometryAnalysis)

    def test_with_custom_card(self):
        """Test convenience function with custom surface card."""
        context = self._create_test_context()
        custom_card = CardData(
            position=[0, 50, 50, 0],
            load=[0, 0, 5000, 5000],
        )
        result = calculate_card_geometry(context, surface_card=custom_card)

        # Area = 50 * 5000 = 250,000
        assert abs(result.area - 250000.0) < 1.0

    def _create_test_context(self) -> DynacardAnalysisContext:
        """Create a test context."""
        return DynacardAnalysisContext(
            api14="TEST-GEO-002",
            surface_card=CardData(position=[0, 100], load=[5000, 15000]),
            rod_string=[RodSection(diameter=0.875, length=3000.0)],
            pump=PumpProperties(diameter=1.75, depth=5000.0),
            surface_unit=SurfaceUnit(),
            spm=6.0,
            runtime=24.0,
        )


class TestDirectCalculationFunctions:
    """Tests for calculate_card_perimeter function."""

    def test_perimeter_rectangle(self):
        """Test perimeter calculation for rectangle."""
        position = [0, 100, 100, 0]
        load = [0, 0, 100, 100]
        result = calculate_card_perimeter(position, load)

        # Perimeter = 4 * 100 = 400
        assert abs(result - 400.0) < 0.1

    def test_perimeter_tall_rectangle(self):
        """Test perimeter for tall rectangle."""
        position = [0, 100, 100, 0]
        load = [0, 0, 10000, 10000]
        result = calculate_card_perimeter(position, load)

        # Perimeter = 2 * (100 + 10000) = 20200
        assert abs(result - 20200.0) < 0.1

    def test_perimeter_triangle(self):
        """Test perimeter for equilateral triangle."""
        # Equilateral triangle with side 100
        position = [0, 100, 50]
        load = [0, 0, 86.6]  # height = 100 * sqrt(3)/2 ≈ 86.6
        result = calculate_card_perimeter(position, load)

        # Each side has different length
        # Side 1: sqrt((100-0)^2 + 0) = 100
        # Side 2: sqrt((50-100)^2 + 86.6^2) = sqrt(2500 + 7499.56) ≈ 100
        # Side 3: sqrt((0-50)^2 + 86.6^2) ≈ 100
        assert result > 290 and result < 310

    def test_perimeter_empty_arrays(self):
        """Test perimeter with empty arrays."""
        result = calculate_card_perimeter([], [])
        assert result == 0.0

    def test_perimeter_single_point(self):
        """Test perimeter with single point."""
        result = calculate_card_perimeter([50], [5000])
        assert result == 0.0

    def test_perimeter_two_points(self):
        """Test perimeter with two points (line segment)."""
        position = [0, 100]
        load = [0, 0]
        result = calculate_card_perimeter(position, load)

        # Should be 2 * 100 = 200 (back and forth)
        assert abs(result - 200.0) < 0.1

    def test_perimeter_mismatched_arrays(self):
        """Test perimeter with mismatched array lengths."""
        result = calculate_card_perimeter([0, 100, 100], [0, 0])
        assert result == 0.0


class TestCardGeometryAnalysisModel:
    """Tests for CardGeometryAnalysis model."""

    def test_analysis_defaults(self):
        """Test CardGeometryAnalysis default values."""
        analysis = CardGeometryAnalysis()
        assert analysis.area == 0.0
        assert analysis.perimeter == 0.0
        assert analysis.position_range == 0.0
        assert analysis.load_range == 0.0
        assert analysis.centroid_position == 0.0
        assert analysis.centroid_load == 0.0
        assert analysis.zone_areas == []
        assert analysis.zone_area_fractions == []

    def test_analysis_with_values(self):
        """Test CardGeometryAnalysis with actual values."""
        analysis = CardGeometryAnalysis(
            area=500000.0,
            perimeter=20200.0,
            position_range=100.0,
            load_range=10000.0,
            centroid_position=50.0,
            centroid_load=5000.0,
            zone_areas=[125000.0, 125000.0, 125000.0, 125000.0],
            zone_area_fractions=[0.25, 0.25, 0.25, 0.25],
        )
        assert analysis.area == 500000.0
        assert analysis.perimeter == 20200.0
        assert analysis.centroid_position == 50.0

    def test_analysis_serialization(self):
        """Test that CardGeometryAnalysis can be serialized."""
        analysis = CardGeometryAnalysis(
            area=500000.0,
            perimeter=20200.0,
        )

        data = analysis.model_dump()
        assert data["area"] == 500000.0
        assert data["perimeter"] == 20200.0


class TestCardGeometryWithRealData:
    """Tests using real well card data."""

    @pytest.fixture
    def well_7699227(self):
        """Load well 7699227 test data."""
        filepath = TEST_DATA_DIR / "7699227.json"
        if filepath.exists():
            return load_from_json_file(filepath)
        pytest.skip("Test data file 7699227.json not found")

    def test_geometry_calculation_7699227(self, well_7699227):
        """Test geometry calculation with well 7699227 data."""
        result = calculate_card_geometry(well_7699227)

        # Should have valid geometry metrics
        assert result.area > 0
        assert result.perimeter > 0
        assert result.position_range > 0
        assert result.load_range > 0

    def test_zoned_areas_7699227(self, well_7699227):
        """Test zoned area calculation with well 7699227 data."""
        result = calculate_card_geometry(well_7699227)

        # Should have 4 zones
        assert len(result.zone_areas) == 4
        assert len(result.zone_area_fractions) == 4

        # Fractions should sum to 1.0
        fraction_sum = sum(result.zone_area_fractions)
        assert abs(fraction_sum - 1.0) < 0.001

    def test_centroid_within_bounds_7699227(self, well_7699227):
        """Test that centroid is within card bounds."""
        result = calculate_card_geometry(well_7699227)

        # Get card bounds
        pos = well_7699227.surface_card.position
        load = well_7699227.surface_card.load

        # Centroid should be within bounds
        assert min(pos) <= result.centroid_position <= max(pos)
        assert min(load) <= result.centroid_load <= max(load)
