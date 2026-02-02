# ABOUTME: Unit tests for load ratio analysis module.
# ABOUTME: Validates peak and low load ratio calculations for equipment monitoring.

import pytest
from pathlib import Path

from digitalmodel.artificial_lift.dynacard.data_loader import (
    load_from_json_file,
)
from digitalmodel.artificial_lift.dynacard.load_analysis import (
    LoadRatioCalculator,
    calculate_load_ratios,
    calculate_peak_load_ratio,
    calculate_low_load_ratio,
)
from digitalmodel.artificial_lift.dynacard.models import (
    DynacardAnalysisContext,
    CardData,
    RodSection,
    PumpProperties,
    SurfaceUnit,
    InputParameters,
    LoadRatioAnalysis,
)


# Test data directory
TEST_DATA_DIR = Path(__file__).parent / "testdata"


class TestLoadRatioCalculator:
    """Tests for the LoadRatioCalculator class."""

    def test_calculator_initialization(self):
        """Test calculator can be initialized with valid context."""
        context = self._create_test_context()
        calculator = LoadRatioCalculator(context)
        assert calculator.ctx == context
        assert calculator.result is not None

    def test_calculate_with_input_params(self):
        """Test load ratio calculation using input parameters."""
        context = self._create_test_context()
        context.input_params = InputParameters(
            strokes_per_minute=6.0,
            stroke_load_peak=15000.0,
            normal_peak_load=12000.0,
            stroke_load_min=3000.0,
            normal_min_load=4000.0,
        )
        calculator = LoadRatioCalculator(context)
        result = calculator.calculate()

        # Peak: 15000 / 12000 = 1.25
        assert result.peak_load_ratio == 1.25
        # Low: 3000 / 4000 = 0.75
        assert result.low_load_ratio == 0.75

    def test_calculate_from_surface_card(self):
        """Test load ratio calculation extracting loads from surface card."""
        context = self._create_test_context()
        context.input_params = InputParameters(
            strokes_per_minute=6.0,
            normal_peak_load=12000.0,
            normal_min_load=4000.0,
        )
        # Surface card with peak=15000, min=5000
        context.surface_card = CardData(
            position=[0, 50, 100, 50],
            load=[5000, 10000, 15000, 8000],
        )
        calculator = LoadRatioCalculator(context)
        result = calculator.calculate()

        # Peak: 15000 / 12000 = 1.25
        assert result.peak_load_ratio == 1.25
        # Low: 5000 / 4000 = 1.25
        assert result.low_load_ratio == 1.25
        assert result.actual_peak_load == 15000
        assert result.actual_min_load == 5000

    def test_calculate_from_provided_card(self):
        """Test calculation with explicitly provided surface card."""
        context = self._create_test_context()
        context.input_params = InputParameters(
            strokes_per_minute=6.0,
            normal_peak_load=10000.0,
            normal_min_load=2000.0,
        )
        custom_card = CardData(
            position=[0, 100],
            load=[4000, 8000],
        )
        calculator = LoadRatioCalculator(context)
        result = calculator.calculate(surface_card=custom_card)

        # Peak: 8000 / 10000 = 0.8
        assert result.peak_load_ratio == 0.8
        # Low: 4000 / 2000 = 2.0
        assert result.low_load_ratio == 2.0

    def test_peak_load_ratio_only(self):
        """Test when only peak load data is available."""
        context = self._create_test_context()
        context.input_params = InputParameters(
            strokes_per_minute=6.0,
            stroke_load_peak=15000.0,
            normal_peak_load=12000.0,
            # No min load data
        )
        calculator = LoadRatioCalculator(context)
        result = calculator.calculate()

        assert result.peak_load_ratio == 1.25
        assert result.low_load_ratio == 0.0  # No min load data

    def test_low_load_ratio_only(self):
        """Test when only low load data is available."""
        context = self._create_test_context()
        context.input_params = InputParameters(
            strokes_per_minute=6.0,
            stroke_load_min=3000.0,
            normal_min_load=4000.0,
            # No peak load data
        )
        calculator = LoadRatioCalculator(context)
        result = calculator.calculate()

        assert result.peak_load_ratio == 0.0  # No peak load data
        assert result.low_load_ratio == 0.75

    def test_no_normal_loads(self):
        """Test when normal (expected) loads are not provided."""
        context = self._create_test_context()
        context.input_params = InputParameters(
            strokes_per_minute=6.0,
            stroke_load_peak=15000.0,
            stroke_load_min=3000.0,
            # No normal loads
        )
        calculator = LoadRatioCalculator(context)
        result = calculator.calculate()

        # Cannot calculate ratios without normal values
        assert result.peak_load_ratio == 0.0
        assert result.low_load_ratio == 0.0

    def test_zero_normal_peak_load(self):
        """Test handling of zero normal peak load."""
        context = self._create_test_context()
        context.input_params = InputParameters(
            strokes_per_minute=6.0,
            stroke_load_peak=15000.0,
            normal_peak_load=0.0,
        )
        calculator = LoadRatioCalculator(context)
        result = calculator.calculate()

        # Division by zero should return 0
        assert result.peak_load_ratio == 0.0

    def test_zero_normal_min_load(self):
        """Test handling of zero normal min load."""
        context = self._create_test_context()
        context.input_params = InputParameters(
            strokes_per_minute=6.0,
            stroke_load_min=3000.0,
            normal_min_load=0.0,
        )
        calculator = LoadRatioCalculator(context)
        result = calculator.calculate()

        # Division by zero should return 0
        assert result.low_load_ratio == 0.0

    def test_load_max_sp_fallback(self):
        """Test fallback to load_max_sp for normal peak."""
        context = self._create_test_context()
        context.input_params = InputParameters(
            strokes_per_minute=6.0,
            stroke_load_peak=15000.0,
            load_max_sp=10000.0,  # Using as normal peak fallback
        )
        calculator = LoadRatioCalculator(context)
        result = calculator.calculate()

        # Peak: 15000 / 10000 = 1.5
        assert result.peak_load_ratio == 1.5

    def test_load_min_sp_fallback(self):
        """Test fallback to load_min_sp for normal min."""
        context = self._create_test_context()
        context.input_params = InputParameters(
            strokes_per_minute=6.0,
            stroke_load_min=3000.0,
            load_min_sp=5000.0,  # Using as normal min fallback
        )
        calculator = LoadRatioCalculator(context)
        result = calculator.calculate()

        # Low: 3000 / 5000 = 0.6
        assert result.low_load_ratio == 0.6

    def test_result_stores_all_loads(self):
        """Test that result stores all load values."""
        context = self._create_test_context()
        context.input_params = InputParameters(
            strokes_per_minute=6.0,
            stroke_load_peak=15000.0,
            normal_peak_load=12000.0,
            stroke_load_min=3000.0,
            normal_min_load=4000.0,
        )
        calculator = LoadRatioCalculator(context)
        result = calculator.calculate()

        assert result.actual_peak_load == 15000.0
        assert result.normal_peak_load == 12000.0
        assert result.actual_min_load == 3000.0
        assert result.normal_min_load == 4000.0

    def _create_test_context(self) -> DynacardAnalysisContext:
        """Create a test context with valid data."""
        return DynacardAnalysisContext(
            api14="TEST-LA-001",
            surface_card=CardData(position=[0, 100], load=[5000, 15000]),
            rod_string=[RodSection(diameter=0.875, length=3000.0)],
            pump=PumpProperties(diameter=1.75, depth=5000.0),
            surface_unit=SurfaceUnit(),
            spm=6.0,
            runtime=24.0,
        )


class TestConvenienceFunction:
    """Tests for calculate_load_ratios convenience function."""

    def test_returns_load_ratio_analysis(self):
        """Test that convenience function returns LoadRatioAnalysis."""
        context = self._create_test_context()
        context.input_params = InputParameters(
            strokes_per_minute=6.0,
            stroke_load_peak=15000.0,
            normal_peak_load=12000.0,
        )
        result = calculate_load_ratios(context)

        assert isinstance(result, LoadRatioAnalysis)

    def test_with_provided_card(self):
        """Test convenience function with provided surface card."""
        context = self._create_test_context()
        context.input_params = InputParameters(
            strokes_per_minute=6.0,
            normal_peak_load=10000.0,
            normal_min_load=2000.0,
        )
        card = CardData(position=[0, 100], load=[3000, 9000])

        result = calculate_load_ratios(context, surface_card=card)

        assert result.actual_peak_load == 9000
        assert result.actual_min_load == 3000

    def _create_test_context(self) -> DynacardAnalysisContext:
        """Create a test context."""
        return DynacardAnalysisContext(
            api14="TEST-LA-002",
            surface_card=CardData(position=[0, 100], load=[5000, 15000]),
            rod_string=[RodSection(diameter=0.875, length=3000.0)],
            pump=PumpProperties(diameter=1.75, depth=5000.0),
            surface_unit=SurfaceUnit(),
            spm=6.0,
            runtime=24.0,
        )


class TestDirectCalculationFunctions:
    """Tests for calculate_peak_load_ratio and calculate_low_load_ratio functions."""

    def test_peak_load_ratio_basic(self):
        """Test basic peak load ratio calculation."""
        result = calculate_peak_load_ratio(15000.0, 12000.0)
        assert result == 1.25

    def test_peak_load_ratio_zero_normal(self):
        """Test peak load ratio with zero normal."""
        result = calculate_peak_load_ratio(15000.0, 0.0)
        assert result == 0.0

    def test_peak_load_ratio_less_than_one(self):
        """Test peak load ratio less than 1.0."""
        result = calculate_peak_load_ratio(10000.0, 15000.0)
        # 10000 / 15000 = 0.6667
        assert abs(result - 0.6667) < 0.001

    def test_low_load_ratio_basic(self):
        """Test basic low load ratio calculation."""
        result = calculate_low_load_ratio(3000.0, 4000.0)
        assert result == 0.75

    def test_low_load_ratio_zero_normal(self):
        """Test low load ratio with zero normal."""
        result = calculate_low_load_ratio(3000.0, 0.0)
        assert result == 0.0

    def test_low_load_ratio_greater_than_one(self):
        """Test low load ratio greater than 1.0."""
        result = calculate_low_load_ratio(5000.0, 4000.0)
        assert result == 1.25


class TestLoadRatioAnalysisModel:
    """Tests for LoadRatioAnalysis model."""

    def test_analysis_defaults(self):
        """Test LoadRatioAnalysis default values."""
        analysis = LoadRatioAnalysis()
        assert analysis.peak_load_ratio == 0.0
        assert analysis.low_load_ratio == 0.0
        assert analysis.actual_peak_load == 0.0
        assert analysis.normal_peak_load == 0.0
        assert analysis.actual_min_load == 0.0
        assert analysis.normal_min_load == 0.0

    def test_analysis_with_values(self):
        """Test LoadRatioAnalysis with actual values."""
        analysis = LoadRatioAnalysis(
            peak_load_ratio=1.25,
            low_load_ratio=0.75,
            actual_peak_load=15000.0,
            normal_peak_load=12000.0,
            actual_min_load=3000.0,
            normal_min_load=4000.0,
        )
        assert analysis.peak_load_ratio == 1.25
        assert analysis.low_load_ratio == 0.75
        assert analysis.actual_peak_load == 15000.0

    def test_analysis_serialization(self):
        """Test that LoadRatioAnalysis can be serialized."""
        analysis = LoadRatioAnalysis(
            peak_load_ratio=1.25,
            low_load_ratio=0.75,
        )

        data = analysis.model_dump()
        assert data['peak_load_ratio'] == 1.25
        assert data['low_load_ratio'] == 0.75


class TestLoadRatioWithRealData:
    """Tests using real well card data."""

    @pytest.fixture
    def well_7699227(self):
        """Load well 7699227 test data."""
        filepath = TEST_DATA_DIR / "7699227.json"
        if filepath.exists():
            return load_from_json_file(filepath)
        pytest.skip("Test data file 7699227.json not found")

    def test_load_ratios_from_card(self, well_7699227):
        """Test extracting load ratios from real card data."""
        # Set normal loads for testing
        if well_7699227.input_params is None:
            well_7699227.input_params = InputParameters(strokes_per_minute=6.0)

        well_7699227.input_params.normal_peak_load = 15000.0
        well_7699227.input_params.normal_min_load = 5000.0

        result = calculate_load_ratios(well_7699227)

        # Should have extracted actual loads from card
        assert result.actual_peak_load > 0
        assert result.actual_min_load < result.actual_peak_load
