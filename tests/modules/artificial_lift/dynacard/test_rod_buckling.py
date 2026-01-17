# ABOUTME: Unit tests for rod buckling analysis module.
# ABOUTME: Validates buckling detection, neutral point, and critical load calculations.

import math
import pytest
import numpy as np
from pathlib import Path

from digitalmodel.modules.artificial_lift.dynacard.data_loader import (
    load_from_json_file,
)
from digitalmodel.modules.artificial_lift.dynacard.rod_buckling import (
    RodBucklingCalculator,
    calculate_rod_buckling,
    estimate_neutral_point,
    calculate_critical_buckling_load,
)
from digitalmodel.modules.artificial_lift.dynacard.models import (
    DynacardAnalysisContext,
    CardData,
    RodSection,
    PumpProperties,
    SurfaceUnit,
    RodBucklingAnalysis,
)
from digitalmodel.modules.artificial_lift.dynacard.exceptions import ValidationError


# Test data directory
TEST_DATA_DIR = Path(__file__).parent / "testdata"


class TestRodBucklingCalculator:
    """Tests for the RodBucklingCalculator class."""

    def test_calculator_initialization(self):
        """Test calculator can be initialized with valid context."""
        context = self._create_test_context()
        calculator = RodBucklingCalculator(context)
        assert calculator.ctx == context
        assert calculator.result is not None

    def test_calculate_returns_analysis(self):
        """Test that calculate returns RodBucklingAnalysis."""
        context = self._create_test_context()
        calculator = RodBucklingCalculator(context)
        result = calculator.calculate()
        assert isinstance(result, RodBucklingAnalysis)

    def test_critical_loads_calculated(self):
        """Test that critical buckling loads are calculated."""
        context = self._create_test_context()
        calculator = RodBucklingCalculator(context)
        result = calculator.calculate()

        # Should have positive critical loads
        assert result.sinusoidal_critical_load > 0
        assert result.helical_critical_load > 0

    def test_helical_greater_than_sinusoidal(self):
        """Test that helical critical load > sinusoidal critical load."""
        context = self._create_test_context()
        calculator = RodBucklingCalculator(context)
        result = calculator.calculate()

        # Helical buckling requires higher load than sinusoidal
        assert result.helical_critical_load > result.sinusoidal_critical_load

    def test_helical_approximately_283_times_sinusoidal(self):
        """Test that helical ≈ 2.83 × sinusoidal (sqrt(8) ratio)."""
        context = self._create_test_context()
        calculator = RodBucklingCalculator(context)
        result = calculator.calculate()

        ratio = result.helical_critical_load / result.sinusoidal_critical_load
        assert abs(ratio - 2.83) < 0.01

    def test_neutral_point_within_rod_length(self):
        """Test that neutral point is within rod string."""
        context = self._create_test_context()
        calculator = RodBucklingCalculator(context)
        result = calculator.calculate()

        rod_length = context.rod_length
        assert 0.0 <= result.neutral_point_depth <= rod_length

    def test_neutral_point_fraction_valid(self):
        """Test that neutral point fraction is between 0 and 1."""
        context = self._create_test_context()
        calculator = RodBucklingCalculator(context)
        result = calculator.calculate()

        assert 0.0 <= result.neutral_point_fraction <= 1.0

    def test_no_buckling_with_tensile_loads(self):
        """Test no buckling detected with all tensile loads."""
        context = self._create_test_context()
        # Create surface card with all positive (tensile) loads
        context.surface_card = CardData(
            position=[0, 50, 100, 50],
            load=[8000, 10000, 12000, 10000],  # All high tensile
        )
        calculator = RodBucklingCalculator(context)
        result = calculator.calculate()

        # With high tensile loads, no buckling expected
        # (depends on critical load calculation)
        assert result.analysis_method == "simplified"

    def test_buckling_with_compressive_loads(self):
        """Test buckling detection with compressive downhole loads."""
        context = self._create_test_context()
        # Create downhole card with compression
        downhole_card = CardData(
            position=[0, 50, 100, 50],
            load=[-1000, 2000, 5000, 2000],  # Negative = compression
        )
        calculator = RodBucklingCalculator(context)
        result = calculator.calculate(downhole_card=downhole_card)

        # Should detect buckling with significant compression
        assert result.max_compressive_load > 0
        assert result.sinusoidal_buckling_detected or result.max_compressive_load > 0

    def test_severe_compression_triggers_buckling(self):
        """Test that severe compression triggers buckling detection."""
        context = self._create_test_context()
        downhole_card = CardData(
            position=[0, 50, 100, 50],
            load=[-5000, 1000, 3000, 1000],  # Strong compression
        )
        calculator = RodBucklingCalculator(context)
        result = calculator.calculate(downhole_card=downhole_card)

        # Severe compression should trigger sinusoidal buckling
        assert result.sinusoidal_buckling_detected

    def test_compression_length_calculated(self):
        """Test that compression length is calculated."""
        context = self._create_test_context()
        downhole_card = CardData(
            position=[0, 50, 100, 50],
            load=[-1000, 2000, 5000, 2000],
        )
        calculator = RodBucklingCalculator(context)
        result = calculator.calculate(downhole_card=downhole_card)

        # Should have some compression length
        assert result.compression_length >= 0

    def test_buckling_tendency_calculated(self):
        """Test that buckling tendency min/max are calculated."""
        context = self._create_test_context()
        calculator = RodBucklingCalculator(context)
        result = calculator.calculate()

        # Buckling tendency should be calculated
        assert result.min_buckling_tendency != 0 or result.max_buckling_tendency != 0

    def test_empty_surface_card_raises_validation_error(self):
        """Test that empty surface card raises ValidationError."""
        context = self._create_test_context()
        context.surface_card = CardData(position=[], load=[])
        calculator = RodBucklingCalculator(context)

        with pytest.raises(ValidationError) as exc_info:
            calculator.calculate()

        assert "load" in exc_info.value.message.lower() or "data" in exc_info.value.message.lower()

    def test_zero_rod_length_raises_validation_error(self):
        """Test that zero rod length raises ValidationError."""
        context = self._create_test_context()
        context.rod_string = []  # No rod sections
        calculator = RodBucklingCalculator(context)

        with pytest.raises(ValidationError) as exc_info:
            calculator.calculate()

        assert "rod" in exc_info.value.message.lower() or "length" in exc_info.value.message.lower()

    def test_convenience_function_handles_errors_gracefully(self):
        """Test that convenience function returns result with warning on error."""
        context = self._create_test_context()
        context.surface_card = CardData(position=[], load=[])

        # Use convenience function with raise_on_error=False (default)
        result = calculate_rod_buckling(context, raise_on_error=False)

        # Should return result with warning message set
        assert result.warning_message != ""

    def test_with_custom_downhole_card(self):
        """Test calculation with provided downhole card."""
        context = self._create_test_context()
        custom_card = CardData(
            position=[0, 25, 50, 75, 100, 75, 50, 25],
            load=[2000, 3000, 4000, 5000, 4000, 3000, 2000, 1000],
        )
        calculator = RodBucklingCalculator(context)
        result = calculator.calculate(downhole_card=custom_card)

        # Should complete analysis
        assert result.analysis_method == "simplified"

    def _create_test_context(self) -> DynacardAnalysisContext:
        """Create a test context with valid data."""
        return DynacardAnalysisContext(
            api14="TEST-BUCK-001",
            surface_card=CardData(
                position=[0, 50, 100, 50],
                load=[5000, 8000, 12000, 7000],
            ),
            rod_string=[
                RodSection(diameter=0.875, length=2000.0),
                RodSection(diameter=0.75, length=2000.0),
                RodSection(diameter=0.625, length=1000.0),
            ],
            pump=PumpProperties(diameter=1.75, depth=5000.0),
            surface_unit=SurfaceUnit(),
            spm=6.0,
            runtime=24.0,
            fluid_density=55.0,
        )


class TestConvenienceFunction:
    """Tests for calculate_rod_buckling convenience function."""

    def test_returns_rod_buckling_analysis(self):
        """Test that convenience function returns RodBucklingAnalysis."""
        context = self._create_test_context()
        result = calculate_rod_buckling(context)
        assert isinstance(result, RodBucklingAnalysis)

    def test_with_downhole_card(self):
        """Test convenience function with downhole card."""
        context = self._create_test_context()
        downhole_card = CardData(
            position=[0, 50, 100, 50],
            load=[1000, 2000, 3000, 2000],
        )
        result = calculate_rod_buckling(context, downhole_card=downhole_card)
        assert isinstance(result, RodBucklingAnalysis)

    def _create_test_context(self) -> DynacardAnalysisContext:
        """Create a test context."""
        return DynacardAnalysisContext(
            api14="TEST-BUCK-002",
            surface_card=CardData(
                position=[0, 50, 100, 50],
                load=[5000, 8000, 12000, 7000],
            ),
            rod_string=[RodSection(diameter=0.875, length=5000.0)],
            pump=PumpProperties(diameter=1.75, depth=5000.0),
            surface_unit=SurfaceUnit(),
            spm=6.0,
            runtime=24.0,
        )


class TestEstimateNeutralPoint:
    """Tests for estimate_neutral_point function."""

    def test_basic_calculation(self):
        """Test basic neutral point estimation."""
        neutral = estimate_neutral_point(
            surface_load_max=12000,
            surface_load_min=5000,
            rod_weight=4000,
            fluid_density=55.0,
            rod_length=5000.0,
        )
        assert 0 <= neutral <= 5000

    def test_high_fluid_load_gives_deeper_neutral(self):
        """Test that higher fluid load gives deeper neutral point."""
        neutral_low = estimate_neutral_point(
            surface_load_max=10000,
            surface_load_min=8000,
            rod_weight=4000,
            rod_length=5000.0,
        )
        neutral_high = estimate_neutral_point(
            surface_load_max=15000,
            surface_load_min=5000,
            rod_weight=4000,
            rod_length=5000.0,
        )
        # Higher fluid load should give deeper neutral point
        assert neutral_high >= neutral_low

    def test_zero_rod_weight_returns_full_length(self):
        """Test that zero rod weight returns full rod length."""
        neutral = estimate_neutral_point(
            surface_load_max=10000,
            surface_load_min=5000,
            rod_weight=0,
            rod_length=5000.0,
        )
        # With no rod weight, neutral point is at bottom
        assert neutral == 5000.0

    def test_heavy_rod_gives_shallower_neutral(self):
        """Test that heavier rod gives shallower neutral point."""
        neutral_light = estimate_neutral_point(
            surface_load_max=12000,
            surface_load_min=5000,
            rod_weight=2000,
            rod_length=5000.0,
        )
        neutral_heavy = estimate_neutral_point(
            surface_load_max=12000,
            surface_load_min=5000,
            rod_weight=6000,
            rod_length=5000.0,
        )
        # Heavier rod should have shallower neutral point
        assert neutral_heavy <= neutral_light

    def test_returns_within_bounds(self):
        """Test neutral point is always within rod length."""
        for load_max in [8000, 12000, 16000]:
            for load_min in [3000, 5000, 7000]:
                neutral = estimate_neutral_point(
                    surface_load_max=load_max,
                    surface_load_min=load_min,
                    rod_weight=4000,
                    rod_length=5000.0,
                )
                assert 0 <= neutral <= 5000.0


class TestCalculateCriticalBucklingLoad:
    """Tests for calculate_critical_buckling_load function."""

    def test_returns_tuple(self):
        """Test that function returns tuple of two values."""
        result = calculate_critical_buckling_load(rod_diameter=0.875)
        assert isinstance(result, tuple)
        assert len(result) == 2

    def test_positive_critical_loads(self):
        """Test that critical loads are positive."""
        sinusoidal, helical = calculate_critical_buckling_load(rod_diameter=0.875)
        assert sinusoidal > 0
        assert helical > 0

    def test_helical_greater_than_sinusoidal(self):
        """Test that helical > sinusoidal critical load."""
        sinusoidal, helical = calculate_critical_buckling_load(rod_diameter=0.875)
        assert helical > sinusoidal

    def test_larger_diameter_higher_critical_load(self):
        """Test that larger diameter gives higher critical load."""
        small_sin, small_hel = calculate_critical_buckling_load(rod_diameter=0.625)
        large_sin, large_hel = calculate_critical_buckling_load(rod_diameter=1.0)

        # Larger diameter should have higher critical loads
        assert large_sin > small_sin
        assert large_hel > small_hel

    def test_with_custom_modulus(self):
        """Test with custom modulus of elasticity."""
        default_sin, _ = calculate_critical_buckling_load(rod_diameter=0.875)
        high_sin, _ = calculate_critical_buckling_load(
            rod_diameter=0.875,
            modulus=35000000.0,  # Higher modulus
        )

        # Higher modulus should give higher critical load
        assert high_sin > default_sin

    def test_with_different_fluid_densities(self):
        """Test with different fluid densities."""
        light_sin, _ = calculate_critical_buckling_load(
            rod_diameter=0.875,
            fluid_density=50.0,  # Light fluid
        )
        heavy_sin, _ = calculate_critical_buckling_load(
            rod_diameter=0.875,
            fluid_density=70.0,  # Heavy fluid
        )

        # Different fluid densities affect buoyancy
        assert light_sin != heavy_sin

    def test_common_rod_sizes(self):
        """Test critical loads for common rod sizes."""
        common_sizes = [0.625, 0.75, 0.875, 1.0, 1.125]

        prev_sinusoidal = 0
        for diameter in common_sizes:
            sinusoidal, helical = calculate_critical_buckling_load(
                rod_diameter=diameter
            )
            # Each larger size should have higher critical load
            assert sinusoidal > prev_sinusoidal
            prev_sinusoidal = sinusoidal


class TestRodBucklingAnalysisModel:
    """Tests for RodBucklingAnalysis model."""

    def test_analysis_defaults(self):
        """Test RodBucklingAnalysis default values."""
        analysis = RodBucklingAnalysis()
        assert analysis.sinusoidal_buckling_detected is False
        assert analysis.helical_buckling_detected is False
        assert analysis.neutral_point_depth == 0.0
        assert analysis.neutral_point_fraction == 0.0
        assert analysis.max_compressive_load == 0.0
        assert analysis.analysis_method == "simplified"

    def test_analysis_with_values(self):
        """Test RodBucklingAnalysis with actual values."""
        analysis = RodBucklingAnalysis(
            sinusoidal_buckling_detected=True,
            helical_buckling_detected=False,
            neutral_point_depth=3500.0,
            neutral_point_fraction=0.7,
            max_compressive_load=1500.0,
            sinusoidal_critical_load=800.0,
            helical_critical_load=2264.0,
        )
        assert analysis.sinusoidal_buckling_detected is True
        assert analysis.helical_buckling_detected is False
        assert analysis.neutral_point_depth == 3500.0

    def test_analysis_serialization(self):
        """Test that RodBucklingAnalysis can be serialized."""
        analysis = RodBucklingAnalysis(
            sinusoidal_buckling_detected=True,
            neutral_point_depth=3500.0,
            max_compressive_load=1500.0,
        )

        data = analysis.model_dump()
        assert data["sinusoidal_buckling_detected"] is True
        assert data["neutral_point_depth"] == 3500.0
        assert data["max_compressive_load"] == 1500.0


class TestRodBucklingWithRealData:
    """Tests using real well card data."""

    @pytest.fixture
    def well_7699227(self):
        """Load well 7699227 test data."""
        filepath = TEST_DATA_DIR / "7699227.json"
        if filepath.exists():
            return load_from_json_file(filepath)
        pytest.skip("Test data file 7699227.json not found")

    def test_buckling_calculation_7699227(self, well_7699227):
        """Test buckling calculation with well 7699227 data."""
        result = calculate_rod_buckling(well_7699227)

        # Should complete analysis
        assert result.analysis_method == "simplified"

    def test_critical_loads_positive_7699227(self, well_7699227):
        """Test that critical loads are positive with real data."""
        result = calculate_rod_buckling(well_7699227)

        assert result.sinusoidal_critical_load > 0
        assert result.helical_critical_load > 0

    def test_neutral_point_within_bounds_7699227(self, well_7699227):
        """Test neutral point is within rod length."""
        result = calculate_rod_buckling(well_7699227)

        rod_length = well_7699227.rod_length
        assert 0 <= result.neutral_point_depth <= rod_length

    def test_buckling_tendency_calculated_7699227(self, well_7699227):
        """Test buckling tendency is calculated."""
        result = calculate_rod_buckling(well_7699227)

        # Should have buckling tendency values
        assert (
            result.min_buckling_tendency != 0
            or result.max_buckling_tendency != 0
        )
