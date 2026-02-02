# ABOUTME: Unit tests for pump efficiency calculation module.
# ABOUTME: Validates efficiency ratio calculations and well test data handling.

import pytest
from pathlib import Path

from digitalmodel.artificial_lift.dynacard.data_loader import (
    load_from_json_file,
)
from digitalmodel.artificial_lift.dynacard.pump_efficiency import (
    PumpEfficiencyCalculator,
    calculate_pump_efficiency,
    calculate_efficiency_from_rates,
)
from digitalmodel.artificial_lift.dynacard.models import (
    DynacardAnalysisContext,
    CardData,
    RodSection,
    PumpProperties,
    SurfaceUnit,
    WellTestData,
    ProductionAnalysis,
)


# Test data directory
TEST_DATA_DIR = Path(__file__).parent / "testdata"


class TestPumpEfficiencyCalculator:
    """Tests for the PumpEfficiencyCalculator class."""

    def test_calculator_initialization(self):
        """Test calculator can be initialized with valid context."""
        context = self._create_test_context()
        calculator = PumpEfficiencyCalculator(context)
        assert calculator.ctx == context

    def test_calculate_with_valid_data(self):
        """Test efficiency calculation with valid well test data."""
        context = self._create_test_context()
        context.well_test = WellTestData(
            oil_rate=100.0,
            water_rate=50.0,
        )
        calculator = PumpEfficiencyCalculator(context)
        efficiency = calculator.calculate(theoretical_production=200.0)

        # (100 + 50) / 200 * 100 = 75%
        assert efficiency == 75.0

    def test_calculate_with_production_analysis(self):
        """Test efficiency using production analysis."""
        context = self._create_test_context()
        context.well_test = WellTestData(oil_rate=80.0, water_rate=20.0)

        production = ProductionAnalysis(theoretical_production=125.0)
        calculator = PumpEfficiencyCalculator(context)
        efficiency = calculator.calculate(production_analysis=production)

        # (80 + 20) / 125 * 100 = 80%
        assert efficiency == 80.0

    def test_calculate_no_well_test(self):
        """Test handling of missing well test data."""
        context = self._create_test_context()
        context.well_test = None
        calculator = PumpEfficiencyCalculator(context)
        efficiency = calculator.calculate(theoretical_production=200.0)

        assert efficiency == 0.0

    def test_calculate_zero_theoretical(self):
        """Test handling of zero theoretical production."""
        context = self._create_test_context()
        context.well_test = WellTestData(oil_rate=100.0, water_rate=50.0)
        calculator = PumpEfficiencyCalculator(context)
        efficiency = calculator.calculate(theoretical_production=0.0)

        assert efficiency == 0.0

    def test_calculate_oil_only(self):
        """Test efficiency with oil production only."""
        context = self._create_test_context()
        context.well_test = WellTestData(
            oil_rate=150.0,
            water_rate=0.0,
        )
        calculator = PumpEfficiencyCalculator(context)
        efficiency = calculator.calculate(theoretical_production=200.0)

        # 150 / 200 * 100 = 75%
        assert efficiency == 75.0

    def test_calculate_water_only(self):
        """Test efficiency with water production only."""
        context = self._create_test_context()
        context.well_test = WellTestData(
            oil_rate=0.0,
            water_rate=100.0,
        )
        calculator = PumpEfficiencyCalculator(context)
        efficiency = calculator.calculate(theoretical_production=200.0)

        # 100 / 200 * 100 = 50%
        assert efficiency == 50.0

    def test_calculate_zero_production(self):
        """Test handling of zero actual production."""
        context = self._create_test_context()
        context.well_test = WellTestData(oil_rate=0.0, water_rate=0.0)
        calculator = PumpEfficiencyCalculator(context)
        efficiency = calculator.calculate(theoretical_production=200.0)

        assert efficiency == 0.0

    def test_calculate_over_100_percent(self):
        """Test efficiency can exceed 100% (production exceeds theoretical)."""
        context = self._create_test_context()
        context.well_test = WellTestData(oil_rate=150.0, water_rate=100.0)
        calculator = PumpEfficiencyCalculator(context)
        efficiency = calculator.calculate(theoretical_production=200.0)

        # (150 + 100) / 200 * 100 = 125%
        assert efficiency == 125.0

    def test_explicit_theoretical_takes_priority(self):
        """Test that explicit theoretical production takes priority."""
        context = self._create_test_context()
        context.well_test = WellTestData(oil_rate=100.0, water_rate=0.0)

        production = ProductionAnalysis(theoretical_production=500.0)
        calculator = PumpEfficiencyCalculator(context)

        # Explicit value 200 should be used over production.theoretical (500)
        efficiency = calculator.calculate(
            theoretical_production=200.0,
            production_analysis=production,
        )

        # 100 / 200 * 100 = 50%
        assert efficiency == 50.0

    def _create_test_context(self) -> DynacardAnalysisContext:
        """Create a test context with valid data."""
        return DynacardAnalysisContext(
            api14="TEST-PE-001",
            surface_card=CardData(position=[0, 100], load=[5000, 15000]),
            rod_string=[RodSection(diameter=0.875, length=3000.0)],
            pump=PumpProperties(diameter=1.75, depth=5000.0),
            surface_unit=SurfaceUnit(),
            spm=6.0,
            runtime=24.0,
        )


class TestConvenienceFunction:
    """Tests for calculate_pump_efficiency convenience function."""

    def test_returns_float(self):
        """Test that convenience function returns a float."""
        context = self._create_test_context()
        context.well_test = WellTestData(oil_rate=100.0, water_rate=50.0)
        result = calculate_pump_efficiency(context, theoretical_production=200.0)

        assert isinstance(result, float)

    def test_with_production_analysis(self):
        """Test convenience function with production analysis."""
        context = self._create_test_context()
        context.well_test = WellTestData(oil_rate=80.0, water_rate=20.0)
        production = ProductionAnalysis(theoretical_production=100.0)

        result = calculate_pump_efficiency(context, production_analysis=production)
        assert result == 100.0

    def _create_test_context(self) -> DynacardAnalysisContext:
        """Create a test context."""
        return DynacardAnalysisContext(
            api14="TEST-PE-002",
            surface_card=CardData(position=[0, 100], load=[5000, 15000]),
            rod_string=[RodSection(diameter=0.875, length=3000.0)],
            pump=PumpProperties(diameter=1.75, depth=5000.0),
            surface_unit=SurfaceUnit(),
            spm=6.0,
            runtime=24.0,
        )


class TestEfficiencyFromRates:
    """Tests for calculate_efficiency_from_rates function."""

    def test_basic_calculation(self):
        """Test basic efficiency calculation from rates."""
        result = calculate_efficiency_from_rates(
            oil_rate=100.0,
            water_rate=50.0,
            theoretical_production=200.0,
        )
        assert result == 75.0

    def test_zero_theoretical(self):
        """Test handling of zero theoretical production."""
        result = calculate_efficiency_from_rates(
            oil_rate=100.0,
            water_rate=50.0,
            theoretical_production=0.0,
        )
        assert result == 0.0

    def test_oil_only(self):
        """Test with oil rate only."""
        result = calculate_efficiency_from_rates(
            oil_rate=100.0,
            water_rate=0.0,
            theoretical_production=100.0,
        )
        assert result == 100.0

    def test_water_only(self):
        """Test with water rate only."""
        result = calculate_efficiency_from_rates(
            oil_rate=0.0,
            water_rate=100.0,
            theoretical_production=100.0,
        )
        assert result == 100.0

    def test_rounding(self):
        """Test that result is rounded to 2 decimal places."""
        result = calculate_efficiency_from_rates(
            oil_rate=100.0,
            water_rate=33.33,
            theoretical_production=200.0,
        )
        # (100 + 33.33) / 200 * 100 = 66.665 -> 66.66
        assert result == 66.66


class TestPumpEfficiencyWithRealData:
    """Tests using real well card data."""

    @pytest.fixture
    def well_7699227(self):
        """Load well 7699227 test data."""
        filepath = TEST_DATA_DIR / "7699227.json"
        if filepath.exists():
            return load_from_json_file(filepath)
        pytest.skip("Test data file 7699227.json not found")

    def test_pump_efficiency_7699227(self, well_7699227):
        """Test pump efficiency with well 7699227 data."""
        if well_7699227.well_test is None:
            pytest.skip("No well test data in 7699227.json")

        # Assume theoretical production from typical calculation
        theoretical = 200.0  # BPD

        result = calculate_pump_efficiency(
            well_7699227,
            theoretical_production=theoretical,
        )

        # Should return valid efficiency or 0 if missing data
        assert result >= 0.0
