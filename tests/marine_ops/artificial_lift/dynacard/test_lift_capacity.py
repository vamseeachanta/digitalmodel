# ABOUTME: Unit tests for lift capacity calculation module.
# ABOUTME: Validates BPD calculations, input handling, and efficiency factors.

import math
import pytest
import numpy as np
from pathlib import Path

from digitalmodel.marine_ops.artificial_lift.dynacard.data_loader import (
    load_from_json_file,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.lift_capacity import (
    LiftCapacityCalculator,
    calculate_lift_capacity,
    BPD_CONVERSION,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.models import (
    DynacardAnalysisContext,
    CardData,
    RodSection,
    PumpProperties,
    SurfaceUnit,
    InputParameters,
    PumpFillageAnalysis,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.exceptions import ValidationError


# Test data directory
TEST_DATA_DIR = Path(__file__).parent / "testdata"


class TestLiftCapacityCalculator:
    """Tests for the LiftCapacityCalculator class."""

    def test_calculator_initialization(self):
        """Test calculator can be initialized with valid context."""
        context = self._create_test_context()
        calculator = LiftCapacityCalculator(context)
        assert calculator.ctx == context
        assert calculator.result is not None

    def test_calculate_basic(self):
        """Test basic lift capacity calculation."""
        context = self._create_test_context()
        calculator = LiftCapacityCalculator(context)
        result = calculator.calculate(downhole_stroke=100.0)

        assert result.lift_capacity > 0
        assert result.plunger_diameter == 1.75
        assert result.stroke_length == 100.0
        assert result.spm_24hr_avg == 6.0

    def test_calculate_with_fillage_analysis(self):
        """Test calculation using fillage analysis stroke data."""
        context = self._create_test_context()
        fillage = PumpFillageAnalysis(
            gross_stroke=95.0,
            net_stroke=90.0,
            fillage=94.7,
        )
        calculator = LiftCapacityCalculator(context)
        result = calculator.calculate(fillage_analysis=fillage)

        assert result.stroke_length == 95.0

    def test_calculate_with_surface_stroke_fallback(self):
        """Test fallback to surface unit stroke length."""
        context = self._create_test_context()
        context.surface_unit.stroke_length = 120.0
        calculator = LiftCapacityCalculator(context)
        result = calculator.calculate()

        assert result.stroke_length == 120.0

    def test_bpd_conversion_constant(self):
        """Test the BPD conversion constant value."""
        # 1440 minutes/day / 9702 in^3/bbl
        expected = 1440.0 / 9702.0
        assert abs(BPD_CONVERSION - expected) < 0.0001

    def test_lift_capacity_formula(self):
        """Test lift capacity formula: LC = (π/4) * d² * SPM * SL * E * (1440/9702)."""
        context = self._create_test_context()
        context.pump.diameter = 2.0  # 2" plunger
        context.spm = 5.0  # 5 SPM

        calculator = LiftCapacityCalculator(context)
        result = calculator.calculate(downhole_stroke=100.0)

        # Manual calculation
        plunger_area = (math.pi / 4.0) * (2.0 ** 2)
        expected_lc = plunger_area * 5.0 * 100.0 * 1.0 * BPD_CONVERSION

        assert abs(result.lift_capacity - expected_lc) < 0.1

    def test_zero_plunger_diameter_raises_validation_error(self):
        """Test that zero plunger diameter raises ValidationError."""
        context = self._create_test_context()
        context.pump.diameter = 0.0
        calculator = LiftCapacityCalculator(context)

        with pytest.raises(ValidationError) as exc_info:
            calculator.calculate(downhole_stroke=100.0)

        assert "pump" in exc_info.value.message.lower() or "diameter" in exc_info.value.message.lower()

    def test_zero_spm_raises_validation_error(self):
        """Test that zero SPM raises ValidationError."""
        context = self._create_test_context()
        context.spm = 0.0
        calculator = LiftCapacityCalculator(context)

        with pytest.raises(ValidationError) as exc_info:
            calculator.calculate(downhole_stroke=100.0)

        assert "spm" in exc_info.value.message.lower()

    def test_zero_stroke_length_raises_validation_error(self):
        """Test that zero stroke length raises ValidationError."""
        context = self._create_test_context()
        context.surface_unit.stroke_length = 0.0
        calculator = LiftCapacityCalculator(context)

        with pytest.raises(ValidationError) as exc_info:
            calculator.calculate()  # No stroke provided

        assert "stroke" in exc_info.value.message.lower()

    def test_convenience_function_handles_errors_gracefully(self):
        """Test that convenience function returns zero values on error."""
        context = self._create_test_context()
        context.pump.diameter = 0.0

        # Use convenience function with raise_on_error=False (default)
        result = calculate_lift_capacity(context, downhole_stroke=100.0, raise_on_error=False)

        # Should return zero lift capacity
        assert result.lift_capacity == 0.0

    def test_24hr_avg_spm_priority(self):
        """Test that 24hr average SPM takes priority over instantaneous."""
        context = self._create_test_context()
        context.spm = 6.0  # Instantaneous
        context.input_params = InputParameters(
            strokes_per_minute=6.0,
            spm_24hr_avg=5.5,  # 24hr average
        )

        calculator = LiftCapacityCalculator(context)
        result = calculator.calculate(downhole_stroke=100.0)

        assert result.spm_24hr_avg == 5.5

    def test_efficiency_from_input_params(self):
        """Test pump efficiency from input parameters."""
        context = self._create_test_context()
        context.input_params = InputParameters(
            strokes_per_minute=6.0,
            assumed_pump_efficiency=0.85,
        )

        calculator = LiftCapacityCalculator(context)
        result = calculator.calculate(downhole_stroke=100.0)

        assert result.assumed_efficiency == 0.85

    def test_efficiency_from_pump_properties(self):
        """Test pump efficiency from pump properties."""
        context = self._create_test_context()
        context.pump.efficiency = 0.90

        calculator = LiftCapacityCalculator(context)
        result = calculator.calculate(downhole_stroke=100.0)

        assert result.assumed_efficiency == 0.90

    def test_efficiency_default(self):
        """Test default pump efficiency is 100%."""
        context = self._create_test_context()
        calculator = LiftCapacityCalculator(context)
        result = calculator.calculate(downhole_stroke=100.0)

        assert result.assumed_efficiency == 1.0

    def test_efficiency_affects_capacity(self):
        """Test that efficiency affects lift capacity proportionally."""
        context = self._create_test_context()

        # 100% efficiency
        context.pump.efficiency = 1.0
        result_100 = calculate_lift_capacity(context, downhole_stroke=100.0)

        # 50% efficiency
        context.pump.efficiency = 0.5
        result_50 = calculate_lift_capacity(context, downhole_stroke=100.0)

        # Half efficiency should give half capacity
        ratio = result_100.lift_capacity / result_50.lift_capacity
        assert abs(ratio - 2.0) < 0.01

    def _create_test_context(self) -> DynacardAnalysisContext:
        """Create a test context with valid data."""
        surface_card = CardData(
            position=[0, 50, 100, 50],
            load=[5000, 10000, 15000, 10000],
        )

        rod_string = [
            RodSection(diameter=0.875, length=3000.0),
        ]

        pump = PumpProperties(
            diameter=1.75,
            depth=5000.0,
            efficiency=1.0,
        )

        surface_unit = SurfaceUnit(
            unit_type="Conventional",
            stroke_length=100.0,
        )

        return DynacardAnalysisContext(
            api14="TEST-LC-001",
            surface_card=surface_card,
            rod_string=rod_string,
            pump=pump,
            surface_unit=surface_unit,
            spm=6.0,
            runtime=24.0,
        )


class TestConvenienceFunction:
    """Tests for calculate_lift_capacity convenience function."""

    def test_returns_lift_capacity_analysis(self):
        """Test that convenience function returns LiftCapacityAnalysis."""
        context = self._create_test_context()
        result = calculate_lift_capacity(context, downhole_stroke=100.0)

        from digitalmodel.marine_ops.artificial_lift.dynacard.models import LiftCapacityAnalysis
        assert isinstance(result, LiftCapacityAnalysis)

    def test_with_fillage_analysis(self):
        """Test convenience function with fillage analysis."""
        context = self._create_test_context()
        fillage = PumpFillageAnalysis(gross_stroke=90.0)
        result = calculate_lift_capacity(context, fillage_analysis=fillage)

        assert result.stroke_length == 90.0

    def _create_test_context(self) -> DynacardAnalysisContext:
        """Create a test context."""
        return DynacardAnalysisContext(
            api14="TEST-LC-002",
            surface_card=CardData(position=[0, 100], load=[5000, 15000]),
            rod_string=[RodSection(diameter=0.875, length=3000.0)],
            pump=PumpProperties(diameter=1.75, depth=5000.0),
            surface_unit=SurfaceUnit(stroke_length=100.0),
            spm=6.0,
            runtime=24.0,
        )


class TestLiftCapacityWithRealData:
    """Tests using real well card data."""

    @pytest.fixture
    def well_7699227(self):
        """Load well 7699227 test data."""
        filepath = TEST_DATA_DIR / "7699227.json"
        if filepath.exists():
            return load_from_json_file(filepath)
        pytest.skip("Test data file 7699227.json not found")

    def test_lift_capacity_7699227(self, well_7699227):
        """Test lift capacity with well 7699227 data."""
        result = calculate_lift_capacity(well_7699227, downhole_stroke=100.0)

        # Should have valid lift capacity
        assert result.lift_capacity > 0
        assert result.plunger_diameter > 0
        assert result.spm_24hr_avg > 0

        # Typical rod pump: 50-500 BPD lift capacity
        assert 10 < result.lift_capacity < 1000


class TestLiftCapacityAnalysisModel:
    """Tests for LiftCapacityAnalysis model."""

    def test_analysis_defaults(self):
        """Test LiftCapacityAnalysis default values."""
        from digitalmodel.marine_ops.artificial_lift.dynacard.models import LiftCapacityAnalysis
        analysis = LiftCapacityAnalysis()
        assert analysis.lift_capacity == 0.0
        assert analysis.plunger_diameter == 0.0
        assert analysis.stroke_length == 0.0
        assert analysis.spm_24hr_avg == 0.0
        assert analysis.assumed_efficiency == 1.0

    def test_analysis_with_values(self):
        """Test LiftCapacityAnalysis with actual values."""
        from digitalmodel.marine_ops.artificial_lift.dynacard.models import LiftCapacityAnalysis
        analysis = LiftCapacityAnalysis(
            lift_capacity=250.5,
            plunger_diameter=1.75,
            stroke_length=95.0,
            spm_24hr_avg=5.5,
            assumed_efficiency=0.85,
        )
        assert analysis.lift_capacity == 250.5
        assert analysis.plunger_diameter == 1.75
        assert analysis.assumed_efficiency == 0.85

    def test_analysis_serialization(self):
        """Test that LiftCapacityAnalysis can be serialized."""
        from digitalmodel.marine_ops.artificial_lift.dynacard.models import LiftCapacityAnalysis
        analysis = LiftCapacityAnalysis(
            lift_capacity=250.5,
            plunger_diameter=1.75,
        )

        data = analysis.model_dump()
        assert data['lift_capacity'] == 250.5
        assert data['plunger_diameter'] == 1.75
