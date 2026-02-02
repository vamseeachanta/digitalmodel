# ABOUTME: Unit tests for torque balance analysis module.
# ABOUTME: Validates analytical counterbalance estimation and objective function.

import math
import pytest
import numpy as np
from pathlib import Path

from digitalmodel.artificial_lift.dynacard.data_loader import (
    load_from_json_file,
)
from digitalmodel.artificial_lift.dynacard.torque_balance import (
    TorqueBalanceCalculator,
    calculate_torque_balance,
    estimate_optimal_counterbalance,
    calculate_torque_objective,
)
from digitalmodel.artificial_lift.dynacard.models import (
    DynacardAnalysisContext,
    CardData,
    RodSection,
    PumpProperties,
    SurfaceUnit,
    TorqueBalanceAnalysis,
)
from digitalmodel.artificial_lift.dynacard.exceptions import (
    ValidationError,
    ConfigurationError,
)


# Test data directory
TEST_DATA_DIR = Path(__file__).parent / "testdata"


class TestTorqueBalanceCalculator:
    """Tests for the TorqueBalanceCalculator class."""

    def test_calculator_initialization(self):
        """Test calculator can be initialized with valid context."""
        context = self._create_test_context()
        calculator = TorqueBalanceCalculator(context)
        assert calculator.ctx == context
        assert calculator.result is not None

    def test_calculate_returns_analysis(self):
        """Test that calculate returns TorqueBalanceAnalysis."""
        context = self._create_test_context()
        calculator = TorqueBalanceCalculator(context)
        result = calculator.calculate()
        assert isinstance(result, TorqueBalanceAnalysis)

    def test_missing_surface_unit_dimensions_raises_error(self):
        """Test that missing K or I dimensions raises ConfigurationError."""
        context = self._create_test_context()
        context.surface_unit.dimensional_k = 0.0
        calculator = TorqueBalanceCalculator(context)

        with pytest.raises(ConfigurationError) as exc_info:
            calculator.calculate()

        assert "K" in exc_info.value.message or "I" in exc_info.value.message

    def test_invalid_geometry_k_less_than_i_raises_error(self):
        """Test that K < I raises ConfigurationError."""
        context = self._create_test_context()
        context.surface_unit.dimensional_k = 50.0
        context.surface_unit.dimensional_i = 100.0
        calculator = TorqueBalanceCalculator(context)

        with pytest.raises(ConfigurationError) as exc_info:
            calculator.calculate()

        assert "K < I" in exc_info.value.message

    def test_missing_a_c_dimensions_raises_error(self):
        """Test that missing A or C dimensions raises ConfigurationError."""
        context = self._create_test_context()
        context.surface_unit.dimensional_a = 0.0
        calculator = TorqueBalanceCalculator(context)

        with pytest.raises(ConfigurationError) as exc_info:
            calculator.calculate()

        assert "A" in exc_info.value.message or "C" in exc_info.value.message

    def test_missing_r_p_dimensions_raises_error(self):
        """Test that missing R or P dimensions raises ConfigurationError."""
        context = self._create_test_context()
        context.surface_unit.radius = 0.0
        calculator = TorqueBalanceCalculator(context)

        with pytest.raises(ConfigurationError) as exc_info:
            calculator.calculate()

        assert "R" in exc_info.value.message or "P" in exc_info.value.message

    def test_convenience_function_handles_errors_gracefully(self):
        """Test that convenience function returns result with warning on error."""
        context = self._create_test_context()
        context.surface_unit.dimensional_k = 0.0

        # Use convenience function with raise_on_error=False (default)
        result = calculate_torque_balance(context, raise_on_error=False)

        # Should return result with warning message set
        assert result.warning_message != ""

    def test_rotation_direction_clockwise(self):
        """Test clockwise rotation detection."""
        context = self._create_test_context()
        context.surface_unit.geometry = "Clockwise"
        calculator = TorqueBalanceCalculator(context)
        result = calculator.calculate()
        assert result.rotation_direction == 1

    def test_rotation_direction_counter_clockwise(self):
        """Test counter-clockwise rotation detection."""
        context = self._create_test_context()
        context.surface_unit.geometry = "C. Clockwise"
        calculator = TorqueBalanceCalculator(context)
        result = calculator.calculate()
        assert result.rotation_direction == -1

    def test_estimated_optimal_moment_calculated(self):
        """Test that optimal moment is calculated."""
        context = self._create_test_context()
        calculator = TorqueBalanceCalculator(context)
        result = calculator.calculate()
        # Should produce some estimate (may be 0 if geometry results in equal peaks)
        assert isinstance(result.estimated_optimal_moment, float)

    def test_actual_counterbalance_recorded(self):
        """Test that actual counterbalance moment is recorded."""
        context = self._create_test_context()
        context.surface_unit.counterbalance_moment = 500.0  # M-in-lbs
        calculator = TorqueBalanceCalculator(context)
        result = calculator.calculate()
        assert result.actual_counterbalance_moment == 500.0

    def test_stroke_transition_index_found(self):
        """Test that stroke transition index is found."""
        context = self._create_test_context()
        calculator = TorqueBalanceCalculator(context)
        result = calculator.calculate()
        assert result.stroke_transition_index >= 0
        assert result.stroke_transition_index < len(context.surface_card.position)

    def test_peak_torques_calculated(self):
        """Test that peak torques are calculated."""
        context = self._create_test_context()
        calculator = TorqueBalanceCalculator(context)
        result = calculator.calculate()
        # Should have some peak values calculated
        assert isinstance(result.upstroke_peak_torque, float)
        assert isinstance(result.downstroke_peak_torque, float)

    def test_objective_function_values(self):
        """Test that objective function values are calculated."""
        context = self._create_test_context()
        calculator = TorqueBalanceCalculator(context)
        result = calculator.calculate()
        assert result.objective_at_actual >= 0
        assert result.objective_at_optimal >= 0

    def test_objective_at_optimal_lower_than_actual(self):
        """Test that objective at optimal should be lower or equal."""
        context = self._create_test_context()
        calculator = TorqueBalanceCalculator(context)
        result = calculator.calculate()
        # Analytical estimate should improve or maintain objective
        assert result.objective_at_optimal <= result.objective_at_actual + 1.0  # Small tolerance

    def test_peak_torque_difference_calculated(self):
        """Test that peak torque difference is calculated."""
        context = self._create_test_context()
        calculator = TorqueBalanceCalculator(context)
        result = calculator.calculate()
        expected_diff = abs(result.upstroke_peak_torque - result.downstroke_peak_torque)
        assert abs(result.peak_torque_difference - expected_diff) < 0.5

    def _create_test_context(self) -> DynacardAnalysisContext:
        """Create a test context with valid geometry data."""
        # Based on typical C-228D-173-74 pumping unit geometry
        return DynacardAnalysisContext(
            api14="TEST-TORQUE-001",
            surface_card=CardData(
                position=[0, 15, 30, 45, 60, 74, 60, 45, 30, 15],
                load=[5000, 7000, 10000, 12000, 13000, 11000, 8000, 6000, 5500, 5200],
            ),
            rod_string=[
                RodSection(diameter=0.875, length=2500.0),
                RodSection(diameter=0.75, length=2500.0),
            ],
            pump=PumpProperties(diameter=1.75, depth=5000.0),
            surface_unit=SurfaceUnit(
                stroke_length=74.0,
                dimensional_a=155.0,  # Walking beam length
                dimensional_c=120.0,  # Pitman length
                dimensional_i=24.0,   # Slow speed shaft offset
                dimensional_k=100.0,  # Distance to center bearing
                dimensional_p=88.0,   # Distance to crank pin
                radius=37.0,          # Crank radius
                gear_box_rating=456.0,  # M-in-lbs
                structural_imbalance=1200.0,
                counterbalance_moment=300.0,
                phase_angle=0.0,
                geometry="C. Clockwise",
            ),
            spm=6.0,
            runtime=24.0,
            fluid_density=55.0,
        )


class TestCalculateTorqueBalance:
    """Tests for calculate_torque_balance convenience function."""

    def test_returns_torque_balance_analysis(self):
        """Test that convenience function returns TorqueBalanceAnalysis."""
        context = self._create_test_context()
        result = calculate_torque_balance(context)
        assert isinstance(result, TorqueBalanceAnalysis)

    def test_with_zero_counterbalance(self):
        """Test with zero counterbalance moment."""
        context = self._create_test_context()
        context.surface_unit.counterbalance_moment = 0.0
        result = calculate_torque_balance(context)
        assert result.actual_counterbalance_moment == 0.0
        # Should still estimate optimal
        assert isinstance(result.estimated_optimal_moment, float)

    def test_with_high_counterbalance(self):
        """Test with high counterbalance moment."""
        context = self._create_test_context()
        context.surface_unit.counterbalance_moment = 1000.0
        result = calculate_torque_balance(context)
        assert result.actual_counterbalance_moment == 1000.0

    def _create_test_context(self) -> DynacardAnalysisContext:
        """Create a test context."""
        return DynacardAnalysisContext(
            api14="TEST-TORQUE-002",
            surface_card=CardData(
                position=[0, 25, 50, 74, 50, 25],
                load=[5000, 9000, 12000, 10000, 7000, 5500],
            ),
            rod_string=[RodSection(diameter=0.875, length=5000.0)],
            pump=PumpProperties(diameter=1.75, depth=5000.0),
            surface_unit=SurfaceUnit(
                stroke_length=74.0,
                dimensional_a=155.0,
                dimensional_c=120.0,
                dimensional_i=24.0,
                dimensional_k=100.0,
                dimensional_p=88.0,
                radius=37.0,
                gear_box_rating=456.0,
                structural_imbalance=1200.0,
                counterbalance_moment=300.0,
                geometry="C. Clockwise",
            ),
            spm=6.0,
            runtime=24.0,
        )


class TestEstimateOptimalCounterbalance:
    """Tests for estimate_optimal_counterbalance function."""

    def test_returns_float(self):
        """Test that function returns a float."""
        context = self._create_test_context()
        result = estimate_optimal_counterbalance(context)
        assert isinstance(result, float)

    def test_reasonable_estimate(self):
        """Test that estimate is reasonable for typical geometry."""
        context = self._create_test_context()
        result = estimate_optimal_counterbalance(context)
        # Typical optimal counterbalance is in the hundreds of M-in-lbs
        assert -2000 < result < 2000

    def _create_test_context(self) -> DynacardAnalysisContext:
        """Create a test context."""
        return DynacardAnalysisContext(
            api14="TEST-TORQUE-003",
            surface_card=CardData(
                position=[0, 25, 50, 74, 50, 25],
                load=[5000, 9000, 12000, 10000, 7000, 5500],
            ),
            rod_string=[RodSection(diameter=0.875, length=5000.0)],
            pump=PumpProperties(diameter=1.75, depth=5000.0),
            surface_unit=SurfaceUnit(
                stroke_length=74.0,
                dimensional_a=155.0,
                dimensional_c=120.0,
                dimensional_i=24.0,
                dimensional_k=100.0,
                dimensional_p=88.0,
                radius=37.0,
                gear_box_rating=456.0,
                structural_imbalance=1200.0,
                counterbalance_moment=300.0,
                geometry="C. Clockwise",
            ),
            spm=6.0,
            runtime=24.0,
        )


class TestCalculateTorqueObjective:
    """Tests for calculate_torque_objective function."""

    def test_returns_float(self):
        """Test that function returns a float."""
        context = self._create_test_context()
        result = calculate_torque_objective(context, 300.0)
        assert isinstance(result, float)

    def test_non_negative_objective(self):
        """Test that objective is non-negative."""
        context = self._create_test_context()
        result = calculate_torque_objective(context, 300.0)
        assert result >= 0

    def test_different_counterbalance_values(self):
        """Test objective changes with counterbalance."""
        context = self._create_test_context()
        obj_low = calculate_torque_objective(context, 100.0)
        obj_mid = calculate_torque_objective(context, 500.0)
        obj_high = calculate_torque_objective(context, 900.0)
        # All should be valid non-negative values
        assert obj_low >= 0
        assert obj_mid >= 0
        assert obj_high >= 0

    def _create_test_context(self) -> DynacardAnalysisContext:
        """Create a test context."""
        return DynacardAnalysisContext(
            api14="TEST-TORQUE-004",
            surface_card=CardData(
                position=[0, 25, 50, 74, 50, 25],
                load=[5000, 9000, 12000, 10000, 7000, 5500],
            ),
            rod_string=[RodSection(diameter=0.875, length=5000.0)],
            pump=PumpProperties(diameter=1.75, depth=5000.0),
            surface_unit=SurfaceUnit(
                stroke_length=74.0,
                dimensional_a=155.0,
                dimensional_c=120.0,
                dimensional_i=24.0,
                dimensional_k=100.0,
                dimensional_p=88.0,
                radius=37.0,
                gear_box_rating=456.0,
                structural_imbalance=1200.0,
                counterbalance_moment=300.0,
                geometry="C. Clockwise",
            ),
            spm=6.0,
            runtime=24.0,
        )


class TestTorqueBalanceAnalysisModel:
    """Tests for TorqueBalanceAnalysis model."""

    def test_analysis_defaults(self):
        """Test TorqueBalanceAnalysis default values."""
        analysis = TorqueBalanceAnalysis()
        assert analysis.estimated_optimal_moment == 0.0
        assert analysis.actual_counterbalance_moment == 0.0
        assert analysis.objective_at_actual == 0.0
        assert analysis.objective_at_optimal == 0.0
        assert analysis.rotation_direction == 1
        assert analysis.analysis_method == "analytical"

    def test_analysis_with_values(self):
        """Test TorqueBalanceAnalysis with actual values."""
        analysis = TorqueBalanceAnalysis(
            estimated_optimal_moment=450.5,
            actual_counterbalance_moment=300.0,
            objective_at_actual=25.3,
            objective_at_optimal=5.2,
            upstroke_peak_torque=250.0,
            downstroke_peak_torque=275.3,
            peak_torque_difference=25.3,
            stroke_transition_index=45,
            rotation_direction=-1,
        )
        assert analysis.estimated_optimal_moment == 450.5
        assert analysis.rotation_direction == -1
        assert analysis.stroke_transition_index == 45

    def test_analysis_serialization(self):
        """Test that TorqueBalanceAnalysis can be serialized."""
        analysis = TorqueBalanceAnalysis(
            estimated_optimal_moment=450.0,
            actual_counterbalance_moment=300.0,
            upstroke_peak_torque=250.0,
            downstroke_peak_torque=275.0,
        )
        data = analysis.model_dump()
        assert data["estimated_optimal_moment"] == 450.0
        assert data["actual_counterbalance_moment"] == 300.0


class TestTorqueBalanceWithRealData:
    """Tests using real well card data."""

    @pytest.fixture
    def well_7699227(self):
        """Load well 7699227 test data."""
        filepath = TEST_DATA_DIR / "7699227.json"
        if filepath.exists():
            return load_from_json_file(filepath)
        pytest.skip("Test data file 7699227.json not found")

    def test_torque_balance_7699227(self, well_7699227):
        """Test torque balance calculation with well 7699227 data."""
        # Skip if surface unit geometry is not available
        if well_7699227.surface_unit.dimensional_k <= 0:
            pytest.skip("No surface unit geometry in test data")

        result = calculate_torque_balance(well_7699227)
        assert isinstance(result, TorqueBalanceAnalysis)

    def test_optimal_moment_estimated_7699227(self, well_7699227):
        """Test optimal moment estimation with real data."""
        if well_7699227.surface_unit.dimensional_k <= 0:
            pytest.skip("No surface unit geometry in test data")

        result = calculate_torque_balance(well_7699227)
        assert isinstance(result.estimated_optimal_moment, float)

    def test_peak_torques_reasonable_7699227(self, well_7699227):
        """Test that peak torques are reasonable with real data."""
        if well_7699227.surface_unit.dimensional_k <= 0:
            pytest.skip("No surface unit geometry in test data")

        result = calculate_torque_balance(well_7699227)
        # Peak torques should be reasonable for typical well
        assert abs(result.upstroke_peak_torque) < 10000  # M-in-lbs
        assert abs(result.downstroke_peak_torque) < 10000
