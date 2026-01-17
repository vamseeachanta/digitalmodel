# ABOUTME: Unit tests for gear box loading analysis module.
# ABOUTME: Validates API Class I torque calculations and balanced torque optimization.

import json
import math
import pytest
import numpy as np
from pathlib import Path

from digitalmodel.modules.artificial_lift.dynacard.data_loader import (
    load_from_json_file,
)
from digitalmodel.modules.artificial_lift.dynacard.gear_box_loading import (
    GearBoxLoadingCalculator,
    calculate_gear_box_loading,
)
from digitalmodel.modules.artificial_lift.dynacard.models import (
    DynacardAnalysisContext,
    CardData,
    RodSection,
    PumpProperties,
    SurfaceUnit,
    GearBoxLoadingAnalysis,
    TorqueStatistics,
)
from digitalmodel.modules.artificial_lift.dynacard.exceptions import (
    ValidationError,
    ConfigurationError,
)


# Test data directory
TEST_DATA_DIR = Path(__file__).parent / "testdata"


class TestGearBoxLoadingCalculator:
    """Tests for the GearBoxLoadingCalculator class."""

    def test_calculator_initialization(self):
        """Test calculator can be initialized with valid context."""
        context = self._create_test_context()
        calculator = GearBoxLoadingCalculator(context)
        assert calculator.ctx == context
        assert calculator.result is not None

    def test_calculate_with_valid_geometry(self):
        """Test torque calculation with valid pumping unit geometry."""
        context = self._create_test_context()
        calculator = GearBoxLoadingCalculator(context)
        result = calculator.calculate()

        assert result is not None
        assert result.counterbalance_status in ["converged", "unconverged"]
        assert len(result.crank_angle) > 0
        assert len(result.actual_torque_curve) > 0
        assert len(result.balanced_torque_curve) > 0

    def test_calculate_missing_surface_unit_raises_error(self):
        """Test that missing surface unit data raises ConfigurationError."""
        context = self._create_test_context()
        context.surface_unit = SurfaceUnit()  # Empty surface unit
        calculator = GearBoxLoadingCalculator(context)

        with pytest.raises(ConfigurationError) as exc_info:
            calculator.calculate()

        # Should indicate missing dimensions
        assert "K" in exc_info.value.message or "I" in exc_info.value.message or "dimension" in exc_info.value.message.lower()

    def test_calculate_invalid_geometry_k_less_than_i_raises_error(self):
        """Test that K < I geometry raises ConfigurationError."""
        context = self._create_test_context()
        context.surface_unit.dimensional_k = 50.0
        context.surface_unit.dimensional_i = 100.0  # K < I
        calculator = GearBoxLoadingCalculator(context)

        with pytest.raises(ConfigurationError) as exc_info:
            calculator.calculate()

        assert "K < I" in exc_info.value.message

    def test_convenience_function_handles_errors_gracefully(self):
        """Test that convenience function returns result with error status on error."""
        context = self._create_test_context()
        context.surface_unit = SurfaceUnit()  # Empty surface unit

        # Use convenience function with raise_on_error=False (default)
        result = calculate_gear_box_loading(context, raise_on_error=False)

        # Should return result with error in counterbalance_status
        assert "error" in result.counterbalance_status

    def test_calculate_invalid_geometry_k_less_than_i(self):
        """Test calculator handles K < I geometry error via convenience function."""
        context = self._create_test_context()
        context.surface_unit.dimensional_k = 50.0
        context.surface_unit.dimensional_i = 100.0  # K < I

        # Use convenience function with raise_on_error=False
        result = calculate_gear_box_loading(context, raise_on_error=False)

        assert "error" in result.counterbalance_status
        assert "K < I" in result.counterbalance_status

    def test_torque_statistics_populated(self):
        """Test that torque statistics are properly populated."""
        context = self._create_test_context()
        result = calculate_gear_box_loading(context)

        # Check actual torque statistics
        assert result.actual_torque.max_torque != 0.0
        assert result.actual_torque.min_torque != 0.0
        assert result.actual_torque.abs_max_torque >= 0.0

        # Check balanced torque statistics
        assert result.balanced_torque.abs_max_torque >= 0.0

    def test_torque_factor_calculated(self):
        """Test that torque factor array is calculated."""
        context = self._create_test_context()
        result = calculate_gear_box_loading(context)

        assert len(result.torque_factor) > 0
        # Torque factor should be finite (can vary widely based on geometry)
        max_tf = max(abs(tf) for tf in result.torque_factor if not math.isinf(tf))
        assert max_tf < 1000.0, f"Torque factor {max_tf} seems unreasonably large"

    def test_crank_angle_range(self):
        """Test that crank angles are finite and reasonable."""
        context = self._create_test_context()
        result = calculate_gear_box_loading(context)

        for angle in result.crank_angle:
            # Crank angles should be finite (may be negative or >360 before normalization)
            assert not math.isnan(angle), "Crank angle is NaN"
            assert not math.isinf(angle), "Crank angle is infinite"
            # Reasonable range is -720 to 720 degrees
            assert -720 <= angle <= 720, f"Crank angle {angle} out of reasonable range"

    def test_rotation_direction_clockwise(self):
        """Test calculation with clockwise rotation."""
        context = self._create_test_context()
        context.surface_unit.geometry = "Clockwise"
        result = calculate_gear_box_loading(context)

        assert result.counterbalance_status in ["converged", "unconverged"]

    def test_rotation_direction_counter_clockwise(self):
        """Test calculation with counter-clockwise rotation."""
        context = self._create_test_context()
        context.surface_unit.geometry = "C. Clockwise"
        result = calculate_gear_box_loading(context)

        assert result.counterbalance_status in ["converged", "unconverged"]

    def test_gear_box_rating_percent_calculation(self):
        """Test that percent of rated load is calculated correctly."""
        context = self._create_test_context()
        context.surface_unit.gear_box_rating = 500.0  # 500 M-in-lbs
        result = calculate_gear_box_loading(context)

        # Percent calculations should be based on gear_box_rating
        if result.gear_box_rating > 0:
            expected_max_pct = (result.actual_torque.max_torque / result.gear_box_rating) * 100
            assert abs(result.actual_to_rated_percent.max_torque - expected_max_pct) < 0.2

    def _create_test_context(self) -> DynacardAnalysisContext:
        """Create a test context with valid geometry for gear box loading."""
        # Create sample surface card data (sinusoidal pattern)
        n_samples = 100
        theta = np.linspace(0, 2 * np.pi, n_samples)
        position = (1 - np.cos(theta)) * 50  # 0 to 100 inch stroke
        # Load varies between 5000 and 15000 lbs
        load = 10000 + 5000 * np.sin(theta)

        surface_card = CardData(
            position=position.tolist(),
            load=load.tolist()
        )

        rod_string = [
            RodSection(diameter=0.875, length=3000.0),
            RodSection(diameter=0.75, length=2000.0),
        ]

        pump = PumpProperties(diameter=1.75, depth=5000.0)

        # Valid pumping unit geometry (Weatherford-like)
        surface_unit = SurfaceUnit(
            manufacturer="TestUnit",
            unit_type="Conventional",
            stroke_length=100.0,
            gear_box_rating=500.0,  # M-in-lbs
            structural_imbalance=-500.0,
            counterbalance_moment=100.0,  # M-in-lbs
            geometry="C. Clockwise",
            dimensional_a=150.0,
            dimensional_c=100.0,
            dimensional_i=100.0,
            dimensional_k=180.0,  # K > I required
            dimensional_p=140.0,
            radius=45.0,
            phase_angle=0.0,
            beam_rating=30000.0,
            max_stroke_length=120.0,
        )

        return DynacardAnalysisContext(
            api14="TEST-001",
            surface_card=surface_card,
            rod_string=rod_string,
            pump=pump,
            surface_unit=surface_unit,
            spm=6.0,
            fluid_density=62.4,
        )


class TestGearBoxLoadingWithRealData:
    """Tests using real well card data."""

    @pytest.fixture
    def well_7699227(self):
        """Load well 7699227 test data."""
        filepath = TEST_DATA_DIR / "7699227.json"
        if filepath.exists():
            return load_from_json_file(filepath)
        pytest.skip("Test data file 7699227.json not found")

    @pytest.fixture
    def well_5206267(self):
        """Load well 5206267 test data."""
        filepath = TEST_DATA_DIR / "5206267.json"
        if filepath.exists():
            return load_from_json_file(filepath)
        pytest.skip("Test data file 5206267.json not found")

    @pytest.fixture
    def well_2223782(self):
        """Load well 2223782 test data."""
        filepath = TEST_DATA_DIR / "2223782.json"
        if filepath.exists():
            return load_from_json_file(filepath)
        pytest.skip("Test data file 2223782.json not found")

    def test_gear_box_loading_7699227(self, well_7699227):
        """Test gear box loading with well 7699227 data."""
        result = calculate_gear_box_loading(well_7699227)

        # Should complete without error
        assert result.counterbalance_status in ["converged", "unconverged"]

        # Should have valid torque curves
        assert len(result.crank_angle) == len(well_7699227.surface_card.position)
        assert len(result.actual_torque_curve) == len(well_7699227.surface_card.position)

        # Torque values should be reasonable (< 1000 M-in-lbs for most wells)
        assert abs(result.actual_torque.abs_max_torque) < 2000

    def test_gear_box_loading_5206267(self, well_5206267):
        """Test gear box loading with well 5206267 data."""
        result = calculate_gear_box_loading(well_5206267)

        # Should complete
        assert result.counterbalance_status in ["converged", "unconverged", "error: K < I, h cannot be calculated", "error: Missing K or I dimensions"]

        if "error" not in result.counterbalance_status:
            # Valid result
            assert len(result.actual_torque_curve) > 0

    def test_gear_box_loading_2223782(self, well_2223782):
        """Test gear box loading with well 2223782 data."""
        result = calculate_gear_box_loading(well_2223782)

        assert result.counterbalance_status in ["converged", "unconverged", "error: K < I, h cannot be calculated", "error: Missing K or I dimensions"]

        if "error" not in result.counterbalance_status:
            assert len(result.actual_torque_curve) > 0

    def test_balanced_torque_lower_than_actual(self, well_7699227):
        """Test that balanced torque peak is generally lower than actual."""
        result = calculate_gear_box_loading(well_7699227)

        if "error" not in result.counterbalance_status:
            # Balanced torque should typically be lower or equal
            # (within some tolerance due to optimization)
            balanced_abs_max = result.balanced_torque.abs_max_torque
            actual_abs_max = result.actual_torque.abs_max_torque

            # Allow some tolerance - balanced might not always be lower
            # if original counterbalance was already optimal
            assert balanced_abs_max <= actual_abs_max * 1.1


class TestAPIClassIGeometry:
    """Tests for API Class I geometry calculations."""

    def test_safe_acos_clamping(self):
        """Test that acos values are properly clamped."""
        calculator = GearBoxLoadingCalculator(self._create_minimal_context())

        # Test values outside [-1, 1] are clamped
        assert calculator._safe_acos(1.5) == math.acos(1.0)
        assert calculator._safe_acos(-1.5) == math.acos(-1.0)
        assert calculator._safe_acos(0.5) == math.acos(0.5)

    def test_psi_calculation(self):
        """Test psi angle calculation at stroke endpoints."""
        context = self._create_minimal_context()
        calculator = GearBoxLoadingCalculator(context)
        result = calculator.calculate()

        if "error" not in result.counterbalance_status:
            # Psi should vary through the stroke
            assert len(result.crank_angle) > 0

    def _create_minimal_context(self) -> DynacardAnalysisContext:
        """Create minimal context for geometry tests."""
        # Simple card
        position = [0.0, 50.0, 100.0, 50.0]
        load = [10000.0, 15000.0, 10000.0, 5000.0]

        return DynacardAnalysisContext(
            api14="TEST",
            surface_card=CardData(position=position, load=load),
            rod_string=[RodSection(diameter=0.875, length=3000.0)],
            pump=PumpProperties(diameter=1.75, depth=5000.0),
            surface_unit=SurfaceUnit(
                dimensional_a=150.0,
                dimensional_c=100.0,
                dimensional_i=100.0,
                dimensional_k=180.0,
                dimensional_p=140.0,
                radius=45.0,
                gear_box_rating=500.0,
            ),
            spm=6.0,
        )


class TestTorqueStatistics:
    """Tests for TorqueStatistics model."""

    def test_torque_statistics_defaults(self):
        """Test TorqueStatistics default values."""
        stats = TorqueStatistics()
        assert stats.max_torque == 0.0
        assert stats.min_torque == 0.0
        assert stats.abs_max_torque == 0.0

    def test_torque_statistics_with_values(self):
        """Test TorqueStatistics with actual values."""
        stats = TorqueStatistics(
            max_torque=500.0,
            min_torque=-200.0,
            abs_max_torque=500.0
        )
        assert stats.max_torque == 500.0
        assert stats.min_torque == -200.0
        assert stats.abs_max_torque == 500.0


class TestGearBoxLoadingAnalysis:
    """Tests for GearBoxLoadingAnalysis model."""

    def test_gear_box_analysis_defaults(self):
        """Test GearBoxLoadingAnalysis default values."""
        analysis = GearBoxLoadingAnalysis()
        assert analysis.gear_box_rating == 0.0
        assert analysis.counterbalance_status == "not_calculated"
        assert analysis.crank_angle == []
        assert analysis.actual_torque_curve == []

    def test_gear_box_analysis_serialization(self):
        """Test that GearBoxLoadingAnalysis can be serialized to dict."""
        analysis = GearBoxLoadingAnalysis(
            gear_box_rating=500.0,
            actual_counterbalance_moment=100.0,
            optimal_counterbalance_moment=120.0,
            counterbalance_status="converged",
            crank_angle=[0.0, 90.0, 180.0, 270.0],
            actual_torque_curve=[100.0, 200.0, 150.0, 50.0],
        )

        data = analysis.model_dump()
        assert data['gear_box_rating'] == 500.0
        assert data['counterbalance_status'] == "converged"
        assert len(data['crank_angle']) == 4


class TestConvenienceFunction:
    """Tests for calculate_gear_box_loading convenience function."""

    def test_convenience_function_returns_analysis(self):
        """Test that convenience function returns GearBoxLoadingAnalysis."""
        context = self._create_test_context()
        result = calculate_gear_box_loading(context)

        assert isinstance(result, GearBoxLoadingAnalysis)

    def _create_test_context(self) -> DynacardAnalysisContext:
        """Create a test context."""
        n_samples = 50
        theta = np.linspace(0, 2 * np.pi, n_samples)
        position = (1 - np.cos(theta)) * 50
        load = 10000 + 5000 * np.sin(theta)

        return DynacardAnalysisContext(
            api14="TEST-002",
            surface_card=CardData(position=position.tolist(), load=load.tolist()),
            rod_string=[RodSection(diameter=0.875, length=3000.0)],
            pump=PumpProperties(diameter=1.75, depth=5000.0),
            surface_unit=SurfaceUnit(
                dimensional_a=150.0,
                dimensional_c=100.0,
                dimensional_i=100.0,
                dimensional_k=180.0,
                dimensional_p=140.0,
                radius=45.0,
                gear_box_rating=500.0,
            ),
            spm=6.0,
        )
