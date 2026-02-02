# ABOUTME: Unit tests for power consumption analysis module.
# ABOUTME: Validates card area calculation, motor design detection, and energy estimates.

import math
import pytest
import numpy as np
from pathlib import Path

from digitalmodel.artificial_lift.dynacard.data_loader import (
    load_from_json_file,
)
from digitalmodel.artificial_lift.dynacard.power_consumption import (
    PowerConsumptionCalculator,
    calculate_power_consumption,
    calculate_card_area,
    F_CL_TABLE,
)
from digitalmodel.artificial_lift.dynacard.models import (
    DynacardAnalysisContext,
    CardData,
    RodSection,
    PumpProperties,
    SurfaceUnit,
    MotorProperties,
    CalculationParameters,
    PowerConsumptionAnalysis,
)
from digitalmodel.artificial_lift.dynacard.exceptions import ValidationError


# Test data directory
TEST_DATA_DIR = Path(__file__).parent / "testdata"


class TestPowerConsumptionCalculator:
    """Tests for the PowerConsumptionCalculator class."""

    def test_calculator_initialization(self):
        """Test calculator can be initialized with valid context."""
        context = self._create_test_context()
        calculator = PowerConsumptionCalculator(context)
        assert calculator.ctx == context
        assert calculator.result is not None

    def test_calculate_with_valid_data(self):
        """Test power calculation with valid card data."""
        context = self._create_test_context()
        calculator = PowerConsumptionCalculator(context)
        result = calculator.calculate()

        assert result is not None
        assert result.card_area > 0
        assert result.polished_rod_horsepower > 0
        assert result.prime_mover_horsepower > 0
        assert result.power_consumption_kw > 0
        assert result.daily_energy_consumption > 0

    def test_calculate_missing_card(self):
        """Test calculator raises ValidationError for missing surface card."""
        context = self._create_test_context()
        context.surface_card = CardData(position=[], load=[])
        calculator = PowerConsumptionCalculator(context)

        with pytest.raises(ValidationError) as exc_info:
            calculator.calculate()

        assert "surface_card" in exc_info.value.message.lower() or "no data" in exc_info.value.message.lower()

    def test_calculate_invalid_spm(self):
        """Test calculator raises ValidationError for zero/negative SPM."""
        context = self._create_test_context()
        context.spm = 0.0
        calculator = PowerConsumptionCalculator(context)

        with pytest.raises(ValidationError) as exc_info:
            calculator.calculate()

        assert "spm" in exc_info.value.message.lower()

    def test_cyclic_load_factor_lookup(self):
        """Test F_CL lookup table values."""
        # Mark II with NEMA B
        assert F_CL_TABLE['Mark II']['NEMA B'] == 1.517
        # Mark II with NEMA D
        assert F_CL_TABLE['Mark II']['NEMA D'] == 1.1
        # Others with NEMA B
        assert F_CL_TABLE['Others']['NEMA B'] == 1.897
        # Others with NEMA D
        assert F_CL_TABLE['Others']['NEMA D'] == 1.375

    def test_motor_design_detection_mark_ii(self):
        """Test Mark II motor design detection."""
        context = self._create_test_context()
        context.surface_unit.unit_type = "Mark II"
        calculator = PowerConsumptionCalculator(context)
        result = calculator.calculate()

        assert result.motor_design == "Mark II"

    def test_motor_design_detection_conventional(self):
        """Test conventional unit is detected as Others."""
        context = self._create_test_context()
        context.surface_unit.unit_type = "Conventional"
        calculator = PowerConsumptionCalculator(context)
        result = calculator.calculate()

        assert result.motor_design == "Others"

    def test_nema_code_detection_b(self):
        """Test NEMA B motor detection."""
        context = self._create_test_context()
        context.motor = MotorProperties(model="NEMA B General Purpose")
        calculator = PowerConsumptionCalculator(context)
        result = calculator.calculate()

        assert result.nema_code == "NEMA B"

    def test_nema_code_detection_d(self):
        """Test NEMA D motor detection."""
        context = self._create_test_context()
        context.motor = MotorProperties(model="NEMA D High Slip")
        calculator = PowerConsumptionCalculator(context)
        result = calculator.calculate()

        assert result.nema_code == "NEMA D"

    def test_nema_code_default(self):
        """Test default NEMA code when motor model not specified."""
        context = self._create_test_context()
        context.motor = MotorProperties(model="")
        calculator = PowerConsumptionCalculator(context)
        result = calculator.calculate()

        assert result.nema_code == "NEMA B"

    def test_efficiency_factors_applied(self):
        """Test that efficiency factors affect power calculation."""
        context = self._create_test_context()

        # Calculate with default efficiencies
        result1 = calculate_power_consumption(context)

        # Calculate with lower efficiencies
        context.calc_params.efficiency_prime_mover = 0.70
        context.calc_params.efficiency_pumping_unit = 0.80
        result2 = calculate_power_consumption(context)

        # Lower efficiency should result in higher power requirement
        assert result2.prime_mover_horsepower > result1.prime_mover_horsepower

    def test_runtime_affects_daily_consumption(self):
        """Test that runtime affects daily energy consumption."""
        context = self._create_test_context()

        # 24-hour runtime
        context.runtime = 24.0
        result1 = calculate_power_consumption(context)

        # 12-hour runtime
        context.runtime = 12.0
        result2 = calculate_power_consumption(context)

        # Half runtime should give half daily consumption
        ratio = result1.daily_energy_consumption / result2.daily_energy_consumption
        assert abs(ratio - 2.0) < 0.01

    def _create_test_context(self) -> DynacardAnalysisContext:
        """Create a test context with valid card data."""
        # Create sample surface card (rectangular pump card)
        # This represents a simplified pump stroke
        n_samples = 100
        theta = np.linspace(0, 2 * np.pi, n_samples)

        # Approximate pump card shape
        position = (1 - np.cos(theta)) * 50  # 0 to 100 inch stroke
        load = 10000 + 5000 * np.sin(theta)  # 5000 to 15000 lbs

        surface_card = CardData(
            position=position.tolist(),
            load=load.tolist()
        )

        rod_string = [
            RodSection(diameter=0.875, length=3000.0),
            RodSection(diameter=0.75, length=2000.0),
        ]

        pump = PumpProperties(diameter=1.75, depth=5000.0)

        surface_unit = SurfaceUnit(
            manufacturer="TestUnit",
            unit_type="Conventional",
            stroke_length=100.0,
        )

        motor = MotorProperties(
            model="NEMA B",
            horsepower=50.0,
        )

        calc_params = CalculationParameters(
            efficiency_prime_mover=0.85,
            efficiency_pumping_unit=0.90,
        )

        return DynacardAnalysisContext(
            api14="TEST-001",
            surface_card=surface_card,
            rod_string=rod_string,
            pump=pump,
            surface_unit=surface_unit,
            motor=motor,
            spm=6.0,
            runtime=24.0,
            fluid_density=62.4,
            calc_params=calc_params,
        )


class TestCardAreaCalculation:
    """Tests for card area calculation using shoelace formula."""

    def test_card_area_rectangle(self):
        """Test card area for a perfect rectangle."""
        # Rectangle: 100 inches position, 10000 lbs load
        position = [0, 100, 100, 0]
        load = [5000, 5000, 15000, 15000]

        card = CardData(position=position, load=load)
        area = calculate_card_area(card)

        # Expected: 100 * 10000 = 1,000,000 in-lbs = 83,333.33 ft-lbs
        expected = 100 * 10000 / 12.0
        assert abs(area - expected) < 1.0

    def test_card_area_positive(self):
        """Test that card area is always positive."""
        # Reversed card (counter-clockwise)
        position = [0, 0, 100, 100]
        load = [5000, 15000, 15000, 5000]

        card = CardData(position=position, load=load)
        area = calculate_card_area(card)

        assert area > 0

    def test_card_area_sinusoidal(self):
        """Test card area for sinusoidal pump card."""
        n = 100
        theta = np.linspace(0, 2 * np.pi, n)
        position = (1 - np.cos(theta)) * 50
        load = 10000 + 5000 * np.sin(theta)

        card = CardData(position=position.tolist(), load=load.tolist())
        area = calculate_card_area(card)

        # Area should be positive and reasonable
        assert area > 0
        # Typical pump card area is 10,000-100,000 ft-lbs
        assert 1000 < area < 500000


class TestPowerConsumptionWithRealData:
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

    def test_power_consumption_7699227(self, well_7699227):
        """Test power consumption with well 7699227 data."""
        result = calculate_power_consumption(well_7699227)

        # Should complete without error
        assert "error" not in result.motor_design

        # Should have valid power values
        assert result.card_area > 0
        assert result.polished_rod_horsepower > 0
        assert result.prime_mover_horsepower > 0
        assert result.power_consumption_kw > 0
        assert result.daily_energy_consumption > 0

        # Power values should be reasonable for a rod pump
        # Typical rod pump: 5-100 HP
        assert 1 < result.prime_mover_horsepower < 500

    def test_power_consumption_5206267(self, well_5206267):
        """Test power consumption with well 5206267 data."""
        result = calculate_power_consumption(well_5206267)

        # Should complete without error
        assert "error" not in result.motor_design

        # Should have valid power values
        assert result.polished_rod_horsepower >= 0
        assert result.daily_energy_consumption >= 0

    def test_card_area_relationship(self, well_7699227):
        """Test that larger card area = more power."""
        result = calculate_power_consumption(well_7699227)

        # Power is proportional to card area * SPM
        # P_PR = (card_area * SPM) / 33000
        expected_pr = (result.card_area * well_7699227.spm) / 33000
        assert abs(result.polished_rod_horsepower - expected_pr) < 0.01


class TestPowerConsumptionAnalysisModel:
    """Tests for PowerConsumptionAnalysis model."""

    def test_analysis_defaults(self):
        """Test PowerConsumptionAnalysis default values."""
        analysis = PowerConsumptionAnalysis()
        assert analysis.card_area == 0.0
        assert analysis.polished_rod_horsepower == 0.0
        assert analysis.prime_mover_horsepower == 0.0
        assert analysis.power_consumption_kw == 0.0
        assert analysis.daily_energy_consumption == 0.0
        assert analysis.motor_design == "Others"
        assert analysis.nema_code == "NEMA B"
        assert analysis.cyclic_load_factor == 0.0

    def test_analysis_with_values(self):
        """Test PowerConsumptionAnalysis with actual values."""
        analysis = PowerConsumptionAnalysis(
            card_area=50000.0,
            polished_rod_horsepower=10.0,
            prime_mover_horsepower=25.0,
            power_consumption_kw=18.64,
            daily_energy_consumption=447.4,
            motor_design="Mark II",
            nema_code="NEMA D",
            cyclic_load_factor=1.1
        )
        assert analysis.card_area == 50000.0
        assert analysis.prime_mover_horsepower == 25.0
        assert analysis.motor_design == "Mark II"
        assert analysis.nema_code == "NEMA D"

    def test_analysis_serialization(self):
        """Test that PowerConsumptionAnalysis can be serialized."""
        analysis = PowerConsumptionAnalysis(
            card_area=50000.0,
            polished_rod_horsepower=10.0,
            prime_mover_horsepower=25.0,
            power_consumption_kw=18.64,
            daily_energy_consumption=447.4,
        )

        data = analysis.model_dump()
        assert data['card_area'] == 50000.0
        assert data['power_consumption_kw'] == 18.64


class TestConvenienceFunction:
    """Tests for calculate_power_consumption convenience function."""

    def test_convenience_function_returns_analysis(self):
        """Test that convenience function returns PowerConsumptionAnalysis."""
        context = self._create_test_context()
        result = calculate_power_consumption(context)

        assert isinstance(result, PowerConsumptionAnalysis)

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
            surface_unit=SurfaceUnit(),
            motor=MotorProperties(model="NEMA B"),
            spm=6.0,
            runtime=24.0,
        )


class TestMotorProperties:
    """Tests for MotorProperties model."""

    def test_motor_defaults(self):
        """Test MotorProperties default values."""
        motor = MotorProperties()
        assert motor.model == ""
        assert motor.horsepower == 0.0
        assert motor.voltage == 0.0
        assert motor.manufacturer == ""

    def test_motor_with_values(self):
        """Test MotorProperties with actual values."""
        motor = MotorProperties(
            model="NEMA D High Slip",
            horsepower=75.0,
            voltage=480.0,
            manufacturer="GE"
        )
        assert motor.model == "NEMA D High Slip"
        assert motor.horsepower == 75.0
        assert motor.voltage == 480.0
        assert motor.manufacturer == "GE"
