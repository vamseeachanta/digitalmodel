# ABOUTME: Unit tests for base calculator module.
# ABOUTME: Validates exception-raising validators, legacy validators, and error setting.

import pytest

from digitalmodel.marine_ops.artificial_lift.dynacard.base import BaseCalculator
from digitalmodel.marine_ops.artificial_lift.dynacard.models import (
    DynacardAnalysisContext,
    CardData,
    RodSection,
    PumpProperties,
    SurfaceUnit,
    RodBucklingAnalysis,
    GearBoxLoadingAnalysis,
    PowerConsumptionAnalysis,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.exceptions import (
    ValidationError,
    ConfigurationError,
)


class ConcreteCalculator(BaseCalculator[RodBucklingAnalysis]):
    """Concrete stub using RodBucklingAnalysis (has warning_message)."""

    def _create_result(self) -> RodBucklingAnalysis:
        return RodBucklingAnalysis()

    def calculate(self) -> RodBucklingAnalysis:
        return self.result


class GearBoxCalculatorStub(BaseCalculator[GearBoxLoadingAnalysis]):
    """Concrete stub using GearBoxLoadingAnalysis (has counterbalance_status)."""

    def _create_result(self) -> GearBoxLoadingAnalysis:
        return GearBoxLoadingAnalysis()

    def calculate(self) -> GearBoxLoadingAnalysis:
        return self.result


class PowerCalculatorStub(BaseCalculator[PowerConsumptionAnalysis]):
    """Concrete stub using PowerConsumptionAnalysis (has motor_design)."""

    def _create_result(self) -> PowerConsumptionAnalysis:
        return PowerConsumptionAnalysis()

    def calculate(self) -> PowerConsumptionAnalysis:
        return self.result


class TestBaseCalculatorValidation:
    """Tests for exception-raising validation methods."""

    def _create_test_context(self) -> DynacardAnalysisContext:
        """Create a valid test context."""
        return DynacardAnalysisContext(
            api14="TEST-BASE-001",
            surface_card=CardData(
                position=[0.0, 50.0, 100.0, 50.0],
                load=[5000.0, 8000.0, 12000.0, 7000.0],
            ),
            rod_string=[
                RodSection(diameter=0.875, length=2000.0),
                RodSection(diameter=0.75, length=2000.0),
            ],
            pump=PumpProperties(diameter=1.75, depth=4000.0),
            surface_unit=SurfaceUnit(
                dimensional_k=120.0,
                dimensional_i=80.0,
                dimensional_a=100.0,
                dimensional_c=60.0,
            ),
            spm=6.0,
        )

    def test_validate_surface_card_valid(self):
        """No error raised with valid surface card."""
        ctx = self._create_test_context()
        calc = ConcreteCalculator(ctx)
        calc.validate_surface_card()  # Should not raise

    def test_validate_surface_card_missing(self):
        """Raises ValidationError when surface_card is None."""
        ctx = self._create_test_context()
        calc = ConcreteCalculator(ctx)
        calc.ctx.surface_card = None

        with pytest.raises(ValidationError) as exc_info:
            calc.validate_surface_card()

        assert exc_info.value.field == "surface_card"

    def test_validate_surface_card_empty(self):
        """Raises ValidationError for empty position array."""
        ctx = self._create_test_context()
        calc = ConcreteCalculator(ctx)
        calc.ctx.surface_card = CardData(position=[], load=[])

        with pytest.raises(ValidationError) as exc_info:
            calc.validate_surface_card()

        assert exc_info.value.field == "surface_card"
        assert "no data points" in exc_info.value.message

    def test_validate_surface_card_length_mismatch(self):
        """Raises ValidationError for mismatched position and load arrays."""
        ctx = self._create_test_context()
        calc = ConcreteCalculator(ctx)
        calc.ctx.surface_card = CardData(
            position=[0.0, 50.0, 100.0],
            load=[5000.0, 8000.0],
        )

        with pytest.raises(ValidationError) as exc_info:
            calc.validate_surface_card()

        assert "mismatch" in exc_info.value.message.lower()

    def test_validate_pump_valid(self):
        """No error raised with valid pump."""
        ctx = self._create_test_context()
        calc = ConcreteCalculator(ctx)
        calc.validate_pump()  # Should not raise

    def test_validate_pump_missing(self):
        """Raises ValidationError when pump is None."""
        ctx = self._create_test_context()
        calc = ConcreteCalculator(ctx)
        calc.ctx.pump = None

        with pytest.raises(ValidationError) as exc_info:
            calc.validate_pump()

        assert exc_info.value.field == "pump"

    def test_validate_pump_zero_diameter(self):
        """Raises ValidationError when pump diameter is zero."""
        ctx = self._create_test_context()
        calc = ConcreteCalculator(ctx)
        calc.ctx.pump = PumpProperties(diameter=0.0, depth=4000.0)

        with pytest.raises(ValidationError) as exc_info:
            calc.validate_pump()

        assert exc_info.value.field == "pump.diameter"

    def test_validate_rod_string_valid(self):
        """No error raised with valid rod string."""
        ctx = self._create_test_context()
        calc = ConcreteCalculator(ctx)
        calc.validate_rod_string()  # Should not raise

    def test_validate_rod_string_empty(self):
        """Raises ValidationError when rod_string is empty."""
        ctx = self._create_test_context()
        calc = ConcreteCalculator(ctx)
        calc.ctx.rod_string = []

        with pytest.raises(ValidationError) as exc_info:
            calc.validate_rod_string()

        assert exc_info.value.field == "rod_string"

    def test_validate_spm_valid(self):
        """No error raised with valid spm."""
        ctx = self._create_test_context()
        calc = ConcreteCalculator(ctx)
        calc.validate_spm()  # Should not raise

    def test_validate_spm_zero(self):
        """Raises ValidationError when spm is zero."""
        ctx = self._create_test_context()
        ctx.spm = 0.0
        calc = ConcreteCalculator(ctx)

        with pytest.raises(ValidationError) as exc_info:
            calc.validate_spm()

        assert exc_info.value.field == "spm"

    def test_validate_surface_unit_valid(self):
        """No error raised with valid surface unit (default SurfaceUnit)."""
        ctx = self._create_test_context()
        calc = ConcreteCalculator(ctx)
        calc.validate_surface_unit()  # Should not raise

    def test_validate_common_valid(self):
        """No error raised when both surface_card and spm are valid."""
        ctx = self._create_test_context()
        calc = ConcreteCalculator(ctx)
        calc.validate_common()  # Should not raise

    def test_validate_common_bad_spm(self):
        """Raises ValidationError when surface_card is ok but spm is bad."""
        ctx = self._create_test_context()
        ctx.spm = 0.0
        calc = ConcreteCalculator(ctx)

        with pytest.raises(ValidationError) as exc_info:
            calc.validate_common()

        assert exc_info.value.field == "spm"

    def test_validate_surface_unit_geometry_missing_k_i(self):
        """Raises ConfigurationError when K or I dimensions are zero."""
        ctx = self._create_test_context()
        ctx.surface_unit = SurfaceUnit(
            dimensional_k=0.0,
            dimensional_i=0.0,
            dimensional_a=100.0,
            dimensional_c=60.0,
        )
        calc = ConcreteCalculator(ctx)

        with pytest.raises(ConfigurationError) as exc_info:
            calc.validate_surface_unit_geometry()

        assert exc_info.value.parameter == "dimensional_k_i"

    def test_validate_surface_unit_geometry_k_less_than_i(self):
        """Raises ConfigurationError when K < I."""
        ctx = self._create_test_context()
        ctx.surface_unit = SurfaceUnit(
            dimensional_k=50.0,
            dimensional_i=80.0,
            dimensional_a=100.0,
            dimensional_c=60.0,
        )
        calc = ConcreteCalculator(ctx)

        with pytest.raises(ConfigurationError) as exc_info:
            calc.validate_surface_unit_geometry()

        assert exc_info.value.parameter == "dimensional_k_i"
        assert "K < I" in exc_info.value.message

    def test_validate_surface_unit_geometry_missing_a_c(self):
        """Raises ConfigurationError when A or C dimensions are zero."""
        ctx = self._create_test_context()
        ctx.surface_unit = SurfaceUnit(
            dimensional_k=120.0,
            dimensional_i=80.0,
            dimensional_a=0.0,
            dimensional_c=0.0,
        )
        calc = ConcreteCalculator(ctx)

        with pytest.raises(ConfigurationError) as exc_info:
            calc.validate_surface_unit_geometry()

        assert exc_info.value.parameter == "dimensional_a_c"

    def test_validate_surface_unit_geometry_valid(self):
        """No error raised with proper geometry dimensions."""
        ctx = self._create_test_context()
        calc = ConcreteCalculator(ctx)
        calc.validate_surface_unit_geometry()  # Should not raise


class TestBaseCalculatorLegacyValidation:
    """Tests for legacy string-returning validation methods."""

    def _create_test_context(self) -> DynacardAnalysisContext:
        """Create a valid test context."""
        return DynacardAnalysisContext(
            api14="TEST-BASE-002",
            surface_card=CardData(
                position=[0.0, 50.0, 100.0, 50.0],
                load=[5000.0, 8000.0, 12000.0, 7000.0],
            ),
            rod_string=[
                RodSection(diameter=0.875, length=2000.0),
            ],
            pump=PumpProperties(diameter=1.75, depth=4000.0),
            surface_unit=SurfaceUnit(),
            spm=6.0,
        )

    def test_legacy_validate_surface_card_valid(self):
        """Returns None for valid surface card."""
        ctx = self._create_test_context()
        calc = ConcreteCalculator(ctx)
        assert calc._validate_surface_card() is None

    def test_legacy_validate_surface_card_missing(self):
        """Returns error string when surface_card is None."""
        ctx = self._create_test_context()
        calc = ConcreteCalculator(ctx)
        calc.ctx.surface_card = None

        result = calc._validate_surface_card()
        assert result is not None
        assert "missing" in result

    def test_legacy_validate_surface_card_empty(self):
        """Returns error string for empty surface card."""
        ctx = self._create_test_context()
        calc = ConcreteCalculator(ctx)
        calc.ctx.surface_card = CardData(position=[], load=[])

        result = calc._validate_surface_card()
        assert result is not None
        assert "no data points" in result

    def test_legacy_validate_surface_card_mismatch(self):
        """Returns error string for mismatched arrays."""
        ctx = self._create_test_context()
        calc = ConcreteCalculator(ctx)
        calc.ctx.surface_card = CardData(
            position=[0.0, 50.0, 100.0],
            load=[5000.0, 8000.0],
        )

        result = calc._validate_surface_card()
        assert result is not None
        assert "different lengths" in result

    def test_legacy_validate_pump_valid(self):
        """Returns None for valid pump."""
        ctx = self._create_test_context()
        calc = ConcreteCalculator(ctx)
        assert calc._validate_pump() is None

    def test_legacy_validate_pump_missing(self):
        """Returns error string when pump is None."""
        ctx = self._create_test_context()
        calc = ConcreteCalculator(ctx)
        calc.ctx.pump = None

        result = calc._validate_pump()
        assert result is not None
        assert "missing" in result

    def test_legacy_validate_pump_zero_diameter(self):
        """Returns error string when pump diameter is zero."""
        ctx = self._create_test_context()
        calc = ConcreteCalculator(ctx)
        calc.ctx.pump = PumpProperties(diameter=0.0, depth=4000.0)

        result = calc._validate_pump()
        assert result is not None
        assert "positive" in result

    def test_legacy_validate_rod_string_valid(self):
        """Returns None for valid rod string."""
        ctx = self._create_test_context()
        calc = ConcreteCalculator(ctx)
        assert calc._validate_rod_string() is None

    def test_legacy_validate_rod_string_empty(self):
        """Returns error string when rod_string is empty."""
        ctx = self._create_test_context()
        calc = ConcreteCalculator(ctx)
        calc.ctx.rod_string = []

        result = calc._validate_rod_string()
        assert result is not None
        assert "missing" in result

    def test_legacy_validate_spm_valid(self):
        """Returns None for valid spm."""
        ctx = self._create_test_context()
        calc = ConcreteCalculator(ctx)
        assert calc._validate_spm() is None

    def test_legacy_validate_spm_zero(self):
        """Returns error string when spm is zero."""
        ctx = self._create_test_context()
        ctx.spm = 0.0
        calc = ConcreteCalculator(ctx)

        result = calc._validate_spm()
        assert result is not None
        assert "positive" in result

    def test_legacy_validate_common_valid(self):
        """Returns None when all common validations pass."""
        ctx = self._create_test_context()
        calc = ConcreteCalculator(ctx)
        assert calc._validate_common() is None

    def test_legacy_validate_common_bad_card(self):
        """Returns error string when surface card is invalid."""
        ctx = self._create_test_context()
        calc = ConcreteCalculator(ctx)
        calc.ctx.surface_card = None

        result = calc._validate_common()
        assert result is not None
        assert "missing" in result

    def test_legacy_validate_inputs_default(self):
        """Default _validate_inputs returns None."""
        ctx = self._create_test_context()
        calc = ConcreteCalculator(ctx)
        assert calc._validate_inputs() is None


class TestBaseCalculatorSetError:
    """Tests for _set_error method across different result types."""

    def test_set_error_warning_message(self):
        """Sets warning_message on RodBucklingAnalysis result."""
        ctx = DynacardAnalysisContext(
            api14="TEST-ERR-001",
            surface_card=CardData(
                position=[0.0, 50.0],
                load=[5000.0, 8000.0],
            ),
            rod_string=[RodSection(diameter=0.875, length=2000.0)],
            pump=PumpProperties(diameter=1.75, depth=4000.0),
            spm=6.0,
        )
        calc = ConcreteCalculator(ctx)
        calc._set_error("test error message")

        assert calc.result.warning_message == "error: test error message"

    def test_set_error_counterbalance_status(self):
        """Sets counterbalance_status on GearBoxLoadingAnalysis result."""
        ctx = DynacardAnalysisContext(
            api14="TEST-ERR-002",
            surface_card=CardData(
                position=[0.0, 50.0],
                load=[5000.0, 8000.0],
            ),
            rod_string=[RodSection(diameter=0.875, length=2000.0)],
            pump=PumpProperties(diameter=1.75, depth=4000.0),
            spm=6.0,
        )
        calc = GearBoxCalculatorStub(ctx)
        calc._set_error("gearbox failure")

        assert calc.result.counterbalance_status == "error: gearbox failure"

    def test_set_error_motor_design(self):
        """Sets motor_design on PowerConsumptionAnalysis result."""
        ctx = DynacardAnalysisContext(
            api14="TEST-ERR-003",
            surface_card=CardData(
                position=[0.0, 50.0],
                load=[5000.0, 8000.0],
            ),
            rod_string=[RodSection(diameter=0.875, length=2000.0)],
            pump=PumpProperties(diameter=1.75, depth=4000.0),
            spm=6.0,
        )
        calc = PowerCalculatorStub(ctx)
        calc._set_error("power calc failed")

        assert calc.result.motor_design == "error: power calc failed"
