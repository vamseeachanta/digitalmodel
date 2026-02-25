# ABOUTME: Tests for the dynacard exceptions module.
# ABOUTME: Tests custom exception classes and factory functions.

import pytest
from digitalmodel.marine_ops.artificial_lift.dynacard import (
    DynacardException,
    ValidationError,
    PhysicsError,
    NumericalError,
    ConfigurationError,
    ConvergenceError,
    DataLoadError,
    missing_data_error,
    invalid_value_error,
    array_length_mismatch_error,
)


class TestDynacardException:
    """Tests for base DynacardException class."""

    def test_basic_instantiation(self):
        """Should create exception with message."""
        exc = DynacardException("Test error")
        assert str(exc) == "[DYNACARD_ERROR] Test error"
        assert exc.message == "Test error"
        assert exc.code == "DYNACARD_ERROR"
        assert exc.details == {}

    def test_with_custom_code(self):
        """Should accept custom error code."""
        exc = DynacardException("Test error", code="CUSTOM_CODE")
        assert exc.code == "CUSTOM_CODE"
        assert "[CUSTOM_CODE]" in str(exc)

    def test_with_details(self):
        """Should accept additional details dict."""
        details = {"field": "value", "count": 42}
        exc = DynacardException("Test error", details=details)
        assert exc.details == details

    def test_is_exception(self):
        """Should be a proper Exception subclass."""
        exc = DynacardException("Test error")
        assert isinstance(exc, Exception)

        with pytest.raises(DynacardException):
            raise exc


class TestValidationError:
    """Tests for ValidationError class."""

    def test_basic_instantiation(self):
        """Should create with message."""
        exc = ValidationError("Invalid input")
        assert "VALIDATION_ERROR" in exc.code
        assert exc.field is None

    def test_with_field(self):
        """Should include field name in code."""
        exc = ValidationError("Invalid SPM", field="spm")
        assert exc.code == "VALIDATION_ERROR:SPM"
        assert exc.field == "spm"

    def test_is_dynacard_exception(self):
        """Should be a DynacardException subclass."""
        exc = ValidationError("Invalid input")
        assert isinstance(exc, DynacardException)


class TestPhysicsError:
    """Tests for PhysicsError class."""

    def test_basic_instantiation(self):
        """Should create with message."""
        exc = PhysicsError("Solver failed")
        assert "PHYSICS_ERROR" in exc.code
        assert exc.solver is None

    def test_with_solver(self):
        """Should include solver name in code."""
        exc = PhysicsError("FFT failed", solver="gibbs")
        assert exc.code == "PHYSICS_ERROR:GIBBS"
        assert exc.solver == "gibbs"


class TestNumericalError:
    """Tests for NumericalError class."""

    def test_basic_instantiation(self):
        """Should create with message."""
        exc = NumericalError("Division by zero")
        assert "NUMERICAL_ERROR" in exc.code

    def test_with_operation_and_value(self):
        """Should store operation and value."""
        exc = NumericalError(
            "Result is NaN",
            operation="torque_calculation",
            value=float('nan')
        )
        assert exc.operation == "torque_calculation"
        assert exc.code == "NUMERICAL_ERROR:TORQUE_CALCULATION"


class TestConfigurationError:
    """Tests for ConfigurationError class."""

    def test_basic_instantiation(self):
        """Should create with message."""
        exc = ConfigurationError("Invalid geometry")
        assert "CONFIGURATION_ERROR" in exc.code

    def test_with_parameter(self):
        """Should include parameter name in code."""
        exc = ConfigurationError("Missing K dimension", parameter="dimensional_k")
        assert exc.code == "CONFIGURATION_ERROR:DIMENSIONAL_K"
        assert exc.parameter == "dimensional_k"


class TestConvergenceError:
    """Tests for ConvergenceError class."""

    def test_basic_instantiation(self):
        """Should create with message."""
        exc = ConvergenceError("Failed to converge")
        assert exc.code == "CONVERGENCE_ERROR"

    def test_with_iterations_and_tolerance(self):
        """Should store iteration count and tolerance."""
        exc = ConvergenceError(
            "Max iterations exceeded",
            iterations=1000,
            tolerance=0.001
        )
        assert exc.iterations == 1000
        assert exc.tolerance == 0.001


class TestDataLoadError:
    """Tests for DataLoadError class."""

    def test_basic_instantiation(self):
        """Should create with message."""
        exc = DataLoadError("Invalid JSON")
        assert "DATA_LOAD_ERROR" in exc.code

    def test_with_source(self):
        """Should include source in code."""
        exc = DataLoadError("Parse failed", source="well_data.json")
        assert exc.code == "DATA_LOAD_ERROR:WELL_DATA.JSON"
        assert exc.source == "well_data.json"


class TestFactoryFunctions:
    """Tests for exception factory functions."""

    def test_missing_data_error(self):
        """Should create ValidationError for missing data."""
        exc = missing_data_error("surface_card")
        assert isinstance(exc, ValidationError)
        assert exc.field == "surface_card"
        assert "missing" in exc.message.lower()
        assert exc.details["error_type"] == "missing_data"

    def test_invalid_value_error(self):
        """Should create ValidationError for invalid values."""
        exc = invalid_value_error("spm", -5, "must be positive")
        assert isinstance(exc, ValidationError)
        assert exc.field == "spm"
        assert "-5" in exc.message
        assert "positive" in exc.message
        assert exc.details["value"] == -5

    def test_array_length_mismatch_error(self):
        """Should create ValidationError for mismatched arrays."""
        exc = array_length_mismatch_error("position", 100, "load", 95)
        assert isinstance(exc, ValidationError)
        assert "100" in exc.message
        assert "95" in exc.message
        assert exc.details["lengths"]["position"] == 100
        assert exc.details["lengths"]["load"] == 95


class TestExceptionHierarchy:
    """Tests for exception class hierarchy."""

    def test_all_exceptions_inherit_from_base(self):
        """All custom exceptions should inherit from DynacardException."""
        exception_classes = [
            ValidationError,
            PhysicsError,
            NumericalError,
            ConfigurationError,
            ConvergenceError,
            DataLoadError,
        ]

        for exc_class in exception_classes:
            exc = exc_class("Test message")
            assert isinstance(exc, DynacardException)
            assert isinstance(exc, Exception)

    def test_can_catch_all_with_base_exception(self):
        """Should be able to catch all dynacard exceptions with base class."""
        exceptions_to_raise = [
            ValidationError("validation"),
            PhysicsError("physics"),
            NumericalError("numerical"),
            ConfigurationError("config"),
            ConvergenceError("convergence"),
            DataLoadError("data"),
        ]

        for exc in exceptions_to_raise:
            with pytest.raises(DynacardException):
                raise exc

    def test_can_catch_specific_exception(self):
        """Should be able to catch specific exception types."""
        with pytest.raises(ValidationError):
            raise ValidationError("validation error")

        with pytest.raises(PhysicsError):
            raise PhysicsError("physics error")
