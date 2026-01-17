# ABOUTME: Custom exceptions for dynacard analysis module.
# ABOUTME: Provides standardized error handling across all calculator classes.

from typing import Optional


class DynacardException(Exception):
    """
    Base exception for all dynacard analysis errors.

    All dynacard-specific exceptions should inherit from this class
    to enable consistent error handling throughout the module.

    Attributes:
        message: Human-readable error description.
        code: Optional error code for programmatic handling.
        details: Optional additional context about the error.
    """

    def __init__(
        self,
        message: str,
        code: Optional[str] = None,
        details: Optional[dict] = None
    ):
        super().__init__(message)
        self.message = message
        self.code = code or "DYNACARD_ERROR"
        self.details = details or {}

    def __str__(self) -> str:
        return f"[{self.code}] {self.message}"


class ValidationError(DynacardException):
    """
    Raised when input validation fails.

    Common causes:
    - Missing required data (surface card, rod string, pump)
    - Invalid parameter values (negative dimensions, zero SPM)
    - Inconsistent data (mismatched array lengths)
    """

    def __init__(
        self,
        message: str,
        field: Optional[str] = None,
        details: Optional[dict] = None
    ):
        code = "VALIDATION_ERROR"
        if field:
            code = f"VALIDATION_ERROR:{field.upper()}"
        super().__init__(message, code, details)
        self.field = field


class PhysicsError(DynacardException):
    """
    Raised when physics calculations fail.

    Common causes:
    - Numerical instability in wave equation solver
    - FFT computation errors
    - Invalid geometry configurations
    """

    def __init__(
        self,
        message: str,
        solver: Optional[str] = None,
        details: Optional[dict] = None
    ):
        code = "PHYSICS_ERROR"
        if solver:
            code = f"PHYSICS_ERROR:{solver.upper()}"
        super().__init__(message, code, details)
        self.solver = solver


class NumericalError(DynacardException):
    """
    Raised when numerical computations produce invalid results.

    Common causes:
    - Division by zero
    - NaN or Inf values
    - Overflow/underflow conditions
    """

    def __init__(
        self,
        message: str,
        operation: Optional[str] = None,
        value: Optional[float] = None,
        details: Optional[dict] = None
    ):
        code = "NUMERICAL_ERROR"
        if operation:
            code = f"NUMERICAL_ERROR:{operation.upper()}"
        super().__init__(message, code, details)
        self.operation = operation
        self.value = value


class ConfigurationError(DynacardException):
    """
    Raised when calculator configuration is invalid.

    Common causes:
    - Missing surface unit geometry parameters
    - Invalid pumping unit dimensions
    - Unsupported solver method
    """

    def __init__(
        self,
        message: str,
        parameter: Optional[str] = None,
        details: Optional[dict] = None
    ):
        code = "CONFIGURATION_ERROR"
        if parameter:
            code = f"CONFIGURATION_ERROR:{parameter.upper()}"
        super().__init__(message, code, details)
        self.parameter = parameter


class ConvergenceError(DynacardException):
    """
    Raised when iterative calculations fail to converge.

    Common causes:
    - Counterbalance optimization not converging
    - Maximum iterations exceeded
    - Oscillating solutions
    """

    def __init__(
        self,
        message: str,
        iterations: Optional[int] = None,
        tolerance: Optional[float] = None,
        details: Optional[dict] = None
    ):
        code = "CONVERGENCE_ERROR"
        super().__init__(message, code, details)
        self.iterations = iterations
        self.tolerance = tolerance


class DataLoadError(DynacardException):
    """
    Raised when loading or parsing input data fails.

    Common causes:
    - Invalid JSON format
    - Missing required fields in input data
    - Type conversion errors
    """

    def __init__(
        self,
        message: str,
        source: Optional[str] = None,
        details: Optional[dict] = None
    ):
        code = "DATA_LOAD_ERROR"
        if source:
            code = f"DATA_LOAD_ERROR:{source.upper()}"
        super().__init__(message, code, details)
        self.source = source


# Exception factory functions for common validation errors

def missing_data_error(field: str) -> ValidationError:
    """Create a ValidationError for missing required data."""
    return ValidationError(
        message=f"Required data missing: {field}",
        field=field,
        details={"error_type": "missing_data"}
    )


def invalid_value_error(field: str, value: any, reason: str) -> ValidationError:
    """Create a ValidationError for invalid parameter values."""
    return ValidationError(
        message=f"Invalid value for {field}: {value}. {reason}",
        field=field,
        details={"value": value, "reason": reason}
    )


def array_length_mismatch_error(
    field1: str,
    len1: int,
    field2: str,
    len2: int
) -> ValidationError:
    """Create a ValidationError for mismatched array lengths."""
    return ValidationError(
        message=f"Array length mismatch: {field1} has {len1} elements, {field2} has {len2}",
        field=f"{field1}_{field2}",
        details={"lengths": {field1: len1, field2: len2}}
    )
