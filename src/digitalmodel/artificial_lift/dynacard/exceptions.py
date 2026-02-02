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

    This exception indicates that input data does not meet the requirements
    for dynacard analysis. It should be raised early in the calculation
    process before any computationally expensive operations begin.

    Attributes:
        message: Human-readable error description.
        code: Error code in format "VALIDATION_ERROR" or "VALIDATION_ERROR:FIELD".
        field: Name of the field that failed validation.
        details: Additional context about the validation failure.

    Common causes:
        - Missing required data (surface card, rod string, pump)
        - Invalid parameter values (negative dimensions, zero SPM)
        - Inconsistent data (mismatched array lengths)

    Example:
        >>> raise ValidationError("SPM must be positive", field="spm")
        >>> raise ValidationError("Surface card has no data points", field="surface_card")
    """

    def __init__(
        self,
        message: str,
        field: Optional[str] = None,
        details: Optional[dict] = None
    ):
        """
        Initialize a ValidationError.

        Args:
            message: Human-readable description of what validation failed.
            field: Optional name of the field that failed validation.
                   Used to generate a more specific error code.
            details: Optional dictionary with additional error context.
        """
        code = "VALIDATION_ERROR"
        if field:
            code = f"VALIDATION_ERROR:{field.upper()}"
        super().__init__(message, code, details)
        self.field = field


class PhysicsError(DynacardException):
    """
    Raised when physics calculations fail.

    This exception indicates that the underlying physics simulation or
    calculation could not complete successfully. It typically occurs
    during wave equation solving, downhole card calculation, or other
    physics-based computations.

    Attributes:
        message: Human-readable error description.
        code: Error code in format "PHYSICS_ERROR" or "PHYSICS_ERROR:SOLVER".
        solver: Name of the solver or calculation that failed.
        details: Additional context about the physics failure.

    Common causes:
        - Numerical instability in wave equation solver
        - FFT computation errors
        - Invalid geometry configurations
        - Boundary condition violations

    Example:
        >>> raise PhysicsError("Wave equation diverged", solver="finite_difference")
        >>> raise PhysicsError("FFT failed: invalid input length", solver="fft")
    """

    def __init__(
        self,
        message: str,
        solver: Optional[str] = None,
        details: Optional[dict] = None
    ):
        """
        Initialize a PhysicsError.

        Args:
            message: Human-readable description of the physics failure.
            solver: Optional name of the solver or algorithm that failed.
                    Used to generate a more specific error code.
            details: Optional dictionary with additional error context.
        """
        code = "PHYSICS_ERROR"
        if solver:
            code = f"PHYSICS_ERROR:{solver.upper()}"
        super().__init__(message, code, details)
        self.solver = solver


class NumericalError(DynacardException):
    """
    Raised when numerical computations produce invalid results.

    This exception indicates that a mathematical operation produced
    an invalid or unreliable result. It is distinct from PhysicsError
    in that it represents pure numerical issues rather than physical
    model failures.

    Attributes:
        message: Human-readable error description.
        code: Error code in format "NUMERICAL_ERROR" or "NUMERICAL_ERROR:OPERATION".
        operation: Name of the mathematical operation that failed.
        value: The problematic numerical value, if applicable.
        details: Additional context about the numerical failure.

    Common causes:
        - Division by zero
        - NaN or Inf values in calculations
        - Overflow/underflow conditions
        - Square root of negative numbers
        - Logarithm of non-positive values

    Example:
        >>> raise NumericalError("Division by zero in torque factor", operation="division")
        >>> raise NumericalError("Result is NaN", operation="sqrt", value=float('nan'))
    """

    def __init__(
        self,
        message: str,
        operation: Optional[str] = None,
        value: Optional[float] = None,
        details: Optional[dict] = None
    ):
        """
        Initialize a NumericalError.

        Args:
            message: Human-readable description of the numerical failure.
            operation: Optional name of the operation that failed (e.g., "division", "sqrt").
                       Used to generate a more specific error code.
            value: Optional problematic value that caused the error.
            details: Optional dictionary with additional error context.
        """
        code = "NUMERICAL_ERROR"
        if operation:
            code = f"NUMERICAL_ERROR:{operation.upper()}"
        super().__init__(message, code, details)
        self.operation = operation
        self.value = value


class ConfigurationError(DynacardException):
    """
    Raised when calculator configuration is invalid.

    This exception indicates that the analysis cannot proceed due to
    missing or invalid configuration settings. Unlike ValidationError
    which deals with input data, ConfigurationError relates to the
    setup of the calculation environment (geometry, solver settings, etc.).

    Attributes:
        message: Human-readable error description.
        code: Error code in format "CONFIGURATION_ERROR" or "CONFIGURATION_ERROR:PARAM".
        parameter: Name of the configuration parameter that is invalid.
        details: Additional context about the configuration failure.

    Common causes:
        - Missing surface unit geometry parameters (K, I, A, C, P dimensions)
        - Invalid pumping unit dimensions (K < I geometry)
        - Unsupported solver method or algorithm
        - Incompatible parameter combinations

    Example:
        >>> raise ConfigurationError("Missing K or I dimensions", parameter="dimensional_k_i")
        >>> raise ConfigurationError("K < I, h cannot be calculated (invalid geometry)")
    """

    def __init__(
        self,
        message: str,
        parameter: Optional[str] = None,
        details: Optional[dict] = None
    ):
        """
        Initialize a ConfigurationError.

        Args:
            message: Human-readable description of the configuration problem.
            parameter: Optional name of the configuration parameter that is invalid.
                       Used to generate a more specific error code.
            details: Optional dictionary with additional error context.
        """
        code = "CONFIGURATION_ERROR"
        if parameter:
            code = f"CONFIGURATION_ERROR:{parameter.upper()}"
        super().__init__(message, code, details)
        self.parameter = parameter


class ConvergenceError(DynacardException):
    """
    Raised when iterative calculations fail to converge.

    This exception indicates that an iterative algorithm did not reach
    a stable solution within the allowed number of iterations or tolerance.
    It provides diagnostic information to help identify why convergence failed.

    Attributes:
        message: Human-readable error description.
        code: Error code "CONVERGENCE_ERROR".
        iterations: Number of iterations attempted before failure.
        tolerance: The convergence tolerance that was not achieved.
        details: Additional context about the convergence failure.

    Common causes:
        - Counterbalance optimization not converging
        - Maximum iterations exceeded
        - Oscillating solutions (solution bounces between values)
        - Tolerance too strict for the problem
        - Poor initial guess

    Example:
        >>> raise ConvergenceError(
        ...     "Counterbalance optimization did not converge",
        ...     iterations=100,
        ...     tolerance=0.001
        ... )
    """

    def __init__(
        self,
        message: str,
        iterations: Optional[int] = None,
        tolerance: Optional[float] = None,
        details: Optional[dict] = None
    ):
        """
        Initialize a ConvergenceError.

        Args:
            message: Human-readable description of the convergence failure.
            iterations: Optional number of iterations attempted.
            tolerance: Optional convergence tolerance that was not met.
            details: Optional dictionary with additional error context,
                     such as final residual value or solution history.
        """
        code = "CONVERGENCE_ERROR"
        super().__init__(message, code, details)
        self.iterations = iterations
        self.tolerance = tolerance


class DataLoadError(DynacardException):
    """
    Raised when loading or parsing input data fails.

    This exception indicates that input data could not be loaded or parsed
    from its source (file, API, database, etc.). It is raised before
    validation and indicates problems with data retrieval or format.

    Attributes:
        message: Human-readable error description.
        code: Error code in format "DATA_LOAD_ERROR" or "DATA_LOAD_ERROR:SOURCE".
        source: Identifier for the data source (filename, URL, etc.).
        details: Additional context about the load failure.

    Common causes:
        - Invalid JSON format or syntax errors
        - Missing required fields in input data
        - Type conversion errors (string to float, etc.)
        - File not found or permission denied
        - Network errors when fetching remote data

    Example:
        >>> raise DataLoadError("Invalid JSON in well data", source="7699227.json")
        >>> raise DataLoadError("Missing 'surface_card' field", source="api_response")
    """

    def __init__(
        self,
        message: str,
        source: Optional[str] = None,
        details: Optional[dict] = None
    ):
        """
        Initialize a DataLoadError.

        Args:
            message: Human-readable description of the data loading failure.
            source: Optional identifier for the data source (filename, URL, etc.).
                    Used to generate a more specific error code.
            details: Optional dictionary with additional error context,
                     such as line number or field name where parsing failed.
        """
        code = "DATA_LOAD_ERROR"
        if source:
            code = f"DATA_LOAD_ERROR:{source.upper()}"
        super().__init__(message, code, details)
        self.source = source


# Exception factory functions for common validation errors


def missing_data_error(field: str) -> ValidationError:
    """
    Create a ValidationError for missing required data.

    This factory function creates a standardized ValidationError for cases
    where required input data is None or not provided.

    Args:
        field: Name of the missing field (e.g., "surface_card", "pump", "rod_string").

    Returns:
        ValidationError with standardized message format and error_type detail.

    Example:
        >>> if context.surface_card is None:
        ...     raise missing_data_error("surface_card")
    """
    return ValidationError(
        message=f"Required data missing: {field}",
        field=field,
        details={"error_type": "missing_data"}
    )


def invalid_value_error(field: str, value: any, reason: str) -> ValidationError:
    """
    Create a ValidationError for invalid parameter values.

    This factory function creates a standardized ValidationError for cases
    where a parameter has an invalid value (e.g., negative when positive required).

    Args:
        field: Name of the field with invalid value.
        value: The invalid value that was provided.
        reason: Human-readable explanation of why the value is invalid.

    Returns:
        ValidationError with the invalid value and reason in details.

    Example:
        >>> if diameter <= 0:
        ...     raise invalid_value_error("pump.diameter", diameter, "must be positive")
    """
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
    """
    Create a ValidationError for mismatched array lengths.

    This factory function creates a standardized ValidationError for cases
    where two related arrays have different lengths (e.g., position and load
    arrays in a dynacard must have the same length).

    Args:
        field1: Name of the first array field.
        len1: Length of the first array.
        field2: Name of the second array field.
        len2: Length of the second array.

    Returns:
        ValidationError with both array lengths in details.

    Example:
        >>> if len(position) != len(load):
        ...     raise array_length_mismatch_error("position", len(position), "load", len(load))
    """
    return ValidationError(
        message=f"Array length mismatch: {field1} has {len1} elements, {field2} has {len2}",
        field=f"{field1}_{field2}",
        details={"lengths": {field1: len1, field2: len2}}
    )
