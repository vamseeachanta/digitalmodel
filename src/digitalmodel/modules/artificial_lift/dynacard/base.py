# ABOUTME: Base calculator class for dynacard analysis modules.
# ABOUTME: Provides common patterns for context management and validation.

from abc import ABC, abstractmethod
from typing import Generic, TypeVar, Optional
from .models import DynacardAnalysisContext
from .exceptions import (
    ValidationError,
    ConfigurationError,
    NumericalError,
    missing_data_error,
    invalid_value_error,
    array_length_mismatch_error,
)

# Generic type for analysis result
T = TypeVar('T')


class BaseCalculator(ABC, Generic[T]):
    """
    Abstract base class for dynacard analysis calculators.

    Provides common initialization and validation patterns used across
    all dynacard calculator modules.

    Type Parameters:
        T: The analysis result type (e.g., GearBoxLoadingAnalysis)

    Usage:
        class MyCalculator(BaseCalculator[MyAnalysisResult]):
            def _create_result(self) -> MyAnalysisResult:
                return MyAnalysisResult()

            def calculate(self) -> MyAnalysisResult:
                self.validate_surface_card()  # Raises ValidationError if invalid
                # ... calculation logic
                return self.result
    """

    def __init__(self, context: DynacardAnalysisContext):
        """
        Initialize calculator with analysis context.

        Args:
            context: Complete well analysis context including surface card,
                    rod string, pump, surface unit, and operating parameters.
        """
        self.ctx = context
        self.result: T = self._create_result()

    @abstractmethod
    def _create_result(self) -> T:
        """
        Create and return a new instance of the result type.

        Must be implemented by subclasses to return their specific
        analysis result type initialized with default values.

        Returns:
            New instance of the analysis result type.
        """
        pass

    @abstractmethod
    def calculate(self) -> T:
        """
        Execute the calculation and return results.

        Must be implemented by subclasses to perform the actual
        analysis calculation.

        Returns:
            Analysis result populated with calculated values.

        Raises:
            ValidationError: If input validation fails.
            ConfigurationError: If configuration is invalid.
            NumericalError: If calculation produces invalid values.
        """
        pass

    # =========================================================================
    # Exception-raising validation methods (preferred for new code)
    # =========================================================================

    def validate_surface_card(self) -> None:
        """
        Validate surface card data is present and valid.

        Raises:
            ValidationError: If surface card is missing or invalid.
        """
        if self.ctx.surface_card is None:
            raise missing_data_error("surface_card")

        if len(self.ctx.surface_card.position) == 0:
            raise ValidationError(
                "Surface card has no data points",
                field="surface_card",
                details={"error_type": "empty_data"}
            )

        pos_len = len(self.ctx.surface_card.position)
        load_len = len(self.ctx.surface_card.load)
        if pos_len != load_len:
            raise array_length_mismatch_error("position", pos_len, "load", load_len)

    def validate_surface_unit(self) -> None:
        """
        Validate surface unit data is present.

        Raises:
            ValidationError: If surface unit is missing.
        """
        if self.ctx.surface_unit is None:
            raise missing_data_error("surface_unit")

    def validate_surface_unit_geometry(self) -> None:
        """
        Validate surface unit has required geometry dimensions.

        Raises:
            ValidationError: If surface unit is missing.
            ConfigurationError: If geometry dimensions are invalid.
        """
        self.validate_surface_unit()
        su = self.ctx.surface_unit

        if su.dimensional_k <= 0 or su.dimensional_i <= 0:
            raise ConfigurationError(
                "Missing K or I dimensions",
                parameter="dimensional_k_i",
                details={"k": su.dimensional_k, "i": su.dimensional_i}
            )

        if su.dimensional_k < su.dimensional_i:
            raise ConfigurationError(
                "K < I, h cannot be calculated (invalid geometry)",
                parameter="dimensional_k_i",
                details={"k": su.dimensional_k, "i": su.dimensional_i}
            )

        if su.dimensional_a <= 0 or su.dimensional_c <= 0:
            raise ConfigurationError(
                "Missing A or C dimensions",
                parameter="dimensional_a_c",
                details={"a": su.dimensional_a, "c": su.dimensional_c}
            )

    def validate_pump(self) -> None:
        """
        Validate pump data is present and valid.

        Raises:
            ValidationError: If pump is missing or has invalid diameter.
        """
        if self.ctx.pump is None:
            raise missing_data_error("pump")

        if self.ctx.pump.diameter <= 0:
            raise invalid_value_error(
                "pump.diameter",
                self.ctx.pump.diameter,
                "must be positive"
            )

    def validate_rod_string(self) -> None:
        """
        Validate rod string data is present.

        Raises:
            ValidationError: If rod string is missing or empty.
        """
        if not self.ctx.rod_string or len(self.ctx.rod_string) == 0:
            raise missing_data_error("rod_string")

    def validate_spm(self) -> None:
        """
        Validate strokes per minute is positive.

        Raises:
            ValidationError: If SPM is not positive.
        """
        if self.ctx.spm <= 0:
            raise invalid_value_error("spm", self.ctx.spm, "must be positive")

    def validate_common(self) -> None:
        """
        Run common validations: surface card and SPM.

        Raises:
            ValidationError: If any validation fails.
        """
        self.validate_surface_card()
        self.validate_spm()

    # =========================================================================
    # Legacy validation methods (return error strings for backwards compatibility)
    # =========================================================================

    def _validate_inputs(self) -> Optional[str]:
        """
        Validate required inputs before calculation.

        Override in subclasses to add specific validation rules.
        Returns an error message string if validation fails,
        or None if validation passes.

        Returns:
            Error message if validation fails, None otherwise.

        Note:
            This is a legacy method. Prefer exception-raising validators
            (validate_*) for new code.
        """
        return None

    def _set_error(self, error_message: str) -> None:
        """
        Set error state on the result object.

        Override in subclasses if the result type has a different
        error field name or structure.

        Args:
            error_message: Description of the validation/calculation error.

        Note:
            This is a legacy method. Prefer raising exceptions for new code.
        """
        # Default implementation tries common error field names
        if hasattr(self.result, 'warning_message'):
            self.result.warning_message = f"error: {error_message}"
        elif hasattr(self.result, 'status'):
            self.result.status = f"error: {error_message}"
        elif hasattr(self.result, 'counterbalance_status'):
            self.result.counterbalance_status = f"error: {error_message}"
        elif hasattr(self.result, 'motor_design'):
            self.result.motor_design = f"error: {error_message}"

    # Legacy string-returning validators (for backwards compatibility)

    def _validate_surface_card(self) -> Optional[str]:
        """Validate surface card data is present and valid."""
        if self.ctx.surface_card is None:
            return "surface card data missing"

        if len(self.ctx.surface_card.position) == 0:
            return "surface card has no data points"

        if len(self.ctx.surface_card.position) != len(self.ctx.surface_card.load):
            return "position and load arrays have different lengths"

        return None

    def _validate_surface_unit(self) -> Optional[str]:
        """Validate surface unit data is present."""
        if self.ctx.surface_unit is None:
            return "surface unit data missing"
        return None

    def _validate_pump(self) -> Optional[str]:
        """Validate pump data is present and valid."""
        if self.ctx.pump is None:
            return "pump data missing"

        if self.ctx.pump.diameter <= 0:
            return "pump diameter must be positive"

        return None

    def _validate_rod_string(self) -> Optional[str]:
        """Validate rod string data is present."""
        if not self.ctx.rod_string or len(self.ctx.rod_string) == 0:
            return "rod string data missing"
        return None

    def _validate_spm(self) -> Optional[str]:
        """Validate strokes per minute is positive."""
        if self.ctx.spm <= 0:
            return "strokes per minute must be positive"
        return None

    def _validate_common(self) -> Optional[str]:
        """
        Run common validations: surface card and SPM.

        Returns:
            First error encountered, or None if all pass.
        """
        for validator in [self._validate_surface_card, self._validate_spm]:
            error = validator()
            if error:
                return error
        return None
