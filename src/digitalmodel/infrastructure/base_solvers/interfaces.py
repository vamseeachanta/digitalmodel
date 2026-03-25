"""
ABOUTME: Protocol definitions for solver implementations
ABOUTME: Type-safe interfaces for structural typing in solver framework
"""

from typing import Protocol, Dict, Any, Tuple, List


class SolverProtocol(Protocol):
    """
    Protocol defining the interface for solver implementations.

    Enables structural typing and static type checking for any solver
    implementation without requiring inheritance from BaseSolver.
    """

    def validate_inputs(self) -> Tuple[bool, List[str]]:
        """
        Validate input parameters.

        Returns:
            Tuple of (is_valid, error_messages)
        """
        ...

    def solve(self) -> Dict[str, Any]:
        """
        Execute solver and return results.

        Returns:
            Results dictionary with solution data
        """
        ...

    def get_solver_metadata(self) -> Dict[str, Any]:
        """
        Get solver metadata for discovery.

        Returns:
            Metadata dictionary with solver information
        """
        ...

    def get_status(self) -> str:
        """
        Get current solver status.

        Returns:
            Status string representation
        """
        ...

    def get_results(self) -> Dict[str, Any]:
        """
        Get cached results from last solve().

        Returns:
            Results dictionary
        """
        ...


class ConfigurableSolverProtocol(SolverProtocol, Protocol):
    """
    Protocol for solvers with configuration management.

    Extends SolverProtocol with configuration methods.
    """

    def get_config(self, key: str, default: Any = None) -> Any:
        """
        Get configuration value by key.

        Args:
            key: Configuration key
            default: Default value if not found

        Returns:
            Configuration value or default
        """
        ...

    def set_config(self, key: str, value: Any) -> None:
        """
        Set configuration value.

        Args:
            key: Configuration key
            value: Value to set
        """
        ...

    def get_all_config(self) -> Dict[str, Any]:
        """
        Get all configuration.

        Returns:
            Configuration dictionary
        """
        ...


class AnalysisSolverProtocol(ConfigurableSolverProtocol, Protocol):
    """
    Protocol for numerical analysis solvers.

    Extends ConfigurableSolverProtocol with input data and validation methods.
    """

    def set_input_data(self, data: Dict[str, Any]) -> None:
        """
        Set input data for analysis.

        Args:
            data: Input data dictionary
        """
        ...

    def get_input_data(self) -> Dict[str, Any]:
        """
        Get input data.

        Returns:
            Input data dictionary
        """
        ...

    def set_validation_errors(self, errors: List[str]) -> None:
        """
        Set validation errors.

        Args:
            errors: List of error messages
        """
        ...

    def get_validation_errors(self) -> List[str]:
        """
        Get validation errors.

        Returns:
            List of error messages
        """
        ...

    def add_validation_error(self, error: str) -> None:
        """
        Add a validation error.

        Args:
            error: Error message
        """
        ...

    def clear_validation_errors(self) -> None:
        """Clear all validation errors."""
        ...

    def has_validation_errors(self) -> bool:
        """
        Check if validation errors exist.

        Returns:
            True if errors present, False otherwise
        """
        ...
