"""
ABOUTME: Base solver classes for digitalmodel mathematical solver framework
ABOUTME: Abstract base classes and interfaces for all mathematical solvers
"""

import logging
from abc import ABC, abstractmethod
from typing import Dict, Any, Optional, List, Tuple
from enum import Enum
from copy import deepcopy

logger = logging.getLogger(__name__)


class SolverStatus(Enum):
    """Status enumeration for solver execution states."""

    PENDING = "pending"
    VALIDATING = "validating"
    EXECUTING = "executing"
    COMPLETED = "completed"
    FAILED = "failed"


class BaseSolver(ABC):
    """
    Abstract base class for all mathematical solvers.

    Defines the interface that all solver implementations must follow.
    Provides common functionality for status tracking and results caching.
    """

    def __init__(self, name: str, version: str) -> None:
        """
        Initialize base solver.

        Args:
            name: Unique name identifier for the solver
            version: Version string (e.g., "1.0.0")
        """
        self.name = name
        self.version = version
        self.status = SolverStatus.PENDING
        self._results: Dict[str, Any] = {}
        logger.debug(f"Initialized solver: {name} v{version}")

    @abstractmethod
    def validate_inputs(self) -> Tuple[bool, List[str]]:
        """
        Validate input parameters.

        Returns:
            Tuple of (is_valid, error_messages)
            - is_valid: True if inputs are valid, False otherwise
            - error_messages: List of validation error messages (empty if valid)
        """
        pass

    @abstractmethod
    def solve(self) -> Dict[str, Any]:
        """
        Execute solver and return results.

        This method should:
        1. Validate inputs
        2. Execute the mathematical solution
        3. Cache results internally
        4. Update status to COMPLETED or FAILED
        5. Return results dictionary

        Returns:
            Dictionary containing solver results with keys:
            - 'success': bool indicating if solve was successful
            - 'status': SolverStatus value
            - Other result keys depend on solver implementation

        Raises:
            ValueError: If validation fails
            RuntimeError: If solve execution fails
        """
        pass

    @abstractmethod
    def get_solver_metadata(self) -> Dict[str, Any]:
        """
        Get solver metadata for discovery and documentation.

        Returns:
            Dictionary with keys:
            - 'name': str
            - 'version': str
            - 'description': str
            - 'inputs': Dict[str, Dict] with input param specs
            - 'outputs': Dict[str, Dict] with output specs
            - 'solver_type': str (e.g., 'structural', 'marine', 'signal')
        """
        pass

    def get_status(self) -> SolverStatus:
        """
        Get current solver status.

        Returns:
            Current SolverStatus enum value
        """
        return self.status

    def get_results(self) -> Dict[str, Any]:
        """
        Get cached results from last solve() execution.

        Returns:
            Deep copy of results dictionary to prevent external modification
        """
        return deepcopy(self._results)

    def _set_status(self, status: SolverStatus) -> None:
        """
        Internal method to update solver status.

        Args:
            status: New SolverStatus value
        """
        self.status = status
        logger.debug(f"{self.name}: status changed to {status.value}")

    def _cache_results(self, results: Dict[str, Any]) -> None:
        """
        Internal method to cache results.

        Args:
            results: Results dictionary to cache
        """
        self._results = deepcopy(results)
        logger.debug(f"{self.name}: cached results with keys {list(results.keys())}")


class ConfigurableSolver(BaseSolver):
    """
    Base class for solvers with configuration management.

    Provides configuration management through ConfigManager from base_configs.
    All solver-specific configurations accessed through this base class.
    """

    def __init__(
        self,
        name: str,
        version: str,
        config: Optional[Dict[str, Any]] = None
    ) -> None:
        """
        Initialize configurable solver with configuration support.

        Args:
            name: Solver name identifier
            version: Version string
            config: Optional initial configuration dictionary

        Raises:
            ImportError: If ConfigManager cannot be imported
        """
        super().__init__(name, version)

        try:
            from digitalmodel.base_configs import ConfigManager
        except ImportError as e:
            logger.error(f"Failed to import ConfigManager: {e}")
            raise ImportError(
                "ConfigManager from base_configs is required for ConfigurableSolver"
            ) from e

        self.config_manager = ConfigManager()

        # Load initial configuration if provided
        if config:
            for key, value in config.items():
                self.config_manager.set(key, value)
                logger.debug(f"Set config {key}={value}")

    def get_config(self, key: str, default: Any = None) -> Any:
        """
        Get configuration value using dot notation.

        Args:
            key: Configuration key with dot notation (e.g., "solver.tolerance")
            default: Default value if key not found

        Returns:
            Configuration value or default
        """
        return self.config_manager.get(key, default)

    def set_config(self, key: str, value: Any) -> None:
        """
        Set configuration value using dot notation.

        Args:
            key: Configuration key with dot notation
            value: Value to set
        """
        self.config_manager.set(key, value)
        logger.debug(f"Updated config {key}={value}")

    def get_all_config(self) -> Dict[str, Any]:
        """
        Get all configuration as dictionary.

        Returns:
            Deep copy of entire configuration
        """
        return deepcopy(self.config_manager.config)


class AnalysisSolver(ConfigurableSolver):
    """
    Base class for numerical analysis solvers.

    Extends ConfigurableSolver with input data management and validation tracking.
    Suitable for structural analysis, marine analysis, signal processing, etc.
    """

    def __init__(
        self,
        name: str,
        version: str,
        config: Optional[Dict[str, Any]] = None
    ) -> None:
        """
        Initialize analysis solver with input data management.

        Args:
            name: Solver name identifier
            version: Version string
            config: Optional initial configuration
        """
        super().__init__(name, version, config)
        self.input_data: Dict[str, Any] = {}
        self.validation_errors: List[str] = []

    def set_input_data(self, data: Dict[str, Any]) -> None:
        """
        Set input data for analysis.

        Args:
            data: Input data dictionary
        """
        self.input_data = deepcopy(data)
        logger.debug(f"{self.name}: set input data with {len(data)} keys")

    def get_input_data(self) -> Dict[str, Any]:
        """
        Get current input data.

        Returns:
            Deep copy of input data
        """
        return deepcopy(self.input_data)

    def set_validation_errors(self, errors: List[str]) -> None:
        """
        Set validation error messages.

        Args:
            errors: List of validation error strings
        """
        self.validation_errors = errors.copy()
        logger.debug(f"{self.name}: set {len(errors)} validation errors")

    def get_validation_errors(self) -> List[str]:
        """
        Get list of validation errors.

        Returns:
            Copy of validation errors list
        """
        return self.validation_errors.copy()

    def add_validation_error(self, error: str) -> None:
        """
        Add a single validation error.

        Args:
            error: Error message string
        """
        self.validation_errors.append(error)
        logger.warning(f"{self.name}: validation error - {error}")

    def clear_validation_errors(self) -> None:
        """Clear all validation errors."""
        self.validation_errors.clear()
        logger.debug(f"{self.name}: cleared validation errors")

    def has_validation_errors(self) -> bool:
        """
        Check if there are validation errors.

        Returns:
            True if validation_errors list is not empty
        """
        return len(self.validation_errors) > 0
