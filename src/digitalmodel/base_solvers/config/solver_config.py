"""
ABOUTME: Solver-specific configuration management for base_solvers
ABOUTME: Configuration schemas and validation for solver types
"""

import logging
from typing import Dict, Any, Optional, List
from copy import deepcopy

logger = logging.getLogger(__name__)


class SolverConfigManager:
    """
    Extended configuration manager for solver-specific settings.

    Provides schema validation and solver-type-specific configuration
    on top of the base ConfigManager.
    """

    # Default schemas for each solver type
    SOLVER_SCHEMAS = {
        "structural": {
            "tolerance": {"type": float, "min": 1e-10, "max": 1e-2, "default": 1e-6},
            "max_iterations": {"type": int, "min": 1, "max": 10000, "default": 1000},
            "method": {
                "type": str,
                "allowed": ["direct", "iterative", "adaptive"],
                "default": "direct"
            },
            "output_format": {
                "type": str,
                "allowed": ["numpy", "csv", "json"],
                "default": "numpy"
            }
        },
        "marine": {
            "water_depth": {"type": float, "min": 0, "default": 0},
            "wave_height": {"type": float, "min": 0, "default": 0},
            "current_velocity": {"type": float, "min": 0, "default": 0},
            "material_density": {"type": float, "min": 0, "default": 1025},
            "safety_factor": {"type": float, "min": 1.0, "max": 5.0, "default": 1.5},
            "analysis_type": {
                "type": str,
                "allowed": ["static", "dynamic", "fatigue"],
                "default": "static"
            }
        },
        "signal": {
            "sampling_rate": {"type": float, "min": 0, "default": 1000},
            "window_size": {"type": int, "min": 1, "default": 1024},
            "overlap": {"type": float, "min": 0, "max": 0.9, "default": 0.5},
            "filter_type": {
                "type": str,
                "allowed": ["lowpass", "highpass", "bandpass", "notch"],
                "default": "lowpass"
            },
            "cutoff_frequency": {"type": float, "min": 0, "default": 100}
        },
        "fatigue": {
            "material_type": {
                "type": str,
                "allowed": ["steel", "aluminum", "composite", "titanium"],
                "default": "steel"
            },
            "design_life": {"type": int, "min": 1, "default": 10},
            "sn_curve": {"type": str, "default": "DNV-RP-C203"},
            "mean_stress_correction": {
                "type": str,
                "allowed": ["none", "goodman", "morrow", "walker"],
                "default": "goodman"
            },
            "stress_concentration": {"type": float, "min": 1.0, "default": 1.0}
        }
    }

    def __init__(self, config: Optional[Dict[str, Any]] = None):
        """
        Initialize solver configuration manager.

        Args:
            config: Optional initial configuration dictionary
        """
        self.config = config.copy() if config else {}
        self._validation_errors: List[str] = []
        logger.debug("Initialized SolverConfigManager")

    def validate_solver_config(
        self,
        solver_type: str,
        config: Dict[str, Any]
    ) -> tuple[bool, List[str]]:
        """
        Validate configuration against solver type schema.

        Args:
            solver_type: Type of solver (structural, marine, signal, fatigue)
            config: Configuration dictionary to validate

        Returns:
            Tuple of (is_valid, error_messages)
        """
        self._validation_errors.clear()

        if solver_type not in self.SOLVER_SCHEMAS:
            msg = f"Unknown solver type: {solver_type}"
            self._validation_errors.append(msg)
            return False, self._validation_errors

        schema = self.SOLVER_SCHEMAS[solver_type]

        for key, spec in schema.items():
            if key in config:
                value = config[key]

                # Type check
                if not isinstance(value, spec.get("type", type(None))):
                    msg = f"{key}: expected {spec['type'].__name__}, got {type(value).__name__}"
                    self._validation_errors.append(msg)
                    continue

                # Range check for numeric types
                if "min" in spec and value < spec["min"]:
                    msg = f"{key}: value {value} less than minimum {spec['min']}"
                    self._validation_errors.append(msg)

                if "max" in spec and value > spec["max"]:
                    msg = f"{key}: value {value} greater than maximum {spec['max']}"
                    self._validation_errors.append(msg)

                # Allowed values check
                if "allowed" in spec and value not in spec["allowed"]:
                    msg = f"{key}: value '{value}' not in allowed {spec['allowed']}"
                    self._validation_errors.append(msg)

        return len(self._validation_errors) == 0, self._validation_errors.copy()

    def get_schema(self, solver_type: str) -> Dict[str, Any]:
        """
        Get schema for solver type.

        Args:
            solver_type: Solver type identifier

        Returns:
            Deep copy of schema for solver type
        """
        if solver_type not in self.SOLVER_SCHEMAS:
            logger.warning(f"Unknown solver type: {solver_type}")
            return {}

        return deepcopy(self.SOLVER_SCHEMAS[solver_type])

    def get_default_config(self, solver_type: str) -> Dict[str, Any]:
        """
        Get default configuration for solver type.

        Args:
            solver_type: Solver type identifier

        Returns:
            Dictionary with default values for all schema keys
        """
        schema = self.get_schema(solver_type)
        defaults = {}

        for key, spec in schema.items():
            if "default" in spec:
                defaults[key] = spec["default"]

        return defaults

    def merge_configs(
        self,
        base_config: Dict[str, Any],
        override_config: Dict[str, Any]
    ) -> Dict[str, Any]:
        """
        Merge configuration with overrides.

        Args:
            base_config: Base configuration dictionary
            override_config: Override values dictionary

        Returns:
            Merged configuration (override values take precedence)
        """
        merged = deepcopy(base_config)
        merged.update(override_config)
        return merged

    def get_validation_errors(self) -> List[str]:
        """
        Get validation errors from last validation.

        Returns:
            List of error messages
        """
        return self._validation_errors.copy()

    def save_config(self, config: Dict[str, Any], filepath: str) -> bool:
        """
        Save configuration to YAML file.

        Args:
            config: Configuration dictionary
            filepath: Path to save configuration

        Returns:
            True if save successful, False otherwise
        """
        try:
            import yaml
        except ImportError:
            logger.error("PyYAML not installed")
            return False

        try:
            with open(filepath, 'w') as f:
                yaml.dump(config, f, default_flow_style=False)
            logger.info(f"Saved configuration to {filepath}")
            return True
        except Exception as e:
            logger.error(f"Failed to save configuration: {e}")
            return False

    def load_config(self, filepath: str) -> Optional[Dict[str, Any]]:
        """
        Load configuration from YAML file.

        Args:
            filepath: Path to configuration file

        Returns:
            Configuration dictionary or None if load failed
        """
        try:
            import yaml
        except ImportError:
            logger.error("PyYAML not installed")
            return None

        try:
            with open(filepath, 'r') as f:
                config = yaml.safe_load(f)
            logger.info(f"Loaded configuration from {filepath}")
            return config if config else {}
        except Exception as e:
            logger.error(f"Failed to load configuration: {e}")
            return None
