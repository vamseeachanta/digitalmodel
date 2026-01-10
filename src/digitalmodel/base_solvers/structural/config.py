"""
ABOUTME: Configuration schema for structural analysis solvers
ABOUTME: Extends Phase 2.1 ConfigManager with structural parameters
"""

from typing import Dict, Any

# Structural Solver Configuration Schema
STRUCTURAL_CONFIG_SCHEMA = {
    "analysis_type": {
        "type": str,
        "allowed": ["static", "buckling", "dynamic", "thermal"],
        "default": "static",
        "description": "Type of structural analysis to perform"
    },
    "element_type": {
        "type": str,
        "allowed": ["beam", "plate", "solid", "shell"],
        "default": "beam",
        "description": "Type of finite element to use"
    },
    "material": {
        "type": str,
        "allowed": ["steel", "aluminum", "composite", "custom"],
        "default": "steel",
        "description": "Material type or custom material name"
    },
    "youngs_modulus": {
        "type": float,
        "min": 1e9,
        "max": 1e12,
        "unit": "Pa",
        "default": 2.1e11,
        "description": "Young's modulus (Pa). Default: steel (2.1e11)"
    },
    "poissons_ratio": {
        "type": float,
        "min": 0.0,
        "max": 0.5,
        "default": 0.3,
        "description": "Poisson's ratio (dimensionless)"
    },
    "density": {
        "type": float,
        "min": 100,
        "max": 20000,
        "unit": "kg/m^3",
        "default": 7850,
        "description": "Material density (kg/m³). Default: steel (7850)"
    },
    "yield_strength": {
        "type": float,
        "min": 1e7,
        "max": 1e9,
        "unit": "Pa",
        "default": 2.5e8,
        "description": "Yield strength (Pa). Default: steel (2.5e8)"
    },
    "solver_type": {
        "type": str,
        "allowed": ["direct", "iterative", "eigenvalue"],
        "default": "direct",
        "description": "Linear system solver type"
    },
    "tolerance": {
        "type": float,
        "min": 1e-10,
        "max": 1e-2,
        "default": 1e-6,
        "description": "Convergence tolerance for iterative solvers"
    },
    "max_iterations": {
        "type": int,
        "min": 1,
        "max": 10000,
        "default": 1000,
        "description": "Maximum iterations for iterative solvers"
    },
    "num_modes": {
        "type": int,
        "min": 1,
        "max": 100,
        "default": 5,
        "description": "Number of modes to extract in eigenvalue analysis"
    },
    "output_format": {
        "type": str,
        "allowed": ["numpy", "csv", "json", "vtk"],
        "default": "numpy",
        "description": "Output format for results export"
    }
}

# Material property presets
MATERIAL_PROPERTIES = {
    "steel": {
        "E": 2.1e11,        # Pa
        "nu": 0.3,          # Dimensionless
        "rho": 7850,        # kg/m³
        "sigma_y": 2.5e8,   # Pa
        "name": "Mild Steel"
    },
    "aluminum": {
        "E": 7.0e10,        # Pa
        "nu": 0.33,         # Dimensionless
        "rho": 2700,        # kg/m³
        "sigma_y": 2.7e8,   # Pa
        "name": "6061-T6 Aluminum"
    },
    "titanium": {
        "E": 1.1e11,        # Pa
        "nu": 0.31,         # Dimensionless
        "rho": 4500,        # kg/m³
        "sigma_y": 8.3e8,   # Pa
        "name": "Grade 2 Titanium"
    },
    "copper": {
        "E": 1.3e11,        # Pa
        "nu": 0.34,         # Dimensionless
        "rho": 8960,        # kg/m³
        "sigma_y": 2.0e8,   # Pa
        "name": "Pure Copper"
    },
    "concrete": {
        "E": 3.0e10,        # Pa
        "nu": 0.2,          # Dimensionless
        "rho": 2400,        # kg/m³
        "sigma_y": 3.0e7,   # Pa (compressive)
        "name": "Normal Strength Concrete"
    }
}


def get_material_properties(material_name: str) -> Dict[str, float]:
    """
    Get material properties by name.

    Args:
        material_name: Name of material (e.g., "steel", "aluminum")

    Returns:
        Dictionary of material properties

    Raises:
        ValueError: If material not found
    """
    if material_name not in MATERIAL_PROPERTIES:
        raise ValueError(
            f"Material '{material_name}' not found. "
            f"Available: {list(MATERIAL_PROPERTIES.keys())}"
        )

    props = MATERIAL_PROPERTIES[material_name].copy()
    del props['name']  # Remove name from returned properties
    return props


def validate_structural_config(config: Dict[str, Any]) -> tuple[bool, list[str]]:
    """
    Validate structural solver configuration.

    Args:
        config: Configuration dictionary

    Returns:
        Tuple of (is_valid, error_list)
    """
    errors = []

    for param, schema in STRUCTURAL_CONFIG_SCHEMA.items():
        if param not in config:
            continue

        value = config[param]
        param_type = schema.get('type')

        # Type checking
        if not isinstance(value, param_type):
            errors.append(
                f"Parameter '{param}': expected {param_type.__name__}, "
                f"got {type(value).__name__}"
            )
            continue

        # Allowed values checking
        if 'allowed' in schema:
            if value not in schema['allowed']:
                errors.append(
                    f"Parameter '{param}': value '{value}' not in "
                    f"allowed values {schema['allowed']}"
                )

        # Range checking (for numeric types)
        if 'min' in schema and value < schema['min']:
            errors.append(
                f"Parameter '{param}': value {value} is below minimum {schema['min']}"
            )

        if 'max' in schema and value > schema['max']:
            errors.append(
                f"Parameter '{param}': value {value} is above maximum {schema['max']}"
            )

    return len(errors) == 0, errors


def get_default_config() -> Dict[str, Any]:
    """
    Get default structural solver configuration.

    Returns:
        Dictionary with all default values
    """
    return {
        param: schema['default']
        for param, schema in STRUCTURAL_CONFIG_SCHEMA.items()
    }


def get_config_description(param: str) -> str:
    """
    Get description for a configuration parameter.

    Args:
        param: Parameter name

    Returns:
        Description string

    Raises:
        KeyError: If parameter not found
    """
    if param not in STRUCTURAL_CONFIG_SCHEMA:
        raise KeyError(f"Unknown parameter: {param}")

    return STRUCTURAL_CONFIG_SCHEMA[param].get('description', '')


def get_config_summary() -> str:
    """
    Get summary of all configuration parameters.

    Returns:
        Formatted string with parameter information
    """
    lines = ["Structural Solver Configuration Parameters:\n"]

    for param, schema in STRUCTURAL_CONFIG_SCHEMA.items():
        description = schema.get('description', 'No description')
        default = schema.get('default', 'N/A')
        param_type = schema.get('type').__name__

        lines.append(f"  {param}:")
        lines.append(f"    Type: {param_type}")
        lines.append(f"    Default: {default}")
        lines.append(f"    Description: {description}")

        if 'allowed' in schema:
            lines.append(f"    Allowed: {schema['allowed']}")
        if 'min' in schema:
            lines.append(f"    Range: [{schema['min']}, {schema['max']}]")

        lines.append("")

    return "\n".join(lines)
