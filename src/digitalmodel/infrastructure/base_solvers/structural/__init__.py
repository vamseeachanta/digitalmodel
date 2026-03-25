"""
ABOUTME: Structural analysis solvers module
ABOUTME: Implements Von Mises, buckling, and finite element analysis
"""

from .base import StructuralSolver

try:
    from .config import (
        STRUCTURAL_CONFIG_SCHEMA,
        MATERIAL_PROPERTIES,
        get_material_properties,
        validate_structural_config,
        get_default_config,
        get_config_description,
        get_config_summary,
    )
    _config_available = True
except ImportError:
    _config_available = False

from .elements import BeamElement

__all__ = [
    'StructuralSolver',
    'BeamElement',
]

if _config_available:
    __all__ += [
        'STRUCTURAL_CONFIG_SCHEMA',
        'MATERIAL_PROPERTIES',
        'get_material_properties',
        'validate_structural_config',
        'get_default_config',
        'get_config_description',
        'get_config_summary',
    ]
