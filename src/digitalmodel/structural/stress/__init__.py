"""
Digital Model Stress Analysis Module

This module provides comprehensive stress analysis capabilities including:
- Von Mises stress calculations for various loading conditions
- Stress-strain curve models and analysis
- Combined loading analysis (pressure, tension, bending)
- Material property handling
- Nonlinear stress analysis

The module is designed for engineering applications requiring accurate
stress calculations for structural integrity assessment.
"""

from .vm_stress import (
    VonMisesStressCalculator,
    PipeStressAnalyzer,
    calculate_vm_stress,
    calculate_principal_stresses,
    StressState,
    LoadingCondition,
    PipeGeometry,
    MaterialProperties
)

from .stress_strain import (
    StressStrainAnalyzer,
    RambergOsgoodModel,
    LinearElasticModel,
    MaterialModel,
    calculate_stress_strain_curve,
    fit_stress_strain_data
)

from .nonlinear import (
    NonlinearStressAnalyzer,
    VonMisesYield,
    LinearHardening,
    PlasticityModel,
    HardeningModel,
    calculate_nonlinear_response,
    solve_nonlinear_stress
)

__version__ = "1.0.0"
__author__ = "Digital Model Team"

__all__ = [
    # Von Mises stress analysis
    "VonMisesStressCalculator",
    "PipeStressAnalyzer",
    "calculate_vm_stress",
    "calculate_principal_stresses",
    "StressState",
    "LoadingCondition",
    "PipeGeometry",
    "MaterialProperties",

    # Stress-strain analysis
    "StressStrainAnalyzer",
    "RambergOsgoodModel",
    "LinearElasticModel",
    "MaterialModel",
    "calculate_stress_strain_curve",
    "fit_stress_strain_data",

    # Nonlinear analysis
    "NonlinearStressAnalyzer",
    "VonMisesYield",
    "LinearHardening",
    "PlasticityModel",
    "HardeningModel",
    "calculate_nonlinear_response",
    "solve_nonlinear_stress"
]