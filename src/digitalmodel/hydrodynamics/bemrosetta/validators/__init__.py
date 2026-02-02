"""BEMRosetta Validators Module.

Validation utilities for hydrodynamic coefficient quality assurance:
- CoefficientValidator: Symmetry, physical limits, completeness checks
- CausalityChecker: Kramers-Kronig relation verification
- MeshValidator: Panel mesh quality metrics (future)
- FrequencyValidator: Frequency range and resolution checks (future)

All validators return detailed reports with warnings and errors.
"""

from .coefficient_validator import CoefficientValidator, validate_coefficients
from .causality_checker import CausalityChecker, check_causality

__all__ = [
    "CoefficientValidator",
    "validate_coefficients",
    "CausalityChecker",
    "check_causality",
]
