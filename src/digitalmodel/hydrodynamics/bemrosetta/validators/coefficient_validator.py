"""Hydrodynamic coefficient validator.

Validates hydrodynamic coefficients (added mass, damping, RAOs) for
physical consistency including symmetry, positive definiteness, and
physical limits.
"""

from typing import Tuple, List
import numpy as np
from loguru import logger

from digitalmodel.hydrodynamics.diffraction import (
    DiffractionResults,
    AddedMassSet,
    DampingSet,
)
from ..core.interfaces import ValidatorInterface, ValidationReport


class CoefficientValidator(ValidatorInterface):
    """Validate hydrodynamic coefficients for physical consistency.

    Performs the following checks:
    - Matrix symmetry (added mass and damping should be symmetric)
    - Positive semi-definiteness (diagonal elements should be non-negative)
    - Physical limits (no NaN/Inf values)
    - RAO magnitude non-negativity

    Attributes:
        check_symmetry: Whether to check matrix symmetry.
        check_positive_definite: Whether to check positive semi-definiteness.
        check_physical_limits: Whether to check for NaN/Inf values.
        tolerance: Relative tolerance for symmetry checks.
    """

    def __init__(
        self,
        check_symmetry: bool = True,
        check_positive_definite: bool = True,
        check_physical_limits: bool = True,
        tolerance: float = 0.01,
    ):
        """Initialize coefficient validator.

        Args:
            check_symmetry: Enable matrix symmetry checks.
            check_positive_definite: Enable positive semi-definiteness checks.
            check_physical_limits: Enable NaN/Inf checks.
            tolerance: Relative tolerance for symmetry comparison.
        """
        self.check_symmetry = check_symmetry
        self.check_positive_definite = check_positive_definite
        self.check_physical_limits = check_physical_limits
        self.tolerance = tolerance

    def validate(self, results: DiffractionResults) -> ValidationReport:
        """Validate hydrodynamic coefficients.

        Args:
            results: Diffraction results containing added mass, damping, RAOs.

        Returns:
            ValidationReport with errors, warnings, and metrics.
        """
        report = ValidationReport()

        # Validate added mass
        if results.added_mass is not None:
            am_errors, am_warnings = self._validate_added_mass(results.added_mass)
            for error in am_errors:
                report.add_error(error)
            for warning in am_warnings:
                report.add_warning(warning)
        else:
            report.add_warning("No added mass data to validate")

        # Validate damping
        if results.damping is not None:
            d_errors, d_warnings = self._validate_damping(results.damping)
            for error in d_errors:
                report.add_error(error)
            for warning in d_warnings:
                report.add_warning(warning)
        else:
            report.add_warning("No damping data to validate")

        # Validate RAOs
        if results.raos is not None:
            rao_errors, rao_warnings = self._validate_raos(results)
            for error in rao_errors:
                report.add_error(error)
            for warning in rao_warnings:
                report.add_warning(warning)

        return report

    def is_valid(self, results: DiffractionResults) -> bool:
        """Quick check if results are valid.

        Args:
            results: Diffraction results to validate.

        Returns:
            True if results pass all validation checks.
        """
        report = self.validate(results)
        return report.is_valid

    def _validate_added_mass(self, added_mass: AddedMassSet) -> Tuple[List[str], List[str]]:
        """Validate added mass matrices.

        Args:
            added_mass: Added mass coefficient set.

        Returns:
            Tuple of (errors, warnings) lists.
        """
        errors = []
        warnings = []

        for matrix_obj in added_mass.matrices:
            freq = matrix_obj.frequency
            m = matrix_obj.matrix

            # Check symmetry (added mass should be symmetric)
            if self.check_symmetry:
                if not np.allclose(m, m.T, rtol=self.tolerance):
                    warnings.append(
                        f"Added mass matrix not symmetric at freq={freq:.3f}"
                    )

            # Check positive semi-definite (diagonal elements non-negative)
            if self.check_positive_definite:
                diag = np.diag(m)
                if np.any(diag < -self.tolerance):
                    errors.append(
                        f"Negative added mass diagonal at freq={freq:.3f}"
                    )

            # Check physical limits
            if self.check_physical_limits:
                if np.any(np.isnan(m)) or np.any(np.isinf(m)):
                    errors.append(f"NaN/Inf in added mass at freq={freq:.3f}")

        return errors, warnings

    def _validate_damping(self, damping: DampingSet) -> Tuple[List[str], List[str]]:
        """Validate damping matrices.

        Args:
            damping: Damping coefficient set.

        Returns:
            Tuple of (errors, warnings) lists.
        """
        errors = []
        warnings = []

        for matrix_obj in damping.matrices:
            freq = matrix_obj.frequency
            m = matrix_obj.matrix

            # Damping should be symmetric
            if self.check_symmetry:
                if not np.allclose(m, m.T, rtol=self.tolerance):
                    warnings.append(
                        f"Damping matrix not symmetric at freq={freq:.3f}"
                    )

            # Radiation damping should be non-negative (energy dissipation)
            if self.check_positive_definite:
                diag = np.diag(m)
                if np.any(diag < -self.tolerance):
                    warnings.append(f"Negative damping at freq={freq:.3f}")

            # Check for NaN/Inf
            if self.check_physical_limits:
                if np.any(np.isnan(m)) or np.any(np.isinf(m)):
                    errors.append(f"NaN/Inf in damping at freq={freq:.3f}")

        return errors, warnings

    def _validate_raos(self, results: DiffractionResults) -> Tuple[List[str], List[str]]:
        """Validate RAO data.

        Args:
            results: Diffraction results containing RAO set.

        Returns:
            Tuple of (errors, warnings) lists.
        """
        errors = []
        warnings = []

        raos = results.raos
        dof_names = ["surge", "sway", "heave", "roll", "pitch", "yaw"]

        for dof in dof_names:
            component = getattr(raos, dof, None)
            if component is None:
                continue

            mag = component.magnitude
            phase = component.phase

            # Check for NaN/Inf in magnitude
            if self.check_physical_limits:
                if np.any(np.isnan(mag)) or np.any(np.isinf(mag)):
                    errors.append(f"NaN/Inf in {dof} RAO magnitude")

            # Check magnitude is non-negative
            if np.any(mag < 0):
                warnings.append(f"Negative {dof} RAO magnitude")

            # Check phase is in reasonable range
            if np.any(np.abs(phase) > 360):
                warnings.append(f"{dof} RAO phase outside [-360, 360] range")

            # Check for NaN/Inf in phase
            if self.check_physical_limits:
                if np.any(np.isnan(phase)) or np.any(np.isinf(phase)):
                    errors.append(f"NaN/Inf in {dof} RAO phase")

        return errors, warnings


def validate_coefficients(
    results: DiffractionResults,
    strict: bool = False,
) -> ValidationReport:
    """Convenience function to validate hydrodynamic coefficients.

    Args:
        results: Diffraction results to validate.
        strict: Enable stricter validation (positive definiteness as error).

    Returns:
        ValidationReport with validation results.
    """
    validator = CoefficientValidator(
        check_symmetry=True,
        check_positive_definite=strict,
        check_physical_limits=True,
    )
    return validator.validate(results)
