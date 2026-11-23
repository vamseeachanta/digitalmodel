"""Causality checker using Kramers-Kronig relations.

Validates that hydrodynamic coefficients satisfy causality through
the Kramers-Kronig relations which connect the real and imaginary
parts of a causal linear response function.
"""

from typing import Optional
import numpy as np
from scipy import integrate
from loguru import logger

from digitalmodel.hydrodynamics.diffraction import (
    DiffractionResults,
    AddedMassSet,
    DampingSet,
)
from ..core.interfaces import ValidatorInterface, ValidationReport


class CausalityChecker(ValidatorInterface):
    """Validate causality using Kramers-Kronig relations.

    The Kramers-Kronig relations connect the real and imaginary parts
    of a causal linear response function. For hydrodynamics:
    - Added mass A(w) and damping B(w) are related through:
      A(w) - A(inf) = (2/pi) * P.V. integral of [B(w')/w'] dw'

    Violation indicates numerical errors or non-physical data.

    Attributes:
        tolerance: Maximum acceptable relative error for K-K relation.
        n_integration_points: Number of points for numerical integration.
    """

    def __init__(
        self,
        tolerance: float = 0.1,
        n_integration_points: int = 1000,
    ):
        """Initialize causality checker.

        Args:
            tolerance: Maximum acceptable relative K-K error (default 10%).
            n_integration_points: Points for numerical integration.
        """
        self.tolerance = tolerance
        self.n_integration_points = n_integration_points

    def validate(self, results: DiffractionResults) -> ValidationReport:
        """Validate causality of hydrodynamic coefficients.

        Args:
            results: Diffraction results with added mass and damping.

        Returns:
            ValidationReport with K-K error metrics and warnings.
        """
        report = ValidationReport()

        if results.added_mass is None or results.damping is None:
            report.add_warning(
                "Both added mass and damping required for causality check"
            )
            return report

        # Check Kramers-Kronig for each DOF pair
        dof_names = ["surge", "sway", "heave", "roll", "pitch", "yaw"]

        for i in range(6):
            for j in range(6):
                kk_error = self._check_kramers_kronig(
                    results.added_mass,
                    results.damping,
                    i,
                    j,
                )

                if kk_error is not None:
                    dof_pair = f"{dof_names[i]}-{dof_names[j]}"
                    report.set_metric(f"kk_error_{dof_pair}", kk_error)

                    if kk_error > self.tolerance:
                        report.add_warning(
                            f"Kramers-Kronig violation for {dof_pair}: "
                            f"error={kk_error:.2%}"
                        )

        return report

    def is_valid(self, results: DiffractionResults) -> bool:
        """Quick check if results satisfy causality.

        Args:
            results: Diffraction results to validate.

        Returns:
            True if results satisfy causality checks.
        """
        report = self.validate(results)
        return report.is_valid

    def _check_kramers_kronig(
        self,
        added_mass: AddedMassSet,
        damping: DampingSet,
        i: int,
        j: int,
    ) -> Optional[float]:
        """Check Kramers-Kronig relation for component (i, j).

        The K-K relation states that for a causal response function:
        Re[H(w)] - Re[H(inf)] = (2/pi) * P.V. integral of [Im[H(w')]/w'] dw'

        For hydrodynamics, added mass A(w) corresponds to the real part
        and damping B(w) to the imaginary part of the response.

        Args:
            added_mass: Added mass coefficient set.
            damping: Damping coefficient set.
            i: Row index (0-5 for 6 DOFs).
            j: Column index (0-5 for 6 DOFs).

        Returns:
            Relative error as float, or None if calculation fails.
        """
        # Extract frequency-dependent values
        frequencies = np.array([m.frequency for m in added_mass.matrices])

        if len(frequencies) < 3:
            return None

        a_values = np.array([m.matrix[i, j] for m in added_mass.matrices])
        b_values = np.array([m.matrix[i, j] for m in damping.matrices])

        # Estimate A(infinity) from high frequency behavior
        # As w -> inf, added mass approaches a constant (infinite freq added mass)
        a_inf = a_values[-1]

        # Simplified Kramers-Kronig check:
        # Compare trend of added mass with integral of damping
        #
        # For a proper check, we would compute:
        # A(w) - A(inf) = (2/pi) * P.V. integral of [B(w')/w'] dw'
        #
        # Simplified: check that damping integral correlates with added mass variation

        try:
            # Integrate B(w)/w
            # Add small epsilon to avoid division by zero at w=0
            integrand = b_values / np.maximum(frequencies, 1e-6)
            integral = integrate.cumulative_trapezoid(
                integrand,
                frequencies,
                initial=0,
            )
            kk_estimate = (2 / np.pi) * integral

            # Compare with actual added mass variation
            a_variation = a_values - a_inf

            # Calculate relative error
            max_variation = np.max(np.abs(a_variation))
            if max_variation > 1e-10:
                error = np.mean(np.abs(kk_estimate - a_variation)) / max_variation
                return float(error)

            return 0.0

        except Exception as e:
            logger.warning(f"Kramers-Kronig check failed for ({i},{j}): {e}")
            return None


def check_causality(results: DiffractionResults) -> ValidationReport:
    """Convenience function to check causality.

    Args:
        results: Diffraction results to validate.

    Returns:
        ValidationReport with K-K validation results.
    """
    checker = CausalityChecker()
    return checker.validate(results)
