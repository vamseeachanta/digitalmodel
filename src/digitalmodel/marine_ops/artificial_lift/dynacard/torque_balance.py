# ABOUTME: Torque balance analysis for pumping unit counterbalance optimization.
# ABOUTME: Provides analytical estimation and objective function for optimal counterbalance.

import math
from typing import Optional, Tuple
import numpy as np

from .models import (
    DynacardAnalysisContext,
    TorqueBalanceAnalysis,
)
from .base import BaseCalculator
from .exceptions import DynacardException, ConfigurationError


class TorqueBalanceCalculator(BaseCalculator[TorqueBalanceAnalysis]):
    """
    Analyzes torque balance for pumping unit counterbalance optimization.

    Provides analytical estimation of optimal counterbalance moment using
    the API 11E torque factor method. This complements the iterative approach
    in GearBoxLoadingCalculator with a faster analytical estimation.

    The objective function J(M) = |max(upstroke torque) - max(downstroke torque)|
    is minimized to find the optimal counterbalance moment.
    """

    def __init__(self, context: DynacardAnalysisContext):
        super().__init__(context)
        # Computed geometry parameters
        self._psi: Optional[np.ndarray] = None
        self._theta: Optional[np.ndarray] = None
        self._torque_factor: Optional[np.ndarray] = None
        self._transition_index: int = 0

    def _create_result(self) -> TorqueBalanceAnalysis:
        return TorqueBalanceAnalysis()

    def calculate(self) -> TorqueBalanceAnalysis:
        """
        Execute torque balance analysis.

        Returns:
            TorqueBalanceAnalysis with analytical counterbalance estimate.

        Raises:
            ValidationError: If surface unit data is missing.
            ConfigurationError: If pumping unit geometry is invalid.
        """
        # Validate surface unit geometry (raises exceptions on failure)
        self.validate_surface_unit_geometry()

        # Additional geometry validation for torque balance
        su = self.ctx.surface_unit
        if su.radius <= 0 or su.dimensional_p <= 0:
            raise ConfigurationError(
                "Missing R or P dimensions for torque balance calculation",
                parameter="dimensional_r_p",
                details={"r": su.radius, "p": su.dimensional_p}
            )

        # Extract geometry parameters
        geometry = self._extract_geometry()
        if geometry is None:
            raise ConfigurationError(
                "Invalid pumping unit geometry - unable to extract parameters",
                parameter="pumping_unit"
            )

        # Calculate torque parameters
        self._calculate_torque_parameters(geometry)

        # Compute analytical optimal counterbalance
        self._compute_analytical_estimate(geometry)

        # Calculate objective function values
        self._calculate_objective_values(geometry)

        return self.result


    def _extract_geometry(self) -> Optional[dict]:
        """Extract and compute geometry parameters from surface unit."""
        su = self.ctx.surface_unit

        try:
            K = su.dimensional_k
            I = su.dimensional_i
            h = math.sqrt(K ** 2 - I ** 2)
            phi = math.atan(I / h)
            tau = math.radians(su.phase_angle)

            # Determine rotation direction
            if su.geometry in ['Clockwise', 'CW']:
                rotation = 1
            else:
                rotation = -1

            # Counterbalance moment in in-lbs
            M = su.counterbalance_moment * 1000.0 if su.counterbalance_moment else 0.0

            self.result.actual_counterbalance_moment = su.counterbalance_moment
            self.result.rotation_direction = rotation

            return {
                'A': su.dimensional_a,
                'B': su.structural_imbalance,
                'C': su.dimensional_c,
                'I': I,
                'K': K,
                'P': su.dimensional_p,
                'R': su.radius,
                'h': h,
                'phi': phi,
                'tau': tau,
                'M': M,
                'rotation': rotation,
            }
        except Exception:
            return None

    def _safe_acos(self, x: float) -> float:
        """Clamp value to [-1, 1] before computing arccos."""
        return math.acos(max(-1.0, min(1.0, x)))

    def _calculate_torque_parameters(self, geometry: dict) -> None:
        """Calculate psi, theta, and torque factor arrays."""
        position = np.array(self.ctx.surface_card.position, dtype=float)
        n = len(position)

        A = geometry['A']
        C = geometry['C']
        K = geometry['K']
        P = geometry['P']
        R = geometry['R']
        phi = geometry['phi']
        rotation = geometry['rotation']

        stroke_length = float(np.max(position))

        # Calculate psi at top and bottom of stroke
        psi_bottom = self._safe_acos((C**2 + K**2 - (P + R)**2) / (2 * C * K))
        psi_top = self._safe_acos((C**2 + K**2 - (P - R)**2) / (2 * C * K))

        # Initialize arrays
        psi = np.zeros(n)
        E = np.zeros(n)
        alpha = np.zeros(n)
        epsilon = np.zeros(n)
        beta = np.zeros(n)
        theta = np.zeros(n)
        torque_factor = np.zeros(n)

        # Calculate psi from position
        for i in range(n):
            psi[i] = psi_bottom - (position[i] / stroke_length) * (psi_bottom - psi_top)

        # Find transition point (top of stroke)
        transition_index = 0
        for i in range(1, n):
            if psi[i] > psi[i - 1]:
                transition_index = i - 1
                break

        self._transition_index = transition_index
        self.result.stroke_transition_index = transition_index

        # Calculate geometry at each point
        direction = rotation
        for i in range(n):
            if i > 0 and psi[i] > psi[i - 1]:
                direction = -rotation

            # Distance from equalizer bearing to slow speed shaft
            E[i] = math.sqrt(C**2 + K**2 - 2 * C * K * math.cos(psi[i]))

            # Alpha angle
            alpha[i] = direction * self._safe_acos(
                (P**2 + R**2 - E[i]**2) / (2 * P * R)
            )

            # Epsilon angle
            epsilon[i] = direction * self._safe_acos(
                (E[i]**2 + P**2 - R**2) / (2 * E[i] * P)
            )

            # Beta angle
            beta[i] = self._safe_acos(
                (C**2 + E[i]**2 - K**2) / (2 * C * E[i])
            ) - epsilon[i]

            # Theta angle (API 11E)
            theta[i] = beta[i] + psi[i] + phi - alpha[i]

            # Torque factor
            if abs(math.sin(beta[i])) > 1e-10:
                torque_factor[i] = (A * R / C) * (math.sin(alpha[i]) / math.sin(beta[i]))
            else:
                torque_factor[i] = 0.0

        self._psi = psi
        self._theta = theta
        self._torque_factor = torque_factor

    def _compute_analytical_estimate(self, geometry: dict) -> None:
        """
        Compute analytical estimate of optimal counterbalance moment.

        Uses the M_naive approach: find indices where torque peaks occur
        and solve for M that makes those peaks equal.
        """
        load = np.array(self.ctx.surface_card.load, dtype=float)
        B = geometry['B']
        tau = geometry['tau']
        M_actual = geometry['M']
        rotation = geometry['rotation']

        tf = self._torque_factor
        theta = self._theta
        idx = self._transition_index

        # Calculate a = TF * (F - B) and b = sin(theta + tau)
        a = tf * (load - B)
        b = np.sin(theta + tau)

        # Calculate torque at actual M
        torque_actual = rotation * (a - M_actual * b) / 1000.0

        # Find peak indices for up and down strokes
        upstroke_torque = torque_actual.copy()
        downstroke_torque = torque_actual.copy()

        # Mask out the wrong stroke
        upstroke_torque[idx:] = -np.inf
        downstroke_torque[:idx] = -np.inf

        i_up = int(np.argmax(upstroke_torque))
        i_down = int(np.argmax(downstroke_torque))

        # Solve for M that makes peaks equal: a[i] - M*b[i] = a[j] - M*b[j]
        # M = (a[i] - a[j]) / (b[i] - b[j])
        denominator = b[i_up] - b[i_down]
        if abs(denominator) > 1e-10:
            M_optimal = (a[i_up] - a[i_down]) / denominator
        else:
            M_optimal = M_actual

        self.result.estimated_optimal_moment = round(M_optimal / 1000.0, 1)  # Convert to M-in-lbs

    def _calculate_objective_values(self, geometry: dict) -> None:
        """Calculate objective function values at actual and optimal M."""
        load = np.array(self.ctx.surface_card.load, dtype=float)
        B = geometry['B']
        tau = geometry['tau']
        M_actual = geometry['M']
        rotation = geometry['rotation']

        tf = self._torque_factor
        theta = self._theta
        idx = self._transition_index

        # Torque at actual M
        a = tf * (load - B)
        b = np.sin(theta + tau)
        torque_actual = rotation * (a - M_actual * b) / 1000.0

        # Calculate peaks at actual M
        if idx > 0 and idx < len(torque_actual):
            max_up = float(np.max(torque_actual[:idx]))
            max_down = float(np.max(torque_actual[idx:]))
        else:
            max_up = float(np.max(torque_actual))
            max_down = max_up

        self.result.upstroke_peak_torque = round(max_up, 1)
        self.result.downstroke_peak_torque = round(max_down, 1)
        self.result.peak_torque_difference = round(abs(max_up - max_down), 1)
        self.result.objective_at_actual = round(abs(max_up - max_down), 1)

        # Torque at optimal M
        M_optimal = self.result.estimated_optimal_moment * 1000.0
        torque_optimal = rotation * (a - M_optimal * b) / 1000.0

        if idx > 0 and idx < len(torque_optimal):
            max_up_opt = float(np.max(torque_optimal[:idx]))
            max_down_opt = float(np.max(torque_optimal[idx:]))
        else:
            max_up_opt = float(np.max(torque_optimal))
            max_down_opt = max_up_opt

        self.result.objective_at_optimal = round(abs(max_up_opt - max_down_opt), 1)


def calculate_torque_balance(
    context: DynacardAnalysisContext,
    raise_on_error: bool = False,
) -> TorqueBalanceAnalysis:
    """
    Convenience function to calculate torque balance analysis.

    Args:
        context: Complete analysis context with surface card and surface unit data.
        raise_on_error: If True, raises exceptions on validation errors.
                       If False, returns result with warning message set.

    Returns:
        TorqueBalanceAnalysis with analytical counterbalance estimate.

    Raises:
        ValidationError: If raise_on_error=True and validation fails.
        ConfigurationError: If raise_on_error=True and configuration is invalid.
    """
    calculator = TorqueBalanceCalculator(context)
    if raise_on_error:
        return calculator.calculate()

    try:
        return calculator.calculate()
    except DynacardException as e:
        calculator.result.warning_message = e.message
        return calculator.result


def estimate_optimal_counterbalance(
    context: DynacardAnalysisContext,
) -> float:
    """
    Quick estimation of optimal counterbalance moment.

    Uses analytical method for fast estimation without full analysis.

    Args:
        context: Complete analysis context.

    Returns:
        Estimated optimal counterbalance moment in M-in-lbs.
    """
    result = calculate_torque_balance(context)
    return result.estimated_optimal_moment


def calculate_torque_objective(
    context: DynacardAnalysisContext,
    counterbalance_moment: float,
) -> float:
    """
    Calculate objective function value for a given counterbalance moment.

    Objective J(M) = |max(upstroke torque) - max(downstroke torque)|

    Args:
        context: Complete analysis context.
        counterbalance_moment: Counterbalance moment in M-in-lbs to evaluate.

    Returns:
        Objective function value (peak torque difference).
    """
    # Create modified context with specified counterbalance
    from copy import deepcopy
    ctx_copy = deepcopy(context)
    ctx_copy.surface_unit.counterbalance_moment = counterbalance_moment

    result = calculate_torque_balance(ctx_copy)
    return result.objective_at_actual
