# ABOUTME: Gear box loading analysis using API 11E torque calculations.
# ABOUTME: Calculates actual and balanced torque for pumping unit gearbox optimization.

import math
import numpy as np
from typing import Tuple, Optional, Dict, Any
from .models import (
    DynacardAnalysisContext,
    GearBoxLoadingAnalysis,
    TorqueStatistics,
)
from .base import BaseCalculator
from .exceptions import (
    DynacardException,
    ConfigurationError,
    ConvergenceError,
)


class GearBoxLoadingCalculator(BaseCalculator[GearBoxLoadingAnalysis]):
    """
    Calculates gear box torque loading per API 11E standard.

    Uses pumping unit geometry (A, C, I, K, P, R) to compute torque factors
    and actual/balanced torque curves throughout the pump stroke.

    Raises:
        ValidationError: If surface unit data is missing.
        ConfigurationError: If pumping unit geometry is invalid.
        ConvergenceError: If counterbalance optimization fails to converge.
    """

    def _create_result(self) -> GearBoxLoadingAnalysis:
        return GearBoxLoadingAnalysis()

    def calculate(self) -> GearBoxLoadingAnalysis:
        """
        Execute gear box loading analysis.

        Returns:
            GearBoxLoadingAnalysis with torque statistics and curves

        Raises:
            ValidationError: If surface unit data is missing.
            ConfigurationError: If pumping unit geometry is invalid.
        """
        # Validate surface unit geometry (raises exceptions on failure)
        self.validate_surface_unit_geometry()

        # Extract and prepare data
        position, load, rotation = self._prepare_card_data()
        pumping_unit = self._build_pumping_unit_dict()

        if pumping_unit is None:
            raise ConfigurationError(
                "Invalid pumping unit geometry - unable to build parameter dictionary",
                parameter="pumping_unit"
            )

        # Calculate torque using API Class I method
        stroke_length = float(np.max(position))
        samples = len(position)

        surface_unit_output = self._api_class_i(
            load, position, pumping_unit, stroke_length, samples, rotation
        )

        # Populate results
        self._populate_results(surface_unit_output)

        return self.result

    def _prepare_card_data(self) -> Tuple[np.ndarray, np.ndarray, int]:
        """
        Prepare surface card data and determine rotation direction.

        Returns:
            (position_array, load_array, rotation_direction)
        """
        # Convert to inches (assuming input is in inches already)
        position = np.array(self.ctx.surface_card.position, dtype=float)
        load = np.array(self.ctx.surface_card.load, dtype=float)

        # Determine rotation direction: 1 = CW, -1 = CCW
        geometry = self.ctx.surface_unit.geometry
        if geometry in ['Clockwise', 'CW']:
            rotation = 1
        else:  # C. Clockwise, CCW, or default
            rotation = -1

        # Rotate card data to start at minimum position
        min_pos_idx = int(np.argmin(position))
        if min_pos_idx != 0:
            position = np.roll(position, -min_pos_idx)
            load = np.roll(load, -min_pos_idx)

        return position, load, rotation

    def _build_pumping_unit_dict(self) -> Optional[Dict[str, float]]:
        """Build pumping unit parameter dictionary from context."""
        su = self.ctx.surface_unit

        # Get counterbalance moment in inch-lbs (input may be M-in-lbs)
        M = su.counterbalance_moment
        if M is None or np.isnan(M):
            M = 0.0
        else:
            M = M * 1000.0  # Convert from M-in-lbs to in-lbs

        try:
            # Calculate derived geometry parameters
            K = su.dimensional_k
            I = su.dimensional_i

            if K < I:
                return None

            h = math.sqrt(K ** 2 - I ** 2)
            phi = math.atan(I / h)
            tau = math.radians(su.phase_angle)

            return {
                'A': su.dimensional_a,
                'B': su.structural_imbalance,
                'C': su.dimensional_c,
                'I': I,
                'K': K,
                'P': su.dimensional_p,
                'phi': phi,
                'h': h,
                'tau': tau,
                'R1': su.radius,
                'M': M,
                'gearboxRating': su.gear_box_rating * 1000.0,  # Convert to in-lbs
                'structureRating': su.beam_rating,
                'maxStrokeLength': su.max_stroke_length,
            }
        except Exception:
            return None

    def _api_class_i(
        self,
        load: np.ndarray,
        position: np.ndarray,
        pumping_unit: Dict[str, float],
        stroke_length: float,
        samples: int,
        rotation: int
    ) -> Dict[str, Any]:
        """
        Calculate torque using API Class I (Conventional) geometry.

        Reference: API 11E - Specification for Pumping Units

        Args:
            load: Surface card load values (lbs)
            position: Surface card position values (inches)
            pumping_unit: Pumping unit geometry parameters
            stroke_length: Total stroke length (inches)
            samples: Number of data points
            rotation: Rotation direction (1=CW, -1=CCW)

        Returns:
            Dictionary with torque analysis results
        """
        # Extract API dimensions
        A = pumping_unit['A']
        B = pumping_unit['B']
        C = pumping_unit['C']
        K = pumping_unit['K']
        phi = pumping_unit['phi']
        P = pumping_unit['P']
        tau = pumping_unit['tau']
        R = pumping_unit['R1']
        M = pumping_unit['M']

        # Calculate psi at top and bottom of stroke (API 11E)
        psi_bottom = self._safe_acos((C**2 + K**2 - (P + R)**2) / (2 * C * K))
        psi_top = self._safe_acos((C**2 + K**2 - (P - R)**2) / (2 * C * K))

        # Initialize arrays
        psi = np.zeros(samples)
        E = np.zeros(samples)
        alpha = np.zeros(samples)
        epsilon = np.zeros(samples)
        beta = np.zeros(samples)
        theta = np.zeros(samples)
        torque_factor = np.zeros(samples)
        actual_torque = np.zeros(samples)
        balanced_torque = np.zeros(samples)
        crank_angle = np.zeros(samples)

        # Calculate psi from fractional polished rod position
        for i in range(samples):
            psi[i] = psi_bottom - (position[i] / stroke_length) * (psi_bottom - psi_top)

        # Find top of stroke transition
        top_of_stroke = 0
        direction = rotation
        for i in range(samples):
            if i == 0:
                direction = rotation
            elif psi[i] > psi[i - 1]:
                top_of_stroke = i - 1
                direction = -rotation
                break

        # Calculate geometry at each sample point
        direction = rotation
        for i in range(samples):
            # Check for upstroke/downstroke transition
            if i == 0:
                direction = rotation
            elif psi[i] > psi[i - 1]:
                top_of_stroke = i - 1
                direction = -rotation

            # Made up dimension E (distance from equalizer bearing to slow speed shaft)
            E[i] = math.sqrt(C**2 + K**2 - 2 * C * K * math.cos(psi[i]))

            # Alpha angle using law of cosines
            alpha[i] = direction * self._safe_acos(
                (P**2 + R**2 - E[i]**2) / (2 * P * R)
            )

            # Epsilon angle (between E and P)
            epsilon[i] = direction * self._safe_acos(
                (E[i]**2 + P**2 - R**2) / (2 * E[i] * P)
            )

            # Beta angle
            beta[i] = self._safe_acos(
                (C**2 + E[i]**2 - K**2) / (2 * C * E[i])
            ) - epsilon[i]

            # Theta angle (API 11E)
            theta[i] = beta[i] + psi[i] + phi - alpha[i]

            # Torque factor (API 11E)
            if abs(math.sin(beta[i])) > 1e-10:
                torque_factor[i] = (A * R / C) * (math.sin(alpha[i]) / math.sin(beta[i]))
            else:
                torque_factor[i] = 0.0

            # Net torque on gearbox (M-in-lbs = thousands of in-lbs)
            actual_torque[i] = rotation * (
                torque_factor[i] * (load[i] - B) - M * math.sin(theta[i] + tau)
            ) / 1000.0

            # Crank angle in degrees
            crank_angle[i] = math.degrees(theta[i]) * rotation
            if crank_angle[i] < 0:
                crank_angle[i] += 360

        # Calculate optimal counterbalance moment
        optimal_m, cb_status, balanced_torque = self._get_balanced_torque(
            B, M, balanced_torque, load, pumping_unit,
            rotation, samples, tau, theta, top_of_stroke, torque_factor
        )

        # Calculate statistics
        gearbox_rating = pumping_unit.get('gearboxRating', 1000000) / 1000  # M-in-lbs

        actual_max = round(float(np.max(actual_torque)), 1)
        actual_min = round(float(np.min(actual_torque)), 1)
        actual_abs_max = max(actual_max, -actual_min)

        balanced_max = round(float(np.max(balanced_torque)), 1)
        balanced_min = round(float(np.min(balanced_torque)), 1)
        balanced_abs_max = max(balanced_max, -balanced_min)

        actual_to_rated_max = round(actual_max / gearbox_rating * 100, 1) if gearbox_rating > 0 else 0
        actual_to_rated_min = round(actual_min / gearbox_rating * 100, 1) if gearbox_rating > 0 else 0
        actual_to_rated_abs_max = max(actual_to_rated_max, -actual_to_rated_min)

        balanced_to_rated_max = round(balanced_max / gearbox_rating * 100, 1) if gearbox_rating > 0 else 0
        balanced_to_rated_min = round(balanced_min / gearbox_rating * 100, 1) if gearbox_rating > 0 else 0
        balanced_to_rated_abs_max = max(balanced_to_rated_max, -balanced_to_rated_min)

        return {
            'CrankAngle': crank_angle.tolist(),
            'ActualTorque': actual_torque.tolist(),
            'BalancedTorque': balanced_torque.tolist(),
            'TorqueFactor': torque_factor.tolist(),
            'ActualTorqueStatistics': {
                'max': actual_max,
                'min': actual_min,
                'abs_max': actual_abs_max
            },
            'BalancedTorqueStatistics': {
                'max': balanced_max,
                'min': balanced_min,
                'abs_max': balanced_abs_max
            },
            'ActualToRatedPercent': {
                'max': actual_to_rated_max,
                'min': actual_to_rated_min,
                'abs_max': actual_to_rated_abs_max
            },
            'BalancedToRatedPercent': {
                'max': balanced_to_rated_max,
                'min': balanced_to_rated_min,
                'abs_max': balanced_to_rated_abs_max
            },
            'GearBoxRating': round(gearbox_rating, 1),
            'OptimalCounterbalanceMoment': round(optimal_m / 1000, 1),
            'ActualCounterbalanceMoment': round(M / 1000, 1),
            'CounterbalanceMomentCalculationStatus': cb_status
        }

    def _get_balanced_torque(
        self,
        B: float,
        M: float,
        balanced_torque: np.ndarray,
        load: np.ndarray,
        pumping_unit: Dict[str, float],
        rotation: int,
        samples: int,
        tau: float,
        theta: np.ndarray,
        top_of_stroke: int,
        torque_factor: np.ndarray
    ) -> Tuple[float, str, np.ndarray]:
        """
        Find optimal counterbalance moment to minimize peak torque difference.

        Uses iterative refinement to balance upstroke and downstroke torque peaks.

        Returns:
            (optimal_M, status, balanced_torque_array)
        """
        import time

        increment = pumping_unit.get('gearboxRating', 1000000)
        diff = increment / 1000
        M_opt = 0.0
        status = "converged"
        start_time = time.time()
        max_iterations = 1000

        iteration = 0
        while abs(increment) > 1 and iteration < max_iterations:
            iteration += 1

            for i in range(samples):
                balanced_torque[i] = rotation * (
                    torque_factor[i] * (load[i] - B) - M_opt * math.sin(theta[i] + tau)
                ) / 1000.0

            # Find max torque on upstroke and downstroke
            upstroke = balanced_torque[:top_of_stroke] if top_of_stroke > 0 else balanced_torque[:1]
            downstroke = balanced_torque[top_of_stroke:] if top_of_stroke < samples else balanced_torque[-1:]

            max_up = float(np.max(upstroke)) if len(upstroke) > 0 else 0.0
            max_down = float(np.max(downstroke)) if len(downstroke) > 0 else 0.0

            # Check convergence
            new_diff = abs(max_up - max_down)
            if new_diff > diff:
                increment = -increment * 0.1
            diff = new_diff
            M_opt = M_opt + increment

            # Timeout check
            if time.time() - start_time > 5:
                status = "unconverged"
                break

        return M_opt, status, balanced_torque

    @staticmethod
    def _safe_acos(x: float) -> float:
        """Clamp value to [-1, 1] before computing arccos."""
        x = max(-1.0, min(1.0, x))
        return math.acos(x)

    def _populate_results(self, output: Dict[str, Any]) -> None:
        """Populate GearBoxLoadingAnalysis from calculation output."""
        self.result.crank_angle = output['CrankAngle']
        self.result.actual_torque_curve = output['ActualTorque']
        self.result.balanced_torque_curve = output['BalancedTorque']
        self.result.torque_factor = output['TorqueFactor']

        self.result.actual_torque = TorqueStatistics(
            max_torque=output['ActualTorqueStatistics']['max'],
            min_torque=output['ActualTorqueStatistics']['min'],
            abs_max_torque=output['ActualTorqueStatistics']['abs_max']
        )

        self.result.balanced_torque = TorqueStatistics(
            max_torque=output['BalancedTorqueStatistics']['max'],
            min_torque=output['BalancedTorqueStatistics']['min'],
            abs_max_torque=output['BalancedTorqueStatistics']['abs_max']
        )

        self.result.actual_to_rated_percent = TorqueStatistics(
            max_torque=output['ActualToRatedPercent']['max'],
            min_torque=output['ActualToRatedPercent']['min'],
            abs_max_torque=output['ActualToRatedPercent']['abs_max']
        )

        self.result.balanced_to_rated_percent = TorqueStatistics(
            max_torque=output['BalancedToRatedPercent']['max'],
            min_torque=output['BalancedToRatedPercent']['min'],
            abs_max_torque=output['BalancedToRatedPercent']['abs_max']
        )

        self.result.gear_box_rating = output['GearBoxRating']
        self.result.actual_counterbalance_moment = output['ActualCounterbalanceMoment']
        self.result.optimal_counterbalance_moment = output['OptimalCounterbalanceMoment']
        self.result.counterbalance_status = output['CounterbalanceMomentCalculationStatus']


def calculate_gear_box_loading(
    context: DynacardAnalysisContext,
    raise_on_error: bool = False,
) -> GearBoxLoadingAnalysis:
    """
    Convenience function to calculate gear box loading.

    Args:
        context: Complete analysis context with surface card and surface unit data
        raise_on_error: If True, raises exceptions on validation/calculation errors.
                       If False, returns result with error status set.

    Returns:
        GearBoxLoadingAnalysis with torque curves and statistics

    Raises:
        ValidationError: If raise_on_error=True and validation fails.
        ConfigurationError: If raise_on_error=True and configuration is invalid.
    """
    calculator = GearBoxLoadingCalculator(context)
    if raise_on_error:
        return calculator.calculate()

    try:
        return calculator.calculate()
    except DynacardException as e:
        calculator.result.counterbalance_status = f"error: {e.message}"
        return calculator.result
