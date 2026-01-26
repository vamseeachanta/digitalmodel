#!/usr/bin/env python3
"""
ABOUTME: Simple catenary riser analysis using analytical catenary equations
for static configuration, tensions, and angles per DNV-OS-F201.
"""

import numpy as np
from typing import Optional, Tuple, Dict
import logging

from .models import (
    RiserConfiguration,
    CatenaryRiserResult,
)

logger = logging.getLogger(__name__)


class SimpleCatenaryAnalyzer:
    """
    Simple catenary riser analysis.

    Uses analytical catenary equations for suspended riser configuration.
    Assumes:
    - Uniform effective weight along riser
    - No bending stiffness (flexible riser)
    - Static equilibrium
    - No current loads

    Standards: DNV-OS-F201, API RP 1111
    """

    def __init__(self, gravity: float = 9.81):
        """
        Initialize catenary analyzer.

        Args:
            gravity: Gravitational acceleration (m/s²)
        """
        self.g = gravity

    def analyze_riser(
        self,
        riser: RiserConfiguration,
        horizontal_tension: Optional[float] = None,
        top_tension: Optional[float] = None,
        water_depth: Optional[float] = None,
        horizontal_offset: Optional[float] = None,
    ) -> CatenaryRiserResult:
        """
        Analyze simple catenary riser configuration.

        Can solve for configuration given either:
        - horizontal_tension (most direct)
        - top_tension (iterative solution)
        - Both water_depth and horizontal_offset (iterative solution)

        Args:
            riser: Riser configuration
            horizontal_tension: Horizontal tension (N)
            top_tension: Top tension (N)
            water_depth: Water depth (m)
            horizontal_offset: Horizontal offset at surface (m)

        Returns:
            CatenaryRiserResult with geometry and forces
        """
        # Use riser properties if not provided
        if water_depth is None:
            water_depth = riser.water_depth
        if horizontal_offset is None:
            horizontal_offset = riser.horizontal_offset

        if water_depth is None or horizontal_offset is None:
            raise ValueError("Must provide water_depth and horizontal_offset")

        # Effective weight
        w = riser.effective_weight_per_length

        if w <= 0:
            raise ValueError(
                f"Riser has negative or zero effective weight ({w:.2f} N/m). "
                "Check buoyancy - riser may float. Consider adding weight coating."
            )

        # Solve for catenary
        if horizontal_tension is not None:
            # Direct solution
            result = self._solve_catenary_from_H(
                riser, horizontal_tension, water_depth, w
            )
        elif top_tension is not None:
            # Solve for H given top tension
            H = self._solve_H_from_top_tension(top_tension, water_depth, w)
            result = self._solve_catenary_from_H(riser, H, water_depth, w)
        else:
            # Solve for H given geometry
            H = self._solve_H_from_geometry(water_depth, horizontal_offset, riser.length, w)
            result = self._solve_catenary_from_H(riser, H, water_depth, w)

        return result

    def _solve_catenary_from_H(
        self,
        riser: RiserConfiguration,
        H: float,
        z: float,
        w: float
    ) -> CatenaryRiserResult:
        """
        Solve catenary given horizontal tension.

        Catenary equations:
        - a = H / w  (catenary parameter)
        - s = a * sinh(z / a)  (arc length)
        - x = a * (cosh(z / a) - 1)  (horizontal distance)
        - T = sqrt(H² + (w*s)²)  (tension)

        Args:
            riser: Riser configuration
            H: Horizontal tension (N)
            z: Vertical height (m)
            w: Effective weight per length (N/m)

        Returns:
            CatenaryRiserResult
        """
        # Catenary parameter
        a = H / w

        # Arc length (suspended)
        if abs(z / a) < 20:
            s = a * np.sinh(z / a)
            x = a * (np.cosh(z / a) - 1)
        else:
            # Approximation for large z/a
            s = a * np.exp(z / a) / 2
            x = a * np.exp(z / a) / 2

        # Tensions
        T_top = np.sqrt(H**2 + (w * s)**2)
        T_td = H  # At touchdown, vertical component = 0

        # Angles
        angle_top = np.degrees(np.arctan2(w * s, H))  # from vertical
        angle_td = 0.0  # At touchdown, angle = 0 from horizontal

        # Grounded length
        grounded = max(0.0, riser.length - s)

        # Maximum tension (at top for catenary)
        T_max = T_top

        # Maximum curvature (at touchdown)
        curvature_td = w / H

        return CatenaryRiserResult(
            riser_name=riser.name,
            water_depth=z,
            horizontal_offset=x,
            horizontal_tension=H,
            catenary_parameter=a,
            effective_weight=w,
            arc_length=s,
            grounded_length=grounded,
            top_tension=T_top,
            touchdown_tension=T_td,
            top_angle=angle_top,
            touchdown_angle=angle_td,
            max_tension=T_max,
            max_curvature=curvature_td,
        )

    def _solve_H_from_top_tension(
        self,
        T_top: float,
        z: float,
        w: float,
        tol: float = 1e-6,
        max_iter: int = 100
    ) -> float:
        """
        Solve for horizontal tension given top tension.

        T_top² = H² + (w*s)²
        where s = (H/w) * sinh(z*w/H)

        Uses bisection method for robustness.

        Args:
            T_top: Top tension (N)
            z: Vertical height (m)
            w: Effective weight per length (N/m)
            tol: Tolerance for convergence
            max_iter: Maximum iterations

        Returns:
            Horizontal tension H (N)
        """
        def calc_top_tension(H: float) -> float:
            """Calculate top tension for given H"""
            a = H / w
            ratio = z / a

            # Protect against overflow - sinh overflows around ratio > 710
            # Use approximation for large ratios
            if ratio > 10:
                # For large ratio: sinh(x) ≈ exp(x)/2
                # s = a * sinh(ratio) ≈ a * exp(ratio) / 2
                # But exp(ratio) overflows for ratio > ~700
                # Instead, compute log of s and T
                log_s = ratio + np.log(a / 2)
                log_ws = np.log(w) + log_s
                # T = sqrt(H² + (ws)²)
                # For large ws >> H: T ≈ ws
                return w * np.exp(log_s)
            else:
                s = a * np.sinh(ratio)
                return np.sqrt(H**2 + (w * s)**2)

        # For a catenary, T_top has a MINIMUM at some optimal H
        # - At very low H: T is very large (long arc, deep sag)
        # - At optimal H: T is at minimum
        # - At very high H: T increases again (nearly straight riser)
        #
        # We want the high-H solution (more practical, nearly straight riser)

        # Find the optimal H that gives minimum T (using scipy if available, else estimate)
        # For now, estimate optimal H ≈ 0.83 * w * z (empirical)
        H_optimal = 0.83 * w * z
        T_min = calc_top_tension(H_optimal)

        if T_top < T_min * 0.99:
            logger.warning(
                f"Requested top tension {T_top:.0f} N is less than minimum "
                f"achievable {T_min:.0f} N for {z:.0f}m depth."
            )
            return H_optimal  # Return H at minimum as best approximation

        # Search for H on the high-H side (H > H_optimal)
        # In this region, T increases as H increases

        # Upper bound: H < T_top (horizontal can't exceed total)
        H_max = T_top * 0.999

        # Lower bound: start just above optimal point
        H_min = H_optimal * 1.05

        # Verify bounds
        T_at_min = calc_top_tension(H_min)
        T_at_max = calc_top_tension(H_max)

        # Adjust bounds if needed
        while T_at_min > T_top and H_min > H_optimal:
            H_min = (H_min + H_optimal) / 2
            T_at_min = calc_top_tension(H_min)

        if T_at_max < T_top:
            logger.warning(f"Cannot achieve T_top={T_top:.0f}N with H < {H_max:.0f}N")
            return H_max

        # Bisection: on high-H side, T increases with H
        for i in range(max_iter):
            H_mid = (H_min + H_max) / 2
            T_mid = calc_top_tension(H_mid)

            if abs(T_mid - T_top) < tol * T_top:
                logger.debug(f"Bisection converged in {i+1} iterations: H = {H_mid:.2f} N")
                return H_mid

            if T_mid < T_top:
                H_min = H_mid  # T too low, need larger H
            else:
                H_max = H_mid  # T too high, need smaller H

        # Return best estimate
        return (H_min + H_max) / 2

    def _solve_H_from_geometry(
        self,
        z: float,
        x: float,
        L: float,
        w: float,
        tol: float = 1e-6,
        max_iter: int = 100
    ) -> float:
        """
        Solve for horizontal tension given geometry.

        Given:
        - z: vertical height
        - x: horizontal offset
        - L: total riser length
        - w: effective weight

        Find H such that:
        - s = a * sinh(z / a) ≤ L  (suspended length)
        - x = a * (cosh(z / a) - 1)  (horizontal offset)

        Args:
            z: Vertical height (m)
            x: Horizontal offset (m)
            L: Total riser length (m)
            w: Effective weight per length (N/m)
            tol: Tolerance
            max_iter: Maximum iterations

        Returns:
            Horizontal tension H (N)
        """
        # Initial guess based on straight line
        H = w * z * x / (2 * z)

        for i in range(max_iter):
            a = H / w

            # Suspended length
            s = a * np.sinh(z / a)

            # Horizontal offset from catenary
            x_calc = a * (np.cosh(z / a) - 1)

            # Residual
            residual = x_calc - x

            if abs(residual) < tol * x and s <= L:
                logger.debug(f"Converged in {i+1} iterations: H = {H:.2f} N")
                return H

            # Derivative dx/dH
            dx_dH = (np.cosh(z / a) - 1) / w - (z / a) * np.sinh(z / a) / w

            # Update
            H = H - residual / dx_dH

            # Keep H positive and reasonable
            H = max(H, 100.0)
            H = min(H, w * L * 10)  # Upper bound

        logger.warning(f"Did not converge in {max_iter} iterations")
        return H

    def calculate_tension_along_riser(
        self,
        result: CatenaryRiserResult,
        n_points: int = 50
    ) -> Tuple[np.ndarray, np.ndarray]:
        """
        Calculate tension distribution along suspended catenary.

        Args:
            result: Catenary result
            n_points: Number of points along riser

        Returns:
            Tuple of (arc_length_array, tension_array)
        """
        H = result.horizontal_tension
        w = result.effective_weight

        # Arc length points
        s_points = np.linspace(0, result.arc_length, n_points)

        # Tension at each point
        T_points = np.sqrt(H**2 + (w * s_points)**2)

        return s_points, T_points

    def calculate_angle_along_riser(
        self,
        result: CatenaryRiserResult,
        n_points: int = 50
    ) -> Tuple[np.ndarray, np.ndarray]:
        """
        Calculate angle distribution along suspended catenary.

        Args:
            result: Catenary result
            n_points: Number of points along riser

        Returns:
            Tuple of (arc_length_array, angle_from_horizontal_array)
        """
        H = result.horizontal_tension
        w = result.effective_weight

        # Arc length points
        s_points = np.linspace(0, result.arc_length, n_points)

        # Angle at each point (from horizontal)
        angles = np.degrees(np.arctan(w * s_points / H))

        return s_points, angles

    def check_design_limits(
        self,
        result: CatenaryRiserResult,
        max_top_tension: Optional[float] = None,
        min_touchdown_angle: Optional[float] = None,
        max_curvature: Optional[float] = None
    ) -> Dict[str, bool]:
        """
        Check riser against design limits.

        Args:
            result: Catenary result
            max_top_tension: Maximum allowable top tension (N)
            min_touchdown_angle: Minimum touchdown angle (degrees from horizontal)
            max_curvature: Maximum allowable curvature (1/m)

        Returns:
            Dictionary of check results
        """
        checks = {}

        if max_top_tension:
            checks['top_tension_ok'] = result.top_tension <= max_top_tension

        if min_touchdown_angle:
            checks['touchdown_angle_ok'] = result.touchdown_angle >= min_touchdown_angle

        if max_curvature:
            checks['curvature_ok'] = result.max_curvature <= max_curvature

        checks['all_pass'] = all(checks.values())

        return checks
