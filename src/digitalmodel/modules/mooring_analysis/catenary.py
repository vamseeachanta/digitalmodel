#!/usr/bin/env python3
"""
ABOUTME: Catenary analysis for mooring lines including geometry calculations,
tension distribution, and horizontal/vertical stiffness per DNV-OS-E301.
"""

import numpy as np
from typing import Dict, Optional
import logging

from .models import (
    MooringLineProperties,
    CatenaryResult,
    StiffnessResult
)

logger = logging.getLogger(__name__)


class CatenaryAnalyzer:
    """
    Analyze catenary mooring line geometry and tensions.

    Implements analytical catenary equations for suspended mooring lines.
    Assumes catenary shape with uniform weight per unit length.
    """

    def __init__(self, water_depth: float):
        """
        Initialize catenary analyzer.

        Args:
            water_depth: Water depth in meters
        """
        self.water_depth = water_depth
        self.g = 9.81  # Gravity acceleration (m/s²)

    def solve_catenary(
        self,
        line: MooringLineProperties,
        horizontal_tension: float,
        vertical_height: Optional[float] = None
    ) -> CatenaryResult:
        """
        Solve catenary equations for mooring line.

        Uses analytical catenary equations to determine line geometry and tension
        distribution. The catenary equation relates the horizontal tension H,
        line weight w, and geometry.

        Args:
            line: Mooring line properties
            horizontal_tension: Horizontal tension component (kN)
            vertical_height: Vertical span (m). If None, uses water_depth.

        Returns:
            CatenaryResult with geometry and tensions

        Raises:
            ValueError: If horizontal tension is too low for catenary solution
        """
        if horizontal_tension <= 0:
            raise ValueError("Horizontal tension must be positive")

        # Line weight per unit length in water (kN/m)
        w = line.weight_water * self.g / 1000.0

        if abs(w) < 1e-6:
            raise ValueError("Line weight too close to zero for catenary analysis")

        # Catenary parameter a = H/w (m)
        H = horizontal_tension
        a = H / w

        # Vertical height (default to water depth)
        z = vertical_height if vertical_height is not None else self.water_depth

        # Calculate suspended arc length using catenary equation
        # For vertical span z: s = a * sinh(z/a)
        # For large z/a, use exponential approximation to avoid overflow
        if abs(z / a) < 20:
            s = a * np.sinh(z / a)
        else:
            # Approximation for large arguments: sinh(x) ≈ exp(x)/2
            s = a * np.exp(z / a) / 2

        # Horizontal distance from touchdown to fairlead
        # x = a * (cosh(z/a) - 1) = a * arccosh(1 + z/a)
        if abs(z / a) < 20:
            x = a * (np.cosh(z / a) - 1)
        else:
            # Approximation: cosh(x) - 1 ≈ exp(x)/2 for large x
            x = a * np.exp(z / a) / 2

        # Tension at fairlead (top of catenary)
        # T = sqrt(H² + (w*s)²)
        T_fairlead = np.sqrt(H**2 + (w * s)**2)

        # Tension at touchdown point (vertical component = 0)
        T_touchdown = H

        # Angle at fairlead (degrees)
        # tan(θ) = w*s / H
        angle_fairlead = np.degrees(np.arctan2(w * s, H))

        # Grounded length (line lying on seabed)
        grounded_length = max(0.0, line.length - s)

        return CatenaryResult(
            horizontal_tension=H,
            fairlead_tension=T_fairlead,
            touchdown_tension=T_touchdown,
            arc_length=s,
            horizontal_distance=x,
            fairlead_angle=angle_fairlead,
            catenary_parameter=a,
            grounded_length=grounded_length,
            vertical_height=z
        )

    def solve_for_horizontal_tension(
        self,
        line: MooringLineProperties,
        target_fairlead_tension: float,
        vertical_height: Optional[float] = None,
        max_iterations: int = 100,
        tolerance: float = 0.01
    ) -> CatenaryResult:
        """
        Solve for horizontal tension given target fairlead tension.

        Uses iterative Newton-Raphson method to find H that produces
        the desired fairlead tension.

        Args:
            line: Mooring line properties
            target_fairlead_tension: Desired tension at fairlead (kN)
            vertical_height: Vertical span (m). If None, uses water_depth.
            max_iterations: Maximum iterations for convergence
            tolerance: Convergence tolerance (kN)

        Returns:
            CatenaryResult with converged solution

        Raises:
            RuntimeError: If solution doesn't converge
        """
        # Initial guess: H ≈ 0.8 * T_fairlead
        H = 0.8 * target_fairlead_tension

        z = vertical_height if vertical_height is not None else self.water_depth
        w = line.weight_water * self.g / 1000.0

        for iteration in range(max_iterations):
            # Calculate fairlead tension with current H
            result = self.solve_catenary(line, H, z)
            T_current = result.fairlead_tension

            # Check convergence
            error = T_current - target_fairlead_tension
            if abs(error) < tolerance:
                logger.debug(f"Converged in {iteration + 1} iterations")
                return result

            # Newton-Raphson update
            # dT/dH ≈ H/T
            dT_dH = H / T_current if T_current > 0 else 1.0
            H -= error / dT_dH

            # Ensure H stays positive
            H = max(0.1, H)

        raise RuntimeError(
            f"Failed to converge after {max_iterations} iterations. "
            f"Last error: {error:.2f} kN"
        )

    def calculate_stiffness(
        self,
        line: MooringLineProperties,
        catenary_result: CatenaryResult
    ) -> StiffnessResult:
        """
        Calculate horizontal and vertical stiffness of mooring line.

        Stiffness includes both geometric (catenary shape change) and
        elastic (material stretch) components.

        Args:
            line: Line properties
            catenary_result: Result from solve_catenary

        Returns:
            StiffnessResult with stiffness components
        """
        H = catenary_result.horizontal_tension
        w = line.weight_water * self.g / 1000.0
        a = catenary_result.catenary_parameter
        s = catenary_result.arc_length
        z = catenary_result.vertical_height

        # Geometric stiffness (due to catenary shape change)
        # k_geom = w * cosh(z/a) / sinh²(z/a)
        if abs(z / a) < 20:
            cosh_term = np.cosh(z / a)
            sinh_term = np.sinh(z / a)
            k_geom = w * cosh_term / (sinh_term**2) if sinh_term > 0 else 0.0
        else:
            # For large z/a: cosh/sinh² ≈ 1/sinh
            k_geom = w / np.sinh(z / a)

        # Elastic stiffness (due to material stretch)
        # k_elastic = EA / s
        k_elastic = line.ea / s if s > 0 else 0.0

        # Combined horizontal stiffness (series combination)
        # 1/k_total = 1/k_geom + 1/k_elastic
        if k_geom > 0 and k_elastic > 0:
            k_horizontal = 1 / (1/k_geom + 1/k_elastic)
        elif k_geom > 0:
            k_horizontal = k_geom
        else:
            k_horizontal = 0.0

        # Vertical stiffness (simplified)
        # For catenary: k_vertical ≈ k_horizontal * tan²(θ)
        if catenary_result.fairlead_angle > 0:
            tan_angle = np.tan(np.radians(catenary_result.fairlead_angle))
            k_vertical = k_horizontal * tan_angle**2
        else:
            k_vertical = 0.0

        return StiffnessResult(
            horizontal_stiffness=k_horizontal,
            vertical_stiffness=k_vertical,
            geometric_stiffness=k_geom,
            elastic_stiffness=k_elastic
        )

    def check_touchdown(
        self,
        line: MooringLineProperties,
        horizontal_distance: float
    ) -> Dict:
        """
        Check if line touches down on seabed for given horizontal distance.

        Args:
            line: Line properties
            horizontal_distance: Horizontal separation (m)

        Returns:
            Dictionary with touchdown status and minimum tension required
        """
        w = line.weight_water * self.g / 1000.0
        z = self.water_depth

        # Minimum H for touchdown
        # Occurs when s = line.length exactly
        # For given z: s = a*sinh(z/a) = line.length
        # This gives: a_min such that line.length = a_min * sinh(z/a_min)

        # Use iterative solution
        def s_from_a(a_val):
            if abs(z / a_val) < 20:
                return a_val * np.sinh(z / a_val)
            else:
                return a_val * np.exp(z / a_val) / 2

        # Binary search for a_min
        a_min = 0.1
        a_max = line.length * 2
        for _ in range(50):
            a_test = (a_min + a_max) / 2
            s_test = s_from_a(a_test)
            if s_test < line.length:
                a_min = a_test
            else:
                a_max = a_test

        a_critical = (a_min + a_max) / 2
        H_min = a_critical * abs(w)

        # Check actual horizontal distance achievable
        if abs(z / a_critical) < 20:
            x_max = a_critical * (np.cosh(z / a_critical) - 1)
        else:
            x_max = a_critical * np.exp(z / a_critical) / 2

        has_touchdown = horizontal_distance <= x_max

        return {
            'has_touchdown': has_touchdown,
            'min_horizontal_tension': H_min,
            'max_horizontal_reach': x_max,
            'required_horizontal_distance': horizontal_distance,
            'margin': x_max - horizontal_distance if has_touchdown else None
        }
