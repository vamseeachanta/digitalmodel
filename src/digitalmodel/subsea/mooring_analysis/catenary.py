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
        # NIST 2022 CODATA standard gravity rounded to two decimal places (m/s²):
        # https://physics.nist.gov/cgi-bin/cuu/Value?gn
        self.g = 9.81

    def solve_catenary(
        self,
        line: MooringLineProperties,
        horizontal_tension: float,
        vertical_height: Optional[float] = None
    ) -> CatenaryResult:
        """
        Solve catenary equations for mooring line.

        Uses analytical catenary equations to determine line geometry and tension
        distribution for an inextensible, uniformly weighted line with zero
        vertical tension at touchdown. The vertical span is measured upward
        from that lowest point; horizontal_distance covers the suspended part.
        Arc length is the required suspended length, even if it exceeds the
        supplied line length; this method does not establish feasibility.

        Args:
            line: Mooring line properties
            horizontal_tension: Horizontal tension component (kN)
            vertical_height: Vertical span (m). If None, uses water_depth.

        Returns:
            CatenaryResult with geometry and tensions

        Raises:
            ValueError: If tension or submerged weight is not finite and positive,
                or vertical height is not finite and nonnegative.
        """
        if not np.isfinite(horizontal_tension) or horizontal_tension <= 0:
            raise ValueError("Horizontal tension must be finite and positive")

        # Line weight per unit length in water (kN/m)
        w = line.weight_water * self.g / 1000.0

        if not np.isfinite(w) or w <= 0:
            raise ValueError("Line weight must be finite and positive for touchdown")

        # Catenary parameter a = H/w (m)
        H = horizontal_tension
        a = H / w

        # Vertical height (default to water depth)
        z = vertical_height if vertical_height is not None else self.water_depth
        if not np.isfinite(z) or z < 0:
            raise ValueError("Vertical height must be finite and nonnegative")

        # Classical Mechanics (LibreTexts, 2017), section 18.3, equations
        # 18.3.4-18.3.6: y=a+z, y²=a²+s² -> s²=z²+2az.
        # https://phys.libretexts.org/@go/page/7046
        s = np.sqrt(z) * np.sqrt(z + 2 * a)

        # x=a*acosh(1+z/a)=a*asinh(s/a), from equations 18.3.4-18.3.5.
        # The asinh form avoids losing the small increment in 1+z/a.
        x = a * np.arcsinh(s / a)

        # Equation 18.3.9: T=w*y=w*(a+z)=H+w*z at fixed vertical height.
        T_fairlead = H + w * z

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

        Uses Newton-Raphson with the exact fixed-height derivative dT/dH=1
        from T=H+w*z (Classical Mechanics, 2017, equation 18.3.9).

        Args:
            line: Mooring line properties
            target_fairlead_tension: Desired tension at fairlead (kN)
            vertical_height: Vertical span (m). If None, uses water_depth.
            max_iterations: Maximum iterations for convergence
            tolerance: Convergence tolerance (kN)

        Returns:
            CatenaryResult with converged solution

        Raises:
            ValueError: If the target is not finite or does not exceed w*z,
                or the touchdown geometry inputs are invalid.
            RuntimeError: If solution doesn't converge
        """
        z = vertical_height if vertical_height is not None else self.water_depth
        w = line.weight_water * self.g / 1000.0
        if (
            not np.isfinite(target_fairlead_tension)
            or target_fairlead_tension <= w * z
        ):
            raise ValueError("Target fairlead tension must be finite and exceed w*z")

        # T itself is a positive initial guess for admissible touchdown inputs.
        H = target_fairlead_tension

        for iteration in range(max_iterations):
            # Calculate fairlead tension with current H
            result = self.solve_catenary(line, H, z)
            T_current = result.fairlead_tension

            # Check convergence
            error = T_current - target_fairlead_tension
            if abs(error) < tolerance:
                logger.debug(f"Converged in {iteration + 1} iterations")
                return result

            # dT/dH=1 at fixed z (equation 18.3.9 cited above); the new H is
            # T_target-w*z, positive by the admissibility check.
            H -= error

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
