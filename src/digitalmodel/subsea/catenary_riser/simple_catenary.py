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
        - top_tension (closed-form solution)
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

        if not np.isfinite(w) or w <= 0:
            raise ValueError(
                f"Riser has negative or zero effective weight ({w:.2f} N/m). "
                "Check buoyancy - riser may float. Consider adding weight coating."
            )

        if not np.isfinite(water_depth) or water_depth <= 0:
            raise ValueError("water_depth must be finite and positive")

        converged = True
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
            H, converged = self._solve_H_from_geometry(water_depth, horizontal_offset, riser.length, w)
            result = self._solve_catenary_from_H(riser, H, water_depth, w)

        result.converged = bool(converged and result.arc_length <= riser.length)
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

        Governing equations: docs/engineering-basis.md E1-E3 (vertex origin).
        z is height, x is horizontal distance, w is weight in N/m.
        """
        if not np.isfinite(H) or H <= 0:
            raise ValueError("horizontal_tension must be finite and positive")
        a = H / w
        # E1: eliminate the hyperbolic parameter using cosh² - sinh² = 1.
        s = np.sqrt(z * (z + 2 * a))
        x = a * np.arccosh(1 + z / a)
        # E2: T = w*(a+z), monotonic in H.
        T_top = H + w * z
        T_td = H  # At touchdown, vertical component = 0

        # Angles
        angle_top = np.degrees(np.arctan2(H, w * s))  # E3: from vertical
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

    def _solve_H_from_top_tension(self, T_top: float, z: float, w: float) -> float:
        """E2 in docs/engineering-basis.md: H = T_top - w*z > 0."""
        if not np.isfinite(T_top) or T_top <= w * z:
            raise ValueError("top_tension must be finite and greater than w*depth")
        return T_top - w * z

    def _solve_H_from_geometry(
        self, z: float, x: float, L: float, w: float,
        tol: float = 1e-6, max_iter: int = 100,
    ) -> Tuple[float, bool]:
        """Bisect E1 on the available-length interval; return convergence.

        Narrow interpretation: x is the suspended span from the vertex,
        consistent with the explicit-H result. See engineering-basis.md E1.
        """
        if not all(np.isfinite(v) and v > 0 for v in (x, L, tol)) or L <= z:
            raise ValueError("geometry requires finite positive offset and length > depth")
        # E1 gives the maximum a that fits the available suspended length.
        low, high = 0.0, (L * L - z * z) / (2 * z)
        if x > high * np.arccosh(1 + z / high):
            raise ValueError("requested suspended span exceeds available riser length")
        a = high
        for _ in range(max_iter):
            a = (low + high) / 2
            residual = a * np.arccosh(1 + z / a) - x
            if abs(residual) <= tol * x:
                return a * w, True
            if residual < 0:
                low = a
            else:
                high = a
        return a * w, False

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
