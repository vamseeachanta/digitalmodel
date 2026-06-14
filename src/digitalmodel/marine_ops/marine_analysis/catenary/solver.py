"""
Catenary Solver for Mooring Line Analysis - FINAL CORRECT VERSION

Implements the proper 2D catenary boundary value problem solver.

For a mooring line with:
  - Length L (unstretched)
  - Horizontal span X (anchor to fairlead)
  - Vertical span Y (fairlead elevation above anchor)
  - Weight per length w
  - Axial stiffness EA

Find horizontal tension H such that the catenary:
  1. Passes through anchor at (0, 0)
  2. Passes through fairlead at (X, Y)
  3. Has total arc length + elongation = L

This solver uses the general catenary formulation where both anchor and fairlead
may be above the catenary's low point.

Author: Digital Model Project
"""

from dataclasses import dataclass
from typing import Optional, Tuple
import numpy as np
from scipy.optimize import brentq


@dataclass
class CatenaryInput:
    """Input parameters for catenary analysis."""
    length: float              # Unstretched line length [m]
    horizontal_span: float     # Horizontal distance [m]
    vertical_span: float       # Vertical distance (+ = fairlead higher) [m]
    weight_per_length: float   # Submerged weight [N/m]
    ea_stiffness: float        # Axial stiffness EA [N]
    water_depth: Optional[float] = None
    seabed_friction: float = 0.0


@dataclass
class CatenaryResults:
    """Results from catenary analysis."""
    horizontal_tension: float
    vertical_tension_fairlead: float
    total_tension_fairlead: float
    total_tension_anchor: float
    elongation: float
    touchdown_distance: Optional[float]
    catenary_parameter: float
    shape_x: np.ndarray
    shape_y: np.ndarray
    tension_distribution: np.ndarray
    converged: bool
    iterations: int


class CatenarySolver:
    """
    Catenary solver using general 2D boundary value problem formulation.

    Solves for horizontal tension H and catenary geometry such that
    the line passes through both specified endpoints with correct length.
    """

    def __init__(self, tolerance: float = 1e-6, max_iterations: int = 200):
        """Initialize the catenary solver.

        Args:
            tolerance: Convergence tolerance for the iterative solver.
            max_iterations: Maximum number of Newton-Raphson iterations.
        """
        self.tolerance = tolerance
        self.max_iterations = max_iterations

    def solve(self, params: CatenaryInput) -> CatenaryResults:
        """
        Solve 2D catenary boundary value problem.

        Parameters
        ----------
        params : CatenaryInput
            Catenary input parameters

        Returns
        -------
        results : CatenaryResults
            Complete catenary solution

        Raises
        ------
        ValueError
            If inputs are invalid or solution cannot be found
        """
        # Validate
        if params.length <= 0:
            raise ValueError("Length must be positive")
        if params.horizontal_span <= 0:
            raise ValueError("Horizontal span must be positive")
        if params.weight_per_length <= 0:
            raise ValueError("Weight per length must be positive")
        if params.ea_stiffness <= 0:
            raise ValueError("EA stiffness must be positive")

        straight_dist = np.sqrt(params.horizontal_span**2 + params.vertical_span**2)
        if params.length < straight_dist:
            raise ValueError(f"Length {params.length}m < straight distance {straight_dist:.2f}m")

        # Solve for H and signed low-point geometry.
        H_solution, u_anchor, u_fairlead, converged, iterations = (
            self._solve_general_catenary(params)
        )

        # Compute all results
        return self._compute_results(
            params, H_solution, u_anchor, u_fairlead, converged, iterations
        )

    def _solve_general_catenary(
        self, params: CatenaryInput
    ) -> Tuple[float, float, float, bool, int]:
        """
        Solve the general catenary BVP for H and endpoint positions.

        Coordinates are signed distances from the catenary low point. This
        allows the low point to lie left of the anchor, between endpoints, or
        right of the fairlead.

        Midpoint identities reduce the geometry to a one-variable solve in
        ``a = H / w``. When elastic stretch creates a second very high-tension
        root, the first bracketed root is the physically relevant sagging line.

        Returns
        -------
        H : float
            Horizontal tension
        u_anchor : float
            Signed coordinate of the anchor from the low point.
        u_fairlead : float
            Signed coordinate of the fairlead from the low point.
        converged : bool
            Convergence flag
        iterations : int
            Number of iterations
        """
        scale = max(
            params.length,
            params.horizontal_span,
            abs(params.vertical_span),
            1.0
        )
        a_low = max(scale * 1e-3, 1e-9)
        f_low = self._length_error_for_a(a_low, params)
        iterations = 1
        while np.isfinite(f_low) and f_low < 0.0:
            a_low *= 0.5
            f_low = self._length_error_for_a(a_low, params)
            iterations += 1

        a_high = a_low
        f_high = f_low
        for _ in range(max(self.max_iterations, 200)):
            a_high *= 2.0
            f_high = self._length_error_for_a(a_high, params)
            iterations += 1
            if np.isfinite(f_low) and np.isfinite(f_high) and f_low * f_high <= 0:
                break
            a_low = a_high
            f_low = f_high
        else:
            raise ValueError("Failed to bracket catenary solution")

        root, result = brentq(
            lambda a: self._length_error_for_a(a, params),
            a_low,
            a_high,
            xtol=self.tolerance,
            maxiter=self.max_iterations,
            full_output=True
        )

        _, u_anchor, u_fairlead = self._geometry_for_a(root, params)
        H_sol = root * params.weight_per_length
        return (
            H_sol,
            u_anchor,
            u_fairlead,
            result.converged,
            iterations + result.iterations
        )

    def _length_error_for_a(self, a: float, params: CatenaryInput) -> float:
        """Return length error for a catenary parameter."""
        arc_length, _, _ = self._geometry_for_a(a, params)
        if not np.isfinite(arc_length):
            return np.inf

        horizontal_tension = a * params.weight_per_length
        elongation = horizontal_tension * params.length / params.ea_stiffness
        return arc_length + elongation - params.length

    def _geometry_for_a(
        self, a: float, params: CatenaryInput
    ) -> Tuple[float, float, float]:
        """Return arc length and signed endpoint coordinates for ``a``."""
        half_span_over_a = params.horizontal_span / (2.0 * a)
        if half_span_over_a > 350.0:
            return np.inf, -np.inf, np.inf

        sinh_half_span = np.sinh(half_span_over_a)
        midpoint_sinh = params.vertical_span / (2.0 * a * sinh_half_span)
        midpoint = np.arcsinh(midpoint_sinh)
        midpoint_cosh = np.sqrt(1.0 + midpoint_sinh**2)

        arc_length = 2.0 * a * midpoint_cosh * sinh_half_span
        u_anchor = a * (midpoint - half_span_over_a)
        u_fairlead = a * (midpoint + half_span_over_a)
        return arc_length, u_anchor, u_fairlead

    def _solve_simplified(self, params: CatenaryInput) -> Tuple[float, bool, int]:
        """
        Simplified solver assuming low point at anchor.

        For catenary with low point at anchor (0,0):
          y(x) = a*(cosh(x/a) - 1)
          s(x) = a*sinh(x/a)

        Solve for H such that arc length + elongation = L.

        This may NOT satisfy the vertical span constraint exactly.
        """

        def length_error(H):
            """Compute arc length error for a given horizontal tension.

            Args:
                H: Horizontal tension [N or consistent force unit].

            Returns:
                Difference between computed arc length (with elongation)
                and the specified line length.
            """
            if H <= 0:
                return 1e10

            a = H / params.weight_per_length
            x_a = params.horizontal_span / a

            if x_a > 700:  # Avoid overflow
                return 1e10

            s = a * np.sinh(x_a)
            elong = H * params.length / params.ea_stiffness

            return (s + elong) - params.length

        # Bounds for H
        H_min = 100.0
        H_max = params.weight_per_length * params.length * 100

        try:
            H_sol = brentq(
                length_error,
                H_min,
                H_max,
                xtol=self.tolerance,
                maxiter=self.max_iterations
            )
            return H_sol, True, 0

        except (ValueError, RuntimeError) as e:
            raise ValueError(f"Failed to solve catenary: {e}")

    def _compute_results(
        self,
        params: CatenaryInput,
        H: float,
        u_anchor: float,
        u_fairlead: float,
        converged: bool,
        iterations: int
    ) -> CatenaryResults:
        """Compute all catenary results from solved H."""

        a = H / params.weight_per_length

        # Endpoint tensions from signed low-point coordinates.
        V_fairlead = H * np.sinh(u_fairlead / a)
        V_anchor = H * np.sinh(u_anchor / a)
        T_fairlead = np.sqrt(H**2 + V_fairlead**2)
        T_anchor = np.sqrt(H**2 + V_anchor**2)

        # Elongation
        elongation = H * params.length / params.ea_stiffness

        # Touchdown
        touchdown = None
        if params.water_depth is not None:
            target = params.water_depth / a + np.cosh(u_anchor / a)
            if target >= 1.0:
                root = a * np.arccosh(target)
                candidates = []
                for u_touchdown in (-root, root):
                    if u_anchor <= u_touchdown <= u_fairlead:
                        candidates.append(u_touchdown - u_anchor)
                touchdown = min(candidates) if candidates else None

        # Line shape
        n_points = 100
        u = np.linspace(u_anchor, u_fairlead, n_points)
        x = u - u_anchor
        y = a * (np.cosh(u / a) - np.cosh(u_anchor / a))

        # Tension distribution
        vertical_tension = H * np.sinh(u / a)
        tension_dist = np.sqrt(H**2 + vertical_tension**2)

        return CatenaryResults(
            horizontal_tension=H,
            vertical_tension_fairlead=V_fairlead,
            total_tension_fairlead=T_fairlead,
            total_tension_anchor=T_anchor,
            elongation=elongation,
            touchdown_distance=touchdown,
            catenary_parameter=a,
            shape_x=x,
            shape_y=y,
            tension_distribution=tension_dist,
            converged=converged,
            iterations=iterations
        )
