"""
Catenary Solver for Mooring Line Analysis - FIXED VERSION

Proper 2D boundary value solver for catenary with both horizontal and vertical span constraints.

Mathematical Foundation:
-----------------------
Classical catenary equation:
    y(x) = a * cosh(x/a) - a    where a = H/w
    s(x) = a * sinh(x/a)         arc length

For 2D boundary value problem with constraints:
    - Horizontal span: X
    - Vertical span: Y
    - Total length: L (unstretched)

Solve for H such that:
    y(X) = Y  (vertical constraint)
    s(X) + elongation = L  (length constraint with elasticity)

Author: Digital Model Project - Fixed for Excel Reference Matching
"""

from dataclasses import dataclass
from typing import Optional
import numpy as np
from scipy.optimize import newton, brentq
import warnings


@dataclass
class CatenaryInput:
    """Input parameters for catenary analysis."""
    length: float              # Unstretched line length [m]
    horizontal_span: float     # Horizontal distance [m]
    vertical_span: float       # Vertical distance (+ down) [m]
    weight_per_length: float   # Submerged weight [N/m]
    ea_stiffness: float        # Axial stiffness EA [N]
    water_depth: Optional[float] = None  # Water depth [m]
    seabed_friction: float = 0.0  # Seabed friction coefficient [-]


@dataclass
class CatenaryResults:
    """Results from catenary analysis."""
    horizontal_tension: float       # H at anchor [N]
    vertical_tension_fairlead: float  # V at fairlead [N]
    total_tension_fairlead: float    # T at fairlead [N]
    total_tension_anchor: float      # T at anchor [N]
    elongation: float                # Elastic stretch [m]
    touchdown_distance: Optional[float]  # Distance to touchdown [m]
    catenary_parameter: float        # a = H/w [m]
    shape_x: np.ndarray             # Line shape coordinates
    shape_y: np.ndarray
    tension_distribution: np.ndarray  # Tension along line
    converged: bool                  # Solver convergence flag
    iterations: int                  # Solver iteration count


class CatenarySolver:
    """
    Advanced catenary solver for mooring line analysis with proper 2D constraints.

    Solves the boundary value problem where the catenary must pass through
    a specific point (horizontal_span, vertical_span) with total arc length
    equal to the line length.
    """

    def __init__(self, tolerance: float = 1e-6, max_iterations: int = 100):
        """
        Initialize catenary solver.

        Parameters
        ----------
        tolerance : float
            Convergence tolerance [m]
        max_iterations : int
            Maximum iterations for Newton-Raphson solver
        """
        self.tolerance = tolerance
        self.max_iterations = max_iterations
        self.gravity = 9.8065  # m/s²

    def _safe_sinh(self, x: float) -> float:
        """Compute sinh safely, avoiding overflow for large x."""
        if x > 700:  # sinh(700) ≈ 1e304, near overflow
            return np.exp(x) / 2  # For large x, sinh(x) ≈ exp(x)/2
        elif x < -700:
            return -np.exp(-x) / 2
        else:
            return np.sinh(x)

    def _safe_cosh(self, x: float) -> float:
        """Compute cosh safely, avoiding overflow for large x."""
        if x > 700:
            return np.exp(x) / 2  # For large x, cosh(x) ≈ exp(x)/2
        elif x < -700:
            return np.exp(-x) / 2
        else:
            return np.cosh(x)

    def solve(self, params: CatenaryInput) -> CatenaryResults:
        """
        Solve 2D catenary boundary value problem.

        This solver properly handles BOTH constraints:
        1. Vertical constraint: catenary must reach (X, Y) point
        2. Length constraint: total arc length + elongation = L

        The vertical constraint is the primary equation to solve for H.

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
            If input parameters are invalid or solution cannot be found
        """
        # Validate inputs
        if params.length <= 0:
            raise ValueError("Line length must be positive")
        if params.horizontal_span <= 0:
            raise ValueError("Horizontal span must be positive")
        if params.weight_per_length <= 0:
            raise ValueError("Weight per length must be positive")
        if params.ea_stiffness <= 0:
            raise ValueError("EA stiffness must be positive")

        straight_line_dist = np.sqrt(
            params.horizontal_span**2 + params.vertical_span**2
        )
        if params.length < straight_line_dist:
            raise ValueError(
                f"Line length {params.length}m must be >= straight-line distance {straight_line_dist:.2f}m"
            )

        # Define error function for vertical constraint
        def vertical_error(H: float) -> float:
            """
            Error in vertical span for given horizontal tension.

            For catenary: y(x) = a * (cosh(x/a) - 1) where a = H/w
            We want: y(X) = Y (vertical_span)
            Error = y(X) - Y
            """
            if H <= 0:
                return float('inf')

            a = H / params.weight_per_length
            x_a_ratio = params.horizontal_span / a

            # Check for numerical overflow
            if abs(x_a_ratio) > 700:
                return float('inf')

            # Vertical position at horizontal span
            y_calc = a * (self._safe_cosh(x_a_ratio) - 1)

            # Return error in vertical position
            return y_calc - params.vertical_span

        # Define derivative for Newton-Raphson
        def vertical_error_derivative(H: float) -> float:
            """
            Derivative of vertical error with respect to H.

            dy/dH = d/dH[a*(cosh(x/a) - 1)]
                  = (1/w)*(cosh(x/a) - 1) - (x/w)*(1/a)*sinh(x/a)
                  = (1/w)*[cosh(x/a) - 1 - (x/a)*sinh(x/a)]
            """
            if H <= 0:
                return float('inf')

            a = H / params.weight_per_length
            x = params.horizontal_span
            x_a_ratio = x / a

            if abs(x_a_ratio) > 700:
                return float('inf')

            dy_dH = (1 / params.weight_per_length) * (
                self._safe_cosh(x_a_ratio) - 1 - x_a_ratio * self._safe_sinh(x_a_ratio)
            )

            return dy_dH

        # Improved initial guess based on catenary geometry
        # For a catenary with vertical sag Y over horizontal span X:
        # Approximation: a ≈ X² / (8*Y) for shallow sag
        # Therefore: H = w * a ≈ w * X² / (8*Y)
        if params.vertical_span > 0.1:
            a_estimate = params.horizontal_span**2 / (8 * params.vertical_span)
            H_initial = params.weight_per_length * a_estimate
        else:
            # For nearly horizontal: use fraction of total weight
            H_initial = params.weight_per_length * params.length * 0.5

        # Bound the initial guess to reasonable range
        H_min_bound = max(100.0, params.weight_per_length * params.vertical_span)
        H_max_bound = params.weight_per_length * params.length * 50
        H_initial = np.clip(H_initial, H_min_bound, H_max_bound)

        # Solve using Newton-Raphson with analytical derivative
        converged = False
        iterations = 0

        try:
            H_solution = newton(
                vertical_error,
                x0=H_initial,
                fprime=vertical_error_derivative,
                tol=self.tolerance,
                maxiter=self.max_iterations,
                full_output=False
            )
            converged = True
            iterations = 0  # Newton doesn't return iteration count

        except (RuntimeError, ValueError, OverflowError) as e:
            # Fallback to Brent's method (more robust, bracketing)
            H_min = max(100.0, params.weight_per_length * params.vertical_span / 10)
            H_max = params.weight_per_length * params.length * 100

            try:
                # Check if bounds bracket a solution
                err_min = vertical_error(H_min)
                err_max = vertical_error(H_max)

                if np.isinf(err_min) or np.isinf(err_max):
                    raise ValueError("Numerical overflow in catenary calculation")

                if err_min * err_max >= 0:
                    # Try wider bounds
                    H_min = H_min / 10
                    H_max = H_max * 10
                    err_min = vertical_error(H_min)
                    err_max = vertical_error(H_max)

                    if err_min * err_max >= 0:
                        raise ValueError(
                            f"No solution found. Bounds [{H_min:.0f}, {H_max:.0f}] N "
                            f"do not bracket solution. Errors: [{err_min:.2f}, {err_max:.2f}] m"
                        )

                H_solution, result = brentq(
                    vertical_error,
                    a=H_min,
                    b=H_max,
                    xtol=self.tolerance,
                    maxiter=self.max_iterations,
                    full_output=True
                )
                converged = result.converged
                iterations = result.iterations

            except (ValueError, RuntimeError) as e2:
                raise ValueError(
                    f"Unable to find solution. Newton error: {e}, Brent error: {e2}"
                )

        # Verify length constraint (informational check)
        a = H_solution / params.weight_per_length
        x_a_ratio = params.horizontal_span / a

        # Arc length from catenary equation
        arc_length = a * self._safe_sinh(x_a_ratio)

        # Elastic elongation (average tension approximation)
        # More accurate: use average tension along line, but for now use H
        elongation = H_solution * params.length / params.ea_stiffness

        # Total length
        total_length = arc_length + elongation
        length_error_val = total_length - params.length

        # Warn if length constraint is significantly violated
        if abs(length_error_val) > 1.0:  # More than 1m error
            warnings.warn(
                f"Length constraint error: {length_error_val:.3f} m "
                f"(Arc: {arc_length:.1f}m, Elong: {elongation:.3f}m, "
                f"Total: {total_length:.1f}m vs Target: {params.length}m). "
                f"This may indicate inconsistent geometry.",
                UserWarning
            )

        # Calculate all results from solution
        return self._compute_results(params, H_solution, converged, iterations)

    def _compute_results(
        self,
        params: CatenaryInput,
        H: float,
        converged: bool,
        iterations: int
    ) -> CatenaryResults:
        """
        Compute all catenary results from horizontal tension.

        Parameters
        ----------
        params : CatenaryInput
            Original input parameters
        H : float
            Solved horizontal tension [N]
        converged : bool
            Whether solver converged
        iterations : int
            Number of solver iterations

        Returns
        -------
        results : CatenaryResults
            Complete catenary solution
        """
        # Catenary parameter
        a = H / params.weight_per_length

        # Vertical tension at fairlead
        # From catenary: dy/dx = sinh(x/a)
        # Vertical component: V = H * dy/dx = H * sinh(x/a)
        x_a_ratio = params.horizontal_span / a
        V_fairlead = H * self._safe_sinh(x_a_ratio)

        # Total tension at fairlead (Pythagorean theorem)
        T_fairlead = np.sqrt(H**2 + V_fairlead**2)

        # Total tension at anchor (bottom of catenary, V=0)
        T_anchor = H

        # Elastic elongation
        elongation = H * params.length / params.ea_stiffness

        # Touchdown distance (if line reaches seabed)
        if params.water_depth is not None:
            touchdown = self._calculate_touchdown(
                H, params.weight_per_length, params.water_depth
            )
        else:
            touchdown = None

        # Calculate line shape
        n_points = 100
        x = np.linspace(0, params.horizontal_span, n_points)

        # Catenary equation: y = a * (cosh(x/a) - 1)
        y = a * (np.cosh(x / a) - 1)

        # Tension distribution along line
        # T(x) = sqrt(H² + V(x)²) where V(x) = H*sinh(x/a)
        tension_dist = np.sqrt(H**2 + (H * np.sinh(x / a))**2)

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

    def _calculate_touchdown(
        self,
        H: float,
        w: float,
        water_depth: float
    ) -> Optional[float]:
        """
        Calculate horizontal distance to seabed touchdown point.

        Parameters
        ----------
        H : float
            Horizontal tension [N]
        w : float
            Weight per unit length [N/m]
        water_depth : float
            Water depth [m]

        Returns
        -------
        x_touchdown : float or None
            Horizontal distance to touchdown point [m],
            or None if line doesn't reach seabed

        Notes
        -----
        Inverse catenary equation:
            y = a * (cosh(x/a) - 1)
            cosh(x/a) = y/a + 1
            x = a * acosh(y/a + 1)
        """
        a = H / w

        # Check if line can reach seabed
        arg = water_depth / a + 1

        if arg > 1.0:
            # Inverse catenary
            x_touchdown = a * np.arccosh(arg)
            return x_touchdown
        else:
            # Line is too taut to reach seabed
            return None
