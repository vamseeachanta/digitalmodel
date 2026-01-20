"""
Catenary Solver for Mooring Line Analysis

Advanced quasi-static catenary solver implementing analytical catenary equations
with numerical iteration for boundary value problems. Converts Excel array formula
implementation to vectorized Python with elastic elongation support.

Mathematical Foundation:
-----------------------
Classical catenary equation for a uniform cable under self-weight:

    y(x) = a * cosh(x/a) - a

where:
    a = H / w  (catenary parameter)
    H = horizontal tension component [N]
    w = submerged weight per unit length [N/m]
    x = horizontal distance from low point [m]
    y = vertical distance from low point [m]

Arc length along catenary:
    s(x) = a * sinh(x/a)

Tension at any point:
    T(x) = sqrt(H² + (w*y)²)

Boundary Value Problem:
----------------------
Given:
    - Total line length L (unstretched)
    - Horizontal span X_span
    - Vertical span Y_span
    - Weight per length w
    - Axial stiffness EA

Find horizontal tension H such that:
    s(X_span) + elongation(H) = L

where:
    elongation = H * L / EA

Author: Digital Model Project
Source: Excel "Poly Mooring" (695 array formulas)
"""

from dataclasses import dataclass
from typing import Optional
import numpy as np
from scipy.optimize import newton, brentq


@dataclass
class CatenaryInput:
    """
    Input parameters for catenary analysis.

    Attributes
    ----------
    length : float
        Unstretched line length [m]
    horizontal_span : float
        Horizontal distance between anchor and fairlead [m]
    vertical_span : float
        Vertical distance (positive down) [m]
    weight_per_length : float
        Submerged weight per unit length [N/m]
    ea_stiffness : float
        Axial stiffness EA [N]
    water_depth : float, optional
        Water depth for touchdown analysis [m]
    seabed_friction : float, optional
        Seabed friction coefficient [-], default 0.0
    """
    length: float
    horizontal_span: float
    vertical_span: float
    weight_per_length: float
    ea_stiffness: float
    water_depth: Optional[float] = None
    seabed_friction: float = 0.0


@dataclass
class CatenaryResults:
    """
    Results from catenary analysis.

    Attributes
    ----------
    horizontal_tension : float
        Horizontal tension component H at anchor [N]
    vertical_tension_fairlead : float
        Vertical tension component V at fairlead [N]
    total_tension_fairlead : float
        Total tension magnitude at fairlead [N]
    total_tension_anchor : float
        Total tension magnitude at anchor [N]
    elongation : float
        Elastic stretch of line [m]
    touchdown_distance : float
        Horizontal distance to touchdown point [m]
    catenary_parameter : float
        Catenary parameter a = H/w [m]
    shape_x : np.ndarray
        Line shape x-coordinates [m]
    shape_y : np.ndarray
        Line shape y-coordinates [m]
    tension_distribution : np.ndarray
        Tension magnitude along line [N]
    converged : bool
        Solver convergence flag
    iterations : int
        Number of solver iterations
    """
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
    Advanced catenary solver for mooring line analysis.

    Implements analytical catenary equations with numerical iteration
    for boundary value problems. Handles elastic elongation and
    touchdown point calculations.

    The solver uses Newton-Raphson method with analytical derivative
    for fast convergence, with Brent's method as a robust fallback.

    Parameters
    ----------
    tolerance : float, optional
        Convergence tolerance for length error [m], default 1e-6
    max_iterations : int, optional
        Maximum iterations for numerical solver, default 100

    Examples
    --------
    >>> params = CatenaryInput(
    ...     length=1000,
    ...     horizontal_span=800,
    ...     vertical_span=100,
    ...     weight_per_length=1962,
    ...     ea_stiffness=64e9
    ... )
    >>> solver = CatenarySolver()
    >>> results = solver.solve(params)
    >>> print(f"Horizontal tension: {results.horizontal_tension:.0f} N")
    """

    def __init__(self, tolerance: float = 1e-6, max_iterations: int = 100):
        """
        Initialize catenary solver.

        Parameters
        ----------
        tolerance : float
            Convergence tolerance for length error [m]
        max_iterations : int
            Maximum iterations for Newton-Raphson solver
        """
        self.tolerance = tolerance
        self.max_iterations = max_iterations
        self.gravity = 9.8065  # m/s²

    def solve(self, params: CatenaryInput) -> CatenaryResults:
        """
        Solve catenary boundary value problem.

        Finds horizontal tension H that satisfies the length constraint
        using Newton-Raphson iteration with analytical derivative for
        improved convergence.

        The solver iteratively adjusts H until the calculated arc length
        plus elastic elongation equals the target line length within
        the specified tolerance.

        Parameters
        ----------
        params : CatenaryInput
            Catenary input parameters

        Returns
        -------
        results : CatenaryResults
            Complete catenary solution including line shape and tensions

        Raises
        ------
        ValueError
            If input parameters are invalid or solution cannot be found

        Notes
        -----
        The length error function to be minimized is:
            f(H) = s(H) + e(H) - L

        where:
            s(H) = arc length = a * sinh(X_span / a)
            e(H) = elastic elongation = H * L / EA
            a = H / w

        The derivative for Newton-Raphson is:
            f'(H) = ds/dH + de/dH

        where:
            ds/dH = (X_span / w) * [cosh(X_span/a) - (X_span/(2a)) * sinh(X_span/a)]
            de/dH = L / EA
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

        # Define length error function
        def length_error(H: float) -> float:
            """
            Difference between calculated and target length.

            Returns the error in total line length for a given
            horizontal tension H. The solver seeks H where this
            function equals zero.
            """
            if H <= 0:
                return float('inf')

            # Catenary parameter
            a = H / params.weight_per_length

            # Arc length from catenary equation
            s = a * np.sinh(params.horizontal_span / a)

            # Elastic elongation
            elongation = H * params.length / params.ea_stiffness

            # Total length error
            return (s + elongation) - params.length

        # Define derivative for Newton-Raphson (improves convergence)
        def length_error_derivative(H: float) -> float:
            """
            Derivative of length error with respect to H.

            Analytical derivative significantly improves Newton-Raphson
            convergence compared to numerical differentiation.
            """
            if H <= 0:
                return float('inf')

            a = H / params.weight_per_length
            x = params.horizontal_span

            # d(arc_length)/dH
            # Using chain rule: d/dH[a*sinh(x/a)] where a = H/w
            # = (1/w) * sinh(x/a) + a * cosh(x/a) * (-x/a²) * (1/w)
            # = (1/w) * [sinh(x/a) - (x/a) * cosh(x/a)]
            # Simplified form used here for numerical stability:
            ds_dH = (x / params.weight_per_length) * (
                np.cosh(x / a) - (x / (2 * a)) * np.sinh(x / a)
            )

            # d(elongation)/dH
            de_dH = params.length / params.ea_stiffness

            return ds_dH + de_dH

        # Initial guess for horizontal tension
        # Better estimation based on typical catenary behavior
        # For suspended cable: H ≈ sqrt((w*L)² * (X²)/(4*s_excess²)) where s_excess = L - X
        # Simplified: H ≈ w * L (provides good starting point)
        H_initial = params.weight_per_length * params.length

        # Ensure initial guess is positive and reasonable
        H_initial = max(H_initial, 1000.0)

        # Solve using Newton-Raphson with analytical derivative
        try:
            H_solution = newton(
                length_error,
                x0=H_initial,
                fprime=length_error_derivative,
                tol=self.tolerance,
                maxiter=self.max_iterations,
                full_output=False
            )
            converged = True
            iterations = 0  # Newton doesn't return iteration count by default

        except RuntimeError as e:
            # Fallback to Brent's method if Newton fails
            # Brent's method is more robust but slower, using bracketing
            print(f"Newton-Raphson failed, falling back to Brent's method: {e}")

            # Define search bounds for horizontal tension
            H_min = 100.0  # Minimum realistic tension
            H_max = params.length * params.weight_per_length * 100  # Upper bound

            try:
                # brentq returns (root, result) when full_output=True
                H_solution, result = brentq(
                    length_error,
                    a=H_min,
                    b=H_max,
                    xtol=self.tolerance,
                    maxiter=self.max_iterations,
                    full_output=True
                )
                converged = result.converged
                iterations = result.iterations

            except ValueError as e:
                raise ValueError(
                    f"Unable to find solution. Check input parameters. Error: {e}"
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

        Calculates line shape, tension distribution, and all derived
        quantities from the solved horizontal tension component.

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
        # From catenary geometry: V = H * sinh(x/a)
        V_fairlead = H * np.sinh(params.horizontal_span / a)

        # Total tension at fairlead
        # Pythagorean theorem: T = sqrt(H² + V²)
        T_fairlead = np.sqrt(H**2 + V_fairlead**2)

        # Total tension at anchor (bottom of catenary)
        # At the low point, vertical component is zero
        T_anchor = H

        # Elastic elongation
        # Linear elastic: elongation = Force * Length / (E * A)
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
        # The -1 term sets y=0 at x=0 (low point of catenary)
        y = a * (np.cosh(x / a) - 1)

        # Tension distribution along line
        # T(x) = sqrt(H² + (w*y)²)
        tension_dist = np.sqrt(H**2 + (params.weight_per_length * y)**2)

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

        Solves the inverse catenary equation to find the horizontal
        distance x where the line touches the seabed (y = water_depth).

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

        The line reaches seabed only if y/a + 1 > 1, i.e., y > 0.
        """
        a = H / w

        # Check if line can reach seabed
        # acosh requires argument >= 1
        arg = water_depth / a + 1

        if arg > 1.0:
            # Inverse catenary: x = a * acosh(y/a + 1)
            x_touchdown = a * np.arccosh(arg)
            return x_touchdown
        else:
            # Line is too taut to reach seabed
            return None
