"""
Simplified catenary calculations for quick analysis.

Provides fast closed-form solutions for common catenary problems:
- Force-based catenary (given F, w, d)
- Angle-based catenary (given q, d)
- Catenary forces calculation

The modern solver uses the closed-form catenary with the low point at the
touchdown:
- a = H / w
- y = a * (cosh(x / a) - 1)
- T = H + w * y
"""

from dataclasses import dataclass
from typing import Optional
import math


@dataclass
class SimplifiedCatenaryInput:
    """Input parameters for simplified catenary calculations.

    Use one of three methods:
    1. Angle-based: angle_deg + vertical_distance
    2. Force-based: force + weight_per_length + vertical_distance
    3. Distance-based: horizontal_distance + vertical_distance (not yet implemented)
    """
    # Angle-based method
    angle_deg: Optional[float] = None  # Angle from horizontal (q) [degrees]
    vertical_distance: Optional[float] = None  # Vertical distance (d) [m]

    # Force-based method
    force: Optional[float] = None  # Applied force (F) [N]
    weight_per_length: Optional[float] = None  # Weight per unit length (w) [N/m]

    # Distance-based (not implemented)
    horizontal_distance: Optional[float] = None  # Horizontal distance (X) [m]


@dataclass
class SimplifiedCatenaryResults:
    """Results from simplified catenary calculations."""
    arc_length: float  # Arc length along catenary (S) [m]
    horizontal_distance: float  # Horizontal span (X) [m]
    bend_radius: Optional[float] = None  # Bend radius (R) [m] - for angle-based
    horizontal_tension: Optional[float] = None  # Horizontal tension (TH) [N] - for force-based
    shape_parameter: Optional[float] = None  # Shape parameter (1/a = w/H) [1/m]
    weight_suspended: Optional[float] = None  # Weight of suspended chain (W) [N]


class SimplifiedCatenarySolver:
    """Fast closed-form catenary solutions.

    This solver provides exact mathematical solutions for simplified catenary
    problems where closed-form expressions exist.

    Example:
        >>> solver = SimplifiedCatenarySolver()
        >>> # Angle-based calculation
        >>> result = solver.solve_from_angle(angle_deg=30.0, vertical_distance=100.0)
        >>> print(f"Arc length: {result.arc_length:.2f} m")
        >>>
        >>> # Force-based calculation
        >>> result = solver.solve_from_force(force=10000.0, weight_per_length=50.0,
        ...                                   vertical_distance=100.0)
        >>> print(f"Horizontal distance: {result.horizontal_distance:.2f} m")
    """

    def solve_from_angle(
        self,
        angle_deg: float,
        vertical_distance: float
    ) -> SimplifiedCatenaryResults:
        """Calculate catenary from angle and vertical distance.

        This method solves the catenary equation when the departure angle
        from horizontal (q) and vertical distance (d) are known.

        Mathematical formulation:
        - slope = tan(q)
        - a = d / (sqrt(1 + slope^2) - 1)
        - S = a * slope
        - X = a * asinh(slope)

        Args:
            angle_deg: Angle from horizontal at departure point [degrees]
                      Valid range: 0° < angle_deg < 90°
            vertical_distance: Vertical distance between endpoints [m]
                             Must be positive

        Returns:
            SimplifiedCatenaryResults with arc_length, horizontal_distance, bend_radius

        Raises:
            ValueError: If angle_deg is outside valid range (0, 90)
            ValueError: If vertical_distance is not positive
            ValueError: If calculation results in divide-by-zero

        Example:
            >>> solver = SimplifiedCatenarySolver()
            >>> result = solver.solve_from_angle(30.0, 100.0)
            >>> result.arc_length
            373.205...
        """
        # Input validation
        if not (0 < angle_deg < 90):
            raise ValueError(
                f"angle_deg must be between 0 and 90 degrees, got {angle_deg}"
            )
        if vertical_distance <= 0:
            raise ValueError(
                f"vertical_distance must be positive, got {vertical_distance}"
            )

        angle_rad = math.radians(angle_deg)
        slope = math.tan(angle_rad)
        cosh_at_fairlead = math.sqrt(1.0 + slope**2)

        denominator = cosh_at_fairlead - 1.0
        if abs(denominator) < 1e-12:
            raise ValueError(
                f"Calculation unstable: angle_deg={angle_deg} gives near-zero sag"
            )

        # The bend radius at touchdown is the catenary parameter a.
        bend_radius = vertical_distance / denominator

        # Calculate arc length
        arc_length = bend_radius * slope

        # Calculate horizontal distance
        horizontal_distance = bend_radius * math.asinh(slope)

        return SimplifiedCatenaryResults(
            arc_length=arc_length,
            horizontal_distance=horizontal_distance,
            bend_radius=bend_radius,
            horizontal_tension=None,
            shape_parameter=None,
            weight_suspended=None
        )

    def solve_from_force(
        self,
        force: float,
        weight_per_length: float,
        vertical_distance: float
    ) -> SimplifiedCatenaryResults:
        """Calculate catenary from force, weight per length, and vertical distance.

        This method solves the catenary equation when the total fairlead force (F),
        weight per unit length (w), and vertical distance (d) are known.

        Mathematical formulation:
        - H = F - w*d
        - a = H/w
        - X = a * acosh(1 + d/a)
        - S = a * sinh(X/a)
        - W = w * S
        - b = 1/a = w/H

        Args:
            force: Total fairlead force [N]. Must be greater than w*d so the
                   horizontal tension is positive.
            weight_per_length: Weight per unit length of cable [N/m]
                             Must be positive
            vertical_distance: Vertical distance between endpoints [m]
                             Must be positive

        Returns:
            SimplifiedCatenaryResults with all fields populated

        Raises:
            ValueError: If any input is not positive
            ValueError: If force is too small (F <= w*d)
            ValueError: If calculation results in invalid catenary geometry

        Example:
            >>> solver = SimplifiedCatenarySolver()
            >>> result = solver.solve_from_force(10000.0, 50.0, 100.0)
            >>> result.arc_length
            173.205...
        """
        # Input validation
        if force <= 0:
            raise ValueError(f"force must be positive, got {force}")
        if weight_per_length <= 0:
            raise ValueError(
                f"weight_per_length must be positive, got {weight_per_length}"
            )
        if vertical_distance <= 0:
            raise ValueError(
                f"vertical_distance must be positive, got {vertical_distance}"
            )

        min_force_needed = weight_per_length * vertical_distance
        if force <= min_force_needed:
            raise ValueError(
                f"force too small: {force} N <= {min_force_needed} N (w*d). "
                f"Need force > {min_force_needed:.2f} N for positive horizontal tension."
            )

        horizontal_tension = force - min_force_needed
        catenary_parameter = horizontal_tension / weight_per_length

        x_over_a = math.acosh(1.0 + vertical_distance / catenary_parameter)
        horizontal_distance = catenary_parameter * x_over_a
        arc_length = catenary_parameter * math.sinh(x_over_a)

        # Calculate weight of suspended chain (W)
        weight_suspended = weight_per_length * arc_length

        # Shape parameter is inverse catenary parameter.
        shape_parameter = weight_per_length / horizontal_tension

        return SimplifiedCatenaryResults(
            arc_length=arc_length,
            horizontal_distance=horizontal_distance,
            bend_radius=None,
            horizontal_tension=horizontal_tension,
            shape_parameter=shape_parameter,
            weight_suspended=weight_suspended
        )

    def calculate_forces(
        self,
        weight_per_length: float,
        arc_length: float,
        angle_deg: float
    ) -> tuple[float, float, float]:
        """Calculate forces on catenary from geometry.

        This method calculates vertical, total, and horizontal force
        components from suspended length and fairlead angle.

        Mathematical formulation:
        - Fv = w * S (vertical force)
        - F = Fv / sin(q) (total force)
        - Fh = F * cos(q) (horizontal force)

        Args:
            weight_per_length: Weight per unit length [N/m]
            arc_length: Arc length of catenary [m]
            angle_deg: Angle from horizontal at departure [degrees]

        Returns:
            Tuple of (Fv, F, Fh) - vertical, total, horizontal forces [N]

        Raises:
            ValueError: If inputs are invalid or angle causes divide-by-zero

        Example:
            >>> solver = SimplifiedCatenarySolver()
            >>> Fv, F, Fh = solver.calculate_forces(50.0, 100.0, 30.0)
            >>> Fv
            5000.0
        """
        # Input validation
        if weight_per_length <= 0:
            raise ValueError(
                f"weight_per_length must be positive, got {weight_per_length}"
            )
        if arc_length <= 0:
            raise ValueError(f"arc_length must be positive, got {arc_length}")
        if not (0 < angle_deg < 90):
            raise ValueError(
                f"angle_deg must be between 0 and 90, got {angle_deg}"
            )

        angle_rad = math.radians(angle_deg)

        sin_angle = math.sin(angle_rad)
        cos_angle = math.cos(angle_rad)

        # Check for divide-by-zero
        if abs(sin_angle) < 1e-12:
            raise ValueError(
                f"Calculation unstable: angle_deg={angle_deg} results in sin≈0"
            )

        # Calculate vertical load on vessel
        Fv = weight_per_length * arc_length

        # Calculate total force along catenary
        F = Fv / sin_angle

        # Calculate horizontal force along catenary
        Fh = F * cos_angle

        return (Fv, F, Fh)


def solve_catenary_dict(data: dict) -> dict:
    """Dictionary wrapper that accepts and returns catenary data.

    This function accepts the historic dictionary shape but uses the modern
    closed-form simplified solver semantics.

    Args:
        data: Dictionary with keys:
              - For force-based: 'F', 'w', 'd'
              - For angle-based: 'q', 'd'
              - For distance-based: 'X', 'd' (raises NotImplementedError)

    Returns:
        Updated dictionary with calculated values added

    Raises:
        NotImplementedError: If X-based calculation requested
        ValueError: If inputs are invalid

    Example:
        >>> data = {'F': 10000.0, 'w': 50.0, 'd': 100.0, 'q': None, 'X': None}
        >>> result = solve_catenary_dict(data)
        >>> result['S']
        173.205...
    """
    solver = SimplifiedCatenarySolver()

    # Determine which method to use.
    if data.get("F") is not None:
        # Force-based method
        result = solver.solve_from_force(
            force=data["F"],
            weight_per_length=data["w"],
            vertical_distance=data["d"]
        )
        data.update({
            "S": result.arc_length,
            "X": result.horizontal_distance,
            "W": result.weight_suspended,
            "THorizontal": result.horizontal_tension,
            "b": result.shape_parameter
        })
        return data

    elif data.get("X") is not None:
        # Distance-based (not implemented)
        raise NotImplementedError("X-based calculation not implemented yet")

    elif data.get("q") is not None:
        # Angle-based method
        result = solver.solve_from_angle(
            angle_deg=data["q"],
            vertical_distance=data["d"]
        )
        data.update({
            "S": result.arc_length,
            "X": result.horizontal_distance,
            "BendRadius": result.bend_radius
        })
        return data

    else:
        raise ValueError(
            "Invalid input: must provide either F, X, or q with d"
        )


def calculate_forces_dict(data: dict) -> dict:
    """Dictionary wrapper for force calculation.

    Args:
        data: Dictionary with keys:
              - 'weightPerUnitLength': Weight per unit length [N/m]
              - 'S': Arc length [m]
              - 'q': Angle from horizontal [degrees]

    Returns:
        Updated dictionary with Fv, F, Fh added

    Example:
        >>> data = {'weightPerUnitLength': 50.0, 'S': 100.0, 'q': 30.0}
        >>> result = calculate_forces_dict(data)
        >>> result['Fv']
        5000.0
    """
    solver = SimplifiedCatenarySolver()

    Fv, F, Fh = solver.calculate_forces(
        weight_per_length=data["weightPerUnitLength"],
        arc_length=data["S"],
        angle_deg=data["q"]
    )

    data.update({"Fv": Fv, "F": F, "Fh": Fh})
    return data


# Export public API
__all__ = [
    'SimplifiedCatenaryInput',
    'SimplifiedCatenaryResults',
    'SimplifiedCatenarySolver',
    'solve_catenary_dict',
    'calculate_forces_dict'
]
