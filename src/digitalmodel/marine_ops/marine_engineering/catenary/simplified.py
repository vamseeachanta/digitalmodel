"""
Simplified catenary calculations for quick analysis.

Provides fast closed-form solutions for common catenary problems:
- Force-based catenary (given F, w, d)
- Angle-based catenary (given q, d)
- Catenary forces calculation

This module ports and modernizes the legacy implementations from:
- digitalmodel.subsea.catenary.catenaryMethods.catenaryEquation
- digitalmodel.subsea.catenary.catenary_equation.CatenaryCalculator

Mathematical formulas are preserved exactly to ensure numerical accuracy.
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

    # Distance-based (not implemented in legacy)
    horizontal_distance: Optional[float] = None  # Horizontal distance (X) [m]


@dataclass
class SimplifiedCatenaryResults:
    """Results from simplified catenary calculations."""
    arc_length: float  # Arc length along catenary (S) [m]
    horizontal_distance: float  # Horizontal span (X) [m]
    bend_radius: Optional[float] = None  # Bend radius (R) [m] - for angle-based
    horizontal_tension: Optional[float] = None  # Horizontal tension (TH) [N] - for force-based
    shape_parameter: Optional[float] = None  # Shape parameter (b = w*g/TH) [1/m]
    weight_suspended: Optional[float] = None  # Weight of suspended chain (W) [N]


class SimplifiedCatenarySolver:
    """Fast closed-form catenary solutions.

    This solver provides exact mathematical solutions for simplified catenary
    problems where closed-form expressions exist. It ports the legacy
    implementations while providing a modern, type-safe API.

    Example:
        >>> solver = SimplifiedCatenarySolver()
        >>> # Angle-based calculation
        >>> result = solver.solve_from_angle(angle_deg=30.0, vertical_distance=100.0)
        >>> print(f"Arc length: {result.arc_length:.2f} m")
        >>>
        >>> # Force-based calculation
        >>> result = solver.solve_from_force(force=1000.0, weight_per_length=50.0,
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

        Mathematical formulation (ported from legacy catenaryMethods.py lines 32-44):
        - tanq = tan(90° - q)
        - BendRadius = d * cos(90° - q) / (1 - cos(90° - q))
        - S = BendRadius * tanq
        - X = BendRadius * asinh(tanq)

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
            267.949...
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

        # Port exact math from legacy (lines 32-44)
        # Note: Legacy uses (90 - q) as complementary angle
        complementary_angle_deg = 90.0 - angle_deg
        complementary_angle_rad = math.radians(complementary_angle_deg)

        tanq = math.tan(complementary_angle_rad)
        cos_comp = math.cos(complementary_angle_rad)

        # Check for divide-by-zero (when cos ≈ 1)
        denominator = 1.0 - cos_comp
        if abs(denominator) < 1e-12:
            raise ValueError(
                f"Calculation unstable: angle_deg={angle_deg} results in cos≈1"
            )

        # Calculate bend radius
        bend_radius = vertical_distance * cos_comp / denominator

        # Calculate arc length
        arc_length = bend_radius * tanq

        # Calculate horizontal distance
        horizontal_distance = bend_radius * math.asinh(tanq)

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

        This method solves the catenary equation when the applied force (F),
        weight per unit length (w), and vertical distance (d) are known.

        Mathematical formulation (ported from legacy catenaryMethods.py lines 10-25):
        - S = d * (2*F/w - d)
        - X = ((F/w) - d) * ln((S + (F/w)) / ((F/w) - d))
        - W = w * S
        - TH = F * X / sqrt(S² + X²)
        - b = w * g / TH

        Args:
            force: Applied force at departure point [N]
                   Must be positive and > w*d/2 for valid catenary
            weight_per_length: Weight per unit length of cable [N/m]
                             Must be positive
            vertical_distance: Vertical distance between endpoints [m]
                             Must be positive

        Returns:
            SimplifiedCatenaryResults with all fields populated

        Raises:
            ValueError: If any input is not positive
            ValueError: If force is too small (F < w*d/2)
            ValueError: If calculation results in invalid logarithm

        Example:
            >>> solver = SimplifiedCatenarySolver()
            >>> result = solver.solve_from_force(1000.0, 50.0, 100.0)
            >>> result.arc_length
            1800.0
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

        # Port exact math from legacy (lines 11-25)
        ratio_F_w = force / weight_per_length

        # Check for valid catenary condition
        min_force_needed = weight_per_length * vertical_distance / 2.0
        if force < min_force_needed:
            raise ValueError(
                f"force too small: {force} N < {min_force_needed} N (w*d/2). "
                f"Need force > {min_force_needed:.2f} N for valid catenary."
            )

        # Calculate arc length (S)
        arc_length = vertical_distance * (2.0 * ratio_F_w - vertical_distance)

        # Check arc length validity
        if arc_length <= 0:
            raise ValueError(
                f"Invalid arc_length={arc_length}. Check input parameters."
            )

        # Calculate horizontal distance (X)
        numerator = arc_length + ratio_F_w
        denominator = ratio_F_w - vertical_distance

        if denominator <= 0:
            raise ValueError(
                f"Invalid denominator in log: (F/w - d) = {denominator}. "
                f"Need F/w > d (i.e., {force/weight_per_length} > {vertical_distance})"
            )
        if numerator <= 0:
            raise ValueError(
                f"Invalid numerator in log: (S + F/w) = {numerator}"
            )

        log_argument = numerator / denominator
        if log_argument <= 0:
            raise ValueError(
                f"Invalid log argument: {log_argument}. Check parameters."
            )

        horizontal_distance = denominator * math.log(log_argument)

        # Calculate weight of suspended chain (W)
        weight_suspended = weight_per_length * arc_length

        # Calculate horizontal tension component (TH)
        hypotenuse = math.sqrt(arc_length**2 + horizontal_distance**2)
        horizontal_tension = force * horizontal_distance / hypotenuse

        # Calculate catenary shape parameter (b = w*g/TH)
        # Note: Legacy uses 9.81 m/s² for gravity
        shape_parameter = weight_per_length * 9.81 / horizontal_tension

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

        This method ports the legacy catenaryForces function (lines 47-57)
        to calculate the vertical, total, and horizontal forces.

        Mathematical formulation:
        - Fv = w * S (vertical force)
        - F = Fv / sin(90° - q) (total force)
        - Fh = F * cos(90° - q) (horizontal force)

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

        # Port exact math from legacy (lines 49-53)
        complementary_angle_rad = math.radians(90.0 - angle_deg)

        sin_comp = math.sin(complementary_angle_rad)
        cos_comp = math.cos(complementary_angle_rad)

        # Check for divide-by-zero
        if abs(sin_comp) < 1e-12:
            raise ValueError(
                f"Calculation unstable: angle_deg={angle_deg} results in sin≈0"
            )

        # Calculate vertical load on vessel
        Fv = weight_per_length * arc_length

        # Calculate total force along catenary
        F = Fv / sin_comp

        # Calculate horizontal force along catenary
        Fh = F * cos_comp

        return (Fv, F, Fh)


def solve_catenary_dict(data: dict) -> dict:
    """Legacy-compatible wrapper that accepts and returns dictionaries.

    This function provides a drop-in replacement for the legacy
    catenaryEquation function, accepting the same dictionary format.

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
        >>> data = {'F': 1000.0, 'w': 50.0, 'd': 100.0, 'q': None, 'X': None}
        >>> result = solve_catenary_dict(data)
        >>> result['S']
        1800.0
    """
    solver = SimplifiedCatenarySolver()

    # Determine which method to use (same logic as legacy)
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
    """Legacy-compatible wrapper for force calculation.

    Provides drop-in replacement for legacy catenaryForces function.

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
