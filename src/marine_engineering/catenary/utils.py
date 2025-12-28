"""
Utility functions for catenary calculations.

Provides safe hyperbolic functions, validation utilities,
and common conversions for catenary analysis.
"""

import numpy as np
from typing import Union, Tuple


def safe_sinh(x: Union[float, np.ndarray], max_arg: float = 700.0) -> Union[float, np.ndarray]:
    """
    Safely compute sinh(x) avoiding overflow.

    For large arguments where sinh would overflow, returns the maximum
    representable float value with appropriate sign.

    Parameters
    ----------
    x : float or np.ndarray
        Input value(s)
    max_arg : float, optional
        Maximum argument before returning max float (default: 700.0)

    Returns
    -------
    result : float or np.ndarray
        sinh(x) or max float for overflow cases
    """
    # Maximum safe exponent before float64 overflow
    overflow_threshold = 709.0
    max_float = np.finfo(np.float64).max

    if isinstance(x, np.ndarray):
        result = np.zeros_like(x, dtype=np.float64)
        mask_small = np.abs(x) <= max_arg
        mask_large_pos = x > max_arg
        mask_large_neg = x < -max_arg

        result[mask_small] = np.sinh(x[mask_small])
        # For very large x, return max float instead of computing exp(x)
        result[mask_large_pos] = max_float
        result[mask_large_neg] = -max_float

        return result
    else:
        if abs(x) <= max_arg:
            return np.sinh(x)
        elif x > 0:
            return max_float
        else:
            return -max_float


def safe_cosh(x: Union[float, np.ndarray], max_arg: float = 700.0) -> Union[float, np.ndarray]:
    """
    Safely compute cosh(x) avoiding overflow.

    For large arguments where cosh would overflow, returns the maximum
    representable float value.

    Parameters
    ----------
    x : float or np.ndarray
        Input value(s)
    max_arg : float, optional
        Maximum argument before returning max float (default: 700.0)

    Returns
    -------
    result : float or np.ndarray
        cosh(x) or max float for overflow cases
    """
    # Maximum safe exponent before float64 overflow
    max_float = np.finfo(np.float64).max

    if isinstance(x, np.ndarray):
        result = np.zeros_like(x, dtype=np.float64)
        mask_small = np.abs(x) <= max_arg
        mask_large = np.abs(x) > max_arg

        result[mask_small] = np.cosh(x[mask_small])
        # For very large |x|, return max float instead of computing exp(|x|)
        result[mask_large] = max_float

        return result
    else:
        if abs(x) <= max_arg:
            return np.cosh(x)
        else:
            return max_float


def validate_catenary_inputs(
    length: float,
    horizontal_span: float,
    vertical_span: float,
    weight_per_length: float,
    ea_stiffness: float
) -> Tuple[bool, str]:
    """
    Validate catenary input parameters.

    Parameters
    ----------
    length : float
        Line length [m]
    horizontal_span : float
        Horizontal distance [m]
    vertical_span : float
        Vertical distance [m]
    weight_per_length : float
        Weight per unit length [N/m]
    ea_stiffness : float
        Axial stiffness [N]

    Returns
    -------
    valid : bool
        True if all inputs are valid
    message : str
        Error message if invalid, empty string if valid
    """
    if length <= 0:
        return False, "Length must be positive"

    if horizontal_span <= 0:
        return False, "Horizontal span must be positive"

    if weight_per_length <= 0:
        return False, "Weight per length must be positive"

    if ea_stiffness <= 0:
        return False, "EA stiffness must be positive"

    # Check if line is long enough to span the distance
    straight_dist = np.sqrt(horizontal_span**2 + vertical_span**2)
    if length < straight_dist:
        return False, f"Length {length:.2f}m is less than straight distance {straight_dist:.2f}m"

    return True, ""


def catenary_parameter(horizontal_tension: float, weight_per_length: float) -> float:
    """
    Calculate catenary parameter 'a' = H/w.

    Parameters
    ----------
    horizontal_tension : float
        Horizontal tension component [N]
    weight_per_length : float
        Weight per unit length [N/m]

    Returns
    -------
    a : float
        Catenary parameter [m]
    """
    if weight_per_length <= 0:
        raise ValueError("Weight per length must be positive")
    if horizontal_tension <= 0:
        raise ValueError("Horizontal tension must be positive")

    return horizontal_tension / weight_per_length


def estimate_initial_tension(
    length: float,
    horizontal_span: float,
    weight_per_length: float
) -> float:
    """
    Estimate initial horizontal tension for solver initialization.

    Uses parabolic approximation: a ≈ X²/(8*sag)
    where sag ≈ (L - X) for small angles.

    Parameters
    ----------
    length : float
        Line length [m]
    horizontal_span : float
        Horizontal distance [m]
    weight_per_length : float
        Weight per unit length [N/m]

    Returns
    -------
    H_initial : float
        Estimated horizontal tension [N]
    """
    sag_estimate = max(length - horizontal_span, 10.0)
    a_estimate = horizontal_span**2 / (8.0 * sag_estimate)
    H_estimate = weight_per_length * a_estimate

    return max(H_estimate, 100.0)  # Ensure minimum value


def calculate_elongation(
    tension: float,
    length: float,
    ea_stiffness: float
) -> float:
    """
    Calculate elastic elongation of line.

    Parameters
    ----------
    tension : float
        Applied tension [N]
    length : float
        Original length [m]
    ea_stiffness : float
        Axial stiffness EA [N]

    Returns
    -------
    elongation : float
        Elongation [m]
    """
    if ea_stiffness <= 0:
        raise ValueError("EA stiffness must be positive")

    return tension * length / ea_stiffness


def touchdown_point(
    catenary_param: float,
    water_depth: float
) -> Union[float, None]:
    """
    Calculate horizontal distance to touchdown point.

    For catenary with low point at anchor, finds x where y = water_depth.

    Parameters
    ----------
    catenary_param : float
        Catenary parameter a = H/w [m]
    water_depth : float
        Water depth [m]

    Returns
    -------
    x_touchdown : float or None
        Horizontal distance to touchdown, or None if no touchdown
    """
    if water_depth <= 0:
        return None

    arg = water_depth / catenary_param + 1.0

    if arg > 1.0:
        return catenary_param * np.arccosh(arg)
    else:
        return None
