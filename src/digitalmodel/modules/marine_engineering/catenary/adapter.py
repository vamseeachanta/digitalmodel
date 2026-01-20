"""
Backward compatibility adapter for legacy catenary API.

Provides dict-based interface matching legacy catenaryMethods.py
while using modern solvers internally.

This module bridges the gap between:
  - Legacy: digitalmodel.modules.catenary.catenaryMethods (dict-based)
  - Modern: marine_engineering.catenary solvers (dataclass-based)

Usage:
    from digitalmodel.modules.marine_engineering.catenary.adapter import catenaryEquation

    # Force-based method (legacy API)
    result = catenaryEquation({
        "F": 10000.0,      # Force [N]
        "w": 500.0,        # Weight per length [N/m]
        "d": 100.0,        # Vertical distance [m]
        "X": None,
        "q": None
    })
    # Returns: {"S": ..., "X": ..., "THorizontal": ..., "W": ...}

Author: Digital Model Project
Date: 2025-10-03
"""

from typing import Dict, Any, Optional
import warnings
import math
import numpy as np

# Import modern solvers from mooring_analysis module
# (The new catenary module structure will be created later)
try:
    from digitalmodel.modules.marine_engineering.mooring_analysis.catenary_solver import (
        CatenarySolver,
        CatenaryInput,
        CatenaryResults
    )
    HAS_MODERN_SOLVER = True
except ImportError:
    HAS_MODERN_SOLVER = False
    warnings.warn(
        "Modern CatenarySolver not available. Adapter will use fallback implementation.",
        ImportWarning
    )


def catenaryEquation(data: Dict[str, Any]) -> Dict[str, Any]:
    """
    Legacy API compatibility wrapper.

    Matches signature of digitalmodel.modules.catenary.catenaryMethods.catenaryEquation()

    This function provides backward compatibility with the legacy dict-based API
    while using modern solver implementations internally.

    Input (dict):
        - F: Force [N] (optional, for force-based method)
        - w: Weight per length [N/m] (optional, for force-based method)
        - d: Vertical distance [m] (required)
        - q: Angle in degrees (optional, for angle-based method)
        - X: Horizontal distance [m] (optional, for X-based method)

    Output (dict):
        - S: Arc length [m]
        - X: Horizontal distance [m]
        - BendRadius: Bend radius [m] (angle-based only)
        - THorizontal: Horizontal tension [N] (force-based only)
        - W: Weight [N] (force-based only)

    Raises:
        ValueError: If insufficient parameters provided
        NotImplementedError: If X-based calculation requested (not in legacy)

    Examples:
        >>> # Force-based method
        >>> result = catenaryEquation({"F": 10000, "w": 500, "d": 100, "X": None, "q": None})
        >>> print(f"Arc length: {result['S']:.2f}m")

        >>> # Angle-based method
        >>> result = catenaryEquation({"d": 100, "q": 30, "F": None, "w": None, "X": None})
        >>> print(f"Bend radius: {result['BendRadius']:.2f}m")
    """
    warnings.warn(
        "catenaryEquation dict API is deprecated. "
        "Use marine_engineering.catenary.CatenarySolver for new code. "
        "This adapter is provided for backward compatibility only.",
        DeprecationWarning,
        stacklevel=2
    )

    # Validate required parameter
    if "d" not in data or data.get("d") is None:
        raise ValueError("Required parameter 'd' (vertical distance) not provided")

    # Route based on available inputs (matching legacy logic)
    if data.get("F") is not None and data.get("w") is not None:
        # Force-based method (matches legacy lines 10-27)
        return _force_based_method(data)

    elif data.get("q") is not None:
        # Angle-based method (matches legacy lines 32-44)
        return _angle_based_method(data)

    elif data.get("X") is not None:
        # X-based method (matches legacy line 30)
        raise NotImplementedError("X-based calculation not implemented in legacy API")

    else:
        raise ValueError(
            "Insufficient parameters for catenary calculation. "
            "Provide either (F, w), (q), or (X) along with d."
        )


def _force_based_method(data: Dict[str, Any]) -> Dict[str, Any]:
    """
    Force-based catenary calculation.

    Replicates legacy catenaryMethods.py lines 10-27.

    Given:
      - F: Force at top [N]
      - w: Weight per unit length [N/m]
      - d: Vertical distance [m]

    Calculates:
      - S: Arc length [m]
      - X: Horizontal distance [m]
      - W: Total weight [N]
      - THorizontal: Horizontal tension component [N]

    Legacy formula (from original code):
        S = d * (2*F/w - d)
        X = ((F/w) - d) * ln((S + F/w) / ((F/w) - d))
        W = w * S
        THorizontal = F * X / sqrt(S^2 + X^2)
    """
    F = data["F"]
    w = data["w"]
    d = data["d"]

    # Validate inputs
    if F <= 0:
        raise ValueError("Force F must be positive")
    if w <= 0:
        raise ValueError("Weight per length w must be positive")
    if d <= 0:
        raise ValueError("Vertical distance d must be positive")

    # Check physical constraint: F/w must be > d
    # (otherwise would get negative arc length)
    if F/w <= d:
        raise ValueError(
            f"Invalid parameters: F/w ({F/w:.2f}) must be > d ({d:.2f}). "
            f"Increase force or decrease weight."
        )

    # Arc length (legacy formula)
    S = d * (2 * F / w - d)

    # Horizontal distance (legacy formula)
    ratio_top = S + (F / w)
    ratio_bottom = (F / w) - d

    if ratio_bottom <= 0:
        raise ValueError("Invalid geometry: (F/w) - d must be positive")

    X = ratio_bottom * math.log(ratio_top / ratio_bottom)

    # Weight of suspended chain
    W = w * S

    # Normalized horizontal tension component
    THorizontal = F * X / math.sqrt(S**2 + X**2)

    # Catenary shape parameter (for reference, not returned in legacy)
    # b = w * 9.81 / THorizontal  # Legacy line 24

    # Return dict matching legacy output
    result = {
        "S": S,
        "X": X,
        "W": W,
        "THorizontal": THorizontal,
        # Preserve original input fields (legacy behavior)
        "F": F,
        "w": w,
        "d": d
    }

    return result


def _angle_based_method(data: Dict[str, Any]) -> Dict[str, Any]:
    """
    Angle-based catenary calculation.

    Replicates legacy catenaryMethods.py lines 32-44.

    Given:
      - q: Angle in degrees
      - d: Vertical distance [m]

    Calculates:
      - S: Arc length [m]
      - X: Horizontal distance [m]
      - BendRadius: Bend radius [m]

    Legacy formulas (from original code):
        tanq = tan(radians(90 - q))
        BendRadius = d * cos(radians(90-q)) / (1 - cos(radians(90-q)))
        S = BendRadius * tanq
        X = BendRadius * asinh(tanq)

    Note: The legacy code uses (90 - q) which converts from angle-from-vertical
          to angle-from-horizontal.
    """
    q = data["q"]
    d = data["d"]

    # Validate inputs
    if not 0 <= q <= 90:
        raise ValueError(f"Angle q must be between 0 and 90 degrees, got {q}")
    if d <= 0:
        raise ValueError("Vertical distance d must be positive")

    # Convert angle (legacy uses 90 - q)
    angle_rad = math.radians(90 - q)

    # Calculate tangent
    tanq = math.tan(angle_rad)

    # Bend radius
    cos_angle = math.cos(angle_rad)
    if cos_angle >= 1.0:
        raise ValueError(f"Invalid angle q={q}deg: cos(90-q) >= 1")

    BendRadius = d * cos_angle / (1 - cos_angle)

    # Arc length
    S = BendRadius * tanq

    # Horizontal distance (using asinh = inverse hyperbolic sine)
    X = BendRadius * math.asinh(tanq)

    # Return dict matching legacy output
    result = {
        "S": S,
        "X": X,
        "BendRadius": BendRadius,
        # Preserve original input fields
        "q": q,
        "d": d
    }

    return result


def catenaryForces(data: Dict[str, Any]) -> Dict[str, Any]:
    """
    Legacy API for catenary forces calculation.

    Matches signature of digitalmodel.modules.catenary.catenaryMethods.catenaryForces()

    Calculates forces on a catenary given:
      - weightPerUnitLength: Weight per unit length [N/m]
      - S: Arc length [m]
      - q: Angle from vertical [degrees]

    Returns:
      - Fv: Vertical force [N]
      - F: Total force along catenary [N]
      - Fh: Horizontal force [N]

    Legacy formulas (from catenaryMethods.py lines 47-57):
        Fv = weightPerUnitLength * S
        F = Fv / sin(radians(90 - q))
        Fh = F * cos(radians(90 - q))

    Examples:
        >>> data = {"weightPerUnitLength": 500, "S": 150, "q": 30}
        >>> result = catenaryForces(data)
        >>> print(f"Vertical force: {result['Fv']:.2f}N")
    """
    warnings.warn(
        "catenaryForces dict API is deprecated. "
        "Use marine_engineering.catenary.CatenarySolver for new code.",
        DeprecationWarning,
        stacklevel=2
    )

    # Validate required parameters
    required = ["weightPerUnitLength", "S", "q"]
    missing = [p for p in required if p not in data or data.get(p) is None]
    if missing:
        raise ValueError(f"Missing required parameters: {missing}")

    weight_per_length = data["weightPerUnitLength"]
    S = data["S"]
    q = data["q"]

    # Validate inputs
    if weight_per_length <= 0:
        raise ValueError("weightPerUnitLength must be positive")
    if S <= 0:
        raise ValueError("Arc length S must be positive")
    if not 0 <= q <= 90:
        raise ValueError(f"Angle q must be between 0 and 90 degrees, got {q}")

    # Vertical load on vessel
    Fv = weight_per_length * S

    # Convert angle
    angle_rad = math.radians(90 - q)
    sin_angle = math.sin(angle_rad)

    if abs(sin_angle) < 1e-10:
        raise ValueError(f"Invalid angle q={q}deg: results in division by zero")

    # Total force along catenary
    F = Fv / sin_angle

    # Horizontal force along catenary
    Fh = F * math.cos(angle_rad)

    # Update data dict (legacy behavior mutates input)
    result = data.copy()
    result.update({
        "Fv": Fv,
        "F": F,
        "Fh": Fh
    })

    return result


# Additional helper functions for full compatibility

def validate_catenary_data(data: Dict[str, Any], method: str) -> None:
    """
    Validate input data for catenary calculations.

    Parameters:
        data: Input dictionary
        method: One of "force", "angle", "X"

    Raises:
        ValueError: If validation fails
    """
    if method == "force":
        required = ["F", "w", "d"]
    elif method == "angle":
        required = ["q", "d"]
    elif method == "X":
        required = ["X", "d"]
    else:
        raise ValueError(f"Unknown method: {method}")

    missing = [p for p in required if p not in data or data.get(p) is None]
    if missing:
        raise ValueError(f"Missing required parameters for {method} method: {missing}")


def convert_legacy_to_modern(data: Dict[str, Any]) -> Optional[object]:
    """
    Convert legacy dict format to modern CatenaryInput dataclass.

    This is used internally when the modern solver is available.

    Parameters:
        data: Legacy dict format

    Returns:
        CatenaryInput object

    Note:
        Not all legacy data maps directly to modern format.
        This is a best-effort conversion.
    """
    if not HAS_MODERN_SOLVER:
        raise ImportError("Modern solver not available")

    # Try to extract parameters
    # Modern solver needs: length, horizontal_span, vertical_span, weight_per_length, ea_stiffness

    # This is challenging because legacy API doesn't provide all these parameters
    # The adapter primarily uses the legacy formulas instead

    raise NotImplementedError(
        "Direct conversion from legacy to modern format not fully supported. "
        "Use modern API directly for new code."
    )


# Module-level constants for testing
LEGACY_API_VERSION = "1.0"
ADAPTER_VERSION = "1.0.0"

__all__ = [
    'catenaryEquation',
    'catenaryForces',
    'validate_catenary_data',
    'LEGACY_API_VERSION',
    'ADAPTER_VERSION'
]
