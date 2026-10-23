"""Tank survey geometry, independent of unverified API 653 acceptance limits.

Least-squares basis: NIST/SEMATECH e-Handbook (2003), sections 4.1.4.1
and 4.4.3.1: https://www.itl.nist.gov/div898/handbook/pmd/section4/pmd431.htm
The circular planar model is derived from z=a+px+qy on a circle:
x=R*cos(theta), y=R*sin(theta). Numerical method: NumPy linalg.lstsq.
No standards-derived constants, tolerances or acceptance formulas are used.
"""
import math

import numpy as np

GEOMETRY_SOURCES = [
    {"document": "NIST/SEMATECH e-Handbook of Statistical Methods",
     "edition": "2003", "section": "4.1.4.1 and 4.4.3.1, least squares",
     "url": "https://www.itl.nist.gov/div898/handbook/pmd/section4/pmd431.htm"},
    {"document": "NumPy reference", "section": "numpy.linalg.lstsq",
     "url": "https://numpy.org/doc/stable/reference/generated/numpy.linalg.lstsq.html"},
]


def finite(value, name, *, positive=False, nonnegative=False):
    """Require explicit finite numbers, excluding booleans and text."""
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        raise ValueError(f"{name} must be a finite number")
    if not math.isfinite(value):
        raise ValueError(f"{name} must be finite")
    if (positive and value <= 0) or (nonnegative and value < 0):
        raise ValueError(f"{name} is outside its physical range")
    return float(value)


def _vector(values, name):
    if not isinstance(values, (list, tuple)):
        raise ValueError(f"{name} must be a list of finite numbers")
    return np.array([finite(v, name) for v in values], dtype=float)


def reduce_shell_survey(diameter_m, angles_deg, elevations_mm):
    """Fit z=a+b*cos(theta)+c*sin(theta); return residuals in input order.

    Angles increase in the survey's declared direction, in [0,360), from its
    declared datum. Elevations are positive up relative to one stable datum.
    Four distinct stations are a software identifiability requirement (three
    coefficients plus a residual), NOT an Annex B survey-spacing criterion.
    Irregular spacing is supported mathematically but not code-qualified.
    """
    diameter = finite(diameter_m, "diameter_m", positive=True)
    angles = _vector(angles_deg, "angles_deg")
    z = _vector(elevations_mm, "elevations_mm")
    if len(angles) != len(z) or len(z) < 4:
        raise ValueError("shell survey needs matching arrays and at least four stations")
    if np.any(angles < 0) or np.any(angles >= 360) or len(set(angles)) != len(angles):
        raise ValueError("angles must be unique and in [0, 360)")
    radians = np.deg2rad(angles)
    matrix = np.column_stack((np.ones(len(z)), np.cos(radians), np.sin(radians)))
    # Centering protects small survey variations from a large absolute datum.
    datum = float(z[0])
    coefficients, _, rank, _ = np.linalg.lstsq(matrix, z - datum, rcond=None)
    if rank != 3:
        raise ValueError("shell survey cannot resolve a plane")
    fitted_relative = matrix @ coefficients
    residuals = z - datum - fitted_relative
    coefficients[0] += datum
    amplitude = math.hypot(coefficients[1], coefficients[2])
    # Geometry: mm per m / 1000; 2R=D. These are unit/geometry identities.
    slope = (amplitude / diameter) * (2 / 1000)
    variation = z - z.mean()
    total = float(variation @ variation)
    error = float(residuals @ residuals)
    r_squared = None if total == 0 else 1 - error / total
    azimuth = None if total == 0 else math.degrees(math.atan2(coefficients[2], coefficients[1])) % 360
    result = {
        "coefficients_mm": coefficients.tolist(),
        "fitted_elevations_mm": (fitted_relative + datum).tolist(),
        "residuals_mm": residuals.tolist(), "amplitude_mm": amplitude,
        "diametral_tilt_mm": 2 * amplitude, "tilt_slope": slope,
        "tilt_angle_deg": math.degrees(math.atan(slope)),
        "high_point_azimuth_deg": azimuth, "r_squared": r_squared,
        "largest_station_gap_deg": float(np.diff(np.r_[np.sort(angles), min(angles)+360]).max()),
        "maximum_absolute_residual_mm": float(np.abs(residuals).max()),
    }
    if not all(math.isfinite(v) for v in [*coefficients, *residuals, amplitude, slope, total, error]):
        raise ValueError("survey reduction exceeds finite numerical range")
    return result


def reduce_edge_settlement(radial_extent_m, shell_elevation_mm, reference_elevation_mm):
    """Diagnostic drop from an explicitly surveyed interior reference to shell.

    Positive drop is downwards. The two elevations must share one datum.
    Slope=drop/extent is a geometric secant, not an Annex B allowable or
    a correction for planar tilt. The reference and weld geometry require review.
    """
    extent = finite(radial_extent_m, "radial_extent_m", positive=True)
    shell = finite(shell_elevation_mm, "shell_elevation_mm")
    reference = finite(reference_elevation_mm, "reference_elevation_mm")
    drop = finite(reference-shell, "edge drop")
    slope = finite((drop / extent) / 1000, "edge slope")
    return {"radial_extent_m": extent, "drop_mm": drop, "slope": slope}


def reduce_bottom_profile(distances_m, elevations_mm):
    """Diagnostic deviations from the chord joining profile endpoints.

    Linear interpolation: chord=z0+(zn-z0)*(x-x0)/(xn-x0).
    Negative deviation indicates depression; positive indicates bulge.
    This diagnostic does not locate Annex B depression boundaries or establish
    a code-defined depression radius from one radial profile.
    """
    x = _vector(distances_m, "distances_m")
    z = _vector(elevations_mm, "elevations_mm")
    if len(x) != len(z) or len(x) < 3 or np.any(x < 0) or np.any(np.diff(x) <= 0):
        raise ValueError("bottom profile needs three matching, increasing nonnegative stations")
    chord = z[0] + (z[-1] - z[0]) * ((x-x[0]) / (x[-1]-x[0]))
    deviations = z-chord
    if not np.isfinite(deviations).all():
        raise ValueError("profile reduction exceeds finite numerical range")
    return {"chord_deviations_mm": deviations.tolist(),
            "maximum_depression_mm": float(max(0, -deviations.min())),
            "maximum_bulge_mm": float(max(0, deviations.max())),
            "profile_span_m": float(x[-1]-x[0])}
