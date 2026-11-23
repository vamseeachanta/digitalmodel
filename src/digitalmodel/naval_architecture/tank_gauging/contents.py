"""Planar gauge correction and liquid mass, without fluid-property defaults."""

import math

from .calibration import bounded_depth, finite, positive


def centre_sounding(
    gauge_sounding_m: float,
    reference_height_m: float,
    *,
    x_gauge_m: float = 0,
    y_gauge_m: float = 0,
    trim_deg: float = 0,
    list_deg: float = 0,
) -> float:
    """Translate a tank-axis depth to the origin of a planar free surface.

    x and y are signed horizontal tank-fixed offsets from the tank centre.
    Positive trim/list means the surface rises towards positive x/y:
    h_g = h_c + x tan(trim) + y tan(list). The correction is subtracted.
    Angles describe independent surface slopes, rather than Euler rotations.
    Zero defaults mean centre gauge and level surface, not a fitted constant.

    Basis: OpenStax, Precalculus 2e (2021), §5.4, tan = opposite/adjacent.
    Only a single-valued plane is represented; vertical slopes are excluded.
    Roof/bottom clipping elsewhere in the tank is not evaluated. A centre
    depth alone determines volume only for an unclipped prismatic tank or
    a calibration qualified for this correction by the caller.
    """
    depth = bounded_depth(gauge_sounding_m, reference_height_m)
    x = finite(x_gauge_m, "x_gauge_m")
    y = finite(y_gauge_m, "y_gauge_m")
    trim = finite(trim_deg, "trim_deg")
    heel = finite(list_deg, "list_deg")
    # A plane's slope is undefined at ±90 degrees (right-triangle identity).
    if abs(trim) >= 90 or abs(heel) >= 90:
        raise ValueError("surface slope angles must lie strictly between -90 and 90")
    corrected = depth - x * math.tan(math.radians(trim))
    corrected -= y * math.tan(math.radians(heel))
    return bounded_depth(corrected, reference_height_m)


def density_at_temperature(
    reference_density_kg_m3: float,
    reference_temperature_c: float,
    temperature_c: float,
    expansion_coefficient_per_c: float,
) -> float:
    """Return rho_ref / (1 + beta (T - T_ref)), in kg/m³.

    OpenStax, University Physics vol. 2 (2016), §1.3, eq. 1.4:
    ΔV = beta V_ref ΔT; constant mass gives the density ratio above.
    beta is volumetric, supplied by the caller with its qualified temperature
    range; no material value or universal temperature range is assumed.
    This is a linear volume-expansion approximation, not custody transfer.
    """
    density = positive(reference_density_kg_m3, "reference_density_kg_m3")
    reference = finite(reference_temperature_c, "reference_temperature_c")
    temperature = finite(temperature_c, "temperature_c")
    beta = finite(expansion_coefficient_per_c, "expansion_coefficient_per_c")
    # The unity term is the reference-volume ratio, not an empirical constant.
    factor = positive(1 + beta * (temperature - reference), "expansion factor")
    return positive(density / factor, "temperature-corrected density")


def liquid_mass_kg(volume_m3: float, density_kg_m3: float) -> float:
    """m = rho V; OpenStax, University Physics vol. 1 (2016), §14.1.

    Volume and density must refer to the same temperature and pressure.
    """
    volume = finite(volume_m3, "volume_m3")
    density = positive(density_kg_m3, "density_kg_m3")
    if volume < 0:
        raise ValueError("volume_m3 must be nonnegative")
    return finite(volume * density, "mass_kg")
