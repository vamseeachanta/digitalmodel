# ABOUTME: Physically-estimated viscous damping coefficients for the rod string.
# ABOUTME: Returns SEPARATE upstroke and downstroke values per taper section.
"""Viscous damping estimation for a sucker-rod string.

Damping is what gives a dynamometer card its area. A single scalar damping
constant cannot reproduce the asymmetry between upstroke and downstroke, so the
card comes out thin and area-less. This module estimates a coefficient for each
taper section, separately for each stroke direction, from the annular geometry
and the produced-fluid viscosity.

The formulation is the standard concentric-annulus viscous drag model: the rod
moves inside the tubing bore, and the drag depends on the diameter ratio
``m = ID_tubing / OD_rod`` through the geometric groups ``B1``, ``B2``, ``B3``.
Couplings are treated separately because they sweep a much tighter clearance
than the rod body does.

All inputs and outputs are SI.
"""

from math import log, pi
from typing import Tuple

import numpy as np

# Validity floor of the coupling-drag correlation: below this coupling-to-bore
# diameter ratio the fractional power has a negative base and is undefined.
COUPLING_RATIO_FLOOR = 0.381


def estimate_damping_coeff(
    od_rod: float,
    od_connect: float,
    od_pump: float,
    id_well: float,
    mu: float,
    l_tap: float,
    ro_rod: float,
) -> Tuple[float, float]:
    """Estimate upstroke and downstroke damping for one taper section.

    Args:
        od_rod: Rod body outer diameter, m.
        od_connect: Coupling outer diameter, m. Sweeps the tight clearance.
        od_pump: Pump bore diameter, m.
        id_well: Tubing inner diameter, m.
        mu: Produced-fluid dynamic viscosity, Pa-s.
        l_tap: Length of this taper section, m.
        ro_rod: Rod material density, kg/m^3.

    Returns:
        ``(c_upstroke, c_downstroke)`` in 1/s.

    Raises:
        ValueError: If the geometry is degenerate — ``id_well`` must exceed
            ``od_rod``, otherwise the annulus has no thickness and ``log(m)``
            is undefined.
    """
    if od_rod <= 0.0 or id_well <= 0.0:
        raise ValueError(
            f"rod OD ({od_rod}) and tubing ID ({id_well}) must be positive"
        )
    if id_well <= od_rod:
        raise ValueError(
            f"tubing ID ({id_well} m) must exceed rod OD ({od_rod} m); "
            "the annulus has no thickness"
        )

    a_rod = pi * od_rod ** 2 / 4.0
    a_well = pi * id_well ** 2 / 4.0

    # Annular diameter ratio and its geometric groups.
    m = id_well / od_rod
    b1 = (m ** 2 - 1.0) / (2.0 * log(m)) - 1.0
    b2 = m ** 4 - 1.0 - (m ** 2 - 1.0) ** 2 / log(m)
    b3 = 1.0 / log(m)

    # Pump-bore to rod ratio, nudged off unity to keep the ratio finite when a
    # rod diameter coincides with the pump bore.
    m2 = od_pump / od_rod + 1.0e-6

    # Coupling drag: tighter clearance, and it differs by stroke direction
    # because the displaced fluid moves with or against the rod.
    #
    # The correlation raises (od_connect/id_well - 0.381) to a fractional
    # power, so it is only defined once the coupling fills more than 38.1% of
    # the bore. Below that the base goes negative and the whole card silently
    # becomes NaN, so refuse explicitly rather than propagate it. This is the
    # regime you land in when a casing ID is passed where a tubing ID belongs.
    coupling_ratio = od_connect / id_well
    if coupling_ratio <= COUPLING_RATIO_FLOOR:
        raise ValueError(
            f"coupling OD / bore ID = {coupling_ratio:.4f} is at or below the "
            f"{COUPLING_RATIO_FLOOR} validity floor of the coupling-drag "
            f"correlation (coupling OD {od_connect} m, bore ID {id_well} m). "
            "Check that a tubing ID was supplied rather than a casing ID."
        )

    clearance = id_well / 2.0 - od_rod / 2.0
    coupling_base = 1.3e4 * mu * (coupling_ratio - COUPLING_RATIO_FLOOR) ** 2.57 / clearance
    k_c1 = coupling_base * (2.77 - 1.69 * (m ** 2 - 1.0) / (m2 ** 2 - 1.0))
    k_c2 = coupling_base * (2.77 + 1.69 * (m ** 2 - 1.0))

    # Rod-body annular drag terms.
    n1 = (
        pi * mu * (b3 + 4 * b1 ** 2 / b2)
        + 4 * mu * pi * (m2 ** 2 - 1) ** 2 / b2
        - 8 * pi * mu * (m2 ** 2 - 1) * b1 / b2
    )
    n2 = (
        pi * mu * (b3 + 4 * b1 ** 2 / b2)
        + 4 * mu * pi / b2
        + 8 * pi * mu * b1 / b2
    )

    # Coupling contribution, distributed over the taper length.
    coupling_scale = (a_well - a_rod) / (2.0 * (m ** 2 - 1.0) ** 2 * l_tap)
    n3 = coupling_scale * (m2 ** 2 - 1.0) ** 2 * k_c1
    n4 = coupling_scale * k_c2

    normaliser = ro_rod * a_rod
    c_upstroke = (n1 + n3) / normaliser
    c_downstroke = (n2 + n4) / normaliser
    return c_upstroke, c_downstroke


# Below this viscosity the estimate degenerates; clamp to a physically sane floor.
MIN_VISCOSITY_PA_S = 0.001
DEFAULT_VISCOSITY_PA_S = 0.01


def estimate_damping_profile(
    rod_diameters: np.ndarray,
    coupling_diameters: np.ndarray,
    taper_lengths: np.ndarray,
    rod_densities: np.ndarray,
    pump_diameter: float,
    tubing_id: float,
    viscosity: float,
) -> np.ndarray:
    """Estimate damping for every taper section, both stroke directions.

    Sections are ordered top-of-string first, matching the rod-string
    convention used throughout the solver.

    Args:
        rod_diameters: Rod body OD per section, m.
        coupling_diameters: Coupling OD per section, m.
        taper_lengths: Length per section, m.
        rod_densities: Material density per section, kg/m^3.
        pump_diameter: Pump bore, m.
        tubing_id: Tubing inner diameter, m.
        viscosity: Produced-fluid dynamic viscosity, Pa-s.

    Returns:
        Array of length ``2 * n_sections``: the first half holds upstroke
        coefficients top-to-bottom, the second half downstroke.

    Raises:
        ValueError: If the per-section arrays disagree in length, or if any
            section's geometry is degenerate.
    """
    n_tap = len(taper_lengths)
    lengths = {
        "rod_diameters": len(rod_diameters),
        "coupling_diameters": len(coupling_diameters),
        "rod_densities": len(rod_densities),
    }
    mismatched = {k: v for k, v in lengths.items() if v != n_tap}
    if mismatched:
        raise ValueError(
            f"per-section arrays must all have length {n_tap} "
            f"(taper_lengths); got {mismatched}"
        )

    mu = viscosity if viscosity > MIN_VISCOSITY_PA_S else DEFAULT_VISCOSITY_PA_S

    coefficients = np.empty(2 * n_tap, dtype=np.float64)
    for i in range(n_tap):
        try:
            c_up, c_dn = estimate_damping_coeff(
                od_rod=rod_diameters[i],
                od_connect=coupling_diameters[i],
                od_pump=pump_diameter,
                id_well=tubing_id,
                mu=mu,
                l_tap=taper_lengths[i],
                ro_rod=rod_densities[i],
            )
        except (ValueError, ZeroDivisionError) as exc:
            raise ValueError(
                f"damping estimation failed for taper section {i} "
                f"(rod OD {rod_diameters[i]} m, tubing ID {tubing_id} m): {exc}"
            ) from exc
        coefficients[i] = c_up
        coefficients[i + n_tap] = c_dn
    return coefficients
