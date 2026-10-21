"""Static, small-angle free-surface loss for a rectangular slack tank.

Source: USNA, EN400 Principles of Ship Performance (Summer 2020),
§4.8.3–4.8.3.1, printed pages 4-29–4-30. These are textbook geometric
relations, without a standards-derived acceptance criterion or fluid default.
"""

from typing import Literal

from .calibration import finite, positive


def rectangular_free_surface_moment_kg_m(
    length_m: float,
    breadth_m: float,
    density_kg_m3: float,
    *,
    fill_state: Literal["slack", "empty", "pressed_full"],
) -> float:
    """rho L B³/12 in kg m; breadth is transverse, length longitudinal.

    fill_state is required: a near-full slack tank is not pressed full.
    Empty and pressed-full tanks return zero. For slack tanks the entire
    rectangular surface must be exposed, without roof/bottom clipping.
    This returns a mass moment, not torque; GM loss divides by mass.
    No dynamic sloshing, baffles, subdivisions or large-angle correction.
    """
    length = positive(length_m, "length_m")
    breadth = positive(breadth_m, "breadth_m")
    density = positive(density_kg_m3, "density_kg_m3")
    if fill_state not in ("slack", "empty", "pressed_full"):
        raise ValueError("fill_state must be slack, empty or pressed_full")
    if fill_state != "slack":
        return 0.0
    # EN400 (Summer 2020), §4.8.3.1: integral y² dA = L B³ / 12.
    try:
        moment = density * length * breadth**3 / 12
    except OverflowError as exc:
        raise ValueError("free-surface moment exceeds numeric range") from exc
    return positive(moment, "free-surface moment")


def gm_loss_m(free_surface_moment_kg_m: float, displacement_kg: float) -> float:
    """FSC = FSM / displacement_mass; EN400 (2020), §4.8.3.

    Subtract this loss from solid GM. It is not an overall stability verdict.
    Both quantities use mass units; gravitational acceleration cancels.
    """
    moment = finite(free_surface_moment_kg_m, "free_surface_moment_kg_m")
    displacement = positive(displacement_kg, "displacement_kg")
    if moment < 0:
        raise ValueError("free_surface_moment_kg_m must be nonnegative")
    return finite(moment / displacement, "gm_loss_m")
