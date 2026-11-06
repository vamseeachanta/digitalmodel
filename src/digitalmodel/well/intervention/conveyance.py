"""Adapter to the existing pump-down package; no copied conveyance physics."""

from dataclasses import dataclass

from digitalmodel.units import Q_
from digitalmodel.well.completion.pump_down import geometry, kinematics
from digitalmodel.well.completion.pump_down._validation import finite, scalar
from digitalmodel.well.completion.pump_down.force_balance import resisting_force


@dataclass(frozen=True)
class ConveyanceResult:
    """Lumped-section feasibility only; not complete well access qualification."""

    status: str
    line_speed_m_s: float | None
    resistance_n: float | None
    limitations: str


def wireline_conveyance(
    *,
    casing_id,
    tool_od,
    drift_id,
    min_radial_clearance,
    rate,
    density,
    buoyed_bha_weight,
    buoyed_cable_weight,
    lateral_length,
    inclination_deg,
    mu,
    eccentricity,
    k,
) -> ConveyanceResult:
    """Screen slickline/e-line access through the existing pump-down core.

    The supplied input set is required for deviated/horizontal wireline
    evaluation. Units are Pint quantities or bare SI; bare inclination is
    degrees from vertical, and angle quantities are converted to degrees.
    Coefficients mu, eccentricity and K are caller inputs.
    Basis: pump_down.force_balance.resisting_force (Coulomb balance) and
    pump_down.kinematics.solve_line_speed (DOE-HDBK-1012/3-92, Fluid Flow).
    No inclination cutoff or implicit coefficient is introduced here.

    Positive solved speed and specified clearance constitute a screen pass,
    not proof of reach. Survey curvature, cable tension, weak point, retrieval,
    circulation/pressure limits and well barriers need separate verification.
    Negative resistance is identified as gravity assisted; the pump-down
    speed model does not qualify that branch. Missing inputs are never
    interpreted as feasible. Tubing dimensions and measured drift are inputs,
    so this adapter does not assume an API nominal drift allowance.
    """
    diameter = scalar(casing_id, "m", "casing ID", positive=True)
    od = scalar(tool_od, "m", "tool OD", positive=True)
    drift = scalar(drift_id, "m", "measured drift ID", positive=True)
    clearance = scalar(min_radial_clearance, "m", "radial clearance")
    if drift > diameter:
        raise ValueError("drift ID cannot exceed casing ID")
    inclination = scalar(inclination_deg, "degree", "inclination")
    inclination = finite(inclination, "inclination", maximum=180)
    scalar(rate, "m**3/s", "pump rate")
    scalar(density, "kg/m**3", "fluid density", positive=True)
    finite(mu, "mu")
    finite(eccentricity, "eccentricity")
    scalar(k, "dimensionless", "K", positive=True)
    resistance = resisting_force(
        buoyed_bha_weight,
        buoyed_cable_weight,
        lateral_length,
        inclination,
        mu,
        eccentricity,
    )
    load = float(resistance.to("N").magnitude)
    limitations = (
        "Lumped-section screen; survey, tension/weak-point, retrieval, "
        "pressure, pumping compatibility and barriers remain unqualified."
    )
    if od >= diameter or not geometry.runnable(
        Q_(od, "m"), Q_(drift, "m"), Q_(clearance, "m")
    ):
        return ConveyanceResult("clearance_fail", None, load, limitations)
    if load < 0:
        return ConveyanceResult(
            "gravity_assisted_requires_review", None, load, limitations
        )
    section = geometry.bypass(Q_(diameter, "m"), Q_(od, "m"))
    speed = kinematics.solve_line_speed(rate, section, density, resistance, k)
    return ConveyanceResult(
        "stalled" if speed.stall else "pump_down_screen_pass",
        float(speed.line_speed.to("m/s").magnitude),
        load,
        limitations,
    )
