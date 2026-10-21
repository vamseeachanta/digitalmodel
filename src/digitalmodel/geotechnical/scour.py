# ABOUTME: Scour screening with sourced thresholds and explicit missing basis.
# ABOUTME: Pipeline scour, monopile scour, and rock armour scour protection.
"""Guarded scour screening with explicit sediment-motion and armour criteria.

Nonzero scour depths are NOT_EVALUATED until a qualified depth model is
available. A current motion threshold alone does not predict local scour.
Critical current uses FHWA-RD-03-052 (2005), chapter 5, equation 3 (SI).
The existing Shields stress helper cites USACE HEC-RAS v6.4.1 (2023).
DNV-RP-F107 concerns accidental pipeline protection; it is not claimed as
qualification of these scour or armour calculations. DNV-RP-0618 (2022),
Rock scour protection for monopiles, is a relevant design reference whose
licensed calculation clauses have not been verified here.
"""

from dataclasses import dataclass, field
import math

STANDARD = "NOT_ESTABLISHED"
# Public research report, not a design standard or a compliance assertion.
CURRENT_SOURCE = (
    "FHWA-RD-03-052 (2005), chapter 5, equation 3 (SI); "
    "https://www.fhwa.dot.gov/publications/research/infrastructure/"
    "hydraulics/03052/05b.cfm"
)


@dataclass
class PipelineScourResult:
    """Result of pipeline scour depth calculation."""

    scour_depth_m: float | None
    pipe_od_m: float
    keulegan_carpenter: float | None
    current_wave_ratio: float
    standard: str = STANDARD
    status: str = "NOT_EVALUATED"
    reason: str = ""
    critical_velocity_ms: float | None = None
    sediment_motion_status: str = "NOT_EVALUATED"
    sources: list[str] = field(default_factory=list)


@dataclass
class MonopileScourResult:
    """Result of monopile scour depth calculation."""

    scour_depth_m: float | None
    pile_diameter_m: float
    scour_to_diameter_ratio: float | None
    standard: str = STANDARD
    status: str = "NOT_EVALUATED"
    reason: str = ""
    critical_velocity_ms: float | None = None
    sediment_motion_status: str = "NOT_EVALUATED"
    sources: list[str] = field(default_factory=list)


@dataclass
class RockArmourResult:
    """Result of rock armour thickness calculation."""

    thickness_m: float | None
    stability_number: float | None
    standard: str = STANDARD
    stability_limit: float | None = None
    stability_status: str = "NOT_EVALUATED"
    status: str = "NOT_EVALUATED"
    reason: str = ""


@dataclass(frozen=True)
class CriticalSedimentMotionResult:
    """Noncohesive incipient-motion thresholds, not a scour-depth assessment.

    Shear stress is in Pa; shear (friction) velocity is in m/s and must not
    be substituted for a depth-averaged or near-bed current velocity.
    """

    critical_shear_stress_pa: float
    critical_shear_velocity_ms: float


def critical_sediment_motion(
    *,
    sediment_d50_m: float,
    sediment_density_kg_m3: float,
    water_density_kg_m3: float,
    critical_shields_parameter: float,
    gravity_ms2: float,
) -> CriticalSedimentMotionResult:
    """Convert a supplied critical Shields parameter to motion thresholds.

    Source: USACE, HEC-RAS 2D Sediment Technical Reference Manual, v6.4.1
    (2023), section 2.14.1, equation 132; section 2.13.1, bed shear velocity
    definition following equation 111 (pages 77 and 69, respectively):
    https://www.hec.usace.army.mil/software/hec-ras/documentation/HEC-RAS%202D%20Sediment%20Technical%20Reference%20Manual-v6.4.1.pdf

    tau_cr = theta_cr * (rho_s - rho_w) * g * d50
    u_star_cr = sqrt(tau_cr / rho_w)

    All inputs are required: median grain diameter in m, grain and water
    densities in kg/m^3, dimensionless critical Shields parameter, and
    gravity in m/s^2. The caller must select and qualify the critical Shields
    parameter for the noncohesive sediment and transport criterion; no
    universal critical parameter or environmental properties are assumed.

    This is a threshold conversion, not a scour predictor. Cohesive erosion,
    depth-averaged critical current velocity, local flow amplification and
    scour depth are not evaluated. In particular, the returned friction
    velocity is distinct from the mean-current threshold returned by
    critical_current_velocity.

    Raises:
        ValueError: Inputs violate the positive submerged-weight domain or
            the thresholds cannot be represented as positive finite floats.
    """
    for name, value in (
        ("sediment_d50_m", sediment_d50_m),
        ("sediment_density_kg_m3", sediment_density_kg_m3),
        ("water_density_kg_m3", water_density_kg_m3),
        ("critical_shields_parameter", critical_shields_parameter),
        ("gravity_ms2", gravity_ms2),
    ):
        # Positive finite quantities are the domain of the cited equations,
        # not an empirical sediment-size range or an acceptance limit.
        if not math.isfinite(value) or value <= 0.0:
            raise ValueError(f"{name} must be positive and finite")
    if sediment_density_kg_m3 <= water_density_kg_m3:
        raise ValueError("sediment_density_kg_m3 must exceed water_density_kg_m3")

    # USACE v6.4.1 (2023), section 2.14.1, equation 132, rearranged.
    stress = (
        critical_shields_parameter
        * (sediment_density_kg_m3 - water_density_kg_m3)
        * gravity_ms2
        * sediment_d50_m
    )
    # USACE v6.4.1 (2023), section 2.13.1, definition after equation 111.
    velocity = math.sqrt(stress / water_density_kg_m3)
    if not all(math.isfinite(value) and value > 0.0 for value in (stress, velocity)):
        raise ValueError("thresholds must be representable as positive finite floats")

    return CriticalSedimentMotionResult(
        critical_shear_stress_pa=stress,
        critical_shear_velocity_ms=velocity,
    )


def _positive(name: str, value: float) -> None:
    """Enforce a mathematical positive finite domain, not a design limit."""
    if not math.isfinite(value) or value <= 0.0:
        raise ValueError(f"{name} must be positive and finite")


def _velocity(name: str, value: float) -> None:
    """Inputs are magnitudes; signed components need a separate flow model."""
    if not math.isfinite(value) or value < 0.0:
        raise ValueError(f"{name} must be nonnegative and finite")


def critical_current_velocity(
    *,
    sediment_d50_mm: float,
    water_depth_m: float,
    critical_shields_parameter: float,
) -> float:
    """Mean incipient-transport current (m/s), Neill/Shields SI relation.

    Source: FHWA-RD-03-052 (May 2005), chapter 5, eq. 3:
    Ucrit = 31.08 * sqrt(theta_cr) * h**(1/6) * d50**(1/3).
    d50 and h are in metres. This empirical SI coefficient shall not be
    used with other units. The caller supplies a qualified Shields value;
    the report's temperature/material-specific theta fits are not defaults.
    The relation concerns steady noncohesive approach flow, not wave motion,
    cohesive erosion, local pile amplification, or an equilibrium depth.
    """
    for name, value in (
        ("sediment_d50_mm", sediment_d50_mm),
        ("water_depth_m", water_depth_m),
        ("critical_shields_parameter", critical_shields_parameter),
    ):
        _positive(name, value)
    # SI prefix definition: 1000 millimetres per metre, not an empirical value.
    diameter_m = sediment_d50_mm / 1000.0
    # FHWA-RD-03-052 (2005), ch. 5, eq. 3: SI coefficient and exponents.
    velocity = (
        31.08
        * math.sqrt(critical_shields_parameter)
        * water_depth_m ** (1.0 / 6.0)
        * math.cbrt(diameter_m)
    )
    _positive("critical velocity result", velocity)
    return velocity


def _motion(
    current: float, diameter: float | None, depth: float | None, shields: float | None
) -> tuple[float | None, str, str]:
    inputs = {
        "sediment_d50_mm": diameter,
        "water_depth_m": depth,
        "critical_shields_parameter": shields,
    }
    for name, value in inputs.items():
        if value is not None:
            _positive(name, value)
    missing = [name for name, value in inputs.items() if value is None]
    if missing:
        return None, "NOT_EVALUATED", "Missing motion inputs: " + ", ".join(missing)
    critical = critical_current_velocity(**inputs)
    status = "AT_OR_ABOVE_THRESHOLD" if current >= critical else "BELOW_THRESHOLD"
    return critical, status, "Current motion only; local scour is not established."


def pipeline_scour_depth(
    pipe_od_m: float,
    current_velocity_ms: float,
    wave_orbital_velocity_ms: float,
    sediment_d50_mm: float,
    burial_ratio: float = 0.0,
    wave_period_s: float | None = None,
    *,
    water_depth_m: float | None = None,
    critical_shields_parameter: float | None = None,
) -> PipelineScourResult:
    """Return diagnostics and an explicitly unevaluated nonzero scour depth.

    Sediment size is in mm, geometry/depth in m, velocity magnitudes in m/s
    and period in s. Existing positional calls remain valid. No unsupported
    current/wave curve, burial reduction or default period is assumed.
    KC = Uw*T/D is the Keulegan-Carpenter dimensionless definition, not a
    depth model. Sediment-motion status describes only the steady current.
    """
    _positive("pipe_od_m", pipe_od_m)
    _positive("sediment_d50_mm", sediment_d50_mm)
    _velocity("current_velocity_ms", current_velocity_ms)
    _velocity("wave_orbital_velocity_ms", wave_orbital_velocity_ms)
    if not math.isfinite(burial_ratio) or not 0.0 <= burial_ratio <= 1.0:
        raise ValueError("burial_ratio must be finite and in [0, 1]")
    if wave_period_s is not None:
        _positive("wave_period_s", wave_period_s)
    critical, motion, reason = _motion(
        current_velocity_ms,
        sediment_d50_mm,
        water_depth_m,
        critical_shields_parameter,
    )
    no_flow = current_velocity_ms == wave_orbital_velocity_ms == 0.0
    kc = 0.0 if wave_orbital_velocity_ms == 0.0 else None
    if wave_period_s is not None:
        kc = wave_orbital_velocity_ms * wave_period_s / pipe_od_m
    if kc is not None and not math.isfinite(kc):
        raise ValueError("keulegan_carpenter must be finite")
    # Ratio of nonnegative magnitudes; scaling avoids overflow in Uc+Uw.
    scale = max(current_velocity_ms, wave_orbital_velocity_ms)
    ratio = (
        current_velocity_ms
        / scale
        / (current_velocity_ms / scale + wave_orbital_velocity_ms / scale)
        if scale
        else 0.0
    )
    if wave_orbital_velocity_ms and wave_period_s is None:
        reason += " Missing wave_period_s for KC."
    reason += " No qualified pipeline depth model or wave/burial model."
    return PipelineScourResult(
        scour_depth_m=0.0 if no_flow else None,
        pipe_od_m=pipe_od_m,
        keulegan_carpenter=kc,
        current_wave_ratio=ratio,
        status="NO_FLOW" if no_flow else "NOT_EVALUATED",
        reason=reason,
        critical_velocity_ms=critical,
        sediment_motion_status=motion,
        sources=[CURRENT_SOURCE] if critical is not None else [],
    )


def monopile_scour_depth(
    pile_diameter_m: float,
    current_velocity_ms: float,
    water_depth_m: float,
    *,
    sediment_d50_mm: float | None = None,
    critical_shields_parameter: float | None = None,
) -> MonopileScourResult:
    """Assess steady approach-current motion; nonzero depth is NOT_EVALUATED.

    Water depth and sediment size enter the sourced mean-current threshold.
    Being below that threshold does not exclude local clear-water scour.
    A qualified pile depth model is required before returning a nonzero depth.
    """
    _positive("pile_diameter_m", pile_diameter_m)
    _positive("water_depth_m", water_depth_m)
    _velocity("current_velocity_ms", current_velocity_ms)
    critical, motion, reason = _motion(
        current_velocity_ms,
        sediment_d50_mm,
        water_depth_m,
        critical_shields_parameter,
    )
    no_flow = current_velocity_ms == 0.0
    return MonopileScourResult(
        scour_depth_m=0.0 if no_flow else None,
        pile_diameter_m=pile_diameter_m,
        scour_to_diameter_ratio=0.0 if no_flow else None,
        status="NO_FLOW" if no_flow else "NOT_EVALUATED",
        reason=reason + " No qualified monopile depth model.",
        critical_velocity_ms=critical,
        sediment_motion_status=motion,
        sources=[CURRENT_SOURCE] if critical is not None else [],
    )


def rock_armour_thickness(
    current_velocity_ms: float,
    rock_density_kg_m3: float,
    water_density_kg_m3: float,
    rock_d50_m: float,
    *,
    gravity_ms2: float | None = None,
    stability_limit: float | None = None,
    layer_count: float | None = None,
) -> RockArmourResult:
    """Compare a dimensionless velocity number with a supplied upper limit.

    Ns=U/sqrt(g*d50*(rho_rock/rho_water-1)) is a nondimensionalization,
    not an Izbash calibration. The caller qualifies the matching velocity
    basis and limit; no universal limit is implied. Nominal thickness is
    layer_count*d50, not a minimum design thickness. Full protection design
    remains NOT_EVALUATED even when this single comparison passes: filter,
    grading, placement, waves and edge scour are outside this calculation.
    """
    _velocity("current_velocity_ms", current_velocity_ms)
    for name, value in (
        ("rock_density_kg_m3", rock_density_kg_m3),
        ("water_density_kg_m3", water_density_kg_m3),
        ("rock_d50_m", rock_d50_m),
    ):
        _positive(name, value)
    if rock_density_kg_m3 <= water_density_kg_m3:
        raise ValueError("rock_density must exceed water_density")
    inputs = {
        "gravity_ms2": gravity_ms2,
        "stability_limit": stability_limit,
        "layer_count": layer_count,
    }
    for name, value in inputs.items():
        if value is not None:
            _positive(name, value)
    stability = None
    status = "NOT_EVALUATED"
    if gravity_ms2 is not None:
        delta = (rock_density_kg_m3 - water_density_kg_m3) / water_density_kg_m3
        scale_squared = gravity_ms2 * rock_d50_m * delta
        _positive("stability velocity scale squared", scale_squared)
        stability = current_velocity_ms / math.sqrt(scale_squared)
        if not math.isfinite(stability):
            raise ValueError("stability_number must be finite")
        if stability_limit is not None:
            status = "PASS" if stability <= stability_limit else "FAIL"
    thickness = None if layer_count is None else layer_count * rock_d50_m
    if thickness is not None:
        _positive("nominal layer thickness", thickness)
    missing = [name for name, value in inputs.items() if value is None]
    reason = "Full protection design NOT_EVALUATED; nominal layer geometry only."
    if missing:
        reason += " Missing inputs: " + ", ".join(missing)
    if status == "FAIL":
        reason += " Stability number exceeds supplied limit; revise grading or loading."
    return RockArmourResult(
        thickness_m=thickness,
        stability_number=stability,
        stability_limit=stability_limit,
        stability_status=status,
        reason=reason,
    )
