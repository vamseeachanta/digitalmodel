"""One line-sizing entry point for liquid, gas and two-phase velocity screens.

SI units throughout. Q is ACTUAL volume at inlet/mixture conditions, never
standard gas volume. Dimensions reuse materials.line_pipe (ASME B36.10M).
Friction reuses the field-development Swamee–Jain implementation, now in
infrastructure.utils.pipe_friction. Source locators ship in basis/wikis.
"""

from dataclasses import dataclass
import math

from digitalmodel.citations.schema import Citation, CitedValue
from digitalmodel.infrastructure.utils.pipe_friction import swamee_jain_friction_factor
from digitalmodel.materials.line_pipe import list_nps, od_for_nps, wt_for_schedule
from ._basis import citations, positive, nonnegative


@dataclass(frozen=True)
class Flow:
    """Actual m³/s; kg/m³; Pa·s; m; and inlet Pa ABSOLUTE for gas.

    Two-phase density is the no-slip mixture density at the governing
    flowing condition. viscosity may be None only for two_phase, whose
    pressure drop is deliberately not calculated. Sand/erosion-corrosion,
    slugging, flashing and slip require another model. C is the API RP 14E
    US-customary constant, with its service/selection basis required.
    Roughness and fittings K are caller inputs, not material defaults.
    """

    phase: str
    flow_m3_s: float
    density_kg_m3: float
    viscosity_pa_s: float | None
    length_m: float
    roughness_m: float
    fittings_k: float = 0
    elevation_change_m: float = 0
    inlet_pressure_pa_abs: float | None = None
    erosional_c_us: float | None = None
    erosional_c_basis: str | None = None


@dataclass(frozen=True)
class LineResult:
    nps: float
    schedule: str
    inner_diameter_m: float
    wall_thickness_m: float
    velocity_m_s: float
    reynolds: float | None
    regime: str
    friction_factor: float | None
    pressure_drop_pa: float | None
    outlet_pressure_pa_abs: float | None
    method: str
    velocity_limit_m_s: float
    erosional_velocity_m_s: float | None
    within_velocity_limit: bool
    notes: tuple[str, ...]
    citations: tuple[Citation, ...]


def service_velocity_limit(service: str) -> CitedValue:
    """Selected water upper bounds, ASHRAE Fundamentals 2017 Ch.22 Table22.

    General water 10 ft/s; pump suction/drains 7 ft/s. Conversion 1 ft =
    0.3048 m exactly (international foot). These are screening guidance,
    not universal process/noise/erosion criteria. No gas/steam or materials
    table is inferred. Table source:
    https://handbook.ashrae.org/handbooks/f17/IP/F17_Ch22/f17_ch22_ip.aspx
    """
    refs = citations("velocity")
    upper_ft_s = {"general_water": 10, "pump_suction_water": 7}
    if service not in upper_ft_s:
        raise ValueError(
            f"unsupported service; choose {tuple(upper_ft_s)} or supply a qualified limit"
        )
    return CitedValue(upper_ft_s[service] * 0.3048, refs[0], "m/s")


def _validate(flow: Flow, velocity_limit_m_s: float) -> None:
    if flow.phase not in ("liquid", "gas", "two_phase"):
        raise ValueError("phase must be liquid, gas or two_phase")
    positive(
        flow_m3_s=flow.flow_m3_s,
        density_kg_m3=flow.density_kg_m3,
        velocity_limit_m_s=velocity_limit_m_s,
    )
    nonnegative(
        length_m=flow.length_m, roughness_m=flow.roughness_m, fittings_k=flow.fittings_k
    )
    nonnegative(elevation_magnitude=abs(flow.elevation_change_m))
    if flow.phase != "two_phase":
        positive(viscosity_pa_s=flow.viscosity_pa_s)
    else:
        positive(erosional_c_us=flow.erosional_c_us)
        if (
            not isinstance(flow.erosional_c_basis, str)
            or not flow.erosional_c_basis.strip()
        ):
            raise ValueError("two-phase C choice requires erosional_c_basis")
    if flow.phase == "gas":
        positive(inlet_pressure_pa_abs=flow.inlet_pressure_pa_abs)
        if flow.elevation_change_m != 0:
            raise ValueError(
                "gas model is horizontal; nonzero elevation is outside scope"
            )


def _isothermal_outlet(
    p1: float, density: float, velocity: float, resistance: float
) -> float:
    """Subcritical root of the constant-T/Z momentum equation.

    p1²-p2² = G²*(p1/rho1)*(R+2 ln(p1/p2)), R=fL/D+K.
    G=rho1*v1. Public derivation/documentation: fluids 1.3.1,
    fluids.compressible.isothermal_gas, equation with explicit RT:
    https://fluids.readthedocs.io/fluids.compressible.html#fluids.compressible.isothermal_gas
    Here RT is replaced by p1/rho1 for constant effective Z and T.
    Local K is approximated as distributed equivalent resistance; fitting
    acceleration/vena-contracta choking is not resolved. The upper-pressure
    root is bracketed between isothermal critical p=sqrt(G²*RT) and p1.
    """
    b = (density * velocity) ** 2 * (p1 / density)
    critical = math.sqrt(b)
    if critical >= p1:
        raise ValueError("inlet reaches isothermal choking; increase candidate size")

    def residual(p2):
        return p1 * p1 - p2 * p2 - b * (resistance + 2 * math.log(p1 / p2))

    if residual(critical) <= 0:
        raise ValueError(
            "requested flow is choked or has no subcritical isothermal solution"
        )
    lo, hi = critical, p1
    for _ in range(100):
        mid = (lo + hi) / 2
        if residual(mid) > 0:
            lo = mid
        else:
            hi = mid
    return (lo + hi) / 2


def evaluate_line(
    flow: Flow, *, nps: float, schedule: str, velocity_limit_m_s: float
) -> LineResult:
    """Evaluate one NPS/schedule using continuity and Darcy-Weisbach.

    V=4Q/(pi D²), Re=rho VD/mu; dp=(f L/D+K)*rho V²/2+rho g dz.
    ASHRAE Fundamentals 2017 Ch.22, fluid-flow equations; f is Darcy,
    not Fanning. g=9.80665 m/s² (standard gravity, BIPM SI Brochure 9th ed.).
    Friction coefficients and validity: Swamee & Jain (1976), ASCE 102(5),
    657-664; see shared helper. Re 2300..5000 is marked transitional.

    Gas: when inlet-density dp/P1 >0.10, solve horizontal isothermal
    momentum including acceleration. The 10% rule is the explicit project
    switching policy, not an ASME requirement or an accuracy guarantee.
    Outlet velocity governs the velocity check even on the low-drop branch.
    Constant viscosity, temperature and Z are assumed. Inlet or calculated
    outlet isothermal choking is rejected. No acoustic-noise prediction.

    Two-phase: API RP 14E 1991 2.5(a) eq.(2.8), Ve=C/sqrt(rho_m) in
    ft/s and lb/ft³. The SI conversion is Ve=.3048*C/sqrt(rho/16.01846337396).
    Public research source: Sani et al. (2019), Wear 426-427, 620-636,
    DOI:10.1016/j.wear.2019.01.119. Continuous clean service commonly uses
    C=100, intermittent C=125; the caller MUST state the choice. This is a
    velocity screen only, not a two-phase pressure-drop/erosion-rate model.

    NPS/schedule dimensions reuse B36.10M catalog helpers. Public spot
    verification for NPS4/6 Sch40: Wheatland A53 Schedule40 submittal chart,
    https://www.wheatland.com/wp-content/uploads/2017/12/ASTM-A53-Schedule-40-Submittal-Sheet.pdf
    No product grade, pressure suitability or schedule adequacy is inferred.
    """
    _validate(flow, velocity_limit_m_s)
    refs = citations("hydraulics", *("erosion",) if flow.phase == "two_phase" else ())
    positive(nps=nps)
    if not isinstance(schedule, str) or not schedule.strip():
        raise ValueError("schedule must be a nonempty catalog label")
    wall = wt_for_schedule(nps, schedule) * 0.0254
    diameter = od_for_nps(nps) * 0.0254 - 2 * wall
    velocity = 4 * flow.flow_m3_s / (math.pi * diameter**2)
    notes = []
    if flow.phase == "two_phase":
        erosional = (
            0.3048
            * flow.erosional_c_us
            / math.sqrt(flow.density_kg_m3 / 16.01846337396)
        )
        notes.extend(
            (
                flow.erosional_c_basis,
                "No pressure-drop, slip, solids or erosion-corrosion model.",
            )
        )
        return LineResult(
            nps,
            schedule,
            diameter,
            wall,
            velocity,
            None,
            "not_calculated",
            None,
            None,
            None,
            "homogeneous_velocity_only",
            velocity_limit_m_s,
            erosional,
            velocity <= min(velocity_limit_m_s, erosional),
            tuple(notes),
            refs,
        )
    reynolds = flow.density_kg_m3 * velocity * diameter / flow.viscosity_pa_s
    relative_roughness = flow.roughness_m / diameter
    if relative_roughness > 0.01:
        raise ValueError("relative roughness exceeds Swamee-Jain screening range .01")
    friction = swamee_jain_friction_factor(reynolds, relative_roughness)
    regime = (
        "laminar"
        if reynolds < 2300
        else "transitional"
        if reynolds < 5000
        else "turbulent"
    )
    if regime == "transitional":
        notes.append(
            "Transition-band friction is approximate; detailed hydraulic verification required."
        )
    if reynolds > 3e8:
        raise ValueError("Re exceeds Swamee-Jain validity range 3e8")
    if reynolds >= 5000 and relative_roughness < 1e-6:
        notes.append(
            "Hydraulically smooth approximation outside stated relative-roughness fit range."
        )
    resistance = friction * flow.length_m / diameter + flow.fittings_k
    dp = resistance * flow.density_kg_m3 * velocity**2 / 2
    p2 = None
    method = "darcy_weisbach"
    if flow.phase == "liquid":
        dp += flow.density_kg_m3 * 9.80665 * flow.elevation_change_m
    else:
        p1 = flow.inlet_pressure_pa_abs
        method = "inlet_density"
        if dp / p1 > 0.10:
            p2 = _isothermal_outlet(p1, flow.density_kg_m3, velocity, resistance)
            dp = p1 - p2
            method = "isothermal"
        else:
            p2 = p1 - dp
        velocity *= p1 / p2
        # Isothermal sonic speed sqrt(RT); this also catches high-velocity
        # inlet cases with negligible resistance that bypass method switching.
        if velocity >= math.sqrt(p1 / flow.density_kg_m3):
            raise ValueError(
                "gas outlet reaches isothermal choking; increase candidate size"
            )
        notes.append(
            "Constant T/Z, horizontal gas; flow is actual at inlet; outlet velocity reported."
        )
        if flow.fittings_k:
            notes.append(
                "K is distributed equivalent resistance; local fitting choking is not resolved."
            )
    return LineResult(
        nps,
        schedule,
        diameter,
        wall,
        velocity,
        reynolds,
        regime,
        friction,
        dp,
        p2,
        method,
        velocity_limit_m_s,
        None,
        velocity <= velocity_limit_m_s,
        tuple(notes),
        refs,
    )


def size_line(
    flow: Flow,
    *,
    velocity_limit_m_s: float,
    schedules: tuple[str, ...] = ("40",),
    candidate_nps: tuple[float, ...] | None = None,
    maximum_pressure_drop_pa: float | None = None,
    minimum_wall_m: float = 0,
) -> LineResult:
    """Pick the smallest catalog NPS meeting supplied wall, velocity and dp limits.

    At each NPS, thinner eligible schedules are tried first (larger bore).
    Continuity gives a lower-bound ID sqrt(4Q/(pi Vlimit)); candidates are
    evaluated explicitly because gas outlet velocity and pressure budget
    may govern. No feasible candidate raises ValueError with rejection
    reasons. Missing catalog sizes/schedules are skipped, never fabricated.
    A required minimum wall may come from pressure_thickness; this picker
    does not independently establish the material's B31.3 design strength.
    Two-phase pressure-drop constraints are rejected, not silently ignored.
    """
    _validate(flow, velocity_limit_m_s)
    nonnegative(minimum_wall_m=minimum_wall_m)
    if maximum_pressure_drop_pa is not None:
        positive(maximum_pressure_drop_pa=maximum_pressure_drop_pa)
        if flow.phase == "two_phase":
            raise ValueError(
                "two-phase pressure-drop sizing is outside the velocity-only model"
            )
    if not schedules or any(not isinstance(s, str) or not s.strip() for s in schedules):
        raise ValueError("at least one nonempty schedule is required")
    sizes = list_nps() if candidate_nps is None else candidate_nps
    for nps in sizes:
        positive(nps=nps)
    rejected = []
    for nps in sorted(set(sizes)):
        candidates = []
        for schedule in schedules:
            try:
                wall = wt_for_schedule(nps, schedule) * 0.0254
            except KeyError:
                continue
            candidates.append((wall, schedule))
        for wall, schedule in sorted(candidates):
            if wall < minimum_wall_m:
                continue
            try:
                result = evaluate_line(
                    flow,
                    nps=nps,
                    schedule=schedule,
                    velocity_limit_m_s=velocity_limit_m_s,
                )
            except ValueError as error:
                rejected.append(f"{nps:g}/{schedule}: {error}")
                continue
            if result.within_velocity_limit and (
                maximum_pressure_drop_pa is None
                or result.pressure_drop_pa <= maximum_pressure_drop_pa
            ):
                return result
            rejected.append(f"{nps:g}/{schedule}: velocity or pressure budget exceeded")
    raise ValueError("no candidate meets supplied criteria; " + "; ".join(rejected))
