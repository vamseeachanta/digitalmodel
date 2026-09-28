"""Text specification of a drilling-riser global OrcaFlex model.

All quantities are SI (m, kg, N, Pa). Elevations are global z, positive up, MSL = 0.
The model topology is fixed:

* ``inner_barrel``: sections from the upper flex-joint pivot (connected to the vessel
  with the upper flex-joint rotational stiffness) down to the tension ring. Its lower end
  rides in the ring on a constraint free along z (the telescopic joint), so it passes
  shear and moment into the ring but no axial load beyond a small stabilising spring.
* ``riser``: sections from the tension ring down to the lower flex-joint pivot.
* ``stack``: sections from the lower flex-joint pivot down to the wellhead datum,
  where the stack is fixed.

Each :class:`LineSection` gives the structural mass per metre *excluding* the fluid in
its main bore; that fluid is the line contents (``Contents``) over ``bore_id_m``. The
displaced volume per metre sets buoyancy and added mass (OrcaFlex uses the equivalent
diameter), independently of the drag diameter.
"""

from __future__ import annotations

import math
from typing import Any, Literal

from pydantic import AliasChoices, BaseModel, Field, model_validator

LENGTH_TOL_M = 1.0e-3


def tube_section(*, od_m: float, wall_m: float, youngs_modulus_pa: float) -> dict[str, float]:
    """Area, EA and EI of a plain tube."""
    if od_m <= 0 or wall_m <= 0 or 2 * wall_m >= od_m:
        raise ValueError(f"invalid tube: OD {od_m} m, wall {wall_m} m")
    idm = od_m - 2 * wall_m
    area = math.pi / 4 * (od_m**2 - idm**2)
    inertia = math.pi / 64 * (od_m**4 - idm**4)
    return {"id_m": idm, "area_m2": area, "ea_n": youngs_modulus_pa * area,
            "ei_nm2": youngs_modulus_pa * inertia}


class LineSection(BaseModel):
    """One homogeneous section of a line (becomes one OrcaFlex line type)."""

    name: str = Field(..., min_length=1)
    length_m: float = Field(..., gt=0)
    segment_length_m: float = Field(..., gt=0)
    mass_per_m_kg: float = Field(..., ge=0, description="structural mass, excluding main-bore contents")
    displaced_volume_per_m_m3: float = Field(..., ge=0)
    bore_id_m: float = Field(0.0, ge=0, description="main bore carrying the line contents; 0 = none")
    ei_nm2: float = Field(..., gt=0)
    ea_n: float = Field(..., gt=0)
    gj_nm2: float | None = Field(None, gt=0)
    drag_diameter_m: float = Field(..., ge=0)
    cd_normal: float = Field(..., ge=0)
    cd_axial: float = Field(0.0, ge=0)
    ca_normal: float = Field(..., ge=0)
    ca_axial: float = Field(0.0, ge=0)
    stress_od_m: float | None = Field(None, gt=0)
    stress_id_m: float | None = Field(None, ge=0)
    source: dict[str, Any] = Field(default_factory=dict, description="provenance per field (free form)")

    @model_validator(mode="after")
    def _bore_inside_body(self) -> "LineSection":
        if self.bore_id_m > 0:
            eq = math.sqrt(4 * self.displaced_volume_per_m_m3 / math.pi)
            if self.bore_id_m >= eq:
                raise ValueError(
                    f"section {self.name!r}: bore {self.bore_id_m:.4f} m is not inside the "
                    f"buoyancy-equivalent diameter {eq:.4f} m")
        return self


class FlexJoint(BaseModel):
    """Flex-joint hinge at a pivot.

    ``rotational_stiffness_nm_per_rad`` is the small-rotation stiffness (used by the hand checks
    and, for a linear joint, by the model). ``moment_rotation_deg_nm`` makes the joint nonlinear:
    points (rotation in degrees, moment in N.m) from (0, 0), both strictly increasing; its first
    segment slope must equal the small-rotation stiffness. OrcaFlex extrapolates the last segment.
    """

    pivot_z_m: float
    rotational_stiffness_nm_per_rad: float = Field(..., gt=0)
    moment_rotation_deg_nm: list[tuple[float, float]] | None = None

    @model_validator(mode="after")
    def _curve(self) -> "FlexJoint":
        c = self.moment_rotation_deg_nm
        if c is None:
            return self
        if len(c) < 2 or tuple(c[0]) != (0.0, 0.0):
            raise ValueError("flex-joint moment-rotation curve must start at (0, 0) and have two or more points")
        for (a0, m0), (a1, m1) in zip(c, c[1:]):
            if not (a1 > a0 and m1 > m0):
                raise ValueError("flex-joint moment-rotation curve must be strictly increasing in rotation and moment")
        k0 = c[1][1] / c[1][0] * 180.0 / math.pi
        if abs(k0 - self.rotational_stiffness_nm_per_rad) > 1e-6 * k0:
            raise ValueError(
                f"small-rotation stiffness {self.rotational_stiffness_nm_per_rad:.6g} N.m/rad is not the first "
                f"segment slope of the curve ({k0:.6g} N.m/rad)")
        return self


def flex_joint_from_secants(*, pivot_z_m: float, secants_nm_per_deg: list[tuple[float, float]]) -> FlexJoint:
    """Nonlinear flex joint from secant stiffnesses (N.m/deg) stated at rotations (deg)."""
    pts = [(0.0, 0.0)] + [(float(a), float(a) * float(k)) for a, k in sorted(secants_nm_per_deg)]
    k0 = pts[1][1] / pts[1][0] * 180.0 / math.pi
    return FlexJoint(pivot_z_m=pivot_z_m, rotational_stiffness_nm_per_rad=k0, moment_rotation_deg_nm=pts)


class TensionRing(BaseModel):
    mass_kg: float = Field(..., ge=0)
    volume_m3: float = Field(0.0, ge=0)
    moments_of_inertia_kgm2: tuple[float, float, float]
    z_static_m: float = Field(..., description="expected static elevation, used for the tensioner-line angle")


class Tensioners(BaseModel):
    """Riser tensioners.

    ``representation``: ``"lines"`` - ``count`` constant-tension lines from sheaves on the vessel to
    the ring (their inclination and the pendulum stiffness T/L tie the ring laterally to the vessel);
    ``"vertical_force"`` - the equivalent string: the telescopic joint is locked, the string top (UFJ
    pivot) is pinned laterally to the vessel and free along z, and a vertical constant tension (the
    tensioner force plus the inner-barrel weight) acts there, so there is no tensioner tie at the ring
    (the sheave geometry is unused; the ring is tied only through the string tension over the inner
    barrel). The two bound the split of rotation between the upper and lower flex joints.
    """

    representation: Literal["lines", "vertical_force"] = "lines"
    count: int = Field(..., ge=1)
    sheave_radius_m: float = Field(..., gt=0)
    sheave_z_m: float
    ring_attach_radius_m: float = Field(..., ge=0)
    total_vertical_tension_n: float = Field(..., gt=0)
    first_azimuth_deg: float = 0.0
    failed_count: int = Field(0, ge=0, description="failed tensioners (API RP 16Q n): the first ``failed_count`` lines "
                                                   "from ``first_azimuth_deg`` are removed; the others keep the intact "
                                                   "line tension, so the vertical force is (count - n) / count of the total")
    wire_stiffness_n: float = Field(1.0e9, gt=0, description="winch wire EA (OrcaFlex 'Stiffness')")
    rated_tension_each_n: float | None = Field(
        None, gt=0, validation_alias=AliasChoices("rated_tension_each_n", "rated_tension_n_each"), description="rated (dynamic tension limit) capacity per tensioner; recorded for capacity "
                                "checks and checked against the applied line tension")

    @model_validator(mode="after")
    def _failed(self) -> "Tensioners":
        if self.failed_count >= self.count:
            raise ValueError(f"failed_count {self.failed_count} must leave at least one of {self.count} tensioners")
        return self


class Contents(BaseModel):
    density_kg_m3: float = Field(..., ge=0)
    pressure_ref_z_m: float = Field(..., description="z of the free surface of the contents (gauge 0)")


class GlobalEnvironment(BaseModel):
    water_depth_m: float = Field(..., gt=0)
    water_density_kg_m3: float = Field(..., gt=0)
    seabed_normal_stiffness_kn_m_m2: float = 100.0
    seabed_shear_stiffness_kn_m_m2: float = 100.0


DOFS = ("surge", "sway", "heave", "roll", "pitch", "yaw")


class DisplacementRAOs(BaseModel):
    """Vessel displacement RAOs per unit wave amplitude (m/m for surge, sway, heave; deg/m for
    roll, pitch, yaw), phases in degrees relative to the wave crest at the RAO origin.

    Conventions are those of the model's vessel axes (x forward, y port, z up; roll positive
    starboard down, pitch positive bow down, yaw positive bow to port). ``directions_deg`` are wave
    propagation directions relative to the vessel x axis (0 = waves travelling bow-ward).
    ``amplitude[i][j]`` and ``phase_deg[i][j]`` are the six DOF values at direction i, frequency j.
    """

    phase_convention: str = Field(..., pattern="^(leads|lags)$")
    directions_deg: list[float] = Field(..., min_length=1)
    frequencies_rad_s: list[float] = Field(..., min_length=2)
    amplitude: list[list[list[float]]]
    phase_deg: list[list[list[float]]]

    @model_validator(mode="after")
    def _shape(self) -> "DisplacementRAOs":
        nd, nf = len(self.directions_deg), len(self.frequencies_rad_s)
        if sorted(set(self.directions_deg)) != self.directions_deg:
            raise ValueError("RAO directions must be unique and ascending")
        if any(b <= a for a, b in zip(self.frequencies_rad_s, self.frequencies_rad_s[1:])):
            raise ValueError("RAO frequencies must be strictly increasing")
        for name, arr in (("amplitude", self.amplitude), ("phase_deg", self.phase_deg)):
            if len(arr) != nd or any(len(r) != nf or any(len(v) != 6 for v in r) for r in arr):
                raise ValueError(f"RAO {name} must be [direction][frequency][6]")
        return self


class VesselMotion(BaseModel):
    """First-order vessel motion from displacement RAOs (the vessel type is named by its class id only)."""

    class_id: str = Field(..., min_length=1, description="vessel_db class id; never a vessel name")
    rao_origin_m: tuple[float, float, float] = Field(..., description="RAO origin in vessel axes (m)")
    raos: DisplacementRAOs
    provenance: dict[str, Any] = Field(default_factory=dict)


class RegularWave(BaseModel):
    height_m: float = Field(..., gt=0)
    period_s: float = Field(..., gt=0)
    direction_deg: float = 0.0


class IrregularWave(BaseModel):
    """JONSWAP sea state (OrcaFlex 'Partially specified': Hs, Tp and gamma) with a fixed random seed,
    so each seed of a multi-seed case is a reproducible text model."""

    hs_m: float = Field(..., gt=0)
    tp_s: float = Field(..., gt=0)
    gamma: float = Field(..., ge=1.0, le=7.0)
    direction_deg: float = 0.0
    seed: int = Field(..., ge=0)
    number_of_components: int = Field(200, ge=10)


class StructuralDamping(BaseModel):
    """Stiffness-proportional Rayleigh damping on every line type, ``ratio_percent`` of critical at
    ``period_s`` (classical coefficients: mass 0, stiffness beta = 2 zeta / omega, applied to the
    geometric stiffness too), so zeta(T) = ratio x period_s / T: less at longer periods."""

    ratio_percent: float = Field(..., gt=0)
    period_s: float = Field(..., gt=0)
    name: str = Field("Structural", min_length=1)

    @property
    def stiffness_coefficient_s(self) -> float:
        return 2.0 * self.ratio_percent / 100.0 / (2.0 * math.pi / self.period_s)


class CurrentProfile(BaseModel):
    """Depth-varying current: (depth below MSL in m, speed in m/s) pairs, depth increasing; the
    speed is held beyond the last depth. ``direction_deg`` is the direction the current flows
    towards, measured from the global x axis."""

    direction_deg: float = 0.0
    depth_speed_m_s: list[tuple[float, float]] = Field(..., min_length=2)

    @model_validator(mode="after")
    def _profile(self) -> "CurrentProfile":
        d = [p[0] for p in self.depth_speed_m_s]
        if d[0] < 0 or any(b <= a for a, b in zip(d, d[1:])):
            raise ValueError("current profile depths must be >= 0 and strictly increasing")
        if any(p[1] < 0 for p in self.depth_speed_m_s):
            raise ValueError("current speeds must be >= 0")
        if max(p[1] for p in self.depth_speed_m_s) <= 0:
            raise ValueError("current profile has no non-zero speed")
        return self


class Dynamics(BaseModel):
    time_step_s: float = Field(0.1, gt=0)
    build_up_s: float = Field(..., gt=0)
    duration_s: float = Field(..., gt=0)


class PYCurve(BaseModel):
    """Lateral soil resistance p (N/m) against displacement y (m) at a depth below the mudline."""

    depth_m: float = Field(..., ge=0)
    y_m: list[float] = Field(..., min_length=2)
    p_n_per_m: list[float] = Field(..., min_length=2)

    @model_validator(mode="after")
    def _curve(self) -> "PYCurve":
        if len(self.y_m) != len(self.p_n_per_m):
            raise ValueError("p-y curve: y and p must have the same length")
        if self.y_m[0] != 0.0 or self.p_n_per_m[0] != 0.0:
            raise ValueError("p-y curve must start at (0, 0)")
        if any(b <= a for a, b in zip(self.y_m, self.y_m[1:])):
            raise ValueError("p-y curve: y must be strictly increasing")
        if any(b < a for a, b in zip(self.p_n_per_m, self.p_n_per_m[1:])):
            raise ValueError("p-y curve: p must not decrease")
        return self


class Foundation(BaseModel):
    """Conductor/casing below the wellhead datum with lateral p-y springs; fixed at its base.

    ``sections`` run top (datum) to bottom. Springs sit at every node except the fixed base, with
    the tributary length of each node; each is a pair of horizontal spring links (x and y) to a
    fixed anchor ``anchor_offset_m`` away, long enough that lateral motion does not rotate them.
    """

    sections: list[LineSection] = Field(..., min_length=1)
    py_curves: list[PYCurve] = Field(..., min_length=1)
    anchor_offset_m: float = Field(100.0, gt=0)
    far_displacement_m: float = Field(10.0, gt=0, validation_alias=AliasChoices("far_displacement_m", "far_extension_m"),
                                      description="p held flat to this extra displacement")
    flooded_density_kg_m3: float | None = Field(
        None, gt=0, description="flooded conductor: fluid of this density fills the section bores (bore_id_m) at "
                                "hydrostatic pressure from the sea surface; None = no contents (bore fluid, if any, "
                                "lumped in the mass and no internal pressure in the stress recovery)")
    provenance: dict[str, Any] = Field(default_factory=dict)

    @model_validator(mode="after")
    def _mesh(self) -> "Foundation":
        for s in self.sections:
            n = s.length_m / s.segment_length_m
            if abs(n - round(n)) > 1e-6:
                raise ValueError(f"foundation section {s.name!r}: length must be a whole number of segments")
        d = [c.depth_m for c in self.py_curves]
        if any(b < a for a, b in zip(d, d[1:])):
            raise ValueError("p-y curves must be ordered by depth")
        return self

    @property
    def depth_m(self) -> float:
        return sum(s.length_m for s in self.sections)


class HangOff(BaseModel):
    """The riser disconnected at the LMRP connector and hung off (``hang_off.hang_off_spec``): ``hard`` - telescopic
    joint locked, no tensioners, the string on the vessel at the upper flex joint; ``soft`` - the string on a vertical
    gas spring at the ring (``spring_*``) with the telescopic joint stroking. ``with_lmrp``: the stack line (the LMRP,
    or the running payload) hangs free below the lower flex joint; without it the riser end is free."""

    mode: Literal["hard", "soft"]
    with_lmrp: bool = True
    spring_tension_n: float | None = Field(None, gt=0, description="soft: spring force at the static ring position")
    spring_stiffness_n_per_m: float | None = Field(None, gt=0)
    stiffness_basis: str = ""
    running: dict[str, Any] | None = None

    @model_validator(mode="after")
    def _soft(self) -> "HangOff":
        if self.mode == "soft" and (self.spring_tension_n is None or self.spring_stiffness_n_per_m is None):
            raise ValueError("a soft hang-off needs spring_tension_n and spring_stiffness_n_per_m")
        return self


class VesselTrajectory(BaseModel):
    """Prescribed low-frequency vessel motion from the start of the main stage (drift-off / drive-off): positions
    relative to the static offset; held at the offset through the build-up. First-order RAO motion is superimposed."""

    t_s: list[float] = Field(..., min_length=2)
    x_m: list[float] = Field(..., min_length=2)
    y_m: list[float] = Field(..., min_length=2)

    @model_validator(mode="after")
    def _rows(self) -> "VesselTrajectory":
        if not (len(self.t_s) == len(self.x_m) == len(self.y_m)):
            raise ValueError("t_s, x_m and y_m must have the same length")
        if any(b <= a for a, b in zip(self.t_s, self.t_s[1:])) or self.t_s[0] != 0.0:
            raise ValueError("t_s must start at 0 and increase")
        return self


class Recoil(BaseModel):
    """EDS disconnect at the LMRP connector at the start of stage 1: the LMRP (the top stack section) is its own line,
    released from the BOP top; the tensioner total vertical tension follows ``stages`` (duration, tension; the last
    stage open-ended), the anti-recoil schedule (``events.anti_recoil_stages``)."""

    stages: list[dict[str, Any]] = Field(..., min_length=1)
    basis: str = ""


class RiserGlobalModelSpec(BaseModel):
    name: str = Field(..., min_length=1)
    description: str = ""
    environment: GlobalEnvironment
    contents: Contents
    upper_flex_joint: FlexJoint
    lower_flex_joint: FlexJoint
    tension_ring: TensionRing
    tensioners: Tensioners
    inner_barrel: list[LineSection] = Field(..., min_length=1)
    riser: list[LineSection] = Field(..., min_length=1)
    stack: list[LineSection] = Field(..., min_length=1)
    wellhead_datum_z_m: float
    vessel_name: str = "Vessel"
    slip_joint_axial_stiffness_n_per_m: float = Field(
        1.0e3, ge=0, description="axial spring on the telescopic-joint constraint; a small value keeps "
                                 "statics on the physical branch and carries only k x ring rise")
    vessel_motion: VesselMotion | None = None
    vessel_offset_m: tuple[float, float] = Field(
        (0.0, 0.0), description="static vessel offset (x, y) from the well centre, m; the vessel is not in "
                                "statics, so it holds this position")
    current: CurrentProfile | None = None
    structural_damping: StructuralDamping | None = None
    regular_wave: RegularWave | None = None
    irregular_wave: IrregularWave | None = None
    dynamics: Dynamics | None = None
    foundation: Foundation | None = None
    hang_off: HangOff | None = None
    vessel_trajectory: VesselTrajectory | None = None
    recoil: Recoil | None = None
    wave_time_origin_s: float = Field(0.0, description="regular wave time origin (the wave phase at t = 0)")
    provenance: dict[str, Any] = Field(default_factory=dict)

    @property
    def ring_z_geometric_m(self) -> float:
        """Unstretched tension-ring elevation (upper pivot minus the inner-barrel line)."""
        return self.upper_flex_joint.pivot_z_m - sum(s.length_m for s in self.inner_barrel)

    @model_validator(mode="after")
    def _geometry_closes(self) -> "RiserGlobalModelSpec":
        names = [s.name for s in (*self.inner_barrel, *self.riser, *self.stack)]
        dup = {n for n in names if names.count(n) > 1}
        if dup:
            raise ValueError(f"section names must be unique (line-type names): {sorted(dup)}")
        z_lfj = self.ring_z_geometric_m - sum(s.length_m for s in self.riser)
        if abs(z_lfj - self.lower_flex_joint.pivot_z_m) > LENGTH_TOL_M:
            raise ValueError(
                f"riser sections end at z = {z_lfj:.4f} m, not at the lower flex-joint pivot "
                f"{self.lower_flex_joint.pivot_z_m:.4f} m")
        z_datum = z_lfj - sum(s.length_m for s in self.stack)
        if abs(z_datum - self.wellhead_datum_z_m) > LENGTH_TOL_M:
            raise ValueError(
                f"stack sections end at z = {z_datum:.4f} m, not at the wellhead datum "
                f"{self.wellhead_datum_z_m:.4f} m")
        if self.wellhead_datum_z_m < -self.environment.water_depth_m - LENGTH_TOL_M:
            raise ValueError("wellhead datum is below the seabed")
        if self.regular_wave is not None and self.irregular_wave is not None:
            raise ValueError("give a regular wave or an irregular wave, not both")
        if self.tensioners.sheave_z_m <= self.tension_ring.z_static_m:
            raise ValueError("tensioner sheaves must be above the tension ring")
        t = self.tensioners
        if t.rated_tension_each_n is not None:
            if t.representation == "vertical_force":
                per_line = t.total_vertical_tension_n / t.count
            else:
                dx = t.sheave_radius_m - t.ring_attach_radius_m
                dz = t.sheave_z_m - self.tension_ring.z_static_m
                per_line = t.total_vertical_tension_n / (t.count * dz / math.hypot(dx, dz))
            if per_line > t.rated_tension_each_n:
                raise ValueError(f"tensioner line tension {per_line:.4g} N exceeds the rated {t.rated_tension_each_n:.4g} N")
        return self
