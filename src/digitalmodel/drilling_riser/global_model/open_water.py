"""Open-water completion / workover riser (the C2 topology): spec, OrcaFlex text model and hand checks.

Topology (global z up, MSL = 0; all SI):

* ``TensionFrame`` - a 3D buoy at the string top (tension frame with the surface tree lumped in), pulled up by a
  vertical constant-tension winch ``TopTensioner`` anchored ``anchor_height_m`` above it in the vessel frame (the
  rig tensioners carry the frame; the force follows the vessel).
* line ``Upper`` - sections from the frame down to the rotary (the upper tension joint).
* constraint ``Rotary`` - on the vessel at ``rotary_z_m``: the string is held laterally there and is free along z
  and in rotation (no moment and no vertical load at the rotary).
* line ``Riser`` - sections from the rotary down to the EDP / LRP interface: the rest of the tension joint, the
  riser joints, the stress joint and, as its last section, the EDP body. End B connects rigidly to the stack; an
  EDP release (``edp_release``) frees it at the start of the main stage.
* line ``Stack`` - sections from the interface down to the wellhead datum (LRP, tree, spool, wellhead housing),
  written bottom-up and fixed at the datum, or on the conductor p-y foundation (``foundation``). The well bore is
  continuous through the stack: a section with a nonzero ``bore_id_m`` carries the riser contents (density and
  pressure) in the solver exactly as the hand checks (``tension_references``, ``beam_model``) count them.

The vertical tensioner force follows the vessel with no lateral tie at the frame (only the anchor-height pendulum
stiffness T / h); the lateral tie of the string to the vessel is the rotary.
"""

from __future__ import annotations

import math
from typing import Any, Literal

from pydantic import BaseModel, Field, model_validator

from .hand_checks import BeamSegment, effective_tension_chain, submerged_length_m, tensioned_beam_periods
from .spec import (
    LENGTH_TOL_M,
    Contents,
    CurrentProfile,
    Dynamics,
    Foundation,
    GlobalEnvironment,
    IrregularWave,
    LineSection,
    RegularWave,
    StructuralDamping,
    VesselMotion,
)

G = 9.80665
FRAME = "TensionFrame"
ROTARY = "Rotary"
ROTARY_BODY = "RotaryBody"
TOP_WINCH = "TopTensioner"


class TensionFrame(BaseModel):
    """Tension frame with the surface equipment lumped in (flow tree, frame); in air at the string top."""

    mass_kg: float = Field(..., gt=0)
    z_m: float = Field(..., description="elevation of the string top (the frame connection), m")


class TopTension(BaseModel):
    """The rig tensioners as one vertical constant tension at the frame."""

    total_vertical_tension_n: float = Field(..., gt=0)
    anchor_height_m: float = Field(1000.0, gt=0, description="winch anchor above the frame, vessel frame")
    wire_stiffness_n: float = Field(1.0e9, gt=0)
    rated_total_n: float | None = Field(None, gt=0, description="tension-system rating, checked against the tension")


class EdpRelease(BaseModel):
    """EDP disconnect: the Riser end B (EDP / LRP interface) is released at the start of the main stage (stage 1)
    and the tensioner tension steps to ``anti_recoil_tension_n`` (the anti-recoil response idealised as a step;
    ``None`` keeps the tension unchanged)."""

    anti_recoil_tension_n: float | None = Field(None, gt=0)


class OpenWaterRiserSpec(BaseModel):
    kind: Literal["open_water"] = "open_water"
    name: str = Field(..., min_length=1)
    description: str = ""
    environment: GlobalEnvironment
    contents: Contents
    tension_frame: TensionFrame
    tensioners: TopTension
    rotary_z_m: float
    rotary_mass_kg: float = Field(10.0, gt=0, description="small lumped body on the rotary constraint (numerical: "
                                  "gives its free rotations inertia; without it the implicit solution is unstable)")
    rotary_inertia_kgm2: float = Field(10.0, gt=0)
    upper: list[LineSection] = Field(..., min_length=1)
    riser: list[LineSection] = Field(..., min_length=2)
    stack: list[LineSection] = Field(..., min_length=1)
    wellhead_datum_z_m: float
    vessel_name: str = "Vessel"
    vessel_motion: VesselMotion | None = None
    vessel_offset_m: tuple[float, float] = (0.0, 0.0)
    current: CurrentProfile | None = None
    structural_damping: StructuralDamping | None = None
    regular_wave: RegularWave | None = None
    irregular_wave: IrregularWave | None = None
    dynamics: Dynamics | None = None
    foundation: Foundation | None = None
    edp_release: EdpRelease | None = None
    provenance: dict[str, Any] = Field(default_factory=dict)

    @property
    def edp_interface_z_m(self) -> float:
        return self.rotary_z_m - sum(s.length_m for s in self.riser)

    @property
    def stress_joint_base_arc_m(self) -> float:
        """Arc length on line Riser of the EDP top (the stress-joint base): the last riser section is the EDP."""
        return sum(s.length_m for s in self.riser[:-1])

    @model_validator(mode="after")
    def _closes(self) -> "OpenWaterRiserSpec":
        names = [s.name for s in (*self.upper, *self.riser, *self.stack)]
        dup = {n for n in names if names.count(n) > 1}
        if dup:
            raise ValueError(f"section names must be unique (line-type names): {sorted(dup)}")
        z_rot = self.tension_frame.z_m - sum(s.length_m for s in self.upper)
        if abs(z_rot - self.rotary_z_m) > LENGTH_TOL_M:
            raise ValueError(f"upper sections end at z = {z_rot:.4f} m, not at the rotary {self.rotary_z_m:.4f} m")
        z_datum = self.edp_interface_z_m - sum(s.length_m for s in self.stack)
        if abs(z_datum - self.wellhead_datum_z_m) > LENGTH_TOL_M:
            raise ValueError(f"stack sections end at z = {z_datum:.4f} m, not at the wellhead datum "
                             f"{self.wellhead_datum_z_m:.4f} m")
        if self.wellhead_datum_z_m < -self.environment.water_depth_m - LENGTH_TOL_M:
            raise ValueError("wellhead datum is below the seabed")
        if self.regular_wave is not None and self.irregular_wave is not None:
            raise ValueError("give a regular wave or an irregular wave, not both")
        t = self.tensioners
        if t.rated_total_n is not None:
            # every commanded tension: the initial (all stages) and the EDP release (anti-recoil) step
            commanded = [("top tension", t.total_vertical_tension_n)]
            if self.edp_release is not None and self.edp_release.anti_recoil_tension_n is not None:
                commanded.append(("EDP release (anti-recoil) tension", self.edp_release.anti_recoil_tension_n))
            for label, value in commanded:
                if value > t.rated_total_n:
                    raise ValueError(f"{label} {value:.4g} N exceeds the rated {t.rated_total_n:.4g} N")
        return self


# ---------------------------------------------------------------- text model
def build_open_water_generic_spec(spec: OpenWaterRiserSpec) -> dict[str, Any]:
    """The modular-generator ProjectInputSpec (``generic:``) for the open-water riser."""
    from . import build as b

    vessel = spec.vessel_name
    rho_c = spec.contents.density_kg_m3 / 1000.0
    ref_z = spec.contents.pressure_ref_z_m
    p_kpa = spec.contents.pressure_pa / 1000.0
    inf = "Infinity"
    f = spec.foundation
    stack_base = (["Conductor", 0, 0, 0, 0, 0, 0, None, "End B"] if f is not None
                  else ["Fixed", 0, 0, spec.wellhead_datum_z_m, 0, 0, 0, None, None])
    release = 1 if spec.edp_release is not None else None
    lines = [
        b._line("Upper", [[FRAME, 0, 0, 0, 0, 180, 0, None, None], [ROTARY, 0, 0, 0, 0, 180, 0, None, None]],
                [[0.0, None], [inf, None]], spec.upper, rho_c, ref_z, pressure_kpa=p_kpa),
        b._line("Riser", [[ROTARY, 0, 0, 0, 0, 180, 0, None, None], ["Stack", 0, 0, 0, 0, 180, 0, release, "End B"]],
                [[inf, None], [inf, None]], spec.riser, rho_c, ref_z, pressure_kpa=p_kpa),
        b._line("Stack", [stack_base, ["Free", 0, 0, spec.edp_interface_z_m, 0, 0, 0, None, None]],
                [[inf, None], []], list(reversed(spec.stack)), rho_c, ref_z, pressure_kpa=p_kpa),
    ]
    extra_lines, links = b._foundation_objects(spec, ref_z)
    lines += extra_lines
    t = spec.tensioners
    tension_kn = t.total_vertical_tension_n / 1000.0
    stage_values = [tension_kn] * (len(b.STAGES_S) + 1)
    if spec.edp_release is not None and spec.edp_release.anti_recoil_tension_n is not None:
        stage_values[-1] = spec.edp_release.anti_recoil_tension_n / 1000.0
    ox, oy = spec.vessel_offset_m
    winch = {"name": TOP_WINCH, "properties": {
        "WinchType": "Simple",
        b.WINCH_CONN_KEY: [[vessel, 0, 0, spec.tension_frame.z_m + t.anchor_height_m], [FRAME, 0, 0, 0]],
        "Stiffness": t.wire_stiffness_n / 1000.0, "Damping": 0, "WinchControlType": "By stage",
        "StageMode, StageValue": [["Specified tension", v] for v in stage_values]}}
    rotary = {"name": ROTARY, "in_frame_connection": vessel, "constraint_type": "Calculated DOFs", "properties": {
        "InFrameInitialPosition": [0, 0, spec.rotary_z_m], "InFrameInitialAttitude": [0, 0, 0],
        "DOFFree, DOFInitialValue": [[False], [False], [True, 0.0], [True, 0.0], [True, 0.0], [False]],
        "StiffnessAndDampingMethod": "Coefficients", "TranslationalStiffness": 0.0, "RotationalStiffness": 0.0}}
    frame = {"name": FRAME, "connection": "Free", "initial_position": [ox, oy, spec.tension_frame.z_m],
             "mass": spec.tension_frame.mass_kg / 1000.0, "volume": 0.0,
             "properties": {"Height": 1.0}}
    vt = b._vessel_type(spec.vessel_motion, f"{vessel} type")
    below = [] if f is None else f.sections
    line_types = [*(b._line_type(s) for s in (*spec.upper, *spec.riser)),
                  *(b._line_type(s, contact_diameter_m=b.MIN_OD_M) for s in (*spec.stack, *below))]
    sd = spec.structural_damping
    if sd is not None:
        for lt in line_types:
            lt["properties"]["RayleighDampingCoefficients"] = sd.name
    generic: dict[str, Any] = {
        "line_types": line_types, "vessel_types": [vt], "vessels": [b._vessel(spec, vt)], "lines": lines,
        "buoys_3d": [frame], "constraints": [rotary], "winches": [winch],
        # a free rotation of a constraint carries no inertia of its own: a small lumped body on the rotary gives it
        # some (without it the C2 model went unstable in calm water at time steps above 0.005 s)
        "buoys_6d": [{"name": ROTARY_BODY, "buoy_type": "Lumped buoy", "connection": ROTARY,
                      "initial_position": [0, 0, 0], "mass": spec.rotary_mass_kg / 1000.0, "volume": 0.0,
                      "properties": {"InitialAttitude": [0, 0, 0], "CentreOfMass": [0, 0, 0], "Height": 1.0,
                                     "CentreOfVolume": [0, 0, 0],
                                     "MomentsOfInertia": [spec.rotary_inertia_kgm2 / 1000.0] * 3}}],
    }
    if links:
        generic["links"] = links
    if sd is not None:
        generic["rayleigh_damping"] = b._rayleigh(sd)
    env, sim = b._environment_and_simulation(spec)
    return {"metadata": {"name": spec.name, "description": spec.description or spec.name,
                         "structure": "riser", "operation": "completion"},
            "environment": env, "simulation": sim, "generic": generic}


# ---------------------------------------------------------------- hand checks
def frame_weight_n(spec: OpenWaterRiserSpec) -> float:
    return spec.tension_frame.mass_kg * G  # in air


def tension_references(spec: OpenWaterRiserSpec) -> dict[str, Any]:
    """Closed-form static effective tensions of the vertical string (no offset, no current)."""
    rho_w, rho_c = spec.environment.water_density_kg_m3, spec.contents.density_kg_m3
    wf = frame_weight_n(spec)
    top = spec.tensioners.total_vertical_tension_n - wf
    upper = effective_tension_chain(spec.upper, top_z_m=spec.tension_frame.z_m, top_tension_n=top,
                                    rho_water=rho_w, rho_contents=rho_c)
    w_rot = spec.rotary_mass_kg * G  # carried through the rotary (free along z) by the string
    riser = effective_tension_chain(spec.riser, top_z_m=spec.rotary_z_m, top_tension_n=upper[-1]["te_bottom_n"] - w_rot,
                                    rho_water=rho_w, rho_contents=rho_c)
    stack = effective_tension_chain(spec.stack, top_z_m=spec.edp_interface_z_m, top_tension_n=riser[-1]["te_bottom_n"],
                                    rho_water=rho_w, rho_contents=rho_c)
    return {
        "frame_weight_n": wf,
        "upper_top_n": top,
        "rotary_n": upper[-1]["te_bottom_n"],
        "stress_joint_base_n": riser[-2]["te_bottom_n"],
        "edp_bottom_n": riser[-1]["te_bottom_n"],
        "stack_bottom_n": stack[-1]["te_bottom_n"],
        "submerged_weight_n": top - stack[-1]["te_bottom_n"],
        "released_weight_n": spec.tensioners.total_vertical_tension_n - riser[-1]["te_bottom_n"],
        "upper_chain": upper, "riser_chain": riser, "stack_chain": stack,
    }


def _split(section: LineSection, max_len: float) -> int:
    return max(1, math.ceil(section.length_m / max_len - 1e-9))


def beam_model(spec: OpenWaterRiserSpec, *, max_element_m: float = 1.0) -> tuple[list[BeamSegment], int]:
    """Beam segments from the tension frame (node 0) to the wellhead datum; returns the rotary node number."""
    rho_w, rho_c = spec.environment.water_density_kg_m3, spec.contents.density_kg_m3
    ref = tension_references(spec)
    segs: list[BeamSegment] = []
    rotary_node = 0
    groups = ((spec.upper, ref["upper_chain"], spec.tension_frame.z_m),
              (spec.riser, ref["riser_chain"], spec.rotary_z_m),
              (spec.stack, ref["stack_chain"], spec.edp_interface_z_m))
    for gi, (sections, chain, z) in enumerate(groups):
        if gi == 1:
            rotary_node = len(segs)
        for s, link in zip(sections, chain):
            n = _split(s, max_element_m)
            le = s.length_m / n
            dt = (link["te_top_n"] - link["te_bottom_n"]) / n
            bore = math.pi / 4 * s.bore_id_m ** 2
            for k in range(n):
                wet = submerged_length_m(z, le) / le
                m = s.mass_per_m_kg + rho_c * bore + s.ca_normal * rho_w * s.displaced_volume_per_m_m3 * wet
                segs.append(BeamSegment(le, s.ei_nm2, link["te_top_n"] - k * dt, link["te_top_n"] - (k + 1) * dt, m))
                z -= le
    return segs, rotary_node


def reference_periods(spec: OpenWaterRiserSpec, n_modes: int = 5, *, max_element_m: float = 1.0) -> list[float]:
    """Tensioned-beam periods, frame to datum: free top with the frame mass and the top-tension pendulum
    stiffness T / h, pinned (rotation free) at the rotary, clamped at the datum (fixed-base gate model)."""
    if spec.foundation is not None:
        raise ValueError("reference_periods models the fixed-base gate model; the spec has a foundation")
    segs, rot = beam_model(spec, max_element_m=max_element_m)
    t = spec.tensioners
    return tensioned_beam_periods(segs, n_modes, pinned_nodes=(rot, len(segs)), clamp_bottom=True,
                                  point_masses={0: spec.tension_frame.mass_kg},
                                  point_springs={0: t.total_vertical_tension_n / t.anchor_height_m})


def _waterline_section(spec: OpenWaterRiserSpec) -> LineSection:
    z = spec.tension_frame.z_m
    for s in (*spec.upper, *spec.riser):
        if z - s.length_m < 0.0 <= z:
            return s
        z -= s.length_m
    raise ValueError("no released section crosses the waterline")


def release_reference(spec: OpenWaterRiserSpec) -> dict[str, Any]:
    """Rigid-body closed form of the EDP release: M z'' = F0 - k_w z, from rest at the release.

    M = frame + structure and bore contents of every released section (+ axial added mass Ca_axial x rho_w x
    displaced volume of the submerged part); F0 = T_ar - W_released; k_w = rho_w g A_wl (buoyancy lost per metre
    of rise at the waterline). z(t) = F0 / k_w (1 - cos wt), v(t) = F0 / (k_w) w sin wt, w = sqrt(k_w / M)."""
    if spec.edp_release is None:
        raise ValueError("the spec has no EDP release")
    rho_w, rho_c = spec.environment.water_density_kg_m3, spec.contents.density_kg_m3
    ref = tension_references(spec)
    t_ar = spec.edp_release.anti_recoil_tension_n or spec.tensioners.total_vertical_tension_n
    m = spec.tension_frame.mass_kg + spec.rotary_mass_kg
    z = spec.tension_frame.z_m
    for s in (*spec.upper, *spec.riser):
        wet = submerged_length_m(z, s.length_m)
        m += (s.mass_per_m_kg + rho_c * math.pi / 4 * s.bore_id_m ** 2) * s.length_m
        m += s.ca_axial * rho_w * s.displaced_volume_per_m_m3 * wet
        z -= s.length_m
    f0 = t_ar - ref["released_weight_n"]
    k_w = rho_w * G * _waterline_section(spec).displaced_volume_per_m_m3
    w = math.sqrt(k_w / m)
    amp = f0 / k_w
    return {"mass_kg": m, "released_weight_n": ref["released_weight_n"], "anti_recoil_tension_n": t_ar,
            "net_force_n": f0, "k_w_n_m": k_w, "omega_rad_s": w,
            "z": lambda t: amp * (1.0 - math.cos(w * t)), "v": lambda t: amp * w * math.sin(w * t)}
