"""Riser global model spec -> modular-generator ``generic`` spec -> OrcaFlex text model.

The OrcaFlex model is written as text YAML by the modular generator (``master.yml`` plus
``includes/``), never through OrcaFlex ``SaveData``, so no user or machine name enters the
file. Units in the emitted model are OrcaFlex SI: te, kN, m, kN.m/deg for end stiffness.
"""

from __future__ import annotations

import math
from pathlib import Path
from typing import Any

import yaml

from .foundation import interpolate_py, spring_stations, spring_table_kn
from .spec import FlexJoint, LineSection, RiserGlobalModelSpec, VesselMotion

MIN_OD_M = 1.0e-3  # sections with no displaced volume (e.g. a slip section) still need an OD
CONN_KEY = ("Connection, ConnectionX, ConnectionY, ConnectionZ, ConnectionAzimuth, "
            "ConnectionDeclination, ConnectionGamma, ConnectionReleaseStage, ConnectionzRelativeTo")
STIFF_KEY = "ConnectionxBendingStiffness, ConnectionyBendingStiffness"
SECTIONS_KEY = "LineType, Length, TargetSegmentLength"
WINCH_CONN_KEY = "Connection, ConnectionX, ConnectionY, ConnectionZ"
STAGES_S = [10.0, 100.0]
SLIP = "SlipJoint"
TORSION_KN_M_PER_DEG = 1.0e6  # lower flex-joint twisting stiffness when the riser carries torsion
TOP = "StringTop"  # equivalent-string top slider on the vessel (tensioner representation 'vertical_force')
TOP_WINCH_HEIGHT_M = 1000.0  # anchor of the vertical top-tension winch above the UFJ pivot, vessel frame
G = 9.80665


def equivalent_string_top_tension_n(spec: RiserGlobalModelSpec) -> float:
    """Top tension of the equivalent string: the tensioner vertical force plus the in-air weight of the
    inner-barrel sections and their contents, which the vessel carries through the UFJ in the lines
    representation - so the tension below the ring is the same in both representations."""
    w_ib = sum((s.mass_per_m_kg + spec.contents.density_kg_m3 * math.pi / 4 * s.bore_id_m ** 2) * s.length_m
               for s in spec.inner_barrel) * G
    return spec.tensioners.total_vertical_tension_n + w_ib


def hydrodynamic_od_m(displaced_volume_per_m_m3: float) -> float:
    """Diameter whose circular area equals the displaced volume per metre."""
    if displaced_volume_per_m_m3 <= 0:
        return MIN_OD_M
    return math.sqrt(4.0 * displaced_volume_per_m_m3 / math.pi)


def _tensioner_geometry(spec: RiserGlobalModelSpec) -> tuple[float, float, float]:
    t = spec.tensioners
    dx = t.sheave_radius_m - t.ring_attach_radius_m
    dz = t.sheave_z_m - spec.tension_ring.z_static_m
    return dx, dz, math.hypot(dx, dz)


def winch_tension_n(spec: RiserGlobalModelSpec) -> float:
    """Tension per tensioner line such that the vertical components sum to the target."""
    _, dz, length = _tensioner_geometry(spec)
    return spec.tensioners.total_vertical_tension_n / (spec.tensioners.count * dz / length)


def _per_deg(k_nm_per_rad: float) -> float:
    return k_nm_per_rad * math.pi / 180.0 / 1000.0  # kN.m/deg


def _fj_stiffness(fj: FlexJoint, name: str, var_data: list[dict[str, Any]]) -> float | str:
    """Numeric kN.m/deg for a linear joint; a Bendingconnectionstiffness variable-data name
    (rotation deg -> moment kN.m) for a nonlinear one."""
    if fj.moment_rotation_deg_nm is None:
        return _per_deg(fj.rotational_stiffness_nm_per_rad)
    var_data.append({"name": name, "data_type": "Bendingconnectionstiffness",
                     "entries": [[float(a), m / 1000.0] for a, m in fj.moment_rotation_deg_nm]})
    return name


RAO_KEY = ("RAOPeriodOrFrequency, RAOSurgeAmp, RAOSurgePhase, RAOSwayAmp, RAOSwayPhase, RAOHeaveAmp, "
           "RAOHeavePhase, RAORollAmp, RAORollPhase, RAOPitchAmp, RAOPitchPhase, RAOYawAmp, RAOYawPhase")


def _vessel_type(vm: VesselMotion | None, fallback: str) -> dict[str, Any]:
    if vm is None:
        return {"name": fallback, "properties": {}}
    r = vm.raos
    tables = []
    for i, d in enumerate(r.directions_deg):
        rows = []
        for j, f in enumerate(r.frequencies_rad_s):
            row = [f]
            for k in range(6):
                row += [r.amplitude[i][j][k], r.phase_deg[i][j][k]]
            rows.append(row)
        tables.append({"RAODirection": d, RAO_KEY: rows})
    half = min(r.directions_deg) >= 0.0 and max(r.directions_deg) <= 180.0
    return {"name": vm.class_id, "properties": {
        "RAOResponseUnits": "degrees",
        "RAOWaveUnit": "amplitude",
        "WavesReferredToBy": "frequency (rad/s)",
        "RAOPhaseConvention": r.phase_convention,
        "RAOPhaseUnitsConvention": "degrees",
        "RAOPhaseRelativeToConvention": "crest",
        "SurgePositive": "forward", "SwayPositive": "port", "HeavePositive": "up",
        "RollPositiveStarboard": "down", "PitchPositiveBow": "down", "YawPositiveBow": "port",
        "Symmetry": "xz plane" if half else "None",
        "Draughts": [{"Name": "Operating", "DisplacementRAOs": {
            "RAOOrigin": [float(x) for x in vm.rao_origin_m], "PhaseOrigin": [None, None, None],
            "RAOs": tables}}],
    }}


def _line_type(s: LineSection, *, contact_diameter_m: float | None = None) -> dict[str, Any]:
    od = hydrodynamic_od_m(s.displaced_volume_per_m_m3)
    gj = s.gj_nm2 if s.gj_nm2 is not None else s.ei_nm2 / 1.3
    props: dict[str, Any] = {
        "CentreOfMass": [0, 0],
        "BulkModulus": "Infinity",
        "CompressionIsLimited": False,
        "PoissonRatio": 0.3,
        "GJ": gj / 1000.0,
        "TensionTorqueCoupling": 0,
        "Ca": [s.ca_normal, None, s.ca_axial],
        "Cs": 0,
        "Ce": 0,
        "Cd": [s.cd_normal, None, s.cd_axial],
        "Cl": 0,
        "NormalDragLiftDiameter": s.drag_diameter_m if s.drag_diameter_m > 0 else None,
        "StressOD": s.stress_od_m,
        "StressID": s.stress_id_m,
        "SeabedLateralFrictionCoefficient": 0.5,
    }
    if contact_diameter_m is not None:
        props["OuterContactDiameter"] = contact_diameter_m
    return {
        "name": s.name,
        "category": "General",
        "outer_diameter": od,
        "inner_diameter": s.bore_id_m,
        "mass_per_length": s.mass_per_m_kg / 1000.0,
        "bending_stiffness": [s.ei_nm2 / 1000.0, None],
        "axial_stiffness": s.ea_n / 1000.0,
        "properties": props,
    }


def _trim(row: list[Any]) -> list[Any]:
    """Drop trailing unset cells: OrcaFlex refuses '~' for a dormant column (e.g. zRelativeTo)."""
    row = list(row)
    while row and row[-1] is None:
        row.pop()
    return row


def _line(name: str, ends: list[list[Any]], stiffness: list[list[Any]], sections: list[LineSection],
          contents_density: float, contents_ref_z: float, *, pressure_kpa: float = 0.0) -> dict[str, Any]:
    ends = [_trim(e) for e in ends]
    return {
        "name": name,
        "line_type_refs": [s.name for s in sections],
        "properties": {
            "IncludeTorsion": False,
            "TopEnd": "End A",
            "LengthAndEndOrientations": "Explicit",
            "Representation": "Finite element",
            CONN_KEY: ends,
            STIFF_KEY: stiffness,
            SECTIONS_KEY: [[s.name, s.length_m, s.segment_length_m] for s in sections],
            "ContentsMethod": "Uniform",
            "ContentsDensity": contents_density,
            "ContentsPressureRefZ": contents_ref_z,
            "ContentsPressure": pressure_kpa,
            "ContentsFlowRate": 0,
            "IncludedInStatics": True,
            "StaticsStep1": "Catenary",
            "StaticsStep2": "Full statics",
        },
    }


def build_generic_spec(spec: RiserGlobalModelSpec) -> dict[str, Any]:
    """The modular-generator ProjectInputSpec (``generic:``) for the riser global model."""
    vessel = spec.vessel_name
    ring = "TensionRing"
    rho_c = spec.contents.density_kg_m3 / 1000.0
    ref_z = spec.contents.pressure_ref_z_m
    ring_z = spec.ring_z_geometric_m
    inf = "Infinity"
    var_data: list[dict[str, Any]] = []
    k_ufj = _fj_stiffness(spec.upper_flex_joint, "UFJ stiffness", var_data)
    k_lfj = _fj_stiffness(spec.lower_flex_joint, "LFJ stiffness", var_data)
    f = spec.foundation
    stack_base = (["Conductor", 0, 0, 0, 0, 0, 0, None, "End B"] if f is not None
                  else ["Fixed", 0, 0, spec.wellhead_datum_z_m, 0, 0, 0, None, None])
    vertical_only = spec.tensioners.representation == "vertical_force"
    top_end = ([TOP, 0, 0, 0, 0, 180, 0, None, None] if vertical_only
               else [vessel, 0, 0, spec.upper_flex_joint.pivot_z_m, 0, 180, 0, None, None])
    lines = [
        _line("InnerBarrel",
              [top_end,
               [SLIP, 0, 0, 0, 0, 180, 0, None, None]],
              [[k_ufj, None], [inf, None]],
              spec.inner_barrel, rho_c, ref_z, pressure_kpa=spec.contents.pressure_pa / 1000.0),
        _line("Riser",
              [[ring, 0, 0, 0, 0, 180, 0, None, None],
               ["Stack", 0, 0, 0, 0, 180, 0, None, "End B"]],
              [[inf, None], [k_lfj, None]],
              spec.riser, rho_c, ref_z, pressure_kpa=spec.contents.pressure_pa / 1000.0),
        _line("Stack",
              [stack_base,
               ["Free", 0, 0, spec.lower_flex_joint.pivot_z_m, 0, 0, 0, None, None]],
              [[inf, None], []],  # a free end takes no connection stiffness
              list(reversed(spec.stack)), 0, ref_z),  # the stack line runs up from the datum
    ]
    if vertical_only:
        # without tensioner lines nothing restrains the ring about z (the lines exclude torsion):
        # the riser carries torsion so the ring yaw is held by the riser down to the stack
        # (OrcaFlex then needs a twisting stiffness at the finite-stiffness lower flex-joint end:
        # a stiff finite value, so the flex joint passes torsion as the stack below it does)
        rp = lines[1]["properties"]
        rp["IncludeTorsion"] = True
        rp.pop(STIFF_KEY)
        rp[STIFF_KEY + ", ConnectionTwistingStiffness"] = [[inf, None, inf], [k_lfj, None, TORSION_KN_M_PER_DEG]]
    extra_lines, links = _foundation_objects(spec, ref_z)
    lines += extra_lines
    t = spec.tensioners
    tension_kn = winch_tension_n(spec) / 1000.0
    winches = []
    for i in range(0 if vertical_only else t.count):
        az = math.radians(t.first_azimuth_deg + 360.0 * i / t.count)
        c, s_ = math.cos(az), math.sin(az)
        winches.append({
            "name": f"Tensioner{i + 1}",
            "properties": {
                "WinchType": "Simple",
                WINCH_CONN_KEY: [[vessel, t.sheave_radius_m * c, t.sheave_radius_m * s_, t.sheave_z_m],
                                 [ring, t.ring_attach_radius_m * c, t.ring_attach_radius_m * s_, 0]],
                "Stiffness": t.wire_stiffness_n / 1000.0,
                "Damping": 0,
                "WinchControlType": "By stage",
                "StageMode, StageValue": [["Specified tension", tension_kn]] * (len(STAGES_S) + 1),
            },
        })
    r = spec.tension_ring
    # Telescopic joint: the inner-barrel end rides in the ring frame, free only along z, so the
    # joint passes shear and moment into the ring but no axial load. The initial DOF value is the
    # expected ring rise under tension (statics converges from there). In the equivalent string
    # (vertical_force) the joint is locked: the string is continuous and carries the top tension.
    slip = {
        "name": SLIP, "in_frame_connection": ring, "constraint_type": "Calculated DOFs",
        "properties": {
            "InFrameInitialPosition": [0, 0, 0], "InFrameInitialAttitude": [0, 0, 0],
            "DOFFree, DOFInitialValue": [[False], [False],
                                         [False] if vertical_only else [True, -(r.z_static_m - ring_z)],
                                         [False], [False], [False]],
            "StiffnessAndDampingMethod": "Coefficients",
            "TranslationalStiffness": spec.slip_joint_axial_stiffness_n_per_m / 1000.0,
        },
    }
    constraints = [slip]
    if vertical_only:
        # equivalent string: the string top (UFJ pivot) is pinned laterally to the vessel but free along z,
        # and a vertical constant-tension winch (anchored far above in the vessel frame) applies the top
        # tension there - the tensioner force follows the vessel with no lateral tie at the ring
        constraints.append({
            "name": TOP, "in_frame_connection": vessel, "constraint_type": "Calculated DOFs",
            "properties": {"InFrameInitialPosition": [0, 0, spec.upper_flex_joint.pivot_z_m],
                           "InFrameInitialAttitude": [0, 0, 0],
                           "DOFFree, DOFInitialValue": [[False], [False], [True, 0.0], [False], [False], [False]],
                           "StiffnessAndDampingMethod": "Coefficients", "TranslationalStiffness": 0.0}})
        winches.append({"name": "TopTensioner", "properties": {
            "WinchType": "Simple",
            WINCH_CONN_KEY: [[vessel, 0, 0, spec.upper_flex_joint.pivot_z_m + TOP_WINCH_HEIGHT_M], [TOP, 0, 0, 0]],
            "Stiffness": t.wire_stiffness_n / 1000.0, "Damping": 0, "WinchControlType": "By stage",
            "StageMode, StageValue": [["Specified tension", equivalent_string_top_tension_n(spec) / 1000.0]]
                                     * (len(STAGES_S) + 1)}})
    ring_props: dict[str, Any] = {
        "DegreesOfFreedomInStatics": "All", "InitialAttitude": [0, 0, 0],
        "MomentsOfInertia": [m / 1000.0 for m in r.moments_of_inertia_kgm2],
        "CentreOfMass": [0, 0, 0], "Height": 1.0, "CentreOfVolume": [0, 0, 0]}
    vt = _vessel_type(spec.vessel_motion, f"{vessel} type")
    below = [] if f is None else f.sections
    line_types = [*(_line_type(s) for s in (*spec.inner_barrel, *spec.riser)),
                  # The stack stands on the wellhead datum at the mudline: seabed contact on
                  # its nodes is not physical and would carry part of its weight into the seabed.
                  *(_line_type(s, contact_diameter_m=MIN_OD_M) for s in (*spec.stack, *below))]
    sd = spec.structural_damping
    if sd is not None:
        for lt in line_types:
            lt["properties"]["RayleighDampingCoefficients"] = sd.name
    ox, oy = spec.vessel_offset_m
    generic = {
        "line_types": line_types,
        "vessel_types": [vt],
        "vessels": [_vessel(spec, vt)],
        "lines": lines,
        "buoys_6d": [{
            "name": ring, "buoy_type": "Lumped buoy", "connection": "Free",
            # the ring hangs from the vessel's tensioners (or, in the equivalent string, rides on the string
            # below the vessel): start it under the (offset) vessel
            "initial_position": [ox, oy, r.z_static_m], "mass": r.mass_kg / 1000.0, "volume": r.volume_m3,
            "properties": ring_props,
        }],
        "constraints": constraints,
        "winches": winches,
    }
    if links:
        generic["links"] = links
    if var_data:
        generic["variable_data_sources"] = var_data
    if sd is not None:
        generic["rayleigh_damping"] = _rayleigh(sd)
    env, sim = _environment_and_simulation(spec)
    return {
        "metadata": {"name": spec.name, "description": spec.description or spec.name,
                     "structure": "riser", "operation": "drilling"},
        "environment": env,
        "simulation": sim,
        "generic": generic,
    }


def _vessel(spec: Any, vt: dict[str, Any]) -> dict[str, Any]:
    """The vessel object (not in statics; first-order RAO motion when the spec has vessel motion)."""
    ox, oy = spec.vessel_offset_m
    return {
        "name": spec.vessel_name, "vessel_type": vt["name"], "connection": "Free",
        "initial_position": [ox, oy, 0] if (ox or oy) else [0, 0, 0],
        "properties": {"Orientation": [0, 0, 0], "IncludedInStatics": "None",
                       "PrimaryMotion": "None",
                       **({"SuperimposedMotion": "RAOs + harmonics", "Draught": "Operating"}
                          if spec.vessel_motion else {"SuperimposedMotion": "None"})},
    }


def _foundation_objects(spec: Any, ref_z: float) -> tuple[list[dict[str, Any]], list[dict[str, Any]]]:
    """Conductor/casing line (fixed at its base, up to the wellhead datum) and its p-y spring links."""
    f = spec.foundation
    if f is None:
        return [], []
    inf = "Infinity"
    lines = [_line("Conductor",
                   [["Fixed", 0, 0, spec.wellhead_datum_z_m - f.depth_m, 0, 0, 0, None, None],
                    ["Free", 0, 0, spec.wellhead_datum_z_m, 0, 0, 0, None, None]],
                   [[inf, None], []], list(reversed(f.sections)),
                   f.flooded_density_kg_m3 / 1000.0 if f.flooded_density_kg_m3 else 0,
                   0.0 if f.flooded_density_kg_m3 else ref_z)]
    links: list[dict[str, Any]] = []
    for i, st in enumerate(spring_stations(f)):
        curve = interpolate_py(f.py_curves, st["depth_m"])
        table = spring_table_kn(curve, st["tributary_m"], f.anchor_offset_m, f.far_displacement_m)
        z = spec.wellhead_datum_z_m - st["depth_m"]
        for axis, (ax, ay) in (("x", (f.anchor_offset_m, 0.0)), ("y", (0.0, f.anchor_offset_m))):
            links.append({"name": f"PY{i + 1:03d}{axis}", "link_type": "Spring/damper", "properties": {
                "Connection, ConnectionX, ConnectionY, ConnectionZ, ConnectionzRelativeTo": [
                    ["Conductor", 0, 0, st["arc_from_base_m"], "End A"], ["Fixed", ax, ay, z]],
                "LinearSpring": "No",
                "SpringLength, SpringTension": table,
            }})
    return lines, links


def _rayleigh(sd: Any) -> dict[str, Any]:
    return {"data": [{
        "Name": sd.name, "Mode": "Coefficients (classical)", "MassCoefficient": 0.0,
        "StiffnessCoefficient": sd.stiffness_coefficient_s, "ApplyToGeometricStiffness": "Yes"}]}


def _environment_and_simulation(spec: Any) -> tuple[dict[str, Any], dict[str, Any]]:
    """Environment (water, seabed, current, regular or JONSWAP wave) and simulation stages of a riser spec."""
    # with a foundation the conductor runs below the mudline: its soil reaction is the p-y links,
    # so seabed contact is switched off (no other line reaches the seabed)
    seabed = ({"normal": 0.0, "shear": 0.0} if spec.foundation is not None else
              {"normal": spec.environment.seabed_normal_stiffness_kn_m_m2,
               "shear": spec.environment.seabed_shear_stiffness_kn_m_m2})
    env: dict[str, Any] = {
        "water": {"depth": spec.environment.water_depth_m,
                  "density": spec.environment.water_density_kg_m3 / 1000.0},
        "seabed": {"stiffness": seabed},
    }
    if spec.current is not None:
        pts = spec.current.depth_speed_m_s
        ref = max(v for _, v in pts)
        env["current"] = {"speed": ref, "direction": spec.current.direction_deg,
                          "profile": [[float(d), v / ref] for d, v in pts]}
    if spec.regular_wave is not None:
        w = spec.regular_wave
        env["waves"] = {"type": "airy", "height": w.height_m, "period": w.period_s, "direction": w.direction_deg}
    if spec.irregular_wave is not None:
        w = spec.irregular_wave
        env["waves"] = {"type": "jonswap", "height": w.hs_m, "period": w.tp_s, "direction": w.direction_deg}
        # the environment builder takes the raw train as its base layer (key order kept) and, with WaveTp
        # present and no WaveTz, writes the period as Tp; gamma is carried by the raw train only (the generator
        # schema rejects gamma = 1, the Pierson-Moskowitz limit, and keeps a raw WaveGamma it was not given);
        # the seed is user-specified so each seed is reproducible
        env["raw_properties"] = {"UserSpecifiedRandomWaveSeeds": "Yes", "WaveTrains": [{
            "Name": "Wave1", "WaveType": "JONSWAP", "WaveDirection": w.direction_deg, "WaveOrigin": [0, 0],
            "WaveTimeOrigin": 0, "WaveNumberOfSpectralDirections": 1,
            # gamma before Tp: OrcaFlex keeps Tz when gamma changes, so a later gamma would move Tp
            "WaveJONSWAPParameters": "Partially specified", "WaveGamma": w.gamma, "WaveHs": w.hs_m,
            "WaveTp": w.tp_s, "WaveSeed": w.seed, "WaveNumberOfComponents": w.number_of_components,
            "WaveSpectrumMinRelFrequency": 0.5, "WaveSpectrumMaxRelFrequency": 10,
            "WaveSpectrumMaxComponentFrequencyRange": 0.05}]}
    dyn = spec.dynamics
    sim = ({"time_step": dyn.time_step_s, "stages": [dyn.build_up_s, dyn.duration_s]} if dyn is not None
           else {"time_step": 0.1, "stages": STAGES_S})
    return env, sim



def write_model(spec: Any, out_dir: Path) -> Path:
    """Write the OrcaFlex text model (``master.yml`` + ``includes/``) and the input spec: a drilling riser
    (:class:`RiserGlobalModelSpec`) or an open-water riser (``open_water.OpenWaterRiserSpec``)."""
    from digitalmodel.solvers.orcaflex.modular_generator import ModularModelGenerator
    from digitalmodel.solvers.orcaflex.modular_generator.schema import ProjectInputSpec

    out_dir = Path(out_dir)
    out_dir.mkdir(parents=True, exist_ok=True)
    from .open_water import OpenWaterRiserSpec, build_open_water_generic_spec

    generic = (build_open_water_generic_spec(spec) if isinstance(spec, OpenWaterRiserSpec)
               else build_generic_spec(spec))
    project = ProjectInputSpec.model_validate(generic)
    ModularModelGenerator.from_spec(project).generate(out_dir)
    (out_dir / "riser-global-spec.yml").write_text(
        yaml.safe_dump(spec.model_dump(mode="json"), sort_keys=False, allow_unicode=True),
        encoding="utf-8")
    return out_dir
