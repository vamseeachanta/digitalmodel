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

from .spec import LineSection, RiserGlobalModelSpec

MIN_OD_M = 1.0e-3  # sections with no displaced volume (e.g. a slip section) still need an OD
CONN_KEY = ("Connection, ConnectionX, ConnectionY, ConnectionZ, ConnectionAzimuth, "
            "ConnectionDeclination, ConnectionGamma, ConnectionReleaseStage, ConnectionzRelativeTo")
STIFF_KEY = "ConnectionxBendingStiffness, ConnectionyBendingStiffness"
SECTIONS_KEY = "LineType, Length, TargetSegmentLength"
WINCH_CONN_KEY = "Connection, ConnectionX, ConnectionY, ConnectionZ"
STAGES_S = [10.0, 100.0]


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
          contents_density: float, contents_ref_z: float) -> dict[str, Any]:
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
            "ContentsPressure": 0,
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
    lines = [
        _line("InnerBarrel",
              [[vessel, 0, 0, spec.upper_flex_joint.pivot_z_m, 0, 180, 0, None, None],
               [ring, 0, 0, 0, 0, 180, 0, None, None]],
              [[_per_deg(spec.upper_flex_joint.rotational_stiffness_nm_per_rad), None], [inf, None]],
              spec.inner_barrel, rho_c, ref_z),
        _line("Riser",
              [[ring, 0, 0, 0, 0, 180, 0, None, None],
               ["Stack", 0, 0, 0, 0, 180, 0, None, "End B"]],
              [[inf, None], [_per_deg(spec.lower_flex_joint.rotational_stiffness_nm_per_rad), None]],
              spec.riser, rho_c, ref_z),
        _line("Stack",
              [["Fixed", 0, 0, spec.wellhead_datum_z_m, 0, 0, 0, None, None],
               ["Free", 0, 0, spec.lower_flex_joint.pivot_z_m, 0, 0, 0, None, None]],
              [[inf, None], []],  # a free end takes no connection stiffness
              list(reversed(spec.stack)), 0, ref_z),  # the stack line runs up from the datum
    ]
    t = spec.tensioners
    tension_kn = winch_tension_n(spec) / 1000.0
    winches = []
    for i in range(t.count):
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
    generic = {
        "line_types": [*(_line_type(s) for s in (*spec.inner_barrel, *spec.riser)),
                       # The stack stands on the wellhead datum at the mudline: seabed contact on
                       # its nodes is not physical and would carry part of its weight into the seabed.
                       *(_line_type(s, contact_diameter_m=MIN_OD_M) for s in spec.stack)],
        "vessel_types": [{"name": f"{vessel} type", "properties": {}}],
        "vessels": [{
            "name": vessel, "vessel_type": f"{vessel} type", "connection": "Free",
            "initial_position": [0, 0, 0],
            "properties": {"Orientation": [0, 0, 0], "IncludedInStatics": "None",
                           "PrimaryMotion": "None", "SuperimposedMotion": "None"},
        }],
        "lines": lines,
        "buoys_6d": [{
            "name": ring, "buoy_type": "Lumped buoy", "connection": "Free",
            "initial_position": [0, 0, ring_z], "mass": r.mass_kg / 1000.0, "volume": r.volume_m3,
            "properties": {"DegreesOfFreedomInStatics": "All", "InitialAttitude": [0, 0, 0],
                           "MomentsOfInertia": [m / 1000.0 for m in r.moments_of_inertia_kgm2],
                           "CentreOfMass": [0, 0, 0], "Height": 1.0, "CentreOfVolume": [0, 0, 0]},
        }],
        "winches": winches,
    }
    return {
        "metadata": {"name": spec.name, "description": spec.description or spec.name,
                     "structure": "riser", "operation": "drilling"},
        "environment": {
            "water": {"depth": spec.environment.water_depth_m,
                      "density": spec.environment.water_density_kg_m3 / 1000.0},
            "seabed": {"stiffness": {"normal": spec.environment.seabed_normal_stiffness_kn_m_m2,
                                     "shear": spec.environment.seabed_shear_stiffness_kn_m_m2}},
        },
        "simulation": {"time_step": 0.1, "stages": STAGES_S},
        "generic": generic,
    }


def write_model(spec: RiserGlobalModelSpec, out_dir: Path) -> Path:
    """Write the OrcaFlex text model (``master.yml`` + ``includes/``) and the input spec."""
    from digitalmodel.solvers.orcaflex.modular_generator import ModularModelGenerator
    from digitalmodel.solvers.orcaflex.modular_generator.schema import ProjectInputSpec

    out_dir = Path(out_dir)
    out_dir.mkdir(parents=True, exist_ok=True)
    project = ProjectInputSpec.model_validate(build_generic_spec(spec))
    ModularModelGenerator.from_spec(project).generate(out_dir)
    (out_dir / "riser-global-spec.yml").write_text(
        yaml.safe_dump(spec.model_dump(mode="json"), sort_keys=False, allow_unicode=True),
        encoding="utf-8")
    return out_dir
