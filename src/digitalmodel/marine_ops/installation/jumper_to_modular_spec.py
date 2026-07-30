"""Convert Ballymore jumper domain specs into generator-ready ProjectInputSpec data.

Bridges the jumper calculation layer (jumper_lift) to the OrcaFlex modular
model generator (issue #602, parent #471/#478):

    domain spec.yml -> run_jumper_analysis() -> build_modular_spec()
        -> ProjectInputSpec(**data) -> ModularModelGenerator -> master.yml

Line-type property shapes mirror the real Ballymore SZ model as extracted in
``docs/domains/orcaflex/jumper/plet_to_plem/spec.yml`` (Homogeneous pipe with
coating for the jumper pipe; General with explicit mass for the OCS 200-V
connector). Where the real model uses arc-length VariableData for the
buoyancy/strake coating, this layer substitutes uniform smeared-equivalent
coating layers computed from the jumper_lift module properties, so the
generated model is self-contained (no VariableData sources required).

Geometry: the 27-section chain runs along an M-profile reconstructed from the
GA segment lengths by a turtle walk (A up, B across, C down, D bottom span,
E up, F across, G down, 90-degree bends of the pipe bend radius between
straights, connectors vertical at the ends). The A-G orientation mapping is
an engineering assumption pending GA-drawing verification (#480/#481); end
coordinates and the spline starting shape derive from that walk.

No OrcFxAPI or license required.
"""
from __future__ import annotations

import math
from typing import Any, Dict, List, Tuple

from digitalmodel.marine_ops.installation.jumper_lift import (
    BarePipeProperties,
    PipeSectionLengths,
    compute_bare_pipe,
    compute_buoyancy,
    compute_pipe_geometry,
    compute_strake,
)
from digitalmodel.marine_ops.installation.jumper_installation import (
    parse_jumper_config,
    stage_1_load_spec,
)
from digitalmodel.marine_ops.installation.jumper_lift import run_jumper_analysis

# Line-type names must match compute_orcaflex_sections() output and the real
# SZ model naming (asserted by test_jumper_plet_to_plem_semantic.py).
LT_COATED = '10.75"Jumper_wCoat'
LT_STRAKE = '10.75"Jumper_wCoat_wStrake'
LT_BUOY = '10.75"Jumper_wCoat_wBuoy'
LT_CONNECTOR = "OCS 200-V"

# Hydrodynamic coefficients mirrored from the extracted real model
# (docs/domains/orcaflex/jumper/plet_to_plem/spec.yml LineTypes).
_CDN_COATED = 2.4
_CDN_KITTED = 3.6  # strake / buoyancy sections
_CDZ = 0.008
_CAN = 1.0
_POISSON = 0.293

# Hub elevation above seabed at End A (manifold side). Not present in the
# domain spec; assumption recorded in the generated metadata description.
_HUB_CLEARANCE_M = 5.0


def _annulus_area(od: float, ident: float) -> float:
    return math.pi / 4.0 * (od * od - ident * ident)


def _smeared_coating(
    pipe: BarePipeProperties,
    od_hydro_m: float,
    kit_mass_kg_per_m: float,
) -> Tuple[float, float]:
    """Equivalent single-layer coating replacing insulation + kit.

    Returns (thickness_m, density_te_m3) for a uniform coating annulus from
    pipe steel OD out to the kit hydrodynamic OD whose mass per length equals
    insulation + kit hardware. Displacement matches by construction (the
    annulus spans the same hydro OD the kit calculations use).
    """
    insulation_mass = (
        _annulus_area(pipe.insulation_od_m, pipe.od_m)
        * pipe.insulation_density_te_m3 * 1000.0
    )
    area = _annulus_area(od_hydro_m, pipe.od_m)
    thickness = (od_hydro_m - pipe.od_m) / 2.0
    density_te_m3 = (insulation_mass + kit_mass_kg_per_m) / area / 1000.0
    return thickness, density_te_m3


def build_line_types(
    pipe: BarePipeProperties | None = None,
    connector_weight_kg: float = 1678.5,
    connector_length_m: float = 1.3,
    buoy=None,
    strake=None,
) -> List[Dict[str, Any]]:
    """Build the four generic line-type definitions for the jumper model.

    ``buoy`` / ``strake`` accept the (possibly spec-overridden) module
    properties from ``run_jumper_analysis`` results; ``None`` falls back to
    the workbook defaults.
    """
    if pipe is None:
        pipe = compute_bare_pipe()
    if buoy is None:
        buoy = compute_buoyancy()
    if strake is None:
        strake = compute_strake()

    def _homogeneous(name: str, coating_t: float, coating_rho: float,
                     cdn: float, drag_od: float) -> Dict[str, Any]:
        return {
            "name": name,
            "category": "Homogeneous pipe",
            "outer_diameter": pipe.od_m,
            "inner_diameter": pipe.id_m,
            "properties": {
                "MaterialDensity": 7.85,
                "E": 212e6,  # kPa, carbon steel (matches real-model modulus)
                "PoissonRatio": _POISSON,
                "CoatingThickness": round(coating_t, 6),
                "CoatingMaterialDensity": round(coating_rho, 6),
                "LiningThickness": 0,
                "Can": _CAN,
                "Caz": 0,
                "Cdn": cdn,
                "Cdz": _CDZ,
                "NormalDragLiftDiameter": round(drag_od, 6),
                "RayleighDampingCoefficients": "(no damping)",
            },
        }

    buoy_t, buoy_rho = _smeared_coating(
        pipe, buoy.od_hydro_m, buoy.dry_weight_kg / buoy.length_m
    )
    strake_t, strake_rho = _smeared_coating(
        pipe, strake.od_hydro_m, strake.dry_weight_kg / strake.length_m
    )

    connector_mass_te_m = connector_weight_kg / connector_length_m / 1000.0

    return [
        _homogeneous(
            LT_COATED, pipe.insulation_thickness_m,
            pipe.insulation_density_te_m3, _CDN_COATED, pipe.insulation_od_m,
        ),
        _homogeneous(LT_STRAKE, strake_t, strake_rho, _CDN_KITTED, strake.od_drag_m),
        _homogeneous(LT_BUOY, buoy_t, buoy_rho, _CDN_KITTED, buoy.od_drag_m),
        {
            "name": LT_CONNECTOR,
            "category": "General",
            "outer_diameter": 1.8,
            "inner_diameter": 1.5,
            "mass_per_length": round(connector_mass_te_m, 6),
            "bending_stiffness": [100e3, None],
            "axial_stiffness": 1e6,
            "properties": {
                "GJ": 80.0,
                "PoissonRatio": _POISSON,
                "Ca": [_CAN, None, 0],
                "Cd": [1.2, None, _CDZ],
                "RayleighDampingCoefficients": "(no damping)",
            },
        },
    ]


def walk_m_profile(
    pipe_geom: Dict[str, Any],
    bend_radius_m: float,
    connector_length_m: float,
    end_a_z: float,
) -> List[Tuple[float, float, float]]:
    """Corner coordinates of the assumed M-profile centerline.

    Turtle walk in the X-Z plane from End A (manifold hub): connector up,
    A up, 90-deg bend, B across, bend, C down, bend, D across (bottom span),
    bend, E up, bend, F across, bend, G down, connector down to End B.
    Each 90-degree bend advances ``bend_radius_m`` in both the leaving and
    entering directions; corner points are the tangent intersections.
    """
    s = pipe_geom["straight_lengths_m"]
    r = bend_radius_m
    pts: List[Tuple[float, float, float]] = []
    x, z = 0.0, end_a_z

    def p() -> None:
        pts.append((round(x, 4), 0.0, round(z, 4)))

    p()                                  # End A (connector base)
    z += connector_length_m + s[0] + r   # connector + A up to bend-1 corner
    p()
    x += r + s[1] + r                    # B across to bend-2 corner
    p()
    z -= r + s[2] + r                    # C down to bend-3 corner
    p()
    x += r + s[3] + r                    # D bottom span to bend-4 corner
    p()
    z += r + s[4] + r                    # E up to bend-5 corner
    p()
    x += r + s[5] + r                    # F across to bend-6 corner
    p()
    z -= r + s[6] + connector_length_m   # G down + connector to End B
    p()
    return pts


def build_modular_spec(spec_path: str) -> Dict[str, Any]:
    """Convert a Ballymore jumper domain spec.yml into ProjectInputSpec data.

    Returns a dict accepted by ``ProjectInputSpec(**data)`` (generic track)
    containing the full 27-section jumper line, the four jumper line types,
    and the environment/simulation data carried over from the domain spec.
    """
    spec = stage_1_load_spec(spec_path)
    config = parse_jumper_config(spec)
    results = run_jumper_analysis(config)

    sections = results["orcaflex_sections"]
    pipe = results["pipe_properties"]
    pipe_geom = results["pipe_geometry"]

    env_in = spec.get("environment", {})
    water = env_in.get("water", {})
    metocean = env_in.get("metocean", {})
    wave = metocean.get("wave", {})
    wind = metocean.get("wind", {})
    seabed = env_in.get("seabed", {})

    water_depth = float(water.get("depth", 1400))
    end_a_z = -(water_depth - _HUB_CLEARANCE_M)

    corners = walk_m_profile(
        pipe_geom, pipe.bend_radius_m, config.connector_length_m, end_a_z
    )
    end_a = corners[0]
    end_b = corners[-1]

    target_seg = 0.5
    section_rows = [
        [sec["line_type"], round(sec["length_m"], 6), target_seg]
        for sec in sections
    ]

    line_properties: Dict[str, Any] = {
        "IncludeTorsion": False,
        "TopEnd": "End A",
        "LengthAndEndOrientations": "Explicit",
        "Representation": "Finite element",
        "DragFormulation": "Standard",
        "StaticsVIV": "None",
        "DynamicsVIV": "None",
        "WaveCalculationMethod": "Specified by environment",
        (
            "Connection, ConnectionX, ConnectionY, ConnectionZ, "
            "ConnectionAzimuth, ConnectionDeclination, ConnectionGamma, "
            "ConnectionReleaseStage, ConnectionzRelativeTo"
        ): [
            ["Fixed", end_a[0], end_a[1], end_a[2], 0, 0, 0, None, None],
            ["Fixed", end_b[0], end_b[1], end_b[2], 0, 180, 0, None, None],
        ],
        "ConnectionxBendingStiffness, ConnectionyBendingStiffness": [
            ["Infinity", None],
            ["Infinity", None],
        ],
        "LineType, Length, TargetSegmentLength": section_rows,
        "ContentsMethod": "Uniform",
        "IncludeAxialContentsInertia": True,
        "ContentsDensity": 0.0,  # air-filled during installation lift
        "ContentsPressure": 0,
        "ContentsFlowRate": 0,
        "IncludedInStatics": True,
        "StaticsStep1": "Spline",
        "StaticsStep2": "Full statics",
        "StaticsSeabedFrictionPolicy": "None",
        "AsLaidTension": 0,
        "SplineOrder": 3,
        "SplineControlPointX, SplineControlPointY, SplineControlPointZ": [
            list(c) for c in corners
        ],
    }

    meta_in = spec.get("metadata", {})
    name = meta_in.get("name", config.name)

    return {
        "metadata": {
            "name": name,
            "description": (
                f"{meta_in.get('description', config.description)} "
                "(generated by jumper_to_modular_spec; M-profile orientation "
                "and hub clearance are assumptions pending GA verification, "
                "see #602/#480)"
            ),
            "structure": "jumper",
            "operation": "generic",
            "project": meta_in.get("project", "ballymore"),
        },
        "environment": {
            "water": {
                "depth": water_depth,
                "density": float(water.get("density", 1.025)),
            },
            "seabed": {
                "slope": 0,
                "stiffness": {
                    "normal": float(
                        seabed.get("stiffness", {}).get("normal", 100)
                    ),
                    "shear": float(
                        seabed.get("stiffness", {}).get(
                            "shear",
                            seabed.get("stiffness", {}).get("normal", 100),
                        )
                    ),
                },
            },
            "waves": {
                "type": "airy",
                "height": float(wave.get("significant", 0)),
                "period": float(wave.get("period", 8)),
                "direction": float(wave.get("direction", 0)),
            },
            "current": {"direction": 0},
            "wind": {
                "speed": float(wind.get("design_speed", 0)),
                "direction": 0,
            },
        },
        "generic": {
            "line_types": build_line_types(
                pipe,
                connector_weight_kg=config.connector_weight_kg,
                connector_length_m=config.connector_length_m,
                buoy=results["buoyancy_module"],
                strake=results["strake_module"],
            ),
            "lines": [
                {
                    "name": f"{name}_jumper",
                    "line_type_refs": sorted(
                        {sec["line_type"] for sec in sections}
                    ),
                    "properties": line_properties,
                }
            ],
        },
        "simulation": {
            "time_step": 0.1,
            "stages": [10.0, 60.0],
        },
    }
