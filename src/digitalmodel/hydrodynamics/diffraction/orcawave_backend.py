"""OrcaWave input backend (WRK-059).

Converts a canonical DiffractionSpec into OrcaWave .yml project files.

Supports two output modes:
- **Single**: One monolithic .yml file containing all OrcaWave sections.
- **Modular**: Separate .yml files per section, plus a master.yml that
  can be used as reference.

OrcaWave YAML conventions:
- PascalCase keys (UnitsSystem, SolveType, BodyMeshFileName, ...)
- Yes/No strings for booleans
- SI unit system: lengths in metres, mass in tonnes, density in t/m^3
- Periods in seconds for PeriodOrFrequency
- Headings in degrees
- Inertia tensor as 3x3 matrix rows

Reference: OrcaWave 11.x YAML format (see docs/domains/orcawave/examples).
"""

from __future__ import annotations

import math
from pathlib import Path
from typing import Any

import yaml

from digitalmodel.hydrodynamics.diffraction.diffraction_units import (
    density_kg_m3_to_t_m3,
    inertia_kg_m2_to_t_m2,
    kg_to_tonnes,
    rad_per_s_to_period_s,
)
from digitalmodel.hydrodynamics.diffraction.input_schemas import (
    BodySpec,
    DiffractionSpec,
    FrequencyInputType,
    LoadRAOMethod,
    SymmetryType,
    IrregularFrequencyMethod,
)


# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

_MESH_FORMAT_MAP: dict[str, str] = {
    "gdf": "Wamit gdf",
    "dat": "Wamit dat",
    "stl": "STL",
    "msh": "Gmsh",
    "obj": "OBJ",
    "auto": "Auto",
}

# Control-surface mesh formats. Native OrcaWave pairs .gdf control surfaces
# with 'Wamit gdf' (docs/domains/orcawave/examples/L03 Semi-sub multibody
# analysis/L03 Semi-sub multibody analysis.yml, BodyControlSurfaceMeshFormat)
# and WAMIT .csf files with 'Wamit csf' (L00_validation_wamit 2.7/2.8 golden
# benchmarks).
_CONTROL_SURFACE_FORMAT_MAP: dict[str, str] = {
    "csf": "Wamit csf",
    "gdf": "Wamit gdf",
    "dat": "Wamit dat",
    "stl": "STL",
}

_SYMMETRY_MAP: dict[str, str] = {
    "none": "None",
    "xz": "xz plane",
    "yz": "yz plane",
    "xz+yz": "xz and yz planes",
}

_DOF_MAP: dict[str, str] = {
    "surge": "BodyFixedDOFx",
    "sway": "BodyFixedDOFy",
    "heave": "BodyFixedDOFz",
    "roll": "BodyFixedDOFRx",
    "pitch": "BodyFixedDOFRy",
    "yaw": "BodyFixedDOFRz",
}

_LOAD_RAO_METHOD_MAP: dict[str, str] = {
    "haskind": "Haskind",
    "direct": "Direct",
    "both": "Both",
}


# ---------------------------------------------------------------------------
# YAML representer to avoid Python-specific tags
# ---------------------------------------------------------------------------


class _CleanDumper(yaml.SafeDumper):
    """Dumper that outputs clean YAML without Python-specific tags.

    Overrides default string behaviour to emit Yes/No unquoted
    (OrcaWave convention) and to keep long keys on a single line.
    """

    # Allow very long keys on one line (OrcaWave composite keys)
    best_width = 2000


def _str_representer(dumper: yaml.Dumper, data: str) -> yaml.Node:
    """Represent strings; Yes/No are plain (no quotes) for OrcaWave."""
    if data in ("Yes", "No"):
        # Force plain style so YAML outputs Yes / No without quotes
        return dumper.represent_scalar(
            "tag:yaml.org,2002:bool", data, style=""
        )
    return dumper.represent_scalar("tag:yaml.org,2002:str", data)


_CleanDumper.add_representer(str, _str_representer)

# Register numpy types so SafeDumper can handle them
try:
    import numpy as np

    _CleanDumper.add_representer(
        np.float64,
        lambda dumper, data: dumper.represent_float(float(data)),
    )
    _CleanDumper.add_representer(
        np.float32,
        lambda dumper, data: dumper.represent_float(float(data)),
    )
    _CleanDumper.add_representer(
        np.int64,
        lambda dumper, data: dumper.represent_int(int(data)),
    )
    _CleanDumper.add_representer(
        np.int32,
        lambda dumper, data: dumper.represent_int(int(data)),
    )
except ImportError:
    pass


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


def _bool_to_yn(value: bool) -> str:
    """Convert Python bool to OrcaWave Yes/No string."""
    return "Yes" if value else "No"


def _periods_from_spec(spec: DiffractionSpec) -> list[float]:
    """Get sorted period list from spec, converting from freq if needed."""
    if spec.frequencies.input_type == FrequencyInputType.PERIOD:
        if spec.frequencies.values is not None:
            periods = [float(v) for v in spec.frequencies.values]
        else:
            r = spec.frequencies.range
            import numpy as np

            if r.distribution.value == "linear":
                periods = [float(v) for v in np.linspace(r.start, r.end, r.count)]
            else:
                periods = [
                    float(v)
                    for v in np.logspace(
                        np.log10(r.start), np.log10(r.end), r.count
                    )
                ]
    else:
        freqs_rad_s = spec.frequencies.to_frequencies_rad_s()
        periods = [rad_per_s_to_period_s(float(w)) for w in freqs_rad_s if w > 0]

    periods = sorted(round(t, 4) for t in periods)
    return periods


def _headings_from_spec(spec: DiffractionSpec) -> list[float]:
    """Get heading list from spec."""
    return spec.wave_headings.to_heading_list()


def _water_depth_value(spec: DiffractionSpec) -> float | str:
    """Map water depth to OrcaWave convention."""
    depth = spec.environment.water_depth
    if isinstance(depth, str) and depth.lower() in (
        "infinite",
        "inf",
        "deep",
    ):
        return "Infinity"
    return float(depth)


def _water_density_tonnes(spec: DiffractionSpec) -> float:
    """Convert water density from kg/m^3 to tonnes/m^3 for OrcaWave SI."""
    return density_kg_m3_to_t_m3(spec.environment.water_density)


def _build_inertia_tensor(body_spec: BodySpec) -> list[list[float]]:
    """Build 3x3 inertia tensor in tonnes.m^2 from body spec.

    OrcaWave SI uses mass in tonnes, so inertia must also be in t.m^2.
    The spec stores mass in kg and inertia in kg.m^2.
    """
    inertia = body_spec.vessel.inertia
    mass_kg = inertia.mass

    if inertia.inertia_tensor is not None:
        t = inertia.inertia_tensor
        # Convert from kg.m^2 to t.m^2
        ixx = inertia_kg_m2_to_t_m2(t.get("Ixx", 0.0))
        iyy = inertia_kg_m2_to_t_m2(t.get("Iyy", 0.0))
        izz = inertia_kg_m2_to_t_m2(t.get("Izz", 0.0))
        ixy = inertia_kg_m2_to_t_m2(t.get("Ixy", 0.0))
        ixz = inertia_kg_m2_to_t_m2(t.get("Ixz", 0.0))
        iyz = inertia_kg_m2_to_t_m2(t.get("Iyz", 0.0))
        return [
            [ixx, ixy, ixz],
            [ixy, iyy, iyz],
            [ixz, iyz, izz],
        ]
    else:
        rog = inertia.radii_of_gyration
        mass_t = kg_to_tonnes(mass_kg)
        ixx = mass_t * rog[0] ** 2
        iyy = mass_t * rog[1] ** 2
        izz = mass_t * rog[2] ** 2
        return [
            [ixx, 0, 0],
            [0, iyy, 0],
            [0, 0, izz],
        ]


def _zero_6x6() -> list[list[float]]:
    """Return a 6x6 zero matrix."""
    return [[0, 0, 0, 0, 0, 0] for _ in range(6)]


def _build_body_dict(
    body_spec: BodySpec,
    spec: DiffractionSpec,
) -> dict[str, Any]:
    """Build an OrcaWave body dict from a BodySpec."""
    vessel = body_spec.vessel
    geom = vessel.geometry
    inertia = vessel.inertia

    mesh_file = Path(geom.mesh_file).name
    mesh_fmt = _MESH_FORMAT_MAP.get(geom.mesh_format.value, "Auto")
    mesh_sym = _SYMMETRY_MAP.get(geom.symmetry.value, "None")

    body: dict[str, Any] = {}
    body["BodyName"] = vessel.name
    body["BodyMeshPosition"] = list(body_spec.position)
    body["BodyMeshAttitude"] = list(body_spec.attitude)
    body["BodyIncludedInAnalysis"] = "Yes"
    body["BodyMeshFileName"] = mesh_file
    body["BodyMeshFormat"] = mesh_fmt
    body["BodyMeshLengthUnits"] = geom.length_units
    body["BodyMeshSymmetry"] = mesh_sym

    # Interior surface panels (for irregular frequency removal). The 3-way
    # method (#501): interior_panels keeps today's emission; none and
    # control_surface emit No (the control surface itself is emitted below
    # from the body's control_surface spec).
    irreg_method = spec.solver_options.irregular_frequency_method
    add_interior = irreg_method is IrregularFrequencyMethod.INTERIOR_PANELS
    body["BodyAddInteriorSurfacePanels"] = _bool_to_yn(add_interior)
    if add_interior:
        body["BodyInteriorSurfacePanelMethod"] = "Triangulation method"

    # Control surface mesh (for mean drift loads). Body-level definitions
    # take precedence over vessel-level (#609).
    cs = body_spec.resolve_control_surface()
    if cs is not None and cs.mesh_file is not None:
        body["BodyControlSurfaceType"] = "Defined by mesh file"
        body["BodyControlSurfaceMeshFileName"] = Path(cs.mesh_file).name
        # Explicit format wins; otherwise infer from the file extension so a
        # .gdf control surface is declared 'Wamit gdf', not 'Wamit csf'
        # (formats have different layouts; see L03 Semi-sub native example).
        cs_format = cs.mesh_format
        if cs_format is None:
            cs_format = Path(cs.mesh_file).suffix.lstrip(".")
        cs_key = str(cs_format).lower()
        if cs_key not in _CONTROL_SURFACE_FORMAT_MAP:
            raise ValueError(
                f"Unsupported control surface mesh format {cs_format!r} for "
                f"body '{vessel.name}' (file {cs.mesh_file!r}). Supported: "
                f"{sorted(_CONTROL_SURFACE_FORMAT_MAP)}"
            )
        body["BodyControlSurfaceMeshFormat"] = _CONTROL_SURFACE_FORMAT_MAP[
            cs_key
        ]
        body["BodyControlSurfaceMeshLengthUnits"] = geom.length_units
    elif cs is not None:
        # Auto-generated control surface (#324): keys per the native OrcaWave
        # exemplar docs/domains/orcawave/L01_aqwa_benchmark/
        # orcawave_001_ship_raos_rev2_matched.yml (BodyControlSurfaceType:
        # Automatically generated, panel size / separation / include free
        # surface). ControlSurfaceSpec validation guarantees panel_size and
        # separation are present when mesh_file is absent.
        body["BodyControlSurfaceType"] = "Automatically generated"
        body["BodyControlSurfacePanelSize"] = cs.panel_size
        body["BodyControlSurfaceSeparationFromBody"] = cs.separation
        body["BodyControlSurfaceIncludeFreeSurface"] = "Yes"

    # OrcaFlex import settings
    body["BodyOrcaFlexImportSymmetry"] = (
        "Use global mesh symmetry"
        if geom.symmetry != SymmetryType.NONE
        else "None"
    )
    body["BodyHydrostaticIntegralMethod"] = "Standard"
    body["BodyHydrostaticStiffnessMethod"] = "Displacement"

    # Inertia
    if inertia.mode == "free_floating":
        # Free-floating body: mass auto-computed from displaced volume
        body["BodyInertiaSpecifiedBy"] = (
            "Radii of gyration (for a free-floating body)"
        )
        cog_z = inertia.cog_z if inertia.cog_z is not None else 0.0
        body["BodyCentreOfMassZRelativeToFreeSurface"] = cog_z
        r = inertia.radii_of_gyration
        radii_key = (
            "BodyRadiiOfGyrationRx, "
            "BodyRadiiOfGyrationRy, "
            "BodyRadiiOfGyrationRz"
        )
        body[radii_key] = [
            [r[0], 0, 0],
            [0, r[1], 0],
            [0, 0, r[2]],
        ]
        body["BodyRadiiOfGyrationOriginType"] = "Body origin"
    else:
        # Explicit mode: mass and inertia tensor specified directly
        body["BodyInertiaSpecifiedBy"] = "Matrix (for a general body)"
        body["BodyCentreOfMass"] = list(inertia.centre_of_gravity)
        body["BodyMass"] = kg_to_tonnes(inertia.mass)

        inertia_key = (
            "BodyInertiaTensorRx, "
            "BodyInertiaTensorRy, "
            "BodyInertiaTensorRz"
        )
        body[inertia_key] = _build_inertia_tensor(body_spec)
        _TENSOR_ORIGIN_MAP = {
            "body_origin": "Body origin",
            "centre_of_mass": "Centre of mass",
        }
        origin = getattr(inertia, "inertia_tensor_origin", "body_origin")
        body["BodyInertiaTensorOriginType"] = _TENSOR_ORIGIN_MAP.get(
            origin, "Body origin"
        )

    # External stiffness matrix
    stiffness_key = (
        "BodyExternalStiffnessMatrixx, "
        "BodyExternalStiffnessMatrixy, "
        "BodyExternalStiffnessMatrixz, "
        "BodyExternalStiffnessMatrixRx, "
        "BodyExternalStiffnessMatrixRy, "
        "BodyExternalStiffnessMatrixRz"
    )
    if vessel.external_stiffness is not None:
        body[stiffness_key] = [list(row) for row in vessel.external_stiffness]
    else:
        body[stiffness_key] = _zero_6x6()
    body["BodyExternalStiffnessMatrixOriginType"] = "Body origin"

    # External damping matrix
    damping_key = (
        "BodyExternalDampingMatrixx, "
        "BodyExternalDampingMatrixy, "
        "BodyExternalDampingMatrixz, "
        "BodyExternalDampingMatrixRx, "
        "BodyExternalDampingMatrixRy, "
        "BodyExternalDampingMatrixRz"
    )
    if vessel.external_damping is not None:
        body[damping_key] = [list(row) for row in vessel.external_damping]
    else:
        body[damping_key] = _zero_6x6()
    body["BodyExternalDampingMatrixOriginType"] = "Body origin"

    # Connection parent
    if body_spec.connection_parent is not None:
        body["BodyConnectionParent"] = body_spec.connection_parent
    else:
        body["BodyConnectionParent"] = "Free"

    body["BodyIncreaseRollDampingToTarget"] = "No"

    # Fixed DOFs
    fixed = set(vessel.fixed_dofs or [])
    for dof_name, field_name in _DOF_MAP.items():
        body[field_name] = _bool_to_yn(dof_name in fixed)

    return body


def _effective_solve_type(spec: DiffractionSpec) -> str:
    """Resolve OrcaWave solve type from explicit solver options + analysis type.

    Older canonical specs may set ``analysis_type: full_qtf`` while relying on
    default ``solver_options.solve_type``.  Treat that top-level analysis intent
    as authoritative unless the solver option is explicitly a non-default mode.
    """
    solve_type = getattr(spec.solver_options, "solve_type", "potential_and_source")
    analysis_type = getattr(getattr(spec, "analysis_type", None), "value", None)
    if analysis_type == "full_qtf" and solve_type == "potential_and_source":
        return "full_qtf"
    return solve_type


# ---------------------------------------------------------------------------
# Section builders
# ---------------------------------------------------------------------------


def _build_general_section(spec: DiffractionSpec) -> dict[str, Any]:
    """Build the general/calculation section."""
    solver = spec.solver_options
    qtf = solver.resolved_qtf()
    method = _LOAD_RAO_METHOD_MAP.get(solver.load_rao_method.value, "Both")

    _SOLVE_TYPE_MAP = {
        "potential_only": "Potential formulation only",
        "potential_and_source": "Potential and source formulations",
        "mean_drift": "Potential and source + mean drift (momentum conservation)",
        "diagonal_qtf": "Potential and source + diagonal QTF",
        "full_qtf": "Full QTF calculation",
    }

    section: dict[str, Any] = {}
    solve_type_key = _effective_solve_type(spec)
    if solve_type_key not in _SOLVE_TYPE_MAP:
        # Fail loud: silently defaulting an unknown solve type to first-order
        # 'Potential and source formulations' generated the wrong analysis
        # with no warning (#324). The schema restricts solve_type to this
        # vocabulary, so this only fires for bypassed validation.
        raise ValueError(
            f"Unknown solve_type {solve_type_key!r}; expected one of "
            f"{sorted(_SOLVE_TYPE_MAP)}"
        )
    section["SolveType"] = _SOLVE_TYPE_MAP[solve_type_key]
    # "Potential formulation only" makes quadratic load and
    # OutputPanelVelocities dormant — only emit when source formulations active
    has_source = solve_type_key != "potential_only"

    section["LoadRAOCalculationMethod"] = method
    preferred = getattr(solver, "preferred_load_rao_method", None)
    if preferred == "diffraction":
        section["PreferredLoadRAOCalculationMethod"] = "Diffraction"
    elif preferred == "haskind":
        section["PreferredLoadRAOCalculationMethod"] = "Haskind"
    else:
        section["PreferredLoadRAOCalculationMethod"] = (
            "Haskind" if method in ("Both", "Haskind") else "Direct"
        )
    # QTF-specific settings — derive from solve_type
    is_qtf = solve_type_key in ("diagonal_qtf", "full_qtf")
    if has_source:
        # For full_qtf: pressure integration preferred, control surface off
        # For non-QTF with qtf_calculation flag: both enabled
        if is_qtf:
            section["QuadraticLoadPressureIntegration"] = "Yes"
        else:
            section["QuadraticLoadPressureIntegration"] = _bool_to_yn(
                qtf.enabled
            )
    # Check if any body has an explicit control surface defined
    # Same resolution rule as per-body emission: body-level overrides
    # vessel-level (#609).
    has_body_control_surface = any(
        body.resolve_control_surface() is not None
        for body in spec.get_bodies()
    )

    if is_qtf:
        section["QuadraticLoadControlSurface"] = "No"
    else:
        section["QuadraticLoadControlSurface"] = _bool_to_yn(
            qtf.enabled or has_body_control_surface
        )
    section["QuadraticLoadMomentumConservation"] = "No"
    if qtf.enabled or is_qtf:
        section["PreferredQuadraticLoadCalculationMethod"] = (
            "Pressure integration" if is_qtf else "Control surface"
        )
    # Damping lid
    has_damping_lid = getattr(spec, "damping_lid", None) is not None
    section["HasResonanceDampingLid"] = _bool_to_yn(has_damping_lid)
    section["LengthTolerance"] = 100e-9
    section["WaterlineZTolerance"] = 1e-6
    section["WaterlineGapTolerance"] = 1e-6
    section["DivideNonPlanarPanels"] = _bool_to_yn(
        getattr(solver, "divide_non_planar_panels", True)
    )
    section["LinearSolverMethod"] = "Direct LU"
    section["OutputPanelPressures"] = "No"
    if has_source:
        section["OutputPanelVelocities"] = "No"
    section["OutputBodyWireFrames"] = "Yes"
    section["OutputIntermediateResults"] = "No"
    section["ValidatePanelArrangement"] = "No"
    section["BodyVolumeWarningLevel"] = 1e-12
    section["PanelAspectRatioWarningLevel"] = 25
    section["PanelsPerWavelengthWarningLevel"] = 5

    return section


def _build_units_section() -> dict[str, Any]:
    """Build the units section."""
    return {"UnitsSystem": "SI"}


def _build_environment_section(spec: DiffractionSpec) -> dict[str, Any]:
    """Build the environment section."""
    section: dict[str, Any] = {}
    section["WaterDepth"] = _water_depth_value(spec)
    section["WaterDensity"] = _water_density_tonnes(spec)
    section["WavesReferredToBy"] = "period (s)"
    section["HasWaveSpectrumForDragLinearisation"] = "No"
    section["MorisonFluidVelocity"] = "Undisturbed incident wave"
    return section


def _build_bodies_section(spec: DiffractionSpec) -> dict[str, Any]:
    """Build the bodies section."""
    bodies_list = spec.get_bodies()
    return {
        "Bodies": [_build_body_dict(b, spec) for b in bodies_list],
    }


def _build_frequencies_section(spec: DiffractionSpec) -> dict[str, Any]:
    """Build the frequency section."""
    return {
        "WavesReferredToBy": "period (s)",
        "PeriodOrFrequency": _periods_from_spec(spec),
    }


def _build_headings_section(spec: DiffractionSpec) -> dict[str, Any]:
    """Build the wave headings section."""
    section: dict[str, Any] = {
        "WaveHeading": _headings_from_spec(spec),
    }
    solve_type = _effective_solve_type(spec)
    is_qtf = solve_type in ("diagonal_qtf", "full_qtf")
    qtf = spec.solver_options.resolved_qtf()
    # Emission stays inside the existing gate (#501 plan C1): crossing-angle
    # overrides never leak into non-QTF outputs.
    if qtf.enabled or is_qtf:
        section["QTFMinCrossingAngle"] = int(qtf.min_crossing_angle)
        section["QTFMaxCrossingAngle"] = int(qtf.max_crossing_angle)
        qtf_max_period = "Infinity"
        qtf_min_period: float | int = 0
        if qtf.max_frequency is not None:
            qtf_min_period = round(
                rad_per_s_to_period_s(qtf.max_frequency), 6
            )
        if qtf.min_frequency is not None:
            qtf_max_period = round(
                rad_per_s_to_period_s(qtf.min_frequency), 6
            )
        section["QTFMinPeriodOrFrequency"] = qtf_min_period
        section["QTFMaxPeriodOrFrequency"] = qtf_max_period
        if solve_type == "full_qtf":
            section["QTFFrequencyTypes"] = "Sum frequencies"
            section["IncludeMeanDriftFullQTFs"] = "No"
    return section


def _build_solver_section(spec: DiffractionSpec) -> dict[str, Any]:
    """Build the solver settings section (subset of general for modularity).

    Note: no precision key is emitted. Native OrcaWave YAML (all files under
    docs/domains/orcawave/examples and L00 'OrcaWave v11.0 files') has no
    solver-precision key; the previously emitted lowercase 'SolverPrecision'
    was an invented key the solver never read. Non-default precision is
    rejected loudly in :func:`_check_supported_solver_options` instead.
    """
    del spec
    return {
        "LinearSolverMethod": "Direct LU",
    }


def _check_supported_solver_options(spec: DiffractionSpec) -> None:
    """Reject spec options the generated OrcaWave input cannot honor.

    ``solver_options.precision`` has no corresponding key in native OrcaWave
    YAML (none exists in the in-repo Orcina references), so a non-default
    value would be silently ignored by the solver. Fail loud instead of
    generating an input that silently ignores the user's setting (#324).
    """
    precision = spec.solver_options.precision
    precision_value = getattr(precision, "value", str(precision))
    if precision_value != "double":
        raise ValueError(
            f"solver_options.precision={precision_value!r} cannot be applied "
            "to an OrcaWave input file: native OrcaWave YAML has no solver "
            "precision key (see docs/domains/orcawave reference files). "
            "Remove the setting (OrcaWave runs at its own default precision) "
            "or use a solver that supports it."
        )


def _build_outputs_section(spec: DiffractionSpec) -> dict[str, Any]:
    """Build the outputs section."""
    solve_type = _effective_solve_type(spec)
    has_source = solve_type != "potential_only"

    section: dict[str, Any] = {}
    # OutputPanelVelocities is dormant for "Potential formulation only"
    if has_source:
        section["OutputPanelVelocities"] = "No"
    outputs = spec.outputs
    section["DetectAndSkipFieldPointsInsideBodies"] = _bool_to_yn(
        outputs.detect_field_points_inside_bodies
    )
    if outputs.field_points:
        all_points = [
            point
            for group in outputs.field_points
            for point in group.points
        ]
        # Combined-key table: one [x, y, z] row per field point — the native
        # OrcaWave convention (docs/domains/orcawave/reference/
        # text-data-files.md, 'FieldPointX, FieldPointY, FieldPointZ' names
        # three COLUMNS; each list entry is one point row, as in the native
        # test01.yml). Emitting rows-per-axis transposed the table (#324).
        section["FieldPointX, FieldPointY, FieldPointZ"] = [
            list(point) for point in all_points
        ]
    return section


def _build_damping_lid_section(spec: DiffractionSpec) -> dict[str, Any]:
    """Build the damping lid section (for moonpool bodies)."""
    lid = spec.damping_lid
    if lid is None:
        return {}

    mesh_fmt = _MESH_FORMAT_MAP.get(lid.mesh_format, "Wamit gdf")
    return {
        "DampingLidMeshFileName": Path(lid.mesh_file).name,
        "DampingLidMeshFormat": mesh_fmt,
        "DampingLidMeshLengthUnits": lid.length_units,
        "DampingFactorEpsilon": lid.damping_factor,
    }


def _build_qtf_section(spec: DiffractionSpec) -> dict[str, Any]:
    """Build QTF-specific properties for Full QTF / diagonal QTF solve types."""
    solve_type = _effective_solve_type(spec)
    if solve_type not in ("diagonal_qtf", "full_qtf"):
        return {}

    qtf = spec.solver_options.resolved_qtf()
    section: dict[str, Any] = {}
    # Direct pass-through of OrcaWave's vocabulary (Direct/Indirect/Both);
    # default "Both" preserves today's emission byte-for-byte (#501).
    section["QTFCalculationMethod"] = qtf.load_calculation_method
    section["PreferredQTFCalculationMethod"] = (
        "Direct method"
        if qtf.load_calculation_method in ("Direct", "Both")
        else "Indirect method"
    )

    # Free surface zone
    fsz = getattr(spec, "free_surface_zone", None)
    if fsz is not None and fsz.mesh_file:
        section["FreeSurfacePanelledZoneType"] = "Defined by mesh file"
        section["FreeSurfacePanelledZoneMeshFileName"] = Path(fsz.mesh_file).name
        mesh_fmt = "Wamit fdf" if fsz.mesh_format == "fdf" else _MESH_FORMAT_MAP.get(
            fsz.mesh_format, fsz.mesh_format
        )
        section["FreeSurfacePanelledZoneMeshFormat"] = mesh_fmt
        section["FreeSurfacePanelledZoneMeshLengthUnits"] = fsz.length_units
        if fsz.inner_radius is not None:
            section["FreeSurfacePanelledZoneInnerRadius"] = fsz.inner_radius

    return section


# ---------------------------------------------------------------------------
# Write helper
# ---------------------------------------------------------------------------


def _write_yml(data: dict[str, Any], path: Path) -> Path:
    """Write an OrcaWave YAML file with proper formatting."""
    with open(path, "w") as f:
        f.write("%YAML 1.1\n")
        f.write("# Type: Diffraction\n")
        f.write("# Generated by: digitalmodel OrcaWave backend\n")
        f.write("---\n")
        yaml.dump(
            data,
            f,
            Dumper=_CleanDumper,
            default_flow_style=False,
            sort_keys=False,
            allow_unicode=True,
            width=2000,
        )
        f.write("...\n")
    return path


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------


class OrcaWaveBackend:
    """Converts a DiffractionSpec to OrcaWave .yml project files.

    Supports two output modes:

    - **Single**: one .yml file with all sections merged.
    - **Modular**: separate .yml files for each section, plus a master.yml.

    Example::

        from digitalmodel.hydrodynamics.diffraction.input_schemas import DiffractionSpec
        from digitalmodel.hydrodynamics.diffraction.orcawave_backend import OrcaWaveBackend

        spec = DiffractionSpec.from_yaml("analysis.yml")
        backend = OrcaWaveBackend()
        path = backend.generate_single(spec, Path("output/"))
    """

    def generate_single(
        self,
        spec: DiffractionSpec,
        output_dir: Path,
    ) -> Path:
        """Generate a single OrcaWave .yml project file.

        Args:
            spec: Canonical diffraction specification.
            output_dir: Directory to write the output file.

        Returns:
            Path to the generated .yml file.
        """
        output_dir = Path(output_dir)
        output_dir.mkdir(parents=True, exist_ok=True)

        _check_supported_solver_options(spec)

        # Build the complete YAML document by merging all sections
        data: dict[str, Any] = {}

        # Units
        data.update(_build_units_section())

        # General / calculation
        data.update(_build_general_section(spec))

        # Environment
        data.update(_build_environment_section(spec))

        # Frequencies
        data.update(_build_frequencies_section(spec))

        # Headings
        data.update(_build_headings_section(spec))

        # Bodies
        data.update(_build_bodies_section(spec))

        # Outputs / field points
        data.update(_build_outputs_section(spec))

        # Damping lid (for moonpool bodies)
        data.update(_build_damping_lid_section(spec))

        # QTF section (free surface zone, QTF method)
        data.update(_build_qtf_section(spec))

        # Determine filename
        name = self._analysis_name(spec)
        output_path = output_dir / f"{name}.yml"
        return _write_yml(data, output_path)

    def generate_modular(
        self,
        spec: DiffractionSpec,
        output_dir: Path,
    ) -> Path:
        """Generate modular OrcaWave .yml files (one per section).

        Creates:
            01_general.yml, 02_units.yml, 03_environment.yml,
            04_bodies.yml, 05_frequencies.yml, 06_headings.yml,
            07_solver.yml, 08_outputs.yml, and master.yml.

        Args:
            spec: Canonical diffraction specification.
            output_dir: Directory to write the output files.

        Returns:
            Path to the master.yml file.
        """
        output_dir = Path(output_dir)
        output_dir.mkdir(parents=True, exist_ok=True)

        _check_supported_solver_options(spec)

        sections = [
            ("01_general.yml", _build_general_section(spec)),
            ("02_units.yml", _build_units_section()),
            ("03_environment.yml", _build_environment_section(spec)),
            ("04_bodies.yml", _build_bodies_section(spec)),
            ("05_frequencies.yml", _build_frequencies_section(spec)),
            ("06_headings.yml", _build_headings_section(spec)),
            ("07_solver.yml", _build_solver_section(spec)),
            ("08_outputs.yml", _build_outputs_section(spec)),
        ]

        section_files = []
        for filename, section_data in sections:
            _write_yml(section_data, output_dir / filename)
            section_files.append(filename)

        # Write master file that references all section files
        master_data: dict[str, Any] = {
            "# OrcaWave modular project": None,
            "# Section files": section_files,
        }

        # For the master, merge all sections into one loadable file
        merged: dict[str, Any] = {}
        merged.update(_build_units_section())
        merged.update(_build_general_section(spec))
        merged.update(_build_environment_section(spec))
        merged.update(_build_frequencies_section(spec))
        merged.update(_build_headings_section(spec))
        merged.update(_build_bodies_section(spec))
        merged.update(_build_outputs_section(spec))
        merged.update(_build_damping_lid_section(spec))
        merged.update(_build_qtf_section(spec))

        master_path = output_dir / "master.yml"
        return _write_yml(merged, master_path)

    @staticmethod
    def _analysis_name(spec: DiffractionSpec) -> str:
        """Derive analysis name from the spec."""
        bodies = spec.get_bodies()
        if len(bodies) == 1:
            return bodies[0].vessel.name
        if spec.metadata and spec.metadata.project:
            return spec.metadata.project
        return "orcawave_project"
