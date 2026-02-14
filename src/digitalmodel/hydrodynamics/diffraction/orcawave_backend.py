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

Reference: OrcaWave 11.x YAML format (see docs/modules/orcawave/examples).
"""

from __future__ import annotations

import math
from pathlib import Path
from typing import Any

import yaml

from digitalmodel.hydrodynamics.diffraction.input_schemas import (
    BodySpec,
    DiffractionSpec,
    FrequencyInputType,
    LoadRAOMethod,
    SymmetryType,
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
        periods = [2.0 * math.pi / float(w) for w in freqs_rad_s if w > 0]

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
    return spec.environment.water_density / 1000.0


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
        ixx = t.get("Ixx", 0.0) / 1000.0
        iyy = t.get("Iyy", 0.0) / 1000.0
        izz = t.get("Izz", 0.0) / 1000.0
        ixy = t.get("Ixy", 0.0) / 1000.0
        ixz = t.get("Ixz", 0.0) / 1000.0
        iyz = t.get("Iyz", 0.0) / 1000.0
        return [
            [ixx, ixy, ixz],
            [ixy, iyy, iyz],
            [ixz, iyz, izz],
        ]
    else:
        rog = inertia.radii_of_gyration
        mass_t = mass_kg / 1000.0
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

    # Interior surface panels (for irregular frequency removal)
    add_interior = spec.solver_options.remove_irregular_frequencies
    body["BodyAddInteriorSurfacePanels"] = _bool_to_yn(add_interior)
    if add_interior:
        body["BodyInteriorSurfacePanelMethod"] = "Triangulation method"

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
        body["BodyMass"] = inertia.mass / 1000.0  # kg -> tonnes

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


# ---------------------------------------------------------------------------
# Section builders
# ---------------------------------------------------------------------------


def _build_general_section(spec: DiffractionSpec) -> dict[str, Any]:
    """Build the general/calculation section."""
    solver = spec.solver_options
    method = _LOAD_RAO_METHOD_MAP.get(solver.load_rao_method.value, "Both")

    _SOLVE_TYPE_MAP = {
        "potential_only": "Potential formulation only",
        "potential_and_source": "Potential and source formulations",
        "mean_drift": "Potential and source + mean drift (momentum conservation)",
        "diagonal_qtf": "Potential and source + diagonal QTF",
        "full_qtf": "Potential and source + full QTF",
    }

    section: dict[str, Any] = {}
    solve_type_key = getattr(solver, "solve_type", "potential_and_source")
    section["SolveType"] = _SOLVE_TYPE_MAP.get(
        solve_type_key, "Potential and source formulations"
    )
    # "Potential formulation only" makes quadratic load and
    # OutputPanelVelocities dormant — only emit when source formulations active
    has_source = solve_type_key != "potential_only"

    section["LoadRAOCalculationMethod"] = method
    section["PreferredLoadRAOCalculationMethod"] = (
        "Haskind" if method in ("Both", "Haskind") else "Direct"
    )
    if has_source:
        section["QuadraticLoadPressureIntegration"] = _bool_to_yn(
            solver.qtf_calculation
        )
    section["QuadraticLoadControlSurface"] = _bool_to_yn(
        solver.qtf_calculation
    )
    section["QuadraticLoadMomentumConservation"] = "No"
    if solver.qtf_calculation:
        section["PreferredQuadraticLoadCalculationMethod"] = "Control surface"
    section["HasResonanceDampingLid"] = "No"
    section["LengthTolerance"] = 100e-9
    section["WaterlineZTolerance"] = 1e-6
    section["WaterlineGapTolerance"] = 1e-6
    section["DivideNonPlanarPanels"] = "Yes"
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
    if spec.solver_options and spec.solver_options.qtf_calculation:
        section["QTFMinCrossingAngle"] = 0
        section["QTFMaxCrossingAngle"] = 180
    return section


def _build_solver_section(spec: DiffractionSpec) -> dict[str, Any]:
    """Build the solver settings section (subset of general for modularity)."""
    solver = spec.solver_options
    return {
        "LinearSolverMethod": "Direct LU",
        "SolverPrecision": solver.precision.value,
    }


def _build_outputs_section(spec: DiffractionSpec) -> dict[str, Any]:
    """Build the outputs section."""
    solve_type = getattr(spec.solver_options, "solve_type", "potential_and_source")
    has_source = solve_type != "potential_only"

    section: dict[str, Any] = {}
    # OutputPanelVelocities is dormant for "Potential formulation only"
    if has_source:
        section["OutputPanelVelocities"] = "No"
    section["DetectAndSkipFieldPointsInsideBodies"] = "Yes"
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
