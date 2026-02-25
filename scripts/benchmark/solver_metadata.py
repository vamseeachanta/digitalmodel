"""Auto-generate solver metadata from spec.yml for benchmark reports.

Replaces hand-built ``_build_*_metadata()`` functions with a single reusable
function: ``build_solver_metadata(spec, solver_name) -> dict``.

The returned dict maps directly to the keys expected by
``benchmark_plotter.py:_PARAM_ROWS``, ensuring every input section row in the
HTML report is populated for any hull.

Usage::

    from scripts.benchmark.solver_metadata import build_solver_metadata

    spec = yaml.safe_load(open("spec.yml"))
    meta = build_solver_metadata(spec, "AQWA")
    # -> {"AQWA": {...}, "OrcaWave": {...}}

    # Or for a single solver:
    aqwa_meta = build_solver_metadata(spec, "AQWA")["AQWA"]
"""

from __future__ import annotations

import math
from pathlib import Path
from typing import Any

import yaml


# ---------------------------------------------------------------------------
# Internal helpers (copied from audit_solver_inputs.py since scripts aren't
# importable as packages)
# ---------------------------------------------------------------------------


def _compute_radii(
    mass: float, inertia_tensor: dict[str, float]
) -> tuple[float, float, float]:
    """Compute (kxx, kyy, kzz) from mass and inertia tensor dict."""
    result = []
    for key in ("Ixx", "Iyy", "Izz"):
        i_val = inertia_tensor.get(key, 0.0)
        if mass > 0 and i_val > 0:
            result.append(math.sqrt(i_val / mass))
        else:
            result.append(0.0)
    return result[0], result[1], result[2]


def _matrix_nonzero_summary(matrix: list[list[float]] | None) -> str:
    """Summarise non-zero entries in a 6x6 damping/stiffness matrix.

    Returns a human-readable string like ``"FIDP M44=36,010 kN-m-s/rad"``,
    or ``"None applied"`` if the matrix is all zeros or absent.
    """
    if matrix is None:
        return "None applied"

    dof_labels = ["Surge", "Sway", "Heave", "Roll", "Pitch", "Yaw"]
    entries = []
    for i, row in enumerate(matrix):
        for j, val in enumerate(row):
            if val != 0.0:
                entries.append(
                    f"M{i + 1}{j + 1}({dof_labels[i]}-{dof_labels[j]})"
                    f"={val:,.0f}"
                )
    if not entries:
        return "None applied"
    return "; ".join(entries)


# ---------------------------------------------------------------------------
# Spec extraction (mirrors audit_solver_inputs._extract_params)
# ---------------------------------------------------------------------------


def _extract_from_spec(spec: dict[str, Any]) -> dict[str, Any]:
    """Extract all parameters from a loaded spec dict.

    Returns a flat dict with typed values (not formatted strings).
    """
    vessel = spec.get("vessel", {})
    # DiffractionSpec uses "bodies" instead of "vessel"
    if not vessel and "bodies" in spec:
        bodies = spec["bodies"]
        if isinstance(bodies, list) and len(bodies) > 0:
            vessel = bodies[0].get("vessel", {})
    inertia = vessel.get("inertia", {})
    geometry = vessel.get("geometry", {})
    dims = geometry.get("dimensions", {})
    env = spec.get("environment", {})
    solver_opts = spec.get("solver_options", {})

    mass = inertia.get("mass", 0.0)
    cog = inertia.get("centre_of_gravity", [0.0, 0.0, 0.0])

    # Radii of gyration: explicit or computed from inertia tensor
    radii_raw = inertia.get("radii_of_gyration")
    tensor = inertia.get("inertia_tensor")

    if radii_raw is not None:
        kxx, kyy, kzz = radii_raw[0], radii_raw[1], radii_raw[2]
    elif tensor is not None and mass > 0:
        kxx, kyy, kzz = _compute_radii(mass, tensor)
    else:
        kxx = kyy = kzz = 0.0

    ext_damp = vessel.get("external_damping")

    return {
        "mass_kg": mass,
        "cog": cog,
        "kxx": kxx,
        "kyy": kyy,
        "kzz": kzz,
        "length": dims.get("length"),
        "beam": dims.get("beam"),
        "draft": dims.get("draft"),
        "water_depth": env.get("water_depth", 0.0),
        "water_density": env.get("water_density", 0.0),
        "gravity": env.get("gravity", 9.80665),
        "mesh_file": geometry.get("mesh_file", ""),
        "mesh_format": geometry.get("mesh_format", ""),
        "symmetry": geometry.get("symmetry", "none"),
        "external_damping": ext_damp,
        "remove_irregular_frequencies": solver_opts.get(
            "remove_irregular_frequencies", False
        ),
        "qtf_calculation": solver_opts.get("qtf_calculation", False),
        "precision": solver_opts.get("precision", "double"),
    }


# ---------------------------------------------------------------------------
# Solver-specific formatting
# ---------------------------------------------------------------------------

# AQWA mesh format mapping
_AQWA_MESH_FORMAT = {
    "gdf": "GDF (WAMIT)",
    "dat": "AQWA native",
    "stl": "STL import",
}

# OrcaWave mesh format mapping
_ORCAWAVE_MESH_FORMAT = {
    "gdf": "WAMIT GDF",
    "dat": "AQWA dat import",
    "stl": "STL import",
}

# AQWA calculation method based on analysis_type
_AQWA_CALC_METHOD = {
    "diffraction": "AQWA Diffraction/Radiation",
    "full_qtf": "AQWA Diffraction/Radiation",
}

# OrcaWave calculation method based on analysis_type and qtf
_ORCAWAVE_CALC_METHOD = {
    ("diffraction", False): "Potential + source formulations",
    ("diffraction", True): "Potential + source formulations",
    ("full_qtf", False): "Potential + source formulations",
    ("full_qtf", True): "Full QTF calculation",
}


def _format_body_dimensions(p: dict[str, Any]) -> str:
    """Format body dimensions as 'L x B x T' or descriptive string."""
    length = p.get("length")
    beam = p.get("beam")
    draft = p.get("draft")
    if length is not None and beam is not None and draft is not None:
        return f"{length} x {beam} x {draft}"
    return "\u2014"


def _format_viscous_damping_aqwa(ext_damp: list[list[float]] | None) -> str:
    """Format viscous damping description for AQWA (FIDP-based)."""
    if ext_damp is None:
        return "None applied"
    summary = _matrix_nonzero_summary(ext_damp)
    if summary == "None applied":
        return summary
    return f"FIDP {summary}"


def _format_viscous_damping_orcawave(ext_damp: list[list[float]] | None) -> str:
    """Format viscous damping description for OrcaWave."""
    if ext_damp is None:
        return "None applied"
    summary = _matrix_nonzero_summary(ext_damp)
    if summary == "None applied":
        return summary
    return f"BodyExternalDampingMatrix: {summary}"


def _build_common(p: dict[str, Any]) -> dict[str, str]:
    """Build solver-agnostic common metadata from extracted params."""
    cog = p["cog"]
    return {
        "body_dimensions": _format_body_dimensions(p),
        "centre_of_gravity": f"({', '.join(str(v) for v in cog)})",
        "radii_of_gyration": f"({p['kxx']:.2f}, {p['kyy']:.2f}, {p['kzz']:.2f})",
        "water_density": f"{p['water_density']:.1f}",
        "gravity": f"{p['gravity']}",
        "mesh_file": p["mesh_file"],
        "remove_irregular_frequencies": (
            "Yes (lid method)" if p["remove_irregular_frequencies"] else "No"
        ),
        "qtf_calculation": "Yes" if p["qtf_calculation"] else "No",
        "precision": str(p["precision"]),
    }


def _build_aqwa_meta(
    p: dict[str, Any],
    common: dict[str, str],
    analysis_type: str,
    *,
    input_file: str | None = None,
) -> dict[str, str]:
    """Build AQWA-specific metadata dict."""
    mass_kg = p["mass_kg"]
    fmt = p["mesh_format"].lower()
    ext_damp = p["external_damping"]

    has_fidp = (
        ext_damp is not None
        and any(val != 0.0 for row in ext_damp for val in row)
    )

    meta = {
        **common,
        "mass": f"{mass_kg:,.0f} kg (SI)",
        "mesh_format": _AQWA_MESH_FORMAT.get(fmt, fmt.upper()),
        "mesh_symmetry": p["symmetry"].capitalize() if p["symmetry"] != "none" else "None",
        "calculation_method": _AQWA_CALC_METHOD.get(
            analysis_type, "AQWA Diffraction/Radiation"
        ),
        "raw_phase_convention": "ISO 6954 (phase lead)",
        "radiation_damping": "Computed (BEM)",
        "viscous_damping": _format_viscous_damping_aqwa(ext_damp),
        "damping_lid": "None",
        "remove_irregular_frequencies": "N/A (AQWA built-in)",
        "fidp_applied": "Yes" if has_fidp else "No",
    }
    if input_file is not None:
        meta["input_file"] = input_file
    return meta


def _build_orcawave_meta(
    p: dict[str, Any],
    common: dict[str, str],
    analysis_type: str,
    *,
    input_file: str | None = None,
) -> dict[str, str]:
    """Build OrcaWave-specific metadata dict."""
    mass_te = p["mass_kg"] / 1000.0
    fmt = p["mesh_format"].lower()
    ext_damp = p["external_damping"]
    qtf = p["qtf_calculation"]

    meta = {
        **common,
        "mass": f"{mass_te:,.1f} te (OrcaFlex units)",
        "mesh_format": _ORCAWAVE_MESH_FORMAT.get(fmt, fmt.upper()),
        "mesh_symmetry": "None",
        "calculation_method": _ORCAWAVE_CALC_METHOD.get(
            (analysis_type, qtf), "Potential + source formulations"
        ),
        "raw_phase_convention": "Orcina (phase lag)",
        "radiation_damping": "Computed (BEM)",
        "viscous_damping": _format_viscous_damping_orcawave(ext_damp),
        "damping_lid": "None",
    }
    if input_file is not None:
        meta["input_file"] = input_file
    return meta


def _build_wamit_meta(
    p: dict[str, Any],
    common: dict[str, str],
    analysis_type: str,
    *,
    input_file: str | None = None,
    wamit_version: str = "v7.3",
) -> dict[str, str]:
    """Build WAMIT-specific metadata dict."""
    mass_kg = p["mass_kg"]

    meta = {
        **common,
        "mass": f"{mass_kg:,.0f} kg (SI)",
        "mesh_format": "WAMIT GDF (native)",
        "mesh_symmetry": p["symmetry"].capitalize() if p["symmetry"] != "none" else "None",
        "calculation_method": f"WAMIT {wamit_version} (linear BEM)",
        "raw_phase_convention": "WAMIT (complex amplitude)",
        "radiation_damping": "Computed (BEM)",
        "viscous_damping": "None applied",
        "damping_lid": "None",
    }
    if input_file is not None:
        meta["input_file"] = input_file
    return meta


# ---------------------------------------------------------------------------
# OrcaWave YAML metadata extraction
# ---------------------------------------------------------------------------


def _read_orcawave_yml(yml_path: Path) -> dict[str, Any]:
    """Read an OrcaWave SaveData() YAML with encoding fallback.

    OrcaWave SaveData() produces multi-document YAML (``---`` separators)
    with a ``%YAML 1.1`` header. This function merges all documents into
    a single dict (Bodies list is concatenated across documents).
    """
    for encoding in ("utf-8", "latin-1"):
        try:
            with open(yml_path, encoding=encoding) as f:
                content = f.read()
            docs = list(yaml.safe_load_all(content))
            if not docs:
                return {}
            # Merge all documents — later docs extend Bodies list
            merged: dict[str, Any] = {}
            for doc in docs:
                if not isinstance(doc, dict):
                    continue
                bodies = doc.pop("Bodies", None)
                merged.update(doc)
                if bodies and isinstance(bodies, list):
                    merged.setdefault("Bodies", []).extend(bodies)
            return merged
        except (UnicodeDecodeError, OSError):
            continue
    return {}


def _fmt_freq_heading_range(
    values: list | None, label: str,
) -> str:
    """Format a list of numeric values as 'min – max (count)'."""
    if not values or not isinstance(values, list):
        return "-"
    nums = [float(v) for v in values if isinstance(v, (int, float))]
    if not nums:
        return "-"
    return f"{min(nums):.4g} – {max(nums):.4g} ({len(nums)})"


def build_orcawave_metadata_from_yml(
    yml_path: Path | str,
    body_index: int = 0,
) -> dict[str, str]:
    """Extract comprehensive OrcaWave parameters from a SaveData() YAML.

    Parameters
    ----------
    yml_path:
        Path to the OrcaWave project YAML exported by ``SaveData()``.
    body_index:
        Which body to extract details from (0-based).

    Returns
    -------
    Flat dict with display-ready string values keyed to match
    ``benchmark_plotter._PARAM_ROWS``.
    """
    yml_path = Path(yml_path)
    if not yml_path.exists():
        return {}

    data = _read_orcawave_yml(yml_path)
    if not data:
        return {}

    meta: dict[str, str] = {}
    meta["input_file"] = str(yml_path)

    # -- General settings --
    meta["ow_units_system"] = str(data.get("UnitsSystem", "-"))
    meta["ow_solve_type"] = str(data.get("SolveType", "-"))
    meta["ow_load_rao_method"] = str(
        data.get("LoadRAOCalculationMethod", "-")
    )
    meta["ow_linear_solver"] = str(data.get("LinearSolverMethod", "-"))
    meta["ow_divide_nonplanar"] = str(
        data.get("DivideNonPlanarPanels", "-")
    )
    meta["ow_length_tolerance"] = str(data.get("LengthTolerance", "-"))
    meta["ow_waves_referred_to_by"] = str(
        data.get("WavesReferredToBy", "-")
    )

    # -- Environment --
    wd = data.get("WaterDepth")
    if wd is not None:
        meta["water_depth"] = str(wd)
    dens = data.get("WaterDensity")
    if dens is not None:
        meta["water_density"] = str(dens)

    # -- Frequencies / Headings --
    periods = data.get("PeriodOrFrequency")
    meta["ow_freq_range"] = _fmt_freq_heading_range(periods, "freq")
    meta["ow_freq_count"] = str(len(periods)) if isinstance(periods, list) else "-"

    headings = data.get("WaveHeading")
    meta["ow_heading_range"] = _fmt_freq_heading_range(headings, "heading")
    meta["ow_heading_count"] = str(len(headings)) if isinstance(headings, list) else "-"

    # -- Body details --
    bodies = data.get("Bodies", [])
    if not isinstance(bodies, list):
        bodies = []
    meta["ow_body_count"] = str(len(bodies))

    if body_index < len(bodies):
        body = bodies[body_index]
        meta["ow_body_name"] = str(body.get("BodyName", "-"))
        meta["mesh_file"] = str(body.get("BodyMeshFileName", "-"))
        meta["mesh_format"] = str(body.get("BodyMeshFormat", "-"))
        meta["mesh_symmetry"] = str(body.get("BodyMeshSymmetry", "-"))
        meta["ow_inertia_spec"] = str(
            body.get("BodyInertiaSpecifiedBy", "-")
        )
        meta["ow_cog_z"] = str(
            body.get("BodyCentreOfMassZRelativeToFreeSurface", "-")
        )

        # Radii of gyration
        radii = body.get("BodyRadiiOfGyration")
        if isinstance(radii, list) and len(radii) >= 3:
            # OrcaWave stores as flat 3x3; diagonal is [0][0], [1][1], [2][2]
            if isinstance(radii[0], list):
                diag = [radii[i][i] for i in range(min(3, len(radii)))]
            else:
                diag = radii[:3]
            meta["radii_of_gyration"] = (
                f"({diag[0]:.2f}, {diag[1]:.2f}, {diag[2]:.2f})"
            )

        # Interior surface panels
        add_interior = body.get("BodyAddInteriorSurfacePanels", "-")
        method = body.get("BodyInteriorSurfacePanelMethod", "")
        meta["ow_interior_panels"] = str(add_interior)
        if method and str(add_interior).lower() == "yes":
            meta["ow_interior_panels"] += f" ({method})"

        # Connection parent
        meta["ow_connection_parent"] = str(
            body.get("BodyConnectionParent", "None")
        )

        # Fixed DOFs
        fixed_dofs = body.get("BodyFixedDOFs")
        if isinstance(fixed_dofs, list):
            active = [d for d in fixed_dofs if d]
            meta["ow_fixed_dofs"] = ", ".join(active) if active else "None"
        elif fixed_dofs:
            meta["ow_fixed_dofs"] = str(fixed_dofs)
        else:
            meta["ow_fixed_dofs"] = "None"

        # Roll damping target
        meta["ow_roll_damping_target"] = str(
            body.get("BodyIncreaseRollDampingToTarget", "-")
        )

        # External damping matrix
        ext_damp = body.get("BodyExternalDampingMatrix")
        if ext_damp is not None and isinstance(ext_damp, list):
            meta["ow_external_damping"] = _matrix_nonzero_summary(ext_damp)
        else:
            meta["ow_external_damping"] = "None applied"

        # External stiffness matrix
        ext_stiff = body.get("BodyExternalStiffnessMatrix")
        if ext_stiff is not None and isinstance(ext_stiff, list):
            meta["ow_external_stiffness"] = _matrix_nonzero_summary(
                ext_stiff
            )
        else:
            meta["ow_external_stiffness"] = "None applied"

        # Mass from body
        mass = body.get("BodyMass")
        if mass is not None:
            meta["mass"] = f"{float(mass):,.1f} te"

        # Centre of gravity
        cog_x = body.get("BodyCentreOfMassX", 0)
        cog_y = body.get("BodyCentreOfMassY", 0)
        cog_z = body.get("BodyCentreOfMassZRelativeToFreeSurface", 0)
        meta["centre_of_gravity"] = f"({cog_x}, {cog_y}, {cog_z})"

    # -- Damping lid (conditional) --
    has_lid = data.get("HasResonanceDampingLid", "No")
    meta["ow_damping_lid"] = str(has_lid)
    # YAML 1.1 parses Yes/No as True/False booleans
    lid_active = str(has_lid).lower() in ("yes", "true")
    if lid_active:
        meta["ow_damping_lid_mesh"] = str(
            data.get("DampingLidMeshFileName", "-")
        )
        meta["ow_damping_epsilon"] = str(
            data.get("DampingFactorEpsilon", "-")
        )
    else:
        meta["ow_damping_lid_mesh"] = "-"
        meta["ow_damping_epsilon"] = "-"

    # -- QTF (conditional) --
    solve_type = str(data.get("SolveType", "")).lower()
    if "qtf" in solve_type:
        meta["ow_qtf_method"] = str(
            data.get("QTFCalculationMethod", "-")
        )
        meta["ow_qtf_freq_types"] = str(
            data.get("QTFFrequencyTypes", "-")
        )
        min_ca = data.get("QTFMinCrossingAngle")
        max_ca = data.get("QTFMaxCrossingAngle")
        if min_ca is not None and max_ca is not None:
            meta["ow_qtf_crossing_angles"] = f"{min_ca} – {max_ca}"
        else:
            meta["ow_qtf_crossing_angles"] = "-"
        meta["ow_qtf_mean_drift"] = str(
            data.get("IncludeMeanDriftFullQTFs", "-")
        )
    else:
        meta["ow_qtf_method"] = "-"
        meta["ow_qtf_freq_types"] = "-"
        meta["ow_qtf_crossing_angles"] = "-"
        meta["ow_qtf_mean_drift"] = "-"

    # -- Free surface zone (conditional) --
    fsz_type = data.get("FreeSurfacePanelledZoneType")
    if fsz_type and str(fsz_type).lower() != "none":
        meta["ow_fsz_type"] = str(fsz_type)
        meta["ow_fsz_mesh"] = str(
            data.get("FreeSurfacePanelledZoneMeshFileName", "-")
        )
        meta["ow_fsz_inner_radius"] = str(
            data.get("FreeSurfacePanelledZoneInnerRadius", "-")
        )
    else:
        meta["ow_fsz_type"] = "-"
        meta["ow_fsz_mesh"] = "-"
        meta["ow_fsz_inner_radius"] = "-"

    # Backward compat keys
    meta["calculation_method"] = meta.get("ow_solve_type", "-")
    meta["raw_phase_convention"] = "Orcina (phase lag)"
    meta["radiation_damping"] = "Computed (BEM)"
    meta["viscous_damping"] = meta.get("ow_external_damping", "None applied")
    meta["damping_lid"] = meta.get("ow_damping_lid", "No")

    return meta


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------


def build_solver_metadata(
    spec: dict[str, Any],
    *,
    aqwa_input_file: str | None = None,
    orcawave_input_file: str | None = None,
    wamit_input_file: str | None = None,
    wamit_version: str = "v7.3",
    panel_count: str | None = None,
    spec_dir: Path | None = None,
) -> dict[str, dict[str, str]]:
    """Build complete solver metadata for AQWA, OrcaWave, and WAMIT from a spec dict.

    Parameters
    ----------
    spec:
        Loaded spec.yml dict (from ``yaml.safe_load``).
    aqwa_input_file:
        Optional path to the AQWA .LIS file, set as ``input_file`` for AQWA.
    orcawave_input_file:
        Optional path to the OrcaWave input, set as ``input_file`` for OrcaWave.
    wamit_input_file:
        Optional path to a WAMIT input file, set as ``input_file`` for WAMIT.
    wamit_version:
        WAMIT version string used in the calculation method description.
        Defaults to ``"v7.3"``.
    panel_count:
        Optional panel count string (e.g. ``"912"``). If not provided, the
        key is set to ``"\u2014"`` (em-dash).
    spec_dir:
        Optional directory containing the spec.yml, used to resolve
        ``mesh_path`` relative to the spec file.

    Returns
    -------
    dict with keys ``"AQWA"``, ``"OrcaWave"``, and ``"WAMIT"``, each mapping
    to a metadata dict compatible with ``benchmark_plotter._PARAM_ROWS``.
    """
    analysis_type = spec.get("analysis_type", "diffraction")
    p = _extract_from_spec(spec)
    common = _build_common(p)

    aqwa_meta = _build_aqwa_meta(
        p, common, analysis_type, input_file=aqwa_input_file
    )
    orcawave_meta = _build_orcawave_meta(
        p, common, analysis_type, input_file=orcawave_input_file
    )
    wamit_meta = _build_wamit_meta(
        p, common, analysis_type,
        input_file=wamit_input_file,
        wamit_version=wamit_version,
    )

    # Panel count
    pc = panel_count if panel_count is not None else "\u2014"
    aqwa_meta["panel_count"] = pc
    orcawave_meta["panel_count"] = pc
    wamit_meta["panel_count"] = pc

    # Resolve mesh_path if spec_dir is available
    if spec_dir is not None and p["mesh_file"]:
        mesh_path = spec_dir / p["mesh_file"]
        if mesh_path.exists():
            resolved = str(mesh_path)
            aqwa_meta["mesh_path"] = resolved
            orcawave_meta["mesh_path"] = resolved
            wamit_meta["mesh_path"] = resolved

    return {"AQWA": aqwa_meta, "OrcaWave": orcawave_meta, "WAMIT": wamit_meta}
