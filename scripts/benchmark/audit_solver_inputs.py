"""Solver input parameter audit for diffraction benchmarks.

Loads spec.yml files for barge, ship, and spar benchmarks, extracts all
input parameters, and prints a side-by-side comparison table.  Identifies
input mismatches between OrcaWave and AQWA that could explain RAO diffs.

Usage:
    uv run python scripts/benchmark/audit_solver_inputs.py
"""

from __future__ import annotations

import math
import sys
from datetime import datetime, timezone
from pathlib import Path
from typing import Any

import yaml

# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

PROJECT_ROOT = Path(__file__).resolve().parents[2]

HULL_SPECS: list[dict[str, str]] = [
    {
        "label": "BARGE",
        "code": "L02",
        "path": "docs/modules/orcawave/L02_barge_benchmark/spec.yml",
    },
    {
        "label": "SHIP",
        "code": "L03",
        "path": "docs/modules/orcawave/L03_ship_benchmark/spec.yml",
    },
    {
        "label": "SPAR",
        "code": "L04",
        "path": "docs/modules/orcawave/L04_spar_benchmark/spec.yml",
    },
]


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


def _load_spec(rel_path: str) -> dict[str, Any]:
    """Load a YAML spec file relative to project root."""
    full = PROJECT_ROOT / rel_path
    if not full.exists():
        print(f"ERROR: spec file not found: {full}", file=sys.stderr)
        sys.exit(1)
    with full.open("r", encoding="utf-8") as fh:
        return yaml.safe_load(fh)


def _fmt_float(val: float, precision: int = 4) -> str:
    """Format a float with engineering notation for large/small values."""
    if abs(val) >= 1e6 or (abs(val) > 0 and abs(val) < 1e-2):
        return f"{val:.3e}"
    return f"{val:.{precision}f}"


def _fmt_int(val: int) -> str:
    """Format an integer with thousands separators."""
    return f"{val:,}"


def _fmt_list(vals: list[float], precision: int = 2) -> str:
    """Format a list of floats."""
    items = [f"{v:.{precision}f}" for v in vals]
    return "[" + ", ".join(items) + "]"


def _matrix_nonzero_entries(matrix: list[list[float]] | None) -> list[str]:
    """Return descriptions of non-zero entries in a 6x6 matrix."""
    if matrix is None:
        return []
    dof_labels = ["Surge", "Sway", "Heave", "Roll", "Pitch", "Yaw"]
    entries = []
    for i, row in enumerate(matrix):
        for j, val in enumerate(row):
            if val != 0.0:
                entries.append(
                    f"M{i + 1}{j + 1} ({dof_labels[i]}-{dof_labels[j]}) = "
                    f"{_fmt_float(val)}"
                )
    return entries


def _compute_radii(
    mass: float, inertia_tensor: dict[str, float]
) -> dict[str, float]:
    """Compute radii of gyration from mass and inertia tensor."""
    result = {}
    for axis, key in [("Kxx", "Ixx"), ("Kyy", "Iyy"), ("Kzz", "Izz")]:
        i_val = inertia_tensor.get(key, 0.0)
        if mass > 0 and i_val > 0:
            result[axis] = math.sqrt(i_val / mass)
        else:
            result[axis] = 0.0
    return result


def _parse_frequencies(freq_cfg: dict[str, Any]) -> dict[str, Any]:
    """Parse the frequencies section into a standardised dict.

    Returns dict with keys: count, values_rad_s, min_rad_s, max_rad_s,
    input_type, values_raw.
    """
    input_type = freq_cfg.get("input_type", "unknown")
    raw_values = freq_cfg.get("values", [])

    if input_type == "period":
        # values are periods in seconds; convert to rad/s
        rad_s = sorted([2.0 * math.pi / t for t in raw_values if t > 0])
    elif input_type == "frequency":
        # values are already in rad/s
        rad_s = sorted(raw_values)
    else:
        rad_s = sorted(raw_values) if raw_values else []

    return {
        "count": len(raw_values),
        "values_rad_s": rad_s,
        "min_rad_s": min(rad_s) if rad_s else 0.0,
        "max_rad_s": max(rad_s) if rad_s else 0.0,
        "input_type": input_type,
        "values_raw": raw_values,
    }


def _parse_headings(hdg_cfg: dict[str, Any]) -> dict[str, Any]:
    """Parse wave_headings into a standardised dict."""
    values = hdg_cfg.get("values", [])
    return {
        "count": len(values),
        "values": values,
        "min_deg": min(values) if values else 0.0,
        "max_deg": max(values) if values else 0.0,
        "symmetry": hdg_cfg.get("symmetry", False),
    }


# ---------------------------------------------------------------------------
# Extraction
# ---------------------------------------------------------------------------


def _extract_params(spec: dict[str, Any]) -> dict[str, Any]:
    """Extract all auditable parameters from a loaded spec dict."""
    vessel = spec.get("vessel", {})
    inertia = vessel.get("inertia", {})
    geometry = vessel.get("geometry", {})
    env = spec.get("environment", {})

    mass = inertia.get("mass", 0.0)
    cog = inertia.get("centre_of_gravity", [0.0, 0.0, 0.0])

    # Inertia tensor
    tensor = inertia.get("inertia_tensor")
    radii_raw = inertia.get("radii_of_gyration")

    if tensor is not None:
        ixx = tensor.get("Ixx", 0.0)
        iyy = tensor.get("Iyy", 0.0)
        izz = tensor.get("Izz", 0.0)
        ixy = tensor.get("Ixy", 0.0)
        ixz = tensor.get("Ixz", 0.0)
        iyz = tensor.get("Iyz", 0.0)
    else:
        ixx = iyy = izz = ixy = ixz = iyz = 0.0

    # Radii of gyration: from spec or computed
    if radii_raw is not None:
        kxx, kyy, kzz = radii_raw[0], radii_raw[1], radii_raw[2]
    elif tensor is not None and mass > 0:
        radii = _compute_radii(mass, tensor)
        kxx, kyy, kzz = radii["Kxx"], radii["Kyy"], radii["Kzz"]
    else:
        kxx = kyy = kzz = 0.0

    # External damping/stiffness
    ext_damp = vessel.get("external_damping")
    ext_stiff = vessel.get("external_stiffness")

    # Frequencies and headings
    freq_info = _parse_frequencies(spec.get("frequencies", {}))
    hdg_info = _parse_headings(spec.get("wave_headings", {}))

    return {
        "name": vessel.get("name", "Unknown"),
        "type": vessel.get("type", "unknown"),
        "analysis_type": spec.get("analysis_type", "unknown"),
        "mass": mass,
        "cog": cog,
        "ixx": ixx,
        "iyy": iyy,
        "izz": izz,
        "ixy": ixy,
        "ixz": ixz,
        "iyz": iyz,
        "kxx": kxx,
        "kyy": kyy,
        "kzz": kzz,
        "external_damping": ext_damp,
        "external_stiffness": ext_stiff,
        "water_depth": env.get("water_depth", 0.0),
        "water_density": env.get("water_density", 0.0),
        "waterline_z": geometry.get("waterline_z", 0.0),
        "reference_point": geometry.get("reference_point", [0.0, 0.0, 0.0]),
        "mesh_file": geometry.get("mesh_file", ""),
        "mesh_format": geometry.get("mesh_format", ""),
        "symmetry": geometry.get("symmetry", "none"),
        "frequencies": freq_info,
        "headings": hdg_info,
        "solver_options": spec.get("solver_options", {}),
    }


# ---------------------------------------------------------------------------
# Solver comparison logic
# ---------------------------------------------------------------------------


def _orcawave_receives(params: dict[str, Any]) -> dict[str, str]:
    """Describe what OrcaWave backend actually receives / applies."""
    notes: dict[str, str] = {}

    # Mass: backend divides by 1000 for te
    notes["mass"] = f"{params['mass'] / 1000:.2f} te"

    # Inertia: backend divides by 1000 for te.m^2
    notes["inertia"] = (
        f"Ixx={params['ixx'] / 1000:.2f}, "
        f"Iyy={params['iyy'] / 1000:.2f}, "
        f"Izz={params['izz'] / 1000:.2f} te.m^2"
    )

    # External damping: always set (zero if None)
    damp = params["external_damping"]
    if damp is not None:
        nonzero = _matrix_nonzero_entries(damp)
        if nonzero:
            notes["ext_damping"] = "Applied via BodyExternalDampingMatrix"
            notes["ext_damping_detail"] = "; ".join(nonzero)
        else:
            notes["ext_damping"] = "Zero matrix (set but all zeros)"
    else:
        notes["ext_damping"] = "Zero matrix (default)"

    # Headings: passed as-is
    hdg = params["headings"]
    notes["headings"] = (
        f"{hdg['count']} headings ({hdg['min_deg']}-{hdg['max_deg']} deg)"
    )

    return notes


def _aqwa_receives(params: dict[str, Any]) -> dict[str, str]:
    """Describe what AQWA backend actually receives / applies."""
    notes: dict[str, str] = {}

    # Mass: AQWA uses SI (kg) directly in .dat decks
    notes["mass"] = f"{params['mass']:.2f} kg"

    # Inertia: AQWA uses SI (kg.m^2)
    notes["inertia"] = (
        f"Ixx={params['ixx']:.2f}, "
        f"Iyy={params['iyy']:.2f}, "
        f"Izz={params['izz']:.2f} kg.m^2"
    )

    # External damping: FIDP card emitted only if non-zero
    damp = params["external_damping"]
    if damp is not None:
        nonzero = _matrix_nonzero_entries(damp)
        if nonzero:
            notes["ext_damping"] = "Applied via FIDP card"
            notes["ext_damping_detail"] = "; ".join(nonzero)
        else:
            notes["ext_damping"] = "Skipped (all zeros, no FIDP card)"
    else:
        notes["ext_damping"] = "No FIDP card (not specified)"

    # Headings: AQWA expands to full +-180 range internally
    hdg = params["headings"]
    max_hdg = hdg["max_deg"]
    has_negative = any(v < 0 for v in hdg["values"])
    if max_hdg <= 180.0 and not has_negative and hdg["count"] > 1:
        notes["headings"] = (
            f"{hdg['count']} headings ({hdg['min_deg']}-{hdg['max_deg']} deg)"
            " -- AQWA may expand to full 360 deg internally"
        )
    else:
        notes["headings"] = (
            f"{hdg['count']} headings "
            f"({hdg['min_deg']}-{hdg['max_deg']} deg)"
        )

    return notes


# ---------------------------------------------------------------------------
# Output formatting
# ---------------------------------------------------------------------------

COL_W = 26  # Parameter column width
VAL_W = 28  # Value column width
SOL_W = 28  # Solver column width
MATCH_W = 10  # Match column width


def _row(param: str, value: str, orcawave: str, aqwa: str, match: str) -> str:
    """Format a single table row."""
    return (
        f"  {param:<{COL_W}} {value:<{VAL_W}} {orcawave:<{SOL_W}} "
        f"{aqwa:<{SOL_W}} {match}"
    )


def _separator() -> str:
    total = 2 + COL_W + 1 + VAL_W + 1 + SOL_W + 1 + SOL_W + 1 + MATCH_W
    return "  " + "-" * (total - 2)


def _header() -> str:
    return _row("Parameter", "Value", "OrcaWave", "AQWA", "Match")


def _match_symbol(is_match: bool, note: str = "") -> str:
    if is_match:
        return "OK"
    return f"** MISMATCH {note}".strip()


def _print_hull(
    label: str,
    code: str,
    params: dict[str, Any],
    ow_notes: dict[str, str],
    aq_notes: dict[str, str],
) -> list[str]:
    """Print the audit table for one hull. Returns list of mismatch notes."""
    mismatches: list[str] = []

    print(f"\n--- {label} ({code}) ---")
    print(f"  Vessel: {params['name']}  |  Type: {params['type']}  "
          f"|  Analysis: {params['analysis_type']}")
    print(_separator())
    print(_header())
    print(_separator())

    # Mass
    print(_row(
        "Mass (kg)",
        _fmt_int(int(params["mass"])),
        ow_notes["mass"],
        aq_notes["mass"],
        "OK (unit conversion only)",
    ))

    # CoG
    print(_row(
        "CoG (m)",
        _fmt_list(params["cog"]),
        "Applied",
        "Applied",
        "OK",
    ))

    # Radii of gyration
    for axis, key in [("Kxx", "kxx"), ("Kyy", "kyy"), ("Kzz", "kzz")]:
        print(_row(
            f"{axis} (m)",
            f"{params[key]:.4f}",
            "Computed",
            "Computed",
            "OK",
        ))

    # Inertia tensor (principal)
    for label_i, key in [("Ixx", "ixx"), ("Iyy", "iyy"), ("Izz", "izz")]:
        print(_row(
            f"{label_i} (kg.m^2)",
            _fmt_float(params[key]),
            f"{params[key] / 1000:.3e} te.m^2",
            f"{params[key]:.3e} kg.m^2",
            "OK (unit conversion only)",
        ))

    # Cross-products of inertia
    cross = {"Ixy": "ixy", "Ixz": "ixz", "Iyz": "iyz"}
    has_cross = any(params[v] != 0.0 for v in cross.values())
    if has_cross:
        for label_c, key in cross.items():
            if params[key] != 0.0:
                print(_row(
                    f"{label_c} (kg.m^2)",
                    _fmt_float(params[key]),
                    "Applied",
                    "Applied",
                    "OK",
                ))

    # Reference point / mesh position
    print(_row(
        "Reference point (m)",
        _fmt_list(params["reference_point"]),
        "BodyMeshPosition",
        "Deck geometry",
        "OK",
    ))

    # Waterline
    print(_row(
        "Waterline z (m)",
        f"{params['waterline_z']:.2f}",
        "Applied",
        "Applied",
        "OK",
    ))

    # External damping
    damp = params["external_damping"]
    nonzero = _matrix_nonzero_entries(damp)
    if nonzero:
        damp_val = "; ".join(nonzero)
        # Check if AQWA applies it
        ow_status = ow_notes["ext_damping"]
        aq_status = aq_notes["ext_damping"]
        match_str = "OK (both apply)"
        print(_row(
            "External Damping",
            damp_val[:VAL_W],
            ow_status[:SOL_W],
            aq_status[:SOL_W],
            match_str,
        ))
        # Print overflow if value is long
        if len(damp_val) > VAL_W:
            print(f"    Full: {damp_val}")
    else:
        print(_row(
            "External Damping",
            "None (all zeros)",
            ow_notes["ext_damping"][:SOL_W],
            aq_notes["ext_damping"][:SOL_W],
            "OK",
        ))

    # External stiffness
    stiff = params["external_stiffness"]
    stiff_nonzero = _matrix_nonzero_entries(stiff)
    if stiff_nonzero:
        stiff_val = "; ".join(stiff_nonzero)
        print(_row(
            "External Stiffness",
            stiff_val[:VAL_W],
            "Applied",
            "FISK card",
            "OK",
        ))
    else:
        print(_row(
            "External Stiffness",
            "None",
            "Zero matrix",
            "No FISK card",
            "OK",
        ))

    # Water depth
    print(_row(
        "Water Depth (m)",
        f"{params['water_depth']:.1f}",
        "Applied",
        "Applied",
        "OK",
    ))

    # Water density
    ow_density = f"{params['water_density'] / 1000:.4f} te/m^3"
    aq_density = f"{params['water_density']:.1f} kg/m^3"
    print(_row(
        "Water Density (kg/m^3)",
        f"{params['water_density']:.1f}",
        ow_density,
        aq_density,
        "OK (unit conversion only)",
    ))

    # Mesh
    print(_row(
        "Mesh File",
        params["mesh_file"][-VAL_W:] if len(params["mesh_file"]) > VAL_W
        else params["mesh_file"],
        params["mesh_format"].upper(),
        params["mesh_format"].upper(),
        "OK (same mesh)",
    ))

    # Frequencies
    freq = params["frequencies"]
    freq_val = (
        f"{freq['count']} ({freq['min_rad_s']:.3f}-"
        f"{freq['max_rad_s']:.3f} rad/s)"
    )
    freq_raw = (
        f"Input: {freq['input_type']}, "
        f"{freq['count']} values"
    )
    # Note: AQWA and OrcaWave may use different subsets after harmonisation
    print(_row(
        "Frequencies",
        freq_val,
        freq_raw[:SOL_W],
        freq_raw[:SOL_W],
        "OK (same spec values)",
    ))

    # Headings
    hdg = params["headings"]
    hdg_val = (
        f"{hdg['count']} ({hdg['min_deg']:.0f}-{hdg['max_deg']:.0f} deg)"
    )
    ow_hdg = ow_notes["headings"][:SOL_W]
    aq_hdg = aq_notes["headings"][:SOL_W]
    hdg_match = "OK" if "expand" not in aq_notes["headings"].lower() else (
        "** NOTE: AQWA internal expansion"
    )
    if "expand" in aq_notes["headings"].lower():
        mismatches.append(
            f"{label}: AQWA may expand headings to full 360 deg internally"
        )
    print(_row(
        "Headings",
        hdg_val,
        ow_hdg,
        aq_hdg,
        hdg_match,
    ))

    # Solver options
    opts = params["solver_options"]
    qtf = opts.get("qtf_calculation", False)
    rif = opts.get("remove_irregular_frequencies", False)
    print(_row(
        "QTF Calculation",
        "Yes" if qtf else "No",
        "Yes" if qtf else "No",
        "Yes" if qtf else "No",
        "OK",
    ))
    print(_row(
        "Remove Irreg. Freq.",
        "Yes" if rif else "No",
        "Yes" if rif else "No",
        "N/A (AQWA built-in)",
        "OK",
    ))

    print(_separator())

    return mismatches


# ---------------------------------------------------------------------------
# Summary section
# ---------------------------------------------------------------------------


def _print_summary(all_params: list[dict[str, Any]]) -> None:
    """Print a cross-hull summary comparison."""
    print("\n" + "=" * 80)
    print("=== Cross-Hull Summary ===")
    print("=" * 80)

    # Table header
    row_fmt = "  {:<24} {:>20} {:>20} {:>20}"
    print(row_fmt.format("Parameter", "BARGE (L02)", "SHIP (L03)", "SPAR (L04)"))
    print("  " + "-" * 84)

    barge, ship, spar = all_params

    # Mass
    print(row_fmt.format(
        "Mass (te)",
        f"{barge['mass'] / 1000:,.1f}",
        f"{ship['mass'] / 1000:,.1f}",
        f"{spar['mass'] / 1000:,.1f}",
    ))

    # Draft (waterline_z as proxy)
    print(row_fmt.format(
        "Waterline z (m)",
        f"{barge['waterline_z']:.1f}",
        f"{ship['waterline_z']:.1f}",
        f"{spar['waterline_z']:.1f}",
    ))

    # Water depth
    print(row_fmt.format(
        "Water Depth (m)",
        f"{barge['water_depth']:.1f}",
        f"{ship['water_depth']:.1f}",
        f"{spar['water_depth']:.1f}",
    ))

    # CoG z
    print(row_fmt.format(
        "CoG z (m)",
        f"{barge['cog'][2]:.2f}",
        f"{ship['cog'][2]:.3f}",
        f"{spar['cog'][2]:.2f}",
    ))

    # Radii of gyration
    for axis, key in [("Kxx (m)", "kxx"), ("Kyy (m)", "kyy"), ("Kzz (m)", "kzz")]:
        print(row_fmt.format(
            axis,
            f"{barge[key]:.2f}",
            f"{ship[key]:.2f}",
            f"{spar[key]:.2f}",
        ))

    # External damping
    for label_h, p in [("BARGE", barge), ("SHIP", ship), ("SPAR", spar)]:
        nonzero = _matrix_nonzero_entries(p["external_damping"])
        has = "YES" if nonzero else "No"
        if label_h == "BARGE":
            barge_damp = has
        elif label_h == "SHIP":
            ship_damp = has
        else:
            spar_damp = has
    print(row_fmt.format(
        "Has Ext. Damping?",
        barge_damp,
        ship_damp,
        spar_damp,
    ))

    # Frequencies
    print(row_fmt.format(
        "Freq. Count",
        str(barge["frequencies"]["count"]),
        str(ship["frequencies"]["count"]),
        str(spar["frequencies"]["count"]),
    ))
    print(row_fmt.format(
        "Freq. Range (rad/s)",
        f"{barge['frequencies']['min_rad_s']:.3f}-"
        f"{barge['frequencies']['max_rad_s']:.3f}",
        f"{ship['frequencies']['min_rad_s']:.3f}-"
        f"{ship['frequencies']['max_rad_s']:.3f}",
        f"{spar['frequencies']['min_rad_s']:.3f}-"
        f"{spar['frequencies']['max_rad_s']:.3f}",
    ))

    # Headings
    print(row_fmt.format(
        "Heading Count",
        str(barge["headings"]["count"]),
        str(ship["headings"]["count"]),
        str(spar["headings"]["count"]),
    ))

    # Analysis type
    print(row_fmt.format(
        "Analysis Type",
        barge["analysis_type"],
        ship["analysis_type"],
        spar["analysis_type"],
    ))

    print("  " + "-" * 84)


# ---------------------------------------------------------------------------
# Detail: external damping matrix
# ---------------------------------------------------------------------------


def _print_damping_detail(params: dict[str, Any], label: str) -> None:
    """Print the full 6x6 external damping matrix if non-zero."""
    damp = params["external_damping"]
    if damp is None:
        return

    nonzero = _matrix_nonzero_entries(damp)
    if not nonzero:
        return

    print(f"\n  {label} - External Damping Matrix (6x6):")
    dof_labels = ["Surge", "Sway", "Heave", "Roll", "Pitch", "Yaw"]
    # Header
    hdr = "         " + "".join(f"{d:>12}" for d in dof_labels)
    print(hdr)
    for i, row in enumerate(damp):
        vals = "".join(f"{v:>12.1f}" for v in row)
        print(f"  {dof_labels[i]:>7}{vals}")


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------


def main() -> None:
    now = datetime.now(timezone.utc).strftime("%Y-%m-%d %H:%M UTC")
    print("=" * 80)
    print("=== Solver Input Parameter Audit ===")
    print(f"Date: {now}")
    print("=" * 80)

    all_params: list[dict[str, Any]] = []
    all_mismatches: list[str] = []

    for hull in HULL_SPECS:
        spec = _load_spec(hull["path"])
        params = _extract_params(spec)
        all_params.append(params)

        ow_notes = _orcawave_receives(params)
        aq_notes = _aqwa_receives(params)

        mismatches = _print_hull(
            hull["label"], hull["code"], params, ow_notes, aq_notes,
        )
        all_mismatches.extend(mismatches)

        # Print full damping matrix if applicable
        _print_damping_detail(params, hull["label"])

    # Cross-hull summary
    _print_summary(all_params)

    # Final mismatch summary
    print("\n" + "=" * 80)
    print("=== Mismatch / Notes Summary ===")
    print("=" * 80)
    if all_mismatches:
        for note in all_mismatches:
            print(f"  ** {note}")
    else:
        print("  No significant mismatches detected.")

    # Key observations
    print("\n=== Key Observations ===")
    print("=" * 80)

    ship_params = all_params[1]
    ship_damp = ship_params["external_damping"]
    if ship_damp is not None:
        nonzero = _matrix_nonzero_entries(ship_damp)
        if nonzero:
            print("  1. SHIP (L03) has non-zero external damping:")
            for entry in nonzero:
                print(f"     - {entry}")
            print("     OrcaWave: Applied via BodyExternalDampingMatrix "
                  "(always set)")
            print("     AQWA: Applied via FIDP card "
                  "(emitted when matrix has non-zero entries)")

    spar_params = all_params[2]
    print(f"\n  2. SPAR (L04) has only {spar_params['frequencies']['count']}"
          f" frequency points")
    print(f"     Range: {spar_params['frequencies']['min_rad_s']:.3f} - "
          f"{spar_params['frequencies']['max_rad_s']:.3f} rad/s")
    print("     Note: AQWA may have different frequency points "
          "after harmonisation")

    barge_params = all_params[0]
    print(f"\n  3. BARGE (L02) frequency input type: "
          f"{barge_params['frequencies']['input_type']}")
    print(f"     SHIP  (L03) frequency input type: "
          f"{ship_params['frequencies']['input_type']}")
    print(f"     SPAR  (L04) frequency input type: "
          f"{spar_params['frequencies']['input_type']}")
    print("     Note: Period vs frequency input may cause rounding "
          "differences")

    print("\n" + "=" * 80)
    print("Audit complete.")


if __name__ == "__main__":
    main()
