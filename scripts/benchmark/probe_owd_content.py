#!/usr/bin/env python3
"""Phase 0 feasibility probe for .owd WAMIT reference extraction.

Inspects OrcaWave .owd files to determine what data is accessible via
OrcFxAPI.Diffraction for validation benchmarking.

Usage:
    uv run python scripts/benchmark/probe_owd_content.py
    uv run python scripts/benchmark/probe_owd_content.py --owd-path path/to/file.owd
"""
from __future__ import annotations

import argparse
import sys
from pathlib import Path
from typing import Any

# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

REPO_ROOT = Path(__file__).parent.parent.parent

DEFAULT_OWD_PATH = (
    REPO_ROOT
    / "docs"
    / "modules"
    / "orcawave"
    / "L00_validation_wamit"
    / "2.7"
    / "OrcaWave v11.0 files"
    / "Pyramid.owd"
)

# Attributes we specifically want to probe for comparison/reference data
COMPARISON_KEYWORDS = [
    "comparison",
    "external",
    "overlay",
    "reference",
    "wamit",
    "import",
    "validation",
    "user",
    "custom",
    "curve",
    "dataset",
    "table",
    "series",
]


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


def _safe_call(obj: Any, method_name: str, *args: Any, **kwargs: Any) -> Any:
    """Call a method on obj, returning the result or an error string."""
    try:
        method = getattr(obj, method_name)
        if callable(method):
            return method(*args, **kwargs)
        return method
    except Exception as exc:
        return f"<error: {exc}>"


def _classify_attribute(name: str, value: Any) -> str:
    """Classify an attribute into a human-readable category."""
    if callable(value):
        return "method"
    type_name = type(value).__name__
    if isinstance(value, (list, tuple)):
        length = len(value)
        if length > 0:
            inner_type = type(value[0]).__name__
            return f"{type_name}[{inner_type}] len={length}"
        return f"{type_name} len=0"
    if isinstance(value, (int, float, bool, str)):
        return f"{type_name} = {value!r}"
    return type_name


def _inspect_array_shape(name: str, value: Any) -> str:
    """Describe shape/size of array-like data."""
    try:
        if hasattr(value, "shape"):
            return f"shape={value.shape}"
        if isinstance(value, (list, tuple)):
            length = len(value)
            if length > 0 and isinstance(value[0], (list, tuple)):
                return f"shape=({length}, {len(value[0])})"
            return f"len={length}"
    except Exception:
        pass
    return ""


def _matches_comparison_keywords(name: str) -> bool:
    """Check if an attribute name matches comparison/reference keywords."""
    name_lower = name.lower()
    return any(kw in name_lower for kw in COMPARISON_KEYWORDS)


# ---------------------------------------------------------------------------
# Probe functions
# ---------------------------------------------------------------------------


def probe_load_data(owd_path: Path) -> dict[str, Any]:
    """Try LoadData on the .owd file and inspect project-level data."""
    import OrcFxAPI

    report = {
        "load_data_success": False,
        "load_data_error": None,
    }

    try:
        diffraction = OrcFxAPI.Diffraction()
        diffraction.LoadData(str(owd_path.resolve()))
        report["load_data_success"] = True
    except Exception as exc:
        report["load_data_error"] = str(exc)
        return report

    report.update(_inspect_diffraction_object(diffraction, "LoadData"))
    return report


def probe_load_results(owd_path: Path) -> dict[str, Any]:
    """Try LoadResults on the .owd file (solved results)."""
    import OrcFxAPI

    report = {
        "load_results_success": False,
        "load_results_error": None,
    }

    try:
        diffraction = OrcFxAPI.Diffraction()
        diffraction.LoadResults(str(owd_path.resolve()))
        report["load_results_success"] = True
    except Exception as exc:
        report["load_results_error"] = str(exc)
        return report

    report.update(_inspect_diffraction_object(diffraction, "LoadResults"))
    return report


def probe_direct_constructor(owd_path: Path) -> dict[str, Any]:
    """Try Diffraction(filename) constructor directly."""
    import OrcFxAPI

    report = {
        "constructor_success": False,
        "constructor_error": None,
    }

    try:
        diffraction = OrcFxAPI.Diffraction(str(owd_path.resolve()))
        report["constructor_success"] = True
    except Exception as exc:
        report["constructor_error"] = str(exc)
        return report

    report.update(_inspect_diffraction_object(diffraction, "Constructor"))
    return report


def _inspect_diffraction_object(
    diffraction: Any, load_method: str
) -> dict[str, Any]:
    """Inspect all attributes on a loaded Diffraction object."""
    report = {}

    # All attributes (non-dunder)
    all_attrs = sorted(
        name for name in dir(diffraction)
        if not name.startswith("_")
    )
    report["all_attributes"] = all_attrs
    report["attribute_count"] = len(all_attrs)

    # Classify each attribute
    classifications = {}
    comparison_candidates = []
    data_arrays = {}
    methods = []
    properties = {}

    for name in all_attrs:
        try:
            value = getattr(diffraction, name)
        except Exception as exc:
            classifications[name] = f"<access error: {exc}>"
            continue

        classification = _classify_attribute(name, value)
        classifications[name] = classification

        if callable(value):
            methods.append(name)
        else:
            properties[name] = classification
            # Check for array-like data
            shape_info = _inspect_array_shape(name, value)
            if shape_info:
                data_arrays[name] = f"{classification} {shape_info}"

        if _matches_comparison_keywords(name):
            comparison_candidates.append((name, classification))

    report["classifications"] = classifications
    report["methods"] = methods
    report["properties"] = properties
    report["data_arrays"] = data_arrays
    report["comparison_candidates"] = comparison_candidates

    # Extract core hydrodynamic data
    report["core_data"] = _extract_core_data(diffraction)

    return report


def _extract_core_data(diffraction: Any) -> dict[str, Any]:
    """Extract core hydrodynamic arrays: frequencies, headings, RAOs, etc."""
    core = {}

    # Frequencies
    freqs = _safe_call(diffraction, "frequencies", None)
    if freqs is None:
        try:
            freqs = diffraction.frequencies
        except Exception:
            freqs = None
    if freqs is not None and not isinstance(freqs, str):
        try:
            core["frequencies_count"] = len(freqs)
            core["frequencies_range"] = (
                f"{min(freqs):.4f} - {max(freqs):.4f}"
                if len(freqs) > 0 else "empty"
            )
            core["frequencies_first_5"] = list(freqs[:5])
        except Exception as exc:
            core["frequencies_error"] = str(exc)

    # Headings
    headings = _safe_call(diffraction, "headings", None)
    if headings is None:
        try:
            headings = diffraction.headings
        except Exception:
            headings = None
    if headings is not None and not isinstance(headings, str):
        try:
            core["headings_count"] = len(headings)
            core["headings_values"] = list(headings)
        except Exception as exc:
            core["headings_error"] = str(exc)

    # Bodies
    for attr_name in ["bodyCount", "BodyCount", "bodies", "Bodies"]:
        val = _safe_call(diffraction, attr_name, None)
        if val is not None and not isinstance(val, str):
            core["body_count_attr"] = attr_name
            core["body_count"] = val
            break

    # Try to access RAOs, added mass, damping
    for data_name in [
        "displacementRAOs",
        "DisplacementRAOs",
        "addedMass",
        "AddedMass",
        "damping",
        "Damping",
        "excitationForce",
        "ExcitationForce",
        "meanDrift",
        "MeanDrift",
        "fieldPoint",
        "FieldPoint",
        "loadRAOs",
        "LoadRAOs",
        "waveElevation",
        "WaveElevation",
    ]:
        val = _safe_call(diffraction, data_name, None)
        if val is not None and not isinstance(val, str):
            shape = _inspect_array_shape(data_name, val)
            core[f"{data_name}_available"] = True
            core[f"{data_name}_info"] = shape or type(val).__name__
        elif isinstance(val, str) and val.startswith("<error"):
            core[f"{data_name}_error"] = val

    return core


# ---------------------------------------------------------------------------
# Deep probe: iterate bodies and DOFs
# ---------------------------------------------------------------------------


def probe_body_details(owd_path: Path) -> dict[str, Any]:
    """If results loaded, probe per-body per-DOF data availability."""
    import OrcFxAPI

    report = {"body_probe_success": False}

    try:
        diffraction = OrcFxAPI.Diffraction(str(owd_path.resolve()))
    except Exception:
        try:
            diffraction = OrcFxAPI.Diffraction()
            diffraction.LoadResults(str(owd_path.resolve()))
        except Exception as exc:
            report["body_probe_error"] = str(exc)
            return report

    report["body_probe_success"] = True

    # Try body count
    body_count = None
    for attr in ["bodyCount", "BodyCount"]:
        try:
            body_count = getattr(diffraction, attr)
            break
        except Exception:
            continue

    if body_count is None:
        # Try via len(bodies)
        for attr in ["bodies", "Bodies"]:
            try:
                bodies = getattr(diffraction, attr)
                body_count = len(bodies)
                break
            except Exception:
                continue

    report["body_count"] = body_count

    # If we have frequencies, try extracting data for body 0, DOF 0
    try:
        freqs = diffraction.frequencies
        headings = diffraction.headings
        report["freq_count"] = len(freqs)
        report["heading_count"] = len(headings)
    except Exception as exc:
        report["freq_heading_error"] = str(exc)
        return report

    # Probe displacement RAOs with various call signatures
    rao_signatures = [
        ("displacementRAOs(0, 0, 0)", lambda: diffraction.displacementRAOs(0, 0, 0)),
        ("displacementRAOs[0][0][0]", lambda: diffraction.displacementRAOs[0][0][0]),
        ("DisplacementRAOs(0, 0, 0)", lambda: diffraction.DisplacementRAOs(0, 0, 0)),
    ]
    for sig_name, sig_fn in rao_signatures:
        try:
            result = sig_fn()
            report[f"rao_access_{sig_name}"] = {
                "success": True,
                "type": type(result).__name__,
                "value": repr(result)[:200],
            }
        except Exception as exc:
            report[f"rao_access_{sig_name}"] = {
                "success": False,
                "error": str(exc)[:200],
            }

    # Probe added mass and damping
    for data_type in ["addedMass", "damping"]:
        sigs = [
            (f"{data_type}(0, 0)", lambda dt=data_type: getattr(diffraction, dt)(0, 0)),
            (f"{data_type}[0][0]", lambda dt=data_type: getattr(diffraction, dt)[0][0]),
        ]
        for sig_name, sig_fn in sigs:
            try:
                result = sig_fn()
                report[f"{data_type}_access_{sig_name}"] = {
                    "success": True,
                    "type": type(result).__name__,
                    "value": repr(result)[:200],
                }
            except Exception as exc:
                report[f"{data_type}_access_{sig_name}"] = {
                    "success": False,
                    "error": str(exc)[:200],
                }

    return report


# ---------------------------------------------------------------------------
# Report formatting
# ---------------------------------------------------------------------------


def print_section(title: str, char: str = "=") -> None:
    """Print a section header."""
    print(f"\n{title}")
    print(char * len(title))


def print_report(
    owd_path: Path,
    load_data_report: dict[str, Any],
    load_results_report: dict[str, Any],
    constructor_report: dict[str, Any],
    body_report: dict[str, Any],
) -> None:
    """Print a structured diagnostic report."""
    print_section("OrcaWave .owd Feasibility Probe Report")
    print(f"File: {owd_path}")
    print(f"Exists: {owd_path.exists()}")
    if owd_path.exists():
        size_mb = owd_path.stat().st_size / (1024 * 1024)
        print(f"Size: {size_mb:.2f} MB")

    # --- LoadData ---
    print_section("1. LoadData() (project data)", "-")
    print(f"  Success: {load_data_report.get('load_data_success')}")
    if load_data_report.get("load_data_error"):
        print(f"  Error: {load_data_report['load_data_error']}")
    _print_data_summary(load_data_report)

    # --- LoadResults ---
    print_section("2. LoadResults() (solved results)", "-")
    print(f"  Success: {load_results_report.get('load_results_success')}")
    if load_results_report.get("load_results_error"):
        print(f"  Error: {load_results_report['load_results_error']}")
    _print_data_summary(load_results_report)

    # --- Constructor ---
    print_section("3. Diffraction(filename) constructor", "-")
    print(f"  Success: {constructor_report.get('constructor_success')}")
    if constructor_report.get("constructor_error"):
        print(f"  Error: {constructor_report['constructor_error']}")
    _print_data_summary(constructor_report)

    # --- Body details ---
    print_section("4. Per-body/DOF data access", "-")
    print(f"  Success: {body_report.get('body_probe_success')}")
    if body_report.get("body_probe_error"):
        print(f"  Error: {body_report['body_probe_error']}")
    if body_report.get("body_count") is not None:
        print(f"  Body count: {body_report['body_count']}")
    if body_report.get("freq_count") is not None:
        print(f"  Frequencies: {body_report['freq_count']}")
        print(f"  Headings: {body_report['heading_count']}")

    # Print RAO/added mass/damping access results
    for key, val in sorted(body_report.items()):
        if key.endswith("_access") or "_access_" in key:
            if isinstance(val, dict):
                status = "OK" if val.get("success") else "FAIL"
                detail = val.get("value", val.get("error", ""))
                print(f"  {key}: [{status}] {detail[:120]}")

    # --- Comparison data candidates ---
    print_section("5. Comparison/reference data candidates", "-")
    found_any = False
    for report_name, report_data in [
        ("LoadData", load_data_report),
        ("LoadResults", load_results_report),
        ("Constructor", constructor_report),
    ]:
        candidates = report_data.get("comparison_candidates", [])
        if candidates:
            found_any = True
            print(f"\n  [{report_name}]")
            for name, classification in candidates:
                print(f"    {name}: {classification}")

    if not found_any:
        print("  No attributes matching comparison/reference keywords found.")
        print("  Keywords searched:", ", ".join(COMPARISON_KEYWORDS))

    # --- Full attribute listing ---
    print_section("6. Complete attribute listing (best load method)", "-")
    # Use whichever loaded successfully, prefer constructor then results then data
    best = None
    best_name = None
    for report_name, report_data in [
        ("Constructor", constructor_report),
        ("LoadResults", load_results_report),
        ("LoadData", load_data_report),
    ]:
        if report_data.get("classifications"):
            best = report_data
            best_name = report_name
            break

    if best:
        print(f"  Source: {best_name}")
        print(f"  Total attributes: {best.get('attribute_count', '?')}")

        print("\n  Methods:")
        for name in best.get("methods", []):
            print(f"    {name}()")

        print("\n  Properties:")
        for name, classification in sorted(best.get("properties", {}).items()):
            print(f"    {name}: {classification}")

        print("\n  Data arrays:")
        for name, info in sorted(best.get("data_arrays", {}).items()):
            print(f"    {name}: {info}")
    else:
        print("  No successful load — cannot list attributes.")

    # --- Core data summary ---
    print_section("7. Core hydrodynamic data summary", "-")
    for report_name, report_data in [
        ("Constructor", constructor_report),
        ("LoadResults", load_results_report),
        ("LoadData", load_data_report),
    ]:
        core = report_data.get("core_data")
        if core:
            print(f"\n  [{report_name}]")
            for key, val in sorted(core.items()):
                print(f"    {key}: {val}")

    # --- Verdict ---
    print_section("8. Feasibility verdict", "-")
    _print_verdict(
        load_data_report, load_results_report,
        constructor_report, body_report,
    )


def _print_data_summary(report: dict[str, Any]) -> None:
    """Print a brief summary of what was found in a load attempt."""
    if not report.get("attribute_count"):
        return
    print(f"  Attributes found: {report['attribute_count']}")
    print(f"  Methods: {len(report.get('methods', []))}")
    print(f"  Properties: {len(report.get('properties', {}))}")
    print(f"  Data arrays: {len(report.get('data_arrays', {}))}")

    core = report.get("core_data", {})
    if core.get("frequencies_count"):
        print(f"  Frequencies: {core['frequencies_count']} ({core.get('frequencies_range', '?')})")
    if core.get("headings_count"):
        print(f"  Headings: {core['headings_count']} -> {core.get('headings_values', '?')}")
    if core.get("body_count"):
        print(f"  Bodies: {core['body_count']}")


def _print_verdict(
    load_data_report: dict[str, Any],
    load_results_report: dict[str, Any],
    constructor_report: dict[str, Any],
    body_report: dict[str, Any],
) -> None:
    """Print the final feasibility assessment."""
    can_load = (
        load_data_report.get("load_data_success")
        or load_results_report.get("load_results_success")
        or constructor_report.get("constructor_success")
    )

    has_frequencies = any(
        r.get("core_data", {}).get("frequencies_count", 0) > 0
        for r in [load_data_report, load_results_report, constructor_report]
    )

    has_raos = any(
        r.get("core_data", {}).get("displacementRAOs_available")
        for r in [load_data_report, load_results_report, constructor_report]
    )

    has_comparison = any(
        len(r.get("comparison_candidates", [])) > 0
        for r in [load_data_report, load_results_report, constructor_report]
    )

    print(f"  Can load .owd file:        {'YES' if can_load else 'NO'}")
    print(f"  Has frequency data:        {'YES' if has_frequencies else 'NO'}")
    print(f"  Has RAO data:              {'YES' if has_raos else 'NO'}")
    print(f"  Has comparison candidates: {'YES' if has_comparison else 'NO'}")

    if has_comparison:
        print("\n  VERDICT: .owd files MAY contain WAMIT reference data.")
        print("  Next step: inspect comparison candidate attributes in detail.")
    elif can_load and has_raos:
        print("\n  VERDICT: .owd files contain OrcaWave results but NO obvious")
        print("  WAMIT reference data attributes. WAMIT data may need to be")
        print("  loaded separately from the Wamit v7.3 output files.")
    elif can_load:
        print("\n  VERDICT: .owd file loads but hydrodynamic data not accessible.")
        print("  The file may contain only project setup (unsolved).")
    else:
        print("\n  VERDICT: Cannot load .owd file. Check OrcFxAPI version and license.")


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------


def main() -> None:
    """Run the .owd feasibility probe."""
    parser = argparse.ArgumentParser(
        description="Probe OrcaWave .owd files for WAMIT reference data accessibility.",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=(
            "Examples:\n"
            "  uv run python scripts/benchmark/probe_owd_content.py\n"
            "  uv run python scripts/benchmark/probe_owd_content.py "
            "--owd-path docs/domains/orcawave/L00_validation_wamit/2.4/"
            '"OrcaWave v11.0 files/Hemisphere.owd"'
        ),
    )
    parser.add_argument(
        "--owd-path",
        type=Path,
        default=DEFAULT_OWD_PATH,
        help="Path to the .owd file to probe (default: Pyramid.owd from case 2.7)",
    )
    args = parser.parse_args()

    owd_path = args.owd_path.resolve()

    print("=" * 72)
    print("Phase 0: OrcaWave .owd Feasibility Probe")
    print("=" * 72)

    # Check file exists
    if not owd_path.exists():
        print(f"\nERROR: File not found: {owd_path}")
        print("Provide a valid .owd path via --owd-path")
        sys.exit(1)

    # Check OrcFxAPI availability
    try:
        import OrcFxAPI
        ver = getattr(OrcFxAPI, "__version__", getattr(OrcFxAPI, "version", "unknown"))
        if callable(ver):
            ver = ver()
        print(f"\nOrcFxAPI version: {ver}")
    except ImportError:
        print("\nERROR: OrcFxAPI is not installed or not importable.")
        print("This script requires OrcFxAPI (the Orcina Python API).")
        print("Install via: pip install OrcFxAPI")
        print("Or ensure the OrcFxAPI package is on your PYTHONPATH.")
        sys.exit(1)

    # Run probes
    print("\nProbing with LoadData()...")
    load_data_report = probe_load_data(owd_path)

    print("Probing with LoadResults()...")
    load_results_report = probe_load_results(owd_path)

    print("Probing with Diffraction(filename)...")
    constructor_report = probe_direct_constructor(owd_path)

    print("Probing per-body data access...")
    body_report = probe_body_details(owd_path)

    # Print report
    print_report(
        owd_path,
        load_data_report,
        load_results_report,
        constructor_report,
        body_report,
    )


if __name__ == "__main__":
    main()
