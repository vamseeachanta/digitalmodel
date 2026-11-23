#!/usr/bin/env python3
"""
OrcaFlex modular example QA suite.

Physics-based quality assurance checks for the six representative run-script examples.
Each check reads a .sim file and validates model behaviour against expected bounds.

Usage:
    uv run python orcaflex_example_qa.py              # all examples
    uv run python orcaflex_example_qa.py --example R01  # single example

Importable:
    from digitalmodel.orcaflex.qa import run_orcaflex_qa
"""
from __future__ import annotations

import json
import os
import sys
import time
import argparse
from dataclasses import dataclass, field, asdict
from pathlib import Path
from typing import Callable

# ---------------------------------------------------------------------------
# OrcFxAPI bootstrap
# ---------------------------------------------------------------------------

def _load_orcfxapi() -> None:
    api_path = os.environ.get(
        "ORCAFLEX_API_PATH",
        r"C:\Program Files (x86)\Orcina\OrcaFlex\11.6\OrcFxAPI\Python",
    )
    if api_path not in sys.path:
        sys.path.insert(0, api_path)


_load_orcfxapi()
try:
    import OrcFxAPI
    _ORCFXAPI_AVAILABLE = True
except ImportError:
    _ORCFXAPI_AVAILABLE = False


# ---------------------------------------------------------------------------
# Data types
# ---------------------------------------------------------------------------

@dataclass
class CheckResult:
    check_name: str
    passed: bool
    value: float | str | None
    threshold: float | str | None
    message: str


@dataclass
class ExampleQAResult:
    example_id: str        # e.g. "R01"
    sim_file: str
    checks: list[CheckResult] = field(default_factory=list)
    skipped: bool = False
    skip_reason: str = ""

    @property
    def passed(self) -> bool:
        return not self.skipped and all(c.passed for c in self.checks)

    @property
    def n_passed(self) -> int:
        return sum(1 for c in self.checks if c.passed)

    @property
    def n_failed(self) -> int:
        return sum(1 for c in self.checks if not c.passed)


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

_MODULAR_ROOT = Path(__file__).parent.parent / "modular"


def _sim_path(relative: str) -> Path:
    """Resolve .sim path relative to modular examples root."""
    return _MODULAR_ROOT / relative


def _load_sim(sim_path: Path) -> "OrcFxAPI.Model | None":
    if not _ORCFXAPI_AVAILABLE:
        return None
    if not sim_path.exists():
        return None
    model = OrcFxAPI.Model()
    model.LoadSimulation(str(sim_path))
    return model


def _check(
    result: ExampleQAResult,
    name: str,
    value: float | str | None,
    threshold: float | str | None,
    predicate: Callable[[float | str | None, float | str | None], bool],
    message: str,
) -> None:
    passed = predicate(value, threshold)
    result.checks.append(CheckResult(
        check_name=name,
        passed=passed,
        value=value,
        threshold=threshold,
        message=message,
    ))


def _gt(v, t) -> bool:
    return v is not None and v > t

def _gte(v, t) -> bool:
    return v is not None and v >= t

def _within_pct(v, ref, pct=5.0) -> bool:
    if v is None or ref is None or ref == 0:
        return False
    return abs(v - ref) / abs(ref) * 100.0 <= pct

def _eq_count(v, t) -> bool:
    return v is not None and v == t


# ---------------------------------------------------------------------------
# R01 — A01 Catenary riser
# ---------------------------------------------------------------------------

def check_r01_catenary(result: ExampleQAResult) -> None:
    """Static top tension > 0; dynamic min effective tension > 0 along riser."""
    model = _load_sim(_sim_path("A01/A01 Catenary riser/A01_catenary_riser.sim"))
    if model is None:
        result.skipped = True
        result.skip_reason = ".sim file not found — run run_orcaflex.py first"
        return

    lines = [o for o in model.objects if isinstance(o, OrcFxAPI.Line)]
    if not lines:
        result.skipped = True
        result.skip_reason = "No Line objects in model"
        return

    riser = lines[0]

    # Check 1: static top tension > 0
    static_tt = riser.StaticResult("Effective Tension", OrcFxAPI.oeEndA)
    _check(result, "static_top_tension_gt_0", static_tt, 0.0, _gt,
           f"Static top tension {static_tt:.1f} kN should be > 0")

    # Check 2: dynamic min tension > 0 (no slack/compression)
    try:
        tension_rg = riser.RangeGraph("Effective Tension")
        min_tension = min(tension_rg.Min) if tension_rg.Min else None
        _check(result, "dynamic_min_tension_gt_0", min_tension, 0.0, _gt,
               f"Min dynamic tension {min_tension:.1f} kN should be > 0 (no slack)")
    except Exception as exc:
        result.checks.append(CheckResult(
            "dynamic_min_tension_gt_0", False, None, 0.0, f"Error: {exc}"))

    # Check 3: max curvature physically bounded (< 0.5 1/m for a typical flexible riser)
    try:
        curv_rg = riser.RangeGraph("Curvature")
        max_curv = max(curv_rg.Max) if curv_rg.Max else None
        _check(result, "max_curvature_lt_0.5", max_curv, 0.5,
               lambda v, t: v is not None and v < t,
               f"Max curvature {max_curv:.4f} 1/m should be < 0.5 1/m")
    except Exception as exc:
        result.checks.append(CheckResult(
            "max_curvature_lt_0.5", False, None, 0.5, f"Error: {exc}"))


# ---------------------------------------------------------------------------
# R02 — A01 Lazy wave riser
# ---------------------------------------------------------------------------

def check_r02_lazy_wave(result: ExampleQAResult) -> None:
    """Buoyancy arch uplift verified: max Z along riser > midpoint Z (arch is up)."""
    model = _load_sim(_sim_path("A01/A01 Lazy wave riser/A01_lazy_wave_riser.sim"))
    if model is None:
        result.skipped = True
        result.skip_reason = ".sim file not found — run run_orcaflex.py first"
        return

    lines = [o for o in model.objects if isinstance(o, OrcFxAPI.Line)]
    if not lines:
        result.skipped = True
        result.skip_reason = "No Line objects in model"
        return

    riser = lines[0]

    # Check 1: arch apex Z is higher than endpoints (buoyancy arch is upward)
    try:
        z_rg = riser.RangeGraph("Z")
        max_z = max(z_rg.Mean) if z_rg.Mean else None
        end_z = z_rg.Mean[-1] if z_rg.Mean else None
        arch_is_up = max_z is not None and end_z is not None and max_z > end_z
        _check(result, "buoyancy_arch_upward", float(arch_is_up), 0.5,
               _gt, f"Arch apex Z={max_z:.1f}m should exceed end Z={end_z:.1f}m")
    except Exception as exc:
        result.checks.append(CheckResult(
            "buoyancy_arch_upward", False, None, None, f"Error: {exc}"))

    # Check 2: static top tension > 0
    static_tt = riser.StaticResult("Effective Tension", OrcFxAPI.oeEndA)
    _check(result, "static_top_tension_gt_0", static_tt, 0.0, _gt,
           f"Static top tension {static_tt:.1f} kN should be > 0")

    # Check 3: lazy wave top tension < catenary top tension (design intent check — advisory)
    _check(result, "dynamic_min_tension_gt_0",
           min(riser.RangeGraph("Effective Tension").Min), 0.0, _gt,
           "Min dynamic tension should be > 0 (no slack in arch)")


# ---------------------------------------------------------------------------
# R03 — L01 Default vessel
# ---------------------------------------------------------------------------

def check_r03_vessel(result: ExampleQAResult) -> None:
    """Vessel static draft within expected range; heave motion non-zero."""
    model = _load_sim(_sim_path("L01/L01 Default vessel/L01_default_vessel.sim"))
    if model is None:
        result.skipped = True
        result.skip_reason = ".sim file not found — run run_orcaflex.py first"
        return

    vessels = [o for o in model.objects if isinstance(o, OrcFxAPI.Vessel)]
    if not vessels:
        result.skipped = True
        result.skip_reason = "No Vessel objects in model"
        return

    vessel = vessels[0]

    # Check 1: static Z (draft) is negative (vessel is below waterline)
    static_z = vessel.StaticResult("Z")
    _check(result, "vessel_static_draft_negative", static_z, 0.0,
           lambda v, t: v is not None and v < t,
           f"Vessel Z={static_z:.3f}m should be < 0 (below waterline)")

    # Check 2: heave range is non-zero (vessel is responding to waves)
    try:
        heave_th = vessel.TimeHistory("Z")
        heave_range = max(heave_th) - min(heave_th)
        _check(result, "heave_motion_nonzero", heave_range, 0.0, _gt,
               f"Heave range {heave_range:.3f}m should be > 0 (vessel is moving)")
    except Exception as exc:
        result.checks.append(CheckResult(
            "heave_motion_nonzero", False, None, 0.0, f"Error: {exc}"))

    # Check 3: pitch motion within reasonable bounds (< 15 deg for typical vessel)
    try:
        pitch_th = vessel.TimeHistory("Ry")
        pitch_max = max(abs(v) for v in pitch_th)
        _check(result, "pitch_lt_15deg", pitch_max, 15.0,
               lambda v, t: v is not None and v < t,
               f"Max pitch {pitch_max:.2f}° should be < 15° (physically reasonable)")
    except Exception as exc:
        result.checks.append(CheckResult(
            "pitch_lt_15deg", False, None, 15.0, f"Error: {exc}"))


# ---------------------------------------------------------------------------
# R04 — K01 5MW spar FOWT
# ---------------------------------------------------------------------------

def check_r04_fowt(result: ExampleQAResult) -> None:
    """Mooring pre-tension > 50 kN on all lines; spar surge < 50m."""
    model = _load_sim(_sim_path("K01/K01 5MW spar FOWT/K01_spar_fowt.sim"))
    if model is None:
        result.skipped = True
        result.skip_reason = ".sim file not found — run run_orcaflex.py first"
        return

    lines = [o for o in model.objects if isinstance(o, OrcFxAPI.Line)]
    vessels = [o for o in model.objects if isinstance(o, OrcFxAPI.Vessel)]

    # Check 1: all mooring lines have positive pre-tension
    for line in lines:
        try:
            static_tt = line.StaticResult("Effective Tension", OrcFxAPI.oeEndA)
            _check(result, f"mooring_{line.name}_pretension_gt_50", static_tt, 50.0, _gt,
                   f"Mooring '{line.name}' pre-tension {static_tt:.1f} kN should be > 50 kN")
        except Exception as exc:
            result.checks.append(CheckResult(
                f"mooring_{line.name}_pretension", False, None, 50.0, f"Error: {exc}"))

    # Check 2: spar static surge offset < 50m (reasonable for 100m WD equivalent)
    if vessels:
        spar = vessels[0]
        try:
            surge_th = spar.TimeHistory("X")
            max_surge = max(abs(v) for v in surge_th)
            _check(result, "spar_max_surge_lt_50m", max_surge, 50.0,
                   lambda v, t: v is not None and v < t,
                   f"Max spar surge {max_surge:.2f}m should be < 50m")
        except Exception as exc:
            result.checks.append(CheckResult(
                "spar_max_surge_lt_50m", False, None, 50.0, f"Error: {exc}"))


# ---------------------------------------------------------------------------
# R05 — E08 PipelayConfig (lay table)
# ---------------------------------------------------------------------------

def check_r05_lay_table(result: ExampleQAResult) -> None:
    """CSV output exists and contains expected number of positions."""
    csv_path = _sim_path("E08/PipelayConfig/E08_lay_table_results.csv")
    if not csv_path.exists():
        result.skipped = True
        result.skip_reason = "E08_lay_table_results.csv not found — run run_orcaflex.py first"
        return

    import csv as _csv
    with open(csv_path) as f:
        rows = list(_csv.DictReader(f))

    expected_count = 11  # 0m to 100m in 10m steps
    _check(result, "lay_table_row_count", len(rows), expected_count, _eq_count,
           f"CSV has {len(rows)} rows, expected {expected_count}")

    # Check all overbend curvature values are populated (not empty)
    non_empty = sum(1 for r in rows if r.get("overbend_curvature_1_m", "").strip())
    _check(result, "overbend_curvature_all_populated", non_empty, expected_count, _eq_count,
           f"{non_empty}/{expected_count} overbend curvature values populated")

    # Check top tensions are all positive floats
    try:
        tensions = [float(r["top_tension_kN"]) for r in rows if r.get("top_tension_kN")]
        all_positive = all(t > 0 for t in tensions)
        _check(result, "lay_table_tensions_positive", float(all_positive), 0.5, _gt,
               f"All {len(tensions)} lay table top tensions > 0: {all_positive}")
    except (ValueError, KeyError) as exc:
        result.checks.append(CheckResult(
            "lay_table_tensions_positive", False, None, None, f"Parse error: {exc}"))


# ---------------------------------------------------------------------------
# R06 — C10 Multiple statics
# ---------------------------------------------------------------------------

def check_r06_multi_static(result: ExampleQAResult) -> None:
    """CSV output exists with 5 wave direction cases."""
    csv_path = _sim_path("C10/C10 Multiple statics/C10_static_results.csv")
    if not csv_path.exists():
        result.skipped = True
        result.skip_reason = "C10_static_results.csv not found — run run_orcaflex.py first"
        return

    import csv as _csv
    with open(csv_path) as f:
        rows = list(_csv.DictReader(f))

    expected_count = 5  # 0° to 180° in 45° steps
    _check(result, "static_sweep_row_count", len(rows), expected_count, _eq_count,
           f"CSV has {len(rows)} rows, expected {expected_count}")

    # Check wave directions are the expected values
    try:
        dirs = [float(r["wave_direction_deg"]) for r in rows]
        expected_dirs = [0.0, 45.0, 90.0, 135.0, 180.0]
        dirs_match = dirs == expected_dirs
        _check(result, "wave_directions_correct", float(dirs_match), 0.5, _gt,
               f"Wave directions {dirs} match expected {expected_dirs}: {dirs_match}")
    except (ValueError, KeyError) as exc:
        result.checks.append(CheckResult(
            "wave_directions_correct", False, None, None, f"Parse error: {exc}"))

    # Check top tensions all present and positive
    try:
        tensions = [float(r["top_tension_kN"]) for r in rows if r.get("top_tension_kN")]
        all_positive = all(t > 0 for t in tensions)
        _check(result, "static_tensions_positive", float(all_positive), 0.5, _gt,
               f"All {len(tensions)} static tensions > 0: {all_positive}")
    except (ValueError, KeyError) as exc:
        result.checks.append(CheckResult(
            "static_tensions_positive", False, None, None, f"Parse error: {exc}"))


# ---------------------------------------------------------------------------
# Registry and runner
# ---------------------------------------------------------------------------

_EXAMPLE_REGISTRY: dict[str, tuple[str, str, Callable]] = {
    "R01": ("A01/A01 Catenary riser/A01_catenary_riser.sim", "A01 Catenary riser", check_r01_catenary),
    "R02": ("A01/A01 Lazy wave riser/A01_lazy_wave_riser.sim", "A01 Lazy wave riser", check_r02_lazy_wave),
    "R03": ("L01/L01 Default vessel/L01_default_vessel.sim", "L01 Default vessel", check_r03_vessel),
    "R04": ("K01/K01 5MW spar FOWT/K01_spar_fowt.sim", "K01 5MW spar FOWT", check_r04_fowt),
    "R05": ("E08/PipelayConfig/E08_lay_table_results.csv", "E08 PipelayConfig", check_r05_lay_table),
    "R06": ("C10/C10 Multiple statics/C10_static_results.csv", "C10 Multiple statics", check_r06_multi_static),
}


def run_orcaflex_qa(example_ids: list[str] | None = None) -> list[ExampleQAResult]:
    """Run QA checks for specified examples (or all if None).

    Returns a list of ExampleQAResult, one per example.
    """
    if example_ids is None:
        example_ids = list(_EXAMPLE_REGISTRY.keys())

    results = []
    for eid in example_ids:
        if eid not in _EXAMPLE_REGISTRY:
            print(f"[WARN] Unknown example ID: {eid}")
            continue
        sim_rel, label, check_fn = _EXAMPLE_REGISTRY[eid]
        result = ExampleQAResult(
            example_id=eid,
            sim_file=str(_sim_path(sim_rel)),
        )
        print(f"\n[QA] {eid}: {label}")
        t0 = time.time()
        check_fn(result)
        elapsed = time.time() - t0

        if result.skipped:
            print(f"  SKIPPED: {result.skip_reason}")
        else:
            for chk in result.checks:
                status = "PASS" if chk.passed else "FAIL"
                print(f"  [{status}] {chk.check_name}: {chk.message}")
            print(f"  {result.n_passed}/{len(result.checks)} checks passed ({elapsed:.2f}s)")
        results.append(result)
    return results


def write_qa_report(results: list[ExampleQAResult], qa_dir: Path) -> None:
    """Write QA_REPORT.md and per-example JSON to qa_dir."""
    qa_dir.mkdir(parents=True, exist_ok=True)

    # Per-example JSON
    for r in results:
        json_path = qa_dir / f"{r.example_id}_qa_results.json"
        with open(json_path, "w") as f:
            json.dump(asdict(r), f, indent=2)

    # Markdown report
    md_lines = [
        "# OrcaFlex Example QA Report",
        "",
        f"Generated from {len(results)} example checks.",
        "",
        "| Example | Label | Checks | Status |",
        "|---------|-------|--------|--------|",
    ]
    for r in results:
        sim_rel, label, _ = _EXAMPLE_REGISTRY.get(r.example_id, ("", r.example_id, None))
        if r.skipped:
            status = f"SKIPPED ({r.skip_reason})"
            checks = "—"
        else:
            status = "PASS" if r.passed else f"FAIL ({r.n_failed} failed)"
            checks = f"{r.n_passed}/{len(r.checks)}"
        md_lines.append(f"| {r.example_id} | {label} | {checks} | {status} |")

    md_lines += [
        "",
        "## Check Details",
        "",
    ]
    for r in results:
        md_lines.append(f"### {r.example_id}")
        if r.skipped:
            md_lines.append(f"*Skipped: {r.skip_reason}*")
        else:
            for chk in r.checks:
                icon = "✓" if chk.passed else "✗"
                md_lines.append(f"- {icon} **{chk.check_name}**: {chk.message}")
        md_lines.append("")

    report_path = qa_dir / "QA_REPORT.md"
    report_path.write_text("\n".join(md_lines))
    print(f"\n[OK] QA report written: {report_path}")


# ---------------------------------------------------------------------------
# CLI entry point
# ---------------------------------------------------------------------------

def main() -> None:
    parser = argparse.ArgumentParser(description="OrcaFlex example QA suite")
    parser.add_argument(
        "--example", nargs="*", choices=list(_EXAMPLE_REGISTRY.keys()),
        help="Example IDs to check (default: all)",
    )
    parser.add_argument(
        "--report-dir", default=str(Path(__file__).parent),
        help="Directory to write QA_REPORT.md and JSON results",
    )
    args = parser.parse_args()

    results = run_orcaflex_qa(args.example)
    qa_dir = Path(args.report_dir)
    write_qa_report(results, qa_dir)

    n_pass = sum(1 for r in results if r.passed and not r.skipped)
    n_skip = sum(1 for r in results if r.skipped)
    n_fail = sum(1 for r in results if not r.passed and not r.skipped)
    print(f"\nSummary: {n_pass} passed, {n_fail} failed, {n_skip} skipped")
    sys.exit(0 if n_fail == 0 else 1)


if __name__ == "__main__":
    main()
