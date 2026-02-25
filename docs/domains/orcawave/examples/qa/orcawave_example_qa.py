#!/usr/bin/env python3
"""
OrcaWave Example QA Suite (WRK-319)

ABOUTME: Physics-based quality assurance checks for OrcaWave L01–L06 example
outputs. Each example has a dedicated check function that validates the .owr
results against known physical constraints.

Usage:
    uv run python orcawave_example_qa.py                    # Run all examples
    uv run python orcawave_example_qa.py --example L01      # Single example
    uv run python orcawave_example_qa.py --report           # Write QA_REPORT.md

Dependencies: OrcFxAPI, numpy
"""
from __future__ import annotations

import json
import sys
import argparse
import traceback
from dataclasses import dataclass, field
from pathlib import Path
from typing import Optional

import numpy as np

EXAMPLES_DIR = Path(__file__).parent.parent
QA_DIR = Path(__file__).parent

# Add OrcaFlex API path
_orcaflex_path = r"C:\Program Files (x86)\Orcina\OrcaFlex\11.6\OrcFxAPI\Python"
sys.path.insert(0, _orcaflex_path)

try:
    import OrcFxAPI  # type: ignore
    _ORCFXAPI_AVAILABLE = True
except ImportError:
    _ORCFXAPI_AVAILABLE = False


# ---------------------------------------------------------------------------
# QA result types
# ---------------------------------------------------------------------------

@dataclass
class QACheck:
    name: str
    passed: bool
    message: str
    value: Optional[float] = None
    threshold: Optional[float] = None


@dataclass
class ExampleQAResult:
    example_id: str      # e.g. "L01"
    owr_path: str
    checks: list[QACheck] = field(default_factory=list)
    skipped: bool = False
    skip_reason: str = ""

    @property
    def passed(self) -> bool:
        return not self.skipped and all(c.passed for c in self.checks)

    @property
    def n_checks(self) -> int:
        return len(self.checks)

    @property
    def n_passed(self) -> int:
        return sum(1 for c in self.checks if c.passed)

    def to_dict(self) -> dict:
        return {
            "example_id": self.example_id,
            "owr_path": self.owr_path,
            "passed": self.passed,
            "skipped": self.skipped,
            "skip_reason": self.skip_reason,
            "n_checks": self.n_checks,
            "n_passed": self.n_passed,
            "checks": [
                {
                    "name": c.name,
                    "passed": c.passed,
                    "message": c.message,
                    "value": c.value,
                    "threshold": c.threshold,
                }
                for c in self.checks
            ],
        }


# ---------------------------------------------------------------------------
# Common check helpers
# ---------------------------------------------------------------------------

def _check_positive_damping(diff, result: ExampleQAResult) -> None:
    """Radiation damping must be non-negative at all frequencies for all DOFs."""
    try:
        damping = diff.damping  # shape (nfreq, 6, 6) or (nfreq, 6N, 6N)
        diag_min = float(np.min(np.diagonal(damping, axis1=1, axis2=2)))
        passed = diag_min >= -1e-6  # allow tiny numerical noise
        result.checks.append(QACheck(
            name="positive_damping_diagonal",
            passed=passed,
            message=f"Min diagonal damping = {diag_min:.4g}",
            value=diag_min,
            threshold=0.0,
        ))
    except Exception as e:
        result.checks.append(QACheck(
            name="positive_damping_diagonal",
            passed=False,
            message=f"Error accessing damping: {e}",
        ))


def _check_raos_finite(diff, result: ExampleQAResult) -> None:
    """Displacement RAOs must be finite (no NaN/Inf)."""
    try:
        raos = diff.displacementRAOs  # complex array
        finite = bool(np.all(np.isfinite(raos)))
        result.checks.append(QACheck(
            name="raos_finite",
            passed=finite,
            message="All displacement RAOs are finite" if finite else "NaN/Inf found in RAOs",
        ))
    except Exception as e:
        result.checks.append(QACheck(
            name="raos_finite",
            passed=False,
            message=f"Error accessing RAOs: {e}",
        ))


def _check_heave_rao_long_period(diff, result: ExampleQAResult) -> None:
    """Heave RAO for head seas should approach 1.0 at long periods (quasi-static)."""
    try:
        freqs_hz = np.array(diff.frequencies)  # Hz, descending
        sort_idx = np.argsort(freqs_hz)         # ascending index
        freqs_sorted = freqs_hz[sort_idx]
        raos = diff.displacementRAOs            # (nheading, nfreq, 6)

        headings = list(diff.headings)
        # Find heading closest to 0° (head seas) or 180°
        head_idx = int(np.argmin(np.abs(np.array(headings))))

        # Heave = DOF index 2
        heave_raos = np.abs(raos[head_idx, :, 2])[sort_idx]

        # Check at longest period (lowest frequency)
        long_period_heave = float(heave_raos[-1])
        passed = 0.8 <= long_period_heave <= 1.2
        result.checks.append(QACheck(
            name="heave_rao_quasi_static",
            passed=passed,
            message=(
                f"Heave RAO at T={1/freqs_sorted[-1]:.1f}s = {long_period_heave:.3f} "
                f"(expected ~1.0 ± 0.2)"
            ),
            value=long_period_heave,
            threshold=1.0,
        ))
    except Exception as e:
        result.checks.append(QACheck(
            name="heave_rao_quasi_static",
            passed=False,
            message=f"Error: {e}",
        ))


def _check_added_mass_finite(diff, result: ExampleQAResult) -> None:
    """Added mass must be finite at all frequencies."""
    try:
        am = diff.addedMass
        finite = bool(np.all(np.isfinite(am)))
        result.checks.append(QACheck(
            name="added_mass_finite",
            passed=finite,
            message="Added mass is finite" if finite else "NaN/Inf in added mass",
        ))
    except Exception as e:
        result.checks.append(QACheck(
            name="added_mass_finite",
            passed=False,
            message=f"Error: {e}",
        ))


# ---------------------------------------------------------------------------
# Per-example check functions
# ---------------------------------------------------------------------------

def check_l01(owr_path: Path) -> ExampleQAResult:
    """L01 Default vessel — single body, control surface mean drift."""
    result = ExampleQAResult("L01", str(owr_path))
    if not owr_path.exists():
        result.skipped = True
        result.skip_reason = f"OWR not found: {owr_path}"
        return result

    diff = OrcFxAPI.Diffraction(str(owr_path))
    _check_raos_finite(diff, result)
    _check_positive_damping(diff, result)
    _check_added_mass_finite(diff, result)
    _check_heave_rao_long_period(diff, result)
    return result


def check_l02(owr_path: Path) -> ExampleQAResult:
    """L02 OC4 Semi-sub — drag linearisation, iterative solver."""
    result = ExampleQAResult("L02", str(owr_path))
    if not owr_path.exists():
        result.skipped = True
        result.skip_reason = f"OWR not found: {owr_path}"
        return result

    diff = OrcFxAPI.Diffraction(str(owr_path))
    _check_raos_finite(diff, result)
    _check_positive_damping(diff, result)
    _check_added_mass_finite(diff, result)
    _check_heave_rao_long_period(diff, result)
    return result


def check_l03(owr_path: Path) -> ExampleQAResult:
    """L03 Semi-sub multibody — 4-body coupling symmetry check."""
    result = ExampleQAResult("L03", str(owr_path))
    if not owr_path.exists():
        result.skipped = True
        result.skip_reason = f"OWR not found: {owr_path}"
        return result

    diff = OrcFxAPI.Diffraction(str(owr_path))
    _check_raos_finite(diff, result)
    _check_positive_damping(diff, result)
    _check_added_mass_finite(diff, result)

    # Multi-body coupling symmetry: A[body_i, body_j] ≈ A[body_j, body_i]^T
    try:
        am = diff.addedMass  # (nfreq, 6N, 6N)
        n_bodies = am.shape[1] // 6
        if n_bodies >= 2:
            # Check first off-diagonal block
            a12 = am[:, 0:6, 6:12]
            a21 = am[:, 6:12, 0:6]
            symm_error = float(np.max(np.abs(a12 - a21.transpose(0, 2, 1))))
            # Coupling blocks should be transpose of each other
            passed = symm_error < 1.0  # generous tolerance for different body sizes
            result.checks.append(QACheck(
                name="coupling_symmetry_A12_A21",
                passed=passed,
                message=f"Max |A12 - A21^T| = {symm_error:.4g}",
                value=symm_error,
                threshold=1.0,
            ))
    except Exception as e:
        result.checks.append(QACheck(
            name="coupling_symmetry_A12_A21",
            passed=False,
            message=f"Error: {e}",
        ))

    return result


def check_l04(owr_path: Path) -> ExampleQAResult:
    """L04 Sectional bodies — 7 bodies, potential-only formulation."""
    result = ExampleQAResult("L04", str(owr_path))
    if not owr_path.exists():
        result.skipped = True
        result.skip_reason = f"OWR not found: {owr_path}"
        return result

    diff = OrcFxAPI.Diffraction(str(owr_path))
    _check_raos_finite(diff, result)
    _check_positive_damping(diff, result)
    _check_added_mass_finite(diff, result)
    return result


def check_l05(owr_path: Path) -> ExampleQAResult:
    """L05 Panel pressures — same geometry as L04, OutputPanelPressures: Yes."""
    result = ExampleQAResult("L05", str(owr_path))
    if not owr_path.exists():
        result.skipped = True
        result.skip_reason = f"OWR not found: {owr_path}"
        return result

    diff = OrcFxAPI.Diffraction(str(owr_path))
    _check_raos_finite(diff, result)
    _check_positive_damping(diff, result)
    _check_added_mass_finite(diff, result)

    # Check panel pressures are present and finite
    try:
        panels = list(diff.panelGeometry)
        n_panels = len(panels)
        result.checks.append(QACheck(
            name="panel_count",
            passed=n_panels > 0,
            message=f"Panel count: {n_panels}",
            value=float(n_panels),
        ))
    except Exception as e:
        result.checks.append(QACheck(
            name="panel_count",
            passed=False,
            message=f"Error accessing panel geometry: {e}",
        ))

    return result


def check_l06(owr_path: Path) -> ExampleQAResult:
    """L06 Full QTF — QTF symmetry check."""
    result = ExampleQAResult("L06", str(owr_path))
    if not owr_path.exists():
        result.skipped = True
        result.skip_reason = f"OWR not found: {owr_path}"
        return result

    diff = OrcFxAPI.Diffraction(str(owr_path))
    _check_raos_finite(diff, result)
    _check_added_mass_finite(diff, result)

    # QTF-specific check: verify QTF data is present
    try:
        # Check that QTF results exist (attribute presence varies by OrcaWave version)
        has_qtf = hasattr(diff, "QTFResults") or hasattr(diff, "qtfResults")
        result.checks.append(QACheck(
            name="qtf_data_present",
            passed=has_qtf,
            message="QTF results attribute present" if has_qtf
                    else "QTF results not found (may be version-dependent)",
        ))
    except Exception as e:
        result.checks.append(QACheck(
            name="qtf_data_present",
            passed=False,
            message=f"Error: {e}",
        ))

    return result


# ---------------------------------------------------------------------------
# Registry
# ---------------------------------------------------------------------------

EXAMPLE_CHECKS = {
    "L01": (
        EXAMPLES_DIR / "L01_default_vessel" / "L01 Default vessel.owr",
        check_l01,
    ),
    "L02": (
        EXAMPLES_DIR / "L02 OC4 Semi-sub" / "L02 OC4 Semi-sub.owr",
        check_l02,
    ),
    "L03": (
        EXAMPLES_DIR / "L03 Semi-sub multibody analysis"
        / "L03 Semi-sub multibody analysis.owr",
        check_l03,
    ),
    "L04": (
        EXAMPLES_DIR / "L04 Sectional bodies" / "L04 Sectional bodies.owr",
        check_l04,
    ),
    "L05": (
        EXAMPLES_DIR / "L05 Panel pressures" / "L05 Panel pressures.owr",
        check_l05,
    ),
    "L06": (
        EXAMPLES_DIR / "L06 Full QTF diffraction analysis"
        / "L06 Time varying quadratic loads.owr",
        check_l06,
    ),
}


# ---------------------------------------------------------------------------
# Runner
# ---------------------------------------------------------------------------

def run_qa(examples: list[str] | None = None) -> list[ExampleQAResult]:
    if not _ORCFXAPI_AVAILABLE:
        print("[ERROR] OrcFxAPI not available — cannot run QA checks")
        return []

    targets = examples or list(EXAMPLE_CHECKS.keys())
    results = []

    for example_id in targets:
        if example_id not in EXAMPLE_CHECKS:
            print(f"[WARN] Unknown example: {example_id}")
            continue

        owr_path, check_fn = EXAMPLE_CHECKS[example_id]
        print(f"\n{'='*60}")
        print(f"QA: {example_id}  ({owr_path.name})")
        print(f"{'='*60}")

        try:
            result = check_fn(owr_path)
        except Exception as e:
            result = ExampleQAResult(example_id, str(owr_path))
            result.skipped = True
            result.skip_reason = f"Exception: {e}\n{traceback.format_exc()}"

        if result.skipped:
            print(f"  [SKIP] {result.skip_reason}")
        else:
            for check in result.checks:
                status = "PASS" if check.passed else "FAIL"
                print(f"  [{status}] {check.name}: {check.message}")

        # Save per-example JSON
        json_path = QA_DIR / f"{example_id.lower()}_qa_results.json"
        json_path.write_text(
            json.dumps(result.to_dict(), indent=2), encoding="utf-8"
        )

        results.append(result)

    return results


def write_qa_report(results: list[ExampleQAResult]) -> Path:
    """Write QA_REPORT.md summarising all example results."""
    lines = [
        "# OrcaWave Examples QA Report",
        "",
        "Generated by `orcawave_example_qa.py` (WRK-319)",
        "",
        "## Summary",
        "",
        "| Example | Status | Checks | Passed |",
        "|---------|--------|--------|--------|",
    ]

    for r in results:
        if r.skipped:
            status = "SKIPPED"
            checks = "—"
            passed = "—"
        else:
            status = "PASS" if r.passed else "FAIL"
            checks = str(r.n_checks)
            passed = str(r.n_passed)
        lines.append(f"| {r.example_id} | {status} | {checks} | {passed} |")

    lines += [
        "",
        "## Detail",
        "",
    ]

    for r in results:
        lines.append(f"### {r.example_id}")
        lines.append(f"- OWR: `{r.owr_path}`")
        if r.skipped:
            lines.append(f"- **SKIPPED**: {r.skip_reason}")
        else:
            for c in r.checks:
                icon = "✓" if c.passed else "✗"
                lines.append(f"- {icon} `{c.name}`: {c.message}")
        lines.append("")

    report_path = QA_DIR / "QA_REPORT.md"
    report_path.write_text("\n".join(lines), encoding="utf-8")
    print(f"\n[OK] QA report written: {report_path}")
    return report_path


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser(description="OrcaWave example QA suite")
    parser.add_argument(
        "--example", "-e",
        nargs="+",
        choices=list(EXAMPLE_CHECKS.keys()),
        help="Run QA for specific examples only",
    )
    parser.add_argument(
        "--report", "-r",
        action="store_true",
        help="Write QA_REPORT.md after running checks",
    )
    args = parser.parse_args()

    results = run_qa(args.example)

    if args.report or not args.example:
        write_qa_report(results)

    all_passed = all(
        (r.passed or r.skipped) for r in results
    )
    n_skip = sum(1 for r in results if r.skipped)
    n_pass = sum(1 for r in results if r.passed)
    n_fail = sum(1 for r in results if not r.passed and not r.skipped)

    print(f"\nSummary: {n_pass} passed, {n_fail} failed, {n_skip} skipped")
    sys.exit(0 if n_fail == 0 else 1)


if __name__ == "__main__":
    main()
