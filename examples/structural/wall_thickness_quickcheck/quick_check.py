#!/usr/bin/env python3
# ABOUTME: Deckhand "quick calc" multi-code pipeline wall-thickness check (DNV + API).
# ABOUTME: Single representative case, deterministic, offline --from-cache mode, HTML attachment.
"""Pipeline Wall Thickness — Multi-Code Quick Check (DNV-ST-F101 + API RP 1111).

A minimal, deterministic, single-case wrapper around the digitalmodel
wall-thickness engine. It is the EXECUTE artifact for a Deckhand "quick calc"
path: a downstream allowlist invokes exactly one stable command and gets back
a short text verdict on stdout plus a self-contained HTML report attachment.

Representative case (hard-coded, traceable):
  - 12-inch export line  (OD 12.75 in = 0.3239 m)
  - 1.125 in wall        (0.028575 m), 3 mm corrosion allowance
  - X65 line-pipe        (SMYS 448 MPa, SMTS 531 MPa)
  - 150 bar internal     (15.0 MPa local incidental pressure)
  - 1500 m water depth   (hydrostatic external pressure ~15.08 MPa)
  - Codes compared: DNV-ST-F101 (2021) and API RP 1111 (2015)

Modes
-----
  --compute       (default) Run the engine live, then write a fresh cache fixture.
  --from-cache    Offline. Load the committed JSON fixture; never touch the engine.
                  Deterministic and network-free — used by CI and the allowlist.

Stable command (document this in README and any downstream allowlist):

    PYTHONPATH=src uv run python \\
        examples/structural/wall_thickness_quickcheck/quick_check.py --from-cache

Both modes print the same summary and write the same HTML report to:
    examples/structural/wall_thickness_quickcheck/output/wall_thickness_quickcheck.html
The absolute path of that report is printed as the final stdout line, prefixed
with "ARTIFACT: " so a caller can scrape it.
"""
from __future__ import annotations

import argparse
import json
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, List

HERE = Path(__file__).resolve().parent
CACHE_PATH = HERE / "data" / "quickcheck_cache.json"
DEFAULT_OUTPUT = HERE / "output" / "wall_thickness_quickcheck.html"

# ---------------------------------------------------------------------------
# Representative case — single source of truth for inputs (SI units).
# Echoed into the report so every assumption is traceable.
# ---------------------------------------------------------------------------
SEAWATER_DENSITY = 1025.0   # kg/m^3
GRAVITY = 9.81              # m/s^2
WATER_DEPTH_M = 1500.0     # m

CASE = {
    "label": '12" Export Line, 1500 m water depth',
    "geometry": {
        "outer_diameter": 0.3239,      # 12.75 in
        "wall_thickness": 0.028575,    # 1.125 in
        "corrosion_allowance": 0.003,  # 3 mm
    },
    "material": {
        "grade": "X65",
        "smys": 448e6,
        "smts": 531e6,
    },
    "loads": {
        "internal_pressure": 150e5,    # 150 bar = 15.0 MPa
        # external_pressure derived from water depth below
    },
    "water_depth_m": WATER_DEPTH_M,
    "codes": ["DNV-ST-F101", "API-RP-1111"],
}


def external_pressure_from_depth(depth_m: float) -> float:
    """Hydrostatic external pressure (Pa) at a given water depth."""
    return SEAWATER_DENSITY * GRAVITY * depth_m


# ---------------------------------------------------------------------------
# Cache-mode result shape — a tiny, code-free mirror of CodeComparisonResult.
# ---------------------------------------------------------------------------
@dataclass
class CachedCodeResult:
    code_label: str
    max_utilisation: float
    governing_check: str
    is_safe: bool
    checks: Dict[str, float]


# ---------------------------------------------------------------------------
# COMPUTE mode — call the real digitalmodel engine.
# ---------------------------------------------------------------------------
def run_compute() -> List[CachedCodeResult]:
    from digitalmodel.structural.analysis.wall_thickness import (
        DesignCode,
        DesignLoads,
        PipeGeometry,
        PipeMaterial,
    )
    from digitalmodel.structural.analysis.wall_thickness_comparison import compare_codes

    geometry = PipeGeometry(**CASE["geometry"])
    material = PipeMaterial(**CASE["material"])
    loads = DesignLoads(
        internal_pressure=CASE["loads"]["internal_pressure"],
        external_pressure=external_pressure_from_depth(CASE["water_depth_m"]),
    )
    code_map = {c.value: c for c in DesignCode}
    codes = [code_map[name] for name in CASE["codes"]]

    results = compare_codes(geometry, material, loads, codes=codes)
    return [
        CachedCodeResult(
            code_label=r.code_label,
            max_utilisation=round(r.max_utilisation, 6),
            governing_check=r.governing_check or "N/A",
            is_safe=bool(r.is_safe),
            checks={k: round(v, 6) for k, v in r.checks.items()},
        )
        for r in results
    ]


def write_cache(results: List[CachedCodeResult]) -> None:
    payload = {
        "case": CASE,
        "external_pressure": round(external_pressure_from_depth(CASE["water_depth_m"]), 3),
        "results": [
            {
                "code_label": r.code_label,
                "max_utilisation": r.max_utilisation,
                "governing_check": r.governing_check,
                "is_safe": r.is_safe,
                "checks": r.checks,
            }
            for r in results
        ],
    }
    CACHE_PATH.parent.mkdir(parents=True, exist_ok=True)
    CACHE_PATH.write_text(json.dumps(payload, indent=2, sort_keys=True) + "\n", encoding="utf-8")


# ---------------------------------------------------------------------------
# FROM-CACHE mode — read the committed fixture, no engine, no network.
# ---------------------------------------------------------------------------
def run_from_cache() -> List[CachedCodeResult]:
    if not CACHE_PATH.exists():
        raise SystemExit(
            f"[ERROR] cache fixture not found: {CACHE_PATH}\n"
            "        Run once with --compute to generate it."
        )
    payload = json.loads(CACHE_PATH.read_text(encoding="utf-8"))
    return [
        CachedCodeResult(
            code_label=r["code_label"],
            max_utilisation=r["max_utilisation"],
            governing_check=r["governing_check"],
            is_safe=r["is_safe"],
            checks=r["checks"],
        )
        for r in payload["results"]
    ]


# ---------------------------------------------------------------------------
# Reporting — short stdout summary + self-contained HTML attachment.
# ---------------------------------------------------------------------------
def print_summary(results: List[CachedCodeResult]) -> None:
    pext = external_pressure_from_depth(CASE["water_depth_m"])
    g = CASE["geometry"]
    m = CASE["material"]
    print("=" * 64)
    print("Pipeline Wall Thickness — Multi-Code Quick Check")
    print("=" * 64)
    print(f"Case            : {CASE['label']}")
    print(
        f"Pipe            : OD {g['outer_diameter']*1000:.1f} mm  "
        f"WT {g['wall_thickness']*1000:.2f} mm  "
        f"CA {g['corrosion_allowance']*1000:.1f} mm"
    )
    print(f"Material        : {m['grade']}  SMYS {m['smys']/1e6:.0f} MPa  SMTS {m['smts']/1e6:.0f} MPa")
    print(
        f"Loads           : internal {CASE['loads']['internal_pressure']/1e6:.1f} MPa  "
        f"external {pext/1e6:.2f} MPa  (depth {CASE['water_depth_m']:.0f} m)"
    )
    print("-" * 64)
    print(f"{'Code':<16}{'Max util':>10}{'Governing':>22}{'Verdict':>10}")
    print("-" * 64)
    for r in results:
        verdict = "PASS" if r.is_safe else "FAIL"
        print(f"{r.code_label:<16}{r.max_utilisation:>10.3f}{r.governing_check:>22}{verdict:>10}")
    print("-" * 64)
    overall = "PASS" if all(r.is_safe for r in results) else "FAIL"
    governing_code = max(results, key=lambda r: r.max_utilisation)
    print(
        f"Overall verdict : {overall}  "
        f"(driven by {governing_code.code_label} at util {governing_code.max_utilisation:.3f})"
    )
    print("=" * 64)


def write_report(results: List[CachedCodeResult], output_path: Path) -> Path:
    """Write the self-contained HTML report via the engine's report builder.

    Reuses the real digitalmodel report HTML/SVG generator. The builder is a
    pure string formatter (no network, no engine recompute). Inputs are passed
    as the real dataclasses (pure data containers — no engine work), and the
    per-code result rows come from a lightweight shim carrying the cached
    numbers. This keeps --from-cache mode fully offline and deterministic.
    """
    from digitalmodel.structural.analysis.wall_thickness import (
        DesignLoads,
        PipeGeometry,
        PipeMaterial,
    )
    from digitalmodel.structural.analysis.wall_thickness_comparison import (
        _build_comparison_html,
    )

    geometry = PipeGeometry(**CASE["geometry"])
    material = PipeMaterial(**CASE["material"])
    loads = DesignLoads(
        internal_pressure=CASE["loads"]["internal_pressure"],
        external_pressure=external_pressure_from_depth(CASE["water_depth_m"]),
    )

    # Shim exposing only the attributes the HTML builder reads from a result.
    class _ResultShim:
        def __init__(self, r: CachedCodeResult):
            self.code = r.code_label
            self.code_label = r.code_label
            self.max_utilisation = r.max_utilisation
            self.governing_check = r.governing_check
            self.is_safe = r.is_safe
            self.checks = r.checks
            self.details = {}

    html = _build_comparison_html(
        geometry,
        material,
        loads,
        [_ResultShim(r) for r in results],
        title=f"Wall Thickness Quick Check — {CASE['label']}",
    )
    output_path.parent.mkdir(parents=True, exist_ok=True)
    output_path.write_text(html, encoding="utf-8")
    return output_path


def main(argv: List[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    mode = parser.add_mutually_exclusive_group()
    mode.add_argument(
        "--from-cache",
        action="store_true",
        help="Offline: load the committed JSON fixture, never run the engine.",
    )
    mode.add_argument(
        "--compute",
        action="store_true",
        help="Run the engine live and refresh the cache fixture (default).",
    )
    parser.add_argument(
        "--output",
        type=Path,
        default=DEFAULT_OUTPUT,
        help=f"HTML report output path (default: {DEFAULT_OUTPUT}).",
    )
    args = parser.parse_args(argv)

    if args.from_cache:
        results = run_from_cache()
    else:
        results = run_compute()
        write_cache(results)

    print_summary(results)
    artifact = write_report(results, args.output)
    print(f"ARTIFACT: {artifact.resolve()}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
