"""Engine route + calc note for multi-code wall thickness comparison.

# ABOUTME: Wire existing compare_codes() to basename `wall_thickness_compare`.
# ABOUTME: Emit a packaged calc note (HTML via generate_comparison_report + MD/JSON).

Runs the same pipe specification through several registered design codes
(DNV-ST-F101, API RP 1111, PD 8010-2, ...), ranks each code's checks, and packages
the cross-code envelope into a calc note. Deterministic, SI units, no synthetic
inputs.
"""
from __future__ import annotations

import json
import logging
from pathlib import Path
from typing import List

from digitalmodel.structural.analysis.wall_thickness import (
    DesignCode,
    DesignFactors,
    DesignLoads,
    PipeGeometry,
    PipeMaterial,
    SafetyClass,
)
from digitalmodel.structural.analysis.wall_thickness_comparison import (
    CodeComparisonResult,
    compare_codes,
    generate_comparison_report,
)

logger = logging.getLogger(__name__)

_SAFETY_CLASS = {
    "low": SafetyClass.LOW,
    "medium": SafetyClass.MEDIUM,
    "high": SafetyClass.HIGH,
}


def run_wall_thickness_compare(cfg: dict) -> dict:
    """Engine router for ``basename: wall_thickness_compare``."""
    wc = cfg["wall_thickness_compare"]
    g = wc["geometry"]
    m = wc["material"]
    ld = wc.get("loads", {})

    geometry = PipeGeometry(
        outer_diameter=g["outer_diameter"],
        wall_thickness=g["wall_thickness"],
        corrosion_allowance=g.get("corrosion_allowance", 0.0),
    )
    material = PipeMaterial(
        grade=m.get("grade", "unspecified"),
        smys=m["smys"],
        smts=m["smts"],
        youngs_modulus=m.get("youngs_modulus", 207e9),
        poissons_ratio=m.get("poissons_ratio", 0.3),
    )
    loads = DesignLoads(
        internal_pressure=ld.get("internal_pressure", 0.0),
        external_pressure=ld.get("external_pressure", 0.0),
        bending_moment=ld.get("bending_moment", 0.0),
        effective_tension=ld.get("effective_tension", 0.0),
    )
    factors = DesignFactors(
        safety_class=_SAFETY_CLASS[wc.get("safety_class", "medium").lower()]
    )
    codes = [DesignCode(c) for c in wc["codes"]]

    results = compare_codes(geometry, material, loads, codes, factors)

    worst = max(results, key=lambda r: r.max_utilisation) if results else None
    overall = "PASS" if all(r.is_safe for r in results) else "FAIL"

    wc["result"] = {
        "overall_status": overall,
        "governing_code": worst.code_label if worst else None,
        "envelope_max_utilisation": worst.max_utilisation if worst else 0.0,
        "codes": [
            {
                "code": r.code_label,
                "checks": r.checks,
                "governing_check": r.governing_check,
                "max_utilisation": r.max_utilisation,
                "is_safe": r.is_safe,
            }
            for r in results
        ],
    }

    out_dir = Path(wc.get("output_dir", "results/wt_multicode"))
    out_dir.mkdir(parents=True, exist_ok=True)
    label = wc.get("label", "pipeline_multicode")
    title = wc.get("title", "Pipeline Multi-Code Wall Thickness Comparison")

    generate_comparison_report(
        geometry, material, loads, codes, title=title,
        factors=factors, output_path=str(out_dir / f"{label}_calcnote.html"),
    )
    (out_dir / f"{label}_calcnote.md").write_text(
        build_compare_note(geometry, material, loads, results, title), encoding="utf-8"
    )
    (out_dir / f"{label}_results.json").write_text(
        json.dumps(wc["result"], indent=2), encoding="utf-8"
    )
    logger.info("Multi-code comparison calc note written to %s", out_dir)

    return cfg


def build_compare_note(
    geometry: PipeGeometry,
    material: PipeMaterial,
    loads: DesignLoads,
    results: List[CodeComparisonResult],
    title: str,
) -> str:
    """Markdown calc note: code x check utilisation matrix + envelope."""
    all_checks: List[str] = []
    seen = set()
    for r in results:
        for c in r.checks:
            if c not in seen:
                all_checks.append(c)
                seen.add(c)

    worst = max(results, key=lambda r: r.max_utilisation) if results else None
    overall = "PASS" if all(r.is_safe for r in results) else "FAIL"

    lines: List[str] = []
    lines.append(f"# {title}")
    lines.append("")
    lines.append(f"**Overall status:** {overall}")
    if worst:
        lines.append(
            f"**Cross-code envelope:** {worst.code_label} governs at "
            f"utilisation {worst.max_utilisation:.3f} "
            f"(check `{worst.governing_check}`)"
        )
    lines.append("")

    lines.append("## Geometry & material")
    lines.append("")
    lines.append("| Property | Value |")
    lines.append("|---|---|")
    lines.append(f"| Outer diameter | {geometry.outer_diameter * 1000:.1f} mm |")
    lines.append(f"| Wall thickness | {geometry.wall_thickness * 1000:.2f} mm |")
    lines.append(f"| D/t ratio | {geometry.d_over_t:.1f} |")
    lines.append(f"| Grade | {material.grade} |")
    lines.append(f"| SMYS | {material.smys / 1e6:.0f} MPa |")
    lines.append(f"| SMTS | {material.smts / 1e6:.0f} MPa |")
    lines.append(f"| Internal pressure | {loads.internal_pressure / 1e6:.1f} MPa |")
    lines.append(f"| External pressure | {loads.external_pressure / 1e6:.1f} MPa |")
    lines.append("")

    lines.append("## Code x check utilisation matrix")
    lines.append("")
    header = "| Check | " + " | ".join(r.code_label for r in results) + " |"
    sep = "|---|" + "|".join(["---"] * len(results)) + "|"
    lines.append(header)
    lines.append(sep)
    for check in all_checks:
        row = f"| {check} |"
        for r in results:
            util = r.checks.get(check)
            row += f" {util:.4f} |" if util is not None else " N/A |"
        lines.append(row)
    lines.append("")

    lines.append("## Per-code summary")
    lines.append("")
    lines.append("| Code | Governing check | Max utilisation | Verdict |")
    lines.append("|---|---|---|---|")
    for r in results:
        verdict = "PASS" if r.is_safe else "FAIL"
        lines.append(
            f"| {r.code_label} | {r.governing_check or 'N/A'} | "
            f"{r.max_utilisation:.3f} | {verdict} |"
        )
    lines.append("")
    lines.append("Generated by digitalmodel structural analysis - "
                 "multi-code wall thickness comparison.")
    lines.append("")
    return "\n".join(lines)
