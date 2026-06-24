"""Riser configuration screening across multiple load cases (API STD 2RD).

# ABOUTME: Iterate operating/extreme/survival/accidental riser load cases.
# ABOUTME: Run API STD 2RD burst+collapse+combined per case, rank governing case.

Each load case carries its own pressures, bending moment, effective tension and
limit-state category. For every case we run the registered API STD 2RD strategy
(burst, collapse, combined moment-tension-pressure Method 1), pick the governing
check and report the max utilisation. Cases are then ranked by max utilisation so
the governing load case for the riser configuration is the first row.

Deterministic and unit-consistent (SI: metres, Pascals, Newtons, N*m). No random
sampling, no synthetic placeholders -- every number traces to a declared input.
"""
from __future__ import annotations

import logging
from dataclasses import dataclass, field
from typing import Dict, List, Optional

from digitalmodel.structural.analysis.wall_thickness import (
    DesignCode,
    DesignFactors,
    DesignLoads,
    PipeGeometry,
    PipeMaterial,
)
from digitalmodel.structural.analysis.wall_thickness_codes import CODE_REGISTRY

logger = logging.getLogger(__name__)


@dataclass
class RiserLoadCase:
    """A single riser design load case (SI units)."""

    name: str
    category: str  # operating | extreme | survival | accidental
    internal_pressure: float = 0.0  # Pa
    external_pressure: float = 0.0  # Pa
    bending_moment: float = 0.0     # N*m
    effective_tension: float = 0.0  # N

    def to_loads(self) -> DesignLoads:
        return DesignLoads(
            internal_pressure=self.internal_pressure,
            external_pressure=self.external_pressure,
            bending_moment=self.bending_moment,
            effective_tension=self.effective_tension,
        )


@dataclass
class CaseScreenResult:
    """Screening result for a single load case."""

    name: str
    category: str
    checks: Dict[str, float] = field(default_factory=dict)
    details: Dict[str, Dict[str, float]] = field(default_factory=dict)
    governing_check: Optional[str] = None
    max_utilisation: float = 0.0
    status: str = "PASS"


def screen_riser_cases(
    geometry: PipeGeometry,
    material: PipeMaterial,
    cases: List[RiserLoadCase],
    code: DesignCode = DesignCode.API_STD_2RD,
    factors: Optional[DesignFactors] = None,
) -> List[CaseScreenResult]:
    """Run the design code over every load case and rank by max utilisation.

    Args:
        geometry: Riser pipe geometry.
        material: Riser pipe material.
        cases: Load cases to screen.
        code: Design code (defaults to API STD 2RD; the only code with the
            combined moment-tension-pressure check).
        factors: Optional design factors (defaults to medium safety class).

    Returns:
        Per-case results sorted by max utilisation, descending (governing first).
    """
    if factors is None:
        factors = DesignFactors()

    strategy_cls = CODE_REGISTRY.get(code)
    if strategy_cls is None:
        raise ValueError(f"No strategy registered for {code}")
    strategy = strategy_cls()

    results: List[CaseScreenResult] = []
    for case in cases:
        raw = strategy.run_checks(geometry, material, case.to_loads(), factors)
        checks = {name: util for name, (util, _det) in raw.items()}
        details = {name: det for name, (_util, det) in raw.items()}

        max_util = max(checks.values()) if checks else 0.0
        governing = max(checks, key=checks.get) if checks else None
        status = "PASS" if max_util <= 1.0 else "FAIL"

        results.append(
            CaseScreenResult(
                name=case.name,
                category=case.category,
                checks=checks,
                details=details,
                governing_check=governing,
                max_utilisation=max_util,
                status=status,
            )
        )
        logger.info(
            "Riser case %s (%s): governing=%s, max_util=%.3f, status=%s",
            case.name, case.category, governing, max_util, status,
        )

    # Stable sort: by max utilisation descending; ties keep input order.
    results.sort(key=lambda r: r.max_utilisation, reverse=True)
    return results


def run_riser_screening(cfg: dict) -> dict:
    """Engine router for ``basename: riser_screening``.

    Reads geometry/material/load_cases from cfg, runs the screening, writes a
    calc-note artifact (Markdown + JSON), and stores the ranked results back on
    cfg under ``riser_screening.result``.
    """
    import json
    from pathlib import Path

    rs = cfg["riser_screening"]
    g = rs["geometry"]
    m = rs["material"]

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
    cases = [
        RiserLoadCase(
            name=c["name"],
            category=c.get("category", "operating"),
            internal_pressure=c.get("internal_pressure", 0.0),
            external_pressure=c.get("external_pressure", 0.0),
            bending_moment=c.get("bending_moment", 0.0),
            effective_tension=c.get("effective_tension", 0.0),
        )
        for c in rs["load_cases"]
    ]

    code = DesignCode(rs.get("code", "API-STD-2RD"))
    results = screen_riser_cases(geometry, material, cases, code=code)

    rs["result"] = {
        "code": code.value,
        "governing_case": results[0].name if results else None,
        "governing_check": results[0].governing_check if results else None,
        "max_utilisation": results[0].max_utilisation if results else 0.0,
        "overall_status": (
            "PASS" if all(r.status == "PASS" for r in results) else "FAIL"
        ),
        "cases": [
            {
                "name": r.name,
                "category": r.category,
                "checks": r.checks,
                "governing_check": r.governing_check,
                "max_utilisation": r.max_utilisation,
                "status": r.status,
            }
            for r in results
        ],
    }

    out_dir = Path(
        rs.get("output_dir")
        or cfg.get("Analysis", {}).get("result_folder", "results/riser_screening")
    )
    out_dir.mkdir(parents=True, exist_ok=True)
    label = rs.get("label", "riser_screening")

    note = build_calc_note(geometry, material, code, results, label)
    (out_dir / f"{label}_calcnote.md").write_text(note, encoding="utf-8")
    (out_dir / f"{label}_results.json").write_text(
        json.dumps(rs["result"], indent=2), encoding="utf-8"
    )
    logger.info("Riser screening calc note written to %s", out_dir)

    return cfg


def build_calc_note(
    geometry: PipeGeometry,
    material: PipeMaterial,
    code: DesignCode,
    results: List[CaseScreenResult],
    label: str,
) -> str:
    """Build a Markdown calc note: per-case utilisation table + ranking."""
    od_mm = geometry.outer_diameter * 1000
    wt_mm = geometry.wall_thickness * 1000
    ca_mm = geometry.corrosion_allowance * 1000

    lines: List[str] = []
    lines.append(f"# Riser Configuration Screening - {label}")
    lines.append("")
    lines.append(f"**Design code:** {code.value} "
                 "(burst, collapse, combined moment-tension Method 1)")
    overall = "PASS" if all(r.status == "PASS" for r in results) else "FAIL"
    lines.append(f"**Overall status:** {overall}")
    if results:
        gov = results[0]
        lines.append(
            f"**Governing case:** {gov.name} ({gov.category}) - "
            f"check `{gov.governing_check}`, utilisation {gov.max_utilisation:.3f}"
        )
    lines.append("")

    lines.append("## Geometry & Material")
    lines.append("")
    lines.append("| Property | Value |")
    lines.append("|---|---|")
    lines.append(f"| Outer diameter | {od_mm:.1f} mm |")
    lines.append(f"| Wall thickness | {wt_mm:.2f} mm |")
    lines.append(f"| Corrosion allowance | {ca_mm:.2f} mm |")
    lines.append(f"| D/t ratio | {geometry.d_over_t:.1f} |")
    lines.append(f"| Grade | {material.grade} |")
    lines.append(f"| SMYS | {material.smys / 1e6:.0f} MPa |")
    lines.append(f"| SMTS | {material.smts / 1e6:.0f} MPa |")
    lines.append("")

    lines.append("## Per-case utilisation (ranked, governing first)")
    lines.append("")
    lines.append("| Rank | Case | Category | Burst | Collapse | Combined | "
                 "Governing | Max util | Status |")
    lines.append("|---|---|---|---|---|---|---|---|---|")
    for i, r in enumerate(results, start=1):
        burst = r.checks.get("burst", float("nan"))
        collapse = r.checks.get("collapse", float("nan"))
        combined = r.checks.get("combined", float("nan"))
        lines.append(
            f"| {i} | {r.name} | {r.category} | {burst:.3f} | {collapse:.3f} | "
            f"{combined:.3f} | {r.governing_check} | {r.max_utilisation:.3f} | "
            f"{r.status} |"
        )
    lines.append("")

    lines.append("## Combined check basis (API STD 2RD Method 1)")
    lines.append("")
    lines.append("Linear moment-tension-pressure interaction:")
    lines.append("")
    lines.append("```")
    lines.append("p_b          = 0.45 * (SMYS + SMTS) * ln(OD / (OD - 2t))")
    lines.append("F_d_corr     = sqrt(max(F_d^2 - ((p_i - p_e) / p_b)^2, 0))")
    lines.append("utilisation  = (|T| / T_y + |M| / M_y) / F_d_corr")
    lines.append("T_y = SMYS * A,   M_y = (pi/4) * SMYS * (D - t)^2 * t")
    lines.append("```")
    lines.append("")
    lines.append("Reference: API STD 2RD, Section 5 (LRFD). The combined check "
                 "reproduces the legacy `APISTD2RDAnalyzer` Method 1 utilisation "
                 "(cross-validated to machine precision).")
    lines.append("")
    lines.append("> Note: DNV-ST-F201 is not implemented in this engine; only "
                 "API STD 2RD load-case screening is available here.")
    lines.append("")
    return "\n".join(lines)
