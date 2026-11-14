"""Riser configuration screening across multiple load cases and design codes.

# ABOUTME: Iterate operating/extreme/survival/accidental riser load cases.
# ABOUTME: Run the selected registered code per case and rank the governing case.

Each load case carries its own pressures, bending moment, effective tension and
limit-state category. For every case the registered design-code strategy is run
(API STD 2RD by default), to pick the governing
check and report the max utilisation. Cases are then ranked by max utilisation so
the governing load case for the riser configuration is the first row.

Deterministic and unit-consistent (SI: metres, Pascals, Newtons, N*m). No random
sampling, no synthetic placeholders -- every number traces to a declared input.
"""
from __future__ import annotations

import logging
import json
from dataclasses import asdict, dataclass, field
from datetime import datetime, timezone
from hashlib import sha256
from pathlib import Path
from typing import Dict, List, Optional

from digitalmodel import __version__
from digitalmodel.reporting.provenance import Provenance
from digitalmodel.structural.analysis.wall_thickness import (
    DesignCode,
    DesignFactors,
    DesignLoads,
    PipeGeometry,
    PipeMaterial,
    SafetyClass,
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
    details: Dict[str, Dict[str, object]] = field(default_factory=dict)
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
        code: Registered design code (defaults to API STD 2RD).
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
    factors = DesignFactors(safety_class=SafetyClass(rs.get("safety_class", "medium")))
    results = screen_riser_cases(geometry, material, cases, code=code, factors=factors)

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
                "details": r.details,
                "governing_check": r.governing_check,
                "max_utilisation": r.max_utilisation,
                "status": r.status,
            }
            for r in results
        ],
    }
    rs["result"]["screening_status"] = rs["result"]["overall_status"].lower()
    cfg["screening_status"] = rs["result"]["screening_status"]
    context = _report_context(geometry, material, cases, code, factors, cfg, results)
    rs["result"].update(context)

    out_dir = Path(
        rs.get("output_dir")
        or cfg.get("Analysis", {}).get("result_folder", "results/riser_screening")
    )
    if not out_dir.is_absolute() and cfg.get("_config_dir_path"):
        out_dir = Path(cfg["_config_dir_path"]) / out_dir
    out_dir.mkdir(parents=True, exist_ok=True)
    label = rs.get("label", "riser_screening")

    note = build_calc_note(geometry, material, code, results, label)
    for title, key in (("Input echo", "input_echo"), ("Provenance", "provenance")):
        note += f"\n## {title}\n\n```json\n{json.dumps(context[key], indent=2)}\n```\n"
    (out_dir / f"{label}_calcnote.md").write_text(note, encoding="utf-8")
    (out_dir / f"{label}_results.json").write_text(
        json.dumps(rs["result"], indent=2), encoding="utf-8"
    )
    logger.info("Riser screening calc note written to %s", out_dir)

    return cfg


def _report_context(geometry, material, cases, code, factors, cfg, results):
    """Echo resolved SI inputs; timestamp the source snapshot, not execution.

    Input-file modification time is stable across consecutive runs. In-memory
    inputs use the implementation-file modification time, explicitly labelled.
    File locators are reduced to filenames to avoid disclosing local paths.
    """
    rs = cfg["riser_screening"]
    strategy = CODE_REGISTRY[code]()
    factor_keys = {"f_d", "f_c", "gamma_sc", "gamma_m", "gamma_dyn", "alpha_U",
                   "f_0", "alpha_fab", "temperature_derating"}
    applied = {name: {k: v for k, v in details.items() if k in factor_keys}
               for name, details in results[0].details.items()} if results else {}
    material_echo = asdict(material)
    material_echo["fabrication_type"] = material.fabrication_type.value
    echo = {
        "geometry": asdict(geometry), "material": material_echo,
        "load_cases": [asdict(case) for case in cases], "code": code.value,
        "factors": {"safety_class": factors.safety_class.value,
                    "strategy": getattr(strategy, "get_factors", lambda: {})(),
                    "applied": applied},
    }
    input_file = cfg.get("_config_file_path")
    source = Path(input_file) if input_file else Path(__file__)
    timestamp = datetime.fromtimestamp(source.stat().st_mtime, timezone.utc).isoformat()
    provenance = Provenance()
    if input_file:
        provenance.add("file", source.name, digest="sha256:" + sha256(source.read_bytes()).hexdigest(),
                       retrieved_at=timestamp, description="Screening input file snapshot")
    provenance.add(
        "configuration", rs.get("label", "riser_screening"),
        digest="sha256:" + sha256(json.dumps(
            echo, sort_keys=True, separators=(",", ":"), ensure_ascii=False,
            allow_nan=False).encode()).hexdigest(),
        retrieved_at=timestamp, description="Resolved screening inputs, including defaults",
    )
    return {"input_echo": echo, "provenance": {
        **provenance.require().model_dump(), "package_version": __version__,
        "timestamp_basis": ("input file modification time (UTC)" if input_file
                            else "implementation file modification time (UTC)"),
        "units": "SI: m, Pa, N, N*m; utilisation and factors dimensionless",
    }}


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
    check_names = CODE_REGISTRY[code].check_names
    lines.append(f"# Riser Configuration Screening - {label}")
    lines.append("")
    lines.append(f"**Design code:** {code.value} ({', '.join(check_names)})")
    overall = "PASS" if all(r.status == "PASS" for r in results) else "FAIL"
    lines.append(f"**Overall status:** {overall}")
    lines.append(f"**screening_status:** {overall.lower()}")
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
    lines.append(f"| Wall thickness | {wt_mm:.3f} mm |")
    lines.append(f"| Corrosion allowance | {ca_mm:.3f} mm |")
    lines.append(f"| D/t ratio | {geometry.d_over_t:.1f} |")
    lines.append(f"| Grade | {material.grade} |")
    lines.append(f"| SMYS | {material.smys / 1e6:.0f} MPa |")
    lines.append(f"| SMTS | {material.smts / 1e6:.0f} MPa |")
    lines.append("")

    lines.append("## Per-case utilisation (ranked, governing first)")
    lines.append("")
    columns = ["Rank", "Case", "Category"] + [
        name.replace("_", " ").title() for name in check_names
    ] + ["Governing", "Max util", "Status"]
    lines.append("| " + " | ".join(columns) + " |")
    lines.append("|" + "|".join("---" for _ in columns) + "|")
    for i, r in enumerate(results, start=1):
        values = " | ".join(f"{r.checks[name]:.3f}" for name in check_names)
        lines.append(
            f"| {i} | {r.name} | {r.category} | {values} | "
            f"{r.governing_check} | {r.max_utilisation:.3f} | "
            f"{r.status} |"
        )
    lines.append("")

    if code == DesignCode.DNV_ST_F201:
        lines.extend(_f201_basis(results))
        return "\n".join(lines)
    if code != DesignCode.API_STD_2RD:
        return "\n".join(lines)

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
    return "\n".join(lines)


def _f201_basis(results):
    """Clause references and the actual strategy factors for the calc note."""
    from digitalmodel.structural.analysis.wall_thickness_codes.dnv_st_f201 import (
        DnvStF201Strategy,
    )

    factors = DnvStF201Strategy().get_factors()
    lines = [
        "## Check basis (DNV-ST-F201 LRFD)", "",
        "DNV-ST-F201 (2021), formerly DNV-OS-F201 (2010), Sec. 5: bursting "
        "limit state and combined loading, clause D. Eqs 5.25-5.26 give "
        "plastic resistances Mk/Tk including the pressure-dependent flow-stress "
        "coefficient alpha_c, recomputed for every load case.", "",
        f"Factors: {factors}. Combined safety class medium maps to normal.",
        "Moment and tension are design load effects (already factored Md/Td). "
        "gamma_dyn acts on the pressure-containment demand only. Temperature "
        "de-rating is zero because screening inputs carry no temperature.", "",
        "For external overpressure, p_min_internal = internal_pressure; no "
        "separate minimum-pressure input is available. Characteristic collapse "
        "resistance p_c uses DNV-ST-F101 Eq. 5.9, with its default ovality "
        "and the supplied material's fabrication factor. Net pressure above "
        "characteristic burst resistance exceeds the combined criterion's "
        "domain and is reported as a failed check with infinite utilisation.", "",
    ]
    if results:
        combined = results[0].details["combined"]
        burst = results[0].details["pressure_containment"]
        lines.extend([
            f"Applied gamma_sc: burst {burst['gamma_sc']}, combined "
            f"{combined['gamma_sc']}; gamma_m {combined['gamma_m']}.", "",
        ])
    return lines
