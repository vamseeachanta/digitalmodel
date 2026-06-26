# ABOUTME: Parametric plate-buckling sweep over ship hull-plate materials (DNV-RP-C201).
# ABOUTME: Pure-Python + pandas domain layer: run_sweep, utility_curves, write_outputs (CSV + API JSON).
"""
Parametric ship-plate buckling sweep
====================================

Builds a full-factorial sweep of unstiffened plate-field buckling checks across
marine hull-plate grades (Grade A / AH36 / EH40), thicknesses, plate breadths,
spans and combined load cases, reusing the verified ``PlateBucklingAnalyzer``
(DNV-RP-C201 elastic buckling + Johnson-Ostenfeld inelastic correction).

The layer is pure data: it produces row dicts, chart-ready utility curves, and
an API-friendly ``results.json`` with an O(1) lookup index. No plotting here.
"""

from __future__ import annotations

import itertools
import json
import math
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any

import pandas as pd

from digitalmodel.structural.structural_analysis.models import (
    MARINE_GRADES,
    PlateGeometry,
)
from digitalmodel.structural.structural_analysis.buckling import PlateBucklingAnalyzer


STANDARD = "DNV-RP-C201"
MODULE_NAME = "digitalmodel.structural.buckling_parametric"


@dataclass
class BucklingSweepConfig:
    """Configuration for a full-factorial plate-buckling sweep.

    All lengths in mm, all stresses in MPa.
    """

    thicknesses: list[float]
    widths: list[float]
    plate_lengths: list[float]
    load_cases: list[tuple[float, float, float]]  # (sigma_x, sigma_y, tau)
    grades: list[str]
    gamma_m: float = 1.15

    def n_cases(self) -> int:
        return (
            len(self.grades)
            * len(self.thicknesses)
            * len(self.widths)
            * len(self.plate_lengths)
            * len(self.load_cases)
        )


# ---------------------------------------------------------------------------
# Default ship-plate sweep.
#
# Thicknesses 8..25 mm (typical hull plating), three stiffener spacings / plate
# breadths, one representative transverse-frame span, and six load cases spanning
# pure axial, axial+shear and biaxial states, across all three IACS marine
# grades. Full factorial => 18 * 3 * 1 * 6 * 3 = 972 cases.
# ---------------------------------------------------------------------------
DEFAULT_SHIP_PLATE_SWEEP = BucklingSweepConfig(
    thicknesses=[float(t) for t in range(8, 26)],          # 8..25 mm, 1 mm step
    widths=[600.0, 800.0, 1000.0],                          # stiffener spacing / breadth
    plate_lengths=[2400.0],                                 # transverse frame span
    load_cases=[
        (100.0, 0.0, 0.0),    # pure axial, moderate
        (150.0, 0.0, 0.0),    # pure axial, high  (chart reference case)
        (200.0, 0.0, 0.0),    # pure axial, severe
        (120.0, 0.0, 40.0),   # axial + shear
        (120.0, 60.0, 0.0),   # biaxial compression
        (100.0, 50.0, 50.0),  # combined biaxial + shear
    ],
    grades=["Grade A", "AH36", "EH40"],
    gamma_m=1.15,
)

# Chart / curve references (used by utility_curves and the demo driver).
REFERENCE_WIDTH = 800.0
REFERENCE_LENGTH = 2400.0
REFERENCE_LOAD = (150.0, 0.0, 0.0)


def slenderness_beta(width: float, thickness: float, fy: float, E: float) -> float:
    """Plate slenderness beta = (b/t) * sqrt(fy/E)."""
    return (width / thickness) * math.sqrt(fy / E)


def _round(x: float, n: int = 4) -> float:
    return round(float(x), n)


def run_sweep(cfg: BucklingSweepConfig) -> list[dict]:
    """Run the full-factorial buckling sweep.

    Returns one dict per case. Analyzers are built once per grade and reused.
    """
    rows: list[dict] = []

    # Build one analyzer per grade (cheap, but avoids re-instantiation in loop).
    analyzers: dict[str, PlateBucklingAnalyzer] = {}
    for grade in cfg.grades:
        material = MARINE_GRADES[grade]
        analyzers[grade] = PlateBucklingAnalyzer(material)

    for grade, thickness, width, length, load in itertools.product(
        cfg.grades,
        cfg.thicknesses,
        cfg.widths,
        cfg.plate_lengths,
        cfg.load_cases,
    ):
        analyzer = analyzers[grade]
        material = MARINE_GRADES[grade]
        fy = material.yield_strength
        E = material.youngs_modulus
        sigma_x, sigma_y, tau = load

        plate = PlateGeometry(length=length, width=width, thickness=thickness)
        res = analyzer.check_plate_buckling(
            plate, sigma_x=sigma_x, sigma_y=sigma_y, tau=tau, gamma_m=cfg.gamma_m
        )

        beta = slenderness_beta(width, thickness, fy, E)

        rows.append(
            {
                "grade": grade,
                "fy": _round(fy, 1),
                "thickness_mm": _round(thickness, 2),
                "width_mm": _round(width, 1),
                "length_mm": _round(length, 1),
                "aspect": _round(length / width, 3),
                "slenderness_beta": _round(beta, 4),
                "sigma_x": _round(sigma_x, 2),
                "sigma_y": _round(sigma_y, 2),
                "tau": _round(tau, 2),
                "utilization": _round(res.utilization, 4),
                "critical_stress_mpa": _round(res.critical_stress, 2),
                "safety_factor": _round(res.safety_factor, 4)
                if math.isfinite(res.safety_factor)
                else None,
                "governing": "plate_buckling",
                "status": "PASS" if res.utilization <= 1.0 else "FAIL",
            }
        )

    return rows


def utility_curves(
    cfg: BucklingSweepConfig,
    ref_width: float = REFERENCE_WIDTH,
    ref_length: float = REFERENCE_LENGTH,
    ref_load: tuple[float, float, float] = REFERENCE_LOAD,
) -> dict:
    """Per-grade chart series.

    For each grade, at the reference width/length/load, produce ordered series
    of utilization vs thickness, critical_stress vs thickness, and utilization
    vs slenderness beta.
    """
    sigma_x, sigma_y, tau = ref_load
    curves: dict[str, Any] = {
        "reference": {
            "width_mm": ref_width,
            "length_mm": ref_length,
            "sigma_x": sigma_x,
            "sigma_y": sigma_y,
            "tau": tau,
        },
        "grades": {},
    }

    thicknesses = sorted(cfg.thicknesses)

    for grade in cfg.grades:
        material = MARINE_GRADES[grade]
        analyzer = PlateBucklingAnalyzer(material)
        fy = material.yield_strength
        E = material.youngs_modulus

        thk_series: list[float] = []
        util_series: list[float] = []
        crit_series: list[float] = []
        beta_series: list[float] = []

        for t in thicknesses:
            plate = PlateGeometry(length=ref_length, width=ref_width, thickness=t)
            res = analyzer.check_plate_buckling(
                plate, sigma_x=sigma_x, sigma_y=sigma_y, tau=tau, gamma_m=cfg.gamma_m
            )
            thk_series.append(_round(t, 2))
            util_series.append(_round(res.utilization, 4))
            crit_series.append(_round(res.critical_stress, 2))
            beta_series.append(_round(slenderness_beta(ref_width, t, fy, E), 4))

        curves["grades"][grade] = {
            "fy": _round(fy, 1),
            "thickness_mm": thk_series,
            "utilization": util_series,
            "critical_stress_mpa": crit_series,
            "slenderness_beta": beta_series,
        }

    return curves


def _index_key(row: dict) -> str:
    return "|".join(
        str(row[k])
        for k in (
            "grade",
            "thickness_mm",
            "width_mm",
            "length_mm",
            "sigma_x",
            "sigma_y",
            "tau",
        )
    )


def write_outputs(
    rows: list[dict],
    curves: dict,
    out_dir: str | Path,
    gamma_m: float = 1.15,
    timestamp: str | None = None,
) -> dict[str, Path]:
    """Write cases.csv and results.json. Returns the written paths."""
    out_dir = Path(out_dir)
    out_dir.mkdir(parents=True, exist_ok=True)

    # cases.csv
    csv_path = out_dir / "cases.csv"
    df = pd.DataFrame(rows)
    df.to_csv(csv_path, index=False)

    # Preserve first-seen grade order for stable, readable output.
    grades: list[str] = []
    for r in rows:
        if r["grade"] not in grades:
            grades.append(r["grade"])

    # O(1) lookup indices keyed by '|'-joined case fields.
    index_util: dict[str, float] = {}
    index_status: dict[str, str] = {}
    for r in rows:
        key = _index_key(r)
        index_util[key] = r["utilization"]
        index_status[key] = r["status"]

    payload: dict[str, Any] = {
        "meta": {
            "standard": STANDARD,
            "module": MODULE_NAME,
            "n_cases": len(rows),
            "grades": grades,
            "gamma_m": gamma_m,
            "units": {"stress": "MPa", "length": "mm"},
            "generated_by": "buckling_parametric",
            "preliminary": False,
        },
        "lookup": rows,
        "index": index_util,
        "index_status": index_status,
        "curves": curves,
    }
    if timestamp is not None:
        payload["meta"]["generated_at"] = timestamp

    json_path = out_dir / "results.json"
    json_path.write_text(json.dumps(payload, indent=2), encoding="utf-8")

    return {"cases_csv": csv_path, "results_json": json_path}
