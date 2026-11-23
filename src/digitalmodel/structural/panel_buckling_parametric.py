# ABOUTME: Parametric stiffened-panel buckling sweep over ship hull grades (DNV-RP-C201).
# ABOUTME: Pure-Python + pandas domain layer: run_sweep, utility_curves, write_outputs (CSV + API JSON).
"""
Parametric ship stiffened-panel buckling sweep
==============================================

Builds a full-factorial sweep of stiffened-panel buckling checks (plate field +
single longitudinal tee stiffener) across marine hull grades (Grade A / AH36 /
EH40), plate thicknesses, stiffener spacings, stiffener profiles and combined
load cases, reusing the *validated* ``StiffenedPanelBucklingAnalyzer``
(DNV-RP-C201 three-mode screening: plate-field, overall column, stiffener
torsional/tripping per Sec. 7.5.2).

The layer is pure data: it produces row dicts, chart-ready utility curves, and
an API-friendly ``panel_results.json`` with O(1) lookup indices (utilisation and
governing mode). No plotting here.
"""

from __future__ import annotations

import itertools
import json
import math
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any

import pandas as pd

from digitalmodel.structural.structural_analysis.models import MARINE_GRADES
from digitalmodel.structural.structural_analysis.panel_buckling import (
    StiffenerGeometry,
    StiffenedPanelGeometry,
    StiffenedPanelBucklingAnalyzer,
)


STANDARD = "DNV-RP-C201"
MODULE_NAME = "digitalmodel.structural.panel_buckling_parametric"
VALID_MODES = ("plate_induced", "column", "torsional")


@dataclass
class PanelSweepConfig:
    """Configuration for a full-factorial stiffened-panel buckling sweep.

    All lengths in mm, all stresses in MPa. ``profiles`` is a list of dicts with
    keys ``name``, ``web_height``, ``web_thickness``, ``flange_width``,
    ``flange_thickness`` (tee sections).
    """

    plate_thicknesses: list[float]
    stiffener_spacings: list[float]
    profiles: list[dict]
    load_cases: list[tuple[float, float]]  # (sigma_x, tau)
    grades: list[str]
    plate_length: float = 2400.0           # transverse-frame span L (mm)
    gamma_m: float = 1.15

    def n_cases(self) -> int:
        return (
            len(self.grades)
            * len(self.plate_thicknesses)
            * len(self.stiffener_spacings)
            * len(self.profiles)
            * len(self.load_cases)
        )


# ---------------------------------------------------------------------------
# Default ship stiffened-panel sweep.
#
# Plate thicknesses 8..20 mm (typical hull plating), three stiffener spacings,
# three realistic tee profiles, one transverse-frame span, four load cases
# (three pure-axial + one axial+shear), across all three IACS grades.
# Full factorial => 3 * 7 * 3 * 3 * 4 = 756 cases.
# ---------------------------------------------------------------------------
DEFAULT_PANEL_SWEEP = PanelSweepConfig(
    plate_thicknesses=[8.0, 10.0, 12.0, 14.0, 16.0, 18.0, 20.0],
    stiffener_spacings=[600.0, 700.0, 800.0],
    profiles=[
        {"name": "T250x90", "web_height": 250.0, "web_thickness": 10.0,
         "flange_width": 90.0, "flange_thickness": 12.0},
        {"name": "T400x120", "web_height": 400.0, "web_thickness": 12.0,
         "flange_width": 120.0, "flange_thickness": 16.0},
        {"name": "T600x200", "web_height": 600.0, "web_thickness": 10.0,
         "flange_width": 200.0, "flange_thickness": 20.0},
    ],
    load_cases=[
        (100.0, 0.0),    # pure axial, moderate
        (150.0, 0.0),    # pure axial, high  (chart reference case)
        (200.0, 0.0),    # pure axial, severe
        (120.0, 40.0),   # axial + shear
    ],
    grades=["Grade A", "AH36", "EH40"],
    plate_length=2400.0,
    gamma_m=1.15,
)

# Chart / curve reference selection (used by utility_curves and the demo driver).
REFERENCE_SPACING = 700.0
REFERENCE_PROFILE = "T400x120"
REFERENCE_LOAD = (150.0, 0.0)


def _round(x: float, n: int = 4) -> float:
    return round(float(x), n)


def _build_panel(
    cfg: PanelSweepConfig, thickness: float, spacing: float, profile: dict
) -> StiffenedPanelGeometry:
    stiff = StiffenerGeometry(
        web_height=profile["web_height"],
        web_thickness=profile["web_thickness"],
        flange_width=profile["flange_width"],
        flange_thickness=profile["flange_thickness"],
        spacing=spacing,
        section_type="tee",
    )
    return StiffenedPanelGeometry(
        plate_length=cfg.plate_length,
        plate_thickness=thickness,
        stiffener=stiff,
    )


def run_sweep(cfg: PanelSweepConfig) -> list[dict]:
    """Run the full-factorial stiffened-panel buckling sweep.

    Returns one dict per case. One analyzer is built per grade and reused.
    """
    rows: list[dict] = []

    analyzers: dict[str, StiffenedPanelBucklingAnalyzer] = {}
    for grade in cfg.grades:
        analyzers[grade] = StiffenedPanelBucklingAnalyzer(MARINE_GRADES[grade])

    for grade, thickness, spacing, profile, load in itertools.product(
        cfg.grades,
        cfg.plate_thicknesses,
        cfg.stiffener_spacings,
        cfg.profiles,
        cfg.load_cases,
    ):
        analyzer = analyzers[grade]
        material = MARINE_GRADES[grade]
        fy = material.yield_strength
        sigma_x, tau = load

        panel = _build_panel(cfg, thickness, spacing, profile)
        res = analyzer.check_panel(
            panel, sigma_x=sigma_x, sigma_y=0.0, tau=tau, gamma_m=cfg.gamma_m
        )

        h_w = profile["web_height"]
        t_w = profile["web_thickness"]

        rows.append(
            {
                "grade": grade,
                "fy": _round(fy, 1),
                "thickness_mm": _round(thickness, 2),
                "spacing_mm": _round(spacing, 1),
                "profile": profile["name"],
                "web_height_mm": _round(h_w, 1),
                "web_thickness_mm": _round(t_w, 1),
                "flange_width_mm": _round(profile["flange_width"], 1),
                "flange_thickness_mm": _round(profile["flange_thickness"], 1),
                "web_slenderness": _round(h_w / t_w, 3),
                "plate_length_mm": _round(cfg.plate_length, 1),
                "sigma_x": _round(sigma_x, 2),
                "tau": _round(tau, 2),
                "utilization": _round(res.utilization, 4),
                "governing_mode": res.governing_mode,
                "plate_util": _round(res.plate_utilization, 4),
                "column_util": _round(res.column_utilization, 4),
                "torsional_util": _round(res.stiffener_utilization, 4),
                "critical_stress_mpa": _round(res.critical_stress, 2),
                "status": "PASS" if res.utilization <= 1.0 else "FAIL",
            }
        )

    return rows


def utility_curves(
    cfg: PanelSweepConfig,
    ref_spacing: float = REFERENCE_SPACING,
    ref_profile: str = REFERENCE_PROFILE,
    ref_load: tuple[float, float] = REFERENCE_LOAD,
) -> dict:
    """Per-grade chart series at a reference spacing / profile / load.

    For each grade produces ordered series of total utilisation, the three
    component utilisations (plate / column / torsional) and governing mode vs
    plate thickness. Also returns the governing-mode split (counts) over the
    full sweep.
    """
    sigma_x, tau = ref_load
    profile = next(
        (p for p in cfg.profiles if p["name"] == ref_profile), cfg.profiles[0]
    )
    ref_profile = profile["name"]

    curves: dict[str, Any] = {
        "reference": {
            "spacing_mm": ref_spacing,
            "profile": ref_profile,
            "sigma_x": sigma_x,
            "tau": tau,
            "plate_length_mm": cfg.plate_length,
        },
        "grades": {},
        "mode_split": {},
    }

    thicknesses = sorted(cfg.plate_thicknesses)

    for grade in cfg.grades:
        material = MARINE_GRADES[grade]
        analyzer = StiffenedPanelBucklingAnalyzer(material)

        thk_series: list[float] = []
        util_series: list[float] = []
        plate_series: list[float] = []
        column_series: list[float] = []
        tors_series: list[float] = []
        mode_series: list[str] = []

        for t in thicknesses:
            panel = _build_panel(cfg, t, ref_spacing, profile)
            res = analyzer.check_panel(
                panel, sigma_x=sigma_x, sigma_y=0.0, tau=tau, gamma_m=cfg.gamma_m
            )
            thk_series.append(_round(t, 2))
            util_series.append(_round(res.utilization, 4))
            plate_series.append(_round(res.plate_utilization, 4))
            column_series.append(_round(res.column_utilization, 4))
            tors_series.append(_round(res.stiffener_utilization, 4))
            mode_series.append(res.governing_mode)

        curves["grades"][grade] = {
            "fy": _round(material.yield_strength, 1),
            "thickness_mm": thk_series,
            "utilization": util_series,
            "plate_util": plate_series,
            "column_util": column_series,
            "torsional_util": tors_series,
            "governing_mode": mode_series,
        }

    # Governing-mode split over the entire sweep.
    rows = run_sweep(cfg)
    split: dict[str, int] = {m: 0 for m in VALID_MODES}
    for r in rows:
        split[r["governing_mode"]] = split.get(r["governing_mode"], 0) + 1
    curves["mode_split"] = split

    return curves


def _index_key(row: dict) -> str:
    return "|".join(
        str(row[k])
        for k in (
            "grade",
            "thickness_mm",
            "spacing_mm",
            "profile",
            "sigma_x",
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
    """Write panel_cases.csv and panel_results.json. Returns the written paths."""
    out_dir = Path(out_dir)
    out_dir.mkdir(parents=True, exist_ok=True)

    csv_path = out_dir / "panel_cases.csv"
    df = pd.DataFrame(rows)
    df.to_csv(csv_path, index=False)

    # Preserve first-seen grade order for stable, readable output.
    grades: list[str] = []
    for r in rows:
        if r["grade"] not in grades:
            grades.append(r["grade"])

    # O(1) lookup indices keyed by '|'-joined case fields.
    index_util: dict[str, float] = {}
    index_mode: dict[str, str] = {}
    for r in rows:
        key = _index_key(r)
        index_util[key] = r["utilization"]
        index_mode[key] = r["governing_mode"]

    payload: dict[str, Any] = {
        "meta": {
            "standard": STANDARD,
            "module": MODULE_NAME,
            "n_cases": len(rows),
            "grades": grades,
            "gamma_m": gamma_m,
            "units": {"stress": "MPa", "length": "mm"},
            "modes": list(VALID_MODES),
            "generated_by": "panel_buckling_parametric",
            "validated": True,
        },
        "lookup": rows,
        "index": index_util,
        "index_mode": index_mode,
        "curves": curves,
    }
    if timestamp is not None:
        payload["meta"]["generated_at"] = timestamp

    json_path = out_dir / "panel_results.json"
    json_path.write_text(json.dumps(payload, indent=2), encoding="utf-8")

    return {"cases_csv": csv_path, "results_json": json_path}
