# ABOUTME: Builds utilisation + governing-mode grids over (plate thickness x
# ABOUTME: applied load) for each material x stiffener-profile x spacing combo,
# ABOUTME: feeding the interactive stiffened-panel design-window explorer.
"""Design-window grid builder for the stiffened-panel buckling explorer.

For every combination of material grade, stiffener profile and stiffener
spacing, this computes a 2-D grid of buckling *utilisation* over a range of
plate thickness (x) and applied longitudinal load (y), plus the governing
failure mode at each grid point and the maximum allowable load per thickness
(the util = 1.0 boundary).

The grids are produced by the validated
:class:`~digitalmodel.structural.structural_analysis.panel_buckling.StiffenedPanelBucklingAnalyzer`
so the interactive dashboard's "acceptable / unacceptable" map is a faithful
view of the same solver that passes the DNV-RP-C201 golden tests.

Units: thickness/spacing/length in mm, stresses in MPa.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Dict, List

from digitalmodel.structural.structural_analysis.models import MARINE_GRADES
from digitalmodel.structural.structural_analysis.panel_buckling import (
    StiffenerGeometry,
    StiffenedPanelGeometry,
    StiffenedPanelBucklingAnalyzer,
)


# Basic stiffener profiles (tee sections) offered in the explorer dropdown.
DEFAULT_PROFILES: List[Dict] = [
    {"name": "T250x90", "web_height": 250, "web_thickness": 10,
     "flange_width": 90, "flange_thickness": 12},
    {"name": "T400x120", "web_height": 400, "web_thickness": 12,
     "flange_width": 120, "flange_thickness": 16},
    {"name": "T600x200", "web_height": 600, "web_thickness": 10,
     "flange_width": 200, "flange_thickness": 20},
]

# Governing-mode integer encoding (for compact grids + categorical colouring).
MODE_CODE = {"plate_induced": 0, "column": 1, "torsional": 2}
MODE_LABEL = {0: "Plate-field", 1: "Column", 2: "Tripping"}


@dataclass
class ExplorerConfig:
    """Axes and selectable dimensions for the design-window explorer."""

    grades: List[str] = field(default_factory=lambda: list(MARINE_GRADES.keys()))
    profiles: List[Dict] = field(default_factory=lambda: DEFAULT_PROFILES)
    spacings: List[float] = field(default_factory=lambda: [600.0, 700.0, 800.0])
    spans: List[float] = field(default_factory=lambda: [1800.0, 2400.0, 3000.0, 3600.0])
    thickness_min: float = 6.0
    thickness_max: float = 24.0
    thickness_step: float = 0.5
    load_min: float = 0.0
    load_max: float = 280.0
    load_step: float = 10.0
    gamma_m: float = 1.15
    tau_levels: List[float] = field(default_factory=lambda: [0.0, 30.0, 60.0, 90.0])


def _frange(lo: float, hi: float, step: float) -> List[float]:
    n = int(round((hi - lo) / step))
    return [round(lo + i * step, 6) for i in range(n + 1)]


def combo_key(grade: str, profile: str, spacing: float, span: float,
              tau: float) -> str:
    """Stable key for a (grade, profile, spacing, span, shear) selection."""
    return f"{grade}|{profile}|{spacing:g}|{span:g}|{tau:g}"


def build_design_grids(cfg: ExplorerConfig | None = None) -> Dict:
    """Compute utilisation / governing-mode grids for every combo.

    Returns a JSON-serialisable dict:

    ``{"meta", "thickness", "load", "grades", "profiles", "spacings",
       "grids": {combo_key: {"util": [[..]], "mode": [[..]], "allow": [..]}}}``

    ``util[j][i]`` is the utilisation at load ``load[j]`` and thickness
    ``thickness[i]``; ``mode[j][i]`` is the MODE_CODE; ``allow[i]`` is the
    maximum load (MPa) with utilisation <= 1.0 at thickness ``thickness[i]``
    (None if even zero load fails — never expected).
    """
    cfg = cfg or ExplorerConfig()
    thicknesses = _frange(cfg.thickness_min, cfg.thickness_max, cfg.thickness_step)
    loads = _frange(cfg.load_min, cfg.load_max, cfg.load_step)
    profile_by_name = {p["name"]: p for p in cfg.profiles}

    grids: Dict[str, Dict] = {}
    for grade in cfg.grades:
        analyzer = StiffenedPanelBucklingAnalyzer(MARINE_GRADES[grade])
        for prof in cfg.profiles:
            for spacing in cfg.spacings:
                for span in cfg.spans:
                    for tau in cfg.tau_levels:
                        util_grid: List[List[float]] = []
                        mode_grid: List[List[int]] = []
                        allow: List[float | None] = [None] * len(thicknesses)
                        for j, load in enumerate(loads):
                            util_row: List[float] = []
                            mode_row: List[int] = []
                            for i, t in enumerate(thicknesses):
                                panel = _panel(prof, spacing, t, span)
                                res = analyzer.check_panel(
                                    panel, sigma_x=load, tau=tau,
                                    gamma_m=cfg.gamma_m,
                                )
                                util_row.append(round(res.utilization, 4))
                                mode_row.append(
                                    MODE_CODE.get(res.governing_mode, 0))
                                if res.utilization <= 1.0:
                                    allow[i] = load  # last passing load
                            util_grid.append(util_row)
                            mode_grid.append(mode_row)
                        grids[combo_key(grade, prof["name"], spacing, span, tau)] = {
                            "util": util_grid,
                            "mode": mode_grid,
                            "allow": allow,
                        }

    return {
        "meta": {
            "standard": "DNV-RP-C201",
            "module": "digitalmodel.structural.panel_design_explorer",
            "gamma_m": cfg.gamma_m,
            "mode_labels": MODE_LABEL,
            "units": {"stress": "MPa", "length": "mm"},
            "validated": True,
        },
        "thickness": thicknesses,
        "load": loads,
        "grades": cfg.grades,
        "profiles": [p["name"] for p in cfg.profiles],
        "spacings": [f"{s:g}" for s in cfg.spacings],
        "spans": [f"{s:g}" for s in cfg.spans],
        "taus": [f"{t:g}" for t in cfg.tau_levels],
        "grids": grids,
    }


def _panel(profile: Dict, spacing: float, thickness: float,
           plate_length: float) -> StiffenedPanelGeometry:
    stiff = StiffenerGeometry(
        web_height=profile["web_height"],
        web_thickness=profile["web_thickness"],
        flange_width=profile["flange_width"],
        flange_thickness=profile["flange_thickness"],
        spacing=spacing,
        section_type="tee",
    )
    return StiffenedPanelGeometry(
        plate_length=plate_length, plate_thickness=thickness, stiffener=stiff
    )
