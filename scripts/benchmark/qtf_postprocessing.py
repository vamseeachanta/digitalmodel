#!/usr/bin/env python3
"""
QTF Postprocessing for L00 validation suite.

Exports .owr → .xlsx via OrcFxAPI.SaveResultsSpreadsheet, parses all
QTF sheets (including direct/indirect potential loads unavailable from the
OrcFxAPI properties directly), and generates comparison plots.

Reference plots (from WAMIT validation paper):
  - Case 3.1: 5-panel sum-frequency QTF (diffraction RAO + 4 QTF components)
  - Case 3.2: 4-panel diff-frequency QTF per DOF (PI, CS, direct, indirect potential)
  - Case 3.3: 6-DOF diff-frequency subplots

Usage:
    uv run python scripts/benchmark/qtf_postprocessing.py --case 3.2
    uv run python scripts/benchmark/qtf_postprocessing.py --case 3.1 3.2 3.3
    uv run python scripts/benchmark/qtf_postprocessing.py --all
    uv run python scripts/benchmark/qtf_postprocessing.py --case 3.2 --force-export
"""

from __future__ import annotations

import argparse
import base64
import cmath
import math
from pathlib import Path

import numpy as np
import plotly.graph_objects as go
from plotly.subplots import make_subplots
import openpyxl
import OrcFxAPI

# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

DOF_NAMES = ["Surge", "Sway", "Heave", "Roll", "Pitch", "Yaw"]
DOF_UNITS_FORCE = ["kN/m", "kN/m", "kN/m", "kN·m/m", "kN·m/m", "kN·m/m"]
DOF_UNITS_DRIFT = ["kN/m²", "kN/m²", "kN/m²", "kN·m/m²", "kN·m/m²", "kN·m/m²"]

_C_REAL = "#1877c5"
_C_IMAG = "#c81818"

L00_DIR = (
    Path(__file__).resolve().parents[2]
    / "docs/modules/orcawave/L00_validation_wamit"
)

CASES: dict[str, dict] = {
    "3.1": {
        "owr": L00_DIR / "3.1/benchmark/Bottom mounted cylinder_ground_truth.owr",
        "output": L00_DIR / "3.1/benchmark",
        "label": "Bottom-mounted cylinder (Full QTF)",
        "qtf_type": "sum_freq",
        "dofs": [0],  # Surge — all DOFs fixed, others zero
    },
    "3.2": {
        "owr": L00_DIR / "3.2/benchmark/Sphere_ground_truth.owr",
        "output": L00_DIR / "3.2/benchmark",
        "label": "Sphere with Lid",
        "qtf_type": "diff_freq",
        "dofs": [0, 2],  # Surge (comparison_1) and Heave (comparison_2)
    },
    "3.3": {
        "owr": L00_DIR / "3.3/benchmark/test_ground_truth.owr",
        "output": L00_DIR / "3.3/benchmark",
        "label": "Multi-body Cylinder + Ellipsoid",
        "qtf_type": "diff_freq",
        "dofs": list(range(6)),  # All DOFs
    },
}

# Reference screenshots from WAMIT validation paper — embedded side-by-side in HTML
_REFERENCE_IMAGES: dict[str, dict] = {
    "3.1": {
        0: L00_DIR / "3.1/QTF_plot.png",
    },
    "3.2": {
        0: L00_DIR / "3.2/comparison_1.png",
        2: L00_DIR / "3.2/comparison_2.png",
    },
    "3.3": {
        "global": L00_DIR / "3.3/Comparison_1.png",
    },
}

# ---------------------------------------------------------------------------
# Digitised WAMIT reference data (approximate × marker values from paper)
# Source: OrcaWave WAMIT Validation Guide figures 31, 33, 34, 36
# Accuracy: ±5–10% of full-scale; sufficient for visual comparison overlay
# ---------------------------------------------------------------------------
#
# Structure:
#   "3.1"  → sum_freq  → panel_key → {"omega", "real", "imag"}
#   "3.2"  → diff_freq → dw → dof  → panel_key → {"omega", "real", "imag"}
#   "3.3"  → diff_freq → dw → panel_key → {dof: {"omega","real","imag"}}
#
_W = {
    "3.1": {
        # Figure 31 — surge sum-frequency QTF, x-axis ω 0.5-3.0 rad/s
        "load_rao_diffraction": {
            "omega": [0.5,  1.0,  1.5,  2.0,  2.5,  3.0],
            "real":  [0.5,  5.0,  17.0, 27.5, 31.0, 28.5],
            "imag":  [0.3,  2.5,  9.0,  11.5, 9.0,  6.5],
        },
        "mean_drift_pi": {
            "omega": [0.5,  1.0,  1.5,  2.0,  2.5,  3.0],
            "real":  [0.5,  3.5,  8.5,  10.0, 10.5, 9.5],
            "imag":  [0.0,  0.0,  0.0,  0.0,  0.0,  0.0],
        },
        "quadratic_pi": {
            "omega": [0.5,  1.0,  1.5,  2.0,  2.5,  3.0],
            "real":  [0.0,  0.5,  2.0,  5.0,  6.0,  5.5],
            "imag":  [0.0, -0.5, -2.5, -7.0, -11.0, -12.0],
        },
        "potential_direct": {
            "omega": [0.5,   1.0,  1.5,  2.0, 2.5, 3.0],
            "real":  [100.0, 8.0,  1.5,  0.0, 0.0, 0.0],
            "imag":  [550.0, 15.0, 0.0,  0.0, 0.0, 0.0],
        },
        "potential_indirect": {
            "omega": [0.5,   1.0,  1.5,  2.0, 2.5, 3.0],
            "real":  [100.0, 8.0,  1.5,  0.0, 0.0, 0.0],
            "imag":  [550.0, 15.0, 0.0,  0.0, 0.0, 0.0],
        },
    },
    "3.2": {
        # Figures 33 (surge) and 34 (heave) — diff-frequency Δω=0.1 rad/s
        # x-axis ω₁ 1.0-4.5 rad/s
        0.1: {
            0: {  # surge (comparison_1.png)
                "quadratic_pi": {
                    "omega": [1.0, 1.5, 2.0, 2.5, 3.0, 3.1, 3.5, 4.0, 4.5],
                    "real":  [0.0, 0.0, 0.1, 0.5, 4.5, 9.0, 6.8, 6.5, 6.2],
                    "imag":  [0.1, 0.1, 0.2, 0.5, 1.0, 1.0, 0.5, 0.3, 0.2],
                },
                "quadratic_cs": {
                    "omega": [1.0, 1.5, 2.0, 2.5, 3.0, 3.1, 3.5, 4.0, 4.5],
                    "real":  [0.0, 0.0, 0.1, 0.5, 4.5, 8.5, 6.8, 6.5, 6.2],
                    "imag":  [0.1, 0.1, 0.2, 0.5, 1.0, 1.0, 0.5, 0.3, 0.2],
                },
                "potential_direct": {
                    "omega": [1.0,  1.5,   2.0,   2.5,   3.0,   3.1,   3.5,   4.0,   4.5],
                    "real":  [0.0,  0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0],
                    "imag":  [-0.40, -0.32, -0.30, -0.29, -0.28, -0.28, -0.28, -0.28, -0.28],
                },
                "potential_indirect": {
                    "omega": [1.0,  1.5,   2.0,   2.5,   3.0,   3.1,   3.5,   4.0,   4.5],
                    "real":  [0.0,  0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0],
                    "imag":  [-0.40, -0.32, -0.30, -0.29, -0.28, -0.28, -0.28, -0.28, -0.28],
                },
            },
            2: {  # heave (comparison_2.png)
                "quadratic_pi": {
                    "omega": [1.0, 1.5, 2.0, 2.5, 3.0,  3.1, 3.5,  4.0,  4.5],
                    "real":  [2.5, 2.5, 2.5, 2.5, 6.5,  7.0, 2.0, -1.5, -3.0],
                    "imag":  [0.0, 0.0, 0.1, 0.1, 0.1,  0.2, 0.1,  0.1,  0.1],
                },
                "quadratic_cs": {
                    "omega": [1.0, 1.5, 2.0, 2.5, 3.0,  3.1, 3.5,  4.0,  4.5],
                    "real":  [2.5, 2.5, 2.5, 2.5, 6.5,  7.2, 2.0, -1.5, -3.0],
                    "imag":  [0.0, 0.0, 0.1, 0.1, 0.1,  0.2, 0.1,  0.1,  0.1],
                },
                "potential_direct": {
                    "omega": [1.0,  1.5,  2.0,  2.5,  3.0,  3.1,  3.5, 4.0,  4.5],
                    "real":  [0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0, 0.0,  0.0],
                    "imag":  [-60.0, -38.0, -22.0, -12.0, -6.0, -5.5, -3.0, -1.5, -0.8],
                },
                "potential_indirect": {
                    "omega": [1.0,  1.5,  2.0,  2.5,  3.0,  3.1,  3.5, 4.0,  4.5],
                    "real":  [0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0, 0.0,  0.0],
                    "imag":  [-60.0, -38.0, -22.0, -12.0, -6.0, -5.5, -3.0, -1.5, -0.8],
                },
            },
        },
    },
    "3.3": {
        # Figure 36 — Full QTF (PI quadratic + indirect potential) for cylinder
        # Δω=2.5 rad/s, β₁=0°, β₂=30°, x-axis ω₁ 1.0-4.0 rad/s
        # NOTE: reference shows combined (quadratic_pi + potential_indirect)
        # We overlay on quadratic_pi as the dominant contribution
        2.5: {
            # Per-DOF full-QTF reference values [dof_index]: {"omega","real","imag"}
            0: {  # Surge
                "omega": [1.0,  1.5,  2.0,  2.5,  3.0, 3.5, 4.0],
                "real":  [0.0,  -0.8, -3.8, -2.0, 0.0, 0.5, 0.3],
                "imag":  [-0.5, -2.2, -2.5, -0.8, 0.3, 0.4, 0.3],
            },
            1: {  # Sway
                "omega": [1.0,  1.5,  2.0, 2.5, 3.0, 3.5, 4.0],
                "real":  [-2.0, -1.5, 0.5, 2.5, 2.0, 1.0, 0.8],
                "imag":  [-7.5, -5.0, -2.0, 0.0, 2.0, 2.5, 2.5],
            },
            2: {  # Heave
                "omega": [1.0, 1.5,  2.0,  2.5,  3.0,  3.5,  4.0],
                "real":  [0.2, 0.3,  0.2,  0.0, -0.1, -0.1,  0.0],
                "imag":  [0.1, -0.2, -0.4, -0.1,  0.2,  0.1,  0.0],
            },
            3: {  # Roll
                "omega": [1.0, 1.5,  2.0,  2.5,  3.0,  3.5,  4.0],
                "real":  [0.0, -0.5, -1.5, -3.5, -5.5, -6.0, -5.0],
                "imag":  [0.0,  0.0,  0.0,  0.0, -0.5, -1.5, -2.0],
            },
            4: {  # Pitch
                "omega": [1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0],
                "real":  [6.5, 5.5, 4.0, 3.0, 2.0, 1.5, 1.0],
                "imag":  [0.5, 2.0, 3.0, 2.5, 1.5, 0.8, 0.3],
            },
            5: {  # Yaw
                "omega": [1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0],
                "real":  [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0],
                "imag":  [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0],
            },
        },
    },
}

# ---------------------------------------------------------------------------
# Excel Export (OrcFxAPI)
# ---------------------------------------------------------------------------

def export_spreadsheet(
    owr_path: Path, xlsx_path: Path | None = None, force: bool = False
) -> Path:
    """Export OrcaWave .owr results to Excel via SaveResultsSpreadsheet.

    Uses the existing file when force=False and it already exists.
    This is the canonical way to get direct/indirect potential loads
    which are not exposed as OrcFxAPI array properties.
    """
    xlsx_path = xlsx_path or owr_path.with_suffix(".xlsx")
    if xlsx_path.exists() and not force:
        print(f"  Using existing:  {xlsx_path.name}")
        return xlsx_path
    print(f"  Exporting to:    {xlsx_path.name} ...")
    d = OrcFxAPI.Diffraction(str(owr_path))
    d.SaveResultsSpreadsheet(str(xlsx_path))
    print(f"  Done.")
    return xlsx_path


# ---------------------------------------------------------------------------
# Excel Parser
# ---------------------------------------------------------------------------

def _to_complex(ampl, phase_deg) -> complex:
    """Polar (amplitude, phase°) → complex.  Uses cmath.exp for complex exponent."""
    return float(ampl) * cmath.exp(1j * math.radians(float(phase_deg)))


def _numeric_rows(ws) -> list[list]:
    """Yield rows whose first cell is a number (skips all header rows)."""
    rows = []
    for row in ws.iter_rows(values_only=True):
        if row[0] is None:
            continue
        try:
            float(row[0])
            rows.append(list(row))
        except (TypeError, ValueError):
            continue
    return rows


def _parse_rao_sheet(ws) -> dict:
    """RAO sheet layout: [heading, omega, Ampl₀, Phase₀, …, Ampl₅, Phase₅]."""
    h, omega, vals = [], [], []
    for row in _numeric_rows(ws):
        try:
            h.append(float(row[0]))
            omega.append(float(row[1]))
            vals.append([_to_complex(row[2 + 2 * i], row[3 + 2 * i]) for i in range(6)])
        except (TypeError, ValueError, IndexError):
            pass
    if not omega:
        return {}
    return {
        "heading": np.array(h),
        "omega": np.array(omega),
        "values": np.array(vals, dtype=complex),  # (nrows, 6)
    }


def _parse_drift_sheet(ws) -> dict:
    """Drift sheet: [heading1, heading2, omega, Ampl₀, Phase₀, …]."""
    h1, h2, omega, vals = [], [], [], []
    for row in _numeric_rows(ws):
        try:
            h1.append(float(row[0]))
            h2.append(float(row[1]))
            omega.append(float(row[2]))
            vals.append([_to_complex(row[3 + 2 * i], row[4 + 2 * i]) for i in range(6)])
        except (TypeError, ValueError, IndexError):
            pass
    if not omega:
        return {}
    return {
        "heading1": np.array(h1),
        "heading2": np.array(h2),
        "omega": np.array(omega),
        "values": np.array(vals, dtype=complex),  # (nrows, 6)
    }


def _parse_qtf_sheet(ws) -> dict:
    """QTF sheet: [h1, h2, omega1, omega2, omega_combined, Ampl₀, Phase₀, …]."""
    h1, h2, o1, o2, oc, vals = [], [], [], [], [], []
    for row in _numeric_rows(ws):
        try:
            h1.append(float(row[0]))
            h2.append(float(row[1]))
            o1.append(float(row[2]))
            o2.append(float(row[3]))
            oc.append(float(row[4]))
            vals.append([_to_complex(row[5 + 2 * i], row[6 + 2 * i]) for i in range(6)])
        except (TypeError, ValueError, IndexError):
            pass
    if not o1:
        return {}
    return {
        "heading1": np.array(h1),
        "heading2": np.array(h2),
        "omega1": np.array(o1),
        "omega2": np.array(o2),
        "omega_c": np.array(oc),
        "values": np.array(vals, dtype=complex),  # (nrows, 6)
    }


_SHEET_PARSERS = {
    "Load RAOs (diffraction)": ("load_rao_diffraction", _parse_rao_sheet),
    "Load RAOs (Haskind)":     ("load_rao_haskind",     _parse_rao_sheet),
    "Displacement RAOs":       ("displacement_rao",      _parse_rao_sheet),
    "Mean drift loads (PI)":   ("mean_drift_pi",         _parse_drift_sheet),
    "Mean drift loads (CS)":   ("mean_drift_cs",         _parse_drift_sheet),
    "Quadratic loads (PI)":    ("quadratic_pi",          _parse_qtf_sheet),
    "Quadratic loads (CS)":    ("quadratic_cs",          _parse_qtf_sheet),
    "Potential loads (direct)":   ("potential_direct",   _parse_qtf_sheet),
    "Potential loads (indirect)": ("potential_indirect", _parse_qtf_sheet),
}


def read_excel(xlsx_path: Path) -> dict:
    """Parse all QTF sheets from an OrcaWave results spreadsheet."""
    wb = openpyxl.load_workbook(str(xlsx_path), read_only=True, data_only=True)
    result: dict = {}
    for sheet_name, (key, parser) in _SHEET_PARSERS.items():
        result[key] = parser(wb[sheet_name]) if sheet_name in wb.sheetnames else {}
    wb.close()
    return result


# ---------------------------------------------------------------------------
# OrcFxAPI Direct Extractor (cross-check / complement)
# ---------------------------------------------------------------------------

def read_owr(owr_path: Path, heading_pair_idx: int = 0) -> dict:
    """Extract QTF data directly from .owr via OrcFxAPI properties.

    Note: direct/indirect potential loads are NOT available here —
    use read_excel() for those (from SaveResultsSpreadsheet output).
    """
    d = OrcFxAPI.Diffraction(str(owr_path))
    hp = heading_pair_idx

    def _get(attr):
        v = getattr(d, attr, None)
        if v is None:
            return None
        arr = np.array(v)
        return arr[hp] if arr.ndim >= 2 else arr

    return {
        "omega_1st": np.sort(2 * np.pi * np.array(d.frequencies)),
        "qtf_omega": np.array(d.QTFAngularFrequencies),   # (nqtf, 3)
        "heading_pairs": np.array(d.QTFHeadingPairs),
        "load_rao_diffraction": _get("loadRAOsDiffraction"),  # (nfreq, 6) complex
        "load_rao_haskind":     _get("loadRAOsHaskind"),
        "mean_drift_pi": _get("meanDriftLoadPressureIntegration"),  # (nfreq, 6)
        "mean_drift_cs": _get("meanDriftLoadControlSurface"),
        "quadratic_pi":  _get("quadraticLoadFromPressureIntegration"),  # (nqtf, 6)
        "quadratic_cs":  _get("quadraticLoadFromControlSurface"),
    }


# ---------------------------------------------------------------------------
# Plot Helpers
# ---------------------------------------------------------------------------

def _cline(color: str, dash: str = "solid") -> dict:
    return {"color": color, "dash": dash, "width": 1.5}


def _traces(omega, values, name: str, show_legend: bool = True) -> list:
    """Pair of real + imaginary Scatter traces."""
    kw = dict(mode="lines+markers", marker=dict(size=5))
    return [
        go.Scatter(x=omega, y=np.real(values), name=f"{name} (real)",
                   line=_cline(_C_REAL), showlegend=show_legend,
                   legendgroup=name, **kw),
        go.Scatter(x=omega, y=np.imag(values), name=f"{name} (imag)",
                   line=_cline(_C_IMAG, "dash"), showlegend=show_legend,
                   legendgroup=name, **kw),
    ]


def _add(fig, traces, row: int, col: int) -> None:
    for t in traces:
        fig.add_trace(t, row=row, col=col)


def _wamit_traces(ref: dict, show_legend: bool = True) -> list:
    """Return marker-only Scatter traces for WAMIT reference data overlay.

    Args:
        ref: dict with keys "omega", "real", "imag" (lists or arrays)
        show_legend: whether to show in legend (set False after first call)
    """
    omega = ref["omega"]
    real = ref["real"]
    imag = ref["imag"]
    _mk = dict(symbol="x", size=10, line=dict(width=2))
    return [
        go.Scatter(
            x=omega, y=real, name="WAMIT (real)",
            mode="markers", marker=dict(color=_C_REAL, **_mk),
            showlegend=show_legend, legendgroup="wamit",
        ),
        go.Scatter(
            x=omega, y=imag, name="WAMIT (imag)",
            mode="markers", marker=dict(color=_C_IMAG, **_mk),
            showlegend=show_legend, legendgroup="wamit",
        ),
    ]


def _slice_delta(qtf_dict: dict, delta_omega: float, tol: float = 0.01) -> tuple:
    """Return (omega1_slice, values_slice) for rows near |ω₁ - ω₂| ≈ delta_omega."""
    if not qtf_dict:
        return np.array([]), np.zeros((0, 6), dtype=complex)
    o1, o2 = qtf_dict["omega1"], qtf_dict["omega2"]
    mask = np.abs(np.abs(o1 - o2) - delta_omega) < tol
    return o1[mask], qtf_dict["values"][mask]


# ---------------------------------------------------------------------------
# Figure builders (return go.Figure — used by both file writers and HTML embed)
# ---------------------------------------------------------------------------

def _build_sum_freq_figure(
    xdata: dict, dof: int, label: str, wamit_ref: dict | None = None
) -> go.Figure:
    """5-panel sum-frequency QTF figure matching Figure 31 of WAMIT validation paper."""
    dname = DOF_NAMES[dof]
    uf, ud = DOF_UNITS_FORCE[dof], DOF_UNITS_DRIFT[dof]

    def _wref(key):
        return (wamit_ref or {}).get(key)

    fig = make_subplots(
        rows=3, cols=2,
        specs=[[{"colspan": 2}, None], [{}, {}], [{}, {}]],
        subplot_titles=[
            f"Diffraction load RAO [{uf}]",
            f"Mean drift load (PI) [{ud}]", f"Quadratic load (PI) [{ud}]",
            f"Direct potential load [{ud}]", f"Indirect potential load [{ud}]",
        ],
        vertical_spacing=0.13, horizontal_spacing=0.10,
    )

    # Panel 1 — Diffraction load RAO (amplitude only, not complex)
    rao = xdata.get("load_rao_diffraction", {})
    if rao:
        fig.add_trace(go.Scatter(
            x=rao["omega"], y=np.abs(rao["values"][:, dof]),
            mode="lines+markers", name="OrcaWave (amplitude)",
            line=_cline(_C_REAL), marker=dict(size=5),
        ), row=1, col=1)
    ref = _wref("load_rao_diffraction")
    if ref:
        _add(fig, _wamit_traces(ref, show_legend=True), 1, 1)

    # Panel 2 — Mean drift PI (real scalar on diagonal)
    drift = xdata.get("mean_drift_pi", {})
    if drift:
        fig.add_trace(go.Scatter(
            x=drift["omega"], y=np.real(drift["values"][:, dof]),
            mode="lines+markers", name="OrcaWave",
            line=_cline(_C_REAL), marker=dict(size=5),
        ), row=2, col=1)
    ref = _wref("mean_drift_pi")
    if ref:
        _add(fig, _wamit_traces(ref, show_legend=False), 2, 1)

    # Panel 3 — Quadratic load PI: diagonal (ω₁==ω₂) or all points
    qpi = xdata.get("quadratic_pi", {})
    if qpi:
        o1, o2 = qpi["omega1"], qpi["omega2"]
        diag = np.abs(o1 - o2) < 1e-6
        sel = diag if diag.any() else np.ones(len(o1), dtype=bool)
        _add(fig, _traces(o1[sel], qpi["values"][sel, dof], "OrcaWave"), 2, 2)
    ref = _wref("quadratic_pi")
    if ref:
        _add(fig, _wamit_traces(ref, show_legend=False), 2, 2)

    # Panel 4 — Direct potential
    pd = xdata.get("potential_direct", {})
    if pd:
        _add(fig, _traces(pd["omega1"], pd["values"][:, dof], "OrcaWave",
                          show_legend=False), 3, 1)
    ref = _wref("potential_direct")
    if ref:
        _add(fig, _wamit_traces(ref, show_legend=False), 3, 1)

    # Panel 5 — Indirect potential
    pi_ = xdata.get("potential_indirect", {})
    if pi_:
        _add(fig, _traces(pi_["omega1"], pi_["values"][:, dof], "OrcaWave",
                          show_legend=False), 3, 2)
    ref = _wref("potential_indirect")
    if ref:
        _add(fig, _wamit_traces(ref, show_legend=False), 3, 2)

    fig.update_xaxes(title_text="ω (rad/s)")
    fig.update_layout(
        title=f"{label} — {dname} QTF components (sum-frequency)",
        height=820, width=960,
        legend=dict(orientation="h", x=0, y=-0.05),
    )
    return fig


def _build_diff_freq_dof_figure(
    xdata: dict, dof: int, delta_omega: float, label: str,
    wamit_ref: dict | None = None,
) -> go.Figure:
    """4-panel diff-frequency QTF figure matching Figures 33/34 of WAMIT paper."""
    dname = DOF_NAMES[dof]
    ud = DOF_UNITS_DRIFT[dof]
    dw_str = f"{delta_omega:.3f}".rstrip("0").rstrip(".")

    fig = make_subplots(
        rows=2, cols=2,
        subplot_titles=[
            f"PI quadratic load [{ud}]", f"CS quadratic load [{ud}]",
            f"Direct potential load [{ud}]", f"Indirect potential load [{ud}]",
        ],
        vertical_spacing=0.15, horizontal_spacing=0.12,
    )

    panels = [
        ("quadratic_pi",       1, 1),
        ("quadratic_cs",       1, 2),
        ("potential_direct",   2, 1),
        ("potential_indirect", 2, 2),
    ]
    first_legend = True
    for key, row, col in panels:
        omega1, vals = _slice_delta(xdata.get(key, {}), delta_omega)
        if omega1.size:
            _add(fig, _traces(omega1, vals[:, dof], "OrcaWave",
                              show_legend=(row == 1 and col == 1)), row, col)
        ref = (wamit_ref or {}).get(key)
        if ref:
            _add(fig, _wamit_traces(ref, show_legend=first_legend), row, col)
            first_legend = False

    fig.update_xaxes(title_text="ω₁ (rad/s)")
    fig.update_layout(
        title=f"{label} — {dname} QTF (diff-frequency, Δω = {dw_str} rad/s)",
        height=680, width=960,
        legend=dict(orientation="h", x=0, y=-0.06),
    )
    return fig


def _build_diff_freq_all_dofs_figure(
    xdata: dict, key: str, delta_omega: float, label: str,
    suffix: str = "", wamit_ref: dict | None = None,
) -> go.Figure | None:
    """6-DOF subplot for one QTF component at a fixed Δω slice (Figure 36 style)."""
    omega1, values = _slice_delta(xdata.get(key, {}), delta_omega)
    if not omega1.size:
        return None

    fig = make_subplots(
        rows=3, cols=2,
        subplot_titles=DOF_NAMES,
        vertical_spacing=0.10, horizontal_spacing=0.12,
    )
    first_wamit_legend = True
    for i in range(6):
        row, col = i // 2 + 1, i % 2 + 1
        _add(fig, _traces(omega1, values[:, i], "OrcaWave", show_legend=(i == 0)),
             row, col)
        fig.update_yaxes(title_text=DOF_UNITS_DRIFT[i], row=row, col=col)
        ref = (wamit_ref or {}).get(i)
        if ref:
            _add(fig, _wamit_traces(ref, show_legend=first_wamit_legend), row, col)
            first_wamit_legend = False

    dw_str = f"{delta_omega:.3f}".rstrip("0").rstrip(".")
    fig.update_xaxes(title_text="ω₁ (rad/s)")
    fig.update_layout(
        title=(
            f"{label} — All DOFs: {key} (Δω = {dw_str} rad/s)"
            + (f" {suffix}" if suffix else "")
        ),
        height=900, width=960,
        legend=dict(orientation="h", x=0, y=-0.04),
    )
    return fig


# ---------------------------------------------------------------------------
# Plot: write standalone HTML files
# ---------------------------------------------------------------------------

def plot_sum_freq(xdata: dict, dof: int, label: str, output_dir: Path) -> Path:
    """Build and write 5-panel sum-frequency QTF HTML file."""
    fig = _build_sum_freq_figure(xdata, dof, label)
    dname = DOF_NAMES[dof]
    out = output_dir / f"qtf_sum_freq_{dname.lower()}.html"
    fig.write_html(str(out))
    print(f"  Plot: {out.name}")
    return out


def plot_diff_freq_dof(
    xdata: dict, dof: int, delta_omega: float, label: str, output_dir: Path
) -> Path:
    """Build and write 4-panel diff-frequency QTF HTML file for one DOF."""
    fig = _build_diff_freq_dof_figure(xdata, dof, delta_omega, label)
    dname = DOF_NAMES[dof]
    dw_str = f"{delta_omega:.3f}".rstrip("0").rstrip(".")
    tag = dw_str.replace(".", "p")
    out = output_dir / f"qtf_diff_freq_dw{tag}_{dname.lower()}.html"
    fig.write_html(str(out))
    print(f"  Plot: {out.name}")
    return out


def plot_diff_freq_all_dofs(
    xdata: dict, key: str, delta_omega: float, label: str,
    output_dir: Path, suffix: str = ""
) -> Path | None:
    """Build and write 6-DOF diff-frequency QTF HTML file."""
    fig = _build_diff_freq_all_dofs_figure(xdata, key, delta_omega, label, suffix)
    if fig is None:
        return None
    dw_str = f"{delta_omega:.3f}".rstrip("0").rstrip(".")
    tag = dw_str.replace(".", "p")
    out = output_dir / f"qtf_{key}_dw{tag}{suffix}.html"
    fig.write_html(str(out))
    print(f"  Plot: {out.name}")
    return out


# ---------------------------------------------------------------------------
# HTML embedding helpers
# ---------------------------------------------------------------------------

def _img_to_base64(path: Path | None) -> str | None:
    """Encode an image file as a base64 data URI for self-contained HTML."""
    if path is None or not path.exists():
        return None
    suffix = path.suffix.lower().lstrip(".")
    mime = {
        "png": "image/png",
        "jpg": "image/jpeg",
        "jpeg": "image/jpeg",
    }.get(suffix, "image/png")
    data = base64.b64encode(path.read_bytes()).decode("ascii")
    return f"data:{mime};base64,{data}"


def _make_figure_section(title: str, fig_html: str, ref_b64: str | None) -> str:
    """Wrap a Plotly inline figure in a styled card with optional reference screenshot."""
    _label_style = (
        "font-size:0.72em;font-weight:700;letter-spacing:0.08em;"
        "text-transform:uppercase;color:#7f8c9a;margin:0 0 8px 0;"
    )
    orcawave_col = (
        f'<div style="flex:1;min-width:380px;padding:1.2em 1.4em;">'
        f'<p style="{_label_style}">OrcaWave Output</p>'
        f"{fig_html}"
        f"</div>"
    )
    ref_col = ""
    if ref_b64:
        ref_col = (
            '<div style="flex:0 0 auto;max-width:460px;min-width:200px;'
            'padding:1.2em 1.4em;border-left:1px solid #e8ecf0;background:#f8f9fb;">'
            f'<p style="{_label_style}">Reference (WAMIT paper)</p>'
            f'<img src="{ref_b64}" style="max-width:100%;display:block;'
            'border:1px solid #dde3ea;border-radius:4px;'
            'box-shadow:0 1px 6px rgba(0,0,0,0.07);"/>'
            "</div>"
        )
    return (
        '<div class="qtf-figure" style="'
        "background:#fff;border:1px solid #dde3ea;border-radius:7px;"
        "overflow:hidden;margin-bottom:2em;"
        'box-shadow:0 2px 8px rgba(0,0,0,0.05);">'
        # Card header
        '<div style="padding:0.65em 1.4em;background:#f0f4f8;'
        'border-bottom:1px solid #dde3ea;">'
        f'<h3 style="margin:0;font-size:0.95em;color:#1a3a5c;font-weight:600;">'
        f"{title}</h3>"
        "</div>"
        # Card body — side-by-side columns
        f'<div style="display:flex;align-items:stretch;flex-wrap:wrap;">'
        f"{orcawave_col}{ref_col}"
        "</div>"
        "</div>"
    )


# ---------------------------------------------------------------------------
# Inline HTML builder (for injecting into benchmark_report.html)
# ---------------------------------------------------------------------------

def build_inline_html(
    case_id: str,
    force_export: bool = False,
    max_delta_omegas: int = 3,
) -> str:
    """Build a self-contained HTML section with QTF plots + reference screenshots.

    Plotly figures are embedded inline (include_plotlyjs=False — assumes the parent
    HTML already loads Plotly, or the CDN script tag included in the section header
    provides it). Reference screenshots are base64-encoded for full portability.

    Parameters
    ----------
    max_delta_omegas:
        Maximum number of Δω slices to embed inline for diff-frequency cases.
        Selects min / middle / max from all available slices to give a
        representative range without bloating the HTML file.
    """
    case = CASES[case_id]
    owr_path: Path = case["owr"]
    label: str = case["label"]
    ref_imgs = _REFERENCE_IMAGES.get(case_id, {})

    if not owr_path.exists():
        return (
            '<section id="qtf-analysis" style="margin:2.5em 0;padding:1em 1.5em;'
            'border:1px solid #f5c6cb;border-radius:7px;background:#fff5f5;">'
            '<p style="margin:0;color:#c0392b;font-weight:600;">'
            f"[QTF] OWR not found: {owr_path.name}</p>"
            "</section>"
        )

    xlsx_path = export_spreadsheet(owr_path, force=force_export)
    xdata = read_excel(xlsx_path)
    populated = [k for k, v in xdata.items() if v]
    print(f"  [QTF inline] Sheets: {populated}")

    sections: list[str] = []

    def _closest_dw_ref(w_case: dict, dw: float) -> dict | None:
        """Return _W sub-dict for the Δω key closest to dw (handles float precision)."""
        if not w_case:
            return None
        closest = min(w_case.keys(), key=lambda k: abs(k - dw))
        return w_case[closest] if abs(closest - dw) < 0.5 else None

    if case["qtf_type"] == "sum_freq":
        wamit_ref_31 = _W.get("3.1")
        for dof in case["dofs"]:
            fig = _build_sum_freq_figure(xdata, dof, label, wamit_ref=wamit_ref_31)
            fig_html = fig.to_html(full_html=False, include_plotlyjs=False)
            ref_b64 = _img_to_base64(ref_imgs.get(dof))
            sections.append(_make_figure_section(
                f"QTF Sum-Frequency — {DOF_NAMES[dof]}",
                fig_html, ref_b64,
            ))

    else:  # diff_freq
        delta_omegas = _unique_delta_omegas(xdata)
        if not delta_omegas:
            delta_omegas = [0.1]

        # Limit inline figures: select min, (mid), max Δω values for representative coverage
        if len(delta_omegas) > max_delta_omegas:
            indices = sorted({
                0,
                len(delta_omegas) // 2,
                len(delta_omegas) - 1,
            })[:max_delta_omegas]
            delta_omegas = [delta_omegas[i] for i in indices]
            print(f"  [QTF inline] Showing {len(delta_omegas)} representative Δω slices")

        for dw in delta_omegas:
            dw_str = f"{dw:.3f}".rstrip("0").rstrip(".")
            for dof in case["dofs"]:
                dw_ref = _closest_dw_ref(_W.get(case_id, {}), dw)
                wamit_ref_dof = (dw_ref or {}).get(dof)
                fig = _build_diff_freq_dof_figure(
                    xdata, dof, dw, label, wamit_ref=wamit_ref_dof
                )
                fig_html = fig.to_html(full_html=False, include_plotlyjs=False)
                ref_b64 = _img_to_base64(ref_imgs.get(dof))
                sections.append(_make_figure_section(
                    f"QTF Diff-Frequency — {DOF_NAMES[dof]} (Δω = {dw_str} rad/s)",
                    fig_html, ref_b64,
                ))

            if case_id == "3.3":
                dw_ref = _closest_dw_ref(_W.get("3.3", {}), dw)
                wamit_ref_alldofs = dw_ref  # dict keyed by dof int
                for key in ("quadratic_pi", "potential_indirect"):
                    fig = _build_diff_freq_all_dofs_figure(
                        xdata, key, dw, label,
                        wamit_ref=wamit_ref_alldofs,
                    )
                    if fig is not None:
                        fig_html = fig.to_html(full_html=False, include_plotlyjs=False)
                        ref_b64 = _img_to_base64(ref_imgs.get("global"))
                        sections.append(_make_figure_section(
                            f"QTF {key} — All DOFs (Δω = {dw_str} rad/s)",
                            fig_html, ref_b64,
                        ))

    # CDN script ensures Plotly is available even if parent HTML doesn't include it
    plotly_cdn = (
        '<script src="https://cdn.plot.ly/plotly-latest.min.js" '
        'charset="utf-8"></script>'
    )
    section_header = (
        # Gradient banner
        '<div style="'
        "background:linear-gradient(135deg,#1a3a5c 0%,#2c6fad 100%);"
        'padding:1.1em 1.6em;display:flex;align-items:center;gap:0.9em;">'
        '<span style="font-size:1.6em;line-height:1;">&#127754;</span>'
        '<div>'
        '<h2 style="margin:0;color:#fff;font-size:1.15em;font-weight:700;'
        'letter-spacing:0.02em;">QTF Analysis</h2>'
        '<p style="margin:0.2em 0 0;color:rgba(255,255,255,0.72);font-size:0.82em;">'
        "Quadratic Transfer Functions — OrcaWave vs. WAMIT reference</p>"
        "</div>"
        "</div>"
    )
    return (
        '<section id="qtf-analysis" style="'
        "margin:2.5em 0;border-radius:8px;overflow:hidden;"
        "border:1px solid #d0d8e4;"
        'box-shadow:0 3px 14px rgba(0,0,0,0.08);">\n'
        f"{plotly_cdn}\n"
        f"{section_header}\n"
        '<div style="padding:1.5em;background:#f4f6f9;">\n'
        + "\n".join(sections)
        + "\n</div>\n</section>\n"
    )


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def _unique_delta_omegas(xdata: dict) -> list[float]:
    """Return sorted list of unique |Δω| values > 0 found in QTF data."""
    for key in ("quadratic_pi", "quadratic_cs", "potential_indirect"):
        d = xdata.get(key, {})
        if d:
            dws = np.abs(d["omega1"] - d["omega2"])
            nz = dws[dws > 0.005]
            if nz.size:
                return sorted(set(np.round(nz, 4).tolist()))
    return []


# ---------------------------------------------------------------------------
# Case Runner
# ---------------------------------------------------------------------------

def run_case(case_id: str, force_export: bool = False) -> list[Path]:
    """Process one case: export Excel → parse → generate all QTF HTML files."""
    case = CASES[case_id]
    owr_path: Path = case["owr"]
    output_dir: Path = case["output"]
    label: str = case["label"]

    if not owr_path.exists():
        print(f"[Case {case_id}] SKIP — {owr_path.name} not found")
        return []

    print(f"\n[Case {case_id}] {label}")

    # 1. Export Excel from .owr
    xlsx_path = export_spreadsheet(owr_path, force=force_export)

    # 2. Parse all QTF sheets
    xdata = read_excel(xlsx_path)
    populated = [k for k, v in xdata.items() if v]
    print(f"  Sheets parsed:   {populated}")

    # 3. Generate plots
    plots: list[Path] = []

    if case["qtf_type"] == "sum_freq":
        for dof in case["dofs"]:
            p = plot_sum_freq(xdata, dof, label, output_dir)
            plots.append(p)

    else:  # diff_freq
        delta_omegas = _unique_delta_omegas(xdata)
        if not delta_omegas:
            print("  WARNING: no off-diagonal QTF pairs found")
            delta_omegas = [0.1]

        for dw in delta_omegas:
            # Per-DOF 4-panel plots
            for dof in case["dofs"]:
                p = plot_diff_freq_dof(xdata, dof, dw, label, output_dir)
                plots.append(p)

            # 6-DOF plots for quadratic_pi and potential_indirect
            if case_id == "3.3":
                for key in ("quadratic_pi", "potential_indirect"):
                    p = plot_diff_freq_all_dofs(xdata, key, dw, label, output_dir)
                    if p:
                        plots.append(p)

    return plots


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------

def main() -> None:
    parser = argparse.ArgumentParser(
        description="QTF postprocessing for L00 OrcaWave validation suite"
    )
    parser.add_argument("--case", nargs="+", choices=list(CASES), default=None,
                        metavar="ID", help="Case IDs to process (e.g. 3.1 3.2)")
    parser.add_argument("--all", action="store_true", help="Process all cases")
    parser.add_argument("--force-export", action="store_true",
                        help="Re-export Excel even if it already exists")
    args = parser.parse_args()

    cases = list(CASES) if args.all else (args.case or list(CASES))
    all_plots: list[Path] = []
    for cid in cases:
        all_plots.extend(run_case(cid, force_export=args.force_export))

    print(f"\nTotal plots generated: {len(all_plots)}")
    for p in all_plots:
        print(f"  {p}")


if __name__ == "__main__":
    main()
