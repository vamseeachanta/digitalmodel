# ABOUTME: Parametric sweep across wall thickness parameters with Plotly visualisation
# ABOUTME: Sweeps WT, OD, grade, load combos and produces utilisation DataFrames + charts

"""
Parametric Wall Thickness Sweep and Visualisation

Sweeps across wall thickness, OD, material grade, and load combinations.
Produces utilisation ratios in a pandas DataFrame and interactive Plotly charts.
"""

from dataclasses import dataclass, field
from typing import Dict, List, Optional
import itertools
import logging
import math

import numpy as np
import pandas as pd
import plotly.graph_objects as go
from plotly.subplots import make_subplots

from digitalmodel.structural.analysis.wall_thickness import (
    DesignCode,
    DesignFactors,
    DesignLoads,
    FabricationType,
    PipeGeometry,
    PipeMaterial,
    SafetyClass,
    WallThicknessAnalyzer,
)

logger = logging.getLogger(__name__)

# Material database (API 5L grades, Pa)
API_5L_GRADES: Dict[str, Dict[str, float]] = {
    "X42": {"smys": 289e6, "smts": 414e6},
    "X52": {"smys": 359e6, "smts": 455e6},
    "X60": {"smys": 414e6, "smts": 517e6},
    "X65": {"smys": 448e6, "smts": 531e6},
    "X70": {"smys": 483e6, "smts": 565e6},
    "X80": {"smys": 552e6, "smts": 621e6},
}


@dataclass
class SweepConfig:
    """Configuration for a parametric sweep."""

    wall_thicknesses: List[float]  # m
    outer_diameters: List[float] = field(default_factory=lambda: [0.27305])  # m
    grades: List[str] = field(default_factory=lambda: ["X65"])
    internal_pressures: List[float] = field(default_factory=lambda: [20e6])  # Pa
    external_pressures: List[float] = field(default_factory=lambda: [0.0])  # Pa
    bending_moments: List[float] = field(default_factory=lambda: [0.0])  # N·m
    effective_tensions: List[float] = field(default_factory=lambda: [0.0])  # N
    safety_class: SafetyClass = SafetyClass.MEDIUM
    fabrication_type: FabricationType = FabricationType.SEAMLESS
    corrosion_allowance: float = 0.0  # m
    code: DesignCode = DesignCode.DNV_ST_F101


class ParametricSweep:
    """Run wall thickness checks across parameter combinations."""

    def __init__(self, config: SweepConfig):
        self.config = config

    def run(self) -> pd.DataFrame:
        """Execute the parametric sweep.

        Returns:
            DataFrame with one row per parameter combination,
            columns for each input parameter and each utilisation check.
        """
        rows: List[Dict] = []
        combos = list(itertools.product(
            self.config.wall_thicknesses,
            self.config.outer_diameters,
            self.config.grades,
            self.config.internal_pressures,
            self.config.external_pressures,
            self.config.bending_moments,
            self.config.effective_tensions,
        ))

        logger.info(f"Running parametric sweep: {len(combos)} combinations")

        for wt, od, grade, pi, pe, m, t in combos:
            # Skip invalid geometries
            if wt >= od / 2 or wt <= 0:
                continue

            grade_props = API_5L_GRADES.get(grade)
            if grade_props is None:
                logger.warning(f"Unknown grade {grade}, skipping")
                continue

            try:
                geom = PipeGeometry(
                    outer_diameter=od,
                    wall_thickness=wt,
                    corrosion_allowance=self.config.corrosion_allowance,
                )
                mat = PipeMaterial(
                    grade=grade,
                    smys=grade_props["smys"],
                    smts=grade_props["smts"],
                    fabrication_type=self.config.fabrication_type,
                )
                loads = DesignLoads(
                    internal_pressure=pi,
                    external_pressure=pe,
                    bending_moment=m,
                    effective_tension=t,
                )
                factors = DesignFactors(safety_class=self.config.safety_class)

                analyzer = WallThicknessAnalyzer(
                    geom, mat, loads, factors, self.config.code
                )
                result = analyzer.perform_analysis()

                row = {
                    "wall_thickness_m": wt,
                    "wall_thickness_mm": wt * 1000,
                    "outer_diameter_m": od,
                    "outer_diameter_mm": od * 1000,
                    "grade": grade,
                    "internal_pressure_MPa": pi / 1e6,
                    "external_pressure_MPa": pe / 1e6,
                    "bending_moment_kNm": m / 1e3,
                    "effective_tension_kN": t / 1e3,
                    "d_over_t": od / wt,
                    "is_safe": result.is_safe,
                    "governing_check": result.governing_check,
                    "max_utilisation": result.max_utilisation,
                }
                for check_name, util_val in result.checks.items():
                    row[f"util_{check_name}"] = util_val

                rows.append(row)

            except (ValueError, ZeroDivisionError) as exc:
                logger.debug(f"Skipping invalid combo: {exc}")
                continue

        df = pd.DataFrame(rows)
        logger.info(f"Sweep complete: {len(df)} valid results")
        return df


# ---------------------------------------------------------------------------
# Plotly visualisation functions
# ---------------------------------------------------------------------------

def plot_utilisation_vs_wall_thickness(
    df: pd.DataFrame,
    title: str = "Utilisation vs Wall Thickness",
) -> go.Figure:
    """Line chart of utilisation ratios vs wall thickness.

    Each design check is a separate line. A horizontal dashed line at 1.0
    marks the acceptance threshold.
    """
    util_cols = [c for c in df.columns if c.startswith("util_")]

    fig = go.Figure()

    for col in util_cols:
        check_name = col.replace("util_", "").replace("_", " ").title()
        fig.add_trace(go.Scatter(
            x=df["wall_thickness_mm"],
            y=df[col],
            mode="lines+markers",
            name=check_name,
        ))

    # Unity line
    fig.add_hline(y=1.0, line_dash="dash", line_color="red",
                  annotation_text="Utilisation = 1.0")

    fig.update_layout(
        title=title,
        xaxis_title="Wall Thickness (mm)",
        yaxis_title="Utilisation Ratio",
        template="plotly_white",
        hovermode="x unified",
    )
    return fig


def plot_utilisation_heatmap(
    df: pd.DataFrame,
    title: str = "Max Utilisation: Wall Thickness vs Outer Diameter",
) -> go.Figure:
    """Heatmap of maximum utilisation across WT and OD combinations."""
    if df.empty:
        return go.Figure()

    pivot = df.pivot_table(
        values="max_utilisation",
        index="outer_diameter_mm",
        columns="wall_thickness_mm",
        aggfunc="min",
    )

    fig = go.Figure(data=go.Heatmap(
        z=pivot.values,
        x=[f"{v:.1f}" for v in pivot.columns],
        y=[f"{v:.1f}" for v in pivot.index],
        colorscale=[
            [0.0, "green"],
            [0.5, "yellow"],
            [1.0, "red"],
        ],
        colorbar=dict(title="Max Utilisation"),
        zmin=0,
        zmax=max(2.0, pivot.values.max()),
    ))

    fig.update_layout(
        title=title,
        xaxis_title="Wall Thickness (mm)",
        yaxis_title="Outer Diameter (mm)",
        template="plotly_white",
    )
    return fig


def plot_governing_check(
    df: pd.DataFrame,
    title: str = "Governing Load Case vs Wall Thickness",
) -> go.Figure:
    """Bar chart showing which check governs at each wall thickness."""
    if df.empty:
        return go.Figure()

    # For each WT, count governing checks or show the governing check
    fig = go.Figure()

    checks = df["governing_check"].unique()
    color_map = {
        "pressure_containment": "#1f77b4",
        "collapse": "#ff7f0e",
        "propagation_buckling": "#2ca02c",
        "combined_loading": "#d62728",
        "burst": "#1f77b4",
        "propagation": "#2ca02c",
    }

    for check in sorted(checks):
        subset = df[df["governing_check"] == check]
        fig.add_trace(go.Bar(
            x=subset["wall_thickness_mm"],
            y=subset["max_utilisation"],
            name=check.replace("_", " ").title(),
            marker_color=color_map.get(check, "#7f7f7f"),
        ))

    fig.add_hline(y=1.0, line_dash="dash", line_color="red",
                  annotation_text="Utilisation = 1.0")

    fig.update_layout(
        title=title,
        xaxis_title="Wall Thickness (mm)",
        yaxis_title="Max Utilisation",
        barmode="group",
        template="plotly_white",
    )
    return fig


def _von_mises_utilisation_grid(
    T_grid: np.ndarray,
    M_grid: np.ndarray,
    A: float,
    Z: float,
    sigma_h: float,
    sigma_allow: float,
) -> np.ndarray:
    """Compute Von Mises utilisation on a 2D (T, M) grid.

    At each point, evaluates both the tension and compression fibres
    and returns the governing (maximum) utilisation.

    sigma_vm = sqrt(sigma_l^2 - sigma_l*sigma_h + sigma_h^2)
    utilisation = sigma_vm / sigma_allow
    """
    sigma_a = T_grid / A       # axial stress
    sigma_b = M_grid / Z       # bending stress (extreme fibre)

    # Tension fibre: sigma_l = sigma_a + sigma_b
    sl_t = sigma_a + sigma_b
    vm_t = np.sqrt(np.maximum(sl_t**2 - sl_t * sigma_h + sigma_h**2, 0.0))

    # Compression fibre: sigma_l = sigma_a - sigma_b
    sl_c = sigma_a - sigma_b
    vm_c = np.sqrt(np.maximum(sl_c**2 - sl_c * sigma_h + sigma_h**2, 0.0))

    # Governing utilisation (max of both fibres)
    util = np.maximum(vm_t, vm_c) / sigma_allow
    return util


def plot_tension_moment_interaction(
    outer_diameter: float,
    wall_thickness: float,
    smys: float,
    smts: float,
    internal_pressure: float = 0.0,
    external_pressure: float = 0.0,
    design_factor: float = 0.72,
    n_points: int = 300,
    title: Optional[str] = None,
) -> go.Figure:
    """API RP 1111 tension-moment interaction diagram.

    Computes the Von Mises equivalent stress utilisation on a 2D grid in
    (T, M) space and draws smooth iso-utilisation contours.  The unity
    contour (util = 1.0) is the acceptance boundary.

    The Von Mises criterion at the pipe wall:
        sigma_vm = sqrt(sigma_l^2 - sigma_l*sigma_h + sigma_h^2) <= f_d * SMYS

    where:
        sigma_l = T/A +/- M/Z  (longitudinal: axial +/- bending)
        sigma_h = p_net * (D - 2t) / (2t)  (hoop stress)

    Both the tension and compression fibres are checked; the governing
    (maximum) utilisation at each grid point is used.

    Args:
        outer_diameter: Pipe OD in metres.
        wall_thickness: Pipe WT in metres.
        smys: Specified Minimum Yield Strength in Pa.
        smts: Specified Minimum Tensile Strength in Pa.
        internal_pressure: Internal pressure in Pa.
        external_pressure: External pressure in Pa.
        design_factor: API RP 1111 design factor (default 0.72).
        n_points: Grid resolution per axis.
        title: Chart title override.

    Returns:
        Plotly Figure with the interaction boundary.
    """
    D = outer_diameter
    t = wall_thickness
    d_i = D - 2 * t

    # Section properties
    A = math.pi / 4 * (D**2 - d_i**2)
    I_val = math.pi / 64 * (D**4 - d_i**4)
    Z = I_val / (D / 2)

    # Hoop stress
    p_net = internal_pressure - external_pressure
    sigma_h = p_net * d_i / (2 * t) if t > 0 else 0.0

    # Allowable stress
    sigma_allow = design_factor * smys

    # Reference capacities
    T_yield = A * smys
    M_p = smys * (D**3 - d_i**3) / 6

    # --- 2D grid ---
    T_max = T_yield * 1.15
    M_max = M_p * 1.15

    T_1d = np.linspace(-T_max, T_max, n_points)
    M_1d = np.linspace(-M_max, M_max, n_points)
    T_grid, M_grid = np.meshgrid(T_1d, M_1d)

    util_grid = _von_mises_utilisation_grid(
        T_grid, M_grid, A, Z, sigma_h, sigma_allow,
    )

    # Convert axes to engineering units
    T_kN = T_1d / 1e3
    M_kNm = M_1d / 1e3

    # --- Build figure ---
    if title is None:
        OD_mm = D * 1000
        WT_mm = t * 1000
        title = (
            f"API RP 1111 Tension–Moment Interaction Diagram<br>"
            f"<sub>OD={OD_mm:.1f} mm, WT={WT_mm:.1f} mm, "
            f"SMYS={smys / 1e6:.0f} MPa, f<sub>d</sub>={design_factor}, "
            f"P<sub>i</sub>={internal_pressure / 1e6:.1f} MPa, "
            f"P<sub>e</sub>={external_pressure / 1e6:.1f} MPa</sub>"
        )

    fig = go.Figure()

    # Filled contour showing utilisation zones
    fig.add_trace(go.Contour(
        x=T_kN,
        y=M_kNm,
        z=util_grid,
        contours=dict(
            start=0.0,
            end=2.0,
            size=0.25,
            showlabels=True,
            labelfont=dict(size=10, color="white"),
        ),
        colorscale=[
            [0.0, "#2166ac"],   # deep blue  – low util
            [0.25, "#67a9cf"],  # light blue
            [0.5, "#fddbc7"],   # pale orange – util = 1.0
            [0.75, "#ef8a62"],  # orange
            [1.0, "#b2182b"],   # deep red – high util
        ],
        colorbar=dict(title="Utilisation", tickformat=".2f"),
        zmin=0.0,
        zmax=2.0,
        hovertemplate=(
            "T = %{x:.0f} kN<br>M = %{y:.0f} kN·m<br>"
            "Util = %{z:.2f}<extra></extra>"
        ),
        name="Von Mises Utilisation",
    ))

    # Bold unity contour
    fig.add_trace(go.Contour(
        x=T_kN,
        y=M_kNm,
        z=util_grid,
        contours=dict(
            start=1.0, end=1.0, size=0,
            coloring="none",
            showlabels=True,
            labelfont=dict(size=12, color="black"),
        ),
        line=dict(color="black", width=3),
        showscale=False,
        hoverinfo="skip",
        name="Util = 1.0",
    ))

    # Reference capacity markers
    fig.add_trace(go.Scatter(
        x=[T_yield / 1e3, -T_yield / 1e3],
        y=[0, 0],
        mode="markers+text",
        marker=dict(color="#D62728", size=10, symbol="diamond"),
        text=[f"T<sub>y</sub> = {T_yield / 1e3:.0f} kN", f"–T<sub>y</sub>"],
        textposition=["top center", "top center"],
        textfont=dict(size=11),
        name="Yield Tension",
    ))

    fig.add_trace(go.Scatter(
        x=[0, 0],
        y=[M_p / 1e3, -M_p / 1e3],
        mode="markers+text",
        marker=dict(color="#2CA02C", size=10, symbol="diamond"),
        text=[f"M<sub>p</sub> = {M_p / 1e3:.0f} kN·m", f"–M<sub>p</sub>"],
        textposition=["middle right", "middle right"],
        textfont=dict(size=11),
        name="Plastic Moment",
    ))

    # Origin
    fig.add_trace(go.Scatter(
        x=[0], y=[0],
        mode="markers",
        marker=dict(color="black", size=6, symbol="x"),
        showlegend=False,
    ))

    fig.update_layout(
        title=title,
        xaxis_title="Effective Tension (kN)",
        yaxis_title="Bending Moment (kN·m)",
        template="plotly_white",
        hovermode="closest",
        showlegend=True,
        legend=dict(
            x=0.01, y=0.99,
            bgcolor="rgba(255,255,255,0.8)",
            bordercolor="#ccc", borderwidth=1,
        ),
        width=900,
        height=750,
    )

    return fig


def generate_report(
    df: pd.DataFrame,
    output_path: Optional[str] = None,
) -> str:
    """Generate an HTML report with summary table and interactive charts.

    Args:
        df: Sweep results DataFrame.
        output_path: If provided, write HTML to this path.

    Returns:
        HTML string.
    """
    fig_util = plot_utilisation_vs_wall_thickness(df)
    fig_heatmap = plot_utilisation_heatmap(df)
    fig_governing = plot_governing_check(df)

    # Summary table
    summary_cols = [
        "wall_thickness_mm", "outer_diameter_mm", "grade",
        "internal_pressure_MPa", "external_pressure_MPa",
        "max_utilisation", "governing_check", "is_safe",
    ]
    available = [c for c in summary_cols if c in df.columns]
    summary_html = df[available].to_html(
        index=False, float_format="{:.3f}".format, classes="table"
    )

    html = f"""<!DOCTYPE html>
<html>
<head>
    <title>Wall Thickness Parametric Analysis Report</title>
    <style>
        body {{ font-family: Arial, sans-serif; margin: 20px; }}
        h1, h2 {{ color: #333; }}
        .table {{ border-collapse: collapse; width: 100%; margin: 20px 0; }}
        .table th, .table td {{ border: 1px solid #ddd; padding: 8px; text-align: right; }}
        .table th {{ background-color: #4472C4; color: white; }}
        .table tr:nth-child(even) {{ background-color: #f9f9f9; }}
    </style>
</head>
<body>
    <h1>Wall Thickness Parametric Analysis</h1>
    <h2>Summary Table</h2>
    {summary_html}
    <h2>Utilisation vs Wall Thickness</h2>
    {fig_util.to_html(full_html=False, include_plotlyjs='cdn')}
    <h2>Utilisation Heatmap</h2>
    {fig_heatmap.to_html(full_html=False, include_plotlyjs=False)}
    <h2>Governing Load Case</h2>
    {fig_governing.to_html(full_html=False, include_plotlyjs=False)}
</body>
</html>"""

    if output_path:
        with open(output_path, "w") as f:
            f.write(html)
        logger.info(f"Report written to {output_path}")

    return html


def export_to_excel(
    df: pd.DataFrame,
    output_path: str,
    sheet_name: str = "Parametric Results",
) -> str:
    """Export parametric sweep results to an Excel workbook.

    Args:
        df: Sweep results DataFrame from ParametricSweep.run().
        output_path: File path for the .xlsx output.
        sheet_name: Worksheet name.

    Returns:
        The output_path written to.
    """
    from pathlib import Path

    Path(output_path).parent.mkdir(parents=True, exist_ok=True)

    with pd.ExcelWriter(output_path, engine="openpyxl") as writer:
        df.to_excel(writer, sheet_name=sheet_name, index=False)

    logger.info(f"Excel export written to {output_path} ({len(df)} rows)")
    return output_path
