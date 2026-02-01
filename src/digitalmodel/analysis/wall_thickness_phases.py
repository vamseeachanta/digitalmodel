# ABOUTME: Pipeline operational phases with multi-code utilisation analysis
# ABOUTME: 5 standard phases, side-by-side code comparison, T-M interaction diagrams

"""Pipeline operational phases — multi-code utilisation analysis.

Composes the existing ``wall_thickness`` analyser and ``wall_thickness_parametric``
plotting utilities. Adds 5 standard operational phases, runs each through multiple
selectable design codes side-by-side, and produces comparative utilisation charts
and T-M interaction diagrams.

No existing files are modified (only imports from them).
"""

from __future__ import annotations

import logging
import math
from dataclasses import dataclass, field
from typing import Dict, List, Optional

import numpy as np
import pandas as pd
import plotly.graph_objects as go
from plotly.subplots import make_subplots

from digitalmodel.analysis.wall_thickness import (
    DesignCode,
    DesignFactors,
    DesignLoads,
    FabricationType,
    PipeGeometry,
    PipeMaterial,
    SafetyClass,
    WallThicknessAnalyzer,
    WallThicknessResult,
)
from digitalmodel.analysis.wall_thickness_parametric import (
    _von_mises_utilisation_grid,
)

logger = logging.getLogger(__name__)

# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

SEAWATER_DENSITY = 1025.0  # kg/m³
GRAVITY = 9.80665  # m/s²


# ---------------------------------------------------------------------------
# Hydrostatic pressure
# ---------------------------------------------------------------------------


def compute_hydrostatic_pressure(
    water_depth: float,
    rho: float = SEAWATER_DENSITY,
    g: float = GRAVITY,
) -> float:
    """Compute hydrostatic pressure at a given water depth.

    Args:
        water_depth: Water depth in metres (must be >= 0).
        rho: Seawater density in kg/m³.
        g: Gravitational acceleration in m/s².

    Returns:
        Hydrostatic pressure in Pascals.
    """
    if water_depth < 0:
        raise ValueError(f"water depth must be >= 0, got {water_depth}")
    return rho * g * water_depth


# ---------------------------------------------------------------------------
# Data models
# ---------------------------------------------------------------------------


@dataclass
class PipeDefinition:
    """Convenience wrapper for pipe OD, WT, grade, SMYS, SMTS.

    Provides conversion methods to PipeGeometry and PipeMaterial.
    """

    outer_diameter: float  # m
    wall_thickness: float  # m
    grade: str  # e.g. "X65"
    smys: float  # Pa
    smts: float  # Pa
    corrosion_allowance: float = 0.0  # m
    fabrication_tolerance: float = 0.125  # fraction
    fabrication_type: FabricationType = FabricationType.SEAMLESS

    def __post_init__(self):
        if self.outer_diameter <= 0:
            raise ValueError(f"outer_diameter must be > 0, got {self.outer_diameter}")
        if self.wall_thickness <= 0:
            raise ValueError(f"wall_thickness must be > 0, got {self.wall_thickness}")
        if self.wall_thickness >= self.outer_diameter / 2:
            raise ValueError(
                f"wall_thickness ({self.wall_thickness}) must be < OD/2 "
                f"({self.outer_diameter / 2})"
            )

    def to_geometry(self) -> PipeGeometry:
        """Convert to PipeGeometry for the analyser."""
        return PipeGeometry(
            outer_diameter=self.outer_diameter,
            wall_thickness=self.wall_thickness,
            corrosion_allowance=self.corrosion_allowance,
            fabrication_tolerance=self.fabrication_tolerance,
        )

    def to_material(self) -> PipeMaterial:
        """Convert to PipeMaterial for the analyser."""
        return PipeMaterial(
            grade=self.grade,
            smys=self.smys,
            smts=self.smts,
            fabrication_type=self.fabrication_type,
        )


@dataclass(frozen=True)
class PipelinePhase:
    """A single operational phase with applied loads.

    Frozen to ensure phases are immutable once created.
    """

    name: str
    internal_pressure: float  # Pa
    external_pressure: float  # Pa
    bending_moment: float  # N·m
    effective_tension: float  # N
    description: str = ""

    def to_design_loads(self) -> DesignLoads:
        """Convert phase loads to DesignLoads for the analyser."""
        return DesignLoads(
            internal_pressure=self.internal_pressure,
            external_pressure=self.external_pressure,
            bending_moment=self.bending_moment,
            effective_tension=self.effective_tension,
        )


# ---------------------------------------------------------------------------
# Phase factory
# ---------------------------------------------------------------------------


def create_standard_phases(
    water_depth: float,
    design_pressure: float,
    install_bending_moment: float = 100e3,
    install_effective_tension: float = 50e3,
    oper_bending_moment: float = 50e3,
    oper_effective_tension: float = 30e3,
) -> List[PipelinePhase]:
    """Create the 5 standard operational phases.

    Args:
        water_depth: Water depth in metres.
        design_pressure: MAOP / design internal pressure in Pa.
        install_bending_moment: Installation bending moment in N·m.
        install_effective_tension: Installation effective tension in N.
        oper_bending_moment: Operational bending moment in N·m.
        oper_effective_tension: Operational effective tension in N.

    Returns:
        List of 5 PipelinePhase objects.
    """
    pe = compute_hydrostatic_pressure(water_depth)

    return [
        PipelinePhase(
            name="Installation (empty)",
            internal_pressure=0.0,
            external_pressure=pe,
            bending_moment=install_bending_moment,
            effective_tension=install_effective_tension,
            description="Empty pipe during installation — collapse + combined governs",
        ),
        PipelinePhase(
            name="Installation (water-filled)",
            internal_pressure=pe,
            external_pressure=pe,
            bending_moment=install_bending_moment,
            effective_tension=install_effective_tension,
            description="Water-filled pipe during installation — combined loading",
        ),
        PipelinePhase(
            name="Hydrotest",
            internal_pressure=1.25 * design_pressure,
            external_pressure=pe,
            bending_moment=0.0,
            effective_tension=0.0,
            description="Hydrotest at 1.25× MAOP — burst governs",
        ),
        PipelinePhase(
            name="Operation",
            internal_pressure=design_pressure,
            external_pressure=pe,
            bending_moment=oper_bending_moment,
            effective_tension=oper_effective_tension,
            description="Normal operation at MAOP — burst + combined",
        ),
        PipelinePhase(
            name="Shutdown",
            internal_pressure=0.0,
            external_pressure=pe,
            bending_moment=0.0,
            effective_tension=0.0,
            description="Depressurised shutdown — collapse governs",
        ),
    ]


# ---------------------------------------------------------------------------
# Result data models
# ---------------------------------------------------------------------------


@dataclass
class PhaseResult:
    """Result for a single (phase, code) combination."""

    phase_name: str
    code: DesignCode
    wt_result: WallThicknessResult
    loads: DesignLoads


@dataclass
class PhaseComparisonResult:
    """Aggregated results across all phases and codes."""

    pipe: PipeDefinition
    phases: List[PipelinePhase]
    codes: List[DesignCode]
    results: List[PhaseResult] = field(default_factory=list)

    def to_dataframe(self) -> pd.DataFrame:
        """Flat DataFrame: one row per (phase, code, check) → utilisation."""
        rows = []
        for r in self.results:
            for check_name, util_value in r.wt_result.checks.items():
                rows.append(
                    {
                        "phase": r.phase_name,
                        "code": r.code.value,
                        "check": check_name,
                        "utilisation": util_value,
                    }
                )
        return pd.DataFrame(rows)

    def summary_dataframe(self) -> pd.DataFrame:
        """One row per (phase, code) with max utilisation and governing check."""
        rows = []
        for r in self.results:
            rows.append(
                {
                    "phase": r.phase_name,
                    "code": r.code.value,
                    "max_utilisation": r.wt_result.max_utilisation,
                    "governing_check": r.wt_result.governing_check,
                    "is_safe": r.wt_result.is_safe,
                }
            )
        return pd.DataFrame(rows)


# ---------------------------------------------------------------------------
# Multi-code runner
# ---------------------------------------------------------------------------


class PhaseAnalysisRunner:
    """Run wall thickness checks across phases × codes."""

    def __init__(
        self,
        pipe: PipeDefinition,
        phases: List[PipelinePhase],
        codes: List[DesignCode],
        safety_class: SafetyClass = SafetyClass.MEDIUM,
    ):
        self.pipe = pipe
        self.phases = phases
        self.codes = codes
        self.safety_class = safety_class

    def run(self) -> PhaseComparisonResult:
        """Execute all (phase × code) combinations.

        Returns:
            PhaseComparisonResult with one PhaseResult per combination.
        """
        geometry = self.pipe.to_geometry()
        material = self.pipe.to_material()
        factors = DesignFactors(safety_class=self.safety_class)

        results: List[PhaseResult] = []

        for phase in self.phases:
            loads = phase.to_design_loads()
            for code in self.codes:
                analyzer = WallThicknessAnalyzer(
                    geometry=geometry,
                    material=material,
                    loads=loads,
                    factors=factors,
                    code=code,
                )
                wt_result = analyzer.perform_analysis()
                results.append(
                    PhaseResult(
                        phase_name=phase.name,
                        code=code,
                        wt_result=wt_result,
                        loads=loads,
                    )
                )
                logger.info(
                    "Phase=%s Code=%s util=%.3f governing=%s",
                    phase.name,
                    code.value,
                    wt_result.max_utilisation,
                    wt_result.governing_check,
                )

        return PhaseComparisonResult(
            pipe=self.pipe,
            phases=self.phases,
            codes=self.codes,
            results=results,
        )


# ---------------------------------------------------------------------------
# Visualisation helpers
# ---------------------------------------------------------------------------

# Color palette for check types
_CHECK_COLORS = {
    "pressure_containment": "#1f77b4",
    "collapse": "#ff7f0e",
    "propagation_buckling": "#2ca02c",
    "combined_loading": "#d62728",
    "burst": "#1f77b4",
    "propagation": "#2ca02c",
}


def _dnv_combined_utilisation_grid(
    T_grid: np.ndarray,
    M_grid: np.ndarray,
    pipe: PipeDefinition,
    internal_pressure: float,
    external_pressure: float,
    safety_class: SafetyClass = SafetyClass.MEDIUM,
) -> np.ndarray:
    """Compute DNV-ST-F101 combined loading utilisation on a 2D (T, M) grid.

    Uses the simplified interaction equation from D700:
        u = sqrt((γ·M/M_p)² + (γ·S/S_p)² + pressure_ratio²)
    """
    D = pipe.outer_diameter
    t2 = pipe.wall_thickness - pipe.corrosion_allowance
    smys = pipe.smys
    alpha_fab = PipeMaterial(
        grade=pipe.grade,
        smys=pipe.smys,
        smts=pipe.smts,
        fabrication_type=pipe.fabrication_type,
    ).alpha_fab

    factors = DesignFactors(safety_class=safety_class)
    gamma_sc = factors.gamma_sc
    gamma_m = factors.gamma_m

    # Plastic capacities
    M_p = smys * alpha_fab * (D - t2) ** 2 * t2
    S_p = smys * alpha_fab * math.pi * (D - t2) * t2

    # Collapse pressure (need a single-point analyzer for this)
    geometry = pipe.to_geometry()
    material = pipe.to_material()
    loads = DesignLoads(
        internal_pressure=internal_pressure,
        external_pressure=external_pressure,
    )
    analyzer = WallThicknessAnalyzer(
        geometry=geometry,
        material=material,
        loads=loads,
        factors=factors,
        code=DesignCode.DNV_ST_F101,
    )
    _, collapse_details = analyzer.check_collapse()
    p_c = collapse_details["p_c"]

    # Pressure ratio
    if p_c > 0:
        pressure_ratio = (internal_pressure - external_pressure) / p_c
    else:
        pressure_ratio = 0.0

    # Interaction on grid
    if M_p > 0 and S_p > 0:
        moment_term = (gamma_sc * gamma_m * np.abs(M_grid) / M_p) ** 2
        tension_term = (gamma_sc * gamma_m * np.abs(T_grid) / S_p) ** 2
        util_grid = np.sqrt(moment_term + tension_term + pressure_ratio**2)
    else:
        util_grid = np.full_like(T_grid, float("inf"))

    return util_grid


# ---------------------------------------------------------------------------
# Main visualisation functions
# ---------------------------------------------------------------------------


def plot_phase_utilisation_comparison(
    comparison: PhaseComparisonResult,
    title: Optional[str] = None,
) -> go.Figure:
    """Grouped bar chart comparing utilisation across phases and codes.

    Subplots: 1 row × N codes (columns). X-axis: phase names. Bars grouped
    by check type. Red dashed line at utilisation = 1.0.
    """
    codes = comparison.codes
    n_codes = len(codes)
    df = comparison.to_dataframe()

    subplot_titles = [c.value for c in codes]
    fig = make_subplots(
        rows=1,
        cols=n_codes,
        subplot_titles=subplot_titles,
        shared_yaxes=True,
        horizontal_spacing=0.06,
    )

    phase_names = [p.name for p in comparison.phases]

    for col_idx, code in enumerate(codes, start=1):
        code_df = df[df["code"] == code.value]
        checks = sorted(code_df["check"].unique())

        for check in checks:
            check_df = code_df[code_df["check"] == check]
            # Ensure phase ordering matches
            check_df = check_df.set_index("phase").reindex(phase_names).reset_index()
            color = _CHECK_COLORS.get(check, "#999999")
            fig.add_trace(
                go.Bar(
                    x=check_df["phase"],
                    y=check_df["utilisation"],
                    name=check,
                    marker_color=color,
                    showlegend=(col_idx == 1),
                    legendgroup=check,
                ),
                row=1,
                col=col_idx,
            )

        # Unity line
        fig.add_hline(
            y=1.0,
            line_dash="dash",
            line_color="red",
            line_width=1.5,
            row=1,
            col=col_idx,
        )

    fig.update_layout(
        title=title or "Phase Utilisation Comparison",
        barmode="group",
        template="plotly_white",
        height=500,
        width=450 * n_codes,
        yaxis_title="Utilisation",
    )

    return fig


def plot_phase_tm_interaction(
    pipe: PipeDefinition,
    phases: List[PipelinePhase],
    codes: List[DesignCode],
    safety_class: SafetyClass = SafetyClass.MEDIUM,
    n_points: int = 100,
    title: Optional[str] = None,
) -> go.Figure:
    """T-M interaction diagram grid: rows = phases, columns = codes.

    Each cell shows filled contour of utilisation with bold unity boundary
    and a red star at the operating point.
    """
    n_phases = len(phases)
    n_codes = len(codes)

    D = pipe.outer_diameter
    t = pipe.wall_thickness
    d_i = D - 2 * t
    smys = pipe.smys

    # Section properties for grid bounds
    A = math.pi / 4 * (D**2 - d_i**2)
    M_p_ref = smys * (D**3 - d_i**3) / 6
    T_yield = A * smys

    T_max = T_yield * 1.15
    M_max = M_p_ref * 1.15

    T_1d = np.linspace(-T_max, T_max, n_points)
    M_1d = np.linspace(-M_max, M_max, n_points)
    T_grid, M_grid = np.meshgrid(T_1d, M_1d)

    T_kN = T_1d / 1e3
    M_kNm = M_1d / 1e3

    subplot_titles = []
    for phase in phases:
        for code in codes:
            subplot_titles.append(f"{phase.name}<br>{code.value}")

    fig = make_subplots(
        rows=n_phases,
        cols=n_codes,
        subplot_titles=subplot_titles,
        horizontal_spacing=0.05,
        vertical_spacing=0.06,
    )

    for row_idx, phase in enumerate(phases, start=1):
        for col_idx, code in enumerate(codes, start=1):
            # Compute utilisation grid
            if code == DesignCode.API_RP_1111:
                # Von Mises approach for API
                p_net = phase.internal_pressure - phase.external_pressure
                sigma_h = p_net * d_i / (2 * t) if t > 0 else 0.0
                I_val = math.pi / 64 * (D**4 - d_i**4)
                Z = I_val / (D / 2)
                sigma_allow = 0.72 * smys
                util_grid = _von_mises_utilisation_grid(
                    T_grid, M_grid, A, Z, sigma_h, sigma_allow
                )
            else:
                # DNV combined loading interaction
                util_grid = _dnv_combined_utilisation_grid(
                    T_grid,
                    M_grid,
                    pipe,
                    phase.internal_pressure,
                    phase.external_pressure,
                    safety_class,
                )

            # Filled contour
            fig.add_trace(
                go.Contour(
                    x=T_kN,
                    y=M_kNm,
                    z=util_grid,
                    contours=dict(
                        start=0.0,
                        end=2.0,
                        size=0.25,
                        showlabels=False,
                    ),
                    colorscale=[
                        [0.0, "#2166ac"],
                        [0.25, "#67a9cf"],
                        [0.5, "#fddbc7"],
                        [0.75, "#ef8a62"],
                        [1.0, "#b2182b"],
                    ],
                    zmin=0.0,
                    zmax=2.0,
                    showscale=(row_idx == 1 and col_idx == n_codes),
                    colorbar=dict(title="Util") if (row_idx == 1 and col_idx == n_codes) else None,
                    hovertemplate=(
                        "T=%{x:.0f}kN M=%{y:.0f}kN·m util=%{z:.2f}<extra></extra>"
                    ),
                    showlegend=False,
                ),
                row=row_idx,
                col=col_idx,
            )

            # Unity boundary
            fig.add_trace(
                go.Contour(
                    x=T_kN,
                    y=M_kNm,
                    z=util_grid,
                    contours=dict(
                        start=1.0,
                        end=1.0,
                        size=0,
                        coloring="none",
                    ),
                    line=dict(color="black", width=2),
                    showscale=False,
                    hoverinfo="skip",
                    showlegend=False,
                ),
                row=row_idx,
                col=col_idx,
            )

            # Operating point marker (red star)
            fig.add_trace(
                go.Scatter(
                    x=[phase.effective_tension / 1e3],
                    y=[phase.bending_moment / 1e3],
                    mode="markers",
                    marker=dict(
                        color="red",
                        size=10,
                        symbol="star",
                        line=dict(color="black", width=1),
                    ),
                    showlegend=False,
                    hovertemplate=(
                        f"Operating Point<br>"
                        f"T={phase.effective_tension / 1e3:.0f}kN "
                        f"M={phase.bending_moment / 1e3:.0f}kN·m"
                        f"<extra></extra>"
                    ),
                ),
                row=row_idx,
                col=col_idx,
            )

    fig.update_layout(
        title=title or "T-M Interaction Diagrams by Phase and Code",
        template="plotly_white",
        height=300 * n_phases,
        width=450 * n_codes,
    )

    # Label axes on edge subplots
    for col_idx in range(1, n_codes + 1):
        fig.update_xaxes(
            title_text="Tension (kN)", row=n_phases, col=col_idx
        )
    for row_idx in range(1, n_phases + 1):
        fig.update_yaxes(
            title_text="Moment (kN·m)", row=row_idx, col=1
        )

    return fig


def generate_phase_summary_table(
    comparison: PhaseComparisonResult,
) -> pd.DataFrame:
    """Generate a summary DataFrame with one row per (phase, code).

    Delegates to PhaseComparisonResult.summary_dataframe().
    """
    return comparison.summary_dataframe()


def generate_phase_report(
    comparison: PhaseComparisonResult,
    output_path: Optional[str] = None,
    title: Optional[str] = None,
) -> str:
    """Generate an HTML report with summary table, bar chart, and T-M grid.

    Args:
        comparison: PhaseComparisonResult from the runner.
        output_path: Optional file path to write the HTML.
        title: Optional report title.

    Returns:
        HTML string.
    """
    report_title = title or "Pipeline Phase Utilisation Report"
    summary_df = comparison.summary_dataframe()

    # Build charts
    bar_fig = plot_phase_utilisation_comparison(comparison)
    tm_fig = plot_phase_tm_interaction(
        comparison.pipe,
        comparison.phases,
        comparison.codes,
        n_points=80,
    )

    # Convert to HTML divs
    bar_html = bar_fig.to_html(full_html=False, include_plotlyjs="cdn")
    tm_html = tm_fig.to_html(full_html=False, include_plotlyjs=False)

    # Summary table HTML
    table_html = summary_df.to_html(
        index=False,
        classes="summary-table",
        float_format=lambda x: f"{x:.3f}" if isinstance(x, float) else str(x),
    )

    # Pipe info
    pipe = comparison.pipe
    pipe_info = (
        f"OD = {pipe.outer_diameter * 1000:.1f} mm, "
        f"WT = {pipe.wall_thickness * 1000:.1f} mm, "
        f"Grade = {pipe.grade}, "
        f"SMYS = {pipe.smys / 1e6:.0f} MPa, "
        f"SMTS = {pipe.smts / 1e6:.0f} MPa"
    )

    html = f"""<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <title>{report_title}</title>
    <style>
        body {{ font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
               margin: 2em; color: #333; }}
        h1 {{ color: #1a1a2e; border-bottom: 2px solid #16213e; padding-bottom: 0.5em; }}
        h2 {{ color: #16213e; margin-top: 2em; }}
        .pipe-info {{ background: #f0f4f8; padding: 1em; border-radius: 6px;
                      margin: 1em 0; font-size: 0.95em; }}
        .summary-table {{ border-collapse: collapse; width: 100%; margin: 1em 0; }}
        .summary-table th, .summary-table td {{
            border: 1px solid #ddd; padding: 8px 12px; text-align: left; }}
        .summary-table th {{ background: #16213e; color: white; }}
        .summary-table tr:nth-child(even) {{ background: #f9f9f9; }}
        .chart-section {{ margin: 2em 0; }}
    </style>
</head>
<body>
    <h1>{report_title}</h1>
    <div class="pipe-info"><strong>Pipe:</strong> {pipe_info}</div>

    <h2>Summary Table</h2>
    {table_html}

    <h2>Phase Utilisation Comparison</h2>
    <div class="chart-section">
        {bar_html}
    </div>

    <h2>T-M Interaction Diagrams</h2>
    <div class="chart-section">
        {tm_html}
    </div>
</body>
</html>"""

    if output_path:
        with open(output_path, "w", encoding="utf-8") as f:
            f.write(html)
        logger.info("Report written to %s", output_path)

    return html
