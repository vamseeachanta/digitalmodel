# ABOUTME: Fatigue analysis worked examples — WRK-157 Phase 4
# ABOUTME: Three canonical offshore cases: pipeline girth weld, SCR touchdown, mooring chain

"""
Fatigue Analysis Worked Examples
=================================

Three canonical offshore structural fatigue cases demonstrating the full pipeline:

    stress histogram → S-N curve → Palmgren-Miner damage → fatigue life estimate → HTML report

Examples
--------
1. **Pipeline girth weld** — DNV-RP-C203 D-curve, seawater+CP, SCF=1.5, DFF=3
2. **SCR touchdown zone** — DNV-RP-C203 F1-curve, seawater free-corrosion, SCF=1.0, DFF=10
3. **Mooring chain link** — DNV-OS-E301 studless chain curve, seawater+CP, SCF=1.0, DFF=5

All stress histograms are synthetic (no client data).  Curve parameters are from
published standards; comments cite the specific table where applicable.

Quick start::

    from digitalmodel.structural.fatigue.worked_examples import (
        pipeline_girth_weld, scr_touchdown, mooring_chain,
        generate_example_report,
    )

    result = pipeline_girth_weld()
    html = generate_example_report(result)
"""

import json
import logging
import math
from dataclasses import dataclass, field
from pathlib import Path
from typing import Dict, List, Optional

import numpy as np
import pandas as pd

from digitalmodel.structural.fatigue.sn_curves import (
    PowerLawSNCurve,
    StandardSNCurves,
)
from digitalmodel.structural.fatigue.damage_accumulation import LinearDamageAccumulation

logger = logging.getLogger(__name__)

# ---------------------------------------------------------------------------
# Environment-adjusted S-N curves
# ---------------------------------------------------------------------------
# DNV-RP-C203 (2016) Table 2-1 (in air) and Table 2-2 (seawater+CP).
# Seawater free-corrosion: same m, no fatigue limit (curve continues).
# Reference thickness: 25 mm.

def _dnv_seawater_cp(curve_class: str) -> PowerLawSNCurve:
    """
    DNV-RP-C203 seawater + cathodic protection curve.

    Uses reduced A coefficient from DNV-RP-C203 Table 2-2.  The fatigue limit
    is maintained (CP suppresses corrosion below CAFL).

    Parameters
    ----------
    curve_class : str
        DNV curve class (e.g. 'D', 'F1').

    Returns
    -------
    PowerLawSNCurve
    """
    # Seawater+CP factors vs air from DNV-RP-C203 Table 2-2.
    # The correction reduces log(A) by ~0.06 log-units (factor 0.87 on cycles).
    SW_CP_FACTORS: Dict[str, float] = {
        'B1': 0.87, 'B2': 0.87,
        'C': 0.87, 'C1': 0.87, 'C2': 0.87,
        'D': 0.87, 'E': 0.87, 'F': 0.87, 'F1': 0.87, 'F3': 0.87,
        'G': 0.87, 'W1': 0.87, 'W2': 0.87, 'W3': 0.87,
    }
    base = StandardSNCurves.get_curve('DNV', curve_class)
    factor = SW_CP_FACTORS.get(curve_class, 0.87)
    return PowerLawSNCurve(
        name=f"DNV-{curve_class}-SwCP",
        A=base.A * factor,
        m=base.m,
        fatigue_limit=base.fatigue_limit,  # CAFL retained under CP
        cutoff_cycles=base.cutoff_cycles,
        material=base.material,
    )


def _dnv_seawater_free(curve_class: str) -> PowerLawSNCurve:
    """
    DNV-RP-C203 seawater free-corrosion curve.

    No cathodic protection: fatigue limit removed and A reduced further.
    DNV-RP-C203 Cl. 2.4.3: use air curve parameters but remove fatigue limit;
    A factor ≈ 0.72 vs air.

    Parameters
    ----------
    curve_class : str
        DNV curve class.

    Returns
    -------
    PowerLawSNCurve
    """
    base = StandardSNCurves.get_curve('DNV', curve_class)
    return PowerLawSNCurve(
        name=f"DNV-{curve_class}-SwFree",
        A=base.A * 0.72,
        m=base.m,
        fatigue_limit=0.0,       # No CAFL in free-corrosion environment
        cutoff_cycles=1e20,      # No cycle cutoff
        material=base.material,
    )


def _chain_seawater_cp() -> PowerLawSNCurve:
    """
    Mooring chain S-N curve — DNVGL-OS-E301 (2021) Table 6-2.

    Grade R3 studless chain, seawater + cathodic protection:
        log(N) = log(K) − m × log(Δσ)
        m = 3.0,  log(K) = 15.19   (N in cycles, Δσ in MPa)
        A = 10^15.19 ≈ 1.55 × 10^15 MPa^3·cycles

    The chain link cross-section area (for stress calculation from tension
    range ΔT) is: A_chain = π/4 × d² × 2  (two parallel bars)
    where d is the nominal wire diameter.  This example uses Δσ directly.
    """
    return PowerLawSNCurve(
        name="Chain-R3-Studless-SwCP",
        A=1.55e15,      # DNVGL-OS-E301 Table 6-2 (log K = 15.19)
        m=3.0,
        fatigue_limit=0.0,   # No fatigue limit for chain in seawater (Cl. 6.3.3)
        cutoff_cycles=1e20,
    )


# ---------------------------------------------------------------------------
# Synthetic stress histograms
# ---------------------------------------------------------------------------

def _weibull_histogram(
    design_life_years: float,
    cycles_per_year: float,
    stress_scale: float,
    shape: float,
    n_bins: int = 10,
) -> pd.DataFrame:
    """
    Generate a synthetic Weibull-distributed stress histogram.

    Represents the long-term stress range distribution common in wave-dominated
    offshore environments.  The Weibull CDF gives the probability that a cycle
    exceeds stress range s:  P(S > s) = exp[-(s/A)^h].

    Parameters
    ----------
    design_life_years : float
        Total design life for cycle count scaling.
    cycles_per_year : float
        Average wave cycles per year (typically ~3-6 × 10^6 for North Sea).
    stress_scale : float
        Weibull scale parameter (characteristic largest stress range) in MPa.
    shape : float
        Weibull shape parameter h (typical offshore: 0.9–1.1).
    n_bins : int
        Number of histogram bins.

    Returns
    -------
    pd.DataFrame
        Columns: ``range`` (MPa), ``count`` (cycles over design life).
    """
    total_cycles = design_life_years * cycles_per_year

    # Bin edges from 0 to 1.5× scale
    edges = np.linspace(0.0, 1.5 * stress_scale, n_bins + 1)
    mids = 0.5 * (edges[:-1] + edges[1:])

    # Weibull exceedance probability at each bin edge
    p_exceed = np.exp(-(edges / stress_scale) ** shape)
    counts = np.diff(-p_exceed) * total_cycles  # cycles in each bin

    # Keep only positive counts and ranges
    mask = (counts > 0) & (mids > 0)
    return pd.DataFrame({"range": mids[mask], "count": counts[mask]})


# ---------------------------------------------------------------------------
# Result dataclass
# ---------------------------------------------------------------------------

@dataclass
class ExampleResult:
    """Result from a fatigue worked example."""
    name: str
    description: str
    histogram: pd.DataFrame
    sn_curve: PowerLawSNCurve
    scf: float
    dff: float
    design_life_years: float
    damage: float
    life_years: float
    passes: bool
    damage_contributions: List[dict] = field(default_factory=list)
    assumptions: List[str] = field(default_factory=list)
    references: List[str] = field(default_factory=list)


# ---------------------------------------------------------------------------
# Worked examples
# ---------------------------------------------------------------------------

def pipeline_girth_weld(
    design_life_years: float = 20.0,
    scf: float = 1.5,
    dff: float = 3.0,
) -> ExampleResult:
    """
    Pipeline girth weld fatigue — DNV-RP-C203 D-curve, seawater + CP.

    Represents a subsea pipeline girth weld in a North Sea environment.
    The stress histogram is synthetic and representative of a wave-dominated
    pipe with SCF=1.5 at a girth weld in class D category.

    Parameters
    ----------
    design_life_years : float, default 20
        Design service life in years.
    scf : float, default 1.5
        Stress concentration factor at the girth weld.
    dff : float, default 3
        Design fatigue factor per DNV-ST-F101 Cl. 6.7 (seabed accessible).

    Returns
    -------
    ExampleResult
    """
    # Synthetic North Sea pipeline histogram (Weibull h=0.9, scale=80 MPa)
    # Total ~3.15 × 10^7 wave cycles over 20 years (0.1 Hz average period)
    histogram = _weibull_histogram(
        design_life_years=design_life_years,
        cycles_per_year=3_150_000,
        stress_scale=80.0,
        shape=0.9,
        n_bins=12,
    )

    curve = _dnv_seawater_cp('D')
    return _compute_example(
        name="Pipeline Girth Weld",
        description=(
            "Subsea pipeline girth weld, North Sea environment. "
            "DNV-RP-C203 D-curve, seawater + cathodic protection, SCF=1.5 "
            "per DNVGL-ST-F101 Fig. 12-1 (typical butt weld, external clad)."
        ),
        histogram=histogram,
        sn_curve=curve,
        scf=scf,
        dff=dff,
        design_life_years=design_life_years,
        assumptions=[
            "Weibull stress distribution: h=0.9, scale=80 MPa",
            "Average wave period 10 s → 3.15 × 10⁶ cycles/year",
            "Seawater + cathodic protection (CP maintained throughout life)",
            "Reference thickness t_ref = 25 mm (no thickness correction applied)",
            "Palmgren-Miner linear damage accumulation",
        ],
        references=[
            "DNV-RP-C203 (2016) Table 2-2 — seawater+CP S-N curves",
            "DNVGL-ST-F101 (2017) Cl. 6.7 — DFF for seabed pipelines",
            "ISO 13628-7 — girth weld SCF guidance",
        ],
    )


def scr_touchdown(
    design_life_years: float = 20.0,
    scf: float = 1.0,
    dff: float = 10.0,
) -> ExampleResult:
    """
    SCR touchdown zone fatigue — DNV-RP-C203 F1-curve, seawater free-corrosion.

    Steel Catenary Riser (SCR) at the seabed touchdown point.  The SCR
    experiences combined wave- and current-induced oscillation.  Free-corrosion
    environment assumed (no CP at this location on the outer surface).

    Parameters
    ----------
    design_life_years : float, default 20
        Design service life in years.
    scf : float, default 1.0
        SCF applied (weld detail already captured by F1 classification).
    dff : float, default 10
        Design fatigue factor per DNVGL-ST-F201 Cl. 5.4.4 (safety class High).

    Returns
    -------
    ExampleResult
    """
    # SCR touchdown histogram: higher proportion of small-amplitude current cycles,
    # fewer large wave-driven cycles. Weibull h=1.1 (slightly heavier tail).
    histogram = _weibull_histogram(
        design_life_years=design_life_years,
        cycles_per_year=5_000_000,   # current + wave combined
        stress_scale=60.0,
        shape=1.1,
        n_bins=12,
    )

    curve = _dnv_seawater_free('F1')
    return _compute_example(
        name="SCR Touchdown Zone",
        description=(
            "Steel Catenary Riser (SCR) at the seabed touchdown point. "
            "DNV-RP-C203 F1-curve, seawater free-corrosion (no CP on outer surface). "
            "Combined wave + current loading, DFF=10 per DNVGL-ST-F201 Safety Class High."
        ),
        histogram=histogram,
        sn_curve=curve,
        scf=scf,
        dff=dff,
        design_life_years=design_life_years,
        assumptions=[
            "Weibull stress distribution: h=1.1, scale=60 MPa",
            "5 × 10⁶ cycles/year (wave + current combined)",
            "Seawater free-corrosion — no cathodic protection at TDP",
            "Fatigue limit removed per DNV-RP-C203 Cl. 2.4.3",
            "F1 class captures the groove detail at touchdown",
        ],
        references=[
            "DNV-RP-C203 (2016) Cl. 2.4.3 — seawater free-corrosion guidance",
            "DNVGL-ST-F201 (2018) Cl. 5.4.4 — SCR DFF requirements",
            "OTC 20228 — SCR touchdown fatigue analysis methodology",
        ],
    )


def mooring_chain(
    design_life_years: float = 25.0,
    scf: float = 1.0,
    dff: float = 5.0,
) -> ExampleResult:
    """
    Mooring chain link fatigue — DNVGL-OS-E301 studless chain R3, seawater+CP.

    FPSO mooring chain on a spread-mooring system.  Stress ranges are expressed
    as nominal link stress (tension range / net cross-section area × 2 bars).
    Chain diameter 105 mm (net cross-section area ≈ 17 340 mm²).

    Parameters
    ----------
    design_life_years : float, default 25
        Design service life in years.
    scf : float, default 1.0
        SCF = 1.0 for chain (inherent stress concentration captured by curve).
    dff : float, default 5
        Design fatigue factor per API RP 2SK / DNVGL-OS-E301 (permanent system).

    Returns
    -------
    ExampleResult
    """
    # FPSO spread mooring: long-period loading (wave + surge), h=0.8
    # Stress ranges from typical 5000 te MBL chain (105 mm diameter)
    histogram = _weibull_histogram(
        design_life_years=design_life_years,
        cycles_per_year=600_000,   # Slow-drift + wave: ~0.02 Hz effective
        stress_scale=120.0,
        shape=0.8,
        n_bins=10,
    )

    curve = _chain_seawater_cp()
    return _compute_example(
        name="Mooring Chain Link",
        description=(
            "FPSO spread mooring studless chain R3, 105 mm diameter. "
            "DNVGL-OS-E301 chain S-N curve, seawater + cathodic protection. "
            "Nominal link stress = tension range / (2 × π/4 × d²). DFF=5 per "
            "API RP 2SK Table 5.5-1 (permanent system, unmanned inspection)."
        ),
        histogram=histogram,
        sn_curve=curve,
        scf=scf,
        dff=dff,
        design_life_years=design_life_years,
        assumptions=[
            "Weibull stress distribution: h=0.8, scale=120 MPa",
            "6 × 10⁵ cycles/year (combined wave + slow-drift surge)",
            "Chain diameter 105 mm; net cross-section area = 2 × π/4 × 105² ≈ 17 340 mm²",
            "Grade R3 studless chain in seawater + cathodic protection",
            "DNVGL-OS-E301 (2021) Table 6-2: log(K)=15.19, m=3.0",
            "No fatigue limit per Cl. 6.3.3",
        ],
        references=[
            "DNVGL-OS-E301 (2021) Table 6-2 — chain S-N curves",
            "API RP 2SK (2005) Table 5.5-1 — mooring DFF requirements",
            "DNVGL-RP-E301 (2020) — cathodic protection for mooring",
        ],
    )


# ---------------------------------------------------------------------------
# Core computation
# ---------------------------------------------------------------------------

def _compute_example(
    name: str,
    description: str,
    histogram: pd.DataFrame,
    sn_curve: PowerLawSNCurve,
    scf: float,
    dff: float,
    design_life_years: float,
    assumptions: List[str],
    references: List[str],
) -> ExampleResult:
    """Run damage accumulation and build ExampleResult."""
    eff = histogram.copy()
    eff["range"] = histogram["range"] * scf

    accumulator = LinearDamageAccumulation()
    raw = accumulator.calculate_damage(eff, sn_curve)
    damage = raw["total_damage"]

    life_years = design_life_years / damage if damage > 0 else math.inf
    passes = (damage * dff) <= 1.0

    return ExampleResult(
        name=name,
        description=description,
        histogram=histogram,
        sn_curve=sn_curve,
        scf=scf,
        dff=dff,
        design_life_years=design_life_years,
        damage=damage,
        life_years=life_years,
        passes=passes,
        damage_contributions=raw.get("damage_contributions", []),
        assumptions=assumptions,
        references=references,
    )


# ---------------------------------------------------------------------------
# HTML report
# ---------------------------------------------------------------------------

def generate_example_report(
    result: ExampleResult,
    output_path: Optional[str] = None,
) -> str:
    """
    Generate a self-contained interactive HTML fatigue report.

    The report includes:

    - Summary card: damage, DFF-adjusted damage, estimated life, pass/fail
    - Interactive S-N curve (Plotly) with operating stress range overlay
    - Stress histogram bar chart (Plotly)
    - Damage contribution table (top contributors by damage)
    - Assumptions and references

    Parameters
    ----------
    result : ExampleResult
        Output from one of the worked example functions.
    output_path : str or Path, optional
        If provided, also write the HTML to this file.

    Returns
    -------
    str
        Self-contained HTML string.
    """
    sn_json = _sn_chart_json(result)
    hist_json = _histogram_chart_json(result)
    summary = _summary_html(result)
    damage_table = _damage_table_html(result)
    assumptions_html = _list_html("Assumptions", result.assumptions)
    references_html = _list_html("References", result.references)

    html = _EXAMPLE_TEMPLATE.format(
        title=result.name,
        description=result.description,
        summary=summary,
        sn_json=sn_json,
        hist_json=hist_json,
        damage_table=damage_table,
        assumptions=assumptions_html,
        references=references_html,
    )

    if output_path:
        Path(output_path).write_text(html, encoding="utf-8")

    return html


# ---------------------------------------------------------------------------
# Private helpers
# ---------------------------------------------------------------------------

def _sn_chart_json(result: ExampleResult) -> str:
    """S-N curve with operating stress range markers."""
    curve = result.sn_curve
    eff_ranges = result.histogram["range"] * result.scf

    # S-N curve line
    n_arr = np.logspace(3, 9, 200)
    s_arr = curve.get_stress_range(n_arr)
    valid = s_arr > 0
    n_arr, s_arr = n_arr[valid], s_arr[valid]

    sn_trace = {
        "type": "scatter", "mode": "lines",
        "name": result.sn_curve.name,
        "x": n_arr.tolist(), "y": s_arr.tolist(),
        "line": {"color": "#2c3e50", "width": 2},
    }

    # Operating stress range markers (use cycle counts as marker size proxy)
    counts = result.histogram["count"].values
    max_count = counts.max() if counts.max() > 0 else 1.0
    marker_sizes = 6 + 18 * (counts / max_count)

    # Map each stress range to its allowable cycles
    n_allowable = [curve.get_allowable_cycles(s) for s in eff_ranges]
    n_allowable = [n if np.isfinite(n) else 1e9 for n in n_allowable]

    ops_trace = {
        "type": "scatter", "mode": "markers",
        "name": "Operating ranges",
        "x": n_allowable,
        "y": eff_ranges.tolist(),
        "marker": {
            "size": marker_sizes.tolist(),
            "color": "#e74c3c", "opacity": 0.7,
            "symbol": "circle",
        },
        "hovertemplate": (
            "Δσ (eff): %{y:.1f} MPa<br>"
            "N_allow: %{x:.2e}<extra></extra>"
        ),
    }

    # DFF limit line: shift S-N curve left by DFF (equivalent lives)
    n_dff = n_arr / result.dff
    dff_trace = {
        "type": "scatter", "mode": "lines",
        "name": f"DFF={result.dff} design limit",
        "x": n_dff.tolist(), "y": s_arr.tolist(),
        "line": {"color": "#e74c3c", "dash": "dash", "width": 1.5},
    }

    layout = {
        "title": f"S-N Curve — {result.sn_curve.name}",
        "xaxis": {"title": "Allowable Cycles N", "type": "log", "exponentformat": "power"},
        "yaxis": {"title": "Stress Range Δσ (MPa)", "type": "log"},
        "height": 420,
        "margin": {"l": 80, "r": 30, "t": 60, "b": 60},
        "plot_bgcolor": "#fafafa",
        "legend": {"orientation": "h", "y": -0.25},
    }
    return json.dumps({"data": [sn_trace, dff_trace, ops_trace], "layout": layout})


def _histogram_chart_json(result: ExampleResult) -> str:
    """Stress histogram bar chart."""
    ranges = result.histogram["range"].tolist()
    counts = result.histogram["count"].tolist()

    trace = {
        "type": "bar",
        "name": "Cycle count",
        "x": ranges,
        "y": counts,
        "marker": {"color": "#3498db", "opacity": 0.8},
        "hovertemplate": "Δσ: %{x:.1f} MPa<br>Cycles: %{y:.2e}<extra></extra>",
    }
    layout = {
        "title": "Stress Range Histogram (design life)",
        "xaxis": {"title": "Stress Range Δσ (MPa)"},
        "yaxis": {"title": "Number of Cycles", "type": "log"},
        "height": 350,
        "margin": {"l": 80, "r": 30, "t": 60, "b": 60},
        "plot_bgcolor": "#fafafa",
    }
    return json.dumps({"data": [trace], "layout": layout})


def _summary_html(result: ExampleResult) -> str:
    dff_damage = result.damage * result.dff
    status_color = "green" if result.passes else "red"
    status_text = "PASS" if result.passes else "FAIL"
    life_str = f"{result.life_years:.1f} years" if np.isfinite(result.life_years) else "∞"
    return (
        f"<table class='summary-table'><tbody>"
        f"<tr><td>Miner Damage (design life)</td><td><b>{result.damage:.6f}</b></td></tr>"
        f"<tr><td>DFF = {result.dff}</td><td><b>D × DFF = {dff_damage:.4f}</b></td></tr>"
        f"<tr><td>Estimated Fatigue Life</td><td><b>{life_str}</b></td></tr>"
        f"<tr><td>Design Life</td><td>{result.design_life_years:.0f} years</td></tr>"
        f"<tr><td>SCF</td><td>{result.scf:.2f}</td></tr>"
        f"<tr><td>S-N Curve</td><td>{result.sn_curve.name}</td></tr>"
        f"<tr><td>Status</td>"
        f"<td><b style='color:{status_color};font-size:1.1em'>{status_text}</b></td></tr>"
        f"</tbody></table>"
    )


def _damage_table_html(result: ExampleResult) -> str:
    if not result.damage_contributions:
        return "<p><em>Damage contributions not available.</em></p>"

    contribs = sorted(result.damage_contributions,
                      key=lambda r: r.get("damage_increment", 0), reverse=True)
    top = contribs[:15]

    rows = []
    for r in top:
        rows.append(
            f"<tr>"
            f"<td>{r.get('stress_range', 0):.1f}</td>"
            f"<td>{r.get('cycles_applied', 0):.2e}</td>"
            f"<td>{r.get('cycles_allowable', 0):.2e}</td>"
            f"<td>{r.get('damage_increment', 0):.6f}</td>"
            f"<td>{r.get('cumulative_damage', 0):.6f}</td>"
            f"</tr>"
        )
    rows_html = "\n".join(rows)
    return (
        "<table class='sweep-table'>"
        "<thead><tr>"
        "<th>Δσ (MPa)</th><th>n Applied</th><th>N Allowable</th>"
        "<th>Damage</th><th>Cumulative D</th>"
        "</tr></thead>"
        f"<tbody>{rows_html}</tbody></table>"
    )


def _list_html(heading: str, items: List[str]) -> str:
    items_html = "\n".join(f"<li>{i}</li>" for i in items)
    return f"<h3>{heading}</h3><ul>{items_html}</ul>"


# ---------------------------------------------------------------------------
# HTML template
# ---------------------------------------------------------------------------

_EXAMPLE_TEMPLATE = """<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <title>{title} — Fatigue Analysis</title>
  <script src="https://cdn.plot.ly/plotly-2.27.0.min.js"></script>
  <style>
    body {{ font-family: Arial, sans-serif; margin: 20px; background: #f5f5f5; }}
    h1 {{ color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 8px; }}
    h2 {{ color: #34495e; margin-top: 28px; }}
    h3 {{ color: #555; }}
    .description {{ background: #fff; border-left: 4px solid #3498db;
                    padding: 12px 16px; margin: 12px 0; border-radius: 4px;
                    box-shadow: 0 1px 3px rgba(0,0,0,.1); }}
    .chart-box {{ background: #fff; border-radius: 6px;
                  box-shadow: 0 1px 4px rgba(0,0,0,.12);
                  padding: 12px; margin-bottom: 22px; }}
    .summary-table {{ border-collapse: collapse; font-size: 14px; width: 460px; }}
    .summary-table td {{ padding: 6px 14px; border-bottom: 1px solid #eee; }}
    .summary-table tr:nth-child(even) td {{ background: #f9f9f9; }}
    .sweep-table {{ width: 100%; border-collapse: collapse; font-size: 13px;
                    background: #fff; border-radius: 6px; overflow: hidden;
                    box-shadow: 0 1px 4px rgba(0,0,0,.12); }}
    .sweep-table th {{ background: #2c3e50; color: #fff; padding: 8px 12px; text-align: left; }}
    .sweep-table td {{ padding: 6px 12px; border-bottom: 1px solid #eee; }}
    .sweep-table tr:hover td {{ background: #f0f8ff; }}
    ul {{ line-height: 1.8; }}
  </style>
</head>
<body>
  <h1>{title}</h1>
  <div class="description">{description}</div>

  <h2>Result Summary</h2>
  <div class="chart-box">{summary}</div>

  <h2>S-N Curve</h2>
  <div class="chart-box"><div id="sn-chart"></div></div>

  <h2>Stress Range Histogram</h2>
  <div class="chart-box"><div id="hist-chart"></div></div>

  <h2>Damage Contributions (Top 15)</h2>
  {damage_table}

  {assumptions}
  {references}

  <script>
    Plotly.newPlot('sn-chart',   {sn_json}.data,   {sn_json}.layout,   {{responsive:true}});
    Plotly.newPlot('hist-chart', {hist_json}.data, {hist_json}.layout, {{responsive:true}});
  </script>
</body>
</html>
"""
