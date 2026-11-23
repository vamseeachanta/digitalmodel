# ABOUTME: Fatigue parametric sweep engine — Cartesian product of SCF/curve/thickness/DFF
# ABOUTME: Produces damage matrix DataFrame and interactive HTML with tornado sensitivity chart

"""
Fatigue Parametric Sweep Engine

Sweeps the Cartesian product of fatigue parameter space:
  SCF × S-N curve × wall thickness × DFF

For each combination computes Palmgren-Miner damage from a stress histogram,
yielding a ranked damage matrix and a sensitivity tornado chart showing which
parameter most affects fatigue life.

Typical usage::

    import pandas as pd
    from digitalmodel.structural.fatigue.parametric_sweep import run_sweep, generate_sweep_report

    # Stress histogram: stress ranges (MPa) and cycle counts over design life
    histogram = pd.DataFrame({
        "range": [50, 100, 150, 200],
        "count": [1e6, 1e5, 1e4, 1e3],
    })

    results = run_sweep(
        histogram,
        scf_values=[1.0, 1.5, 2.0],
        curves=[("DNV", "D"), ("DNV", "F"), ("API", "X")],
        thickness_values=[25.0, 32.0, 40.0],
        dff_values=[1.0, 2.0, 3.0],
    )

    html = generate_sweep_report(histogram, results, title="Pipeline Girth Weld Sweep")
"""

import itertools
import logging
from pathlib import Path
from typing import List, Optional, Tuple

import numpy as np
import pandas as pd

from digitalmodel.structural.fatigue.sn_curves import (
    StandardSNCurves,
    ThicknessCorrection,
)
from digitalmodel.structural.fatigue.damage_accumulation import LinearDamageAccumulation

logger = logging.getLogger(__name__)

# Reference thickness for thickness correction (DNV-RP-C203 default)
_REFERENCE_THICKNESS_MM = 25.0

# Columns in the results DataFrame
RESULT_COLS = [
    "scf",
    "standard",
    "curve_class",
    "curve_label",
    "thickness_mm",
    "dff",
    "damage",
    "life_factor",
    "dff_adjusted_damage",
    "passes_dff",
]


# ---------------------------------------------------------------------------
# Core sweep
# ---------------------------------------------------------------------------

def run_sweep(
    histogram: pd.DataFrame,
    scf_values: List[float],
    curves: List[Tuple[str, str]],
    thickness_values: List[float],
    dff_values: List[float],
    reference_thickness_mm: float = _REFERENCE_THICKNESS_MM,
) -> pd.DataFrame:
    """Run a Cartesian-product fatigue parametric sweep.

    Parameters
    ----------
    histogram : pd.DataFrame
        Stress histogram with columns ``range`` (MPa) and ``count`` (cycles).
        Rows with non-positive range or count are ignored.
    scf_values : list of float
        Stress concentration factors to sweep. SCF multiplies all stress
        ranges in ``histogram`` before damage calculation.
    curves : list of (standard, curve_class) tuples
        S-N curves to include, e.g. ``[("DNV", "D"), ("API", "X")]``.
        Invalid combinations are skipped with a warning.
    thickness_values : list of float
        Wall thicknesses in mm. Values equal to ``reference_thickness_mm``
        apply no correction; others are corrected per DNV thickness exponent
        (default 0.25).
    dff_values : list of float
        Design fatigue factors. DFF scales the damage: a component passes
        when ``damage * dff <= 1.0``.
    reference_thickness_mm : float, default 25.0
        Reference thickness for thickness correction.

    Returns
    -------
    pd.DataFrame
        One row per parameter combination with columns defined in
        ``RESULT_COLS``.  Sorted by ``dff_adjusted_damage`` ascending.
    """
    _validate_histogram(histogram)

    accumulator = LinearDamageAccumulation()
    rows: List[dict] = []

    for scf, (standard, curve_class), thickness, dff in itertools.product(
        scf_values, curves, thickness_values, dff_values
    ):
        try:
            base_curve = StandardSNCurves.get_curve(standard, curve_class)
        except ValueError:
            logger.warning("Skipping unknown curve %s-%s", standard, curve_class)
            continue

        # Thickness correction (no-op when thickness == reference)
        if abs(thickness - reference_thickness_mm) > 1e-6:
            curve = ThicknessCorrection.apply_thickness_effect(
                base_curve, thickness, reference_thickness_mm
            )
        else:
            curve = base_curve

        # Apply SCF to stress histogram
        eff_histogram = histogram.copy()
        eff_histogram["range"] = histogram["range"] * scf

        result = accumulator.calculate_damage(eff_histogram, curve)
        damage = result["total_damage"]

        life_factor = 1.0 / damage if damage > 0 else np.inf
        dff_adjusted = damage * dff
        passes = dff_adjusted <= 1.0

        rows.append(
            {
                "scf": scf,
                "standard": standard,
                "curve_class": curve_class,
                "curve_label": f"{standard}-{curve_class}",
                "thickness_mm": thickness,
                "dff": dff,
                "damage": damage,
                "life_factor": life_factor,
                "dff_adjusted_damage": dff_adjusted,
                "passes_dff": passes,
            }
        )

    if not rows:
        return pd.DataFrame(columns=RESULT_COLS)

    df = pd.DataFrame(rows, columns=RESULT_COLS)
    return df.sort_values("dff_adjusted_damage", ascending=True).reset_index(drop=True)


# ---------------------------------------------------------------------------
# Sensitivity analysis
# ---------------------------------------------------------------------------

def compute_sensitivity(
    histogram: pd.DataFrame,
    scf_values: List[float],
    curves: List[Tuple[str, str]],
    thickness_values: List[float],
    dff_values: List[float],
    reference_thickness_mm: float = _REFERENCE_THICKNESS_MM,
) -> pd.DataFrame:
    """One-at-a-time sensitivity analysis for tornado chart.

    For each parameter, all others are fixed at their median index; the
    target parameter is swept across its full range.  The reported
    sensitivity is ``max_damage - min_damage`` (using raw Miner sum).

    Parameters
    ----------
    histogram, scf_values, curves, thickness_values, dff_values, reference_thickness_mm
        Same as :func:`run_sweep`.

    Returns
    -------
    pd.DataFrame
        Columns: ``parameter``, ``min_damage``, ``max_damage``,
        ``sensitivity``, ``min_label``, ``max_label``.
        Sorted by ``sensitivity`` descending (widest bar at top of tornado).
    """
    _validate_histogram(histogram)

    # Base values (median index of each parameter)
    base_scf = scf_values[len(scf_values) // 2]
    base_curve = curves[len(curves) // 2]
    base_thickness = thickness_values[len(thickness_values) // 2]
    base_dff = dff_values[len(dff_values) // 2]

    results = []

    def _single_damage(scf, curve_tuple, thickness, dff):
        try:
            standard, curve_class = curve_tuple
            base = StandardSNCurves.get_curve(standard, curve_class)
        except ValueError:
            return np.nan
        if abs(thickness - reference_thickness_mm) > 1e-6:
            crv = ThicknessCorrection.apply_thickness_effect(
                base, thickness, reference_thickness_mm
            )
        else:
            crv = base
        eff = histogram.copy()
        eff["range"] = histogram["range"] * scf
        r = LinearDamageAccumulation().calculate_damage(eff, crv)
        return r["total_damage"] * dff

    # --- SCF sensitivity ---
    damages = [
        _single_damage(s, base_curve, base_thickness, base_dff)
        for s in scf_values
    ]
    damages = [d for d in damages if np.isfinite(d)]
    if damages:
        results.append(
            _sensitivity_row(
                "SCF",
                damages,
                [str(s) for s in scf_values],
            )
        )

    # --- S-N curve sensitivity ---
    damages = [
        _single_damage(base_scf, c, base_thickness, base_dff)
        for c in curves
    ]
    damages = [d for d in damages if np.isfinite(d)]
    if damages:
        results.append(
            _sensitivity_row(
                "S-N Curve",
                damages,
                [f"{s}-{c}" for s, c in curves],
            )
        )

    # --- Thickness sensitivity ---
    damages = [
        _single_damage(base_scf, base_curve, t, base_dff)
        for t in thickness_values
    ]
    damages = [d for d in damages if np.isfinite(d)]
    if damages:
        results.append(
            _sensitivity_row(
                "Thickness (mm)",
                damages,
                [str(t) for t in thickness_values],
            )
        )

    # --- DFF sensitivity ---
    damages = [
        _single_damage(base_scf, base_curve, base_thickness, d)
        for d in dff_values
    ]
    damages = [d for d in damages if np.isfinite(d)]
    if damages:
        results.append(
            _sensitivity_row(
                "DFF",
                damages,
                [str(d) for d in dff_values],
            )
        )

    if not results:
        return pd.DataFrame(
            columns=["parameter", "min_damage", "max_damage", "sensitivity",
                     "min_label", "max_label"]
        )

    df = pd.DataFrame(results)
    return df.sort_values("sensitivity", ascending=False).reset_index(drop=True)


def _sensitivity_row(param: str, damages: List[float], labels: List[str]) -> dict:
    """Build one sensitivity row from a list of DFF-adjusted damage values."""
    finite = [(d, l) for d, l in zip(damages, labels) if np.isfinite(d)]
    if not finite:
        return {
            "parameter": param, "min_damage": np.nan, "max_damage": np.nan,
            "sensitivity": 0.0, "min_label": "", "max_label": "",
        }
    min_d, min_label = min(finite, key=lambda x: x[0])
    max_d, max_label = max(finite, key=lambda x: x[0])
    return {
        "parameter": param,
        "min_damage": min_d,
        "max_damage": max_d,
        "sensitivity": max_d - min_d,
        "min_label": min_label,
        "max_label": max_label,
    }


# ---------------------------------------------------------------------------
# HTML report
# ---------------------------------------------------------------------------

def generate_sweep_report(
    histogram: pd.DataFrame,
    results: pd.DataFrame,
    title: str = "Fatigue Parametric Sweep",
    output_path: Optional[str] = None,
    max_table_rows: int = 50,
) -> str:
    """Generate a self-contained interactive HTML report.

    The report includes:

    1. **Tornado chart** — horizontal bar chart showing which parameter most
       affects DFF-adjusted damage (computed via one-at-a-time sensitivity).
    2. **Damage heatmap** — scatter/bubble plot of all combinations coloured
       by pass/fail status against DFF criterion.
    3. **Results table** — ranked top ``max_table_rows`` combinations.

    Parameters
    ----------
    histogram : pd.DataFrame
        The original stress histogram used for the sweep.
    results : pd.DataFrame
        Output of :func:`run_sweep`.
    title : str
        Report title shown in the HTML heading.
    output_path : str or Path, optional
        If provided, the HTML is also written to this file.
    max_table_rows : int, default 50
        Maximum rows shown in the results table.

    Returns
    -------
    str
        Self-contained HTML string.
    """
    if results.empty:
        html = _empty_report(title)
        if output_path:
            Path(output_path).write_text(html, encoding="utf-8")
        return html

    # Derive parameter lists from results for sensitivity
    scf_values = sorted(results["scf"].unique().tolist())
    curves = [
        (r["standard"], r["curve_class"])
        for _, r in results[["standard", "curve_class"]]
        .drop_duplicates()
        .iterrows()
    ]
    thickness_values = sorted(results["thickness_mm"].unique().tolist())
    dff_values = sorted(results["dff"].unique().tolist())

    sensitivity_df = compute_sensitivity(
        histogram, scf_values, curves, thickness_values, dff_values
    )

    tornado_json = _build_tornado_json(sensitivity_df)
    scatter_json = _build_scatter_json(results)
    table_html = _build_table_html(results, max_table_rows)
    summary = _build_summary(results)

    html = _HTML_TEMPLATE.format(
        title=title,
        summary=summary,
        tornado_json=tornado_json,
        scatter_json=scatter_json,
        table_html=table_html,
    )

    if output_path:
        Path(output_path).write_text(html, encoding="utf-8")

    return html


# ---------------------------------------------------------------------------
# Private helpers
# ---------------------------------------------------------------------------

def _validate_histogram(histogram: pd.DataFrame) -> None:
    if "range" not in histogram.columns:
        raise ValueError("histogram must have a 'range' column (stress ranges in MPa)")
    if "count" not in histogram.columns:
        raise ValueError("histogram must have a 'count' column (cycle counts)")


def _build_tornado_json(df: pd.DataFrame) -> str:
    """Build Plotly JSON for a horizontal tornado bar chart."""
    if df.empty:
        return '{"data":[],"layout":{}}'

    params = df["parameter"].tolist()
    sensitivities = df["sensitivity"].tolist()
    min_labels = df["min_label"].tolist()
    max_labels = df["max_label"].tolist()

    colors = ["#2ecc71" if s == min(sensitivities) else "#e74c3c"
              for s in sensitivities]

    # Build hover text
    hover = [
        f"Range: {mi} → {ma}<br>Sensitivity: {s:.4f}"
        for mi, ma, s in zip(min_labels, max_labels, sensitivities)
    ]

    data = [
        {
            "type": "bar",
            "orientation": "h",
            "y": params,
            "x": sensitivities,
            "marker": {"color": colors},
            "text": [f"{s:.4f}" for s in sensitivities],
            "textposition": "outside",
            "hovertext": hover,
            "hoverinfo": "text+y",
            "name": "Sensitivity (max − min DFF-adjusted damage)",
        }
    ]
    layout = {
        "title": "Parameter Sensitivity (One-at-a-Time)",
        "xaxis": {"title": "DFF-Adjusted Damage Range (max − min)"},
        "yaxis": {"autorange": "reversed"},
        "height": 350,
        "margin": {"l": 140, "r": 80, "t": 60, "b": 50},
        "plot_bgcolor": "#fafafa",
    }
    import json
    return json.dumps({"data": data, "layout": layout})


def _build_scatter_json(results: pd.DataFrame) -> str:
    """Scatter plot: x=SCF, y=damage, colour=curve, shape=pass/fail."""
    import json
    traces = []
    for curve_label in results["curve_label"].unique():
        sub = results[results["curve_label"] == curve_label]
        passes = sub[sub["passes_dff"]]
        fails = sub[~sub["passes_dff"]]

        for group, marker_symbol, name_suffix in [
            (passes, "circle", ""),
            (fails, "x", " (FAIL)"),
        ]:
            if group.empty:
                continue
            traces.append(
                {
                    "type": "scatter",
                    "mode": "markers",
                    "name": f"{curve_label}{name_suffix}",
                    "x": group["scf"].tolist(),
                    "y": group["dff_adjusted_damage"].tolist(),
                    "marker": {
                        "symbol": marker_symbol,
                        "size": 9,
                    },
                    "hovertemplate": (
                        "Curve: " + curve_label + "<br>"
                        "SCF: %{x}<br>"
                        "DFF×Damage: %{y:.4f}<br>"
                        "t=%{customdata[0]}mm DFF=%{customdata[1]}<extra></extra>"
                    ),
                    "customdata": list(
                        zip(
                            group["thickness_mm"].tolist(),
                            group["dff"].tolist(),
                        )
                    ),
                }
            )

    # Failure threshold line at y=1
    x_range = sorted(results["scf"].unique().tolist())
    traces.append(
        {
            "type": "scatter",
            "mode": "lines",
            "name": "DFF Limit (D×DFF = 1)",
            "x": [x_range[0], x_range[-1]],
            "y": [1.0, 1.0],
            "line": {"color": "red", "dash": "dash", "width": 2},
            "hoverinfo": "skip",
        }
    )

    layout = {
        "title": "DFF-Adjusted Damage vs SCF",
        "xaxis": {"title": "SCF"},
        "yaxis": {"title": "DFF × Miner Damage", "type": "log"},
        "height": 420,
        "margin": {"l": 80, "r": 30, "t": 60, "b": 60},
        "plot_bgcolor": "#fafafa",
        "legend": {"orientation": "v"},
    }
    return json.dumps({"data": traces, "layout": layout})


def _build_table_html(results: pd.DataFrame, max_rows: int) -> str:
    """Build an HTML table of the top-ranked sweep results."""
    display = results.head(max_rows).copy()
    display["damage"] = display["damage"].apply(lambda x: f"{x:.6f}")
    display["dff_adjusted_damage"] = display["dff_adjusted_damage"].apply(
        lambda x: f"{x:.6f}"
    )
    display["life_factor"] = display["life_factor"].apply(
        lambda x: f"{x:.1f}" if np.isfinite(x) else "∞"
    )
    display["passes_dff"] = display["passes_dff"].apply(
        lambda x: '<span style="color:green">✓ PASS</span>'
        if x
        else '<span style="color:red">✗ FAIL</span>'
    )
    display = display.rename(
        columns={
            "scf": "SCF",
            "curve_label": "S-N Curve",
            "thickness_mm": "Thickness (mm)",
            "dff": "DFF",
            "damage": "Miner Damage",
            "life_factor": "Life Factor",
            "dff_adjusted_damage": "DFF×Damage",
            "passes_dff": "Status",
        }
    )
    cols = ["SCF", "S-N Curve", "Thickness (mm)", "DFF",
            "Miner Damage", "DFF×Damage", "Life Factor", "Status"]
    display = display[[c for c in cols if c in display.columns]]
    return display.to_html(
        index=False, escape=False, border=0,
        classes="sweep-table"
    )


def _build_summary(results: pd.DataFrame) -> str:
    n_total = len(results)
    n_pass = results["passes_dff"].sum()
    n_fail = n_total - n_pass
    min_row = results.iloc[0]
    max_row = results.iloc[-1]
    return (
        f"<b>Total combinations:</b> {n_total} &nbsp;|&nbsp; "
        f"<b style='color:green'>Pass:</b> {n_pass} &nbsp;|&nbsp; "
        f"<b style='color:red'>Fail:</b> {n_fail}<br>"
        f"<b>Best:</b> {min_row['curve_label']} SCF={min_row['scf']} "
        f"t={min_row['thickness_mm']}mm DFF={min_row['dff']} "
        f"→ D×DFF={float(min_row['dff_adjusted_damage']):.4f}<br>"
        f"<b>Worst:</b> {max_row['curve_label']} SCF={max_row['scf']} "
        f"t={max_row['thickness_mm']}mm DFF={max_row['dff']} "
        f"→ D×DFF={float(max_row['dff_adjusted_damage']):.4f}"
    )


def _empty_report(title: str) -> str:
    return (
        f"<html><head><title>{title}</title></head>"
        f"<body><h1>{title}</h1>"
        f"<p>No valid sweep results — check curve names and histogram data.</p>"
        f"</body></html>"
    )


# ---------------------------------------------------------------------------
# HTML template
# ---------------------------------------------------------------------------

_HTML_TEMPLATE = """<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <title>{title}</title>
  <script src="https://cdn.plot.ly/plotly-2.27.0.min.js"></script>
  <style>
    body {{ font-family: Arial, sans-serif; margin: 20px; background: #f5f5f5; }}
    h1 {{ color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 8px; }}
    h2 {{ color: #34495e; margin-top: 30px; }}
    .summary {{ background: #fff; border-left: 4px solid #3498db;
                padding: 12px 16px; margin: 16px 0; border-radius: 4px;
                box-shadow: 0 1px 3px rgba(0,0,0,.1); }}
    .chart-box {{ background: #fff; border-radius: 6px;
                  box-shadow: 0 1px 4px rgba(0,0,0,.12); padding: 12px;
                  margin-bottom: 24px; }}
    .sweep-table {{ width: 100%; border-collapse: collapse; font-size: 13px;
                    background: #fff; border-radius: 6px; overflow: hidden;
                    box-shadow: 0 1px 4px rgba(0,0,0,.12); }}
    .sweep-table th {{ background: #2c3e50; color: #fff; padding: 8px 12px;
                       text-align: left; }}
    .sweep-table td {{ padding: 6px 12px; border-bottom: 1px solid #eee; }}
    .sweep-table tr:hover td {{ background: #f0f8ff; }}
  </style>
</head>
<body>
  <h1>{title}</h1>
  <div class="summary">{summary}</div>

  <h2>Parameter Sensitivity (Tornado Chart)</h2>
  <div class="chart-box">
    <div id="tornado"></div>
  </div>

  <h2>DFF-Adjusted Damage vs SCF</h2>
  <div class="chart-box">
    <div id="scatter"></div>
  </div>

  <h2>Ranked Results</h2>
  {table_html}

  <script>
    Plotly.newPlot('tornado', {tornado_json}.data, {tornado_json}.layout,
                   {{responsive: true}});
    Plotly.newPlot('scatter', {scatter_json}.data, {scatter_json}.layout,
                   {{responsive: true}});
  </script>
</body>
</html>
"""
