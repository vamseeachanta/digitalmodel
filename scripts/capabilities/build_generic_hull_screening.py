# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Generate a generic hull screening estimate, data and independent tests.

Run: python scripts/capabilities/build_generic_hull_screening.py
Only the standard library and the public U-tube model are required.
"""

from __future__ import annotations

import argparse
import csv
import html
import io
from collections.abc import Callable
import hashlib
import importlib.util
import json
import math
from pathlib import Path
import sys
from time import perf_counter
from typing import Any
from urllib.parse import quote

ROOT = Path(__file__).resolve().parents[2]
MODEL_PATH = "src/digitalmodel/hydrodynamics/utube_tank.py"
PAGE = "docs/api/cfd/generic-hull-roll-screening.html"
DATA = "docs/api/cfd/data/generic_hull_roll_screening.json"
TEST = "tests/hydrodynamics/test_generic_hull_screening.py"
PARENT_PAGE = "docs/api/cfd/dual-tank-sloshing-cfd-study.html"
SCRIPT = "scripts/capabilities/build_generic_hull_screening.py"
MODEL_SPEC = importlib.util.spec_from_file_location("_generic_roll_tank", ROOT / MODEL_PATH)
if MODEL_SPEC is None or MODEL_SPEC.loader is None:
    raise ImportError("Public tank model cannot be loaded")
MODEL = importlib.util.module_from_spec(MODEL_SPEC)
sys.modules[MODEL_SPEC.name] = MODEL
MODEL_SPEC.loader.exec_module(MODEL)

# Public reference tank inputs; no actual hull mass, inertia or restoring is used.
GEOMETRY = MODEL.UTubeGeometry(20.0, 6.0, 5.0, 5.0)
CONDUIT_AREA = 6.8
ALPHAS = (0.02, 0.05, 0.10, 0.15, 0.20)
CRITERIA = "generic-coupled-screening-v1"
GENERATION_DATE = "2026-10-25"


def canonical_numbers(value: Any, *, field: str = "") -> Any:
    """Round output, retaining search precision only inside the calculation."""
    if isinstance(value, float):
        # Peak locations are ill-conditioned at machine precision: indistinct
        # response values can select nearby periods on different runtimes.
        if field.endswith("_peak_period_s"):
            return round(value, 4)
        return float(format(value, ".12g"))
    if isinstance(value, dict):
        return {key: canonical_numbers(child, field=key) for key, child in value.items()}
    if isinstance(value, list):
        return [canonical_numbers(child) for child in value]
    return value


def maximize(
    function: Callable[[float], float], periods: list[float],
) -> tuple[float, float, list[float]]:
    """Refine every sampled local maximum, retaining boundary candidates."""
    values = [function(period) for period in periods]
    candidates = list(zip(values, periods))
    golden = (math.sqrt(5) - 1) / 2
    for index in range(1, len(periods) - 1):
        if values[index] < max(values[index - 1], values[index + 1]):
            continue
        low, high = periods[index - 1], periods[index + 1]
        left, right = high - golden * (high - low), low + golden * (high - low)
        fleft, fright = function(left), function(right)
        for _ in range(36):
            if fleft < fright:
                low, left, fleft = left, right, fright
                right = low + golden * (high - low)
                fright = function(right)
            else:
                high, right, fright = right, left, fleft
                left = high - golden * (high - low)
                fleft = function(left)
        candidates.extend(((fleft, left), (fright, right)))
    peak, period = max(candidates)
    if period in (periods[0], periods[-1]):
        raise ValueError("Peak lies at a search boundary; expand the period band")
    return peak, period, values


def screen_case(
    roll_period: float, damping: float, alpha: float, *, samples: int = 121,
) -> dict[str, Any]:
    """Compare separate bare and coupled peaks under identical wave moment."""
    if (not all(math.isfinite(value) for value in (roll_period, damping, alpha))
            or roll_period <= 0 or not 0 < damping < 0.5 or not 0 <= alpha < 1
            or not isinstance(samples, int) or samples < 21):
        raise ValueError("Finite period, positive light damping and 0 <= alpha < 1 required")
    periods = [roll_period * (0.7 + 0.6 * index / (samples - 1))
               for index in range(samples)]
    common = dict(vessel_roll_period=roll_period, hull_damping_ratio=damping,
                  tank_authority=alpha, geometry=GEOMETRY, conduit_area=CONDUIT_AREA)
    wave_moment = 2 * damping * math.radians(5)
    stiffness = MODEL._quasi_static_moment_per_radian(
        GEOMETRY, CONDUIT_AREA, MODEL.DEFAULT_DENSITY, MODEL.DEFAULT_GRAVITY)
    residuals = []

    def response(period: float, enabled: bool = True) -> float:
        value: complex = MODEL.coupled_roll_response(
            forcing_period=period, tank_enabled=enabled, **common)
        if enabled and alpha:
            amplitude = abs(value) * wave_moment
            moment = MODEL.tank_moment(
                geometry=GEOMETRY, conduit_area=CONDUIT_AREA,
                forcing_period=period, roll_amplitude=amplitude)
            impedance = alpha * complex(moment.in_phase, moment.quadrature) / (
                stiffness * amplitude)
            ratio = roll_period / period
            denominator = 1 - ratio**2 + 2j * damping * ratio - impedance
            residual = abs(value * denominator - 1)
            residuals.append(residual)
            if not math.isfinite(residual) or residual >= 1e-5:
                raise ValueError("Coupled response fails the equilibrium residual criterion")
        return abs(value)

    bare_peak, bare_period, bare_values = maximize(
        lambda period: response(period, False), periods)
    tank_peak, tank_period, tank_values = maximize(response, periods)
    long_period = 60.0
    penalty = 100 * (response(long_period) / response(long_period, False) - 1)
    return {
        "roll_period_s": roll_period, "hull_damping_ratio": damping, "alpha": alpha,
        "bare_peak_rao": bare_peak, "bare_peak_period_s": bare_period,
        "tank_peak_rao": tank_peak, "tank_peak_period_s": tank_period,
        "reduction_percent": 100 * (1 - tank_peak / bare_peak),
        "forcing_periods_s": periods, "bare_rao": bare_values, "tank_rao": tank_values,
        "long_forcing_period_s": long_period, "long_period_penalty_percent": penalty,
        # Cancellation makes the raw residual sensitive to last-bit math drift.
        # Report an upper bound, while enforcing the raw criterion above.
        "max_relative_residual_bound": (
            10.0 ** math.ceil(math.log10(max(residuals))) if any(residuals) else 0.0),
    }


def compute() -> dict[str, Any]:
    """Calculate the declared generic parameter matrix and damping ratios."""
    cases = [screen_case(period, damping, alpha)
             for period in (18.0, 21.0) for damping in (0.02, 0.05) for alpha in ALPHAS]
    damping_comparison = []
    for period in (18.0, 21.0):
        for alpha in ALPHAS:
            lower_damping, higher_damping = (
                next(row for row in cases if row["roll_period_s"] == period
                     and row["hull_damping_ratio"] == damping and row["alpha"] == alpha)
                for damping in (0.02, 0.05))
            damping_comparison.append({
                "roll_period_s": period, "alpha": alpha,
                "higher_to_lower_damping_benefit_ratio": (
                    higher_damping["reduction_percent"] / lower_damping["reduction_percent"]),
            })
    return {
        "criteria_revision": CRITERIA,
        "generation_date": GENERATION_DATE,
        "numeric_serialization": "At most 12 significant digits; refined peak periods rounded to 0.0001 s",
        "residual_reporting": "Conservative power-of-ten upper bound; raw residuals checked before rounding",
        "readiness": "screening estimate; engineering review pending",
        "source_classification": "Public source-neutral model; no private payload used",
        "model_revision": "public U-tube equations; source digests identify exact bytes",
        "assumptions": {
            "hull": "Generic normalized hull; no real hull data",
            "tank_geometry_basis": "Public reference tank inputs; ASSUMED - to be confirmed",
            "calibration_transfer": "ASSUMED - to be confirmed",
            "bare_roll_reference_basis": "ASSUMED - to be confirmed",
            "leg_length_m": 20.0, "leg_width_m": 6.0, "fill_depth_m": 5.0,
            "centroid_offset_m": 5.0, "conduit_area_m2": CONDUIT_AREA,
            "effective_conduit_length_m": GEOMETRY.effective_conduit_length,
            "loss_coefficient": MODEL.DEFAULT_LOSS_COEFFICIENT,
            "loss_area_exponent": MODEL.DEFAULT_AREA_EXPONENT,
            "density_kg_m3": MODEL.DEFAULT_DENSITY, "gravity_m_s2": MODEL.DEFAULT_GRAVITY,
            "bare_roll_reference_deg": 5.0,
            "forcing": "Constant within each comparison; proportional to hull damping between groups",
            "normalized_hull_restoring": 1.0,
            "alpha_definition": "Tank quasi-static moment stiffness / hull roll restoring",
            "period_band_over_roll_period": [0.7, 1.3],
            "samples": 121, "peak_refinement_iterations": 36,
            "equilibrium_residual_limit": 1e-5,
            "moment_validated_min_forcing_period_s": 20.0,
        },
        "assumption_units": {
            "leg_length_m": "m", "leg_width_m": "m", "fill_depth_m": "m",
            "centroid_offset_m": "m", "conduit_area_m2": "m2",
            "effective_conduit_length_m": "m", "loss_area_exponent": "1",
            "loss_coefficient": "m^(2*loss_area_exponent-4)",
            "density_kg_m3": "kg/m3", "gravity_m_s2": "m/s2",
            "bare_roll_reference_deg": "deg", "normalized_hull_restoring": "1",
            "period_band_over_roll_period": "1", "samples": "count",
            "peak_refinement_iterations": "count", "equilibrium_residual_limit": "1",
            "moment_validated_min_forcing_period_s": "s",
        },
        "units": {"roll_period_s": "s", "hull_damping_ratio": "1", "alpha": "1",
                  "bare_peak_rao": "1", "tank_peak_rao": "1",
                  "bare_peak_period_s": "s", "tank_peak_period_s": "s",
                  "forcing_periods_s": "s", "bare_rao": "1", "tank_rao": "1",
                  "reduction_percent": "%", "long_forcing_period_s": "s",
                  "long_period_penalty_percent": "%", "max_relative_residual_bound": "1",
                  "higher_to_lower_damping_benefit_ratio": "1"},
        "cases": cases, "damping_comparison": damping_comparison,
    }


# Adapted from build_dual_tank_sloshing_study.chart: retain its axes and palette.
def chart_svg(identifier, title, xs, series, xlabel, ylabel, supported_from=None):
    """Inline accessible SVG with independent axes, legend and validity shading."""
    colors = ("#0B3D91", "#0f8a7e", "#b8860b", "#943a78", "#555555")
    left, right, top, bottom = 84, 770, 46, 300
    xmin, xmax = min(xs), max(xs)
    ymin = min(0, min(min(ys) for _, ys in series))
    ymax = max(max(ys) for _, ys in series)

    def x(value):
        return left + (value - xmin) / (xmax - xmin) * (right - left)

    def y(value):
        return bottom - (value - ymin) / (ymax - ymin) * (bottom - top)

    shading = (" Shaded periods are outside the published moment fit band."
               if supported_from is not None else "")
    parts = [f'<svg id="{identifier}" class="chart" viewBox="0 0 820 440" role="img" '
             f'aria-labelledby="{identifier}-title {identifier}-desc" '
             'style="width:100%;height:auto">',
             f'<title id="{identifier}-title">{html.escape(title)}</title>',
             f'<desc id="{identifier}-desc">{html.escape(ylabel)} against '
             f'{html.escape(xlabel)}. {html.escape("; ".join(n for n, _ in series))}.'
             f'{shading}</desc>',
             '<rect width="820" height="440" fill="white"/>',
             f'<text class="chart-title" x="84" y="22" fill="#16324f">{html.escape(title)}</text>']
    if supported_from is not None:
        parts.append(f'<rect x="{left}" y="{top}" '
                     f'width="{x(supported_from)-left:.1f}" '
                     f'height="{bottom-top}" fill="#fff0d0"/>')
    for i in range(6):
        xv = xmin + (xmax - xmin) * i / 5
        yv = ymin + (ymax - ymin) * i / 5
        parts.extend([
            f'<path d="M {left} {y(yv):.1f} H {right}" stroke="#e2e7eb"/>',
            f'<text x="{left-8}" y="{y(yv)+4:.1f}" text-anchor="end" '
            f'font-size="12">{yv:.1f}</text>',
            f'<text x="{x(xv):.1f}" y="322" text-anchor="middle" '
            f'font-size="12">{xv:.1f}</text>',
        ])
    parts.extend([
        f'<text class="axis-title" x="427" y="345" text-anchor="middle">{html.escape(xlabel)}</text>',
        '<text class="axis-title" transform="translate(18 175) rotate(-90)" text-anchor="middle">'
        f'{html.escape(ylabel)}</text>',
    ])
    for index, (name, ys) in enumerate(series):
        color = colors[index % len(colors)]
        points = " ".join(f"{x(xv):.1f},{y(yv):.1f}" for xv, yv in zip(xs, ys))
        parts.extend([
            f'<g data-series="{index}"><polyline points="{points}" fill="none" stroke="{color}" stroke-width="2"/>',
            f'<text x="84" y="{365+index*16}" fill="{color}" '
            f'font-size="12">{html.escape(name)}</text>',
        ])
        for xv, yv in zip(xs, ys):
            parts.append(
                f'<circle cx="{x(xv):.1f}" cy="{y(yv):.1f}" r="2.5" '
                f'fill="{color}" tabindex="0" data-series="{index}" '
                f'data-x="{xv}" data-y="{yv}" '
                f'aria-label="{html.escape(name)}: {xv:.4g}, {yv:.4g}">'
                f'<title>{html.escape(name)}: {xv:.4g}, {yv:.4g}</title></circle>')
        parts.append('</g>')
    return "\n".join(parts) + "\n</svg>"



def schematic(record: dict[str, Any]) -> str:
    """Draw proportional recorded dimensions; no hull or duct height is inferred.

    Presentation basis: the single-tank study builder's equal-aspect water/wall
    treatment, with the dual-tank chart palette. Coordinates are drawing units,
    not engineering inputs; all dimension values come from record.assumptions.
    """
    a = record["assumptions"]
    scale, centre, waterline = 24, 410, 150
    width = a["leg_width_m"] * scale
    depth = a["fill_depth_m"] * scale
    offset = a["centroid_offset_m"] * scale
    left, right = centre - offset - width / 2, centre + offset - width / 2
    base = waterline + depth
    # Conduit height is deliberately symbolic: only its area is specified.
    duct = 18
    return f'''<svg id="hull-section" class="schematic" viewBox="0 0 820 450" role="img"
aria-labelledby="hull-title hull-desc" style="width:100%;height:auto">
<title id="hull-title">Hull section with U-tube anti-roll tank</title>
<desc id="hull-desc">Proportional transverse leg widths, fill depths and centroid offsets
from Table 3.1. Hull outline and conduit height are symbolic; no hull dimensions are supplied.</desc>
<defs><marker id="dimension-arrow" viewBox="0 0 10 10" refX="5" refY="5"
markerWidth="6" markerHeight="6" orient="auto-start-reverse">
<path d="M 9 1 L 1 5 L 9 9" fill="none" stroke="#526579" stroke-width="1.5"/>
</marker></defs>
<rect width="820" height="450" fill="white"/>
<text x="410" y="28" text-anchor="middle" font-size="16">U-tube anti-roll tank · transverse section</text>
<path d="M 115 135 L 175 324 Q 410 404 645 324 L 705 135"
fill="#f2f6fa" stroke="#526579" stroke-width="2"/>
<path d="M {left:.1f} {waterline:.1f} v {depth:.1f} H {right + width:.1f} v {-depth:.1f}
 h {-width:.1f} v {depth - duct:.1f} H {left + width:.1f} v {duct - depth:.1f} Z"
 fill="#c4d9ef" stroke="#0B3D91" stroke-width="2"/>
<path d="M {left:.1f} {waterline:.1f} h {width:.1f} M {right:.1f} {waterline:.1f} h {width:.1f}"
 stroke="#0f8a7e" stroke-width="2"/>
<path d="M {centre} 105 V 367 M {centre-offset:.1f} 122 V {base+10:.1f}
 M {centre+offset:.1f} 122 V {base+10:.1f}" stroke="#8293a4" stroke-dasharray="6 4" fill="none"/>
<g stroke="#526579" stroke-width="1" fill="none">
<path d="M {left:.1f} 140 V 73 M {left+width:.1f} 140 V 73
 M {centre} 100 V 108 M {centre+offset:.1f} 100 V 122
 M {left-40:.1f} {waterline:.1f} H {left-8:.1f}
 M {left-40:.1f} {base:.1f} H {left-8:.1f}"/>
<path d="M {left:.1f} 82 H {left+width:.1f}" marker-start="url(#dimension-arrow)" marker-end="url(#dimension-arrow)"/>
<path d="M {centre} 113 H {centre+offset:.1f}" marker-start="url(#dimension-arrow)" marker-end="url(#dimension-arrow)"/>
<path d="M {left-30:.1f} {waterline:.1f} V {base:.1f}" marker-start="url(#dimension-arrow)" marker-end="url(#dimension-arrow)"/>
<path d="M {centre+25:.1f} {base-duct/2:.1f} L 603 310 H 715 M 410 360 V 380 H 600"/>
</g>
<g fill="#16324f" font-size="14">
<text x="{left+width/2:.1f}" y="67" text-anchor="middle">Leg width {a['leg_width_m']:.1f} m</text>
<text x="{centre+offset/2:.1f}" y="98" text-anchor="middle">Offset {a['centroid_offset_m']:.1f} m</text>
<text transform="translate({left-46:.1f} {waterline+depth/2:.1f}) rotate(-90)" text-anchor="middle">Fill {a['fill_depth_m']:.1f} m</text>
<text x="{centre}" y="390" text-anchor="end">Centreline</text>
<text x="605" y="300">A_c {a['conduit_area_m2']:.1f} m²</text>
<text x="605" y="330">Symbolic duct height</text>
<text x="410" y="418" text-anchor="middle">Leg length {a['leg_length_m']:.1f} m (out of section) · dimensions ASSUMED</text>
<text x="410" y="442" text-anchor="middle">Hull section: outline and duct height not to scale</text>
</g></svg>'''


def chart_specs(record: dict[str, Any]) -> list[tuple]:
    """Select existing calculated samples without interpolation or new cases."""
    cases = [row for row in record["cases"] if row["roll_period_s"] == 18.0]
    reference = [row for row in cases if row["alpha"] == .05]
    response = next(row for row in reference if row["hull_damping_ratio"] == .02)
    return [
        ("damping", "Peak roll reduction against hull damping",
         [100 * row["hull_damping_ratio"] for row in reference],
         [("Alpha 0.05", [row["reduction_percent"] for row in reference])],
         "Hull damping (% of critical)", "Peak roll reduction (%)"),
        ("response", "Roll response with and without the tank",
         response["forcing_periods_s"],
         [("Without tank", response["bare_rao"]), ("With tank", response["tank_rao"])],
         "Wave forcing period (s)", "Roll / unit wave moment (1)"),
        ("alpha", "Peak roll reduction against alpha", sorted({row["alpha"] for row in cases}),
         [(f"Hull damping {100 * damping:.0f} %",
           [row["reduction_percent"] for row in cases if row["hull_damping_ratio"] == damping])
          for damping in sorted({row["hull_damping_ratio"] for row in cases})],
         "Alpha: normalized stiffness (1)", "Peak roll reduction (%)"),
    ]


def source_note() -> str:
    """Identify source lines in the same data record and calculation code."""
    return ('<a href="data/generic_hull_roll_screening.json">Data record</a>: '
            'assumptions and cases fields. '
            f'<a href="../../../{SCRIPT}">Builder</a>: '
            f'compute, line {compute.__code__.co_firstlineno}; '
            f'screen_case, line {screen_case.__code__.co_firstlineno}; '
            f'chart_specs, line {chart_specs.__code__.co_firstlineno}.')


def interactive_chart(identifier, title, xs, series, xlabel, ylabel) -> str:
    """Use identical values for SVG hover targets and downloadable CSV rows."""
    stream = io.StringIO(newline="")
    writer = csv.writer(stream, lineterminator="\n")
    writer.writerow(["series", xlabel, ylabel])
    for name, ys in series:
        writer.writerows((name, x, y) for x, y in zip(xs, ys))
    controls = ''.join(
        f'<label><input type="checkbox" checked aria-controls="{identifier}" '
        f'onchange="toggleSeries(\'{identifier}\', {i}, this.checked)">'
        f'{html.escape(name)}</label> ' for i, (name, _) in enumerate(series))
    svg = chart_svg(identifier, title, xs, series, xlabel, ylabel,
                    supported_from=20 if identifier == "response" else None)
    number = {"damping": "5.1", "response": "5.2", "alpha": "5.3"}[identifier]
    return (f'<figure id="fig-{identifier}">{svg}'
            f'<figcaption>Figure {number} – {html.escape(title)}.</figcaption>'
            f'<div class="series-controls">{controls}</div>'
            f'<a class="chart-export" download="{identifier}.csv" href="data:text/csv;charset=utf-8,'
            f'{quote(stream.getvalue(), safe="")}">Export CSV</a>'
            '<p class="notes">Source: <a href="#ref-data">[I1]</a>. '
            '<a href="#sensitivity-notes">Chart notes in Appendix A</a>.</p></figure>')


def render(record: dict[str, Any]) -> str:
    """Present the recorded calculation in the selected engineering structure."""
    a = record["assumptions"]
    rows = []
    for row in record["cases"]:
        rows.append('<tr>' + ''.join(f'<td>{value}</td>' for value in (
            f'{row["roll_period_s"]:.0f}', f'{100 * row["hull_damping_ratio"]:.0f}',
            f'{row["alpha"]:.2f}', f'{row["reduction_percent"]:.1f}',
            f'{row["tank_peak_period_s"]:.1f}')) + '</tr>')

    def case(damping: float, alpha: float = 0.05) -> dict[str, Any]:
        return next(row for row in record["cases"] if row["roll_period_s"] == 18.0
                    and row["hull_damping_ratio"] == damping and row["alpha"] == alpha)

    lower_damping_benefit = case(0.02)["reduction_percent"]
    higher_damping_benefit = case(0.05)["reduction_percent"]
    ratio = higher_damping_benefit / lower_damping_benefit
    ratios = [row["higher_to_lower_damping_benefit_ratio"]
              for row in record["damping_comparison"]]
    ratio_groups = '; '.join(
        f'{period:.0f} s: ' + ', '.join(
            f'{row["higher_to_lower_damping_benefit_ratio"]:.2f}'
            for row in record["damping_comparison"] if row["roll_period_s"] == period)
        for period in (18.0, 21.0))
    natural_period = MODEL.natural_period(
        leg_area=GEOMETRY.leg_area, conduit_area=CONDUIT_AREA,
        fill_depth=GEOMETRY.fill_depth, effective_conduit_length=GEOMETRY.effective_conduit_length)
    model_bytes = (ROOT / MODEL_PATH).read_bytes()
    model_blob = hashlib.sha1(
        f'blob {len(model_bytes)}\0'.encode() + model_bytes).hexdigest()
    charts = ''.join(interactive_chart(*spec) for spec in chart_specs(record))

    # Input values/units are record.assumptions; table citations identify their
    # existing basis. Values are not selected or recalibrated for presentation.
    input_fields = [
        ("Each leg length", "leg_length_m"), ("Each leg width", "leg_width_m"),
        ("Fill depth", "fill_depth_m"), ("Centroid offset", "centroid_offset_m"),
        ("Conduit area", "conduit_area_m2"),
        ("Effective conduit length", "effective_conduit_length_m"),
        ("Fluid density", "density_kg_m3"), ("Gravity", "gravity_m_s2"),
        ("Loss coefficient", "loss_coefficient"), ("Loss area exponent n", "loss_area_exponent"),
        ("Bare-roll reference", "bare_roll_reference_deg"),
        ("Normalized hull restoring", "normalized_hull_restoring"),
    ]
    # Display symbols retain the units in record.assumption_units; n is the
    # recorded loss-area exponent, not an additional coefficient.
    display_units = {"m2": "m²", "kg/m3": "kg/m³", "m/s2": "m/s²", "deg": "°",
                     "m^(2*loss_area_exponent-4)": "m^(2n−4)"}
    inputs = ''.join(
        '<tr><td>' + label + '</td><td>' + format(a[key], '.4g') + '</td><td>'
        + html.escape(display_units.get(record["assumption_units"][key],
                                        record["assumption_units"][key]))
        + '</td><td>ASSUMED; <a href="#ref-data">[I1]</a>'
        + ('; <a href="#ref-model">[I2]</a>' if key in (
            "effective_conduit_length_m", "density_kg_m3", "gravity_m_s2",
            "loss_coefficient", "loss_area_exponent") else '') + '</td></tr>'
        for label, key in input_fields)
    return f'''<!DOCTYPE html>
<html lang="en"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Generic hull coupled roll screening estimate</title>
<link rel="stylesheet" href="../_assets/brand.css">
<style>
/* Presentation basis: single-tank study spacing and dual-tank SVG chart axes. */
*{{box-sizing:border-box}}body{{font-family:system-ui,sans-serif;line-height:1.6;max-width:960px;margin:auto;padding:24px;color:#16324f;background:white}}
header{{border-bottom:3px solid #0B3D91;padding-bottom:16px}}h1{{font-size:1.9rem;line-height:1.2}}
h2{{font-size:1.3rem;margin-top:2em;border-bottom:1px solid #ccd4dd;padding-bottom:.3em}}h3{{font-size:1.05rem;margin-top:1.5em}}
p,li,td{{font-weight:400}}a{{color:#0B3D91}}nav ul{{columns:2;padding-left:20px}}nav li{{margin:.3em 0}}
table{{border-collapse:collapse;width:100%;font-size:.9rem;font-variant-numeric:tabular-nums}}
th,td{{text-align:left;padding:8px;border-bottom:1px solid #ccd4dd;overflow-wrap:anywhere}}
caption{{caption-side:bottom;text-align:left;padding:10px 0;font-size:.9rem}}
figure{{margin:24px 0;break-inside:avoid}}figcaption{{font-size:.9rem;margin-top:8px}}
.chart text{{font-family:system-ui,sans-serif;font-weight:400;font-size:12px}}
.chart .axis-title{{font-size:14px}}.chart .chart-title{{font-size:14px}}
.schematic text{{font-family:system-ui,sans-serif;font-weight:400}}
.notice{{padding:12px 16px;background:#fff0d0;border-left:3px solid #b8860b}}
.notes{{font-size:.9rem;color:#43586c}}pre{{white-space:pre-wrap;overflow-wrap:anywhere;background:#f2f6fa;padding:16px}}
[hidden]{{display:none}}circle:hover,circle:focus{{r:5;stroke:#16324f;stroke-width:1.5}}
.series-controls label{{display:inline-block;margin-right:16px;font-size:.9rem}}
.chart-export{{font-size:.9rem}}details{{margin-top:2em}}summary{{cursor:pointer;font-size:1.05rem}}
footer{{margin-top:32px;padding-top:16px;border-top:1px solid #ccd4dd;font-size:.85rem}}
@media(max-width:600px){{body{{padding:16px}}nav ul{{columns:1}}th,td{{padding:5px;font-size:.8rem}}}}
@media print{{
@page{{margin:16mm}}body{{max-width:none;padding:0;color:#16324f;font-size:10pt}}
h1{{font-size:19pt}}h2{{font-size:14pt;break-after:avoid}}h3{{font-size:11pt;break-after:avoid}}
.chart text,.chart .axis-title,.chart .chart-title{{font-size:12px!important;font-weight:400!important}}
.schematic text{{font-size:14px!important;font-weight:400!important}}
figcaption,caption,.notes,.series-controls label,.chart-export{{font-size:9pt!important;font-weight:400!important}}
.series-controls,.chart-export{{display:none}}[hidden]{{display:inline!important}}
figure,pre{{break-inside:avoid}}thead{{display:table-header-group}}tr{{break-inside:avoid}}
nav{{display:none}}a{{color:inherit;text-decoration:none}}details>table{{display:table}}
}}
</style></head><body>
<header><h1>Generic hull coupled roll screening estimate</h1>
<p class="subtitle">Generic hull roll-plus-tank screening estimate from a public coupled model.</p>
<p>Internal engineering report · generic normalized hull</p></header>
<nav aria-label="Contents"><ul>
<li><a href="#summary">1 Executive summary</a></li><li><a href="#introduction">2 Introduction</a></li>
<li><a href="#design-data">3 Design data</a></li><li><a href="#method">4 Analysis methodology</a></li>
<li><a href="#comparison">5 Results</a></li><li><a href="#validation">6 Validation and benchmark</a></li>
<li><a href="#limits">7 Limitations</a></li><li><a href="#conclusions">8 Conclusions and next steps</a></li>
<li><a href="#references">9 References</a></li><li><a href="#sensitivity">Appendix A Sensitivity matrix</a></li>
<li><a href="#reproduce">Appendix B Reproduce commands</a></li><li><a href="#revisions">Appendix C Revision history</a></li>
</ul></nav>
<section id="summary"><h2>1 Executive summary</h2>
<p>The coupled model can be used to compare generic hull roll with and without the connected tank.
The results are screening estimates; no result represents a real hull.</p>
<p class="notice">The published moment fit does not validate coupled hull reduction.
No tank-on and tank-off real-vessel comparison establishes these reductions.
The implications are listed in <a href="#limits">Section 7</a>.</p>
<ul>
<li>At an 18 s hull roll period, alpha 0.05 with no units, and conduit area A_c = 6.8 m²,
the peak roll reduction screening estimates are {lower_damping_benefit:.1f} % at 2 % hull damping and {higher_damping_benefit:.1f} % at 5 %.
See <a href="#fig-damping">Figure 5.1</a>.</li>
<li>At 60 s forcing and 18 s hull period with 2 % hull damping, there is {case(0.02)["long_period_penalty_percent"]:.1f} % more roll at alpha 0.05
and {case(0.02, 0.20)["long_period_penalty_percent"]:.1f} % at alpha 0.20, against a {100 * (1 / (1 - 0.20) - 1):.1f} % limit.
See <a href="#penalty">Section 5.2</a>.</li>
<li>The moment fit has 4.3 % RMS error for forcing periods of 20 s and longer; it degrades below about 16 s for a reason not established.
See <a href="#validation">Section 6</a>.</li>
<li>The reference tank's natural period is {natural_period:.1f} s; the 18 s sweep starts at {record["cases"][0]["forcing_periods_s"][0]:.1f} s.
Every sampled forcing period below 20 s lies outside that band, including parts of the 21 s sweep.
See <a href="#sensitivity">Appendix A</a>.</li>
</ul></section>
<section id="introduction"><h2>2 Introduction</h2>
<p>The generic hull roll comparison describes the effect of a connected anti-roll tank for engineers considering the simplified coupled model.
The scope is a normalized hull and the public reference tank, with no actual hull mass, inertia or restoring.</p>
<p>These screening estimates exclude sea-state weighting, structural loads and design qualification.
No full-scale roll reduction has been calculated here.</p></section>
<section id="design-data"><h2>3 Design data</h2>
<p>The calculation uses public reference tank inputs (ASSUMED - to be confirmed), with no actual hull geometry.
The input values and their units are listed in <a href="#inputs">Table 3.1</a>.</p>
<h3>3.1 Site or field data</h3><p>Not applicable: no site or field is represented.</p>
<h3>3.2 Structure or equipment data</h3>
<p>Each leg measures 20.0 m by 6.0 m (120 m² in plan). Fill depth is 5.0 m; centroid offset is 5.0 m.
Conduit area A_c is 6.8 m². The model supplies effective length, fluid properties and calibrated loss parameters.</p>
<table id="inputs"><thead><tr><th>Input</th><th>Value</th><th>Unit</th><th>Basis</th></tr></thead>
<tbody>{inputs}</tbody><caption>Table 3.1 – Recorded tank and normalized-hull inputs.</caption></table>
<p class="notes">In the loss-coefficient unit, n is the loss area exponent in Table 3.1.</p>
<p>Calibration transfer: ASSUMED - to be confirmed.
These screening estimates use the public reference tank, which differs from the illustrative tank on the linked study page <a href="#ref-study">[I3]</a>.</p>
<p>The transverse leg widths, fill depths and centroid offsets in <a href="#fig-section">Figure 3.1</a> use the dimensions in <a href="#inputs">Table 3.1</a>.
Tank dimensions are recorded assumptions; the hull outline supplies no hull dimensions.</p>
<figure id="fig-section">{schematic(record)}<figcaption>Figure 3.1 – Connected-tank transverse section.</figcaption></figure>
<h3>3.3 Environmental data</h3>
<p>No sea state is specified. Forcing stays fixed within each tank-on and tank-off comparison and is rescaled between damping groups.</p>
<h3>3.4 Vessel data</h3>
<p>Generic hull periods are 18 s and 21 s; hull damping is 2 % and 5 % of critical, and alpha is 0.02, 0.05, 0.10, 0.15 and 0.20.
All hull values are ASSUMED; no real hull data is used. The full load-case matrix is in <a href="#sensitivity">Appendix A</a>.</p>
<h3>3.5 Criteria and sources</h3>
<p>No design acceptance criterion is specified. The arithmetic checks and the published moment-fit band are distinguished in <a href="#validation">Section 6</a>.
Their sources are <a href="#ref-builder">[I4]</a> and <a href="#ref-public">[R1]</a>.</p>
<h3>3.6 Method</h3><p>The coupled roll method is described in <a href="#method">Section 4</a>.</p>
<h3>3.7 Load cases</h3>
<p>Every combination of the assumed hull periods, damping and alpha values is retained in <a href="#sensitivity-table">Table A.1</a>.
The response sweep covers 0.7 to 1.3 times each hull period; the additional long-period comparison uses 60 s forcing <a href="#ref-data">[I1]</a>.</p>
</section>
<section id="method"><h2>4 Analysis methodology</h2>
<p>The model links hull roll to water transfer between connected tank legs. Tank moments feed back into the hull roll equation.
Alpha is the tank's quasi-static free-surface moment stiffness divided by the hull's roll restoring. Alpha has no units.</p>
<p>The public U-tube model is used; a software version is not declared. Its exact source is identified in <a href="#ref-model">[I2]</a>.
The public model includes vertical-centroid and local free-surface terms in that quasi-static stiffness.
Ks also includes water redistribution between the legs. Alpha therefore follows the complete public moment convention.</p>
<p id="eq-bare">(4.1) The normalized bare-hull impedance is D = 1 − (T₀/T)² + 2iζ(T₀/T).</p>
<p id="eq-coupled">(4.2) The coupled impedance is D − αH/Kₛ. H is tank moment per roll angle; Kₛ is its quasi-static limit.</p>
<p>T₀ is the hull roll period (s), T is the forcing period (s), ζ is the hull damping ratio (1), and i is the imaginary unit.
H and Kₛ use the same moment-per-angle units, so both impedances and alpha are dimensionless <a href="#ref-model">[I2]</a>.</p>
<p>The hull equation uses normalized restoring of 1, with no units. The model sets forcing to give 5° bare roll at the nominal resonance
(ASSUMED - to be confirmed). Forcing stays fixed within each tank-on and tank-off comparison.
Forcing is rescaled between damping groups to keep the 5 degree bare-roll reference, so the comparison also changes the tank amplitude.</p>
<p>The calculation finds each response peak separately over 0.7 to 1.3 times the hull roll period, with no units.
It refines all sampled local peaks. Reduction equals 100 % times one minus the coupled peak divided by the bare peak.
The input basis and calculation are recorded in <a href="#ref-data">[I1]</a> and <a href="#ref-builder">[I4]</a>.</p>
</section>
<section id="comparison"><h2>5 Results</h2>
<p>All charts select the existing 18 s hull roll period and conduit area {a['conduit_area_m2']:.1f} m².
The wave-period chart uses 2 % hull damping and alpha 0.05. Alpha is normalized stiffness as defined in <a href="#method">Section 4</a>.
The calculated tank natural period is {natural_period:.1f} s.</p>
<h3>5.1 Peak roll reduction</h3>
<p><a href="#fig-damping">Figure 5.1</a> shows peak reduction against hull damping;
<a href="#fig-response">Figure 5.2</a> shows roll response against forcing period;
<a href="#fig-alpha">Figure 5.3</a> shows peak reduction against alpha.
The full sensitivity matrix and damping comparison are in <a href="#sensitivity">Appendix A</a>.</p>
{charts}
<h3 id="penalty">5.2 Long-period penalty</h3>
<p>Slow water transfer reduces the effective hull restoring. In the slow-forcing limit, the roll ratio approaches 1/(1−α), with no units.
At alpha 0.05, with no units, that limit gives {100 * (1 / (1 - 0.05) - 1):.1f} % more roll.</p>
<p>At 60 s forcing and 18 s hull period with 2 % hull damping, there is {case(0.02)["long_period_penalty_percent"]:.1f} % more roll at alpha 0.05
and {case(0.02, 0.20)["long_period_penalty_percent"]:.1f} % at alpha 0.20, against a {100 * (1 / (1 - 0.20) - 1):.1f} % limit.
The slow-forcing limit understates the penalty at 60 s. A near-resonance reduction does not remove this penalty.
The comparison is recorded in <a href="#ref-data">[I1]</a>.</p></section>
<section id="validation"><h2>6 Validation and benchmark</h2>
<p>The published moment fit and the mathematical hand checks address different quantities.
The moment fit has 4.3 % RMS error for forcing periods of 20 s and longer; it degrades below about 16 s for a reason not established <a href="#ref-public">[R1]</a>.</p>
<p>The published moment fit does not validate coupled hull reduction.
No tank-on and tank-off real-vessel comparison establishes these reductions. No full-scale roll reduction has been calculated here.</p>
<p>Alpha = 0, with no units, gives zero reduction, or 0 %.
An undamped box hand check copies the model's own moment formula, so it checks arithmetic and the coupling denominator,
not the physics of the moment model. It also checks both natural frequencies <a href="#ref-tests">[I5]</a>.</p>
<p>For that mathematical check, each leg measures 2 m by 1 m, with 1 m depth and 1 m offset.
The conduit has 1 m² area and 1 m effective length. Fluid density is 1 kg/m³; gravity is 1 m/s².</p>
<p>The hand calculation gives Kₛ = 53/6 N m/rad and H = 359/24 N m/rad at 0.5 rad/s.
At alpha 0.1, with no units, the response is 2120/1231 times normalized forcing, with no units <a href="#ref-tests">[I5]</a>.</p>
</section>
<section id="limits"><h2>7 Limitations</h2>
<p>The reductions are screening estimates for generic hulls, with no real-hull evidence; the missing comparison is described in <a href="#validation">Section 6</a>.</p>
<ul>
<li>The reference tank's natural period is {natural_period:.1f} s; the 18 s sweep starts at {record["cases"][0]["forcing_periods_s"][0]:.1f} s.
Every sampled forcing period below 20 s lies outside the published moment-fit band, including parts of the 21 s sweep; see <a href="#sensitivity-notes">Appendix A notes</a>.</li>
<li>The damping comparisons rescale forcing between damping groups and therefore change tank amplitude; see <a href="#damping-comparison">Appendix A damping comparison</a>.</li>
<li>Calibration transfer and the reference tank dimensions remain assumed; their basis is in <a href="#inputs">Table 3.1</a>.</li>
<li>These screening estimates exclude sea-state weighting, structural loads and design qualification; the assessed scope is in <a href="#introduction">Section 2</a>.</li>
<li>A near-resonance reduction does not remove the long-period penalty; see <a href="#penalty">Section 5.2</a>.</li>
</ul></section>
<section id="conclusions"><h2>8 Conclusions and next steps</h2>
<p>The peak reduction depends on the assumed hull damping and tank authority; the separate peak comparisons are shown in <a href="#comparison">Section 5</a> and <a href="#sensitivity">Appendix A</a>.
The near-resonance reduction does not remove the long-period penalty in <a href="#penalty">Section 5.2</a>.</p>
<p>Calibration transfer and the bare-roll reference remain ASSUMED.
The missing real-vessel tank-on and tank-off comparison in <a href="#validation">Section 6</a> remains the evidence gap for coupled hull reduction.</p>
</section>
<section id="references"><h2>9 References</h2>
<p>The public model basis and the calculation records identify the inputs, results and mathematical checks used in this report.</p>
<h3>9.1 Public references</h3>
<ul><li id="ref-public">[R1] Public U-tube moment correlation: 4.3 % RMS error for forcing periods of 20 s and longer; degradation below about 16 s has no established cause.
The published statement is retained in the <a href="#ref-model">model reference [I2]</a>; no independent coupled-hull validation is cited.</li></ul>
<h3>9.2 Data record and builder links</h3>
<ul>
<li id="ref-data">[I1] Model/data: <a href="data/generic_hull_roll_screening.json">{DATA}</a>.
Assumptions, units, cases, damping comparisons, numeric arrays and source digests.</li>
<li id="ref-model">[I2] Solver/program: public U-tube model; version not declared.
<a href="../../../{MODEL_PATH}">{MODEL_PATH}</a>. Git blob: {model_blob}.</li>
<li id="ref-study">[I3] Model context: <a href="dual-tank-sloshing-cfd-study.html">{PARENT_PAGE}</a>.
The illustrative tank differs from the reference tank used here.</li>
<li id="ref-builder">[I4] Calculation and figure builder: <a href="../../../{SCRIPT}">{SCRIPT}</a>.
{source_note()}</li>
<li id="ref-tests">[I5] Mathematical benchmark: <a href="../../../{TEST}">{TEST}</a>.
Hand-check inputs and rational oracle values are defined in test_undamped_two_degree_hand_check.</li>
</ul></section>
<section id="sensitivity"><h2>Appendix A Sensitivity matrix</h2>
<p>The full matrix retains every calculated hull-period, damping and tank-authority combination.
<a href="#sensitivity-table">Table A.1</a> gives the peak reduction and the coupled peak forcing period <a href="#ref-data">[I1]</a>.</p>
<table id="sensitivity-table"><thead><tr><th>Hull roll period (s)</th><th>Hull damping (% of critical)</th><th>Alpha (1)</th>
<th>Peak reduction (%)</th><th>Coupled peak forcing period (s)</th></tr></thead><tbody>{chr(10).join(rows)}</tbody>
<caption>Table A.1 – Peak reduction sensitivity matrix.</caption></table>
<p id="damping-comparison">In this case (18 s, alpha 0.05), hull damping roughly halves the benefit (ratio {ratio:.3f}).
The ratio of the 5 % benefit to the 2 % benefit ranges from {min(ratios):.2f} to {max(ratios):.2f} across the matrix.
For alpha 0.02, 0.05, 0.10, 0.15, 0.20 respectively, the ratios are {ratio_groups}.
These comparisons rescale forcing between damping groups and therefore change the tank amplitude <a href="#ref-data">[I1]</a>.</p>
<ol id="sensitivity-notes" class="notes">
<li>The wave-period chart shades sampled forcing periods below 20 s, outside the published moment-fit band.
The 18 s sweep starts at 12.6 s; the band also excludes parts of the 21 s sweep <a href="#ref-data">[I1]</a>, <a href="#ref-public">[R1]</a>.</li>
<li>All charts select existing calculated samples without interpolation or new cases.
CSV contains each plotted series point; hover or focus points for values and use checkboxes to toggle series.
The static SVG retains axes, units, legend and plotted samples for printing <a href="#ref-builder">[I4]</a>.</li>
<li>Reference tank geometry and calibration transfer are assumed; no actual hull geometry is supplied.
Hull outline and conduit height are symbolic; widths, depths and centroid offsets use the recorded dimensions <a href="#ref-data">[I1]</a>.</li>
</ol></section>
<section id="reproduce"><h2>Appendix B Reproduce commands</h2>
<p>The calculation and generated artifacts are reproduced with the following commands.</p>
<pre>python scripts/capabilities/build_generic_hull_screening.py
python -m pytest tests/hydrodynamics/test_generic_hull_screening.py</pre>
<p>The generator writes this page, the data record and the independent-oracle test from one script.
The tests check limiting cases, raw equilibrium residuals, frequency refinement and byte-identical regeneration of canonical numeric output.
JSON floats retain at most 12 significant digits; refined peak periods are rounded to 0.0001 s, and residuals are reported as conservative power-of-ten upper bounds.
Last-bit math perturbation tests check output stability; native platform runs are required to confirm each supported runtime.</p>
<p>AI generated the script, calculations, tests and draft text. Engineering checks and acceptance remain pending.
P.E. sign-off remains pending; no engineer signature is asserted.</p></section>
<details id="revisions"><summary>Appendix C Revision history</summary>
<table><thead><tr><th>Revision</th><th>Date</th><th>Change and reason</th></tr></thead><tbody>
<tr><td>Initial</td><td>2026-10-24</td><td>Generic hull screening estimate added to distinguish model predictions from real-hull evidence.</td></tr>
<tr><td>Clarification</td><td>{GENERATION_DATE}</td><td>Damping basis, assumptions and validity limits clarified; numeric output stabilized and unpublished comparison removed.</td></tr>
<tr><td>Interactive summary</td><td>{GENERATION_DATE}</td><td>Input-driven tank section, interactive charts and matching CSV exports added; calculated values retained and limits placed before method.</td></tr>
<tr><td>Report layout</td><td>{GENERATION_DATE}</td><td>Engineering reporting conventions P6, 13 October 2026, commit 08f081f46 applied to presentation: numbered sections, input table, dimensioned section, figure captions and appendices. Calculated values, claims and conclusions retained.</td></tr>
</tbody><caption>Table C.1 – Revision history.</caption></table></details>
<script>
function toggleSeries(chart, series, visible) {{
  document.querySelectorAll('#' + chart + ' g[data-series="' + series + '"]')
    .forEach(group => {{ group.hidden = !visible; group.toggleAttribute('hidden', !visible); }});
}}
</script>
<footer>Screening estimate only.</footer></body></html>
'''


def build(
    output_root: Path = ROOT, *, record: dict[str, Any] | None = None,
) -> None:
    """Write and read back the page, data and fixed independent-oracle tests."""
    output_root = Path(output_root).resolve()
    output_record: dict[str, Any] = canonical_numbers(compute() if record is None else record)
    page = render(output_record).encode("utf-8")
    tests = TEST_SOURCE.encode("utf-8")
    output_record = {**output_record, "provenance": {
        "generator_command": f"python {SCRIPT}",
        "input_sha256": {relative: hashlib.sha256((ROOT / relative).read_bytes()).hexdigest()
                         for relative in (SCRIPT, MODEL_PATH)},
        "output_sha256": {PAGE: hashlib.sha256(page).hexdigest(),
                          TEST: hashlib.sha256(tests).hexdigest()},
        "data_digest_scope": "Data bytes are checked by deterministic regeneration; no self digest",
    }}
    payloads = {PAGE: page, TEST: tests,
                DATA: (json.dumps(output_record, indent=2, allow_nan=False) + "\n").encode("utf-8")}
    for relative, payload in payloads.items():
        target = output_root / relative
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_bytes(payload)
        if target.read_bytes() != payload:
            raise OSError("Generated artifact readback failed")


def main() -> None:
    """Generate the three declared artifacts in the selected output tree."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-root", type=Path, default=ROOT)
    args = parser.parse_args()
    start = perf_counter()
    build(args.output_root)
    print(f"Generated three screening artifacts in {perf_counter() - start:.2f} s")


TEST_SOURCE = r'''# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent checks generated by the generic hull screening script."""

import hashlib
import importlib.util
import json
import math
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts/capabilities/build_generic_hull_screening.py"


@pytest.fixture(scope="module")
def builder():
    spec = importlib.util.spec_from_file_location("generic_screening", SCRIPT)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


@pytest.fixture(scope="module")
def record(builder):
    return builder.compute()


def test_zero_authority_has_zero_reduction(builder):
    result = builder.screen_case(18.0, 0.02, 0.0)
    assert result["reduction_percent"] == pytest.approx(0.0, abs=1e-9)
    assert result["tank_peak_period_s"] == pytest.approx(result["bare_peak_period_s"])


def test_undamped_two_degree_hand_check(builder):
    """Unit-density/gravity box: Kfixed=29/6, Ktransfer=4, J=19/2.

    At omega=1/2 rad/s and hull omega=1 rad/s, H=359/24.
    Kstatic=53/6; alpha=1/10 gives roll/moment=2120/1231.
    The characteristic polynomial is 946*x*x-1529*x+477.
    These rational values come from prismatic integrals, not model output.
    """
    model = builder.MODEL
    geometry = model.UTubeGeometry(2.0, 1.0, 1.0, 1.0, 1.0)
    moment = model.tank_moment(
        geometry=geometry, conduit_area=1.0, forcing_period=4 * math.pi,
        roll_amplitude=1e-7, density=1.0, gravity=1.0,
        loss_coefficient=1e-20,
    )
    assert moment.in_phase / 1e-7 == pytest.approx(359 / 24, rel=1e-10)
    assert abs(moment.quadrature / 1e-7) < 1e-10
    response = model.coupled_roll_response(
        forcing_period=4 * math.pi, vessel_roll_period=2 * math.pi,
        hull_damping_ratio=0.0, tank_authority=0.1,
        geometry=geometry, conduit_area=1.0, density=1.0, gravity=1.0,
        loss_coefficient=1e-20,
    )
    assert response == pytest.approx(2120 / 1231, rel=1e-10)
    for sign in (-1, 1):
        x = (1529 + sign * math.sqrt(532873)) / 1892
        period = 2 * math.pi / math.sqrt(x)
        h = model.tank_moment(
            geometry=geometry, conduit_area=1.0, forcing_period=period,
            roll_amplitude=1e-7, density=1.0, gravity=1.0,
            loss_coefficient=1e-20,
        ).in_phase / 1e-7
        assert 1 - x - 0.1 * h / (53 / 6) == pytest.approx(0, abs=1e-10)


def test_long_period_free_surface_limit(builder):
    args = dict(vessel_roll_period=18.0, hull_damping_ratio=0.02,
                tank_authority=0.05, geometry=builder.GEOMETRY,
                conduit_area=builder.CONDUIT_AREA)
    bare = abs(builder.MODEL.coupled_roll_response(
        forcing_period=1e6, tank_enabled=False, **args))
    tank = abs(builder.MODEL.coupled_roll_response(forcing_period=1e6, **args))
    assert tank / bare == pytest.approx(1 / 0.95, rel=1e-5)


@pytest.mark.parametrize("period,damping,alpha", [
    (18.0, 0.02, 0.02), (18.0, 0.02, 0.05), (18.0, 0.02, 0.20),
    (21.0, 0.05, 0.02), (21.0, 0.05, 0.05), (21.0, 0.05, 0.20),
])
def test_peak_refinement_and_bare_closed_form(builder, period, damping, alpha):
    coarse = builder.screen_case(period, damping, alpha)
    fine = builder.screen_case(period, damping, alpha, samples=241)
    assert coarse["bare_peak_rao"] == pytest.approx(
        1 / (2 * damping * math.sqrt(1 - damping**2)), rel=1e-9)
    assert coarse["bare_peak_period_s"] == pytest.approx(
        period / math.sqrt(1 - 2 * damping**2), rel=1e-7)
    assert abs(coarse["reduction_percent"] - fine["reduction_percent"]) < 0.1
    assert abs(coarse["tank_peak_period_s"] - fine["tank_peak_period_s"]) < 0.01


def test_unconverged_response_is_rejected(builder, monkeypatch):
    original = builder.MODEL.coupled_roll_response

    def perturbed(**kwargs):
        return 1.01 * original(**kwargs)

    monkeypatch.setattr(builder.MODEL, "coupled_roll_response", perturbed)
    with pytest.raises(ValueError, match="residual"):
        builder.screen_case(18, .02, .05)


def test_table_basis_and_damping_comparison(record):
    assert len(record["cases"]) == 20
    assert {row["roll_period_s"] for row in record["cases"]} == {18.0, 21.0}
    assert {row["hull_damping_ratio"] for row in record["cases"]} == {0.02, 0.05}
    assert {row["alpha"] for row in record["cases"]} == {0.02, 0.05, 0.1, 0.15, 0.2}
    for row in record["cases"]:
        expected = 100 * (1 - row["tank_peak_rao"] / row["bare_peak_rao"])
        assert row["reduction_percent"] == pytest.approx(expected)
        assert row["max_relative_residual_bound"] <= 1e-5
        assert len(row["forcing_periods_s"]) == len(row["tank_rao"]) == 121
    assert "quote_checks" not in record


@pytest.mark.parametrize("direction", [-math.inf, math.inf])
def test_numeric_serialization_ignores_last_bit_math_drift(
    builder, record, tmp_path, monkeypatch, direction,
):
    """Exercise optimization and serialization with perturbed libm results."""
    builder.build(tmp_path / "reference", record=record)
    for name in ("sin", "tan", "hypot"):
        original = getattr(math, name)

        def perturbed(*args, original=original):
            return math.nextafter(original(*args), direction)

        monkeypatch.setattr(math, name, perturbed)
    builder.build(tmp_path / "perturbed")
    for relative in (builder.PAGE, builder.DATA, builder.TEST):
        assert ((tmp_path / "reference" / relative).read_bytes()
                == (tmp_path / "perturbed" / relative).read_bytes())


def test_saved_floats_have_at_most_twelve_significant_digits(builder):
    def check(value):
        if isinstance(value, float):
            assert value == float(format(value, ".12g"))
        elif isinstance(value, dict):
            for child in value.values():
                check(child)
        elif isinstance(value, list):
            for child in value:
                check(child)

    check(json.loads((ROOT / builder.DATA).read_text(encoding="utf-8")))


def test_invalid_screen_inputs_are_rejected(builder):
    for values in [(0, .02, .05), (18, 0, .05), (18, .02, -1),
                   (math.nan, .02, .05), (18, math.inf, .05), (18, .02, 1)]:
        with pytest.raises(ValueError):
            builder.screen_case(*values)
    with pytest.raises(ValueError):
        builder.screen_case(18, .02, .05, samples=2)


def test_generator_reproduces_all_three_outputs(builder, record, tmp_path):
    builder.build(tmp_path, record=record)
    for relative in (builder.PAGE, builder.DATA, builder.TEST):
        assert (tmp_path / relative).read_bytes() == (ROOT / relative).read_bytes()
    saved = json.loads((tmp_path / builder.DATA).read_text())
    for relative, digest in saved["provenance"]["output_sha256"].items():
        assert hashlib.sha256((tmp_path / relative).read_bytes()).hexdigest() == digest
    for relative, digest in saved["provenance"]["input_sha256"].items():
        assert hashlib.sha256((ROOT / relative).read_bytes()).hexdigest() == digest



def test_build_without_existing_outputs(builder, tmp_path, monkeypatch):
    """Freshness checks remove outputs before running a registered generator."""
    import shutil

    source = tmp_path / "source"
    for relative in (builder.SCRIPT, builder.MODEL_PATH):
        target = source / relative
        target.parent.mkdir(parents=True, exist_ok=True)
        shutil.copyfile(ROOT / relative, target)
    monkeypatch.setattr(builder, "ROOT", source)
    builder.build(tmp_path / "output")
    for relative in (builder.PAGE, builder.DATA, builder.TEST):
        assert (tmp_path / "output" / relative).read_bytes() == (ROOT / relative).read_bytes()


def test_published_reductions_are_preserved(record):
    rows = [row for row in record["cases"]
            if row["roll_period_s"] == 18 and row["alpha"] == .05]
    assert [round(row["reduction_percent"], 1) for row in rows] == [46.1, 23.3]


def test_calculation_record_is_unchanged(builder):
    """Pin the complete numeric/data basis of the presentation baseline."""
    saved = json.loads((ROOT / builder.DATA).read_text(encoding="utf-8"))
    saved.pop("provenance")
    digest = hashlib.sha256(json.dumps(saved, sort_keys=True).encode()).hexdigest()
    assert digest == "a41b54eb0653e046d90d15db80f30cfc6bde47faef7e76da4e0144186a4e628c"


def test_schematic_uses_record_dimensions(builder, record):
    import copy
    from xml.etree import ElementTree

    svg = builder.schematic(record)
    ElementTree.fromstring(svg)
    for field in ("leg_length_m", "leg_width_m", "fill_depth_m",
                  "centroid_offset_m", "conduit_area_m2"):
        altered = copy.deepcopy(record)
        altered["assumptions"][field] += 1
        assert builder.schematic(altered) != svg
        assert f'{altered["assumptions"][field]:.1f}' in builder.schematic(altered)
    assert 'U-tube anti-roll tank' in svg
    assert 'Hull section' in svg


@pytest.mark.parametrize("index", [0, 1, 2])
def test_chart_points_and_csv_have_identical_values(builder, record, index):
    import csv
    import io
    from html.parser import HTMLParser
    from urllib.parse import unquote

    class Chart(HTMLParser):
        def __init__(self):
            super().__init__()
            self.points, self.exports, self.controls = [], [], []

        def handle_starttag(self, tag, attrs):
            attrs = dict(attrs)
            if tag == "circle":
                self.points.append((int(attrs["data-series"]),
                                    float(attrs["data-x"]), float(attrs["data-y"])))
            if tag == "a" and "download" in attrs:
                self.exports.append(attrs["href"])
            if tag == "input" and attrs.get("type") == "checkbox":
                self.controls.append(attrs)

    chart = builder.chart_specs(record)[index]
    parsed = Chart()
    rendered = builder.interactive_chart(*chart)
    parsed.feed(rendered)
    identifier, title, xs, series, xlabel, ylabel = chart
    expected = [(i, x, y) for i, (_, ys) in enumerate(series) for x, y in zip(xs, ys)]
    assert parsed.points == expected
    assert len(parsed.exports) == 1
    rows = list(csv.reader(io.StringIO(unquote(parsed.exports[0].split(",", 1)[1]))))
    assert rows[0] == ["series", xlabel, ylabel]
    assert rows[1:] == [[series[i][0], str(x), str(y)] for i, x, y in expected]
    assert len(parsed.controls) == len(series)
    assert rendered.count("<title>") == len(expected)
    assert "onchange=" in rendered and "toggleAttribute('hidden'" in builder.render(record)
    assert 'href="#ref-data"' in rendered
    if index == 0:
        assert xs == [100 * row["hull_damping_ratio"] for row in record["cases"]
                      if row["roll_period_s"] == 18 and row["alpha"] == .05]
    elif index == 1:
        row = next(row for row in record["cases"] if row["roll_period_s"] == 18
                   and row["alpha"] == .05 and row["hull_damping_ratio"] == .02)
        assert xs == row["forcing_periods_s"]
        assert series[0][1] == row["bare_rao"]
        assert series[1][1] == row["tank_rao"]
    else:
        assert xs == list(builder.ALPHAS)


def test_page_section_order_and_collapsed_revisions(builder, record):
    from html.parser import HTMLParser

    class Headings(HTMLParser):
        def __init__(self):
            super().__init__()
            self.headings, self.current = [], None

        def handle_starttag(self, tag, attrs):
            if tag == "h2":
                self.current = ""

        def handle_data(self, data):
            if self.current is not None:
                self.current += data

        def handle_endtag(self, tag):
            if tag == "h2":
                self.headings.append(self.current)
                self.current = None

    page = builder.render(record)
    parsed = Headings()
    parsed.feed(page)
    assert parsed.headings == [
        "1 Executive summary", "2 Introduction", "3 Design data",
        "4 Analysis methodology", "5 Results", "6 Validation and benchmark",
        "7 Limitations", "8 Conclusions and next steps", "9 References",
        "Appendix A Sensitivity matrix", "Appendix B Reproduce commands",
    ]
    markers = ['id="summary"', 'id="introduction"', 'id="design-data"',
               'id="hull-section"', 'id="method"', 'id="comparison"',
               'id="damping"', 'id="response"', 'id="alpha"', 'id="validation"',
               'id="limits"', 'id="conclusions"', 'id="references"',
               'id="sensitivity"', 'id="reproduce"', '<details id="revisions">']
    positions = [page.index(marker) for marker in markers]
    assert positions == sorted(positions)
    assert '<details id="revisions" open' not in page
    assert '<summary>Appendix C Revision history</summary>' in page
    assert '46.1 % at 2 % hull damping and 23.3 % at 5 %' in page
    assert ': screening estimate</h' not in page
    assert '<h2>Left</h2>' not in page


def test_report_traceability_and_print_contract(builder, record):
    from html.parser import HTMLParser
    from xml.etree import ElementTree

    class Report(HTMLParser):
        def __init__(self):
            super().__init__()
            self.ids, self.targets, self.locators = [], [], []
            self.in_references = False
            self.captions, self.caption = [], None

        def handle_starttag(self, tag, attrs):
            attrs = dict(attrs)
            if attrs.get("id") == "references":
                self.in_references = True
            if tag == "section" and attrs.get("id") == "sensitivity":
                self.in_references = False
            if "id" in attrs:
                self.ids.append(attrs["id"])
            href = attrs.get("href", "")
            if href.startswith("#"):
                self.targets.append(href[1:])
            if href.startswith(("../../../", "data/", "dual-tank")):
                self.locators.append(self.in_references)
            if tag in ("caption", "figcaption"):
                self.caption = ""

        def handle_data(self, data):
            if self.caption is not None:
                self.caption += data

        def handle_endtag(self, tag):
            if tag in ("caption", "figcaption"):
                self.captions.append(self.caption)
                self.caption = None

    page = builder.render(record)
    parsed = Report()
    parsed.feed(page)
    assert len(parsed.ids) == len(set(parsed.ids))
    assert set(parsed.targets) <= set(parsed.ids)
    assert parsed.locators and all(parsed.locators)
    assert all(caption.startswith(("Figure ", "Table ")) for caption in parsed.captions)
    assert 'Table A.1' in page and 'Figure 3.1' in page and 'Figure 5.3' in page
    assert '08f081f46' in page and 'P6' in page
    assert '@media print' in page and '.chart text' in page
    assert 'r="2.5"' in page and 'font-size="12"' in page
    svg = ElementTree.fromstring(builder.schematic(record))
    assert len(svg.findall('.//marker')) >= 1
    assert 'marker-start=' in builder.schematic(record)
    summary = page.split('<section id="summary">')[1].split('</section>')[0]
    assert 3 <= summary.count('<li>') <= 5
    numbered = page.split('<section id="summary">')[1].split('<section id="sensitivity">')[0]
    import re
    assert not re.search(r'\bline \d+', numbered.split('<section id="references">')[0])


def test_page_wording_links_and_preservation(builder, record):
    from html.parser import HTMLParser
    from urllib.parse import urlsplit

    class Links(HTMLParser):
        def __init__(self):
            super().__init__()
            self.links = []

        def handle_starttag(self, tag, attrs):
            self.links.extend(value for key, value in attrs if key == "href")

    text = builder.render(record)
    assert '<title>Generic hull coupled roll screening estimate</title>' in text
    assert 'Generic hull roll-plus-tank screening estimate' in text
    assert '18 s sweep starts at 12.6 s' in text
    assert 'No tank-on and tank-off real-vessel comparison' in text
    assert 'P.E. sign-off remains pending' in text
    assert 'AI generated' in text
    assert 'Revision history' in text
    assert 'quote' not in text.lower()
    assert '46.1 % at 2 % hull damping and 23.3 % at 5 %' in text
    assert 'conduit area A_c = 6.8 m²' in text
    assert '120 m² in plan' in text
    assert 'Ks also includes water redistribution between the legs' in text
    assert 'Hull damping (% of critical)' in text
    assert '4.3 % RMS error' in text
    assert 'below about 16 s' in text
    assert '14.3 s' in text
    assert 'ratio 0.505' in text
    assert '0.43 to 0.72' in text
    assert '6.0 % more roll' in text
    assert '29.5 %' in text
    assert '25.0 % limit' in text
    assert text.count('ASSUMED - to be confirmed') >= 3
    assert "checks arithmetic and the coupling denominator" in text
    assert "not the physics of the moment model" in text
    assert 'Mon 26 Oct' not in text
    assert 'Questions:' not in text
    assert '\u2014' not in text
    for folder in ("home", "mnt"):
        assert "/" + folder + "/" not in text
    links = Links()
    links.feed(text)
    page = ROOT / builder.PAGE
    for href in links.links:
        url = urlsplit(href)
        if not url.scheme:
            assert (page.parent / url.path if url.path else page).is_file(), href
    parent = (ROOT / builder.PARENT_PAGE).read_text()
    assert f'href="{Path(builder.PAGE).name}"' in parent
'''


if __name__ == "__main__":
    main()
