#!/usr/bin/env python3
"""ABOUTME: Self-contained HTML engineering-report layer for the OpenFOAM sloshing
gravity-conduit surrogate study (#1528).

Renders two report kinds from plain result shapes (dicts or dataclasses):

* a **case-level report** for one CFD case (elevation, pressure, mass,
  centre-of-mass and inlet/outlet-flow traces plus validation), and
* an **aggregate matrix report** over the fill-fraction x T_roll/T_natural sweep
  (heatmaps + a full case matrix).

Every report is a single, offline HTML document: all plots are hand-built inline
SVG (no matplotlib, no external CSS/JS/font/CDN), so the file renders with no
network access. Output is de-identified - client names, field IDs and absolute
machine paths are scrubbed before rendering. Invalid / failed cases are never
silently dropped; they appear explicitly flagged.

The public functions accept the sibling schemas by structural shape so they drop
onto the real ``sloshing_sweep.SweepCase`` / ``time_history.SynchronizedTimeHistory``
objects once those land on ``main``.
"""

from __future__ import annotations

import html as _html
import re
from typing import Any, Dict, Iterable, List, Mapping, Optional, Sequence, Tuple

from loguru import logger

REQUIRED_SECTIONS: Tuple[str, ...] = (
    "Inputs",
    "Methodology",
    "Visualizations",
    "Validation",
    "Outputs",
    "Limitations",
)

_MISSING = object()

# Absolute-path scrubbing: unix paths with >=2 components, or windows drive paths.
_UNIX_PATH_RE = re.compile(r"(?:/[\w.+\-]+){2,}/?")
_WIN_PATH_RE = re.compile(r"[A-Za-z]:\\[\w.+\\\-]*")


# ============================================================================
# Structural accessors (dict OR dataclass/object)
# ============================================================================


def _get(obj: Any, key: str, default: Any = _MISSING) -> Any:
    """Read ``key`` from a mapping or an object attribute.

    Args:
        obj: A mapping or an object.
        key: Field / attribute name.
        default: Returned when absent; if omitted a ValueError is raised.

    Returns:
        The looked-up value.

    Raises:
        ValueError: When the key is absent and no default was supplied.
    """
    if isinstance(obj, Mapping):
        if key in obj:
            return obj[key]
    elif hasattr(obj, key):
        return getattr(obj, key)
    if default is _MISSING:
        raise ValueError(f"required field '{key}' missing from result shape")
    return default


def _channel_values(channels: Mapping[str, Any], name: str) -> List[float]:
    """Return numeric values for a named channel, raising if it is absent."""
    if name not in channels:
        raise ValueError(
            f"required channel '{name}' missing; available: "
            f"{sorted(channels.keys())}"
        )
    ch = channels[name]
    values = _get(ch, "values")
    return [float(v) for v in values]


def _channel_units(channels: Mapping[str, Any], name: str) -> str:
    ch = channels.get(name, {})
    return str(_get(ch, "units", "")) if ch else ""


# ============================================================================
# De-identification
# ============================================================================


def _scrub(text: Any) -> str:
    """Strip absolute machine paths (unix + windows) down to their basename."""
    s = str(text)
    s = _UNIX_PATH_RE.sub(lambda m: m.group(0).rstrip("/").split("/")[-1] or "path", s)
    s = _WIN_PATH_RE.sub(
        lambda m: m.group(0).replace("\\", "/").rstrip("/").split("/")[-1] or "path",
        s,
    )
    return s


def _esc(text: Any) -> str:
    """HTML-escape after path-scrubbing."""
    return _html.escape(_scrub(text))


# ============================================================================
# Inline SVG primitives (self-contained, no xmlns -> no http reference)
# ============================================================================

_SERIES_COLORS = ("#1f6feb", "#d1242f", "#1a7f37", "#9a6700", "#8250df")


def _num_fmt(v: float) -> str:
    av = abs(v)
    if v == 0:
        return "0"
    if av >= 1000 or av < 0.01:
        return f"{v:.2e}"
    return f"{v:.3g}"


def _line_plot_svg(
    t: Sequence[float],
    series: Sequence[Dict[str, Any]],
    title: str,
    ylabel: str,
    width: int = 540,
    height: int = 220,
) -> str:
    """Build a self-contained inline-SVG line plot.

    Args:
        t: Shared x-axis (time) values.
        series: List of ``{"label", "values", "color"}`` traces.
        title: Plot title (rendered as a caption above the SVG).
        ylabel: Y-axis label.
        width: SVG width in px.
        height: SVG height in px.

    Returns:
        An ``<figure>`` containing the caption and inline ``<svg>``.
    """
    ml, mr, mt, mb = 58, 14, 12, 34
    pw, ph = width - ml - mr, height - mt - mb
    xs = [float(x) for x in t]
    x_lo, x_hi = (min(xs), max(xs)) if xs else (0.0, 1.0)
    if x_hi == x_lo:
        x_hi = x_lo + 1.0
    all_y: List[float] = []
    for s in series:
        all_y.extend(float(v) for v in s["values"])
    y_lo, y_hi = (min(all_y), max(all_y)) if all_y else (0.0, 1.0)
    if y_hi == y_lo:
        pad = abs(y_hi) * 0.1 or 1.0
        y_lo, y_hi = y_lo - pad, y_hi + pad

    def px(x: float) -> float:
        return ml + (x - x_lo) / (x_hi - x_lo) * pw

    def py(y: float) -> float:
        return mt + ph - (y - y_lo) / (y_hi - y_lo) * ph

    parts: List[str] = []
    parts.append(
        f'<svg class="lineplot" role="img" width="{width}" height="{height}" '
        f'viewBox="0 0 {width} {height}">'
    )
    # plot frame
    parts.append(
        f'<rect x="{ml}" y="{mt}" width="{pw}" height="{ph}" '
        f'fill="none" stroke="#c9d1d9"/>'
    )
    # gridlines + y ticks
    for i in range(5):
        yv = y_lo + (y_hi - y_lo) * i / 4
        yy = py(yv)
        parts.append(
            f'<line x1="{ml}" y1="{yy:.1f}" x2="{ml + pw}" y2="{yy:.1f}" '
            f'stroke="#eaeef2"/>'
        )
        parts.append(
            f'<text x="{ml - 6}" y="{yy + 3:.1f}" text-anchor="end" '
            f'font-size="9" fill="#57606a">{_num_fmt(yv)}</text>'
        )
    # x ticks
    for i in range(5):
        xv = x_lo + (x_hi - x_lo) * i / 4
        xx = px(xv)
        parts.append(
            f'<text x="{xx:.1f}" y="{mt + ph + 14}" text-anchor="middle" '
            f'font-size="9" fill="#57606a">{_num_fmt(xv)}</text>'
        )
    # series polylines
    for idx, s in enumerate(series):
        color = s.get("color") or _SERIES_COLORS[idx % len(_SERIES_COLORS)]
        pts = " ".join(
            f"{px(xs[j]):.1f},{py(float(s['values'][j])):.1f}"
            for j in range(min(len(xs), len(s["values"])))
        )
        parts.append(
            f'<polyline fill="none" stroke="{color}" stroke-width="1.8" '
            f'points="{pts}"/>'
        )
    # legend
    lx = ml + 6
    for idx, s in enumerate(series):
        color = s.get("color") or _SERIES_COLORS[idx % len(_SERIES_COLORS)]
        parts.append(
            f'<rect x="{lx}" y="{mt + 4}" width="10" height="6" fill="{color}"/>'
        )
        label = _esc(s["label"])
        parts.append(
            f'<text x="{lx + 14}" y="{mt + 10}" font-size="9" '
            f'fill="#24292f">{label}</text>'
        )
        lx += 16 + 7 * len(str(s["label"]))
    # axis titles
    parts.append(
        f'<text x="{ml + pw / 2:.0f}" y="{height - 4}" text-anchor="middle" '
        f'font-size="10" fill="#24292f">time (s)</text>'
    )
    parts.append(
        f'<text x="12" y="{mt + ph / 2:.0f}" text-anchor="middle" font-size="10" '
        f'fill="#24292f" transform="rotate(-90 12 {mt + ph / 2:.0f})">'
        f"{_esc(ylabel)}</text>"
    )
    parts.append("</svg>")
    return (
        f'<figure class="plot"><figcaption>{_esc(title)}</figcaption>'
        + "".join(parts)
        + "</figure>"
    )


def _color_scale(t: float) -> str:
    """Blue->yellow->red diverging colour for normalised ``t`` in [0, 1]."""
    t = 0.0 if t < 0 else 1.0 if t > 1 else t
    if t < 0.5:
        f = t / 0.5
        r = int(44 + (255 - 44) * f)
        g = int(123 + (255 - 123) * f)
        b = int(182 + (191 - 182) * f)
    else:
        f = (t - 0.5) / 0.5
        r = int(255 + (215 - 255) * f)
        g = int(255 + (25 - 255) * f)
        b = int(191 + (28 - 191) * f)
    return f"rgb({r},{g},{b})"


def _heatmap_svg(
    row_labels: Sequence[str],
    col_labels: Sequence[str],
    matrix: Sequence[Sequence[Optional[float]]],
    title: str,
    x_axis_title: str,
    y_axis_title: str,
) -> str:
    """Build a self-contained inline-SVG heatmap.

    ``None`` cells (missing / invalid) are drawn hatched-grey with an X marker so
    they are visible rather than omitted.
    """
    cell = 46
    ml, mt = 90, 40
    rows, cols = len(row_labels), len(col_labels)
    width = ml + cols * cell + 20
    height = mt + rows * cell + 46
    flat = [v for r in matrix for v in r if v is not None]
    vmin, vmax = (min(flat), max(flat)) if flat else (0.0, 1.0)
    if vmax == vmin:
        vmax = vmin + 1.0
    parts: List[str] = [
        f'<svg class="heatmap" role="img" width="{width}" height="{height}" '
        f'viewBox="0 0 {width} {height}">'
    ]
    for i in range(rows):
        for j in range(cols):
            x = ml + j * cell
            y = mt + i * cell
            v = matrix[i][j]
            if v is None:
                parts.append(
                    f'<rect x="{x}" y="{y}" width="{cell}" height="{cell}" '
                    f'fill="#e1e4e8" stroke="#fff"/>'
                )
                parts.append(
                    f'<text x="{x + cell / 2:.0f}" y="{y + cell / 2 + 4:.0f}" '
                    f'text-anchor="middle" font-size="12" fill="#57606a">n/a</text>'
                )
                continue
            color = _color_scale((v - vmin) / (vmax - vmin))
            parts.append(
                f'<rect x="{x}" y="{y}" width="{cell}" height="{cell}" '
                f'fill="{color}" stroke="#fff"/>'
            )
            parts.append(
                f'<text x="{x + cell / 2:.0f}" y="{y + cell / 2 + 4:.0f}" '
                f'text-anchor="middle" font-size="9" fill="#0b1015">'
                f"{_num_fmt(v)}</text>"
            )
    # row labels (y axis)
    for i, lab in enumerate(row_labels):
        y = mt + i * cell + cell / 2 + 3
        parts.append(
            f'<text x="{ml - 8}" y="{y:.0f}" text-anchor="end" font-size="10" '
            f'fill="#24292f">{_esc(lab)}</text>'
        )
    # col labels (x axis)
    for j, lab in enumerate(col_labels):
        x = ml + j * cell + cell / 2
        parts.append(
            f'<text x="{x:.0f}" y="{mt - 8}" text-anchor="middle" font-size="10" '
            f'fill="#24292f">{_esc(lab)}</text>'
        )
    parts.append(
        f'<text x="{ml + cols * cell / 2:.0f}" y="{height - 6}" '
        f'text-anchor="middle" font-size="11" fill="#24292f">'
        f"{_esc(x_axis_title)}</text>"
    )
    parts.append(
        f'<text x="14" y="{mt + rows * cell / 2:.0f}" text-anchor="middle" '
        f'font-size="11" fill="#24292f" '
        f'transform="rotate(-90 14 {mt + rows * cell / 2:.0f})">'
        f"{_esc(y_axis_title)}</text>"
    )
    parts.append("</svg>")
    return (
        f'<figure class="plot"><figcaption>{_esc(title)}</figcaption>'
        + "".join(parts)
        + "</figure>"
    )


# ============================================================================
# Shared HTML shell + validity logic
# ============================================================================

_STYLE = """
body{font-family:-apple-system,Segoe UI,Roboto,Helvetica,Arial,sans-serif;
color:#24292f;margin:0;padding:0 24px 48px;line-height:1.5;background:#fff}
h1{font-size:22px;margin:20px 0 4px}h2{font-size:16px;border-bottom:1px solid #d0d7de;
padding-bottom:4px;margin-top:32px}nav a{margin-right:12px;font-size:12px}
table{border-collapse:collapse;font-size:12px;margin:8px 0}
th,td{border:1px solid #d0d7de;padding:4px 8px;text-align:right}
th{background:#f6f8fa}td.k{text-align:left}
tr.invalid{background:#ffebe9}.badge{font-weight:700;padding:1px 6px;border-radius:6px}
.badge.pass{background:#dafbe1;color:#116329}.badge.fail{background:#ffebe9;color:#cf222e}
figure.plot{display:inline-block;margin:8px 12px 8px 0;vertical-align:top}
figcaption{font-size:12px;font-weight:600;margin-bottom:2px}
.meta{color:#57606a;font-size:12px}ul{font-size:13px}
""".strip()


def _shell(title: str, subtitle: str, sections_html: str) -> str:
    nav = " ".join(
        f'<a href="#{s.lower()}">{s}</a>' for s in REQUIRED_SECTIONS
    )
    doc = (
        "<!doctype html>\n"
        '<html lang="en"><head><meta charset="utf-8">'
        '<meta name="viewport" content="width=device-width,initial-scale=1">'
        f"<title>{_esc(title)}</title><style>{_STYLE}</style></head><body>"
        f"<h1>{_esc(title)}</h1>"
        f'<p class="meta">{_esc(subtitle)}</p>'
        f"<nav>{nav}</nav>"
        f"{sections_html}"
        "</body></html>"
    )
    # Safety net: scrub any absolute path that slipped through free-text fields.
    return _scrub(doc)


def _section(name: str, body: str) -> str:
    return f'<section id="{name.lower()}"><h2>{name}</h2>{body}</section>'


def _is_valid(validation: Mapping[str, Any]) -> bool:
    """A case is valid when mass balance holds and no failure event fired."""
    return bool(
        _get(validation, "mass_balance_ok", True)
        and not _get(validation, "dry_out", False)
        and not _get(validation, "overflow", False)
        and not _get(validation, "impact", False)
    )


def _flag_reasons(validation: Mapping[str, Any]) -> List[str]:
    reasons = []
    if not _get(validation, "mass_balance_ok", True):
        reasons.append("mass-balance fail")
    if _get(validation, "dry_out", False):
        reasons.append("dry-out")
    if _get(validation, "overflow", False):
        reasons.append("overflow")
    if _get(validation, "impact", False):
        reasons.append("impact")
    return reasons


# ============================================================================
# Case-level report
# ============================================================================


def render_case_report(history: Any, case: Any = None) -> str:
    """Render a self-contained HTML report for a single CFD case.

    Args:
        history: A ``SynchronizedTimeHistory``-shaped object/mapping with
            ``case_id, time, channels, validation, metadata``.
        case: Optional ``SweepCase``-shaped object/mapping with the case inputs.

    Returns:
        A complete offline HTML document (str).

    Raises:
        ValueError: When a required channel is absent from the history.
    """
    case_id = str(_get(history, "case_id"))
    logger.info("Rendering case report for {}", case_id)
    t = [float(x) for x in _get(history, "time")]
    channels = _get(history, "channels")
    validation = _get(history, "validation", {})
    metadata = _get(history, "metadata", {})

    # Required channels (raise, never silently skip).
    elevation = _channel_values(channels, "free_surface_elevation")
    pressure = _channel_values(channels, "wall_pressure")
    mass = _channel_values(channels, "liquid_mass")
    inlet = _channel_values(channels, "inlet_flow_rate")
    outlet = _channel_values(channels, "outlet_flow_rate")
    valid = _is_valid(validation)
    reasons = _flag_reasons(validation)

    # ---- Inputs ----
    if case is not None:
        rows = [
            ("Case ID", _esc(_get(case, "case_id", case_id))),
            ("Fill fraction", _num_fmt(float(_get(case, "fill_fraction", 0.0)))),
            ("Fill depth (m)", _num_fmt(float(_get(case, "fill_depth", 0.0)))),
            ("Conduit capacity", _num_fmt(float(_get(case, "conduit_capacity", 0.0)))),
            ("Period ratio T_roll/T_natural",
             _num_fmt(float(_get(case, "period_ratio", 0.0)))),
            ("Natural frequency (Hz)",
             _num_fmt(float(_get(case, "natural_frequency_hz", 0.0)))),
            ("Natural period (s)",
             _num_fmt(float(_get(case, "natural_period_s", 0.0)))),
            ("Roll frequency (Hz)",
             _num_fmt(float(_get(case, "roll_frequency_hz", 0.0)))),
            ("Roll period (s)", _num_fmt(float(_get(case, "roll_period_s", 0.0)))),
            ("Near resonance", "yes" if _get(case, "near_resonance", False) else "no"),
        ]
    else:
        rows = [("Case ID", _esc(case_id))]
    inputs_tbl = "".join(
        f'<tr><td class="k">{k}</td><td>{v}</td></tr>' for k, v in rows
    )
    inputs = _section(
        "Inputs",
        "<p>Governing inputs for this case (gravity-conduit sloshing surrogate).</p>"
        f"<table>{inputs_tbl}</table>",
    )

    # ---- Methodology ----
    methodology = _section(
        "Methodology",
        "<ul>"
        "<li>Roll excitation is imposed on a partially filled prismatic tank; the "
        "internal free surface is modelled with a reduced-order gravity-conduit "
        "surrogate calibrated to the CFD (VOF) response.</li>"
        "<li>Channels are extracted per timestep and synchronized onto a common "
        "time base; a mass-balance residual check guards conservation.</li>"
        "<li>Dry-out, overflow and wall-impact events are detected and reported.</li>"
        "</ul>",
    )

    # ---- Visualizations ----
    plots = [
        _line_plot_svg(
            t,
            [
                {"label": "eta", "values": elevation},
            ]
            + (
                [{"label": "eta_max", "values": _channel_values(channels, "elevation_max")}]
                if "elevation_max" in channels
                else []
            ),
            "Free-Surface Elevation",
            f"elevation ({_channel_units(channels, 'free_surface_elevation') or 'm'})",
        ),
        _line_plot_svg(
            t,
            [{"label": "p_wall", "values": pressure}],
            "Wall Pressure",
            f"pressure ({_channel_units(channels, 'wall_pressure') or 'Pa'})",
        ),
        _line_plot_svg(
            t,
            [{"label": "mass", "values": mass}],
            "Liquid Mass",
            f"mass ({_channel_units(channels, 'liquid_mass') or 'kg'})",
        ),
        _line_plot_svg(
            t,
            [
                {"label": lbl, "values": _channel_values(channels, ch)}
                for lbl, ch in (("x", "com_x"), ("y", "com_y"), ("z", "com_z"))
                if ch in channels
            ],
            "Center of Mass",
            "position (m)",
        ),
        _line_plot_svg(
            t,
            [
                {"label": "inlet", "values": inlet},
                {"label": "outlet", "values": outlet},
            ],
            "Inlet / Outlet Flow",
            f"flow rate ({_channel_units(channels, 'inlet_flow_rate') or 'm^3/s'})",
        ),
    ]
    visualizations = _section(
        "Visualizations",
        "<p>All plots are inline SVG and render offline.</p>" + "".join(plots),
    )

    # ---- Validation ----
    badge = (
        '<span class="badge pass">PASS - valid</span>'
        if valid
        else '<span class="badge fail">INVALID - flagged: '
        + _esc(", ".join(reasons) or "failed")
        + "</span>"
    )
    val_rows = [
        ("Overall status", badge),
        ("Mass balance residual",
         _num_fmt(float(_get(validation, "mass_balance_residual", float("nan"))))),
        ("Mass balance OK",
         "yes" if _get(validation, "mass_balance_ok", True) else "no"),
        ("Dry-out", "yes" if _get(validation, "dry_out", False) else "no"),
        ("Overflow", "yes" if _get(validation, "overflow", False) else "no"),
        ("Impact", "yes" if _get(validation, "impact", False) else "no"),
        ("Synchronized samples",
         str(len(_get(validation, "synchronized_time", t) or t))),
    ]
    validation_html = _section(
        "Validation",
        "<table>"
        + "".join(f'<tr><td class="k">{k}</td><td>{v}</td></tr>' for k, v in val_rows)
        + "</table>",
    )

    # ---- Outputs ----
    out_rows = [
        ("Peak free-surface elevation (m)", _num_fmt(max(elevation))),
        ("Peak wall pressure (Pa)", _num_fmt(max(pressure))),
        ("Liquid mass drift (kg)", _num_fmt(max(mass) - min(mass))),
        ("Peak inlet flow", _num_fmt(max(inlet))),
        ("Peak outlet flow", _num_fmt(max(outlet))),
        ("Solver", _esc(_get(metadata, "solver", "gravity_conduit_surrogate"))),
    ]
    outputs = _section(
        "Outputs",
        "<table>"
        + "".join(f'<tr><td class="k">{k}</td><td>{v}</td></tr>' for k, v in out_rows)
        + "</table>",
    )

    # ---- Limitations ----
    limitations = _section(
        "Limitations",
        "<ul>"
        "<li>Reduced-order gravity-conduit surrogate; not a substitute for a fully "
        "resolved VOF CFD run at extreme fills or violent impact regimes.</li>"
        "<li>Single-case view: cross-case resonance context is in the aggregate "
        "report.</li>"
        "<li>Wall-impact pressures are indicative and require dedicated slam "
        "analysis for design loads.</li>"
        "</ul>",
    )

    body = (
        inputs + methodology + visualizations + validation_html + outputs + limitations
    )
    subtitle = f"Case {case_id} - status: {'VALID' if valid else 'INVALID (flagged)'}"
    return _shell("Sloshing CFD Case Report", subtitle, body)


# ============================================================================
# Aggregate matrix report
# ============================================================================


def _grid_axes(cases: Sequence[Any]) -> Tuple[List[float], List[float]]:
    fills = sorted({round(float(_get(c, "fill_fraction")), 6) for c in cases})
    ratios = sorted({round(float(_get(c, "period_ratio")), 6) for c in cases})
    return fills, ratios


def render_aggregate_report(manifest: Any, histories: Any = None) -> str:
    """Render a self-contained HTML matrix report over the sweep.

    Args:
        manifest: A shape with ``schema_version, study_name, provenance, cases,
            content_hash`` where ``cases`` is a sequence of ``SweepCase`` shapes.
        histories: Optional mapping ``case_id -> SynchronizedTimeHistory`` used to
            populate result heatmaps and validity flags. When absent the heatmaps
            fall back to input-derived quantities.

    Returns:
        A complete offline HTML document (str).

    Raises:
        ValueError: When the manifest carries no cases.
    """
    cases = list(_get(manifest, "cases"))
    if not cases:
        raise ValueError("aggregate report requires at least one case in the manifest")
    hist_map: Dict[str, Any] = dict(histories or {})
    study = _get(manifest, "study_name", "CFD Sloshing Study")
    logger.info("Rendering aggregate report '{}' over {} cases", study, len(cases))

    fills, ratios = _grid_axes(cases)
    # lookup (fill, ratio) -> case
    lut: Dict[Tuple[float, float], Any] = {}
    for c in cases:
        lut[(round(float(_get(c, "fill_fraction")), 6),
             round(float(_get(c, "period_ratio")), 6))] = c

    # validity per case
    def case_valid(c: Any) -> Optional[bool]:
        h = hist_map.get(str(_get(c, "case_id")))
        if h is None:
            return None
        return _is_valid(_get(h, "validation", {}))

    invalid_ids = [
        str(_get(c, "case_id")) for c in cases if case_valid(c) is False
    ]

    # ---- heatmap matrices ----
    have_hist = bool(hist_map)
    m1: List[List[Optional[float]]] = []
    m2: List[List[Optional[float]]] = []
    for f in fills:
        r1, r2 = [], []
        for r in ratios:
            c = lut.get((f, r))
            if c is None:
                r1.append(None)
                r2.append(None)
                continue
            h = hist_map.get(str(_get(c, "case_id")))
            if have_hist and h is not None and _is_valid(_get(h, "validation", {})):
                ch = _get(h, "channels")
                r1.append(max(_channel_values(ch, "free_surface_elevation")))
                r2.append(max(_channel_values(ch, "wall_pressure")))
            elif have_hist and h is not None:
                r1.append(None)  # invalid -> shown as n/a in heatmap, flagged in table
                r2.append(None)
            else:
                r1.append(float(_get(c, "period_ratio")))
                r2.append(1.0 if _get(c, "near_resonance", False) else 0.0)
        m1.append(r1)
        m2.append(r2)

    row_labels = [f"fill {f:g}" for f in fills]
    col_labels = [f"{r:g}" for r in ratios]
    if have_hist:
        t1 = "Peak Free-Surface Elevation (m)"
        t2 = "Peak Wall Pressure (Pa)"
    else:
        t1 = "Period Ratio (T_roll / T_natural)"
        t2 = "Near Resonance (1 = yes)"
    heat1 = _heatmap_svg(
        row_labels, col_labels, m1, t1,
        "Period Ratio (T_roll / T_natural)", "Fill Fraction",
    )
    heat2 = _heatmap_svg(
        row_labels, col_labels, m2, t2,
        "Period Ratio (T_roll / T_natural)", "Fill Fraction",
    )

    # ---- Inputs ----
    in_rows = [
        ("Study", _esc(study)),
        ("Schema version", _esc(_get(manifest, "schema_version", "?"))),
        ("Provenance", _esc(_get(manifest, "provenance", "n/a"))),
        ("Content hash", _esc(_get(manifest, "content_hash", "n/a"))),
        ("Number of cases", str(len(cases))),
        ("Fill fractions", _esc(", ".join(f"{f:g}" for f in fills))),
        ("Period ratios", _esc(", ".join(f"{r:g}" for r in ratios))),
    ]
    inputs = _section(
        "Inputs",
        "<table>"
        + "".join(f'<tr><td class="k">{k}</td><td>{v}</td></tr>' for k, v in in_rows)
        + "</table>",
    )

    # ---- Methodology ----
    methodology = _section(
        "Methodology",
        "<ul>"
        "<li>Two-axis parametric sweep over fill fraction and the roll-to-natural "
        "period ratio T_roll/T_natural.</li>"
        "<li>Each case is solved with the gravity-conduit sloshing surrogate and "
        "synchronized; per-case validation guards mass conservation and detects "
        "dry-out / overflow / impact.</li>"
        "<li>Heatmaps aggregate per-case peak responses across the two sweep axes; "
        "invalid cases are excluded from colour scaling and shown explicitly "
        "flagged below.</li>"
        "</ul>",
    )

    # ---- Visualizations ----
    visualizations = _section(
        "Visualizations",
        "<p>Response heatmaps over the fill x period-ratio grid (inline SVG).</p>"
        + heat1
        + heat2,
    )

    # ---- Validation ----
    n_invalid = len(invalid_ids)
    if hist_map:
        summary = (
            f"<p><b>{n_invalid} invalid</b> of {len(cases)} cases flagged "
            f"(mass-balance / dry-out / overflow / impact).</p>"
        )
        if invalid_ids:
            inv_rows = []
            for cid in invalid_ids:
                h = hist_map.get(cid)
                reasons = _flag_reasons(_get(h, "validation", {})) if h else ["failed"]
                inv_rows.append(
                    f'<tr class="invalid"><td class="k">{_esc(cid)}</td>'
                    f'<td>{_esc(", ".join(reasons))}</td></tr>'
                )
            summary += (
                "<table><tr><th>Flagged case</th><th>Reason</th></tr>"
                + "".join(inv_rows)
                + "</table>"
            )
        else:
            summary += "<p>All cases passed validation.</p>"
    else:
        summary = (
            "<p>No time-history validation supplied; per-case validity is not "
            "available in this render.</p>"
        )
    validation_html = _section("Validation", summary)

    # ---- Outputs: full matrix, every case, invalids flagged ----
    header = (
        "<tr><th>Case</th><th>Fill</th><th>Ratio</th><th>T_nat (s)</th>"
        "<th>T_roll (s)</th><th>Resonance</th><th>Status</th></tr>"
    )
    body_rows = []
    for c in cases:
        cid = str(_get(c, "case_id"))
        v = case_valid(c)
        if v is False:
            status = '<span class="badge fail">INVALID</span>'
            rowcls = ' class="invalid"'
        elif v is True:
            status = '<span class="badge pass">valid</span>'
            rowcls = ""
        else:
            status = "n/a"
            rowcls = ""
        body_rows.append(
            f"<tr{rowcls}><td class=\"k\">{_esc(cid)}</td>"
            f"<td>{_num_fmt(float(_get(c, 'fill_fraction')))}</td>"
            f"<td>{_num_fmt(float(_get(c, 'period_ratio')))}</td>"
            f"<td>{_num_fmt(float(_get(c, 'natural_period_s', 0.0)))}</td>"
            f"<td>{_num_fmt(float(_get(c, 'roll_period_s', 0.0)))}</td>"
            f"<td>{'yes' if _get(c, 'near_resonance', False) else 'no'}</td>"
            f"<td>{status}</td></tr>"
        )
    outputs = _section(
        "Outputs",
        "<p>Full case matrix - no case omitted; invalid cases are flagged.</p>"
        f"<table>{header}{''.join(body_rows)}</table>",
    )

    # ---- Limitations ----
    limitations = _section(
        "Limitations",
        "<ul>"
        "<li>Grid resolution is limited to the sampled fills and period ratios; "
        "peaks between grid nodes are not captured.</li>"
        "<li>Invalid cases are excluded from heatmap colour scaling (shown n/a) but "
        "retained and flagged in the matrix and validation tables.</li>"
        "<li>Surrogate model; extreme regimes require full VOF CFD confirmation.</li>"
        "</ul>",
    )

    body = (
        inputs + methodology + visualizations + validation_html + outputs + limitations
    )
    subtitle = (
        f"{len(cases)} cases - {n_invalid} flagged invalid - "
        f"schema {_esc(_get(manifest, 'schema_version', '?'))}"
    )
    return _shell(f"Sloshing Sweep Aggregate Report", subtitle, body)
