"""Charts for the fender design report and workbook (matplotlib, PNG).

Every chart has one y-scale per panel. Sensitivity charts stack two panels
sharing the x-axis: energy per fender (with the fender's energy capacity) and
reaction per fender (with the reaction limit). The credible range of the
input is shaded and the base value is marked.

Colours follow a fixed categorical order; limit lines use one reserved
"limit" colour and always carry a text label, so colour is never the only cue.
"""

from __future__ import annotations

import math
from pathlib import Path

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt  # noqa: E402

SERIES = ["#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4", "#008300", "#6250d6", "#e34948"]
LIMIT = "#d03b3b"
INK = "#0b0b0b"
INK2 = "#52514e"
MUTED = "#898781"
GRID = "#e1e0d9"
BAND = "#e8e7e1"
SURFACE = "#fcfcfb"


def _style(ax):
    ax.set_facecolor(SURFACE)
    ax.grid(True, color=GRID, linewidth=0.6)
    for side in ("top", "right"):
        ax.spines[side].set_visible(False)
    for side in ("left", "bottom"):
        ax.spines[side].set_color("#c3c2b7")
    ax.tick_params(colors=INK2, labelsize=8)
    ax.set_axisbelow(True)
    ax.yaxis.label.set_color(INK2)
    ax.xaxis.label.set_color(INK2)


def _fig(nrows=1, height=3.6, width=7.0, sharex=False):
    fig, axes = plt.subplots(nrows, 1, figsize=(width, height), sharex=sharex, dpi=150)
    fig.patch.set_facecolor(SURFACE)
    return fig, axes


def _save(fig, path):
    path = Path(path)
    path.parent.mkdir(parents=True, exist_ok=True)
    fig.tight_layout()
    fig.savefig(path, facecolor=SURFACE)
    plt.close(fig)
    return path


def sweep_chart(sweep, path, title: str, fender_label: str):
    """Two-panel sensitivity chart for one input."""
    fig, (ax1, ax2) = _fig(2, height=5.2, sharex=True)
    xs = [p.value for p in sweep.points]
    e = [p.energy_per_fender_knm for p in sweep.points]
    r = [p.reaction_kn if p.reaction_kn is not None else math.nan for p in sweep.points]
    for ax in (ax1, ax2):
        _style(ax)
        ax.axvspan(sweep.range_low, sweep.range_high, color=BAND, zorder=0)
        ax.axvline(sweep.base_value, color=MUTED, linewidth=1.0, linestyle=":")
    ax1.plot(xs, e, color=SERIES[0], linewidth=2, label="energy per fender")
    base = sweep.at(sweep.base_value)
    ax1.plot([sweep.base_value], [base.energy_per_fender_knm], "o", color=SERIES[0],
             markersize=7, markeredgecolor=SURFACE, markeredgewidth=1.5)
    ax1.annotate(f"base {sweep.base_value:.4g} {sweep.unit}\n{base.energy_per_fender_knm:,.0f} kNm",
                 (sweep.base_value, base.energy_per_fender_knm), textcoords="offset points",
                 xytext=(6, 6), fontsize=7.5, color=INK)
    ax1.axhline(sweep.capacity_knm, color=LIMIT, linewidth=1.5, linestyle="--")
    ax1.text(xs[0], sweep.capacity_knm, f" energy capacity {sweep.capacity_knm:,.0f} kNm ({fender_label})",
             va="bottom", fontsize=7.5, color=INK)
    ax1.set_ylabel("energy per fender (kNm)")
    ax1.set_ylim(bottom=0)
    ax2.plot(xs, r, color=SERIES[1], linewidth=2, label="reaction per fender")
    if sweep.reaction_limit_kn is not None:
        ax2.axhline(sweep.reaction_limit_kn, color=LIMIT, linewidth=1.5, linestyle="--")
        ax2.text(xs[0], sweep.reaction_limit_kn, f" reaction limit {sweep.reaction_limit_kn:,.0f} kN",
                 va="bottom", fontsize=7.5, color=INK)
    over = [p.value for p in sweep.points if p.reaction_kn is None]
    if over:
        ax2.axvspan(min(over), max(xs), color=LIMIT, alpha=0.08, zorder=0)
        ax2.text(min(over), 0.0, " above rated energy:\n reaction not rated", fontsize=7,
                 color=INK2, va="bottom")
    ax2.set_ylabel("reaction per fender (kN)")
    ax2.set_ylim(bottom=0)
    ax2.set_xlabel(f"{sweep.label} ({sweep.unit})")
    ax1.set_title(title, fontsize=9.5, color=INK, loc="left")
    ax2.text(1.0, -0.32, f"Shaded: credible range {sweep.range_low:g} to {sweep.range_high:g} {sweep.unit} "
             f"({sweep.range_basis}). Dotted: base.", transform=ax2.transAxes, ha="right",
             fontsize=6.5, color=MUTED)
    return _save(fig, path)


def tornado_chart(bars, path, title: str, base_energy_knm: float):
    """Horizontal bars: % change of design energy at the low and high end of each range."""
    fig, ax = _fig(1, height=0.45 * len(bars) + 1.4)
    _style(ax)
    ys = list(range(len(bars)))[::-1]
    for y, b in zip(ys, bars):
        lo, hi = sorted((b.low_change_pct, b.high_change_pct))
        ax.barh(y, hi - lo, left=lo, height=0.55, color=SERIES[0], edgecolor=SURFACE)
        ax.text(hi, y, f"  {b.low_value:.4g} to {b.high_value:.4g}: {b.low_change_pct:+.0f} % / {b.high_change_pct:+.0f} %",
                va="center", fontsize=7.5, color=INK)
    ax.axvline(0, color=MUTED, linewidth=1)
    ax.set_yticks(ys)
    ax.set_yticklabels([b.label for b in bars], fontsize=8, color=INK)
    ax.set_xlabel(f"change of design energy from base {base_energy_knm:,.0f} kNm (%)")
    ax.set_title(title, fontsize=9.5, color=INK, loc="left")
    lo = min(min(b.low_change_pct, b.high_change_pct) for b in bars)
    hi = max(max(b.low_change_pct, b.high_change_pct) for b in bars)
    ax.set_xlim(min(lo, 0) - 10, hi + 1.0 * (hi - min(lo, 0)) + 10)
    return _save(fig, path)


def method_chart(results, path, title: str, capacity_knm: float | None = None,
                 capacity_label: str = ""):
    """Design energy by method (horizontal bars), with an optional capacity line."""
    fig, ax = _fig(1, height=0.5 * len(results) + 1.6, width=8.5)
    _style(ax)
    ys = list(range(len(results)))[::-1]
    for y, res in zip(ys, results):
        ax.barh(y, res.design_energy_knm, height=0.55, color=SERIES[0], edgecolor=SURFACE)
        ax.text(res.design_energy_knm, y, f"  {res.design_energy_knm:,.0f} kNm", va="center",
                fontsize=7.5, color=INK)
    ax.set_yticks(ys)
    ax.set_yticklabels([res.label for res in results], fontsize=8, color=INK)
    if capacity_knm:
        ax.axvline(capacity_knm, color=LIMIT, linestyle="--", linewidth=1.5)
        ax.text(capacity_knm, ys[-1] - 0.75, f" {capacity_label}: {capacity_knm:,.0f} kNm",
                fontsize=7.5, color=INK, va="center")
    ax.set_xlabel("design energy (kNm)")
    ax.set_xlim(0, max(r.design_energy_knm for r in results) * 1.3)
    ax.set_ylim(ys[-1] - 1.2, ys[0] + 0.6)
    ax.set_title(title, fontsize=9.5, color=INK, loc="left")
    return _save(fig, path)


def line_family_chart(series, path, title: str, xlabel: str, ylabel: str,
                      logx: bool = False, marker=None, note: str = ""):
    """Several labelled lines on one axis (<= 8 series, fixed colour order).

    ``series`` is a list of (label, xs, ys). ``marker`` is (x, y, text) for the
    base case.
    """
    fig, ax = _fig(1, height=3.8)
    _style(ax)
    for i, (label, xs, ys) in enumerate(series):
        ax.plot(xs, ys, color=SERIES[i % len(SERIES)], linewidth=2, label=label)
    if marker:
        x, y, text = marker
        ax.plot([x], [y], "o", color=INK, markersize=7, markeredgecolor=SURFACE)
        ax.annotate(text, (x, y), textcoords="offset points", xytext=(6, 6), fontsize=7.5, color=INK)
    if logx:
        ax.set_xscale("log")
    ax.set_xlabel(xlabel)
    ax.set_ylabel(ylabel)
    ax.legend(fontsize=7.5, frameon=False)
    ax.set_title(title, fontsize=9.5, color=INK, loc="left")
    if note:
        ax.text(1.0, -0.22, note, transform=ax.transAxes, ha="right", fontsize=6.5, color=MUTED)
    return _save(fig, path)


def range_by_type_chart(stats, path, title: str, xlabel: str):
    """Min-to-max range with the median for each fender type (log x)."""
    fig, ax = _fig(1, height=0.42 * len(stats) + 1.4)
    _style(ax)
    ys = list(range(len(stats)))[::-1]
    for y, s in zip(ys, stats):
        ax.plot([s["min"], s["max"]], [y, y], color=SERIES[0], linewidth=4, solid_capstyle="round")
        ax.plot([s["median"]], [y], "o", color=INK, markersize=6, markeredgecolor=SURFACE)
        ax.text(s["max"], y, f"  n = {s['n']}", va="center", fontsize=7.5, color=INK2)
    ax.set_yticks(ys)
    ax.set_yticklabels([s["type"] for s in stats], fontsize=8, color=INK)
    ax.set_xscale("log")
    ax.set_xlabel(xlabel)
    ax.set_title(title, fontsize=9.5, color=INK, loc="left")
    ax.text(1.0, -0.2, "Bar: smallest to largest rated value; dot: median.", transform=ax.transAxes,
            ha="right", fontsize=6.5, color=MUTED)
    return _save(fig, path)


def size_comparison_chart(rows, path, title: str, design_energy_knm: float,
                          reaction_limit_kn: float | None = None):
    """Rated energy and reaction of neighbouring sizes (two panels, one scale each)."""
    fig, (ax1, ax2) = _fig(2, height=5.0, sharex=True)
    names = [r["label"] for r in rows]
    xs = list(range(len(rows)))
    for ax in (ax1, ax2):
        _style(ax)
    c1 = [SERIES[1] if r.get("is_base") else SERIES[0] for r in rows]
    ax1.bar(xs, [r["energy_knm"] for r in rows], color=c1, edgecolor=SURFACE, width=0.6)
    ax1.axhline(design_energy_knm, color=LIMIT, linestyle="--", linewidth=1.5)
    ax1.text(-0.4, design_energy_knm, f" energy per fender {design_energy_knm:,.0f} kNm", va="bottom",
             fontsize=7.5, color=INK)
    for x, r in zip(xs, rows):
        ax1.text(x, r["energy_knm"], f"{r['energy_change_pct']:+.0f} %", ha="center", va="bottom",
                 fontsize=7, color=INK)
    ax1.set_ylabel("energy capacity (kNm)")
    ax2.bar(xs, [r["reaction_kn"] for r in rows], color=c1, edgecolor=SURFACE, width=0.6)
    if reaction_limit_kn:
        ax2.axhline(reaction_limit_kn, color=LIMIT, linestyle="--", linewidth=1.5)
        ax2.text(-0.4, reaction_limit_kn, f" reaction limit {reaction_limit_kn:,.0f} kN", va="bottom",
                 fontsize=7.5, color=INK)
    for x, r in zip(xs, rows):
        ax2.text(x, r["reaction_kn"], f"{r['reaction_change_pct']:+.0f} %", ha="center", va="bottom",
                 fontsize=7, color=INK)
    ax2.set_ylabel("rated reaction (kN)")
    ax2.set_xticks(xs)
    ax2.set_xticklabels(names, fontsize=7.5, rotation=20, ha="right", color=INK)
    ax1.set_title(title, fontsize=9.5, color=INK, loc="left")
    ax2.text(1.0, -0.45, "Orange: base choice. Labels: % change from the base.", transform=ax2.transAxes,
             ha="right", fontsize=6.5, color=MUTED)
    return _save(fig, path)
