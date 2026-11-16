"""Charts for the fender design report and workbook (matplotlib, PNG).

Every chart has one y-scale per panel. Sensitivity charts stack two panels
sharing the x-axis: energy per fender (with the fender's energy capacity) and
reaction per fender (with the reaction limit). The credible range of the
input is shaded and the base value is marked.

Colours follow a fixed categorical order; limit lines use one reserved
"limit" colour and always carry a text label, so colour is never the only cue.

Layout: value axes keep headroom above the largest value or limit
(:data:`HEADROOM`), limit labels sit just clear of their line and of any
vertical limit line, a label beside a vertical line turns to the side with
room, the limits of the size chart are named in the headroom, long category
names are wrapped, footnotes are wrapped inside a band reserved below the
axes, and every chart is checked
before it is written (:func:`layout_issues`): text outside the figure,
overlapping text or text across a limit line raises a
:class:`ChartLayoutWarning`.
"""

from __future__ import annotations

import math
import textwrap
import warnings
from pathlib import Path

import matplotlib

matplotlib.use("Agg")
import matplotlib.colors as mcolors  # noqa: E402
import matplotlib.pyplot as plt  # noqa: E402
import numpy as np  # noqa: E402
from matplotlib.text import Annotation  # noqa: E402

SERIES = ["#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4", "#008300", "#6250d6", "#e34948"]
LIMIT = "#d03b3b"
INK = "#0b0b0b"
INK2 = "#52514e"
MUTED = "#898781"
GRID = "#e1e0d9"
BAND = "#e8e7e1"
SURFACE = "#fcfcfb"

#: Top of a value axis as a multiple of the largest value or limit drawn, so
#: limit labels and corner labels sit in clear space above the data.
HEADROOM = 1.35
#: Footnote font size (pt).
NOTE_PT = 6.5
#: Gap (pt) between a label and the line it labels or keeps clear of.
GAP_PT = 2.5


class ChartLayoutWarning(UserWarning):
    """A chart has text outside the figure, overlapping text or text across a limit line."""


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
    fig._after_layout = []
    return fig, axes


def _footnote(fig, text: str):
    """Footnote below the axes, wrapped to the figure width, left-aligned
    inside it; :func:`_save` reserves its height before the axes are laid out."""
    width_pt = fig.get_figwidth() * 72.0 - 16.0
    lines = textwrap.wrap(text, width=max(int(width_pt / (0.55 * NOTE_PT)), 40))
    fig._footnote = fig.text(8.0 / (fig.get_figwidth() * 72.0), 5.0 / (fig.get_figheight() * 72.0),
                             "\n".join(lines), ha="left", va="bottom", fontsize=NOTE_PT, color=MUTED,
                             linespacing=1.3)
    return fig._footnote


def _headroom(ax, *values):
    """Value axis from 0 to :data:`HEADROOM` x the largest finite value."""
    top = max((v for v in values if v is not None and math.isfinite(v)), default=0.0)
    ax.set_ylim(0, HEADROOM * top if top > 0 else 1.0)


def _limit_label(ax, xs, ys, y_line, text, clear_x=()):
    """Label a horizontal limit line just above it, at the end of the x range
    where the data are lower (an end without data counts as lower).

    ``clear_x`` are the x positions of vertical lines in the same axes: after
    layout a label that one of them would cross is moved beside it, to the
    side with room (:func:`_beside`).
    """
    first, last = ys[0], ys[-1]
    if not math.isfinite(first):
        left = True
    elif not math.isfinite(last):
        left = False
    else:
        left = first <= last
    x, ha, dx = (xs[0], "left", 2.0) if left else (xs[-1], "right", -2.0)
    label = ax.annotate(text, (x, y_line), textcoords="offset points", xytext=(dx, GAP_PT),
                        ha=ha, va="bottom", fontsize=7.5, color=INK)
    if clear_x:
        _beside(ax.figure, ax, label, clear_x)
    return label


def _beside(fig, ax, label, at):
    """After layout, move the offset-point annotation ``label`` off each
    vertical line at the data positions ``at`` that crosses it: to the right
    of the line when the label then still ends inside the axes, else to its
    left."""
    def hook(renderer):
        gap = (GAP_PT + 1.0) * fig.dpi / 72.0
        ab = ax.get_window_extent(renderer)
        for v in sorted(at):
            bb = label.get_window_extent(renderer)
            p = ax.transData.transform((v, 0.0))[0]
            if not (bb.x0 - gap < p < bb.x1 + gap):
                continue
            right = p + gap + bb.width <= ab.x1 - 1.0
            shift = (p + gap - bb.x0) if right else (p - gap - bb.x1)
            dx, dy = label.xyann
            label.xyann = (dx + shift * 72.0 / fig.dpi, dy)
    fig._after_layout.append(hook)


def _corner_label(ax, x_mark, text):
    """Text in a top corner of ``ax``, inside the headroom: the corner away
    from the x position ``x_mark`` (a marked line)."""
    lo, hi = ax.get_xlim()
    right = (x_mark - lo) / (hi - lo) < 0.4 if hi > lo else True
    return ax.text(0.99 if right else 0.01, 0.97, text, transform=ax.transAxes,
                   ha="right" if right else "left", va="top", fontsize=7.5, color=INK)


def _clear_of(fig, ax, texts, at, axis="y"):
    """After layout, move each text in ``texts`` that crosses a line at one of
    the data positions ``at`` to just past it: above a horizontal line
    (``axis`` "y") or right of a vertical line (``axis`` "x")."""
    def hook(renderer):
        gap = GAP_PT * fig.dpi / 72.0
        for t in texts:
            for v in sorted(at):
                bb = t.get_window_extent(renderer)
                if axis == "y":
                    p, lo, hi = ax.transData.transform((0.0, v))[1], bb.y0, bb.y1
                else:
                    p, lo, hi = ax.transData.transform((v, 0.0))[0], bb.x0, bb.x1
                if lo - gap < p < hi + gap:
                    shift = p + gap - lo
                    if isinstance(t, Annotation) and t.anncoords == "offset points":
                        dx, dy = t.xyann
                        pts = shift * 72.0 / fig.dpi
                        t.xyann = (dx, dy + pts) if axis == "y" else (dx + pts, dy)
                        continue
                    x0, y0 = ax.transData.transform(t.get_position())
                    xy = (x0, y0 + shift) if axis == "y" else (x0 + shift, y0)
                    t.set_position(ax.transData.inverted().transform(xy))
    fig._after_layout.append(hook)


def _drawn_texts(fig):
    """Every visible, non-empty text the figure draws (titles, axis labels,
    tick labels in view, legends, annotations, footnotes), with its axes."""
    out = [(t, None) for t in fig.texts]
    for ax in fig.axes:
        texts = list(ax.texts) + [ax.title, ax._left_title, ax._right_title, ax.xaxis.label, ax.yaxis.label]
        for axis in (ax.xaxis, ax.yaxis):
            lo, hi = sorted(axis.get_view_interval())
            tol = 1e-9 * ((hi - lo) or 1.0)
            for tick, loc in zip(axis.get_major_ticks(), axis.get_majorticklocs()):
                if lo - tol <= loc <= hi + tol:
                    texts += [tick.label1, tick.label2]
        leg = ax.get_legend()
        if leg is not None:
            texts += list(leg.get_texts())
        out += [(t, ax) for t in texts]
    return [(t, ax) for t, ax in out if t.get_visible() and t.get_text().strip()]


def _limit_lines(ax):
    """(kind, display position, (lo, hi) display extent, half width) of each
    straight limit line (colour :data:`LIMIT`) of ``ax``."""
    out = []
    for ln in ax.lines:
        if mcolors.to_hex(ln.get_color()) != LIMIT:
            continue
        xd, yd = np.asarray(ln.get_xdata(), float), np.asarray(ln.get_ydata(), float)
        if len(xd) < 2:
            continue
        pts = ln.get_transform().transform(np.column_stack([xd, yd]))
        half = ln.get_linewidth() * ln.figure.dpi / 72.0 / 2.0
        if np.allclose(yd, yd[0]):
            out.append(("h", pts[0, 1], (pts[:, 0].min(), pts[:, 0].max()), half))
        elif np.allclose(xd, xd[0]):
            out.append(("v", pts[0, 0], (pts[:, 1].min(), pts[:, 1].max()), half))
    return out


def layout_issues(fig) -> list[str]:
    """Layout defects of a figure: text outside the figure or within 2 px of
    its edge, an annotation or label outside its own axes, texts that overlap
    one another, and text across a limit line (colour :data:`LIMIT`)."""
    fig.canvas.draw()
    renderer = fig.canvas.get_renderer()
    fb = fig.bbox
    texts = [(t, ax, t.get_window_extent(renderer)) for t, ax in _drawn_texts(fig)]
    issues = []
    tol = -2.0                      # at least 2 px inside the figure edge
    for t, _, bb in texts:
        if bb.x0 < fb.x0 - tol or bb.y0 < fb.y0 - tol or bb.x1 > fb.x1 + tol or bb.y1 > fb.y1 + tol:
            issues.append(f"text outside the figure: {t.get_text()!r}")
    for ax in fig.axes:
        ab = ax.get_window_extent(renderer)
        for t in ax.texts:
            if not (t.get_visible() and t.get_text().strip()):
                continue
            bb = t.get_window_extent(renderer)
            if bb.x0 < ab.x0 - 1.0 or bb.y0 < ab.y0 - 1.0 or bb.x1 > ab.x1 + 1.0 or bb.y1 > ab.y1 + 1.0:
                issues.append(f"text outside its axes: {t.get_text()!r}")
    for i, (t1, _, b1) in enumerate(texts):
        for t2, _, b2 in texts[i + 1:]:
            if min(b1.x1, b2.x1) - max(b1.x0, b2.x0) > 1.0 and min(b1.y1, b2.y1) - max(b1.y0, b2.y0) > 1.0:
                issues.append(f"text overlaps: {t1.get_text()!r} and {t2.get_text()!r}")
    for ax in fig.axes:
        for kind, at, (lo, hi), half in _limit_lines(ax):
            for t, tax, bb in texts:
                if tax is not ax:
                    continue
                a0, a1, s0, s1 = (bb.y0, bb.y1, bb.x0, bb.x1) if kind == "h" else (bb.x0, bb.x1, bb.y0, bb.y1)
                if a0 < at + half and a1 > at - half and min(s1, hi) - max(s0, lo) > 1.0:
                    issues.append(f"text across a limit line: {t.get_text()!r}")
    return issues


def _finish(fig, path):
    fig.savefig(path, facecolor=SURFACE)
    plt.close(fig)
    return path


def _save(fig, path):
    """Lay out with the footnote band reserved, place the post-layout labels,
    check the layout (one :class:`ChartLayoutWarning` per defect) and write."""
    path = Path(path)
    path.parent.mkdir(parents=True, exist_ok=True)
    note = getattr(fig, "_footnote", None)
    bottom = 0.0
    if note is not None:
        n = note.get_text().count("\n") + 1
        bottom = (n * NOTE_PT * 1.3 + 9.0) / (fig.get_figheight() * 72.0)
    fig.tight_layout(rect=(0.0, bottom, 1.0, 1.0))
    hooks = getattr(fig, "_after_layout", [])
    if hooks:
        fig.canvas.draw()
        renderer = fig.canvas.get_renderer()
        for hook in hooks:
            hook(renderer)
    for issue in layout_issues(fig):
        warnings.warn(f"{path.name}: {issue}", ChartLayoutWarning, stacklevel=3)
    return _finish(fig, path)


def _num(value: float) -> str:
    """A label number: thousands separated from 1,000 up, else 4 significant figures."""
    return f"{value:,.0f}" if abs(value) >= 1000 else f"{value:.4g}"


def _with_unit(value: float, unit: str) -> str:
    return _num(value) if unit in ("-", "") else f"{_num(value)} {unit}"


def sweep_chart(sweep, path, title: str, fender_label: str):
    """Two-panel sensitivity chart for one input."""
    fig, (ax1, ax2) = _fig(2, height=5.4, sharex=True)
    xs = [p.value for p in sweep.points]
    e = [p.energy_per_fender_knm for p in sweep.points]
    r = [p.reaction_kn if p.reaction_kn is not None else math.nan for p in sweep.points]
    pad = 0.02 * (xs[-1] - xs[0])
    for ax in (ax1, ax2):
        _style(ax)
        ax.axvspan(sweep.range_low, sweep.range_high, color=BAND, zorder=0)
        ax.axvline(sweep.base_value, color=MUTED, linewidth=1.0, linestyle=":")
    ax1.set_xlim(xs[0] - pad, xs[-1] + pad)
    ax1.plot(xs, e, color=SERIES[0], linewidth=2, label="energy per fender")
    base = sweep.at(sweep.base_value)
    ax1.plot([sweep.base_value], [base.energy_per_fender_knm], "o", color=SERIES[0],
             markersize=7, markeredgecolor=SURFACE, markeredgewidth=1.5)
    ax1.axhline(sweep.capacity_knm, color=LIMIT, linewidth=1.5, linestyle="--")
    _headroom(ax1, sweep.capacity_knm, *e)
    _limit_label(ax1, xs, e, sweep.capacity_knm,
                 f"energy capacity {sweep.capacity_knm:,.0f} kNm ({fender_label})")
    _corner_label(ax1, sweep.base_value,
                  f"base (dot) at {_with_unit(sweep.base_value, sweep.unit)}: {base.energy_per_fender_knm:,.0f} kNm")
    ax1.set_ylabel("energy per fender (kNm)")
    ax2.plot(xs, r, color=SERIES[1], linewidth=2, label="reaction per fender")
    _headroom(ax2, sweep.reaction_limit_kn, *r)
    if sweep.reaction_limit_kn is not None:
        ax2.axhline(sweep.reaction_limit_kn, color=LIMIT, linewidth=1.5, linestyle="--")
        x_lim = sweep.reaction_limit_value()
        _limit_label(ax2, xs, r, sweep.reaction_limit_kn, f"reaction limit {sweep.reaction_limit_kn:,.0f} kN",
                     clear_x=() if x_lim is None else (x_lim,))
        if x_lim is not None:
            ax2.axvline(x_lim, color=LIMIT, linewidth=1.0, linestyle=":")
            lo, hi = ax2.get_xlim()
            right = (x_lim - lo) / (hi - lo) < 0.6
            ax2.annotate(f"reaches limit at {_with_unit(x_lim, sweep.unit)}", (x_lim, 1.0),
                         xycoords=("data", "axes fraction"), textcoords="offset points",
                         xytext=(4 if right else -4, -3), ha="left" if right else "right", va="top",
                         fontsize=7, color=INK)
    spans = unrated_intervals(xs, [p.reaction_kn is not None for p in sweep.points])
    x0, x1 = ax2.get_xlim()
    for i, (lo, hi) in enumerate(spans):
        ax2.axvspan(lo, hi, color=LIMIT, alpha=0.08, zorder=0)
        # labelled in place only where the span is wide enough to hold the label
        if i == 0 and (hi - lo) / (x1 - x0) >= 0.2:
            ax2.annotate("above rated energy:\nreaction not rated", (lo, 0.0), textcoords="offset points",
                         xytext=(3, 3), fontsize=7, color=INK2, va="bottom")
    rf = getattr(sweep, "reaction_factor", 1.0)
    ax2.set_ylabel("reaction per fender (kN)" if rf == 1.0 else f"reaction per fender x {rf:.2f} (kN)")
    ax2.set_xlabel(f"{sweep.label} ({sweep.unit})")
    ax1.set_title(title, fontsize=9.5, color=INK, loc="left")
    rf_note = "" if rf == 1.0 else f" Reaction includes the reaction factor {rf:.2f} (manufacturing tolerance)."
    pink = " Pink: above the rated energy, where the catalogue does not rate the reaction." if spans else ""
    _footnote(fig, f"Shaded: credible range {_num(sweep.range_low)} to {_num(sweep.range_high)} {sweep.unit} "
                   f"({sweep.range_basis}). Dotted: base. Dashed: limit.{pink}{rf_note}")
    return _save(fig, path)


def unrated_intervals(xs, rated) -> list[tuple[float, float]]:
    """Contiguous x-intervals of the points that are not rated.

    ``xs`` are the sweep values in order and ``rated`` says, point by point,
    whether a reaction exists. Each run of unrated points becomes one
    interval that extends halfway to the neighbouring rated point (or to the
    end of the sweep), so rated points are never shaded.
    """
    xs, rated = list(xs), list(rated)
    if len(xs) != len(rated):
        raise ValueError("xs and rated differ in length")
    out, i, n = [], 0, len(xs)
    while i < n:
        if rated[i]:
            i += 1
            continue
        j = i
        while j + 1 < n and not rated[j + 1]:
            j += 1
        lo = xs[i] if i == 0 else 0.5 * (xs[i - 1] + xs[i])
        hi = xs[j] if j == n - 1 else 0.5 * (xs[j] + xs[j + 1])
        out.append((lo, hi))
        i = j + 1
    return out


def tornado_chart(bars, path, title: str, base_energy_knm: float):
    """Horizontal bars: % change of design energy at the low and high end of each range."""
    fig, ax = _fig(1, height=0.45 * len(bars) + 1.4)
    _style(ax)
    ys = list(range(len(bars)))[::-1]
    for y, b in zip(ys, bars):
        lo, hi = sorted((b.low_change_pct, b.high_change_pct))
        ax.barh(y, hi - lo, left=lo, height=0.55, color=SERIES[0], edgecolor=SURFACE)
        ax.text(hi, y, f"  {_num(b.low_value)} to {_num(b.high_value)}: {b.low_change_pct:+.0f} % / {b.high_change_pct:+.0f} %",
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


VALIDITY_MARK = "outside its stated validity"


def outside_validity(res) -> list[str]:
    """Flags of a result that put a formula outside its stated validity."""
    return [f for f in getattr(res, "flags", ()) if VALIDITY_MARK in f]


def _vline_label(ax, x, y, text):
    """Label a vertical limit line at ``x`` just right of it; after layout a
    label that would run past the right end of the axes turns to the left of
    the line."""
    label = ax.annotate(text, (x, y), textcoords="offset points", xytext=(GAP_PT + 1.0, 0.0),
                        fontsize=7.5, color=INK, va="center")
    fig = ax.figure

    def hook(renderer):
        if label.get_window_extent(renderer).x1 > ax.get_window_extent(renderer).x1 - 1.0:
            label.set_ha("right")
            label.xyann = (-(GAP_PT + 1.0), 0.0)
    fig._after_layout.append(hook)
    return label


def _wrap_names(fig, names, font_pt: float) -> list[str]:
    """Category names wrapped at spaces so that each fits its share of the
    figure width (about 80 % of the width is axes) at ``font_pt``."""
    slot_pt = 0.8 * fig.get_figwidth() * 72.0 / max(len(names), 1)
    chars = max(int(slot_pt / (0.62 * font_pt)), 6)
    return ["\n".join(textwrap.wrap(str(n), width=chars, break_long_words=False, break_on_hyphens=False)
                      or [str(n)]) for n in names]


def method_chart(results, path, title: str, capacity_knm: float | None = None,
                 capacity_label: str = ""):
    """Design energy by method (horizontal bars), with an optional capacity line.

    The capacity line is common to every bar, so it only applies to bars that
    share one capacity (for example steps of one method on one fender). Method
    results whose capacity divisors differ (WG211 gamma_m) are refused with a
    capacity line: use :func:`method_utilisation_chart`, which compares each
    method with its own capacity. A result with a flag that puts a formula
    outside its stated validity is hatched, labelled "(outside validity)" and
    explained in a note below.
    """
    if capacity_knm and len({getattr(r, "capacity_divisor", 1.0) for r in results}) > 1:
        raise ValueError("results have different capacity divisors; one capacity line would compare a "
                         "method with another method's capacity: use method_utilisation_chart")
    fig, ax = _fig(1, height=0.5 * len(results) + 1.6, width=8.5)
    _style(ax)
    ys = list(range(len(results)))[::-1]
    notes, labels = [], []
    for y, res in zip(ys, results):
        bad = outside_validity(res)
        ax.barh(y, res.design_energy_knm, height=0.55, color=SERIES[0], edgecolor=SURFACE,
                hatch="///" if bad else None, alpha=0.55 if bad else 1.0)
        labels.append(ax.text(res.design_energy_knm, y, f"  {res.design_energy_knm:,.0f} kNm"
                              + ("  (outside validity)" if bad else ""), va="center", fontsize=7.5, color=INK))
        notes += [f"{res.label}: {f}" for f in bad]
    if notes:
        _footnote(fig, "Hatched: " + "; ".join(notes) + ".")
    ax.set_yticks(ys)
    ax.set_yticklabels([res.label for res in results], fontsize=8, color=INK)
    if capacity_knm:
        ax.axvline(capacity_knm, color=LIMIT, linestyle="--", linewidth=1.5)
        _vline_label(ax, capacity_knm, ys[-1] - 0.75, f"{capacity_label}: {capacity_knm:,.0f} kNm")
        _clear_of(fig, ax, labels, [capacity_knm], axis="x")
    ax.set_xlabel("design energy (kNm)")
    ax.set_xlim(0, max(max(r.design_energy_knm for r in results), capacity_knm or 0.0) * 1.45)
    ax.set_ylim(ys[-1] - 1.2, ys[0] + 0.6)
    ax.set_title(title, fontsize=9.5, color=INK, loc="left")
    return _save(fig, path)


def method_utilisation_chart(results, utilisations, path, title: str, note: str = ""):
    """Utilisation by method (horizontal bars) against the limit 1.0.

    ``utilisations`` pairs with ``results``: each method's energy per fender
    over the fender capacity under that method (its own capacity divisor,
    WG211 gamma_m), so no method is compared with another method's capacity.
    Each bar is labelled with the utilisation and the design energy; a bar
    above 1.0 also says "over capacity". A result with a flag that puts a
    formula outside its stated validity is hatched, labelled "(outside
    validity)" and explained in a note below, after ``note``.
    """
    if len(utilisations) != len(results):
        raise ValueError("one utilisation per method result")
    fig, ax = _fig(1, height=0.5 * len(results) + 1.8, width=8.5)
    _style(ax)
    ys = list(range(len(results)))[::-1]
    notes, labels = [], []
    for y, res, u in zip(ys, results, utilisations):
        bad = outside_validity(res)
        ax.barh(y, u, height=0.55, color=SERIES[0], edgecolor=SURFACE,
                hatch="///" if bad else None, alpha=0.55 if bad else 1.0)
        labels.append(ax.text(u, y, f"  {u:.2f} ({res.design_energy_knm:,.0f} kNm)"
                              + ("  over capacity" if u > 1.0 else "")
                              + ("  (outside validity)" if bad else ""), va="center", fontsize=7.5, color=INK,
                              bbox=dict(facecolor=SURFACE, edgecolor="none", pad=0.4)))
        notes += [f"{res.label}: {f}" for f in bad]
    below = ([note] if note else []) + (["Hatched: " + "; ".join(notes) + "."] if notes else [])
    if below:
        _footnote(fig, " ".join(below))
    ax.set_yticks(ys)
    ax.set_yticklabels([res.label for res in results], fontsize=8, color=INK)
    ax.axvline(1.0, color=LIMIT, linestyle="--", linewidth=1.5)
    _vline_label(ax, 1.0, ys[-1] - 0.75, "utilisation 1.0: each method's own fender capacity")
    _clear_of(fig, ax, labels, [1.0], axis="x")
    ax.set_xlabel("utilisation: energy per fender / design capacity under the method (-)")
    ax.set_xlim(0, max(max(utilisations), 1.0) * 1.6)
    ax.set_ylim(ys[-1] - 1.2, ys[0] + 0.6)
    ax.set_title(title, fontsize=9.5, color=INK, loc="left")
    return _save(fig, path)


def line_family_chart(series, path, title: str, xlabel: str, ylabel: str,
                      logx: bool = False, marker=None, note: str = ""):
    """Several labelled lines on one axis (<= 8 series, fixed colour order).

    ``series`` is a list of (label, xs, ys). ``marker`` is (x, y, text) for the
    base case; its label sits in the top corner away from the marker.
    """
    fig, ax = _fig(1, height=4.0)
    _style(ax)
    for i, (label, xs, ys) in enumerate(series):
        ax.plot(xs, ys, color=SERIES[i % len(SERIES)], linewidth=2, label=label)
    if logx:
        ax.set_xscale("log")
    top = max(max(ys) for _, _, ys in series)
    ax.set_ylim(min(0.0, min(min(ys) for _, _, ys in series)), top * 1.45)
    if marker:
        x, y, text = marker
        ax.plot([x], [y], "o", color=INK, markersize=7, markeredgecolor=SURFACE)
        lo, hi = ax.get_xlim()
        fx = ((math.log(x) - math.log(lo)) / (math.log(hi) - math.log(lo))) if logx else (x - lo) / (hi - lo)
        ax.text(0.99 if fx < 0.5 else 0.01, 0.97, f"{text} (dot)", transform=ax.transAxes,
                ha="right" if fx < 0.5 else "left", va="top", fontsize=7.5, color=INK)
        loc = "upper left" if fx < 0.5 else "upper right"
    else:
        loc = "upper right"
    ax.set_xlabel(xlabel)
    ax.set_ylabel(ylabel)
    ax.legend(fontsize=7.0, frameon=False, loc=loc)
    ax.set_title(title, fontsize=9.5, color=INK, loc="left")
    if note:
        _footnote(fig, note)
    return _save(fig, path)


def range_by_type_chart(stats, path, title: str, xlabel: str):
    """Min-to-max range with the median for each fender type (log x)."""
    fig, ax = _fig(1, height=0.42 * len(stats) + 1.6)
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
    _footnote(fig, "Bar: smallest to largest rated value; dot: median.")
    return _save(fig, path)


def size_comparison_chart(rows, path, title: str, design_energy_knm: float,
                          reaction_limit_kn: float | None = None, basis_note: str = ""):
    """Energy capacity and reaction of neighbouring sizes (two panels, one scale each).

    ``energy_knm`` and ``reaction_kn`` in each row are the values to compare
    with the demand and the limit, i.e. after the case's tolerances,
    reductions and capacity divisor; ``basis_note`` states that basis. Bar
    labels that would cross a limit line are moved just above it; the limit
    lines are named in the headroom above the bars, and the size names are
    wrapped and upright.
    """
    fig, (ax1, ax2) = _fig(2, height=5.4, sharex=True)
    names = [r["label"] for r in rows]
    xs = list(range(len(rows)))
    for ax in (ax1, ax2):
        _style(ax)
    c1 = [SERIES[1] if r.get("is_base") else SERIES[0] for r in rows]
    energy = [r["energy_knm"] for r in rows]
    reaction = [r["reaction_kn"] for r in rows]
    ax1.bar(xs, energy, color=c1, edgecolor=SURFACE, width=0.6)
    ax1.axhline(design_energy_knm, color=LIMIT, linestyle="--", linewidth=1.5)
    ax1.set_xlim(-0.6, len(rows) - 0.4)
    _headroom(ax1, design_energy_knm, *energy)
    # a bar can end at any height, so the dashed lines are named in the headroom
    # above every bar, not beside the line where a bar label may sit
    ax1.text(0.01, 0.97, f"dashed: energy per fender {design_energy_knm:,.0f} kNm", transform=ax1.transAxes,
             ha="left", va="top", fontsize=7.5, color=INK)
    lab1 = [ax1.annotate(f"{r['energy_change_pct']:+.0f} %", (x, r["energy_knm"]), textcoords="offset points",
                         xytext=(0, 1.5), ha="center", va="bottom", fontsize=7, color=INK)
            for x, r in zip(xs, rows)]
    _clear_of(fig, ax1, lab1, [design_energy_knm])
    ax1.set_ylabel("design energy capacity (kNm)")
    ax2.bar(xs, reaction, color=c1, edgecolor=SURFACE, width=0.6)
    _headroom(ax2, reaction_limit_kn, *reaction)
    if reaction_limit_kn:
        ax2.axhline(reaction_limit_kn, color=LIMIT, linestyle="--", linewidth=1.5)
        ax2.text(0.01, 0.97, f"dashed: reaction limit {reaction_limit_kn:,.0f} kN", transform=ax2.transAxes,
                 ha="left", va="top", fontsize=7.5, color=INK)
    lab2 = [ax2.annotate(f"{r['reaction_change_pct']:+.0f} %", (x, r["reaction_kn"]), textcoords="offset points",
                         xytext=(0, 1.5), ha="center", va="bottom", fontsize=7, color=INK)
            for x, r in zip(xs, rows)]
    if reaction_limit_kn:
        _clear_of(fig, ax2, lab2, [reaction_limit_kn])
    ax2.set_ylabel("design reaction (kN)")
    ax2.set_xticks(xs)
    ax2.set_xticklabels(_wrap_names(fig, names, 7.0), fontsize=7.0, color=INK)
    ax1.set_title(title, fontsize=9.5, color=INK, loc="left")
    _footnote(fig, "Orange: base choice. Labels: % change from the base. Dashed: demand and limit."
              + (f" {basis_note}" if basis_note else ""))
    return _save(fig, path)
