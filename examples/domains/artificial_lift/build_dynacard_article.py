"""Build the dynacard article (revision 3) as one static, offline HTML page.

Inputs, all saved in this repository:
- examples/report-evals/dynacard-pump-card/results.json: public example well results;
- docs/domains/artificial_lift/dynacard-examples/results.json: the ten worked examples;
- docs/apps/dynacard/reproduction.json: browser application against the module;
- docs/domains/artificial_lift/dynacard-article/article-inputs.json: the example well's
  cards, inputs and shape reading, written by ``--refresh`` from the report analysis.

Charts are inline SVG written here; the page loads nothing from the network. Without
JavaScript every example panel is shown; the inline script adds the hover view.

Usage (from the repository root):
    PYTHONPATH=src python examples/domains/artificial_lift/build_dynacard_article.py [--refresh] [--check]
"""
from __future__ import annotations

import argparse
import csv
import html
import io
import json
import math
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[3]
FOLDER = Path("docs/domains/artificial_lift/dynacard-article")
ARTICLE = FOLDER / "dynacard-article.html"
INPUTS = FOLDER / "article-inputs.json"
CARDS_CSV = FOLDER / "figure-1-cards.csv"
REPORT = Path("examples/report-evals/dynacard-pump-card/results.json")
EXAMPLES = Path("docs/domains/artificial_lift/dynacard-examples/results.json")
REPRODUCTION = Path("docs/apps/dynacard/reproduction.json")

REPO = "https://github.com/vamseeachanta/digitalmodel"
# Pinned public commits of the linked pages: the pump-card report and its prompt, and the
# worked examples with the browser application.
REPORT_COMMIT = "7d02ae07248b038dcdec5e7097565c02fcd152f8"
APP_COMMIT = "feeccc283ec431540dbd0d282b60a7a2402e5475"
PLATE_DOI = "https://doi.org/10.17632/hnfwm9mjs6.1"

REVISION = "Revision 3, 13 Nov 2026"
CLOSING = ("If you have run your own analysis, see a way to improve this one, or have an idea "
           "for a future issue, write to support@aceengineer.com. We read every reply and "
           "credit the ideas we use and provide results we obtained.")

PLAIN = {
    "NORMAL": "normal operation", "FLUID_POUND": "fluid pound",
    "GAS_INTERFERENCE": "gas interference", "GAS_LOCK": "gas lock",
    "VALVE_LEAK_TV": "traveling valve leak", "VALVE_LEAK_SV": "standing valve leak",
    "ROD_PARTING": "rod parting", "PUMP_TAGGING_UP": "tagging up",
    "PUMP_TAGGING_DOWN": "tagging down", "PLUNGER_OUT_OF_BARREL": "plunger out of barrel",
    "SAND_ABRASION": "sand wear", "STUCK_PUMP": "stuck pump", "BENT_BARREL": "bent barrel",
    "WORN_BARREL": "worn barrel", "PARAFFIN_RESTRICTION": "paraffin restriction",
    "DELAYED_TV_CLOSURE": "delayed traveling valve closure",
    "EXCESSIVE_FRICTION": "excessive friction", "EXCESSIVE_VIBRATION": "excessive vibration",
    "PLUNGER_UNDERTRAVEL": "plunger undertravel",
}

GLOSS = {"tagging up": ", the plunger striking at the top of its stroke"}

BLUE, ORANGE, GREY = "#2a78d6", "#eb6834", "#a3a29b"


# ---------------------------------------------------------------- data refresh

def refresh_inputs(root: Path) -> dict:
    """Run the report analysis once and keep what the article draws (needs the solver)."""
    sys.path.insert(0, str(root / "examples/report-evals/dynacard-pump-card"))
    import analysis  # noqa: PLC0415  (report script, not a package)

    result = analysis.run(include_sensitivity=False, resource_root=root)
    cards = result["cards"]

    def card(name):
        return {"position_in": [round(v, 4) for v in cards[name]["position_in"]],
                "load_lbf": [round(v, 2) for v in cards[name]["load_lbf"]]}

    inputs = result["inputs"]
    return {
        "source": "examples/report-evals/dynacard-pump-card/analysis.py run(include_sensitivity=False)",
        "fixture": "tests/marine_ops/artificial_lift/test_data/cleansed_well_001.json",
        "fixture_commit": result["commit"],
        "cards": {"surface": card("surface"), "pump": card("default"),
                  "pump_frequency_domain": card("gibbs")},
        "key_results": {k: round(v, 6) for k, v in result["key_results"].items()},
        "well": {"strokes_per_minute": inputs["strokes_per_minute"],
                 "pump_depth_ft": inputs["pump"]["depth_ft"],
                 "plunger_diameter_in": inputs["pump"]["diameter_in"],
                 "fluid_density_lb_ft3": round(inputs["fluid_density_lb_ft3"], 3),
                 "tubing_pressure_psi": inputs["tubing_pressure_psi"],
                 "rod_string": [{"diameter_in": r["diameter_in"], "length_ft": r["length_ft"]}
                                for r in inputs["rod_string"]]},
        "shape_top_three": [{"label": h["label"], "probability": h["probability"]}
                            for h in result["shape_hypothesis"]["top_three"]],
        "plate_score": result["shape_hypothesis"]["plate_score"],
    }


def figure_csv(inputs: dict) -> str:
    cards = inputs["cards"]
    names = ("surface", "pump", "pump_frequency_domain")
    stream = io.StringIO(newline="")
    writer = csv.writer(stream, lineterminator="\n")
    writer.writerow(["sample", "surface_position_in", "surface_load_lbf", "pump_position_in",
                     "pump_load_lbf", "pump_frequency_domain_position_in",
                     "pump_frequency_domain_load_lbf"])
    for i in range(len(cards["surface"]["position_in"])):
        row = [i]
        for name in names:
            row += [f"{cards[name]['position_in'][i]:.4f}", f"{cards[name]['load_lbf'][i]:.2f}"]
        writer.writerow(row)
    return stream.getvalue()


# ---------------------------------------------------------------- formatting

def esc(text) -> str:
    return html.escape(str(text), quote=True)


def blob(commit: str, path: str) -> str:
    return f"{REPO}/blob/{commit}/{path}"


def tree(commit: str, path: str) -> str:
    return f"{REPO}/tree/{commit}/{path}"


def pct_score(p: float) -> str:
    value = 100 * p
    if value >= 99.5:
        return "over 99 %"
    if value < 0.5:
        return "under 1 %"
    return f"{value:.0f} %"


def series_list(items) -> str:
    items = list(items)
    return items[0] if len(items) == 1 else ", ".join(items[:-1]) + " and " + items[-1]


# ---------------------------------------------------------------- charts

def nice_step(span: float, target: int = 5) -> float:
    raw = span / target
    power = 10 ** math.floor(math.log10(raw))
    for m in (1, 2, 2.5, 5, 10):
        if raw <= m * power:
            return m * power
    return 10 * power


def ticks(low: float, high: float, target: int = 5):
    step = nice_step(high - low, target)
    start = math.floor(low / step) * step
    stop = math.ceil(high / step) * step
    values = []
    v = start
    while v <= stop + 1e-9:
        values.append(round(v, 6))
        v += step
    return values


def fmt_tick(v: float) -> str:
    return f"{v:,.0f}" if abs(v - round(v)) < 1e-9 else f"{v:g}"


def loop_svg(series, *, width, height, label, x_title, y_title) -> str:
    """Closed load-position loops on one linear, to-scale pair of axes."""
    xs = [x for s in series for x in s["x"]]
    ys = [y for s in series for y in s["y"]]
    xt = ticks(min(0.0, min(xs)), max(xs), 5)
    y_low = min(0.0, min(ys))
    if -y_low < 0.03 * (max(ys) - y_low):  # a few lbf below zero does not need a tick below it
        y_low = 0.0
    yt = ticks(y_low, max(ys), 5)
    left, right, top, bottom = 58, 14, 12, 42
    pw, ph = width - left - right, height - top - bottom

    def px(x):
        return left + (x - xt[0]) / (xt[-1] - xt[0]) * pw

    def py(y):
        return top + ph - (y - yt[0]) / (yt[-1] - yt[0]) * ph

    out = [f'<svg viewBox="0 0 {width} {height}" role="img" aria-label="{esc(label)}" '
           f'class="chart" xmlns="http://www.w3.org/2000/svg">']
    for y in yt:
        out.append(f'<line x1="{left}" x2="{left + pw}" y1="{py(y):.1f}" y2="{py(y):.1f}" class="grid"/>')
        out.append(f'<text x="{left - 6}" y="{py(y) + 4:.1f}" class="tick" text-anchor="end">{fmt_tick(y)}</text>')
    for x in xt:
        out.append(f'<text x="{px(x):.1f}" y="{top + ph + 16}" class="tick" text-anchor="middle">{fmt_tick(x)}</text>')
    out.append(f'<line x1="{left}" x2="{left + pw}" y1="{top + ph}" y2="{top + ph}" class="axis"/>')
    out.append(f'<line x1="{left}" x2="{left}" y1="{top}" y2="{top + ph}" class="axis"/>')
    out.append(f'<text x="{left + pw / 2:.1f}" y="{height - 6}" class="axis-title" text-anchor="middle">{esc(x_title)}</text>')
    out.append(f'<text transform="translate(14 {top + ph / 2:.1f}) rotate(-90)" class="axis-title" '
               f'text-anchor="middle">{esc(y_title)}</text>')
    for s in series:
        points = " ".join(f"{px(x):.1f},{py(y):.1f}" for x, y in zip(s["x"], s["y"]))
        out.append(f'<polygon points="{points}" class="{s["cls"]}"><title>{esc(s["name"])}</title></polygon>')
        if s.get("label_at"):
            lx, ly = s["label_at"]
            out.append(f'<text x="{px(lx):.1f}" y="{py(ly):.1f}" class="direct">{esc(s["name"])}</text>')
    out.append("</svg>")
    return "".join(out)


def shifted(card, key_x="position_in", key_y="load_lbf"):
    low = min(card[key_x])
    return [x - low for x in card[key_x]], list(card[key_y])


# ---------------------------------------------------------------- facts

def load_sources(root: Path) -> dict:
    def read(path):
        return json.loads((root / path).read_text(encoding="utf-8"))
    return {"report": read(REPORT), "examples": read(EXAMPLES),
            "reproduction": read(REPRODUCTION), "inputs": read(INPUTS)}


def facts(src: dict) -> dict:
    report = {r["id"]: r["value"] for r in src["report"]["results"]}
    inputs = src["inputs"]
    # The saved cards must be the ones the report results describe.
    for key, decimals in (("pump_stroke_in", 2), ("fluid_load_lbf", 0), ("fillage_pct", 2),
                          ("surface_stroke_in", 2)):
        if round(inputs["key_results"][key], decimals) != round(report[key], decimals):
            raise ValueError(f"article-inputs.json disagrees with the report on {key}; run --refresh")
    records = src["examples"]["examples"]
    matches = [r for r in records if r["diagnosis"]["shape_only"]["classification"] == r["crosswalk_mode"]]
    shipped = [r for r in records if r["diagnosis"]["shipped"]["classification"] == r["crosswalk_mode"]]
    misses = [r for r in records if r not in matches]
    nrmse = [r["round_trip"]["load_nrmse_pct"] for r in records]
    repro = src["reproduction"]["examples"]
    rods = inputs["well"]["rod_string"]
    return {
        "report": report, "records": records, "matches": len(matches), "shipped": len(shipped),
        "misses": misses, "nrmse_min": min(nrmse), "nrmse_max": max(nrmse),
        "repro_load": max(e["max_pump_load_difference_lbf"] for e in repro),
        "repro_labels": all(e["labels_equal"] for e in repro),
        "precision_load": max(abs(r["published_precision_effect"]["fluid_load_difference_lbf"])
                              for r in records),
        "rod_length": sum(r["length_ft"] for r in rods),
        "unused_capacity": report["gross_displacement_bbl_day"] - report["pump_displacement_bbl_day"],
        "basis": src["examples"]["basis"], "inputs": inputs,
    }


# ---------------------------------------------------------------- page parts

def figure_one(f) -> str:
    cards = f["inputs"]["cards"]
    r = f["report"]
    sx, sy = cards["surface"]["position_in"], cards["surface"]["load_lbf"]
    px_, py_ = shifted(cards["pump"])
    svg = loop_svg(
        [{"x": sx, "y": sy, "cls": "card-surface", "name": "Surface card (measured)",
          "label_at": (min(sx) + 1, max(sy) + 350)},
         {"x": px_, "y": py_, "cls": "card-pump", "name": "Pump card (computed)",
          "label_at": (1, max(py_) + 350)}],
        width=680, height=380, label="Surface card and computed pump card of the public example well",
        x_title="Position (in), each card from the bottom of its stroke", y_title="Load (lbf)")
    return (f'<figure id="figure-1">{svg}'
            '<div class="legend"><span class="key k-surface"></span>Surface card, measured'
            '<span class="key k-pump"></span>Pump card, computed</div>'
            '<figcaption><b>Figure 1. Surface card and computed pump card, public example well.</b> '
            f'Surface stroke {r["surface_stroke_in"]:.1f} in, pump stroke {r["pump_stroke_in"]:.1f} in. '
            'Data: <a href="figure-1-cards.csv">figure-1-cards.csv</a>.</figcaption></figure>')


def example_panel(rec) -> str:
    a = rec["analysis"]
    shape = rec["diagnosis"]["shape_only"]
    probs = sorted(shape["probabilities"].items(), key=lambda kv: -kv[1])[:3]
    sx, sy = rec["surface_card"]["position"], rec["surface_card"]["load"]
    cx, cy = shifted(rec["computed_pump_card"], "position", "load")
    gx, gy = shifted(rec["source_pump_card"], "position", "load")
    svg = loop_svg(
        [{"x": sx, "y": sy, "cls": "card-surface", "name": "Surface card"},
         {"x": gx, "y": gy, "cls": "card-plate", "name": "Plate shape"},
         {"x": cx, "y": cy, "cls": "card-pump", "name": "Pump card, computed"}],
        width=420, height=280, label=f"Example {rec['index']}, {rec['english_gloss']}: cards",
        x_title="Position (in)", y_title="Load (lbf)")
    name = PLAIN[shape["classification"]]
    agrees = shape["classification"] == rec["crosswalk_mode"]
    reading = (f"The shape model reads {name} ({pct_score(shape['confidence'])}), which matches the plate label."
               if agrees else
               f"The shape model reads {name} ({pct_score(shape['confidence'])}); the plate label is "
               f"{rec['english_gloss'].lower()}.")
    bars = "".join(
        f'<div class="prob-row"><span class="prob-name">{esc(PLAIN[k])}</span>'
        f'<span class="prob-track"><span class="prob-bar" style="width:{100 * v:.1f}%"></span></span>'
        f'<span class="prob-value">{pct_score(v)}</span></div>' for k, v in probs)
    return (f'<section class="example-panel" aria-live="polite" id="example-{rec["index"]}">'
            f'<h3>Example {rec["index"]}: {esc(rec["english_gloss"])}</h3>{svg}'
            '<div class="legend small"><span class="key k-surface"></span>Surface card'
            '<span class="key k-plate"></span>Plate shape<span class="key k-pump"></span>Pump card, computed</div>'
            f'<div class="probs" aria-label="Shape model, three highest classes">{bars}</div><ul>'
            f'<li>Pump card recovered within {rec["round_trip"]["load_nrmse_pct"]:.1f} % of its load range.</li>'
            f'<li>Pump fills to {a["fillage_pct"]:.1f} % and lifts a fluid load of {a["fluid_load_lbf"]:,.0f} lbf.</li>'
            f'<li>{esc(reading)}</li></ul></section>')


def examples_view(f) -> str:
    rows = []
    for rec in f["records"]:
        shape = rec["diagnosis"]["shape_only"]
        agrees = shape["classification"] == rec["crosswalk_mode"]
        rows.append(
            f'<tr tabindex="0" data-example="{rec["index"]}"><td>{rec["index"]}</td>'
            f'<td>{esc(rec["english_gloss"])}</td>'
            f'<td>{esc(PLAIN[shape["classification"]])} ({pct_score(shape["confidence"])})</td>'
            f'<td class="{"yes" if agrees else "no"}">{"yes" if agrees else "no"}</td>'
            f'<td class="num">{rec["analysis"]["fillage_pct"]:.0f}</td>'
            f'<td class="num">{rec["analysis"]["fluid_load_lbf"]:,.0f}</td></tr>')
    table = ('<div class="table-wrap"><table class="examples"><thead><tr><th>No.</th><th>Condition (plate label)</th>'
             '<th>Model reading</th><th>Match</th><th>Fillage (%)</th><th>Fluid load (lbf)</th>'
             '</tr></thead><tbody>' + "".join(rows) + '</tbody></table></div>')
    panels = "".join(example_panel(rec) for rec in f["records"])
    readme = blob(APP_COMMIT, "docs/domains/artificial_lift/dynacard-examples/README.md")
    return (f'<figure id="figure-2"><div class="examples-view"><div class="examples-table">{table}'
            '<p class="hint">Point at a row, or select it, to show that example.</p></div>'
            f'<div class="examples-panels">{panels}</div></div>'
            '<figcaption><b>Figure 2. Ten pump conditions: model reading and cards.</b> '
            f'Data: one CSV per example in the <a href="{readme}">worked examples</a>.</figcaption></figure>')


def contents(sections) -> str:
    items = "".join(f'<li><a href="#{sid}">{esc(title)}</a></li>' for sid, title in sections)
    return f'<nav class="contents" aria-label="Contents"><p>Contents</p><ol>{items}</ol></nav>'


def body(f) -> str:
    r = f["report"]
    top = f["inputs"]["shape_top_three"][0]
    misses_by = {}
    for rec in f["misses"]:
        misses_by.setdefault(rec["diagnosis"]["shape_only"]["classification"], []).append(
            rec["english_gloss"].lower())
    miss_lines = "; ".join(f"it reads {series_list(names)} as a {PLAIN[k]}"
                           for k, names in misses_by.items())
    report_md = blob(REPORT_COMMIT, "docs/domains/artificial_lift/dynacard-pump-card-report.md")
    app = blob(APP_COMMIT, "docs/apps/dynacard/README.md")
    return f"""
<p class="label">Energy Street · Rod pump diagnostics</p>
<h1>Dynamometer card analysis: from the surface card to the pump card and a diagnosis</h1>
<blockquote class="takeaway">A surface card, measured at the top of a rod-pumped well, carries enough to see the pump at the bottom of the well and to screen that pump from the office.</blockquote>
<div class="opening">
<p>A rod pump works at the bottom of the well, out of sight. What the operator measures is the surface card: the load on the polished rod against its position, over one stroke. That card mixes the pump's work with the weight, stretch and vibration of the rod string, so a pump problem can hide inside it.</p>
<p>The capability shown here takes the rod string out of the picture. From the surface card and the well's rod and pump data, it computes the pump card, reads how far the plunger travels, how full the pump fills and what load it lifts, and gives a first reading of the pump's condition. It runs in a web page with nothing to install, so an engineer or a manager can screen a well from the office and plan the well visit around a specific check.</p>
</div>
<p>Based on public data, two of the cards are shown in Figure 1.</p>
<ul>
<li>The blue loop is the surface card of a public example well, measured at the top of the rod string.</li>
<li>The orange loop is the pump card computed from it, at the bottom of the {f['rod_length']:,.0f} ft rod string.</li>
<li>The rods stretch and vibrate, so the {r['surface_stroke_in']:.1f} in surface stroke becomes a {r['pump_stroke_in']:.1f} in stroke at the pump.</li>
<li>The lower right corner of the pump card is cut away: on the way down, the load leaves the plunger only when the plunger reaches liquid. This is the mark of a pump that is not full.</li>
</ul>
{figure_one(f)}
<h2 id="key-conclusions">Key conclusions</h2>
<p>The key conclusions for the example well are as follows:</p>
<ul>
<li>The pump fills to {r['fillage_pct']:.0f} % of its stroke. At 24 hours a day and no slip, it displaces {r['pump_displacement_bbl_day']:.1f} bbl/day, against {r['gross_displacement_bbl_day']:.1f} bbl/day for a full pump, so about {f['unused_capacity']:.0f} bbl/day of pump capacity is not used.</li>
<li>The pump has more capacity than the liquid reaching it, which supports a review of pumping speed or run time against the well's inflow.</li>
<li>The plunger lifts a fluid load of {r['fluid_load_lbf']:,.0f} lbf on each upstroke.</li>
<li>The pump stroke is {r['pump_stroke_in']:.1f} in, {100 * r['stroke_ratio']:.0f} % of the surface stroke. Rod stretch and rod dynamics take the rest, which the surface card alone does not separate.</li>
<li>The card shape reads as {top['label'].lower()}{GLOSS.get(top['label'].lower(), '')}. This reading is a hypothesis to check at the well; the pump card, the fillage and the fluid load are the result.</li>
</ul>
<p>Two numerical methods are available for the pump card: a finite-difference method and a frequency-domain method. On this well they agree closely, within {r['gibbs_default_load_nrmse_pct']:.2f} % of the load range, so either gives the same result. Both are in the reusable prompt. The comparison is given in Appendix B, and the inputs and limits of the example well in Appendix A.</p>
<h2 id="ten-conditions">Ten pump conditions read from their surface cards</h2>
<p>Based on public data, ten common pump conditions are run through the same analysis, from surface card to pump card to a reading of the pump's condition. They are shown in Figure 2.</p>
<ul>
<li>Every pump card is recovered within {f['nrmse_min']:.1f} to {f['nrmse_max']:.1f} % of its load range.</li>
<li>The shape model names {f['matches']} of the 10 conditions correctly.</li>
<li>Where it misses, {miss_lines}. A model reading is a hypothesis to check at the well.</li>
</ul>
{examples_view(f)}
<p>How the ten examples are built, and the full values, are given in Appendix C.</p>
<h2 id="own-card">Run the analysis on your own card</h2>
<p>The same analysis runs in a web page that works offline. Load a surface card with the well's rod and pump data, and the page draws the pump card and reads fillage, fluid load and the model readings, with the same numbers as the library. The page is in the <a href="{app}">digitalmodel library</a>.</p>
<p><a href="{report_md}">Read the full study</a></p>
<p>To try it out or adapt the analysis, see <a href="#appendix-d">reusable prompt</a>.</p>
<p>The work leaves open the accuracy of the model readings on field cards: both models are trained on synthetic cards, and no labelled field cards were available to test them.</p>
<aside class="offer">AceEngineer runs analyses of this kind as fixed-scope engineering projects.</aside>
<p class="closing">{CLOSING}</p>
<p class="signoff">Prepared: Analyst Deckhand. Checked: Editor Deckhand. Approved: Vamsee Achanta. {REVISION}.</p>
<h2 id="references">References</h2>
<ol class="refs">
<li>Gibbs, S. G. (1963). Predicting the Behavior of Sucker-Rod Pumping Systems. SPE-588-PA. <a href="https://doi.org/10.2118/588-PA">doi:10.2118/588-PA</a>.</li>
<li>Everitt, T. A. and Jennings, J. W. (1992). An Improved Finite-Difference Calculation of Downhole Dynamometer Cards for Sucker-Rod Pumps. SPE-18189-PA. <a href="https://doi.org/10.2118/18189-PA">doi:10.2118/18189-PA</a>.</li>
<li>Diagnostic plate of ten pump working conditions. Mendeley Data, version 1, CC0 1.0. <a href="{PLATE_DOI}">doi:10.17632/hnfwm9mjs6.1</a>, retrieved 11 Oct 2026.</li>
<li>digitalmodel library, MIT licence. <a href="{REPO}">github.com/vamseeachanta/digitalmodel</a>.</li>
</ol>
"""


def table(headers, rows, caption) -> str:
    head = "".join(f"<th>{esc(h)}</th>" for h in headers)
    body_rows = "".join("<tr>" + "".join(f"<td>{c}</td>" for c in row) + "</tr>" for row in rows)
    return (f'<div class="table-wrap"><table class="data"><thead><tr>{head}</tr></thead><tbody>{body_rows}</tbody></table></div>'
            f'<p class="caption"><b>{caption}</b></p>')


def appendices(f) -> str:
    r = f["report"]
    w = f["inputs"]["well"]
    plate = f["inputs"]["plate_score"]
    top = f["inputs"]["shape_top_three"]
    rods = w["rod_string"]
    inputs_rows = [
        ["Pumping speed", f"{w['strokes_per_minute']:.2f}", "SPM"],
        ["Pump depth", f"{w['pump_depth_ft']:,.0f}", "ft"],
        ["Plunger diameter", f"{w['plunger_diameter_in']:.3f}", "in"],
        ["Rod string, surface first", "; ".join(f"{x['diameter_in']:.3f} in × {x['length_ft']:,.0f} ft" for x in rods), "in; ft"],
        ["Fluid density", f"{w['fluid_density_lb_ft3']:.3f}", "lb/ft³"],
        ["Tubing pressure", f"{w['tubing_pressure_psi']:.0f}", "psi"],
    ]
    result_rows = [
        ["Surface stroke", f"{r['surface_stroke_in']:.2f}", "in"],
        ["Peak and minimum polished-rod load", f"{r['peak_polished_rod_load_lbf']:,.0f}; {r['minimum_polished_rod_load_lbf']:,.0f}", "lbf"],
        ["Pump stroke", f"{r['pump_stroke_in']:.2f}", "in"],
        ["Effective plunger travel", f"{r['effective_travel_in']:.2f}", "in"],
        ["Pump fillage", f"{r['fillage_pct']:.2f}", "%"],
        ["Fluid load", f"{r['fluid_load_lbf']:,.0f}", "lbf"],
        ["Maximum and minimum pump load", f"{r['maximum_pump_load_lbf']:,.0f}; {r['minimum_pump_load_lbf']:,.0f}", "lbf"],
        ["Displacement, fillage-adjusted and full pump", f"{r['pump_displacement_bbl_day']:.2f}; {r['gross_displacement_bbl_day']:.2f}", "bbl/day"],
    ]
    a = f"""
<h2 id="appendix-a">Appendix A Example well: inputs, results and limits</h2>
<p>The data, results and limits of the public example well are given in this appendix. The well is one anonymised field well in the digitalmodel library.</p>
{table(["Input", "Value", "Unit"], inputs_rows, "Table A1. Example well inputs, from the well record.")}
{table(["Result", "Value", "Unit"], result_rows, "Table A2. Example well results, finite-difference method; displacement at 24 h/day and no slip.")}
<ul>
<li>The well record labels this card a shutdown card. The source system reports that its own corner detection, which finds the points where the valves open and close, failed on it, so the fillage and the fluid load rest on the corner detection of the digitalmodel library alone.</li>
<li>The rod string in the record ends at {f['rod_length']:,.0f} ft and the pump is at {w['pump_depth_ft']:,.0f} ft. The end of the rod string is taken as the pump; the {w['pump_depth_ft'] - f['rod_length']:,.0f} ft between them is not represented.</li>
<li>The shape model gives {top[0]['label'].lower()} a model score of {100 * top[0]['probability']:.2f} %. The score is the model's own output, not the chance that the fault is present. The model is trained on synthetic cards; on the ten plate shapes it is correct on {plate['including_assumed']['correct']} of {plate['including_assumed']['count']} ({plate['literal']['correct']} of the {plate['literal']['count']} whose plate label has the same name as a model class). The plate shapes also shape the synthetic training cards, so they are not an independent test.</li>
<li>The inputs that matter most: a 1 % larger rod diameter lowers the maximum pump load by about {abs(r['factor_1pct_rod_diameter_maximum_pump_load_lbf']):.1f} % and the fillage by about {abs(r['factor_1pct_rod_diameter_fillage_pct']):.1f} %; a 1 % load-cell gain error raises the maximum pump load by about {r['factor_1pct_surface_load_scale_maximum_pump_load_lbf']:.1f} %; a 1 % surface position scale error moves the pump stroke by about {r['factor_1pct_surface_position_scale_pump_stroke_in']:.1f} %. The full sensitivity table is in the full study.</li>
<li>Limits: unanchored tubing is not modelled and only steel rods are accepted. The frequency-domain method treats the well as vertical. A card alone does not give production; run time and slip are needed. The web page assumes a vertical well, anchored tubing and samples evenly spaced in time. Fluid load and fillage step with the detected corner samples, so rounding the last digit of a card can move the fluid load by tens of lbf: {f['precision_load']:.0f} lbf at most in the ten examples.</li>
</ul>"""
    b_rows = [["Frequency domain", "Finite difference (default)", f"{r['gibbs_default_load_nrmse_pct']:.2f}"],
              ["Finite difference (default)", "Card stored in the well record", f"{r['default_reference_load_nrmse_pct']:.2f}"],
              ["Frequency domain", "Card stored in the well record", f"{r['gibbs_reference_load_nrmse_pct']:.2f}"]]
    b = f"""
<h2 id="appendix-b">Appendix B The two numerical methods compared</h2>
<p>The two methods that compute the pump card are compared in this appendix.</p>
<ul>
<li>Finite-difference method, the default: it marches the damped wave equation for axial motion through the tapered rod string, after Everitt and Jennings (1992) [2].</li>
<li>Frequency-domain method: it solves the rod model of Gibbs (1963) [1] harmonic by harmonic, with 15 harmonics, and treats the well as vertical.</li>
<li>The well record also stores a pump card from the source system's own finite-difference calculation.</li>
</ul>
{table(["Pump card", "Compared with", "Load difference (% of load range)"], b_rows, "Table B1. Root-mean-square load difference between pump cards.")}
<p>The two methods agree to {r['gibbs_default_load_nrmse_pct']:.2f} % of the load range, and each is within {max(r['default_reference_load_nrmse_pct'], r['gibbs_reference_load_nrmse_pct']):.2f} % of the stored card. The stored card uses damping close to that of the default method, so its agreement with that method is a regression check; the frequency-domain method is the independent one. All results in this article use the default method.</p>"""
    c_rows = []
    for rec in f["records"]:
        an, d = rec["analysis"], rec["diagnosis"]
        c_rows.append([str(rec["index"]), esc(rec["english_gloss"]), f"{an['surface_stroke_in']:.2f}",
                       f"{an['pump_gross_stroke_in']:.2f}", f"{an['fillage_pct']:.1f}",
                       f"{an['fluid_load_lbf']:,.0f}", f"{rec['round_trip']['load_nrmse_pct']:.2f}",
                       esc(PLAIN[d['shape_only']['classification']]), esc(PLAIN[d['shipped']['classification']])])
    basis = f["basis"]
    readme = blob(APP_COMMIT, "docs/domains/artificial_lift/dynacard-examples/README.md")
    matrix = blob(APP_COMMIT, "docs/domains/artificial_lift/dynacard-examples/diagnostics-matrix.png")
    c = f"""
<h2 id="appendix-c">Appendix C The ten examples in detail</h2>
<p>How the ten examples are built and their full values are given in this appendix.</p>
<ul>
<li>Each of the ten shapes of the public diagnostic plate [3] is scaled to an assumed {basis['stroke_in']['value']:.0f} in pump stroke and to the static fluid load of a {w['plunger_diameter_in']:.1f} in plunger at {w['pump_depth_ft']:,.0f} ft ({basis['load_range_lbf']['value']:,.0f} lbf). The plate gives shapes only, with no stroke or load.</li>
<li>Each scaled shape is lifted to a surface card through the rod string of the example well and then analysed like a measured card. The surface cards are therefore synthesised, not measured.</li>
<li>The plate labels insufficient supply and sand production have no model class of the same name; they are matched to fluid pound and sand wear by assumption.</li>
<li>The shape model names {f['matches']} of the 10 conditions; the second, shipped model names {f['shipped']}. Model scores are the models' own outputs, not the chance that a fault is present.</li>
<li>The web page reproduces the library on all ten examples: pump loads within {f['repro_load']:.0e} lbf and {"the same" if f['repro_labels'] else "different"} model labels.</li>
</ul>
{table(["No.", "Condition", "Surface stroke (in)", "Pump stroke (in)", "Fillage (%)", "Fluid load (lbf)", "Round trip (% of load range)", "Shape model", "Shipped model"], c_rows, "Table C1. Ten worked examples, finite-difference method.")}
<p>The probabilities of all model classes are shown in the <a href="{matrix}">diagnostics matrix</a>; each example's figure and data are in the <a href="{readme}">worked examples</a>.</p>"""
    prompt = blob(REPORT_COMMIT, "examples/report-evals/dynacard-pump-card/PROMPT.md")
    fixture = blob(f["inputs"]["fixture_commit"], f["inputs"]["fixture"])
    d = f"""
<h2 id="appendix-d">Appendix D Reusable prompts</h2>
<p>The reusable prompt lets your team, or your assistant, rebuild this analysis from public code and then run it on a card of your own. To have a study of your own rod pump done for you instead, see below.</p>
<p><b>What you need to run this</b></p>
<ul>
<li>Data required: none beyond the public data below.</li>
<li>Data required to adapt: a surface card over one stroke, the rod string, the pump depth and plunger diameter, and the pumping speed.</li>
<li>Paid or behind an account: none.</li>
<li>Changes over time: none; the code is pinned to one commit.</li>
</ul>
<p>Give this prompt to your assistant: <a href="{prompt}">reusable prompt for the pump card analysis</a>, in the digitalmodel library.</p>
<p>Data: public example well, file <a href="{fixture}">cleansed_well_001.json</a> of the digitalmodel library, MIT licence, version of 12 Nov 2026; public diagnostic plate, Mendeley Data, <a href="{PLATE_DOI}">doi:10.17632/hnfwm9mjs6.1</a>, CC0 1.0, retrieved 11 Oct 2026.</p>
<p>To have a study of your own rod pump done for you, write to support@aceengineer.com.</p>"""
    return a + b + c + d


CSS = """
:root{--ink:#0b0b0b;--ink2:#52514e;--muted:#8a8984;--line:#e4e3de;--surface:#fcfcfb;--navy:#1d3557;--teal:#1b8a8f;
--blue:%s;--orange:%s;--grey:%s}
*{box-sizing:border-box}
body{margin:0;background:#fff;color:var(--ink);font:16px/1.6 Georgia,'Times New Roman',serif}
.page{max-width:1180px;margin:0 auto;padding:24px 20px 60px;display:grid;grid-template-columns:minmax(0,760px) 220px;gap:40px;justify-content:center}
main{min-width:0}
main{grid-column:1;grid-row:1}.contents{grid-column:2;grid-row:1;position:sticky;top:20px;align-self:start;font:14px/1.5 system-ui,sans-serif;color:var(--ink2);border-left:2px solid var(--line);padding-left:14px}
.contents p{margin:0 0 6px;font-weight:600}.contents ol{margin:0;padding-left:18px}
@media(max-width:1060px){.page{display:block;max-width:800px}.contents{position:static;margin-bottom:16px}}
.table-wrap{overflow-x:auto;max-width:100%%}
#deckhand-opening{border:2px dashed var(--teal);border-radius:8px;min-height:120px;display:flex;align-items:center;justify-content:center;
text-align:center;color:var(--teal);font:14px/1.4 system-ui,sans-serif;padding:16px;margin-bottom:20px}
.label{font:600 13px system-ui,sans-serif;letter-spacing:.04em;text-transform:uppercase;color:var(--teal);margin:0}
h1{font-size:30px;line-height:1.25;margin:6px 0 16px;color:var(--navy)}
h2{font-size:22px;color:var(--navy);margin:36px 0 10px}h3{font-size:16px;margin:0 0 6px;font-family:system-ui,sans-serif}
blockquote.takeaway{margin:0 0 20px;padding:12px 18px;border-left:4px solid var(--teal);background:#f3f8f8;font-size:17px}
figure{margin:18px 0}figcaption,.caption{font:13px/1.45 system-ui,sans-serif;color:var(--ink2);margin-top:6px}
.chart{width:100%%;height:auto;display:block;background:var(--surface)}
.chart .grid{stroke:var(--line);stroke-width:1}.chart .axis{stroke:var(--muted);stroke-width:1}
.chart .tick{font:11px system-ui,sans-serif;fill:var(--ink2)}.chart .axis-title{font:12px system-ui,sans-serif;fill:var(--ink2)}
.chart .direct{font:12px system-ui,sans-serif;fill:var(--ink)}
.chart polygon{fill:none;stroke-linejoin:round}
.card-surface{stroke:var(--blue);stroke-width:2}.card-pump{stroke:var(--orange);stroke-width:2.2}
.card-plate{stroke:var(--grey);stroke-width:6;stroke-opacity:.45}
.legend{font:13px system-ui,sans-serif;color:var(--ink2);margin-top:4px}.legend.small{font-size:12px}
.key{display:inline-block;width:18px;height:0;border-top:3px solid;vertical-align:middle;margin:0 6px 0 12px}
.key:first-child{margin-left:0}.k-surface{border-color:var(--blue)}.k-pump{border-color:var(--orange)}.k-plate{border-color:var(--grey);border-top-width:6px;opacity:.6}
table{border-collapse:collapse;width:100%%;font:13px/1.4 system-ui,sans-serif}
th,td{border-bottom:1px solid var(--line);padding:5px 6px;text-align:left;vertical-align:top}th{color:var(--ink2);font-weight:600}
td.num{text-align:right;font-variant-numeric:tabular-nums}td.yes{color:#1b6e3a}td.no{color:#9a3412}
table.examples tbody tr{cursor:pointer}table.examples tbody tr:hover,table.examples tbody tr:focus,table.examples tbody tr.active{background:#eef4fb;outline:none}
.hint{font:12px system-ui,sans-serif;color:var(--muted);margin:6px 0 0;display:none}
.examples-panels{display:grid;grid-template-columns:repeat(auto-fill,minmax(300px,1fr));gap:18px;margin-top:16px}
.example-panel ul{font-size:14px;padding-left:18px;margin:6px 0 0}
.probs{font:12px system-ui,sans-serif;margin-top:8px}.prob-row{display:grid;grid-template-columns:150px 1fr 64px;gap:8px;align-items:center;margin:2px 0}
.prob-track{background:#f0efec;height:8px;border-radius:4px;overflow:hidden}.prob-bar{display:block;height:8px;background:var(--orange);border-radius:4px}
.prob-value{text-align:right;color:var(--ink2)}
.js .hint{display:block}
.js .examples-view{display:grid;grid-template-columns:minmax(0,1fr) minmax(0,1fr);gap:20px;align-items:start}
.js .examples-panels{display:block;margin-top:0;position:sticky;top:16px}
.js .example-panel{display:none}.js .example-panel.active{display:block}
@media(max-width:820px){.js .examples-view{grid-template-columns:1fr}.js .examples-panels{position:static}}
aside.offer{border:1px solid var(--line);border-radius:6px;padding:10px 14px;font:14px system-ui,sans-serif;color:var(--ink2);margin:16px 0}
.signoff{font:12px system-ui,sans-serif;color:var(--muted)}
ol.refs{font-size:14px}
@media print{.contents,.hint{display:none}.page{display:block}}
""" % (BLUE, ORANGE, GREY)

SCRIPT = """
(function(){
var root=document.documentElement;root.className+=' js';
var rows=Array.prototype.slice.call(document.querySelectorAll('tr[data-example]'));
function show(n){
  rows.forEach(function(r){var on=r.getAttribute('data-example')===n;r.classList.toggle('active',on);r.setAttribute('aria-selected',on?'true':'false');});
  Array.prototype.forEach.call(document.querySelectorAll('.example-panel'),function(p){p.classList.toggle('active',p.id==='example-'+n);});
}
rows.forEach(function(r){var n=r.getAttribute('data-example');
  r.addEventListener('mouseenter',function(){show(n);});r.addEventListener('focus',function(){show(n);});
  r.addEventListener('click',function(){show(n);});
  r.addEventListener('keydown',function(e){var i=rows.indexOf(r);
    if(e.key==='ArrowDown'&&i<rows.length-1){rows[i+1].focus();e.preventDefault();}
    if(e.key==='ArrowUp'&&i>0){rows[i-1].focus();e.preventDefault();}});
});
if(rows.length){show(rows[0].getAttribute('data-example'));}
})();
"""


def build_html(root: Path = ROOT) -> str:
    f = facts(load_sources(root))
    sections = [("key-conclusions", "Key conclusions"), ("ten-conditions", "Ten pump conditions"),
                ("own-card", "Run it on your own card"), ("references", "References"),
                ("appendix-a", "A Example well"), ("appendix-b", "B Two methods"),
                ("appendix-c", "C Ten examples"), ("appendix-d", "D Reusable prompts")]
    csp = ("default-src 'none'; style-src 'unsafe-inline'; script-src 'unsafe-inline'; "
           "img-src data:; connect-src 'none'; base-uri 'none'; form-action 'none'")
    return (
        "<!DOCTYPE html>\n<html lang=\"en\">\n<head>\n<meta charset=\"utf-8\">\n"
        f"<meta http-equiv=\"Content-Security-Policy\" content=\"{csp}\">\n"
        "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n"
        "<title>Dynamometer card analysis: from the surface card to the pump card and a diagnosis</title>\n"
        "<meta name=\"description\" content=\"A surface card from a rod-pumped well turned into the pump card, "
        "its fillage and fluid load, and a first reading of the pump's condition, on public data.\">\n"
        f"<style>{CSS}</style>\n</head>\n<body>\n<div class=\"page\">\n"
        + contents(sections) + "\n<main>\n"
        "<div id=\"deckhand-opening\" data-slot=\"deckhand-opening\" role=\"img\" "
        "aria-label=\"Deckhand opening graphic\">Deckhand opening graphic: the shared hand and mooring "
        "rope graphic with the Deckhand logo is placed here at publication.</div>\n"
        + body(f) + appendices(f) +
        "\n</main>\n</div>\n"
        f"<script>{SCRIPT}</script>\n</body>\n</html>\n")


def main(argv=None) -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--refresh", action="store_true",
                        help="rerun the report analysis and rewrite article-inputs.json (needs the solver)")
    parser.add_argument("--check", action="store_true", help="compare outputs without writing")
    args = parser.parse_args(argv)
    outputs = {}
    if args.refresh:
        inputs = refresh_inputs(ROOT)
        outputs[INPUTS] = json.dumps(inputs, indent=1, ensure_ascii=False) + "\n"
        if not args.check:
            (ROOT / INPUTS).parent.mkdir(parents=True, exist_ok=True)
            (ROOT / INPUTS).write_text(outputs[INPUTS], encoding="utf-8", newline="\n")
    inputs = json.loads((ROOT / INPUTS).read_text(encoding="utf-8"))
    outputs[CARDS_CSV] = figure_csv(inputs)
    outputs[ARTICLE] = build_html(ROOT)
    if args.check:
        stale = [str(p) for p, text in outputs.items()
                 if not (ROOT / p).exists() or (ROOT / p).read_text(encoding="utf-8") != text]
        for path in stale:
            print(f"stale: {path}")
        return 1 if stale else 0
    for path, text in outputs.items():
        (ROOT / path).write_text(text, encoding="utf-8", newline="\n")
        print(f"wrote {path}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
