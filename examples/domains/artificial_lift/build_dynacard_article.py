"""Build the dynacard article (revision 4) as one static, offline HTML page, and its
detail pages in this repository.

Inputs, all saved in this repository:
- examples/report-evals/dynacard-pump-card/results.json: public example well results;
- docs/domains/artificial_lift/dynacard-examples/results.json: the ten worked examples;
- docs/apps/dynacard/reproduction.json: browser application against the module;
- docs/domains/artificial_lift/dynacard-article/article-inputs.json: the example well's
  cards, inputs and shape reading, written by ``--refresh`` from the report analysis.

Outputs: the article page, the Figure 1 data, and the Markdown detail pages that the
article's appendices summarise and link (README, example well, numerical methods, ten
examples, reusable prompts). The article links the detail pages at DETAIL_COMMIT.

Charts are inline SVG written here; the page loads nothing from the network. Without
JavaScript every example panel is shown; the inline script adds the hover view, opens on
DEFAULT_EXAMPLE and marks the selected row.

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
# Detail pages: each holds in full what one appendix of the article summarises.
PAGES = {
    "readme": FOLDER / "README.md",
    "example_well": FOLDER / "example-well.md",
    "methods": FOLDER / "numerical-methods.md",
    "examples": FOLDER / "ten-examples.md",
    "prompts": FOLDER / "reusable-prompts.md",
}
PROMPT = Path("examples/report-evals/dynacard-pump-card/PROMPT.md")
REPORT_PAGE = Path("docs/domains/artificial_lift/dynacard-pump-card-report.md")
BUILDER = Path("examples/domains/artificial_lift/build_dynacard_article.py")
EXAMPLES_BUILDER = Path("examples/domains/artificial_lift/build_dynacard_worked_examples.py")

REPO = "https://github.com/vamseeachanta/digitalmodel"
# Pinned public commits of the linked pages: the pump-card report and its prompt, and the
# worked examples with the browser application (the test checks that the linked app,
# worked-example and fixture files have not changed since APP_COMMIT).
REPORT_COMMIT = "7d02ae07248b038dcdec5e7097565c02fcd152f8"
APP_COMMIT = "f5c0009c6dd9e009cdac636d3dfbd7ae0ec1f79c"
APP_VERSION_DATE = "14 Nov 2026"  # UTC date of APP_COMMIT
# Commit that added figure-1-cards.csv; the test checks the file has not changed since.
CARDS_COMMIT = "242f931efefa8facbe337502a2cdb4cebe08fd78"
PLATE_DOI = "https://doi.org/10.17632/hnfwm9mjs6.1"
# Commit that holds the detail pages as published; the test checks they have not changed
# since. None until the pages are first committed (links then follow the branch).
DETAIL_COMMIT = "f41219d99c28ae2e12e883a176603b932df7dd38"
DETAIL_BRANCH = "feat/dynacard-article-r3"
# The example selected when the page opens: not Example 1, so the reader sees at once
# that selecting a row changes the row colour and the chart (maintainer review of revision 3, note 4).
DEFAULT_EXAMPLE = 3

REVISION = "Revision 4, 18 Nov 2026"
REVISION_SHORT = "revision 4 of 18 Nov 2026"
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


def detail(key: str) -> str:
    """Link to a detail page, pinned to DETAIL_COMMIT once the pages are committed."""
    return blob(DETAIL_COMMIT or DETAIL_BRANCH, PAGES[key].as_posix())


def repo_link(key: str) -> str:
    name = PAGES[key].name
    return f'<a href="{detail(key)}">digitalmodel on GitHub, {esc(name)}</a>'


def caption(notes, title) -> str:
    """Numbered notes first, the figure title below them (FW-FIG-14)."""
    js_attr = ' class="js-only"'
    items = "".join(f"<li{js_attr if js else ''}>{text}</li>" for text, js in notes)
    return (f'<figcaption><ol class="fig-notes">{items}</ol>'
            f'<p class="fig-title"><b>{title}</b></p></figcaption>')


def pct_score(p: float) -> str:
    value = 100 * p
    if value >= 99.5:
        return "over 99 %"
    if value < 0.5:
        return "under 1 %"
    return f"{value:.0f} %"


def upper_bound(value: float) -> str:
    """One-digit upper bound in plain notation, for example 2.29e-09 -> 3 × 10⁻⁹."""
    exponent = math.floor(math.log10(value))
    digit = math.ceil(value / 10 ** exponent - 1e-12)
    if digit == 10:
        digit, exponent = 1, exponent + 1
    sup = str(exponent).translate(str.maketrans("-0123456789", "⁻⁰¹²³⁴⁵⁶⁷⁸⁹"))
    return f"{digit} × 10{sup}"


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
            + caption([(f'Surface stroke {r["surface_stroke_in"]:.1f} in, pump stroke {r["pump_stroke_in"]:.1f} in; '
                        'each card is drawn from the bottom of its stroke.', False),
                       (f'Data: <a href="{blob(CARDS_COMMIT, CARDS_CSV.as_posix())}">figure-1-cards.csv</a>, '
                        'version of 13 Nov 2026.', False)],
                      "Figure 1. Surface card and computed pump card, public example well.")
            + '</figure>')


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
    return (f'<section class="example-panel" id="example-{rec["index"]}">'
            f'<h3>Example {rec["index"]}: {esc(rec["english_gloss"])}</h3>'
            f'<p class="select-note">Example {rec["index"]} is selected in the table. '
            f'Select another row to show its cards and reading.</p>{svg}'
            '<div class="legend small"><span class="key k-surface"></span>Surface card'
            '<span class="key k-plate"></span>Plate shape<span class="key k-pump"></span>Pump card, computed</div>'
            f'<div class="probs" aria-label="Shape model, three highest classes">{bars}</div><ul>'
            f'<li>Pump card recovered within {rec["round_trip"]["load_nrmse_pct"]:.1f} % of its load range.</li>'
            f'<li>Fillage reads {a["fillage_pct"]:.1f} % and fluid load {a["fluid_load_lbf"]:,.0f} lbf.</li>'
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
            f'<td class="{"yes" if agrees else "no"}">{("yes, assumed" if rec["crosswalk_kind"] == "assumed" else "yes") if agrees else "no"}</td>'
            f'<td class="num">{rec["analysis"]["fillage_pct"]:.0f}</td>'
            f'<td class="num">{rec["analysis"]["fluid_load_lbf"]:,.0f}</td></tr>')
    table = ('<div class="table-wrap"><table class="examples"><thead><tr><th>No.</th><th>Condition (plate label)</th>'
             '<th>Model reading</th><th>Match</th><th>Fillage (%)</th><th>Fluid load (lbf)</th>'
             '</tr></thead><tbody>' + "".join(rows) + '</tbody></table></div>')
    panels = "".join(example_panel(rec) for rec in f["records"])
    readme = blob(APP_COMMIT, "docs/domains/artificial_lift/dynacard-examples/README.md")
    default = next(r for r in f["records"] if r["index"] == DEFAULT_EXAMPLE)
    notes = [
        (f'Example {DEFAULT_EXAMPLE}, {esc(default["english_gloss"].lower())}, is selected when the page opens. '
         'Pointing at a row also selects it; the arrow keys move between rows.', True),
        ('Each panel shows the surface card (blue), the plate shape (grey) and the computed pump card '
         "(orange), with the shape model's three highest classes.", False),
        ('Match "yes, assumed": the plate label is matched by assumption to a model class of another name.', False),
        (f'Data: one CSV per example in the <a href="{readme}">worked examples</a>, version of {APP_VERSION_DATE}.', False),
    ]
    title = ('Figure 2. Ten pump conditions: model reading and cards'
             '<span class="js-only">. Select a row in the table to show that example</span>.')
    return (f'<figure id="figure-2" data-default="{DEFAULT_EXAMPLE}">'
            f'<div class="examples-view"><div class="examples-table">{table}</div>'
            f'<div class="examples-panels" aria-live="polite">{panels}</div></div>'
            + caption(notes, title) + '</figure>')


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
<p>A rod pump works at the bottom of the well, out of sight. What the operator measures is the surface card: the load on the polished rod against its position, over one stroke. The card data contains all of this information at once: the pump's work, and the weight, stretch and vibration of the rod string, so a pump problem can hide inside it.</p>
<p>To keep the problem simple, the capability shown here takes the rod string out of the picture. From the surface card and the well's rod and pump data, it computes the pump card, reads how far the plunger travels, how full the pump fills and what load it lifts, and gives a first reading of the pump's condition. It runs in a web page, so a well can be screened from the office and the well visit planned around a specific check. The rod string itself is the subject of later articles in this rod pump series.</p>
</div>
<p>Based on public data, two of the cards are shown in Figure 1.</p>
<ul>
<li>The blue loop is the surface card of a public example well, measured at the top of the rod string.</li>
<li>The orange loop is the pump card computed from it, at the bottom of the {f['rod_length']:,.0f} ft rod string.</li>
<li>The rods stretch and vibrate, so the {r['surface_stroke_in']:.1f} in surface stroke becomes a {r['pump_stroke_in']:.1f} in stroke at the pump.</li>
</ul>
{figure_one(f)}
<h2 id="key-conclusions">Key conclusions</h2>
<p>The key conclusions for the example well are as follows:</p>
<ul>
<li>The pump fills to {r['fillage_pct']:.0f} % of its stroke. At 24 hours a day and no slip, it displaces {r['pump_displacement_bbl_day']:.1f} bbl/day, against {r['gross_displacement_bbl_day']:.1f} bbl/day for a full pump, so about {f['unused_capacity']:.0f} bbl/day of pump capacity is not used. The cause, whether inflow, gas, valve leakage or a corner-detection error, is a check for the well.</li>
<li>The plunger lifts a fluid load of {r['fluid_load_lbf']:,.0f} lbf on each upstroke.</li>
<li>The pump stroke is {r['pump_stroke_in']:.1f} in, {100 * r['stroke_ratio']:.0f} % of the surface stroke. Rod stretch and rod dynamics take the rest, which the surface card alone does not separate.</li>
<li>The card shape reads as {top['label'].lower()}{GLOSS.get(top['label'].lower(), '')}. This reading is a hypothesis to check at the well; the pump card, the fillage and the fluid load are the result.</li>
</ul>
<p>Two numerical methods are available for the pump card: a finite-difference method and a frequency-domain method. On this well their pump cards agree within {r['gibbs_default_load_nrmse_pct']:.2f} % of the load range, so for this problem the two methods are equivalent. Appendices A and B give the example well and the comparison.</p>
<h2 id="ten-conditions">Ten pump conditions read from their surface cards</h2>
<p>Based on public data, ten common pump conditions are run through the same analysis, from surface card to pump card to a reading of the pump's condition. Each surface card is built from a published card shape, taken as a pump card by assumption, at an assumed stroke and load, so the cards illustrate shapes rather than field wells. They are shown in Figure 2.</p>
<ul>
<li>Every pump card is recovered within {f['nrmse_min']:.1f} to {f['nrmse_max']:.1f} % of its load range.</li>
<li>The shape model names {f['matches']} of the 10 conditions correctly.</li>
<li>Where it misses, {miss_lines}.</li>
</ul>
{examples_view(f)}
<p>How the ten examples are built, and the full values, are given in Appendix C.</p>
<h2 id="own-card">Run the analysis on your own card</h2>
<p>The same analysis runs in a web page in the <a href="{app}">digitalmodel library</a> (version of {APP_VERSION_DATE}); it needs no installation and no network once its files are on disk. Load a surface card with the well's rod and pump data, and the page draws the pump card and reads fillage, fluid load and the model readings.</p>
<p><a href="{report_md}">Read the full study</a> (version of 12 Nov 2026)</p>
<p>To try it out or adapt the analysis, see <a href="#appendix-d">reusable prompt</a>.</p>
<p>The work leaves open the accuracy of the model readings on field cards: both models are trained on generated cards, and no labelled field cards were available to test them.</p>
<aside class="offer">AceEngineer runs analyses of this kind as fixed-scope engineering projects.</aside>
<p class="closing">{CLOSING}</p>
<p class="signoff">Prepared: Analyst Deckhand. Checked: Editor Deckhand. Approved: Vamsee Achanta. {REVISION}.</p>
<h2 id="references">References</h2>
<ol class="refs">
<li>Gibbs, S. G. (1963). Predicting the Behavior of Sucker-Rod Pumping Systems. SPE-588-PA. <a href="https://doi.org/10.2118/588-PA">doi:10.2118/588-PA</a>.</li>
<li>Everitt, T. A. and Jennings, J. W. (1992). An Improved Finite-Difference Calculation of Downhole Dynamometer Cards for Sucker-Rod Pumps. SPE-18189-PA. <a href="https://doi.org/10.2118/18189-PA">doi:10.2118/18189-PA</a>.</li>
<li>Diagnostic plate of ten pump working conditions. Mendeley Data, version 1, CC0 1.0. <a href="{PLATE_DOI}">doi:10.17632/hnfwm9mjs6.1</a>, retrieved 11 Oct 2026.</li>
<li>digitalmodel library, MIT licence, version of {APP_VERSION_DATE}. <a href="{tree(APP_COMMIT, "")}">github.com/vamseeachanta/digitalmodel</a>.</li>
</ol>
"""


def md_table(headers, rows, caption_text) -> str:
    """Markdown table with its caption below it (engineering register rule 6)."""
    lines = ["| " + " | ".join(headers) + " |", "| " + " | ".join("---" for _ in headers) + " |"]
    lines += ["| " + " | ".join(str(c) for c in row) + " |" for row in rows]
    return "\n".join(lines) + f"\n\n{caption_text}\n"


def well_rows(f):
    r = f["report"]
    w = f["inputs"]["well"]
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
    return inputs_rows, result_rows


def method_figures(f):
    r = f["report"]
    spread = max(r["default_reference_load_nrmse_pct"], r["gibbs_reference_load_nrmse_pct"])
    return r["gibbs_default_load_nrmse_pct"], spread


def appendices(f) -> str:
    """Short appendices: a summary and a pinned link to the detail page (maintainer review of revision 3, notes 5 to 7)."""
    r = f["report"]
    w = f["inputs"]["well"]
    pair, spread = method_figures(f)
    a = f"""
<h2 id="appendix-a">Appendix A Example well: inputs, results and limits</h2>
<p>The example well behind Figure 1 and the key conclusions is summarised in this appendix.</p>
<ul>
<li>The well is one anonymised field well in the digitalmodel library: a {w['plunger_diameter_in']:.3f} in plunger at {w['pump_depth_ft']:,.0f} ft on a two-taper steel rod string, pumped at {w['strokes_per_minute']:.2f} strokes a minute. Its record marks this card as a shutdown card that the source system could not analyse, so the fillage and the fluid load rest on this calculation alone.</li>
<li>The inputs that matter most: a 1 % larger rod diameter lowers the maximum pump load by about {abs(r['factor_1pct_rod_diameter_maximum_pump_load_lbf']):.1f} %, and a 1 % load-cell gain error raises it by about {r['factor_1pct_surface_load_scale_maximum_pump_load_lbf']:.1f} %.</li>
<li>Unanchored tubing is not modelled and only steel rods are accepted; the frequency-domain method and the web page treat the well as vertical.</li>
</ul>
<p>Inputs, results, sensitivity and limits in full: {repo_link("example_well")}.</p>"""
    b = f"""
<h2 id="appendix-b">Appendix B The two numerical methods compared</h2>
<p>The key conclusion of the comparison of the two methods is given in this appendix.</p>
<ul>
<li>A finite-difference method, the default, and a frequency-domain method compute the pump card. On the example well their pump cards differ by {pair:.2f} % of the load range, and each is within {spread:.2f} % of the card stored in the well record.</li>
<li>For this problem, at this accuracy, the methods are equivalent. All results in this article use the default method.</li>
<li>As more types of card come in, the two methods are kept side by side: some physical behaviour may be captured better by one method than by the other.</li>
</ul>
<p>Methods and comparison in full: {repo_link("methods")}.</p>"""
    c = f"""
<h2 id="appendix-c">Appendix C The ten examples in detail</h2>
<p>How the ten examples of Figure 2 are built is summarised in this appendix.</p>
<ul>
<li>Each surface card starts from one of the ten shapes of the public diagnostic plate [3], taken as a pump card by assumption and scaled to an assumed stroke and load. It is lifted to the surface through the rod string of the example well and then analysed like a measured card.</li>
<li>The web page reproduces the library on all ten examples: pump loads within {upper_bound(f['repro_load'])} lbf and {"the same" if f['repro_labels'] else "different"} model labels.</li>
</ul>
<p>Basis, assumptions and the full values of each example: {repo_link("examples")}.</p>"""
    fixture = blob(APP_COMMIT, f["inputs"]["fixture"])
    d = f"""
<h2 id="appendix-d">Appendix D Reusable prompts</h2>
<ul>
<li>Rebuild this analysis from public code with your own assistant, and check it against the published values.</li>
<li>Then run it on a surface card of your own well.</li>
</ul>
<p>Prompts, data and check values: {repo_link("prompts")}.</p>
<p>Public data set: the example well, file <a href="{fixture}">cleansed_well_001.json</a> of the digitalmodel library, MIT licence, version of {APP_VERSION_DATE}; the diagnostic plate [3], CC0 1.0, retrieved 11 Oct 2026.</p>"""
    return a + b + c + d


# ---------------------------------------------------------------- detail pages (Markdown)

ARTICLE_TITLE = "Dynamometer card analysis: from the surface card to the pump card and a diagnosis"


def page_head(title: str, appendix: str, purposes) -> str:
    bullets = "\n".join(f"- {p}" for p in purposes)
    return (f"# {title}\n\n"
            f'Dynacard article ("{ARTICLE_TITLE}"), {REVISION_SHORT}. This page holds in full what '
            f"{appendix} of the article summarises. `{BUILDER.as_posix()}` "
            "writes it from the saved results.\n\n"
            f"Purpose:\n\n{bullets}\n")


def page_readme(f) -> str:
    rows = [
        ["[Example well](example-well.md)", "Inputs, results, sensitivity and limits of the public example well", "Appendix A"],
        ["[Numerical methods](numerical-methods.md)", "The finite-difference and frequency-domain methods and their comparison", "Appendix B"],
        ["[Ten examples](ten-examples.md)", "How the ten pump conditions are built, and their full values", "Appendix C"],
        ["[Reusable prompts](reusable-prompts.md)", "The prompts to rebuild the analysis and run it on your own card", "Appendix D"],
    ]
    return (f"# Dynacard article: detail pages\n\n"
            f'Article: "{ARTICLE_TITLE}", {REVISION_SHORT}. The article gives a short summary in each '
            "appendix; this folder holds the detail in full, one page per appendix.\n\n"
            + md_table(["Page", "What it holds", "Article part"], rows, "Table 1. Detail pages of the article.")
            + "\nOther files in this folder:\n\n"
            "- `dynacard-article.html`: the article page, static and offline.\n"
            "- `article-inputs.json`: the example well's cards, inputs and shape reading, from the report analysis.\n"
            "- `figure-1-cards.csv`: the data of Figure 1.\n\n"
            "Rebuild every file in this folder, or check that none is stale, from the repository root:\n\n"
            "```text\n"
            f"PYTHONPATH=src python {BUILDER.as_posix()}\n"
            f"PYTHONPATH=src python {BUILDER.as_posix()} --check\n"
            "```\n")


def page_example_well(f) -> str:
    r = f["report"]
    w = f["inputs"]["well"]
    plate = f["inputs"]["plate_score"]
    top = f["inputs"]["shape_top_three"]
    inputs_rows, result_rows = well_rows(f)
    fixture = blob(APP_COMMIT, f["inputs"]["fixture"])
    report_md = blob(REPORT_COMMIT, REPORT_PAGE.as_posix())
    return (page_head("Example well: inputs, results and limits", "Appendix A", [
                "Give the inputs and results of the public example well behind Figure 1 and the key conclusions.",
                "State the sensitivity and the limits that apply to the example well and to the web page."])
            + f"\nThe well is one anonymised field well in this library: [cleansed_well_001.json]({fixture}), "
              f"version of {APP_VERSION_DATE}.\n\n"
            + "## Inputs\n\n"
            + md_table(["Input", "Value", "Unit"], inputs_rows,
                       "Table 1. Example well inputs, from the well record; fluid density derived from its well test.")
            + "\n## Results\n\n"
            + md_table(["Result", "Value", "Unit"], result_rows,
                       "Table 2. Example well results, finite-difference method; displacement at 24 h/day and no slip.")
            + "\n## Notes and limits\n\n"
            + f"- The well record labels this card a shutdown card. The source system reports that its own corner "
              "detection, which finds the points where the valves open and close, failed on it, so the fillage and "
              "the fluid load rest on the corner detection of the digitalmodel library alone.\n"
            + f"- The rod string in the record ends at {f['rod_length']:,.0f} ft and the pump is at "
              f"{w['pump_depth_ft']:,.0f} ft. The end of the rod string is taken as the pump; the "
              f"{w['pump_depth_ft'] - f['rod_length']:,.0f} ft between them is not represented.\n"
            + f"- The shape model gives {top[0]['label'].lower()} a model score of {100 * top[0]['probability']:.2f} %. "
              "The score is the model's own output, not the chance that the fault is present. The model is trained on "
              f"generated cards; on the ten plate shapes it is correct on {plate['including_assumed']['correct']} of "
              f"{plate['including_assumed']['count']} ({plate['literal']['correct']} of the {plate['literal']['count']} "
              "whose plate label has the same name as a model class). The plate shapes also shape the generated "
              "training cards, so they are not an independent test.\n"
            + f"- The inputs that matter most: a 1 % larger rod diameter lowers the maximum pump load by about "
              f"{abs(r['factor_1pct_rod_diameter_maximum_pump_load_lbf']):.1f} % and the fillage by about "
              f"{abs(r['factor_1pct_rod_diameter_fillage_pct']):.1f} %; a 1 % load-cell gain error raises the maximum "
              f"pump load by about {r['factor_1pct_surface_load_scale_maximum_pump_load_lbf']:.1f} %; a 1 % surface "
              f"position scale error moves the pump stroke by about "
              f"{r['factor_1pct_surface_position_scale_pump_stroke_in']:.1f} %. The full sensitivity table is in "
              f"Appendix A of the [full study]({report_md}).\n"
            + "- Limits: unanchored tubing is not modelled and only steel rods are accepted. The frequency-domain "
              "method treats the well as vertical. A card alone does not give production; run time and slip are needed. The web page "
              "assumes a vertical well, anchored tubing and samples evenly spaced in time. Fluid load and fillage step "
              "with the detected corner samples, so rounding the last digit of a card can move the fluid load by tens "
              f"of lbf: {f['precision_load']:.0f} lbf at most in the ten examples.\n")


def page_methods(f) -> str:
    r = f["report"]
    pair, spread = method_figures(f)
    rows = [["Frequency domain", "Finite difference (default)", f"{r['gibbs_default_load_nrmse_pct']:.2f}"],
            ["Finite difference (default)", "Card stored in the well record", f"{r['default_reference_load_nrmse_pct']:.2f}"],
            ["Frequency domain", "Card stored in the well record", f"{r['gibbs_reference_load_nrmse_pct']:.2f}"]]
    report_md = blob(REPORT_COMMIT, REPORT_PAGE.as_posix())
    return (page_head("Two numerical methods for the pump card", "Appendix B", [
                "Describe the two methods that compute the pump card from the surface card.",
                "Compare their pump cards on the public example well, and state what the comparison means."])
            + "\n## Key conclusion\n\n"
            + f"- On the public example well the pump cards of the two methods differ by {pair:.2f} % of the load "
              f"range, and each is within {spread:.2f} % of the card stored in the well record (Table 1).\n"
            + "- For this problem, at this accuracy, the methods are equivalent. All results in the article use the "
              "default method.\n"
            + "- As more types of card come in, the two methods are kept side by side: some physical behaviour may be "
              "captured better by one method than by the other.\n"
            + "\n## Methods\n\n"
            + "- Finite-difference method, the default: it marches the damped wave equation for axial motion through "
              "the tapered rod string, after Everitt and Jennings (1992), "
              "[SPE-18189-PA](https://doi.org/10.2118/18189-PA).\n"
            + "- Frequency-domain method: it solves the rod model of Gibbs (1963), "
              "[SPE-588-PA](https://doi.org/10.2118/588-PA), harmonic by harmonic, with 15 harmonics, and treats the "
              "well as vertical.\n"
            + "- The well record also stores a pump card from the source system's own finite-difference calculation.\n"
            + "\n## Comparison on the example well\n\n"
            + md_table(["Pump card", "Compared with", "Load difference (% of load range)"], rows,
                       "Table 1. Root-mean-square load difference between pump cards.")
            + "\nThe stored card uses damping close to that of the default method, so its agreement with that method "
              "is a regression check; the frequency-domain method is the independent one.\n\n"
            + f"The method and the checks of both solvers are given in sections 3 and 4 of the [full study]({report_md}).\n")


def page_examples(f) -> str:
    w = f["inputs"]["well"]
    basis = f["basis"]
    rows = []
    for rec in f["records"]:
        an, d = rec["analysis"], rec["diagnosis"]
        rows.append([str(rec["index"]), rec["english_gloss"], f"{an['surface_stroke_in']:.2f}",
                     f"{an['pump_gross_stroke_in']:.2f}", f"{an['fillage_pct']:.1f}",
                     f"{an['fluid_load_lbf']:,.0f}", f"{rec['round_trip']['load_nrmse_pct']:.2f}",
                     PLAIN[d['shape_only']['classification']], PLAIN[d['shipped']['classification']]])
    readme = blob(APP_COMMIT, "docs/domains/artificial_lift/dynacard-examples/README.md")
    matrix = blob(APP_COMMIT, "docs/domains/artificial_lift/dynacard-examples/diagnostics-matrix.png")
    return (page_head("Ten pump conditions: how the examples are built, and their values", "Appendix C", [
                "Say how the ten examples of Figure 2 are built from the public diagnostic plate.",
                "Give the full values of each example and the model readings."])
            + "\n## How the examples are built\n\n"
            + f"- Each of the ten shapes of the public diagnostic plate "
              f"([doi:10.17632/hnfwm9mjs6.1]({PLATE_DOI}), CC0 1.0, retrieved 11 Oct 2026) is scaled to an assumed "
              f"{basis['stroke_in']['value']:.0f} in pump stroke and to the static fluid load of a "
              f"{w['plunger_diameter_in']:.1f} in plunger at {w['pump_depth_ft']:,.0f} ft with "
              f"{w['tubing_pressure_psi']:.0f} psi tubing pressure and zero pump intake pressure "
              f"({basis['load_range_lbf']['value']:,.0f} lbf). These are shape illustrations, not field pump cards. "
              "The plate gives shapes only, with no stroke or load, and does not say whether they are surface or pump "
              "cards; they are taken as pump cards by assumption.\n"
            + "- Each scaled shape is lifted to a surface card through the rod string of the example well and then "
              "analysed like a measured card. The surface cards are therefore synthesised, not measured.\n"
            + "- The plate labels insufficient supply and sand production have no model class of the same name; they "
              "are matched to fluid pound and sand wear by assumption.\n"
            + f"- The shape model names {f['matches']} of the 10 conditions; the second, shipped model names "
              f"{f['shipped']}. Model scores are the models' own outputs, not the chance that a fault is present.\n"
            + f"- The web page reproduces the library on all ten examples: pump loads within "
              f"{upper_bound(f['repro_load'])} lbf and {'the same' if f['repro_labels'] else 'different'} model labels.\n"
            + "\n## Values\n\n"
            + md_table(["No.", "Condition", "Surface stroke (in)", "Pump stroke (in)", "Fillage (%)",
                        "Fluid load (lbf)", "Round trip (% of load range)", "Shape model", "Shipped model"], rows,
                       "Table 1. Ten worked examples, finite-difference method.")
            + f"\nThe probabilities of all model classes are shown in the [diagnostics matrix]({matrix}); each "
              f"example's figure and data are in the [worked examples]({readme}). Both are versions of "
              f"{APP_VERSION_DATE}.\n")


def page_prompts(f) -> str:
    r = f["report"]
    prompt = blob(REPORT_COMMIT, PROMPT.as_posix())
    fixture = blob(APP_COMMIT, f["inputs"]["fixture"])
    app = blob(APP_COMMIT, "docs/apps/dynacard/README.md")
    template = blob(REPORT_COMMIT, "examples/report-evals/dynacard-pump-card/reader-card-template.json")
    checks = (f"surface stroke {r['surface_stroke_in']:.2f} in, pump stroke {r['pump_stroke_in']:.2f} in, "
              f"fluid load {r['fluid_load_lbf']:,.0f} lbf, fillage {r['fillage_pct']:.2f} %, displacement "
              f"{r['pump_displacement_bbl_day']:.2f} bbl/day fillage-adjusted and "
              f"{r['gross_displacement_bbl_day']:.2f} bbl/day for a full pump, at 24 h/day and no slip")
    return (f"# Reusable prompts: surface card to pump card\n\n"
            f'Dynacard article ("{ARTICLE_TITLE}"), {REVISION_SHORT}. This page holds in full what Appendix D '
            f"of the article summarises. `{BUILDER.as_posix()}` writes it from the saved "
            "results.\n\n"
            "These prompts let your team, or your assistant, rebuild the article's analysis from public code and "
            "then run it on a card of your own. To have a study of your own rod pump done for you instead, see "
            "below.\n\n"
            "## What you need to run this\n\n"
            "- Data required: none beyond the public data set below.\n"
            "- Data required to adapt: a surface card over one stroke, the rod string, the pump depth and plunger "
            "diameter, and the pumping speed.\n"
            "- Paid or behind an account: none.\n"
            "- Changes over time: the prompt runs the latest version of the analysis code until that version is "
            "published; the data links are pinned to a commit.\n\n"
            "## Reproduce the published result\n\n"
            "Give this prompt to your assistant:\n\n"
            "> Turn the surface dynamometer card of the public example well in the digitalmodel library into a pump "
            "card with the wave equation, and read pump stroke, fluid load and fillage from it. Follow PROMPT.md "
            "in examples/report-evals/dynacard-pump-card step by step: it gives the commands, the inputs to ask "
            "for and the expected values. Grade the result with eval.py beside it.\n\n"
            f"- Complete prompt, to copy whole: [PROMPT.md]({prompt}), version of 12 Nov 2026.\n"
            f"- Check values on the public example well, finite-difference method: {checks}.\n"
            "- The ten examples of Figure 2 rebuild from the repository root with "
            f"`PYTHONPATH=src python {EXAMPLES_BUILDER.as_posix()} --check`; their values are in "
            "[ten-examples.md](ten-examples.md).\n\n"
            "## Adapt it to your project\n\n"
            "Give this prompt to your assistant:\n\n"
            "> Copy reader-card-template.json beside PROMPT.md, replace the surface card samples and the as-run rod "
            "string, pump depth, plunger diameter and pumping speed with my well's values, and run analysis.py on "
            "the new file as PROMPT.md shows. Ask me for every item PROMPT.md lists under \"Ask me before you go on "
            "if\". Keep every ASSUMED input, and report every shape reading as a hypothesis to check at the well.\n\n"
            f"- Template: [reader-card-template.json]({template}).\n"
            f"- Without an assistant: the [web page]({app}) of the library, version of {APP_VERSION_DATE}, loads a "
            "surface card as a CSV with the well's rod and pump data and draws the pump card.\n\n"
            "## Data set\n\n"
            f"- Public example well: [cleansed_well_001.json]({fixture}) of the digitalmodel library, MIT licence, "
            f"version of {APP_VERSION_DATE}.\n"
            f"- Public diagnostic plate of ten pump working conditions: Mendeley Data, version 1, "
            f"[doi:10.17632/hnfwm9mjs6.1]({PLATE_DOI}), CC0 1.0, retrieved 11 Oct 2026.\n\n"
            "To have a study of your own rod pump done for you, write to support@aceengineer.com.\n")


def detail_pages(f) -> dict:
    return {PAGES["readme"]: page_readme(f), PAGES["example_well"]: page_example_well(f),
            PAGES["methods"]: page_methods(f), PAGES["examples"]: page_examples(f),
            PAGES["prompts"]: page_prompts(f)}


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
table.examples tbody tr{cursor:pointer}
table.examples tbody tr:hover,table.examples tbody tr:focus,table.examples tbody tr.active{background:#d6e6f8;box-shadow:inset 4px 0 0 var(--blue);outline:none}
.select-note{display:none;font:13px/1.4 system-ui,sans-serif;color:var(--blue);margin:0 0 6px}
.js-only{display:none}.js li.js-only{display:list-item}.js span.js-only{display:inline}
figcaption .fig-notes{margin:6px 0 2px;padding-left:18px}figcaption .fig-title{margin:2px 0 0}
.examples-panels{display:grid;grid-template-columns:repeat(auto-fill,minmax(300px,1fr));gap:18px;margin-top:16px}
.example-panel ul{font-size:14px;padding-left:18px;margin:6px 0 0}
.probs{font:12px system-ui,sans-serif;margin-top:8px}.prob-row{display:grid;grid-template-columns:150px 1fr 64px;gap:8px;align-items:center;margin:2px 0}
.prob-track{background:#f0efec;height:8px;border-radius:4px;overflow:hidden}.prob-bar{display:block;height:8px;background:var(--orange);border-radius:4px}
.prob-value{text-align:right;color:var(--ink2)}
.js .select-note{display:block}
.js .examples-view{display:grid;grid-template-columns:minmax(0,1fr) minmax(0,1fr);gap:20px;align-items:start}
.js .examples-panels{display:block;margin-top:0;position:sticky;top:16px}
.js .example-panel{display:none}.js .example-panel.active{display:block}
@media(max-width:820px){.js .examples-view{grid-template-columns:1fr}.js .examples-panels{position:static}}
aside.offer{border:1px solid var(--line);border-radius:6px;padding:10px 14px;font:14px system-ui,sans-serif;color:var(--ink2);margin:16px 0}
.signoff{font:12px system-ui,sans-serif;color:var(--muted)}
ol.refs{font-size:14px}
@media print{.contents,.select-note,.js-only{display:none}.page{display:block}.js .example-panel{display:block}.js .examples-view{display:block}}
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
var fig=document.getElementById('figure-2');var first=fig&&fig.getAttribute('data-default');
if(rows.length){show(first||rows[0].getAttribute('data-example'));}
})();
"""


def build_html(root: Path = ROOT) -> str:
    f = facts(load_sources(root))
    sections = [("key-conclusions", "Key conclusions"), ("ten-conditions", "Ten pump conditions"),
                ("own-card", "Run the analysis on your own card"), ("references", "References"),
                ("appendix-a", "A Example well"), ("appendix-b", "B Two numerical methods"),
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
    outputs.update(detail_pages(facts(load_sources(ROOT))))
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
