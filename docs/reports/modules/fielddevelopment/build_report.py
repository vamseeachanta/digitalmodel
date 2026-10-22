# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Rebuild the standalone report using the documented public workflow example.

Run with PYTHONPATH=src in an environment without worldenergydata. No external
datasets, licensed applications, or network requests are used by this builder.
"""

import ast
import hashlib
import importlib.util
import json
import math
import re
from dataclasses import asdict, fields, replace
from html import escape
from pathlib import Path

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[3]
MODULE = "src/digitalmodel/field_development/"
EXEMPLAR = "docs/api/cfd/turbulent-flat-plate-verification.html"
SOURCES = [
    (MODULE + "workflow.py", "Documented example and public orchestration"),
    (MODULE + "concept_selection.py", "Heuristic ranking and depth limits"),
    (MODULE + "capex_estimator.py", "Capital cost correlations; stated historical basis"),
    (MODULE + "opex_estimator.py", "Operating cost correlations; stated historical basis"),
    (MODULE + "economics.py", "Production, cash flow, discounting and fallback behaviour"),
    ("tests/field_development/test_workflow.py", "Workflow regression assertions"),
    ("tests/field_development/test_concept_selection.py", "Ranking regression assertions"),
    ("tests/field_development/test_capex_estimator.py", "Capital cost regression assertions"),
    ("tests/field_development/test_opex_estimator.py", "Operating cost regression assertions"),
    ("tests/field_development/test_economics.py", "Economics assertions and optional integrations"),
    ("tests/field_development/test_benchmarks.py", "Explicitly synthetic benchmark fixtures"),
    ("docs/domains/references/field_development/catalog/README.md", "Public reference catalogue scope"),
    (EXEMPLAR, "Report layout exemplar"),
    ("docs/api/_assets/brand.css", "Shared report stylesheet"),
]
LABELS = {"TLP": "Tension-leg platform", "Spar": "Spar",
          "Semi": "Semi-submersible", "FPSO": "Floating production, storage and offloading",
          "Subsea_Tieback": "Subsea tieback"}

# Versioned reference assumptions, cited to the pinned module editions in §9.2.
# These describe this worked example, not independently qualified design inputs.
RECOVERY_FACTOR = 0.15
PLATEAU_FRACTION = 0.60
ABANDONMENT_FRACTION = 0.07
RANKING_WEIGHTS = {"depth": 0.40, "reservoir": 0.30,
                   "distance": 0.20, "fluid": 0.10}
SPAR_DEPTH_BAND = (1200.0, 3000.0)


class BackendUnavailable(Exception):
    """The isolated local-fallback example cannot run with an external backend."""


def read_lf(path):
    """Read UTF-8 source text with platform newline sequences normalized to LF."""
    with Path(path).open(encoding="utf-8", newline=None) as stream:
        return stream.read()


def source_records():
    """Bind citations to normalized source text, independent of checkout newlines."""
    return [{"path": path, "description": description,
             "digest": hashlib.sha256(read_lf(ROOT / path).encode("utf-8")).hexdigest()}
            for path, description in SOURCES]


def example_spec():
    """Read numerical inputs from the existing Usage example, without executing it."""
    from digitalmodel.field_development.workflow import FieldDevelopmentSpec
    from digitalmodel.field_development.economics import FiscalRegime

    doc = ast.get_docstring(ast.parse(read_lf(ROOT / MODULE / "workflow.py")))
    code = "\n".join(line[4:] for line in doc.splitlines()
                     if line.startswith((">>> ", "... ")))
    call = next(node for node in ast.walk(ast.parse(code))
                if isinstance(node, ast.Call) and isinstance(node.func, ast.Name)
                and node.func.id == "FieldDevelopmentSpec")
    values = {kw.arg: ast.literal_eval(kw.value) for kw in call.keywords
              if kw.arg not in {"field_name", "fiscal_regime"}}
    fiscal = next(kw.value.attr for kw in call.keywords if kw.arg == "fiscal_regime")
    return FieldDevelopmentSpec(field_name="Documented example",
                                fiscal_regime=FiscalRegime[fiscal], **values)


def reference_schedule(spec, costs, decline_rate):
    """Independently sum the cited annual schedule without private model helpers."""
    plateau = max(1, int(spec.field_life_years * PLATEAU_FRACTION))
    factors = [0.0] + [1.0 if year <= plateau else math.exp(
        -decline_rate * (year - plateau)) for year in range(1, spec.field_life_years + 1)]
    annual_volume = spec.production_capacity_bopd * 365.0 / 1_000_000.0
    cash = [(annual_volume * spec.oil_price_usd_per_bbl - costs.opex_usd_mm_per_yr)
            * factor for factor in factors]
    for year, fraction in enumerate((0.30, 0.50, 0.20)):
        cash[year] -= costs.capex_usd_mm * fraction
    cash[-1] -= costs.abex_usd_mm
    return {"reconciled_npv": math.fsum(value / (1 + spec.discount_rate) ** year
                                       for year, value in enumerate(cash)),
            "production_mmbbl": annual_volume * math.fsum(factors),
            "plateau_mmbbl": annual_volume * plateau, "plateau_years": plateau}


def cost_values(costs):
    return {"capital": costs.capex_usd_mm, "operating": costs.opex_usd_mm_per_yr,
            "abandonment": costs.abex_usd_mm}


def compute():
    """Call public APIs; keep module results separate from reference arithmetic."""
    if importlib.util.find_spec("worldenergydata") is not None:
        raise BackendUnavailable("Local fallback example skipped: use an isolated "
                                 "environment without the optional external backend")
    from digitalmodel.field_development.workflow import (
        compare_concepts, evaluate_field_development,
    )
    from digitalmodel.field_development.economics import EconomicsInput

    spec = example_spec()
    result = evaluate_field_development(spec)
    costs, metrics = result.economics.costs, result.economics.metrics
    economic_fields = {item.name for item in fields(EconomicsInput)}
    inp = EconomicsInput(host_type=result.evaluated_host, **{
        key: value for key, value in asdict(spec).items() if key in economic_fields})
    reference = reference_schedule(spec, costs, inp.decline_rate)
    inputs = {key: value for key, value in asdict(spec).items()
              if isinstance(value, (float, int))}
    sweep = []
    # Sampling convention, not an engineering coefficient: half to one-and-a-half
    # of the documented price, in ten equal intervals. Defined in Appendix 10.1.
    for index in range(5, 16):
        price = spec.oil_price_usd_per_bbl * index / 10
        evaluated = evaluate_field_development(replace(spec, oil_price_usd_per_bbl=price))
        sweep.append({"price": price, "npv": evaluated.economics.metrics.npv_usd_mm})
    top_score = result.concept.ranked_options[0].score
    tied_hosts = {option.host_type.value for option in result.concept.ranked_options
                  if option.score == top_score}
    alternatives = [{"host": item.evaluated_host,
                     "npv": item.economics.metrics.npv_usd_mm,
                     "costs": cost_values(item.economics.costs)}
                    for item in compare_concepts(spec, top_n=len(tied_hosts)).results
                    if item.evaluated_host != result.evaluated_host]
    eur = spec.reservoir_size_mmbbl * RECOVERY_FACTOR
    return {"inputs": inputs, "selected": result.evaluated_host,
            "ranking": [{"host": o.host_type.value, "score": o.score}
                        for o in result.concept.ranked_options],
            "costs": cost_values(costs), "tied_alternatives": alternatives,
            "npv": metrics.npv_usd_mm, "sensitivity": sweep,
            **reference, "eur_mmbbl": eur,
            "production_to_eur": reference["production_mmbbl"] / eur,
            "recovery_factor": RECOVERY_FACTOR, "plateau_fraction": PLATEAU_FRACTION,
            "abandonment_fraction": ABANDONMENT_FRACTION,
            "ranking_weights": RANKING_WEIGHTS,
            "excess_percent": (reference["production_mmbbl"] / spec.reservoir_size_mmbbl - 1) * 100,
            "decline_rate": inp.decline_rate,
            "depth_low": SPAR_DEPTH_BAND[0], "depth_high": SPAR_DEPTH_BAND[1],
            "tax_applied": metrics.pre_tax_npv_usd_mm is not None}


def display_values(data):
    """Enumerate all displayed worked-example numbers for readback testing."""
    values = {**data["inputs"], **data["costs"]}
    for key in ("npv", "production_mmbbl", "excess_percent", "decline_rate",
                "depth_low", "depth_high", "eur_mmbbl", "plateau_mmbbl", "plateau_years",
                "recovery_factor", "plateau_fraction", "abandonment_fraction",
                "production_to_eur"):
        values[key] = data[key]
    values.update({"weight_" + key: value for key, value in data["ranking_weights"].items()})
    alternative = data["tied_alternatives"][0]
    values["tied_npv"] = alternative["npv"]
    values.update({"tied_" + key: value for key, value in alternative["costs"].items()})
    values.update({"score_" + row["host"]: row["score"] for row in data["ranking"]})
    for i, point in enumerate(data["sensitivity"]):
        values[f"price_{i}"] = point["price"]
        values[f"npv_{i}"] = point["npv"]
    return values


def format_value(value, key=None):
    """Use engineering precision, retaining small nonzero values."""
    if key in {"water_depth_m", "production_capacity_bopd", "field_life_years",
               "depth_low", "depth_high", "plateau_years"}:
        return f"{value:,.0f}"
    if key in {"excess_percent", "production_to_eur", "decline_rate"}:
        return f"{value:,.1f}"
    return (f"{value:,.2f}" if abs(value) >= 0.01 or value == 0
            else f"{value:.3g}").replace("-", "−")


def table(headers, rows, caption):
    return ("<table><thead><tr>" + "".join(f"<th>{h}</th>" for h in headers)
            + "</tr></thead><tbody>" + "".join("<tr>" + "".join(
                f"<td>{cell}</td>" for cell in row) + "</tr>" for row in rows)
            + f"</tbody></table><p class=\"sub\">{caption}</p>")


def chart(data):
    """Inline SVG with accessible hover/focus readback and a drawn zero criterion."""
    points = data["sensitivity"]
    xmin, xmax = points[0]["price"], points[-1]["price"]
    low = min(0, min(p["npv"] for p in points))
    high = max(0, max(p["npv"] for p in points))
    padding = max(high - low, 1.0) * 0.05  # Plot convention, not an acceptance limit.
    ymin, ymax = low - padding, high + padding
    x = lambda value: 90 + (value - xmin) / (xmax - xmin) * 600
    y = lambda value: 300 - (value - ymin) / (ymax - ymin) * 230
    marks = []
    for i, point in enumerate(points):
        label = (f"Documented example; {LABELS[data['selected']]}; price "
                 f"{format_value(point['price'])} USD/bbl; net present value "
                 f"{format_value(point['npv'])} million USD; comparison limit zero million USD")
        marks.append(f'<circle data-point="{i}" tabindex="0" cx="{x(point["price"]):.2f}" '
                     f'cy="{y(point["npv"]):.2f}" r="6" fill="var(--accent)" '
                     f'aria-label="{escape(label)}"><title>{escape(label)}</title></circle>')
    coordinates = " ".join(f"{x(p['price']):.2f},{y(p['npv']):.2f}" for p in points)
    return (f'<svg viewBox="0 0 780 370" role="img" aria-label="Price sensitivity">'
            f'<path d="M90 60 V300 H710" fill="none" stroke="var(--ink)"/>'
            f'<line id="zero-limit" x1="90" x2="710" y1="{y(0):.2f}" y2="{y(0):.2f}" '
            'stroke="var(--red)" stroke-dasharray="6 4"/>'
            f'<polyline points="{coordinates}" fill="none" stroke="var(--accent)"/>'
            + "".join(marks)
            + f'<text x="90" y="325">{format_value(xmin)}</text>'
            + f'<text x="665" y="325">{format_value(xmax)}</text>'
            + f'<text x="35" y="65">{format_value(ymax)}</text>'
            + f'<text x="5" y="305">{format_value(ymin)}</text>'
            + '<text x="260" y="350">Oil price (USD/bbl)</text>'
            '<text transform="translate(22 285) rotate(-90)">Net present value (million USD)</text>'
            '<text x="100" y="35">Dashed line: zero net present value</text></svg>'
            '<p id="chart-readout" aria-live="polite">Hover or focus a point for its value and limit.</p>')


def fragments(data):
    """Build numeric tables from computed values only."""
    values = display_values(data)
    spans = {key: f'<span data-value="{key}">{format_value(value, key)}</span>'
             for key, value in values.items()}
    labels = {"water_depth_m": ("Water depth", "m"),
              "reservoir_size_mmbbl": ("Reservoir size", "million bbl"),
              "production_capacity_bopd": ("Production capacity", "bbl/day"),
              "oil_price_usd_per_bbl": ("Oil price", "USD/bbl"),
              "discount_rate": ("Annual discount rate", "fraction"),
              "field_life_years": ("Field life", "years")}
    inputs = table(["Input", "Value", "Unit", "Source"],
                   [(labels[k][0], spans[k], labels[k][1], "workflow.py — Usage example")
                    for k in data["inputs"]], "Table A. Documented illustrative inputs.")
    ranking = table(["Host concept", "Suitability score (dimensionless)", "Source"],
                    [(LABELS[r["host"]], spans["score_" + r["host"]],
                      "concept_selection.py — concept_selection and composite score")
                     for r in data["ranking"]], "Table B. Spar and Semi tie; stable legacy enumeration "
                    "order places Spar first. Score is not probability.")
    sensitivity = table(["Oil price (USD/bbl)", "Net present value (million USD)", "Source"],
                        [(spans[f"price_{i}"], spans[f"npv_{i}"],
                          "workflow.py — evaluate_field_development; economics.py — annual cash flow")
                         for i in range(len(data["sensitivity"]))], "Table C. Price sensitivity readback.")
    refs = "".join(f'<li>{escape(r["description"])}: <code>{r["path"]}</code>. '
                   f'Text edition SHA-256: <code>{r["digest"]}</code>.</li>' for r in source_records())
    return {**spans, "input_table": inputs, "ranking_table": ranking,
            "sensitivity_table": sensitivity, "references": refs,
            "selected": LABELS[data["selected"]], "chart": chart(data)}


def render():
    """Rebuild deterministic HTML and its engineering change record in memory."""
    data = compute()
    replacements = fragments(data)
    body = read_lf(HERE / "report_body.html.in")
    for key, value in replacements.items():
        body = body.replace("{{" + key + "}}", value)
    if re.search(r"{{.*?}}", body):
        raise ValueError("Unresolved report field")
    style = re.search(r"<style>.*?</style>", read_lf(ROOT / EXEMPLAR), re.S).group(0)
    brand = read_lf(ROOT / "docs/api/_assets/brand.css")
    interaction = read_lf(HERE / "interaction.js")
    changes = {"revision": "1", "date": "2026-10-22", "status": "Draft for technical review",
               "changes": ["Numeric evidence is stabilized across calculation platforms.",
                           "The concept tie and fallback production basis are disclosed."],
               "history": [{"revision": "Initial", "date": "2026-10-21",
                            "changes": ["Concept selection and local economic screening are documented.",
                                        "The production-volume discrepancy is identified as a limit of use."]},
                           {"revision": "1", "date": "2026-10-22",
                            "changes": ["Numeric precision, tied economics and assumptions are clarified."]}]}
    html = ('<!DOCTYPE html>\n<html lang="en" data-theme="light"><head><meta charset="utf-8">'
            '<meta name="viewport" content="width=device-width, initial-scale=1">'
            '<title>Field development: concept and economic screening</title>'
            f'<style>{brand}</style>{style}</head><body class="hs-doc"><main class="wrap">'
            + body + '</main><script id="evidence" type="application/json">'
            + json.dumps(evidence_values(data), sort_keys=True, allow_nan=False) + '</script>'
            + '<script id="changes" type="application/json">'
            + json.dumps(changes, sort_keys=True) + '</script>'
            + f'<script>{interaction}</script></body></html>\n')
    return html, json.dumps(changes, indent=2) + "\n"


def evidence_values(value):
    """Retain 13 significant digits: rounding error is below relative 1e-12."""
    if isinstance(value, float):
        return float(format(value, ".13g"))
    if isinstance(value, dict):
        return {key: evidence_values(item) for key, item in value.items()}
    if isinstance(value, list):
        return [evidence_values(item) for item in value]
    return value


def build(destination=HERE):
    """Save both rebuilt artifacts with deterministic line endings."""
    html, changes = render()
    destination = Path(destination)
    destination.mkdir(parents=True, exist_ok=True)
    (destination / "report.html").write_text(html, encoding="utf-8", newline="\n")
    (destination / "changes.json").write_text(changes,
                                             encoding="utf-8", newline="\n")


if __name__ == "__main__":
    try:
        build()
    except BackendUnavailable as exc:
        print(f"SKIPPED: {exc}")
