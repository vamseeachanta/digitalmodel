"""Deterministic CSV and Markdown views of saved worked-example results."""
from __future__ import annotations

import csv
import io
import json

from digitalmodel.marine_ops.artificial_lift.dynacard.worked_examples import (
    diagnostics_matrix,
)

ANALYSIS_FIELDS = {
    "surface_stroke_in": ("Surface stroke", "in"),
    "peak_polished_rod_load_lbf": ("Peak polished-rod load", "lbf"),
    "minimum_polished_rod_load_lbf": ("Minimum polished-rod load", "lbf"),
    "pump_gross_stroke_in": ("Pump gross stroke", "in"),
    "pump_net_stroke_in": ("Effective pump travel", "in"),
    "fillage_pct": ("Pump fillage", "%"),
    "fluid_load_lbf": ("Fluid load (upstroke minus downstroke mean)", "lbf"),
    "upstroke_mean_load_lbf": ("Upstroke mean load", "lbf"),
    "downstroke_mean_load_lbf": ("Downstroke mean load", "lbf"),
    "pump_intake_pressure_psi": ("CPIP intake pressure", "psi"),
    "pump_discharge_pressure_psi": ("CPIP discharge pressure", "psi"),
    "cpip_fluid_density_lb_ft3": ("CPIP module fluid density", "lb/ft³"),
    "cpip_tubing_gradient_psi_ft": ("CPIP tubing gradient", "psi/ft"),
    "gross_displacement_bpd": ("Gross displacement", "bbl/day"),
    "net_displacement_bpd": ("Net displacement", "bbl/day"),
    "theoretical_production_bpd": ("Theoretical production", "bbl/day"),
    "pump_efficiency_pct": ("Reported ASSUMED pump efficiency", "%"),
    "runtime_hours": ("ASSUMED runtime", "h/day"),
    "pump_maximum_load_lbf": ("Pump maximum load", "lbf"),
    "pump_minimum_load_lbf": ("Pump minimum load", "lbf"),
    "pump_work_per_stroke_ft_lbf": ("Pump-card work per stroke", "ft·lbf"),
    "polished_rod_work_per_stroke_ft_lbf": ("Polished-rod work per stroke", "ft·lbf"),
}

UNIT_DECIMALS = {"in": 2, "lbf": 0, "%": 1, "psi": 1, "bbl/day": 1,
                 "ft·lbf": 0, "lb/ft³": 3, "psi/ft": 4, "h/day": 1}


def _precision_sentence(records) -> str:
    """State the published-precision effect on the stepped results."""
    effects = [(r["index"], r["published_precision_effect"]) for r in records]
    load = max(abs(e["fluid_load_difference_lbf"]) for _, e in effects)
    fill = max(abs(e["fillage_difference_pct"]) for _, e in effects)
    moved = [str(i) for i, e in effects if e["corners_changed"]]
    where = ("examples " + ", ".join(moved)) if moved else "no example"
    return ("Every analysis runs on the surface card at its published CSV precision "
            "(0.0001 in, 0.01 lbf), so rerunning a published CSV reproduces the stored "
            "values. Precision-effect deltas are reported at an absolute resolution "
            "of 1e-6 lbf and 1e-6 percentage points; smaller cancellation noise is "
            "reported as zero. Fluid load and fillage step with the detected corner samples: the "
            "unrounded synthesised cards give fluid loads differing by up to "
            f"{load:.0f} lbf and " + (f"fillage by up to {fill:.1f} percentage points"
            if fill >= 0.05 else "the same fillage to one decimal") + ", with "
            f"corners moved in {where}. Differences of this size between two tools "
            "reading the same card are expected unless both use identical input digits.\n\n")


def stem(record):
    return f"example-{record['index']:02d}-{record['slug']}"


def csv_bytes(rows):
    stream = io.StringIO(newline="")
    csv.writer(stream, lineterminator="\n").writerows(rows)
    return stream.getvalue().encode("utf-8")


def card_csvs(record):
    cards = [record[k] for k in (
        "surface_card", "computed_pump_card", "source_pump_card")]
    rows = [["sample", "surface_position_in", "surface_load_lbf",
             "pump_position_in", "pump_load_lbf", "source_pump_position_in",
             "source_pump_load_lbf"]]
    for i in range(max(len(card["position"]) for card in cards)):
        row = [i]
        for card in cards:
            row.extend([f"{card['position'][i]:.4f}", f"{card['load'][i]:.2f}"]
                       if i < len(card["position"]) else ["", ""])
        rows.append(row)
    surface = [["position_in", "load_lbf"]]
    surface.extend([f"{x:.4f}", f"{y:.2f}"] for x, y in zip(
        cards[0]["position"], cards[0]["load"], strict=True))
    return {f"{stem(record)}.csv": csv_bytes(rows),
            f"{stem(record)}-surface-card.csv": csv_bytes(surface)}


def matrix_header(column):
    if column["mode"] == "ALL_OTHER_MODES":
        return "All other modes"
    suffix = " [ASSUMED]" if column["crosswalk_kind"] == "assumed" else ""
    return column["mode"] + suffix


def matrix_csv(matrix):
    rows = [["example", "condition"] + [matrix_header(c) + " (%)"
            for c in matrix["columns"]] + ["expected_mode", "shape_only_argmax",
            "shipped_model_call", "shape_only_matches_expected",
            "shipped_matches_expected"]]
    for row in matrix["rows"]:
        rows.append([row["index"], row["english_gloss"]] +
                    [f"{p:.9f}" for p in row["probabilities_pct"]] +
                    [row[k] for k in ("expected_mode", "shape_only_argmax",
                     "shipped_model_call", "shape_only_matches_expected",
                     "shipped_matches_expected")])
    return csv_bytes(rows)


def table(headers, rows, caption):
    text = ["| " + " | ".join(headers) + " |",
            "| " + " | ".join("---" for _ in headers) + " |"]
    text.extend("| " + " | ".join(str(c) for c in row) + " |" for row in rows)
    return "\n".join(text) + f"\n\n{caption}\n"


def summary_table(records):
    rows = []
    for r in records:
        a, d = r["analysis"], r["diagnosis"]
        rows.append([r["index"], r["english_gloss"], r["crosswalk_kind"].upper(),
                     f"{a['pump_gross_stroke_in']:.2f}", f"{a['fillage_pct']:.1f}",
                     f"{a['fluid_load_lbf']:.0f}", d["shipped"]["classification"],
                     d["shape_only"]["classification"],
                     f"{r['round_trip']['load_nrmse_pct']:.1f}"])
    return table(["Example", "Condition", "Crosswalk", "Pump stroke (in)",
                  "Fillage (%)", "Fluid load (lbf)", "Shipped HYPOTHESIS",
                  "Shape-only HYPOTHESIS (computed pump card)", "Round-trip nRMSE (%)"], rows,
                 "Table 1. Computed values under the ASSUMED basis; calls are hypotheses.")


def top_three(diagnosis):
    return "; ".join(f"{d['mode']} {100 * d['probability']:.2f} %"
                     for d in diagnosis["differential"][:3])


def matrix_section(matrix):
    rows = []
    for row in matrix["rows"]:
        cells = [f"{p:.2f}" for p in row["probabilities_pct"]]
        rows.append([row["index"]] + cells + [row["shipped_model_call"]])
    return ("## Diagnostics matrix\n\n"
            "![Shape-only diagnostic probabilities](diagnostics-matrix.png)\n\n"
            "Figure 1. Probabilities are from the shape-only model on the pump card "
            "computed from the synthesised surface card. "
            "An outline marks the expected cell; a star marks the single-mode argmax "
            "(located in the aggregate column when outside the plate modes). "
            "The right-hand column reports the shipped-model HYPOTHESIS.\n\n" +
            table(["Example"] + [matrix_header(c) + " (%)" for c in matrix["columns"]]
                  + ["Shipped HYPOTHESIS"], rows,
                  "Table 2. Shape-only model probabilities (%) on the pump card computed "
                  "from the synthesised surface card; the final probability column sums all "
                  "modes without a plate-condition column. Display rounding may affect sums.") +
            "\nThe [matrix CSV](diagnostics-matrix.csv) also records expected modes, "
            "argmax calls and both equality flags. [Vector figure](diagnostics-matrix.svg).\n")


def analysis_table(record):
    analysis = record["analysis"]
    rows = [[label, unit, f"{analysis[key]:.{UNIT_DECIMALS[unit]}f}"]
            for key, (label, unit) in ANALYSIS_FIELDS.items()]
    rows.extend([["Production runtime source", "category", analysis["runtime_source"]],
                 ["Buckling compression flag", "boolean", analysis["buckling_detected"]],
                 ["Round-trip load nRMSE", "%", f"{record['round_trip']['load_nrmse_pct']:.1f}"],
                 ["Source-step resolution limit", "%", f"{record['round_trip']['tolerance_pct']:.1f}"],
                 ["Round-trip stroke difference", "in", f"{record['round_trip']['stroke_difference_in']:.2f}"]])
    return table(["Quantity", "Unit", "Value"], rows,
                 f"Table {record['index'] + 2}. Example {record['index']} workflow results; "
                 "numeric zero is a calculated value, not missing evidence.")


def example_section(record):
    r, d = record, record["diagnosis"]
    source = (f"[DOI {r['source_doi']}]({r['dataset_url']}); "
              f"[original workbook]({r['file_url']}); sheet `{r['sheet_name']}`; "
              f"{r['licence']}; retrieved {r['retrieval_date']}.")
    return (f"## Example {r['index']}: {r['english_gloss']}\n\n"
            f"![Example {r['index']} surface and pump cards]({stem(r)}.png)\n\n"
            f"Figure {r['index'] + 1}. Synthesised surface card, not a measured card; "
            "computed pump card overlaid with the ASSUMED scaled source shape; "
            "each card plotted from the bottom of its own stroke; the computed card's "
            "solver datum (static rod stretch) is removed for display.\n\n" +
            analysis_table(r) + "\n"
            f"- **HYPOTHESIS — shape-only model, computed pump card:** "
            f"`{d['shape_only']['classification']}`. Top three: {top_three(d['shape_only'])}.\n"
            f"- **HYPOTHESIS — shipped model:** `{d['shipped']['classification']}`; "
            f"confidence {100 * d['shipped']['confidence']:.2f} %. "
            f"Differential: {top_three(d['shipped'])}.\n"
            f"- Expected condition / crosswalk: `{r['crosswalk_mode']}` / "
            f"**{r['crosswalk_kind'].upper()}**.\n"
            f"- Source-shape check: `{r['consistency']['source_shape_only']['classification']}`.\n\n"
            f"Buckling criterion: {r['analysis']['buckling_criterion']}. "
            f"{r['analysis']['buckling_limitation']} Detailed stability is NOT_EVALUATED.\n\n"
            f"Source: {source}\n\n"
            f"[Paired card CSV]({stem(r)}.csv) · "
            f"[Browser surface-card CSV]({stem(r)}-surface-card.csv) · "
            f"[Vector figure]({stem(r)}.svg).\n")


def _basis_leaves(path, value):
    if isinstance(value, dict) and value:
        for key, item in value.items():
            yield from _basis_leaves(f"{path}.{key}", item)
    elif isinstance(value, list) and value:
        for i, item in enumerate(value):
            yield from _basis_leaves(f"{path}[{i}]", item)
    else:
        yield path, value


def basis_section(document):
    rows = []
    for key, entry in document["basis"].items():
        for i, (path, value) in enumerate(_basis_leaves(key, entry["value"])):
            reason = (entry["reason"] if i == 0 else f"Same ASSUMED {key} basis.")
            reason += " Provenance: " + entry["provenance"] + "."
            rows.append([f"`{path}`", entry["status"],
                         "`" + json.dumps(value, ensure_ascii=False) + "`", reason])
    return ("## Basis and assumptions\n\n" +
            table(["Item", "Status", "Value", "Reason"], rows,
                  "Table 13. Every assigned input and interpretation is ASSUMED; "
                  "the source does not provide a dimensioned well or measured surface card.") +
            "\nThe production module reports gross displacement from pump area, gross "
            "stroke and speed; net displacement multiplies gross by fillage; theoretical "
            "production multiplies net by runtime/24. The reported pump-efficiency value "
            "is not a multiplier in that calculation. Runtime source `card` denotes the "
            "InputParameters default, not measured runtime. Both CPIP density and "
            "gradient are retained above; negative computed intake pressures are "
            "retained without clamping. The module's CPIP uses its own default mixture "
            "density (35° API oil, 1.03 water specific gravity, 50 % water cut, "
            "58.651 lb/ft³), not the context fluid density of 63.237 lb/ft³ used to scale "
            "the load, so the computed fluid load can exceed the CPIP discharge-pressure "
            "basis; CPIP for these examples is therefore not established.\n")


def limitations():
    return ("## What these examples do not establish\n\n"
            "The surface cards are synthesised, not measured. The plate is normalised "
            "and dimensionless; engineering scale and pump-card space are ASSUMED. "
            "There is one exemplar per condition. The shape-only model was trained on "
            "generators held to these plate shapes, so agreement measures consistency "
            "with dependent shapes, not field accuracy. Field accuracy is "
            "**NOT_EVALUATED** pending independently labelled, dimensioned field cards.\n\n"
            "All ten shapes receive the same fluid-load range. For rod parting and any "
            "condition whose physical pump load would be near zero, that uniform scale "
            "makes the result a shape illustration, not a physical pump card. Classifier "
            "probabilities describe model class separation; they are not calibrated "
            "field probabilities. Every classifier call remains **HYPOTHESIS**.\n\n"
            "The forward model is the discrete inverse of the default solver on the "
            "same grid. Round-trip load nRMSE tests inverse consistency under that "
            "basis, not physical accuracy. The numerical criterion is derived from "
            "the source samples independently of solver output: RMS recovery error "
            "is less than the largest adjacent source-load increment, including "
            "closure. In percent of source load range, the limit is "
            "100 * max(abs(F[i+1] - F[i])) / range(F). This compares recovery error "
            "with one sample of source-load resolution; it is not an engineering "
            "acceptance limit or evidence of field accuracy. An eight-sample "
            "hand calculation with 100 lbf steps over a 400 lbf range gives "
            "25.0 %; constant offsets of 80 lbf and 120 lbf give RMS errors "
            "of 20.0 % and 30.0 %, respectively, on opposite sides of that limit. "
            "The former 3.0 % limit depended on an unretained prior run; it remains "
            "an additional regression ceiling, without serving as independent "
            "qualification evidence. Direction and cyclic "
            "phase follow compare_cards alignment; no load offset is introduced. "
            "Computed pump position retains the solver datum (static rod stretch "
            "offset) in JSON and CSV; stroke is compared by range. Each card is plotted "
            "from the bottom of its own stroke; the computed card's solver datum "
            "(static rod stretch) is removed for display only.\n")


def score_section(records):
    literal = [r for r in records if r["crosswalk_kind"] == "literal"]
    correct = lambda rs, model: sum(r["diagnosis"][model]["classification"] ==
                                    r["crosswalk_mode"] for r in rs)
    source_correct = lambda rs: sum(r["consistency"]["source_shape_only"]["classification"] ==
                                    r["crosswalk_mode"] for r in rs)
    changed = [f"example {r['index']} ({r['english_gloss']}: "
               f"{r['consistency']['source_shape_only']['classification']} → "
               f"{r['diagnosis']['shape_only']['classification']})" for r in records
               if r["diagnosis"]["shape_only"]["classification"] !=
               r["consistency"]["source_shape_only"]["classification"]]
    difference = ("Rows differing from the source-shape call: " + "; ".join(changed) + ". "
                  if changed else "No rows differ from the source-shape call. ")
    errors = [r["round_trip"]["load_nrmse_pct"] for r in records]
    negative = "; ".join(f"example {r['index']}: {r['analysis']['pump_intake_pressure_psi']:.3f} psi"
                         for r in records if r["analysis"]["pump_intake_pressure_psi"] < 0)
    limits = [r["round_trip"]["tolerance_pct"] for r in records]
    return (f"Computed-card shape-only matches {correct(records, 'shape_only')}/10 "
            f"expected modes; the shipped model matches {correct(records, 'shipped')}/10; "
            f"source-shape consistency is {source_correct(literal)}/8 literal and "
            f"{source_correct(records)}/10 including ASSUMED crosswalks, against the "
            "[recorded plate score](../plate-classifier-score.md) of 5/8 and 6/10. " +
            difference +
            f"The observed round-trip load nRMSE range is {min(errors):.1f}–{max(errors):.1f} %; "
            f"source-step resolution limits range from {min(limits):.1f}–{max(limits):.1f} %. "
            "Each unrounded error is checked against its own source-derived limit.\n\n"
            + _precision_sentence(records) +
            f"Computed CPIP below zero: {negative or 'none'}. These values are retained "
            "as reported by the module; physical intake pressure is not established "
            "without a consistent dimensioned fluid-pressure basis and measured cards.\n")


def reading_guide(records):
    intro = ("## How to read each example\n\n"
             "The surface card is what a dynamometer records at the polished rod; "
             "the pump card is the load and travel at the plunger after the wave "
             "equation removes rod weight, rod stretch and rod dynamics. Fillage is "
             "effective plunger travel divided by gross travel. Fluid load is the "
             "upstroke mean load minus the downstroke mean load. The matrix shows "
             "the shape-only model probability for each condition on the pump card "
             "computed from the synthesised surface card.\n\n")
    cells = []
    for r in records:
        title = f"Example {r['index']}: {r['english_gloss']}"
        anchor = f"example-{r['index']}-{r['slug']}"
        cells.append(f'[{title}](#{anchor})<br><a href="#{anchor}">'
                     f'<img src="{stem(r)}.png" width="360" alt="{title}"></a>')
    return intro + table(["Example preview", "Example preview"],
                         [cells[i:i + 2] for i in range(0, len(cells), 2)],
                         "Gallery. Ten surface-card and computed-pump-card examples.")


def readme(document, matrix):
    records = document["examples"]
    header = ("<!-- Generated by examples/domains/artificial_lift/build_dynacard_worked_examples.py; do not edit. -->\n"
              "# Ten worked dynacard examples\n\n"
              "Ten worked examples relate a surface card to its pump card, analysis and "
              "diagnosis. Every surface card is synthesised from a public diagnostic "
              "plate shape, not measured. The pump-card interpretation, physical scale "
              "and illustrative well are ASSUMED. All classifier calls are HYPOTHESIS.\n\n"
              "The [results document](results.json) is the reporting source for this "
              "README and all CSVs. Sample numbers are zero-based. Each card retains its "
              "own sample index; where counts differ, the shorter card's columns are "
              "blank rather than interpolated. Browser inputs contain exactly position_in "
              "and load_lbf. Positions have four decimals and loads two.\n\n")
    rebuild = ("## Rebuild\n\nFrom the repository root, with the existing environment:\n\n"
               "```sh\nPYTHONPATH=src PYTHONIOENCODING=utf-8 python "
               "examples/domains/artificial_lift/build_dynacard_worked_examples.py\n"
               "PYTHONPATH=src PYTHONIOENCODING=utf-8 python "
               "examples/domains/artificial_lift/build_dynacard_worked_examples.py --check\n```\n\n"
               "The offline builder uses the retained digest-pinned workbook and models. "
               "The check compares results.json, all CSVs and README byte for byte; "
               "figures are excluded. SVG and PNG use fixed sizes and 150 dpi.\n")
    revisions = ("## Revision history\n\n" + table(
        ["Revision", "Date", "Change and reason"],
        [["1", "2026-11-12", "Ten dependent plate examples with assumed physical scales."],
         ["2", "2026-11-13", "Absolute delta resolution, lossless model storage, source-derived numerical criterion and input provenance correct reproducibility and evidence gaps."]],
        "Table R1. Report revisions.") + "\n")
    digests = "\nModel SHA-256 digests (decompressed inference JSON): " + "; ".join(
        f"{key} `{digest}`" for key, digest in document["model_digests"].items()) + ".\n"
    return (header + revisions + reading_guide(records) + "\n" + score_section(records) + "\n" + summary_table(records) + "\n" +
            matrix_section(matrix) + "\n" + "\n".join(example_section(r) for r in records) +
            "\n" + basis_section(document) + digests + "\n" + limitations() + "\n" + rebuild)


def text_artifacts(document):
    matrix = diagnostics_matrix(document["examples"])
    artifacts = {"results.json": (json.dumps(document, indent=2, ensure_ascii=False,
                                             allow_nan=False) + "\n").encode("utf-8"),
                 "README.md": readme(document, matrix).encode("utf-8"),
                 "diagnostics-matrix.csv": matrix_csv(matrix)}
    for record in document["examples"]:
        artifacts.update(card_csvs(record))
    return artifacts
