"""Fixed-layout, deterministic figure renderers for worked dynacard results."""
from __future__ import annotations

import textwrap

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.patches import Rectangle
import numpy as np

from digitalmodel.marine_ops.artificial_lift.dynacard.worked_examples import (
    diagnostics_matrix,
)

matplotlib.rcParams.update({"svg.hashsalt": "dynacard-worked-examples-v1",
                            "font.family": "DejaVu Sans", "font.size": 10})
BLUE, ORANGE, GREEN, GREY = "#0072B2", "#D55E00", "#009E73", "#626262"


def _save(fig, out, stem):
    fig.savefig(out / f"{stem}.png", dpi=150, metadata={"Software": None})
    svg = out / f"{stem}.svg"
    fig.savefig(svg, dpi=150, metadata={"Date": None})
    # Matplotlib emits trailing spaces; normalize both whitespace and newlines.
    svg.write_bytes(b"\n".join(line.rstrip() for line in svg.read_bytes().splitlines()) + b"\n")
    plt.close(fig)


def _loop(axis, card, **kwargs):
    position, load = card["position"], card["load"]
    position = [x - min(position) for x in position]
    axis.plot(position + position[:1], load + load[:1], **kwargs)


def _axes(fig, record):
    surface = fig.add_axes([0.075, 0.49, 0.385, 0.355])
    pump = fig.add_axes([0.565, 0.49, 0.385, 0.355])
    _loop(surface, record["surface_card"], color=BLUE, lw=1.8)
    surface.set_title("Polished-rod surface card", fontsize=12, pad=10)
    _loop(pump, record["computed_pump_card"], color=BLUE, lw=1.8,
          label="Computed pump card")
    _loop(pump, record["source_pump_card"], color=ORANGE, lw=1.4,
          ls="--", label="Source plate shape (ASSUMED scale)")
    pump.set_title("Pump card and source shape", fontsize=12, pad=10)
    a = record["analysis"]
    for key, label in (("upstroke_mean_load_lbf", "Upstroke mean"),
                       ("downstroke_mean_load_lbf", "Downstroke mean")):
        pump.axhline(a[key], color=GREEN, ls=":" if "upstroke" in key else "-.",
                     lw=1.1, label=f"{label}: {a[key]:.0f} lbf")
    pump.legend(loc="upper center", bbox_to_anchor=(0.5, -0.22),
                fontsize=8, ncol=2, frameon=False)
    for axis in (surface, pump):
        axis.set_xlabel("Position (in)")
        axis.set_ylabel("Load (lbf)")
        axis.grid(alpha=0.2)
        axis.margins(x=0.04, y=0.12)
        axis.ticklabel_format(axis="y", style="plain", useOffset=False)


def _analysis_text(record):
    a = record["analysis"]
    return "\n".join([
        "WORKFLOW ANALYSIS | ASSUMED WELL AND SCALE",
        f"Surface stroke: {a['surface_stroke_in']:.2f} in",
        f"Polished-rod max / min: {a['peak_polished_rod_load_lbf']:.0f} / "
        f"{a['minimum_polished_rod_load_lbf']:.0f} lbf",
        f"Pump gross stroke: {a['pump_gross_stroke_in']:.2f} in",
        f"Effective travel: {a['pump_net_stroke_in']:.2f} in | "
        f"fillage: {a['fillage_pct']:.1f} %",
        f"Fluid load: {a['fluid_load_lbf']:.0f} lbf (difference of means)",
        f"CPIP intake / discharge: {a['pump_intake_pressure_psi']:.0f} / "
        f"{a['pump_discharge_pressure_psi']:.0f} psi",
        f"Gross / net displacement: {a['gross_displacement_bpd']:.2f} / "
        f"{a['net_displacement_bpd']:.2f} bbl/day",
        f"Theoretical production: {a['theoretical_production_bpd']:.2f} bbl/day "
        f"at {a['runtime_hours']:g} h/day",
        f"Pump work: {a['pump_work_per_stroke_ft_lbf']:.0f} ft·lbf/stroke",
        f"Round-trip load nRMSE: {record['round_trip']['load_nrmse_pct']:.1f} % "
        f"(source-step limit {record['round_trip']['tolerance_pct']:.1f} %)",
    ])


def _diagnosis_text(record):
    d = record["diagnosis"]
    lines = ["CLASSIFIER CALLS | HYPOTHESIS",
             f"Shape-only (computed pump card): {d['shape_only']['classification']}"]
    lines.extend(f"  {p['mode']}: {100 * p['probability']:.2f} %"
                 for p in d["shape_only"]["differential"][:3])
    lines.extend([f"Shipped model: {d['shipped']['classification']}",
                  f"  Confidence: {100 * d['shipped']['confidence']:.2f} %",
                  "Expected condition / crosswalk:",
                  f"  {record['crosswalk_mode']} / {record['crosswalk_kind'].upper()}",
                  "Source-shape check: " + record['consistency']['source_shape_only']['classification']])
    return "\n".join(lines)


def card_figure(record, out):
    fig = plt.figure(figsize=(14, 9.6), dpi=150, facecolor="white")
    fig.suptitle(f"Example {record['index']}: {record['english_gloss']}",
                 x=0.075, y=0.972, ha="left", fontsize=19, weight="bold")
    fig.text(0.075, 0.922,
             "Surface card synthesised from the plate shape; not a measured card. "
             "Pump space, scale, timing and well: ASSUMED.", fontsize=10, color=GREY)
    _axes(fig, record)
    fig.text(0.075, 0.405, _analysis_text(record), va="top", fontsize=10,
             linespacing=1.7)
    fig.text(0.565, 0.337, _diagnosis_text(record), va="top", fontsize=9.2,
             linespacing=1.45)
    caveat = ("Figure. Source: DOI 10.17632/hnfwm9mjs6.1, CC0 1.0. Uniform "
              "scaling makes near-zero-load conditions, including rod parting, "
              "shape illustrations; each card plotted from the bottom of its own stroke; "
              "the computed card's solver datum (static rod stretch) is removed for display. "
              "Field accuracy: NOT_EVALUATED.")
    fig.text(0.075, 0.025, textwrap.fill(caveat, 145), va="bottom", fontsize=8.5,
             color=GREY, linespacing=1.4)
    _save(fig, out, f"example-{record['index']:02d}-{record['slug']}")


def _matrix_labels(columns):
    labels = []
    for column in columns:
        mode = column["mode"]
        label = mode.replace("_", " ")
        label = textwrap.fill(label, 13)
        if column["crosswalk_kind"] == "assumed":
            label += "\n[ASSUMED]"
        labels.append(label)
    return labels


def _matrix_cells(axis, matrix):
    modes = [c["mode"] for c in matrix["columns"]]
    for i, row in enumerate(matrix["rows"]):
        expected = modes.index(row["expected_mode"])
        argmax = (modes.index(row["shape_only_argmax"])
                  if row["shape_only_argmax"] in modes else len(modes) - 1)
        axis.add_patch(Rectangle((expected - 0.48, i - 0.48), 0.96, 0.96,
                                 fill=False, edgecolor=ORANGE, lw=2.4))
        for j, probability in enumerate(row["probabilities_pct"]):
            label = f"{probability:.1f}" + (" *" if j == argmax else "")
            axis.text(j, i, label, ha="center", va="center", fontsize=8.5,
                      color="white" if probability > 48 else "#161616")


def matrix_figure(document, out):
    matrix = diagnostics_matrix(document["examples"])
    fig = plt.figure(figsize=(18, 9.7), dpi=150, facecolor="white")
    fig.suptitle("Ten plate conditions | diagnostics matrix from the computed pump cards",
                 x=0.035, y=0.965, ha="left", fontsize=19, weight="bold")
    fig.text(0.035, 0.917,
             "Probabilities: shape-only model on the pump card computed from the synthesised surface card; "
             "not measured (ASSUMED card space and scale).",
             fontsize=10, color=GREY)
    axis = fig.add_axes([0.18, 0.225, 0.60, 0.61])
    data = np.array([row["probabilities_pct"] for row in matrix["rows"]])
    image = axis.imshow(data, cmap="Blues", vmin=0, vmax=100, aspect="auto")
    axis.set_xticks(range(len(matrix["columns"])), _matrix_labels(matrix["columns"]),
                    fontsize=8)
    axis.set_yticks(range(len(matrix["rows"])),
                    [f"{r['index']:02d}  {r['english_gloss']}" for r in matrix["rows"]],
                    fontsize=10)
    axis.tick_params(length=0, pad=9)
    axis.set_xticks(np.arange(-0.5, 11, 1), minor=True)
    axis.set_yticks(np.arange(-0.5, 10, 1), minor=True)
    axis.grid(which="minor", color="white", linewidth=1)
    axis.tick_params(which="minor", bottom=False, left=False)
    _matrix_cells(axis, matrix)
    _shipped_column(fig, matrix)
    bar = fig.colorbar(image, cax=fig.add_axes([0.18, 0.105, 0.60, 0.018]),
                       orientation="horizontal")
    bar.set_label("Shape-only probability (%)", fontsize=10)
    caption = ("Figure. Probabilities: shape-only model on the pump card computed from the synthesised surface card. "
               "Orange outline: expected condition; *: single-mode argmax. "
               "All other modes aggregates non-plate classes. FLUID POUND and SAND "
               "ABRASION crosswalks are ASSUMED. Calls: HYPOTHESIS; field accuracy: "
               "NOT_EVALUATED. Source: DOI 10.17632/hnfwm9mjs6.1, CC0 1.0.")
    fig.text(0.035, 0.025, textwrap.fill(caption, 178), fontsize=9, color=GREY,
             linespacing=1.4)
    _save(fig, out, "diagnostics-matrix")


def _shipped_column(fig, matrix):
    axis = fig.add_axes([0.795, 0.225, 0.195, 0.61])
    axis.set_xlim(0, 1)
    axis.set_ylim(9.5, -0.5)
    axis.axis("off")
    axis.set_title("Shipped model | HYPOTHESIS", loc="left", fontsize=10, pad=14)
    for i, row in enumerate(matrix["rows"]):
        axis.text(0, i, row["shipped_model_call"], va="center", fontsize=8.5)


def render_figures(document, out):
    for record in document["examples"]:
        card_figure(record, out)
    matrix_figure(document, out)
