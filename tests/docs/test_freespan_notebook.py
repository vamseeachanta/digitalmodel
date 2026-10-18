# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Execute the freespan notebook cells and compare saved results with the demo."""

from __future__ import annotations

import contextlib
import io
import json
import sys
from pathlib import Path

import pytest


ROOT = Path(__file__).resolve().parents[2]
NOTEBOOK = ROOT / "docs/notebooks/freespan-viv-demo.ipynb"
PLOTLY_MIME = "application/vnd.plotly.v1+json"


@pytest.fixture(scope="module")
def executed():
    """Execute ordinary Python cells with captured notebook display messages."""
    assert NOTEBOOK.is_file(), "The executed freespan walkthrough is missing"
    notebook = json.loads(NOTEBOOK.read_text(encoding="utf-8"))
    displays = []
    namespace = {"display": lambda data, raw: displays.append(data)}
    streams = []
    original_path = sys.path[:]
    try:
        for cell in notebook["cells"]:
            if cell["cell_type"] != "code":
                continue
            source = "".join(cell["source"])
            output = io.StringIO()
            with contextlib.redirect_stdout(output):
                exec(compile(source, "freespan-viv-demo.ipynb", "exec"), namespace)
            streams.append(output.getvalue())
    finally:
        sys.path[:] = original_path
    return notebook, namespace, streams, displays


def test_printed_results_match_demo_outputs(executed):
    """Detect changes in printed frequency, onset, gap and size results."""
    _, ns, streams, _ = executed
    demo = ns["demo"]
    results, frame = demo.run_parametric_sweep(ns["pipes"], ns["jumper"], ns["config"])
    assert ns["results"] == results
    assert ns["frame"].equals(frame)
    assert f"Computed cases: {len(results)}" in "".join(streams)
    case = results[0]
    assert ns["case"] == case
    printed = "".join(streams)
    assert f"Natural frequency (Hz): {case['f_n_hz']:.1f}" in printed
    assert f"Reduced velocity (-): {case['v_r']:.1f}" in printed
    assert f"Screening status: {case['status']}" in printed
    for row in ns["span_rows"]:
        pipe = demo.get_pipe_by_size(ns["pipes"], row["size"])
        span = demo.calc_max_allowable_span_status(
            pipe["od_m"], demo.get_thinnest_wt(pipe)["wt_m"], row["current"],
            e_over_d=row["gap"], content_density=ns["density"],
            coating_thickness_m=pipe["anti_corrosion_coating"]["thickness_m"],
            c_n=ns["boundary"],
        )
        assert row["span"] == span["span_m"]
        assert row["status"] == span["status"]
        assert f"{row['size']} | {row['current']:.1f} | {row['gap']:.1f} | {row['span']:.1f}" in printed


def test_saved_outputs_are_current_and_executed(executed):
    """Reject stale saved text, errors, or unexecuted code cells."""
    notebook, _, streams, _ = executed
    cells = [cell for cell in notebook["cells"] if cell["cell_type"] == "code"]
    assert [cell["execution_count"] for cell in cells] == list(range(1, len(cells) + 1))
    for cell, fresh in zip(cells, streams):
        assert not any(output["output_type"] == "error" for output in cell["outputs"])
        saved = "".join(
            "".join(output["text"]) for output in cell["outputs"]
            if output["output_type"] == "stream" and output["name"] == "stdout"
        )
        assert saved == fresh


def test_span_map_matches_computed_rows_without_remote_assets(executed):
    """Check every plotted value and prohibit remote executable output assets."""
    notebook, ns, _, displays = executed
    fresh_figures = [item[PLOTLY_MIME] for item in displays]
    saved_figures = [
        output["data"][PLOTLY_MIME] for cell in notebook["cells"]
        for output in cell.get("outputs", []) if PLOTLY_MIME in output.get("data", {})
    ]
    assert fresh_figures == saved_figures
    assert len(saved_figures) == len(ns["config"].pipelines.sizes)
    for figure, size in zip(saved_figures, ns["config"].pipelines.sizes):
        trace = figure["data"][0]
        expected = [
            [round(row["span"], 1) for row in ns["span_rows"]
             if row["size"] == size and row["gap"] == gap]
            for gap in ns["config"].pipelines.gap_ratios
        ]
        assert trace["z"] == expected
        assert trace["x"] == ns["config"].pipelines.current_velocities_ms
        assert trace["y"] == ns["config"].pipelines.gap_ratios
    for cell in notebook["cells"]:
        for output in cell.get("outputs", []):
            assert set(output.get("data", {})) <= {PLOTLY_MIME, "text/plain"}


def test_frequency_walkthrough_uses_demo_mass_and_stiffness(executed):
    """Check the displayed beam calculation against the module before rounding."""
    _, ns, _, _ = executed
    case = ns["case"]
    demo = ns["demo"]
    frequency = demo.calc_natural_frequency(
        case["od_m"], case["wt_m"], case["span_m"],
        content_density=ns["density"], coating_thickness_m=ns["coating"],
        c_n=ns["boundary"],
    )
    assert ns["frequency_from_parts"] == pytest.approx(frequency)
    assert round(frequency, 4) == case["f_n_hz"]


def test_pipe_size_differences_use_unrounded_module_spans(executed):
    """Detect incorrect size differences or percentages in the printed comparison."""
    _, ns, streams, _ = executed
    printed = "".join(streams)
    reference = ns["comparison"][0]["span"]
    for row in ns["comparison"]:
        pipe = ns["demo"].get_pipe_by_size(ns["pipes"], row["size"])
        wall = ns["demo"].get_thinnest_wt(pipe)["wt_m"]
        difference = row["span"] - reference
        percentage = difference / reference * 100
        assert (
            f"{row['size']} | {wall:.3f} | {row['span']:.1f} | "
            f"{difference:.1f} | {percentage:.1f}"
        ) in printed
