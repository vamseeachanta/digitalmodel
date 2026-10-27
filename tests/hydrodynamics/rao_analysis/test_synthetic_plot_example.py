# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Exercise the synthetic comparison without result files or a browser."""

import os
import runpy
import subprocess
import sys
import webbrowser
from pathlib import Path
from unittest.mock import patch

import plotly.graph_objects as go
import pytest

from digitalmodel.hydrodynamics.rao_analysis import RAOAnalysis


EXAMPLE = (
    Path(__file__).resolve().parents[3]
    / "examples/hydrodynamics/rao_analysis/basic_usage.py"
)


@pytest.fixture
def hand_values():
    """Independent fractions from README.md, 'Analytic basis'.

    T/8 - 1 = -1/2, 0, 1/2, 1 gives squared denominators
    5/4, 1, 5/4, 2; reciprocals and their three-quarter multiples
    below are evaluated independently of the example.
    """
    return {
        "periods": (4, 8, 12, 16),
        "Synthetic reference": (4 / 5, 1, 4 / 5, 1 / 2),
        "Synthetic scaled comparison": (3 / 5, 3 / 4, 3 / 5, 3 / 8),
    }


def test_synthetic_rao_plot_preserves_hand_values(hand_values, tmp_path, monkeypatch):
    monkeypatch.chdir(tmp_path)
    example = runpy.run_path(str(EXAMPLE))
    original = RAOAnalysis.add_trace_to_fig
    with patch.object(RAOAnalysis, "add_trace_to_fig", autospec=True) as add_trace:
        add_trace.side_effect = original
        figure = example["build_figure"]()

    expected_names = tuple(name for name in hand_values if name != "periods")
    assert add_trace.call_count == len(expected_names)
    assert tuple(trace.name for trace in figure.data) == expected_names
    for trace in figure.data:
        assert tuple(trace.x) == hand_values["periods"]
        assert tuple(trace.y) == pytest.approx(hand_values[trace.name])
        assert trace.mode == "lines+markers"
        assert trace.showlegend
    assert figure.layout.xaxis.title.text == "Period (s)"
    assert figure.layout.yaxis.title.text == "Amplitude RAO (m/m)"
    assert "no physical shielding qualification" in figure.layout.title.text
    assert list(tmp_path.iterdir()) == []


def test_import_writes_nothing(tmp_path, monkeypatch):
    monkeypatch.chdir(tmp_path)
    with (
        patch.object(go.Figure, "write_html") as write_html,
        patch.object(go.Figure, "show") as show,
        patch.object(webbrowser, "open") as open_browser,
    ):
        runpy.run_path(str(EXAMPLE))
    write_html.assert_not_called()
    show.assert_not_called()
    open_browser.assert_not_called()
    assert list(tmp_path.iterdir()) == []


def test_executable_entrypoint_writes_requested_html(tmp_path):
    output = tmp_path / "requested comparison.html"
    result = subprocess.run(
        [sys.executable, str(EXAMPLE), "--output", str(output)],
        cwd=tmp_path,
        env={**os.environ, "PYTHONPATH": os.pathsep.join(sys.path)},
        capture_output=True,
        text=True,
        check=True,
    )
    assert not result.stdout
    html = output.read_text(encoding="utf-8")
    assert "Plotly.newPlot" in html
    assert "Synthetic reference" in html
    assert "Synthetic scaled comparison" in html
    assert "Period (s)" in html
    # Plotly escapes slashes in embedded JSON; the decoded label retains units.
    assert "Amplitude RAO (m/m)" in html.replace(r"\u002f", "/")
    assert "no physical shielding qualification" in html
    assert 'src="https://' not in html
    assert list(tmp_path.iterdir()) == [output]


def test_entrypoint_exports_without_opening_browser(tmp_path, monkeypatch):
    output = tmp_path / "no-browser.html"
    monkeypatch.chdir(tmp_path)
    monkeypatch.setattr(sys, "argv", [str(EXAMPLE), "--output", str(output)])
    with (
        patch.object(go.Figure, "show") as show,
        patch.object(webbrowser, "open") as open_browser,
    ):
        runpy.run_path(str(EXAMPLE), run_name="__main__")
    show.assert_not_called()
    open_browser.assert_not_called()
    assert output.is_file()
    assert list(tmp_path.iterdir()) == [output]


def test_entrypoint_requires_explicit_output_without_opening_browser(
    tmp_path, monkeypatch
):
    monkeypatch.chdir(tmp_path)
    monkeypatch.setattr(sys, "argv", [str(EXAMPLE)])
    with (
        patch.object(go.Figure, "write_html") as write_html,
        patch.object(webbrowser, "open") as open_browser,
        pytest.raises(SystemExit) as stopped,
    ):
        runpy.run_path(str(EXAMPLE), run_name="__main__")
    assert stopped.value.code != 0
    write_html.assert_not_called()
    open_browser.assert_not_called()
    assert list(tmp_path.iterdir()) == []
