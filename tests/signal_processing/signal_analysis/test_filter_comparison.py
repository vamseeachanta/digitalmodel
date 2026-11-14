# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Analytical synthetic controls for time-trace and amplitude comparisons.

Distinct integer-bin sinusoids are orthogonal over the record. Expected
traces use direct sine samples; expected amplitudes use their coefficients.
Absolute tolerance 1e-12 permits double-precision transform roundoff, with
zero relative tolerance so removed components receive no relative slack.
"""

import json
import socket
import webbrowser

import numpy as np
import plotly.graph_objects as go
import plotly.io as pio
import pytest

from digitalmodel.signal_processing.signal_analysis.visualization import (
    plot_filter_comparison,
)


@pytest.mark.parametrize("length", [63, 64])
@pytest.mark.parametrize(
    "low, high, retained",
    [
        (None, 4, (0,)),
        (12, None, (1,)),
        (4, 4, (0,)),
        (None, None, (0, 1)),
        (5, 11, ()),
    ],
    ids=["lowpass", "highpass", "bandpass", "identity", "empty-band"],
)
def test_comparison_matches_analytical_traces_and_amplitudes(
    length, low, high, retained
):
    """N=fs gives 1 Hz bins; tones have amplitudes 2 and 5 at 4 and 12 Hz."""
    time = np.arange(length) / length
    components = [
        2 * np.sin(2 * np.pi * 4 * time),
        5 * np.sin(2 * np.pi * 12 * time + np.pi / 3),
    ]
    signal = sum(components)
    original = signal.copy()
    signal.flags.writeable = False
    expected = sum((components[i] for i in retained), np.zeros(length))

    figure = plot_filter_comparison(signal, length, low, high, units="mm")

    assert isinstance(figure, go.Figure)
    assert len(figure.data) == 4
    source, filtered, source_spectrum, filtered_spectrum = figure.data
    for trace, samples in [(source, signal), (filtered, expected)]:
        np.testing.assert_array_equal(trace.x, time)
        np.testing.assert_allclose(trace.y, samples, rtol=0, atol=1e-12)
        assert trace.mode == "lines"
        assert trace.xaxis == "x" and trace.yaxis == "y"
    amplitudes = np.zeros(length // 2 + 1)
    amplitudes[[4, 12]] = [2, 5]
    kept_amplitudes = np.zeros_like(amplitudes)
    for i in retained:
        kept_amplitudes[[4, 12][i]] = [2, 5][i]
    for trace, values in [
        (source_spectrum, amplitudes),
        (filtered_spectrum, kept_amplitudes),
    ]:
        np.testing.assert_array_equal(trace.x, np.arange(length // 2 + 1))
        np.testing.assert_allclose(trace.y, values, rtol=0, atol=1e-12)
        assert trace.xaxis == "x2" and trace.yaxis == "y2"
    assert [trace.name for trace in figure.data] == [
        "Original",
        "Filtered",
        "Original",
        "Filtered",
    ]
    assert figure.layout.xaxis.title.text == "Time (s)"
    assert figure.layout.xaxis2.title.text == "Frequency (Hz)"
    assert figure.layout.yaxis.title.text == "Signal (mm)"
    assert figure.layout.yaxis2.title.text == "Amplitude (mm)"
    np.testing.assert_array_equal(signal, original)


@pytest.mark.parametrize("length", [1, 2, 15, 16])
def test_endpoint_amplitudes_and_short_records(length):
    """DC=7; highest cosine=3, including the odd-length final interior bin."""
    samples = np.arange(length)
    signal = 7 + 3 * np.cos(2 * np.pi * (length // 2) * samples / length)

    figure = plot_filter_comparison(signal.tolist(), length)

    amplitudes = np.zeros(length // 2 + 1)
    amplitudes[0] = 7
    amplitudes[-1] += 3
    for trace in figure.data[2:]:
        np.testing.assert_allclose(trace.y, amplitudes, rtol=0, atol=1e-12)
    np.testing.assert_allclose(figure.data[1].y, signal, rtol=0, atol=1e-12)
    assert figure.layout.yaxis.title.text == "Signal (input units)"


def test_coordinates_use_sampling_rate_independently_of_record_length():
    """N=10, fs=3 gives time step 1/3 s and frequency step 0.3 Hz."""
    samples = np.arange(10)
    signal = 2 * np.cos(2 * np.pi * 3 * samples / 10)

    figure = plot_filter_comparison(signal, np.float64(3), 0.9, 0.9)

    for trace in figure.data[:2]:
        np.testing.assert_array_equal(trace.x, samples / 3)
        np.testing.assert_allclose(trace.y, signal, rtol=0, atol=1e-12)
    for trace in figure.data[2:]:
        np.testing.assert_allclose(trace.x, np.arange(6) * 0.3, rtol=0, atol=1e-12)
        assert trace.y[3] == pytest.approx(2, rel=0, abs=1e-12)


@pytest.mark.parametrize(
    "signal, rate, low, high, message",
    [
        ([], 8, None, None, "signal"),
        ([[1, 2]], 8, None, None, "signal"),
        ([1 + 0j], 8, None, None, "signal"),
        ([np.nan], 8, None, None, "signal"),
        (["1"], 8, None, None, "signal"),
        ([1], 0, None, None, "sampling_rate"),
        ([1], np.inf, None, None, "sampling_rate"),
        ([1], "8", None, None, "sampling_rate"),
        ([1], 8, -1, None, "low_cutoff"),
        ([1], 8, None, 5, "high_cutoff"),
        ([1], 8, np.nan, None, "low_cutoff"),
        ([1], 8, 3, 2, "low_cutoff.*high_cutoff"),
    ],
)
def test_invalid_filter_arguments_remain_errors(signal, rate, low, high, message):
    """Invalid records and cutoffs propagate the numeric kernel's ValueError."""
    with pytest.raises(ValueError, match=message):
        plot_filter_comparison(signal, rate, low, high)


def test_nonfinite_time_coordinates_raise():
    """A finite subnormal rate can overflow time without invalidating the kernel."""
    with pytest.raises(ValueError, match="finite.*coordinates"):
        plot_filter_comparison([1, -2, 3], np.nextafter(0.0, 1.0))


def test_nonfinite_amplitude_coordinates_raise():
    """Finite samples can overflow the transform and must not yield plot gaps."""
    with pytest.raises(ValueError, match="finite.*coordinates"):
        plot_filter_comparison([1e308, 1e308], 2)


def test_comparison_has_no_automatic_output(monkeypatch, tmp_path):
    """Calling the helper returns data without writing, display or connections."""

    def unexpected_output(*_args, **_kwargs):
        pytest.fail("comparison must return its figure without external output")

    monkeypatch.chdir(tmp_path)
    monkeypatch.setattr(go.Figure, "show", unexpected_output)
    monkeypatch.setattr(go.Figure, "write_html", unexpected_output)
    monkeypatch.setattr(pio, "show", unexpected_output)
    monkeypatch.setattr(pio, "write_html", unexpected_output)
    monkeypatch.setattr(webbrowser, "open", unexpected_output)
    monkeypatch.setattr(socket, "create_connection", unexpected_output)

    plot_filter_comparison([7, 10, 7, 4], 4, high_cutoff=1)

    assert list(tmp_path.iterdir()) == []


def test_standalone_html_preserves_both_panels_and_four_traces(tmp_path):
    """Explicit export embeds Plotly assets and the tested figure data offline."""
    figure = plot_filter_comparison([7, 10, 7, 4], 4, high_cutoff=1)
    output = tmp_path / "comparison.html"

    figure.write_html(output, include_plotlyjs=True, full_html=True, auto_open=False)

    html = output.read_text(encoding="utf-8")
    assert "plotly.js v" in html
    assert '<script src="' not in html
    arguments = html.rsplit("Plotly.newPlot(", 1)[1].lstrip()
    decoder = json.JSONDecoder()
    decoded = []
    for _ in range(3):
        value, offset = decoder.raw_decode(arguments)
        decoded.append(value)
        arguments = arguments[offset:].lstrip(" ,\n")
    _, traces, layout = decoded
    assert len(traces) == 4
    for saved, trace in zip(traces, figure.data):
        assert saved["name"] == trace.name
        np.testing.assert_allclose(saved["y"], trace.y, rtol=0, atol=1e-12)
    assert layout["xaxis"]["title"]["text"] == "Time (s)"
    assert layout["xaxis2"]["title"]["text"] == "Frequency (Hz)"
