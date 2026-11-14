"""Check faithful display of synthetic inputs using real Agg artists."""

from types import SimpleNamespace
from unittest.mock import Mock

import matplotlib
import numpy as np
import pandas as pd
import pytest

matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.axes import Axes
from matplotlib.colors import to_rgba

from digitalmodel.fatigue import plotting


@pytest.fixture(autouse=True)
def close_figures():
    """Close every figure created by a test, including on assertion failure."""
    existing = set(plt.get_fignums())
    yield
    for number in set(plt.get_fignums()) - existing:
        plt.close(number)


@pytest.fixture
def curve():
    # Declared ordinates only: numerical S-N qualification belongs elsewhere.
    return SimpleNamespace(cycles=Mock(return_value=np.linspace(1e4, 2e6, 200)))


@pytest.fixture
def damage():
    return pd.DataFrame({"stress_range": [100, 200], "damage": [0.125, 0.25]})


def test_sn_creates_axes(curve):
    ax = plotting.plot_sn_curve(curve)

    assert isinstance(ax, Axes)
    assert matplotlib.get_backend().lower() == "agg"
    assert len(ax.lines) == 1
    assert ax.lines[0].get_label() == "S-N Curve"
    assert ax.get_legend().get_texts()[0].get_text() == "S-N Curve"


def test_sn_reuses_supplied_axes(curve):
    _, supplied = plt.subplots()
    figures = plt.get_fignums()

    assert plotting.plot_sn_curve(curve, ax=supplied) is supplied
    assert plt.get_fignums() == figures
    assert len(supplied.lines) == 1


def test_sn_named_lookup_transports_values_and_default_label(monkeypatch, curve):
    lookup = Mock(return_value=curve)
    monkeypatch.setattr(plotting, "get_sn_curve", lookup)

    ax = plotting.plot_sn_curve("F")

    lookup.assert_called_once_with("F")
    curve.cycles.assert_called_once()
    stresses = curve.cycles.call_args.args[0]
    assert len(stresses) == 200
    assert stresses[[0, -1]] == pytest.approx([10, 500])
    assert np.all(np.diff(stresses) > 0)
    line = ax.lines[0]
    np.testing.assert_array_equal(line.get_xdata(), curve.cycles.return_value)
    np.testing.assert_array_equal(line.get_ydata(), stresses)
    assert line.get_label() == "DNV F"
    assert ax.get_legend().get_texts()[0].get_text() == "DNV F"


@pytest.mark.parametrize("named", [False, True])
def test_sn_explicit_label_and_style(monkeypatch, curve, named):
    monkeypatch.setattr(plotting, "get_sn_curve", Mock(return_value=curve))

    ax = plotting.plot_sn_curve(
        "F" if named else curve,
        label="Synthetic curve", color="purple", linestyle=":", linewidth=2.5,
    )

    line = ax.lines[0]
    assert line.get_label() == "Synthetic curve"
    assert line.get_color() == "purple"
    assert line.get_linestyle() == ":"
    assert line.get_linewidth() == 2.5
    assert ax.get_legend().get_texts()[0].get_text() == "Synthetic curve"


def test_sn_log_scales_and_unit_labels(curve):
    ax = plotting.plot_sn_curve(curve)

    assert ax.get_xscale() == "log"
    assert ax.get_yscale() == "log"
    assert ax.get_xlabel() == "Cycles to Failure (N)"
    assert ax.get_ylabel() == "Stress Range (MPa)"
    assert ax.get_title() == "S-N Curve"


def test_histogram_bar_heights_and_labels(damage):
    ax = plotting.plot_damage_histogram(damage)

    assert isinstance(ax, Axes)
    assert [bar.get_height() for bar in ax.patches] == [0.125, 0.25]
    assert [label.get_text() for label in ax.get_xticklabels()] == ["100", "200"]
    assert ax.get_xlabel() == "Stress Range (MPa)"
    assert ax.get_ylabel() == "Damage (n/N)"


def test_histogram_reuses_axes_and_forwards_style(damage):
    _, supplied = plt.subplots()
    figures = plt.get_fignums()

    ax = plotting.plot_damage_histogram(
        damage, ax=supplied, color="orange", width=0.5, alpha=0.4,
    )

    assert ax is supplied
    assert plt.get_fignums() == figures
    for bar in ax.patches:
        assert bar.get_width() == 0.5
        assert bar.get_facecolor() == to_rgba("orange", alpha=0.4)


def test_histogram_missing_total_uses_hand_sum_without_mutating_input(damage):
    damage.attrs["source"] = "synthetic"
    original = damage.copy(deep=True)

    ax = plotting.plot_damage_histogram(damage)

    # Independent hand sum: 0.125 + 0.25 = 0.375.
    assert ax.get_title() == "Damage per Bin — Total D = 0.3750"
    pd.testing.assert_frame_equal(damage, original)
    assert damage.attrs == {"source": "synthetic"}


@pytest.mark.parametrize("total, text", [(0.9, "0.9000"), (0.0, "0.0000")])
def test_histogram_explicit_metadata_takes_precedence(damage, total, text):
    damage.attrs["total_damage"] = total

    ax = plotting.plot_damage_histogram(damage)

    assert ax.get_title() == f"Damage per Bin — Total D = {text}"
    assert damage.attrs == {"total_damage": total}
    assert [bar.get_height() for bar in ax.patches] == [0.125, 0.25]


def test_histogram_empty_input_has_zero_total():
    damage = pd.DataFrame({"stress_range": [], "damage": []})

    ax = plotting.plot_damage_histogram(damage)

    assert len(ax.patches) == 0
    assert ax.get_title() == "Damage per Bin — Total D = 0.0000"
    assert damage.empty
    assert damage.attrs == {}
