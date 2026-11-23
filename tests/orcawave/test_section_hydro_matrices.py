"""Tests for reporting/sections/hydro_matrices.py — build_hydro_matrices()."""
from __future__ import annotations

from unittest.mock import MagicMock, PropertyMock, patch
import numpy as np
import pytest

from digitalmodel.orcawave.reporting.config import HydroMatricesConfig
from digitalmodel.orcawave.reporting.sections.hydro_matrices import (
    build_hydro_matrices,
    _DOF_LABELS,
    _MAX_BODIES,
)


@pytest.fixture
def config():
    return HydroMatricesConfig()


@pytest.fixture
def synthetic_matrix_diff():
    """Two-body synthetic codes, independently defined rather than physical data.

    Hand-calculation basis: each row is one frequency in input order
    (0.5, 0.125, 0.25 Hz). Columns are Surge, Sway, Heave, Roll, Pitch, Yaw
    for the first body, then the same six DOFs for the second body. Added
    mass codes use row hundreds, body tens and DOF units; damping codes use
    row thousands, body tens and DOF units. The off-diagonal sentinel -999
    must never be plotted. These arbitrary codes distinguish every sample.

    Sorting frequencies by hand gives input rows 2, 3, 1. Reciprocals give
    periods 8, 4, 2 seconds: 1/0.125 = 8, 1/0.25 = 4, 1/0.5 = 2.
    Expected trace triples below are handwritten in that row order.
    """
    added_mass = [
        [101, 102, 103, 104, 105, 106, 111, 112, 113, 114, 115, 116],
        [201, 202, 203, 204, 205, 206, 211, 212, 213, 214, 215, 216],
        [301, 302, 303, 304, 305, 306, 311, 312, 313, 314, 315, 316],
    ]
    damping = [
        [1001, 1002, 1003, 1004, 1005, 1006, 1011, 1012, 1013, 1014, 1015, 1016],
        [2001, 2002, 2003, 2004, 2005, 2006, 2011, 2012, 2013, 2014, 2015, 2016],
        [3001, 3002, 3003, 3004, 3005, 3006, 3011, 3012, 3013, 3014, 3015, 3016],
    ]
    diff = MagicMock()
    diff.frequencies = np.array([0.5, 0.125, 0.25])
    for attribute, diagonals in (("addedMass", added_mass), ("damping", damping)):
        matrices = np.full((len(diagonals), len(diagonals[0]), len(diagonals[0])), -999.0)
        for matrix, diagonal in zip(matrices, diagonals):
            np.fill_diagonal(matrix, diagonal)
        setattr(diff, attribute, matrices)
    return diff


@pytest.fixture
def captured_matrix_figures(monkeypatch):
    """Capture real Plotly figures at the HTML boundary; missing Plotly fails."""
    import plotly.graph_objects as go

    figures = []

    def capture(figure, **kwargs):
        assert kwargs == {"include_plotlyjs": False, "full_html": False}
        figures.append(figure)
        return "<div>captured matrix figure</div>"

    monkeypatch.setattr(go.Figure, "to_html", capture)
    return figures


def test_hydro_matrix_trace_values_match_synthetic_data(
    synthetic_matrix_diff, captured_matrix_figures, config
):
    """Check every first-body diagonal against the fixture's hand-ordered codes."""
    build_hydro_matrices(synthetic_matrix_diff, config)
    assert len(captured_matrix_figures) == 2
    figure = captured_matrix_figures[0]
    expected = [
        ("A11", [201, 301, 101]), ("B11", [2001, 3001, 1001]),
        ("A22", [202, 302, 102]), ("B22", [2002, 3002, 1002]),
        ("A33", [203, 303, 103]), ("B33", [2003, 3003, 1003]),
        ("A44", [204, 304, 104]), ("B44", [2004, 3004, 1004]),
        ("A55", [205, 305, 105]), ("B55", [2005, 3005, 1005]),
        ("A66", [206, 306, 106]), ("B66", [2006, 3006, 1006]),
    ]
    assert len(figure.data) == len(expected)
    for trace, (name, values) in zip(figure.data, expected):
        assert trace.name == name
        np.testing.assert_array_equal(trace.x, [8, 4, 2])
        np.testing.assert_array_equal(trace.y, values)
    assert [annotation.text for annotation in figure.layout.annotations] == [
        "Surge diag", "Sway diag", "Heave diag", "Roll diag", "Pitch diag", "Yaw diag",
    ]
    # Six DOFs map to six subplot axes; each A/B pair shares its DOF's axes.
    assert [trace.xaxis for trace in figure.data] == [
        "x", "x", "x2", "x2", "x3", "x3", "x4", "x4", "x5", "x5", "x6", "x6",
    ]
    assert [trace.yaxis for trace in figure.data] == [
        "y", "y", "y2", "y2", "y3", "y3", "y4", "y4", "y5", "y5", "y6", "y6",
    ]


def test_hydro_matrix_second_body_uses_own_dofs(
    synthetic_matrix_diff, captured_matrix_figures, config
):
    """Handwritten second-body triples detect reuse or shifting of body columns."""
    build_hydro_matrices(synthetic_matrix_diff, config)
    assert len(captured_matrix_figures) == 2
    expected = [
        ("A11", [211, 311, 111]), ("B11", [2011, 3011, 1011]),
        ("A22", [212, 312, 112]), ("B22", [2012, 3012, 1012]),
        ("A33", [213, 313, 113]), ("B33", [2013, 3013, 1013]),
        ("A44", [214, 314, 114]), ("B44", [2014, 3014, 1014]),
        ("A55", [215, 315, 115]), ("B55", [2015, 3015, 1015]),
        ("A66", [216, 316, 116]), ("B66", [2016, 3016, 1016]),
    ]
    figure = captured_matrix_figures[1]
    assert len(figure.data) == len(expected)
    for trace, (name, values) in zip(figure.data, expected):
        assert trace.name == name
        np.testing.assert_array_equal(trace.x, [8, 4, 2])
        np.testing.assert_array_equal(trace.y, values)


@pytest.mark.parametrize(
    ("frequencies", "periods"),
    [([0.5, 0.125, 0.25], [8, 4, 2]), ([1.0, 0.25, 0.5], [4, 2, 1])],
)
def test_hydro_matrix_period_axis_independent(
    synthetic_matrix_diff, captured_matrix_figures, config, frequencies, periods
):
    """Doubling fixture frequencies halves T: 1/0.25=4, 1/0.5=2, 1/1=1."""
    synthetic_matrix_diff.frequencies = np.array(frequencies)
    build_hydro_matrices(synthetic_matrix_diff, config)
    assert len(captured_matrix_figures) == 2
    for figure in captured_matrix_figures:
        assert len(figure.data) == 12  # Six DOFs, each with added mass and damping.
        for trace in figure.data:
            np.testing.assert_array_equal(trace.x, periods)
        for axis in ("xaxis", "xaxis2", "xaxis3", "xaxis4", "xaxis5", "xaxis6"):
            assert figure.layout[axis].title.text == "Period (s)"


# ---------------------------------------------------------------------------
# build_hydro_matrices — plotly unavailable
# ---------------------------------------------------------------------------


class TestHydroMatricesNoPlotly:

    @patch.dict("sys.modules", {"plotly": None, "plotly.graph_objects": None})
    def test_returns_placeholder_without_plotly(self, mock_diff_single_body, config):
        result = build_hydro_matrices(mock_diff_single_body, config)
        assert "Plotly required" in result
        assert result == (
            "<p class='section-placeholder'>Plotly required for hydro matrices.</p>"
        )


# ---------------------------------------------------------------------------
# build_hydro_matrices — data unavailable
# ---------------------------------------------------------------------------


class TestHydroMatricesDataUnavailable:

    def test_returns_warning_when_added_mass_fails(self, config):
        diff = MagicMock()
        type(diff).addedMass = PropertyMock(side_effect=RuntimeError("no AM"))
        result = build_hydro_matrices(diff, config)
        assert "alert-warning" in result
        assert "unavailable" in result.lower()
        assert "no AM" in result

    @pytest.mark.parametrize("attribute", ["damping", "frequencies"])
    def test_returns_warning_when_other_data_fails(
        self, attribute, synthetic_matrix_diff, captured_matrix_figures, config
    ):
        setattr(
            type(synthetic_matrix_diff), attribute,
            PropertyMock(side_effect=RuntimeError("synthetic data unavailable")),
        )
        result = build_hydro_matrices(synthetic_matrix_diff, config)
        assert "alert-warning" in result
        assert "Hydrodynamic matrices unavailable: synthetic data unavailable" in result
        assert captured_matrix_figures == []


# ---------------------------------------------------------------------------
# build_hydro_matrices — with plotly and valid data
# ---------------------------------------------------------------------------


class TestHydroMatricesWithPlotly:

    def test_returns_html_string(self, mock_diff_single_body, config):
        result = build_hydro_matrices(mock_diff_single_body, config)
        assert isinstance(result, str)
        assert len(result) > 0

    def test_contains_card_structure(self, mock_diff_single_body, config):
        result = build_hydro_matrices(mock_diff_single_body, config)
        assert "card" in result
        assert "card-header" in result
        assert "card-body" in result

    def test_single_body_has_one_card(self, mock_diff_single_body, config):
        result = build_hydro_matrices(mock_diff_single_body, config)
        # Single body -> "Body" (not "Body 1")
        assert "Body" in result

    def test_two_body_has_two_cards(self, mock_diff_two_body, config):
        result = build_hydro_matrices(mock_diff_two_body, config)
        assert "Body 1" in result
        assert "Body 2" in result

    def test_card_mentions_added_mass_and_damping(self, mock_diff_single_body, config):
        result = build_hydro_matrices(mock_diff_single_body, config)
        assert "Added Mass" in result
        assert "Damping" in result or "damping" in result.lower()

    def test_max_bodies_limit(self, config):
        """Even with 3+ bodies, only first _MAX_BODIES are plotted."""
        diff = MagicMock()
        n_dofs = 18  # 3 bodies
        diff.addedMass = np.zeros((4, n_dofs, n_dofs))
        diff.damping = np.zeros((4, n_dofs, n_dofs))
        diff.frequencies = np.array([0.3, 0.2, 0.1, 0.05])
        result = build_hydro_matrices(diff, config)
        # Should show at most _MAX_BODIES cards
        assert result.count("card-header") <= _MAX_BODIES


# ---------------------------------------------------------------------------
# Module constants
# ---------------------------------------------------------------------------


class TestHydroConstants:

    def test_dof_labels_has_six(self):
        assert len(_DOF_LABELS) == 6
        assert "Surge" in _DOF_LABELS
        assert "Yaw" in _DOF_LABELS

    def test_max_bodies_positive(self):
        assert _MAX_BODIES > 0
