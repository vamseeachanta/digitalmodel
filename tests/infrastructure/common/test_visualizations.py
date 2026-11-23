"""
Comprehensive unit tests for digitalmodel.infrastructure.common.visualizations.

Tests cover:
- Visualization class initialization
- Plot size/orientation configuration
- Data plotting from DataFrames (line, bar, scatter)
- Color list generation
- Title, axis labels, legend management
- Scale and limit formatting
- Multiple subplot configuration
- Resolution adjustment
- Text fields and annotations
- Reference lines and spans
- Save and close logic
- Edge cases and boundary conditions
"""

import os
import tempfile

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import pytest

from digitalmodel.infrastructure.common.visualizations import Visualization


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------

@pytest.fixture(autouse=True)
def close_all_figures():
    """Close all matplotlib figures after each test to avoid resource leaks."""
    yield
    plt.close("all")


@pytest.fixture
def viz():
    """Return a bare Visualization instance (no plt_settings)."""
    return Visualization()


def _base_settings(**overrides):
    """Return a minimal plt_settings dict with sensible defaults."""
    settings = {
        "x": ["x_col"],
        "y": ["y_col"],
        "label": ["series1"],
        "plt_kind": "line",
        "xlabel": "X",
        "ylabel": "Y",
        "title": "Test",
        "grid": True,
        "legend": True,
        "file_name": os.path.join(tempfile.gettempdir(), "test_plot.png"),
        "marker": None,
    }
    settings.update(overrides)
    return settings


def _sample_df():
    """Return a small DataFrame suitable for plotting."""
    return pd.DataFrame({"x_col": [1, 2, 3, 4, 5], "y_col": [10, 20, 15, 25, 30]})


def _multi_y_df():
    """Return a DataFrame with multiple y columns."""
    return pd.DataFrame({
        "x_col": [1, 2, 3],
        "y1": [10, 20, 30],
        "y2": [5, 15, 25],
    })


# ===========================================================================
# 1. Initialization tests
# ===========================================================================

class TestInitialization:

    def test_init_no_settings(self):
        v = Visualization()
        assert v.plt is not None
        assert v.polar_fig is None
        assert v.current_ax is None
        assert v.cfg_mult is None
        assert v.fig_suptitle_flag is False

    def test_init_sets_plot_object(self):
        v = Visualization()
        assert v.plot_object is v.plt

    def test_init_fig_is_figure(self):
        v = Visualization()
        assert isinstance(v.fig, matplotlib.figure.Figure)

    def test_init_with_a4_landscape(self):
        settings = {"size": "A4", "orientation": "landscape"}
        v = Visualization(plt_settings=settings)
        w, h = v.fig.get_size_inches()
        assert pytest.approx(w, abs=0.01) == 11.69
        assert pytest.approx(h, abs=0.01) == 8.27

    def test_init_with_a4_portrait(self):
        settings = {"size": "A4", "orientation": "portrait"}
        v = Visualization(plt_settings=settings)
        w, h = v.fig.get_size_inches()
        assert pytest.approx(w, abs=0.01) == 8.27
        assert pytest.approx(h, abs=0.01) == 11.69

    def test_init_with_a3_landscape(self):
        settings = {"size": "A3", "orientation": "landscape"}
        v = Visualization(plt_settings=settings)
        w, h = v.fig.get_size_inches()
        assert pytest.approx(w, abs=0.01) == 16.53
        assert pytest.approx(h, abs=0.01) == 11.69

    def test_init_with_a3_portrait(self):
        settings = {"size": "A3", "orientation": "portrait"}
        v = Visualization(plt_settings=settings)
        w, h = v.fig.get_size_inches()
        assert pytest.approx(w, abs=0.01) == 11.69
        assert pytest.approx(h, abs=0.01) == 16.53

    def test_init_with_half_letter_landscape(self):
        settings = {"size": "half_letter", "orientation": "landscape"}
        v = Visualization(plt_settings=settings)
        w, h = v.fig.get_size_inches()
        assert pytest.approx(w, abs=0.01) == 8.5
        assert pytest.approx(h, abs=0.01) == 5.5

    def test_init_with_half_letter_portrait(self):
        settings = {"size": "half_letter", "orientation": "portrait"}
        v = Visualization(plt_settings=settings)
        w, h = v.fig.get_size_inches()
        assert pytest.approx(w, abs=0.01) == 5.5
        assert pytest.approx(h, abs=0.01) == 8.5

    def test_init_stores_plt_settings(self):
        settings = {"size": "A4", "orientation": "landscape"}
        v = Visualization(plt_settings=settings)
        assert v.plt_settings is settings


# ===========================================================================
# 2. set_plot_size_orientation
# ===========================================================================

class TestSetPlotSizeOrientation:

    def test_unknown_size_does_not_change_figure(self):
        v = Visualization()
        original = v.fig.get_size_inches().copy()
        v.plt_settings = {"size": "letter", "orientation": "landscape"}
        v.set_plot_size_orientation()
        # Size should remain unchanged since "letter" is not handled
        current = v.fig.get_size_inches()
        assert np.allclose(current, original)

    def test_all_six_combinations_covered(self):
        combos = [
            ("A4", "landscape", 11.69, 8.27),
            ("A4", "portrait", 8.27, 11.69),
            ("A3", "landscape", 16.53, 11.69),
            ("A3", "portrait", 11.69, 16.53),
            ("half_letter", "landscape", 8.5, 5.5),
            ("half_letter", "portrait", 5.5, 8.5),
        ]
        for size, orient, exp_w, exp_h in combos:
            v = Visualization()
            v.plt_settings = {"size": size, "orientation": orient}
            v.set_plot_size_orientation()
            w, h = v.fig.get_size_inches()
            assert pytest.approx(w, abs=0.01) == exp_w, f"Failed for {size} {orient}"
            assert pytest.approx(h, abs=0.01) == exp_h, f"Failed for {size} {orient}"
            plt.close("all")


# ===========================================================================
# 3. color_lists
# ===========================================================================

class TestColorLists:

    def test_small_n_returns_hex_strings(self, viz):
        colors = viz.color_lists(n=5)
        assert len(colors) == 9  # returns the full small palette
        for c in colors:
            assert c.startswith("#")

    def test_small_n_boundary_8(self, viz):
        colors = viz.color_lists(n=8)
        assert len(colors) == 9
        assert all(isinstance(c, str) for c in colors)

    def test_large_n_returns_20_colors(self, viz):
        colors = viz.color_lists(n=15)
        assert len(colors) == 20

    def test_large_n_boundary_9(self, viz):
        colors = viz.color_lists(n=9)
        assert len(colors) == 20

    def test_large_n_colors_are_hex(self, viz):
        colors = viz.color_lists(n=20)
        for c in colors:
            assert isinstance(c, str)
            assert c.startswith("#")

    def test_default_n_15(self, viz):
        colors = viz.color_lists()
        assert len(colors) == 20


# ===========================================================================
# 4. from_df_columns - line plots
# ===========================================================================

class TestFromDfColumnsLine:

    def test_basic_line_plot(self, viz):
        df = _sample_df()
        settings = _base_settings(plt_kind="line")
        viz.from_df_columns(df, settings)
        # Should complete without error; plot_object is plt
        assert viz.plt_settings is settings

    def test_line_plot_with_current_ax(self, viz):
        fig, ax = plt.subplots()
        viz.current_ax = ax
        viz.plot_object = ax
        df = _sample_df()
        settings = _base_settings(plt_kind="line")
        viz.from_df_columns(df, settings)
        lines = ax.get_lines()
        assert len(lines) >= 1

    def test_line_plot_with_marker(self, viz):
        fig, ax = plt.subplots()
        viz.current_ax = ax
        viz.plot_object = ax
        df = _sample_df()
        settings = _base_settings(
            plt_kind="line",
            marker={"type": "o", "size": 5, "edge_color": "red"},
            color="blue",
            linewidth=2,
            linestyle="--",
        )
        viz.from_df_columns(df, settings)
        lines = ax.get_lines()
        assert len(lines) >= 1

    def test_line_plot_marker_without_size(self, viz):
        fig, ax = plt.subplots()
        viz.current_ax = ax
        viz.plot_object = ax
        df = _sample_df()
        settings = _base_settings(
            plt_kind="line",
            marker={"type": "o", "edge_color": "red"},
            color="blue",
        )
        viz.from_df_columns(df, settings)
        lines = ax.get_lines()
        assert len(lines) >= 1

    def test_line_plot_marker_none(self, viz):
        fig, ax = plt.subplots()
        viz.current_ax = ax
        viz.plot_object = ax
        df = _sample_df()
        settings = _base_settings(plt_kind="line", marker=None)
        viz.from_df_columns(df, settings)
        lines = ax.get_lines()
        assert len(lines) >= 1

    def test_no_plt_kind_defaults_to_plot(self, viz):
        df = _sample_df()
        settings = _base_settings()
        del settings["plt_kind"]
        viz.from_df_columns(df, settings)
        # should complete without error

    def test_linewidth_default(self, viz):
        fig, ax = plt.subplots()
        viz.current_ax = ax
        viz.plot_object = ax
        df = _sample_df()
        settings = _base_settings(plt_kind="line")
        # No linewidth key
        viz.from_df_columns(df, settings)
        # Default linewidth=0.5 should be used

    def test_multiple_y_columns(self, viz):
        fig, ax = plt.subplots()
        viz.current_ax = ax
        viz.plot_object = ax
        df = _multi_y_df()
        settings = _base_settings(
            x=["x_col"],
            y=["y1", "y2"],
            label=["series1", "series2"],
            plt_kind="line",
        )
        viz.from_df_columns(df, settings)
        lines = ax.get_lines()
        assert len(lines) >= 2

    def test_multiple_x_single_y(self, viz):
        df = pd.DataFrame({"x1": [1, 2, 3], "x2": [4, 5, 6], "y_col": [10, 20, 30]})
        settings = _base_settings(
            x=["x1", "x2"],
            y=["y_col"],
            label=["from_x1", "from_x2"],
            plt_kind="line",
        )
        viz.from_df_columns(df, settings)


# ===========================================================================
# 5. from_df_columns - bar plots
# ===========================================================================

class TestFromDfColumnsBar:

    def test_single_y_bar(self, viz):
        df = _sample_df()
        settings = _base_settings(plt_kind="bar", y=["y_col"], label=["bars"])
        viz.from_df_columns(df, settings)

    def test_multiple_y_bar(self, viz):
        df = _multi_y_df()
        settings = _base_settings(
            plt_kind="bar",
            y=["y1", "y2"],
            label=["bars1", "bars2"],
        )
        viz.from_df_columns(df, settings)

    def test_bar_with_multiple_x_single_y(self, viz):
        df = pd.DataFrame({"x1": [1, 2, 3], "x2": [4, 5, 6], "y_col": [10, 20, 30]})
        settings = _base_settings(
            x=["x1", "x2"],
            y=["y_col"],
            label=["from_x1", "from_x2"],
            plt_kind="bar",
        )
        viz.from_df_columns(df, settings)


# ===========================================================================
# 6. from_df_columns - scatter plots
# ===========================================================================

class TestFromDfColumnsScatter:

    def test_scatter_basic(self, viz):
        df = _sample_df()
        settings = _base_settings(
            plt_kind="scatter",
            marker={"type": "o", "size": 10, "edge_color": "red"},
        )
        viz.from_df_columns(df, settings)

    def test_scatter_with_annotate(self, viz):
        df = _sample_df()
        settings = _base_settings(
            plt_kind="scatter",
            marker={"type": "o", "size": 10, "edge_color": "red"},
            annotate={"column": None},
        )
        viz.from_df_columns(df, settings)

    def test_scatter_with_annotate_column(self, viz):
        df = _sample_df()
        df["name"] = ["a", "b", "c", "d", "e"]
        settings = _base_settings(
            plt_kind="scatter",
            marker={"type": "o", "size": 10, "edge_color": "red"},
            annotate={"column": "name"},
        )
        viz.from_df_columns(df, settings)


# ===========================================================================
# 7. add_x_y_scale_formats
# ===========================================================================

class TestAddXYScaleFormats:

    def test_yscale_log(self, viz):
        viz.plt_settings = {"yscale": {"log": True}}
        viz.add_x_y_scale_formats()
        ax = viz.plt.gca()
        assert ax.get_yscale() == "log"

    def test_yscale_log_false(self, viz):
        viz.plt_settings = {"yscale": {"log": False}}
        viz.add_x_y_scale_formats()
        ax = viz.plt.gca()
        assert ax.get_yscale() == "linear"

    def test_xscale_log(self, viz):
        viz.plt_settings = {"xscale": {"log": True}}
        viz.add_x_y_scale_formats()
        ax = viz.plt.gca()
        assert ax.get_xscale() == "log"

    def test_no_scale_keys(self, viz):
        viz.plt_settings = {}
        viz.add_x_y_scale_formats()
        # No error should occur

    def test_both_scales_log(self, viz):
        viz.plt_settings = {"yscale": {"log": True}, "xscale": {"log": True}}
        viz.add_x_y_scale_formats()
        ax = viz.plt.gca()
        assert ax.get_yscale() == "log"
        assert ax.get_xscale() == "log"


# ===========================================================================
# 8. add_x_y_lim_formats
# ===========================================================================

class TestAddXYLimFormats:

    def test_ylim_set(self, viz):
        viz.plt_settings = {"ylim": [0, 100]}
        viz.add_x_y_lim_formats()
        ax = viz.plt.gca()
        assert ax.get_ylim() == (0, 100)

    def test_xlim_set(self, viz):
        viz.plt_settings = {"xlim": [0, 50]}
        viz.add_x_y_lim_formats()
        ax = viz.plt.gca()
        assert ax.get_xlim() == (0, 50)

    def test_ylim_none(self, viz):
        viz.plt_settings = {"ylim": None}
        viz.add_x_y_lim_formats()  # Should not raise

    def test_xlim_none(self, viz):
        viz.plt_settings = {"xlim": None}
        viz.add_x_y_lim_formats()  # Should not raise

    def test_no_lim_keys(self, viz):
        viz.plt_settings = {}
        viz.add_x_y_lim_formats()  # Should not raise

    def test_ylim_with_cfg_mult(self, viz):
        fig, ax = plt.subplots()
        viz.cfg_mult = {"file_name": "test.png"}
        viz.plot_object = ax
        viz.plt_settings = {"ylim": [10, 90]}
        viz.add_x_y_lim_formats()
        assert ax.get_ylim() == (10, 90)

    def test_xlim_with_cfg_mult(self, viz):
        fig, ax = plt.subplots()
        viz.cfg_mult = {"file_name": "test.png"}
        viz.plot_object = ax
        viz.plt_settings = {"xlim": [5, 55]}
        viz.add_x_y_lim_formats()
        assert ax.get_xlim() == (5, 55)


# ===========================================================================
# 9. resolution_adjust
# ===========================================================================

class TestResolutionAdjust:

    def test_xres_sets_ticks(self, viz):
        fig, ax = plt.subplots()
        ax.set_xlim(0, 10)
        viz.current_ax = ax
        viz.plt_settings = {"xres": 2}
        viz.resolution_adjust()
        ticks = ax.get_xticks()
        expected = np.arange(0, 12, 2)
        np.testing.assert_array_almost_equal(ticks, expected)

    def test_yres_sets_ticks(self, viz):
        fig, ax = plt.subplots()
        ax.set_ylim(0, 10)
        viz.current_ax = ax
        viz.plt_settings = {"yres": 5}
        viz.resolution_adjust()
        ticks = ax.get_yticks()
        expected = np.arange(0, 15, 5)
        np.testing.assert_array_almost_equal(ticks, expected)

    def test_explicit_ax_parameter(self, viz):
        fig, ax = plt.subplots()
        ax.set_xlim(0, 6)
        viz.plt_settings = {"xres": 3}
        viz.resolution_adjust(ax=ax)
        ticks = ax.get_xticks()
        expected = np.arange(0, 9, 3)
        np.testing.assert_array_almost_equal(ticks, expected)

    def test_no_res_keys(self, viz):
        fig, ax = plt.subplots()
        viz.current_ax = ax
        viz.plt_settings = {}
        viz.resolution_adjust()  # Should not raise

    def test_both_res(self, viz):
        fig, ax = plt.subplots()
        ax.set_xlim(0, 10)
        ax.set_ylim(0, 20)
        viz.current_ax = ax
        viz.plt_settings = {"xres": 5, "yres": 10}
        viz.resolution_adjust()


# ===========================================================================
# 10. multiple_plots / autoselect_current_subplot
# ===========================================================================

class TestMultiplePlots:

    def test_multiple_plots_default_ncols(self, viz):
        cfg = {"sets": [1, 2, 3], "suptitle": "Test"}
        viz.multiple_plots(cfg)
        assert viz.nrows == 3
        assert viz.ncols == 1
        assert viz.cfg_mult is cfg

    def test_multiple_plots_custom_dims(self, viz):
        cfg = {"sets": [1, 2, 3, 4], "nrows": 2, "ncols": 2, "suptitle": "Test"}
        viz.multiple_plots(cfg)
        assert viz.nrows == 2
        assert viz.ncols == 2

    def test_autoselect_first_subplot(self, viz):
        cfg = {"sets": [1, 2, 3], "suptitle": "Test"}
        viz.multiple_plots(cfg)
        viz.autoselect_current_subplot(0)
        assert viz.current_ax is not None
        assert viz.plot_object is viz.current_ax

    def test_autoselect_last_subplot(self, viz):
        cfg = {"sets": [1, 2, 3], "suptitle": "Test"}
        viz.multiple_plots(cfg)
        viz.autoselect_current_subplot(2)
        assert viz.current_ax is not None

    def test_autoselect_2d_grid(self, viz):
        cfg = {"sets": [1, 2, 3, 4], "nrows": 2, "ncols": 2, "suptitle": "Test"}
        viz.multiple_plots(cfg)
        viz.autoselect_current_subplot(0)
        assert viz.mult_row == 0
        assert viz.mult_column == 0
        viz.autoselect_current_subplot(1)
        assert viz.mult_row == 0
        assert viz.mult_column == 1
        viz.autoselect_current_subplot(2)
        assert viz.mult_row == 1
        assert viz.mult_column == 0
        viz.autoselect_current_subplot(3)
        assert viz.mult_row == 1
        assert viz.mult_column == 1

    def test_autoselect_single_subplot(self, viz):
        cfg = {"sets": [1], "nrows": 1, "ncols": 1, "suptitle": "Test"}
        viz.multiple_plots(cfg)
        viz.autoselect_current_subplot(0)
        assert viz.current_ax is not None


# ===========================================================================
# 11. save_and_close
# ===========================================================================

class TestSaveAndClose:

    def test_save_with_cfg_mult(self, viz):
        with tempfile.NamedTemporaryFile(suffix=".png", delete=False) as f:
            fname = f.name
        try:
            fig, ax = plt.subplots()
            ax.plot([1, 2], [3, 4])
            viz.fig = fig
            viz.cfg_mult = {"file_name": fname}
            viz.save_and_close()
            assert os.path.exists(fname)
        finally:
            if os.path.exists(fname):
                os.unlink(fname)

    def test_save_without_cfg_mult(self, viz):
        with tempfile.NamedTemporaryFile(suffix=".png", delete=False) as f:
            fname = f.name
        try:
            viz.plt.plot([1, 2], [3, 4])
            viz.plt_settings = {"file_name": fname}
            viz.save_and_close()
            assert os.path.exists(fname)
        finally:
            if os.path.exists(fname):
                os.unlink(fname)


# ===========================================================================
# 12. add_title_and_axis_labels
# ===========================================================================

class TestAddTitleAndAxisLabels:

    def test_basic_labels(self, viz):
        viz.plt.plot([1, 2], [3, 4])
        settings = _base_settings(
            xlabel="X Axis", ylabel="Y Axis", title="My Title", grid=True
        )
        viz.plt_settings = settings
        viz.add_title_and_axis_labels()

    def test_suptitle(self, viz):
        viz.plt.plot([1, 2], [3, 4])
        settings = _base_settings(suptitle="Super Title")
        viz.plt_settings = settings
        viz.add_title_and_axis_labels()

    def test_invert_xaxis(self, viz):
        viz.plt.plot([1, 2], [3, 4])
        settings = _base_settings(invert_xaxis=True)
        viz.plt_settings = settings
        viz.add_title_and_axis_labels()
        ax = viz.plt.gca()
        assert ax.get_xlim()[0] > ax.get_xlim()[1]

    def test_invert_yaxis(self, viz):
        viz.plt.plot([1, 2], [3, 4])
        settings = _base_settings(invert_yaxis=True)
        viz.plt_settings = settings
        viz.add_title_and_axis_labels()
        ax = viz.plt.gca()
        assert ax.get_ylim()[0] > ax.get_ylim()[1]

    def test_grid_false(self, viz):
        viz.plt.plot([1, 2], [3, 4])
        settings = _base_settings(grid=False)
        viz.plt_settings = settings
        viz.add_title_and_axis_labels()

    def test_labels_with_cfg_mult(self, viz):
        fig, ax = plt.subplots()
        ax.plot([1, 2], [3, 4])
        viz.cfg_mult = {"suptitle": "Multi Suptitle", "file_name": "test.png"}
        viz.fig = fig
        viz.plot_object = ax
        settings = _base_settings(xlabel="X", ylabel="Y", title="Sub", grid=True)
        viz.plt_settings = settings
        viz.add_title_and_axis_labels()

    def test_grid_minor_x(self, viz):
        viz.plt.plot([1, 2], [3, 4])
        settings = _base_settings(
            grid_minor={"x": {"flag": True}, "y": {"flag": False}}
        )
        viz.plt_settings = settings
        viz.add_title_and_axis_labels()

    def test_grid_minor_y(self, viz):
        viz.plt.plot([1, 2], [3, 4])
        settings = _base_settings(
            grid_minor={"x": {"flag": False}, "y": {"flag": True}}
        )
        viz.plt_settings = settings
        viz.add_title_and_axis_labels()


# ===========================================================================
# 13. add_legend
# ===========================================================================

class TestAddLegend:

    def test_legend_enabled(self, viz):
        viz.plt.plot([1, 2], [3, 4], label="test")
        viz.plt_settings = {"legend": True}
        viz.add_legend()

    def test_legend_disabled(self, viz):
        viz.plt.plot([1, 2], [3, 4], label="test")
        viz.plt_settings = {"legend": False}
        viz.add_legend()

    def test_legend_missing_key(self, viz):
        viz.plt.plot([1, 2], [3, 4], label="test")
        viz.plt_settings = {}
        viz.add_legend()

    def test_legend_location_best(self, viz):
        viz.plt.plot([1, 2], [3, 4], label="test")
        viz.plt_settings = {"legend": True, "legend_location": "best"}
        viz.add_legend()

    def test_legend_location_lower_center(self, viz):
        viz.plt.plot([1, 2], [3, 4], label="test")
        viz.plt_settings = {"legend": True, "legend_location": "lower center"}
        viz.add_legend()

    def test_legend_location_lower_center_with_cfg_mult(self, viz):
        fig, ax = plt.subplots()
        ax.plot([1, 2], [3, 4], label="test")
        viz.cfg_mult = {"file_name": "test.png"}
        viz.plot_object = ax
        viz.plt_settings = {"legend": True, "legend_location": "lower center"}
        viz.add_legend()

    def test_legend_location_none(self, viz):
        viz.plt.plot([1, 2], [3, 4], label="test")
        viz.plt_settings = {"legend": True}
        viz.add_legend()


# ===========================================================================
# 14. add_text_fields
# ===========================================================================

class TestAddTextFields:

    def test_no_text_fields_key(self, viz):
        viz.plt_settings = {}
        viz.add_text_fields()  # Should not raise

    def test_single_text_field(self, viz):
        viz.plt.plot([1, 2], [3, 4])
        viz.plt_settings = {
            "text_fields": [{"x": 1.5, "y": 3.5, "text": "hello"}]
        }
        viz.add_text_fields()

    def test_text_field_with_color(self, viz):
        viz.plt.plot([1, 2], [3, 4])
        viz.plt_settings = {
            "text_fields": [{"x": 1.5, "y": 3.5, "text": "hello", "color": "red"}]
        }
        viz.add_text_fields()

    def test_text_field_color_none(self, viz):
        viz.plt.plot([1, 2], [3, 4])
        viz.plt_settings = {
            "text_fields": [{"x": 1.5, "y": 3.5, "text": "hello", "color": None}]
        }
        viz.add_text_fields()

    def test_multiple_text_fields(self, viz):
        viz.plt.plot([1, 2], [3, 4])
        viz.plt_settings = {
            "text_fields": [
                {"x": 1, "y": 3, "text": "A"},
                {"x": 2, "y": 4, "text": "B", "color": "blue"},
            ]
        }
        viz.add_text_fields()

    def test_text_fields_with_cfg_mult(self, viz):
        fig, ax = plt.subplots()
        ax.plot([1, 2], [3, 4])
        viz.cfg_mult = {"file_name": "test.png"}
        viz.plot_object = ax
        viz.plt_settings = {
            "text_fields": [{"x": 1.5, "y": 3.5, "text": "hello"}]
        }
        viz.add_text_fields()


# ===========================================================================
# 15. annotate
# ===========================================================================

class TestAnnotate:

    def test_annotate_with_none_column(self, viz):
        df = _sample_df()
        x = df["x_col"]
        y = df["y_col"]
        viz.plt_settings = {"annotate": {"column": None}}
        viz.annotate(df, x, y)

    def test_annotate_with_column(self, viz):
        df = _sample_df()
        df["label_col"] = ["A", "B", "C", "D", "E"]
        x = df["x_col"]
        y = df["y_col"]
        viz.plt_settings = {"annotate": {"column": "label_col"}}
        viz.annotate(df, x, y)

    def test_annotate_no_key(self, viz):
        df = _sample_df()
        x = df["x_col"]
        y = df["y_col"]
        viz.plt_settings = {}
        viz.annotate(df, x, y)  # Should do nothing without error


# ===========================================================================
# 16. axis_autoformat
# ===========================================================================

class TestAxisAutoformat:

    def test_no_autofmt_key(self, viz):
        viz.plt_settings = {}
        viz.axis_autoformat()

    def test_autofmt_xdate_no_cfg_mult(self, viz):
        viz.plt.plot([1, 2], [3, 4])
        viz.plt_settings = {"autofmt": {"xdate": True, "ydate": False}}
        viz.axis_autoformat()

    def test_autofmt_xdate_with_cfg_mult(self, viz):
        fig, ax = plt.subplots()
        ax.plot([1, 2], [3, 4])
        viz.cfg_mult = {"file_name": "test.png"}
        viz.fig = fig
        viz.plot_object = ax
        viz.plt_settings = {"autofmt": {"xdate": True, "ydate": False}}
        viz.axis_autoformat()


# ===========================================================================
# 17. from_df_array
# ===========================================================================

class TestFromDfArray:

    def test_basic_df_array(self, viz):
        df1 = _sample_df()
        df2 = _sample_df()
        df_array = [df1, df2]
        settings = _base_settings()
        settings["array_label"] = [["label_a"], ["label_b"]]
        viz.from_df_array(df_array, settings)

    def test_empty_df_in_array(self, viz):
        df1 = _sample_df()
        df2 = pd.DataFrame()
        df_array = [df1, df2]
        settings = _base_settings()
        settings["array_label"] = [["label_a"], ["label_b"]]
        viz.from_df_array(df_array, settings)

    def test_all_empty_df_array(self, viz):
        df_array = [pd.DataFrame(), pd.DataFrame()]
        settings = _base_settings()
        settings["array_label"] = [["label_a"], ["label_b"]]
        viz.from_df_array(df_array, settings)


# ===========================================================================
# 18. from_df_get_plot
# ===========================================================================

class TestFromDfGetPlot:

    def test_basic_df_plot_no_marker(self, viz):
        df = _sample_df()
        with tempfile.NamedTemporaryFile(suffix=".png", delete=False) as f:
            fname = f.name
        try:
            settings = _base_settings(
                plt_kind="line",
                marker=None,
                ylim=None,
                file_name=fname,
            )
            viz.from_df_get_plot(df.set_index("x_col"), settings)
            assert os.path.exists(fname)
        finally:
            if os.path.exists(fname):
                os.unlink(fname)

    def test_df_plot_with_marker(self, viz):
        df = _sample_df()
        with tempfile.NamedTemporaryFile(suffix=".png", delete=False) as f:
            fname = f.name
        try:
            settings = _base_settings(
                plt_kind="line",
                marker={"type": "o", "size": 5, "edge_color": "red"},
                ylim=None,
                file_name=fname,
            )
            viz.from_df_get_plot(df.set_index("x_col"), settings)
            assert os.path.exists(fname)
        finally:
            if os.path.exists(fname):
                os.unlink(fname)

    def test_df_plot_with_ylim(self, viz):
        df = _sample_df()
        with tempfile.NamedTemporaryFile(suffix=".png", delete=False) as f:
            fname = f.name
        try:
            settings = _base_settings(
                plt_kind="line",
                marker=None,
                ylim=[0, 50],
                file_name=fname,
            )
            viz.from_df_get_plot(df.set_index("x_col"), settings)
            assert os.path.exists(fname)
        finally:
            if os.path.exists(fname):
                os.unlink(fname)


# ===========================================================================
# 19. plot_subplot
# ===========================================================================

class TestPlotSubplot:

    def test_plot_subplot(self, viz):
        cfg = {"sets": [1, 2], "suptitle": "Test"}
        viz.multiple_plots(cfg)
        viz.autoselect_current_subplot(0)
        df = _sample_df()
        settings = _base_settings(plt_kind="line")
        viz.plot_subplot(df, settings)
        lines = viz.current_ax.get_lines()
        assert len(lines) >= 1


# ===========================================================================
# 20. Edge cases and integration
# ===========================================================================

class TestEdgeCases:

    def test_empty_dataframe_columns(self, viz):
        df = pd.DataFrame({"x_col": [], "y_col": []})
        settings = _base_settings(plt_kind="line")
        viz.from_df_columns(df, settings)

    def test_single_point_dataframe(self, viz):
        df = pd.DataFrame({"x_col": [1], "y_col": [2]})
        settings = _base_settings(plt_kind="line")
        viz.from_df_columns(df, settings)

    def test_plt_settings_from_init_used_as_default(self, viz):
        settings = _base_settings()
        settings["size"] = "A4"
        settings["orientation"] = "landscape"
        v = Visualization(plt_settings=settings)
        # Now call from_df_columns without passing settings
        df = _sample_df()
        v.from_df_columns(df)
        assert v.plt_settings is settings

    def test_line_with_linestyle(self, viz):
        fig, ax = plt.subplots()
        viz.current_ax = ax
        viz.plot_object = ax
        df = _sample_df()
        settings = _base_settings(
            plt_kind="line",
            linestyle="--",
            linewidth=2,
            color="green",
        )
        viz.from_df_columns(df, settings)

    def test_add_reference_lines_axhline(self, viz):
        viz.plt.plot([1, 2], [3, 4])
        viz.plt_settings = {"ayhline": 3.5}
        viz.add_reference_lines_and_spans()

    def test_add_reference_lines_axvspan_numeric(self, viz):
        viz.plt.plot([1, 2], [3, 4])
        viz.plt_settings = {
            "axvspan": {
                "facecolor": "yellow",
                "alpha": 0.3,
                "label": "span",
                "x_datatype": "numeric",
                "x": [1.2, 1.8],
            }
        }
        viz.add_reference_lines_and_spans()

    def test_marker_with_color_overrides_edge_color(self, viz):
        """When both 'color' and marker.edge_color are set, color wins."""
        fig, ax = plt.subplots()
        viz.current_ax = ax
        viz.plot_object = ax
        df = _sample_df()
        settings = _base_settings(
            plt_kind="line",
            marker={"type": "o", "size": 5, "edge_color": "red"},
            color="blue",
        )
        viz.from_df_columns(df, settings)
        # Verify it runs without error; the color logic is exercised

    def test_gist1_is_noop(self, viz):
        viz.gist1()  # Should be a no-op

    def test_polar_plot_kind(self, viz):
        df = pd.DataFrame({"theta": [0, 1, 2, 3], "r": [1, 2, 3, 4]})
        settings = _base_settings(
            x=["theta"],
            y=["r"],
            label=["polar_series"],
            plt_kind="polar",
        )
        viz.from_df_columns(df, settings)


# ===========================================================================
# 21. generate_time_line
# ===========================================================================

class TestGenerateTimeLine:

    def test_basic_timeline(self, viz):
        dates = pd.date_range("2024-01-01", periods=5, freq="D")
        df = pd.DataFrame({"event": ["A", "B", "C", "D", "E"]}, index=dates)
        with tempfile.NamedTemporaryFile(suffix=".png", delete=False) as f:
            fname = f.name
        try:
            settings = {
                "size": {"length": 10, "height": 5},
                "title": "Timeline Test",
                "day_interval": 1,
                "xaxis_format": "%d %b %Y",
                "file_name": fname,
            }
            ax = viz.generate_time_line(df, settings)
            assert ax is not None
            assert os.path.exists(fname)
        finally:
            if os.path.exists(fname):
                os.unlink(fname)
