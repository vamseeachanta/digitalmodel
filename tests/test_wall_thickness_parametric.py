# ABOUTME: Tests for parametric wall thickness sweep and Plotly visualisation
# ABOUTME: Validates DataFrame shape, no NaN values, chart generation, HTML report

"""Tests for wall_thickness_parametric module."""

import tempfile

import pytest
import pandas as pd

from digitalmodel.structural.analysis.wall_thickness import (
    DesignCode,
    FabricationType,
    SafetyClass,
)
from digitalmodel.structural.analysis.wall_thickness_parametric import (
    API_5L_GRADES,
    ParametricSweep,
    SweepConfig,
    generate_report,
    plot_governing_check,
    plot_utilisation_heatmap,
    plot_utilisation_vs_wall_thickness,
)


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def make_basic_sweep_config(**overrides):
    defaults = dict(
        wall_thicknesses=[0.010, 0.015, 0.020, 0.025, 0.030],
        outer_diameters=[0.27305],
        grades=["X65"],
        internal_pressures=[20e6],
        external_pressures=[5e6],
    )
    defaults.update(overrides)
    return SweepConfig(**defaults)


def make_multi_od_config():
    return SweepConfig(
        wall_thicknesses=[0.010, 0.015, 0.020, 0.025],
        outer_diameters=[0.2191, 0.27305, 0.32385],
        grades=["X65"],
        internal_pressures=[20e6],
        external_pressures=[5e6],
    )


# ===================================================================
# ParametricSweep
# ===================================================================

class TestParametricSweep:
    def test_sweep_returns_dataframe(self):
        cfg = make_basic_sweep_config()
        sweep = ParametricSweep(cfg)
        df = sweep.run()
        assert isinstance(df, pd.DataFrame)
        assert len(df) > 0

    def test_sweep_correct_row_count(self):
        cfg = make_basic_sweep_config()
        sweep = ParametricSweep(cfg)
        df = sweep.run()
        # 5 WT × 1 OD × 1 grade × 1 Pi × 1 Pe = 5 combos
        assert len(df) == 5

    def test_sweep_no_nan_utilisation(self):
        cfg = make_basic_sweep_config()
        sweep = ParametricSweep(cfg)
        df = sweep.run()
        util_cols = [c for c in df.columns if c.startswith("util_")]
        assert len(util_cols) > 0
        for col in util_cols:
            assert df[col].isna().sum() == 0, f"NaN found in {col}"

    def test_sweep_has_max_utilisation_column(self):
        cfg = make_basic_sweep_config()
        sweep = ParametricSweep(cfg)
        df = sweep.run()
        assert "max_utilisation" in df.columns
        assert (df["max_utilisation"] >= 0).all()

    def test_sweep_has_governing_check_column(self):
        cfg = make_basic_sweep_config()
        sweep = ParametricSweep(cfg)
        df = sweep.run()
        assert "governing_check" in df.columns
        assert df["governing_check"].notna().all()

    def test_sweep_thicker_pipe_lower_burst_utilisation(self):
        cfg = make_basic_sweep_config()
        sweep = ParametricSweep(cfg)
        df = sweep.run()
        # Burst utilisation should decrease with increasing WT
        burst_col = "util_pressure_containment"
        if burst_col in df.columns:
            sorted_df = df.sort_values("wall_thickness_m")
            utils = sorted_df[burst_col].values
            # Each subsequent WT should have lower or equal burst utilisation
            for i in range(1, len(utils)):
                assert utils[i] <= utils[i - 1] + 1e-6

    def test_sweep_multi_od_correct_count(self):
        cfg = make_multi_od_config()
        sweep = ParametricSweep(cfg)
        df = sweep.run()
        # 4 WT × 3 OD × 1 grade × 1 Pi × 1 Pe = 12
        assert len(df) == 12

    def test_sweep_skips_invalid_geometry(self):
        cfg = SweepConfig(
            wall_thicknesses=[0.001, 0.200],  # 0.200 >= OD/2 for 0.27305
            outer_diameters=[0.27305],
            grades=["X65"],
            internal_pressures=[20e6],
        )
        sweep = ParametricSweep(cfg)
        df = sweep.run()
        # Only the valid WT should appear
        assert len(df) == 1

    def test_sweep_multi_grade(self):
        cfg = SweepConfig(
            wall_thicknesses=[0.020],
            outer_diameters=[0.27305],
            grades=["X52", "X65", "X80"],
            internal_pressures=[20e6],
        )
        sweep = ParametricSweep(cfg)
        df = sweep.run()
        assert len(df) == 3
        assert set(df["grade"]) == {"X52", "X65", "X80"}

    def test_sweep_api_code(self):
        cfg = SweepConfig(
            wall_thicknesses=[0.015, 0.020, 0.025],
            outer_diameters=[0.27305],
            grades=["X65"],
            internal_pressures=[20e6],
            external_pressures=[5e6],
            code=DesignCode.API_RP_1111,
        )
        sweep = ParametricSweep(cfg)
        df = sweep.run()
        assert len(df) == 3
        assert "util_burst" in df.columns
        assert "util_collapse" in df.columns
        assert "util_propagation" in df.columns

    def test_sweep_d_over_t_column(self):
        cfg = make_basic_sweep_config()
        sweep = ParametricSweep(cfg)
        df = sweep.run()
        assert "d_over_t" in df.columns
        # D/t should decrease with increasing WT
        sorted_df = df.sort_values("wall_thickness_m")
        d_over_t = sorted_df["d_over_t"].values
        for i in range(1, len(d_over_t)):
            assert d_over_t[i] < d_over_t[i - 1]


# ===================================================================
# Plotly charts
# ===================================================================

class TestPlotlyCharts:
    @pytest.fixture
    def sweep_df(self):
        cfg = make_basic_sweep_config()
        return ParametricSweep(cfg).run()

    @pytest.fixture
    def multi_od_df(self):
        return ParametricSweep(make_multi_od_config()).run()

    def test_plot_utilisation_vs_wt_returns_figure(self, sweep_df):
        fig = plot_utilisation_vs_wall_thickness(sweep_df)
        assert fig is not None
        assert len(fig.data) > 0

    def test_plot_utilisation_vs_wt_has_traces_per_check(self, sweep_df):
        fig = plot_utilisation_vs_wall_thickness(sweep_df)
        util_cols = [c for c in sweep_df.columns if c.startswith("util_")]
        assert len(fig.data) == len(util_cols)

    def test_plot_heatmap_returns_figure(self, multi_od_df):
        fig = plot_utilisation_heatmap(multi_od_df)
        assert fig is not None
        assert len(fig.data) > 0

    def test_plot_heatmap_empty_df_returns_empty_figure(self):
        fig = plot_utilisation_heatmap(pd.DataFrame())
        assert fig is not None
        assert len(fig.data) == 0

    def test_plot_governing_check_returns_figure(self, sweep_df):
        fig = plot_governing_check(sweep_df)
        assert fig is not None
        assert len(fig.data) > 0

    def test_plot_governing_check_empty_df_returns_empty_figure(self):
        fig = plot_governing_check(pd.DataFrame())
        assert fig is not None
        assert len(fig.data) == 0


# ===================================================================
# HTML report
# ===================================================================

class TestHTMLReport:
    def test_generate_report_returns_html_string(self):
        cfg = make_basic_sweep_config()
        df = ParametricSweep(cfg).run()
        html = generate_report(df)
        assert isinstance(html, str)
        assert "<html>" in html
        assert "Wall Thickness" in html

    def test_generate_report_writes_to_file(self):
        cfg = make_basic_sweep_config()
        df = ParametricSweep(cfg).run()

        with tempfile.NamedTemporaryFile(suffix=".html", delete=False) as f:
            path = f.name

        generate_report(df, output_path=path)

        with open(path) as f:
            content = f.read()
        assert "<html>" in content
        assert len(content) > 1000  # Should have substantial content

    def test_generate_report_contains_summary_table(self):
        cfg = make_basic_sweep_config()
        df = ParametricSweep(cfg).run()
        html = generate_report(df)
        assert "max_utilisation" in html
        assert "governing_check" in html
