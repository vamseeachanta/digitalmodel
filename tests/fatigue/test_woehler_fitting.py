"""Tests for Woehler curve fitting from fatigue test data."""

import numpy as np
import pandas as pd
import pytest

from digitalmodel.fatigue.woehler_fitting import (
    design_curve,
    fit_woehler_curve,
    generate_test_data,
)


class TestGenerateTestData:
    """Verify synthetic test data generation."""

    def test_returns_correct_columns(self):
        df = generate_test_data(k_1=3.0, log_a=11.855)
        assert set(df.columns) == {"stress_range", "cycles", "runout"}

    def test_returns_requested_sample_count(self):
        df = generate_test_data(k_1=3.0, log_a=11.855, n_samples=15)
        assert len(df) == 15

    def test_includes_runouts(self):
        df = generate_test_data(k_1=3.0, log_a=11.855, n_samples=20)
        assert df["runout"].any(), "Should include at least one runout"

    def test_includes_fractures(self):
        df = generate_test_data(k_1=3.0, log_a=11.855, n_samples=20)
        assert (~df["runout"]).any(), "Should include at least one fracture"

    def test_reproducible_with_seed(self):
        df1 = generate_test_data(k_1=3.0, log_a=11.855, seed=123)
        df2 = generate_test_data(k_1=3.0, log_a=11.855, seed=123)
        pd.testing.assert_frame_equal(df1, df2)

    def test_endurance_stress_controls_runouts(self):
        df = generate_test_data(k_1=3.0, log_a=11.855, endurance_stress=100)
        runout_stresses = df.loc[df["runout"], "stress_range"]
        assert (runout_stresses <= 100).all()


class TestFitWoehlerCurve:
    """Test curve fitting with synthetic data."""

    @pytest.fixture()
    def synthetic_data(self):
        """Generate data with known DNV Cat F parameters."""
        return generate_test_data(
            k_1=3.0, log_a=11.855, n_samples=30, scatter_log=0.10, seed=42
        )

    def test_elementary_returns_required_keys(self, synthetic_data):
        result = fit_woehler_curve(synthetic_data, method="elementary")
        for key in ("k_1", "SD", "ND", "TN"):
            assert key in result.index, f"Missing key: {key}"

    def test_elementary_slope_reasonable(self, synthetic_data):
        result = fit_woehler_curve(synthetic_data, method="elementary")
        # Slope should be in the ballpark of 3.0 (true value)
        assert 1.5 < result["k_1"] < 6.0, f"Slope {result['k_1']} out of range"

    def test_elementary_sd_positive(self, synthetic_data):
        result = fit_woehler_curve(synthetic_data, method="elementary")
        assert result["SD"] > 0, "SD must be positive"

    def test_maxlike_returns_required_keys(self, synthetic_data):
        result = fit_woehler_curve(synthetic_data, method="maxlike")
        for key in ("k_1", "SD", "ND", "TN"):
            assert key in result.index, f"Missing key: {key}"

    def test_maxlike_with_fixed_slope(self, synthetic_data):
        result = fit_woehler_curve(
            synthetic_data, method="maxlike", fixed_params={"k_1": 3.0}
        )
        assert abs(result["k_1"] - 3.0) < 0.01, "Fixed k_1 should be 3.0"

    def test_invalid_method_raises(self, synthetic_data):
        with pytest.raises(ValueError, match="method must be"):
            fit_woehler_curve(synthetic_data, method="bogus")

    def test_load_column_accepted(self):
        """Verify 'load' is accepted as alternative to 'stress_range'."""
        df = pd.DataFrame({
            "load": [200, 200, 150, 150, 100, 100, 80, 80],
            "cycles": [1e4, 1.2e4, 5e4, 6e4, 3e5, 4e5, 2e6, 3e6],
            "fracture": [True, True, True, True, True, True, False, False],
        })
        result = fit_woehler_curve(df, method="elementary")
        assert "k_1" in result.index


class TestInputValidation:
    """Test data validation and error handling."""

    def test_empty_dataframe_raises(self):
        with pytest.raises(ValueError, match="non-empty"):
            fit_woehler_curve(pd.DataFrame())

    def test_missing_stress_column_raises(self):
        df = pd.DataFrame({"cycles": [1e5], "runout": [False]})
        with pytest.raises(ValueError, match="stress_range.*load"):
            fit_woehler_curve(df)

    def test_missing_cycles_column_raises(self):
        df = pd.DataFrame({"stress_range": [100], "runout": [False]})
        with pytest.raises(ValueError, match="cycles"):
            fit_woehler_curve(df)

    def test_too_few_fractures_raises(self):
        df = pd.DataFrame({
            "stress_range": [100],
            "cycles": [1e5],
            "runout": [False],
        })
        with pytest.raises(ValueError, match="at least 2"):
            fit_woehler_curve(df)


class TestDesignCurve:
    """Test design curve probability shift."""

    @pytest.fixture()
    def mean_curve(self):
        return pd.Series({
            "k_1": 3.0,
            "SD": 52.63,
            "ND": 1e7,
            "TN": 3.0,
            "TS": 1.44,
            "failure_probability": 0.5,
        })

    def test_design_sd_lower_than_mean(self, mean_curve):
        dc = design_curve(mean_curve, failure_probability=0.023)
        assert dc["SD"] < mean_curve["SD"], "Design SD must be lower than mean"

    def test_design_nd_lower_than_mean(self, mean_curve):
        dc = design_curve(mean_curve, failure_probability=0.023)
        assert dc["ND"] < mean_curve["ND"], "Design ND must be lower than mean"

    def test_failure_probability_stored(self, mean_curve):
        dc = design_curve(mean_curve, failure_probability=0.05)
        assert dc["failure_probability"] == 0.05

    def test_lower_probability_gives_lower_sd(self, mean_curve):
        dc_5pct = design_curve(mean_curve, failure_probability=0.05)
        dc_2pct = design_curve(mean_curve, failure_probability=0.023)
        assert dc_2pct["SD"] < dc_5pct["SD"]

    def test_invalid_probability_raises(self, mean_curve):
        with pytest.raises(ValueError, match="between 0 and 1"):
            design_curve(mean_curve, failure_probability=0.0)
        with pytest.raises(ValueError, match="between 0 and 1"):
            design_curve(mean_curve, failure_probability=1.0)

    def test_50pct_returns_near_original(self, mean_curve):
        dc = design_curve(mean_curve, failure_probability=0.5)
        assert abs(dc["SD"] - mean_curve["SD"]) / mean_curve["SD"] < 0.01
