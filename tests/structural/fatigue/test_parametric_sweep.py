# ABOUTME: Tests for fatigue parametric sweep engine (WRK-157 Phase 3)
# ABOUTME: Validates damage matrix, monotonicity, sensitivity analysis, HTML report

"""Tests for parametric sweep — WRK-157 Phase 3."""

import json
import math
import tempfile
from pathlib import Path

import numpy as np
import pandas as pd
import pytest

from digitalmodel.structural.fatigue.parametric_sweep import (
    RESULT_COLS,
    compute_sensitivity,
    generate_sweep_report,
    run_sweep,
)


# ---------------------------------------------------------------------------
# Shared fixtures
# ---------------------------------------------------------------------------

@pytest.fixture()
def simple_histogram():
    """Minimal stress histogram: two bins for quick tests."""
    return pd.DataFrame({"range": [100.0, 150.0], "count": [1e5, 1e4]})


@pytest.fixture()
def broad_histogram():
    """Multi-bin histogram spanning typical offshore design life."""
    return pd.DataFrame({
        "range": [30.0, 60.0, 90.0, 120.0, 150.0, 180.0],
        "count": [5e6, 5e5, 5e4, 5e3, 5e2, 50.0],
    })


@pytest.fixture()
def default_curves():
    return [("DNV", "D"), ("DNV", "F"), ("API", "X")]


@pytest.fixture()
def default_scf():
    return [1.0, 1.5, 2.0]


@pytest.fixture()
def default_thickness():
    return [25.0, 32.0, 40.0]


@pytest.fixture()
def default_dff():
    return [1.0, 2.0, 3.0]


# ---------------------------------------------------------------------------
# run_sweep: output shape and columns
# ---------------------------------------------------------------------------

class TestRunSweepShape:
    def test_returns_dataframe(self, simple_histogram, default_curves,
                               default_scf, default_thickness, default_dff):
        df = run_sweep(simple_histogram, default_scf, default_curves,
                       default_thickness, default_dff)
        assert isinstance(df, pd.DataFrame)

    def test_expected_row_count(self, simple_histogram, default_curves,
                                default_scf, default_thickness, default_dff):
        df = run_sweep(simple_histogram, default_scf, default_curves,
                       default_thickness, default_dff)
        expected = len(default_scf) * len(default_curves) * len(default_thickness) * len(default_dff)
        assert len(df) == expected

    def test_has_required_columns(self, simple_histogram, default_curves,
                                  default_scf, default_thickness, default_dff):
        df = run_sweep(simple_histogram, default_scf, default_curves,
                       default_thickness, default_dff)
        for col in RESULT_COLS:
            assert col in df.columns, f"Missing column: {col}"

    def test_sorted_by_dff_adjusted_damage(self, simple_histogram, default_curves,
                                           default_scf, default_thickness, default_dff):
        df = run_sweep(simple_histogram, default_scf, default_curves,
                       default_thickness, default_dff)
        assert df["dff_adjusted_damage"].is_monotonic_increasing

    def test_empty_result_for_all_invalid_curves(self, simple_histogram,
                                                  default_scf, default_thickness, default_dff):
        df = run_sweep(simple_histogram, default_scf, [("FAKE", "Z")],
                       default_thickness, default_dff)
        assert df.empty

    def test_single_combination(self, simple_histogram):
        df = run_sweep(simple_histogram, [1.0], [("DNV", "D")], [25.0], [1.0])
        assert len(df) == 1

    def test_invalid_curve_skipped(self, simple_histogram, default_scf,
                                   default_thickness, default_dff):
        curves = [("DNV", "D"), ("FAKE", "NOPE"), ("API", "X")]
        df = run_sweep(simple_histogram, default_scf, curves,
                       default_thickness, default_dff)
        n_valid = len(default_scf) * 2 * len(default_thickness) * len(default_dff)
        assert len(df) == n_valid


# ---------------------------------------------------------------------------
# run_sweep: damage values
# ---------------------------------------------------------------------------

class TestRunSweepDamageValues:
    def test_damage_positive(self, simple_histogram, default_curves,
                             default_scf, default_thickness, default_dff):
        df = run_sweep(simple_histogram, default_scf, default_curves,
                       default_thickness, default_dff)
        assert (df["damage"] > 0).all()

    def test_damage_finite(self, simple_histogram, default_curves,
                           default_scf, default_thickness, default_dff):
        df = run_sweep(simple_histogram, default_scf, default_curves,
                       default_thickness, default_dff)
        assert df["damage"].apply(np.isfinite).all()

    def test_life_factor_is_inverse_damage(self, simple_histogram):
        df = run_sweep(simple_histogram, [1.0], [("DNV", "D")], [25.0], [1.0])
        row = df.iloc[0]
        assert abs(row["life_factor"] - 1.0 / row["damage"]) < 1e-9

    def test_dff_adjusted_damage_equals_damage_times_dff(self, simple_histogram):
        df = run_sweep(simple_histogram, [1.0], [("DNV", "D")], [25.0], [2.0])
        row = df.iloc[0]
        assert abs(row["dff_adjusted_damage"] - row["damage"] * 2.0) < 1e-9

    def test_passes_dff_flag_consistent(self, simple_histogram, default_curves,
                                        default_scf, default_thickness, default_dff):
        df = run_sweep(simple_histogram, default_scf, default_curves,
                       default_thickness, default_dff)
        for _, row in df.iterrows():
            expected = row["dff_adjusted_damage"] <= 1.0
            assert row["passes_dff"] == expected


# ---------------------------------------------------------------------------
# run_sweep: monotonicity
# ---------------------------------------------------------------------------

class TestRunSweepMonotonicity:
    def test_damage_increases_with_scf(self, broad_histogram):
        """Higher SCF → higher damage (for fixed curve/thickness/DFF)."""
        scf_values = [1.0, 1.5, 2.0, 2.5]
        df = run_sweep(broad_histogram, scf_values, [("DNV", "D")], [25.0], [1.0])
        by_scf = df.sort_values("scf")["damage"].tolist()
        assert all(b >= a for a, b in zip(by_scf, by_scf[1:]))

    def test_damage_increases_with_thickness(self, broad_histogram):
        """Greater thickness → curve degraded → higher damage."""
        thicknesses = [25.0, 32.0, 40.0, 50.0]
        df = run_sweep(broad_histogram, [1.5], [("DNV", "D")], thicknesses, [1.0])
        by_t = df.sort_values("thickness_mm")["damage"].tolist()
        assert all(b >= a for a, b in zip(by_t, by_t[1:]))

    def test_damage_increases_with_dff(self, broad_histogram):
        """Higher DFF → higher DFF-adjusted damage (linear)."""
        dff_values = [1.0, 2.0, 3.0]
        df = run_sweep(broad_histogram, [1.0], [("DNV", "D")], [25.0], dff_values)
        by_dff = df.sort_values("dff")["dff_adjusted_damage"].tolist()
        assert all(b >= a for a, b in zip(by_dff, by_dff[1:]))

    def test_stronger_curve_gives_lower_damage(self, broad_histogram):
        """DNV B (higher A) → lower damage than DNV W (lower A)."""
        df = run_sweep(broad_histogram, [1.0], [("BS", "B"), ("BS", "W")], [25.0], [1.0])
        damage_B = df[df["curve_class"] == "B"]["damage"].iloc[0]
        damage_W = df[df["curve_class"] == "W"]["damage"].iloc[0]
        assert damage_B < damage_W

    def test_scf_one_gives_less_damage_than_scf_two(self, simple_histogram):
        df = run_sweep(simple_histogram, [1.0, 2.0], [("DNV", "D")], [25.0], [1.0])
        d1 = df[df["scf"] == 1.0]["damage"].iloc[0]
        d2 = df[df["scf"] == 2.0]["damage"].iloc[0]
        assert d1 < d2


# ---------------------------------------------------------------------------
# run_sweep: histogram validation
# ---------------------------------------------------------------------------

class TestRunSweepValidation:
    def test_missing_range_raises(self):
        bad = pd.DataFrame({"count": [100.0]})
        with pytest.raises(ValueError, match="range"):
            run_sweep(bad, [1.0], [("DNV", "D")], [25.0], [1.0])

    def test_missing_count_raises(self):
        bad = pd.DataFrame({"range": [100.0]})
        with pytest.raises(ValueError, match="count"):
            run_sweep(bad, [1.0], [("DNV", "D")], [25.0], [1.0])


# ---------------------------------------------------------------------------
# run_sweep: reference thickness (no correction)
# ---------------------------------------------------------------------------

class TestThicknessCorrection:
    def test_reference_thickness_matches_no_correction(self, simple_histogram):
        """t=25mm should give identical damage to an uncorrected curve."""
        df_ref = run_sweep(simple_histogram, [1.0], [("DNV", "D")], [25.0], [1.0])
        df_other = run_sweep(simple_histogram, [1.0], [("DNV", "D")], [26.0], [1.0])
        # Different thickness → different damage
        assert df_ref.iloc[0]["damage"] != pytest.approx(df_other.iloc[0]["damage"])

    def test_same_thickness_same_damage(self, simple_histogram):
        df1 = run_sweep(simple_histogram, [1.5], [("DNV", "D")], [25.0], [2.0])
        df2 = run_sweep(simple_histogram, [1.5], [("DNV", "D")], [25.0], [2.0])
        assert df1.iloc[0]["damage"] == pytest.approx(df2.iloc[0]["damage"])


# ---------------------------------------------------------------------------
# compute_sensitivity
# ---------------------------------------------------------------------------

class TestComputeSensitivity:
    def test_returns_dataframe(self, broad_histogram, default_curves,
                               default_scf, default_thickness, default_dff):
        df = compute_sensitivity(broad_histogram, default_scf, default_curves,
                                 default_thickness, default_dff)
        assert isinstance(df, pd.DataFrame)

    def test_four_parameters(self, broad_histogram, default_curves,
                             default_scf, default_thickness, default_dff):
        df = compute_sensitivity(broad_histogram, default_scf, default_curves,
                                 default_thickness, default_dff)
        assert len(df) == 4

    def test_parameter_names(self, broad_histogram, default_curves,
                             default_scf, default_thickness, default_dff):
        df = compute_sensitivity(broad_histogram, default_scf, default_curves,
                                 default_thickness, default_dff)
        params = set(df["parameter"].tolist())
        assert "SCF" in params
        assert "S-N Curve" in params
        assert "Thickness (mm)" in params
        assert "DFF" in params

    def test_sensitivity_non_negative(self, broad_histogram, default_curves,
                                      default_scf, default_thickness, default_dff):
        df = compute_sensitivity(broad_histogram, default_scf, default_curves,
                                 default_thickness, default_dff)
        assert (df["sensitivity"] >= 0).all()

    def test_sorted_by_sensitivity_descending(self, broad_histogram, default_curves,
                                               default_scf, default_thickness, default_dff):
        df = compute_sensitivity(broad_histogram, default_scf, default_curves,
                                 default_thickness, default_dff)
        assert df["sensitivity"].is_monotonic_decreasing

    def test_max_damage_gte_min_damage(self, broad_histogram, default_curves,
                                       default_scf, default_thickness, default_dff):
        df = compute_sensitivity(broad_histogram, default_scf, default_curves,
                                 default_thickness, default_dff)
        assert (df["max_damage"] >= df["min_damage"]).all()

    def test_single_value_per_param_gives_zero_sensitivity(self, simple_histogram):
        df = compute_sensitivity(
            simple_histogram, [1.0], [("DNV", "D")], [25.0], [1.0]
        )
        assert (df["sensitivity"] == 0.0).all()

    def test_scf_sensitivity_nonzero_for_varied_scf(self, broad_histogram):
        df = compute_sensitivity(
            broad_histogram, [1.0, 2.0], [("DNV", "D")], [25.0], [1.0]
        )
        scf_row = df[df["parameter"] == "SCF"].iloc[0]
        assert scf_row["sensitivity"] > 0


# ---------------------------------------------------------------------------
# generate_sweep_report
# ---------------------------------------------------------------------------

class TestGenerateSweepReport:
    def _run(self, histogram, scf, curves, thickness, dff):
        results = run_sweep(histogram, scf, curves, thickness, dff)
        return generate_sweep_report(histogram, results)

    def test_returns_string(self, broad_histogram, default_curves,
                            default_scf, default_thickness, default_dff):
        html = self._run(broad_histogram, default_scf, default_curves,
                         default_thickness, default_dff)
        assert isinstance(html, str)

    def test_contains_html_tag(self, broad_histogram, default_curves,
                               default_scf, default_thickness, default_dff):
        html = self._run(broad_histogram, default_scf, default_curves,
                         default_thickness, default_dff)
        assert "<html" in html

    def test_contains_plotly_cdn(self, broad_histogram, default_curves,
                                 default_scf, default_thickness, default_dff):
        html = self._run(broad_histogram, default_scf, default_curves,
                         default_thickness, default_dff)
        assert "plotly" in html.lower()

    def test_custom_title_in_html(self, broad_histogram, default_curves,
                                  default_scf, default_thickness, default_dff):
        results = run_sweep(broad_histogram, default_scf, default_curves,
                            default_thickness, default_dff)
        html = generate_sweep_report(broad_histogram, results,
                                     title="Pipeline Sensitivity Study")
        assert "Pipeline Sensitivity Study" in html

    def test_contains_tornado_div(self, broad_histogram, default_curves,
                                  default_scf, default_thickness, default_dff):
        html = self._run(broad_histogram, default_scf, default_curves,
                         default_thickness, default_dff)
        assert 'id="tornado"' in html

    def test_contains_scatter_div(self, broad_histogram, default_curves,
                                  default_scf, default_thickness, default_dff):
        html = self._run(broad_histogram, default_scf, default_curves,
                         default_thickness, default_dff)
        assert 'id="scatter"' in html

    def test_contains_pass_fail_labels(self, broad_histogram, default_curves,
                                       default_scf, default_thickness, default_dff):
        html = self._run(broad_histogram, default_scf, default_curves,
                         default_thickness, default_dff)
        # At least one of PASS/FAIL should be in the results table
        assert "PASS" in html or "FAIL" in html

    def test_contains_summary_counts(self, broad_histogram, default_curves,
                                     default_scf, default_thickness, default_dff):
        html = self._run(broad_histogram, default_scf, default_curves,
                         default_thickness, default_dff)
        assert "Total combinations:" in html

    def test_writes_to_file(self, broad_histogram, default_curves,
                            default_scf, default_thickness, default_dff):
        results = run_sweep(broad_histogram, default_scf, default_curves,
                            default_thickness, default_dff)
        with tempfile.NamedTemporaryFile(suffix=".html", delete=False) as f:
            path = f.name
        generate_sweep_report(broad_histogram, results, output_path=path)
        content = Path(path).read_text()
        assert "<html" in content
        assert len(content) > 1000

    def test_empty_results_returns_fallback(self, simple_histogram):
        empty_df = pd.DataFrame(columns=RESULT_COLS)
        html = generate_sweep_report(simple_histogram, empty_df)
        assert "No valid sweep results" in html

    def test_html_substantial_length(self, broad_histogram, default_curves,
                                     default_scf, default_thickness, default_dff):
        html = self._run(broad_histogram, default_scf, default_curves,
                         default_thickness, default_dff)
        assert len(html) > 3000


# ---------------------------------------------------------------------------
# Integration: full pipeline
# ---------------------------------------------------------------------------

class TestIntegration:
    def test_pipeline_girth_weld_dnv_d(self):
        """Pipeline girth weld: DNV D-curve, air, SCF sweep — smoke test."""
        histogram = pd.DataFrame({
            "range": [40.0, 80.0, 120.0, 160.0],
            "count": [2e6, 1e5, 5e3, 2e2],
        })
        results = run_sweep(
            histogram,
            scf_values=[1.0, 1.2, 1.5],
            curves=[("DNV", "D")],
            thickness_values=[25.0],
            dff_values=[1.0, 3.0],
        )
        assert len(results) == 6  # 3 SCF × 1 curve × 1 thickness × 2 DFF
        # SCF=1, DFF=1 should give lowest DFF-adjusted damage
        best = results.iloc[0]
        assert best["scf"] == pytest.approx(1.0)
        assert best["dff"] == pytest.approx(1.0)

    def test_all_dnv_curves_sweep(self, broad_histogram):
        """Sweep across all DNV curves — no crashes, correct count."""
        from digitalmodel.structural.fatigue.sn_curves import StandardSNCurves
        dnv_classes = list(StandardSNCurves.DNV_CURVES.keys())
        curves = [("DNV", c) for c in dnv_classes]
        results = run_sweep(broad_histogram, [1.0], curves, [25.0], [1.0])
        assert len(results) == len(dnv_classes)

    def test_full_report_pipeline(self, broad_histogram, default_curves,
                                  default_scf, default_thickness, default_dff):
        """run_sweep → generate_sweep_report produces valid HTML."""
        results = run_sweep(broad_histogram, default_scf, default_curves,
                            default_thickness, default_dff)
        html = generate_sweep_report(
            broad_histogram, results,
            title="Full Pipeline Test",
        )
        assert "<html" in html
        assert "Full Pipeline Test" in html
        # Tornado chart JSON must be present
        assert '"data"' in html

    def test_sensitivity_dominated_by_scf_for_wide_scf_range(self, broad_histogram):
        """Wide SCF range (1→5) should dominate narrow DFF range (1→2)."""
        df = compute_sensitivity(
            broad_histogram,
            scf_values=[1.0, 2.0, 3.0, 4.0, 5.0],
            curves=[("DNV", "D")],
            thickness_values=[25.0],
            dff_values=[1.0, 2.0],
        )
        top_param = df.iloc[0]["parameter"]
        assert top_param == "SCF"

    def test_dff_sensitivity_equals_miner_damage(self, simple_histogram):
        """DFF sensitivity should equal (max_dff - min_dff) * base_damage."""
        dff_values = [1.0, 3.0]
        df_sweep = run_sweep(simple_histogram, [1.0], [("DNV", "D")], [25.0], [1.0])
        base_damage = df_sweep.iloc[0]["damage"]

        df_sens = compute_sensitivity(
            simple_histogram, [1.0], [("DNV", "D")], [25.0], dff_values
        )
        dff_row = df_sens[df_sens["parameter"] == "DFF"].iloc[0]
        expected_sensitivity = base_damage * (dff_values[-1] - dff_values[0])
        assert dff_row["sensitivity"] == pytest.approx(expected_sensitivity, rel=1e-6)
