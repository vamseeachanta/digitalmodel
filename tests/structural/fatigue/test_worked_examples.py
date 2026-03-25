# ABOUTME: Tests for fatigue worked examples — WRK-157 Phase 4
# ABOUTME: Three canonical offshore cases: pipeline, SCR, mooring chain

"""Tests for worked_examples — WRK-157 Phase 4."""

import math
import tempfile
from pathlib import Path

import numpy as np
import pandas as pd
import pytest

from digitalmodel.structural.fatigue.worked_examples import (
    ExampleResult,
    generate_example_report,
    mooring_chain,
    pipeline_girth_weld,
    scr_touchdown,
    _weibull_histogram,
    _dnv_seawater_cp,
    _dnv_seawater_free,
    _chain_seawater_cp,
)


# ---------------------------------------------------------------------------
# Synthetic histogram helper
# ---------------------------------------------------------------------------

class TestWeibullHistogram:
    def test_returns_dataframe(self):
        df = _weibull_histogram(20, 3_150_000, 80.0, 0.9)
        assert isinstance(df, pd.DataFrame)

    def test_has_range_and_count_columns(self):
        df = _weibull_histogram(20, 3_150_000, 80.0, 0.9)
        assert "range" in df.columns
        assert "count" in df.columns

    def test_all_ranges_positive(self):
        df = _weibull_histogram(20, 3_150_000, 80.0, 0.9)
        assert (df["range"] > 0).all()

    def test_all_counts_positive(self):
        df = _weibull_histogram(20, 3_150_000, 80.0, 0.9)
        assert (df["count"] > 0).all()

    def test_total_cycles_approximate(self):
        """Total cycles should be close to design_life × cycles_per_year."""
        design_life = 20.0
        cpy = 3_150_000
        df = _weibull_histogram(design_life, cpy, 80.0, 0.9, n_bins=20)
        total = df["count"].sum()
        expected = design_life * cpy
        # Weibull tail is truncated at 1.5× scale; up to ~30% may be excluded
        assert total < expected * 1.05   # never more than expected
        assert total > expected * 0.60   # at least 60% captured in bins

    def test_longer_life_more_cycles(self):
        df20 = _weibull_histogram(20, 1e6, 80.0, 0.9)
        df40 = _weibull_histogram(40, 1e6, 80.0, 0.9)
        assert df40["count"].sum() > df20["count"].sum()

    def test_custom_n_bins(self):
        df = _weibull_histogram(20, 1e6, 80.0, 0.9, n_bins=5)
        assert len(df) <= 5


# ---------------------------------------------------------------------------
# Environment-adjusted curves
# ---------------------------------------------------------------------------

class TestEnvironmentCurves:
    def test_seawater_cp_lower_a_than_air(self):
        from digitalmodel.structural.fatigue.sn_curves import StandardSNCurves
        air_curve = StandardSNCurves.get_curve("DNV", "D")
        sw_curve = _dnv_seawater_cp("D")
        assert sw_curve.A < air_curve.A

    def test_seawater_cp_retains_fatigue_limit(self):
        air_curve = _dnv_seawater_cp("D")
        # CAFL retained under CP
        assert air_curve.fatigue_limit > 0

    def test_seawater_free_no_fatigue_limit(self):
        sw_free = _dnv_seawater_free("F1")
        assert sw_free.fatigue_limit == 0.0

    def test_seawater_free_lower_a_than_cp(self):
        sw_cp = _dnv_seawater_cp("D")
        sw_free = _dnv_seawater_free("D")
        assert sw_free.A < sw_cp.A

    def test_chain_curve_name(self):
        chain = _chain_seawater_cp()
        assert "Chain" in chain.name or "chain" in chain.name.lower()

    def test_chain_curve_no_fatigue_limit(self):
        chain = _chain_seawater_cp()
        assert chain.fatigue_limit == 0.0

    def test_seawater_free_gives_more_damage_than_cp(self):
        """Free corrosion should give higher damage than CP (same curve class)."""
        from digitalmodel.structural.fatigue.damage_accumulation import LinearDamageAccumulation
        hist = pd.DataFrame({"range": [60.0, 80.0, 100.0], "count": [1e5, 1e4, 1e3]})
        acc = LinearDamageAccumulation()
        d_cp = acc.calculate_damage(hist, _dnv_seawater_cp("D"))["total_damage"]
        d_free = acc.calculate_damage(hist, _dnv_seawater_free("D"))["total_damage"]
        assert d_free > d_cp


# ---------------------------------------------------------------------------
# ExampleResult structure
# ---------------------------------------------------------------------------

class TestExampleResultStructure:
    @pytest.fixture()
    def pipe_result(self):
        return pipeline_girth_weld()

    def test_is_example_result(self, pipe_result):
        assert isinstance(pipe_result, ExampleResult)

    def test_name_set(self, pipe_result):
        assert pipe_result.name == "Pipeline Girth Weld"

    def test_histogram_is_dataframe(self, pipe_result):
        assert isinstance(pipe_result.histogram, pd.DataFrame)

    def test_damage_positive(self, pipe_result):
        assert pipe_result.damage > 0

    def test_damage_finite(self, pipe_result):
        assert np.isfinite(pipe_result.damage)

    def test_life_years_positive(self, pipe_result):
        assert pipe_result.life_years > 0

    def test_life_years_finite_for_finite_damage(self, pipe_result):
        assert np.isfinite(pipe_result.life_years)

    def test_life_years_equals_design_life_over_damage(self, pipe_result):
        expected = pipe_result.design_life_years / pipe_result.damage
        assert pipe_result.life_years == pytest.approx(expected, rel=1e-6)

    def test_scf_set(self, pipe_result):
        assert pipe_result.scf == pytest.approx(1.5)

    def test_dff_set(self, pipe_result):
        assert pipe_result.dff == pytest.approx(3.0)

    def test_passes_consistent_with_damage(self, pipe_result):
        expected_pass = (pipe_result.damage * pipe_result.dff) <= 1.0
        assert pipe_result.passes == expected_pass

    def test_assumptions_nonempty(self, pipe_result):
        assert len(pipe_result.assumptions) > 0

    def test_references_nonempty(self, pipe_result):
        assert len(pipe_result.references) > 0

    def test_damage_contributions_list(self, pipe_result):
        assert isinstance(pipe_result.damage_contributions, list)


# ---------------------------------------------------------------------------
# Pipeline girth weld
# ---------------------------------------------------------------------------

class TestPipelineGirthWeld:
    def test_smoke(self):
        result = pipeline_girth_weld()
        assert result.damage > 0

    def test_higher_scf_more_damage(self):
        r1 = pipeline_girth_weld(scf=1.0)
        r2 = pipeline_girth_weld(scf=2.0)
        assert r2.damage > r1.damage

    def test_scf_damage_scales_with_slope(self):
        """For m=3, doubling SCF should increase damage by 2^3 = 8×."""
        r1 = pipeline_girth_weld(scf=1.0)
        r2 = pipeline_girth_weld(scf=2.0)
        # Approximately 8×; cycles below fatigue limit reduce exact ratio
        assert r2.damage / r1.damage > 4.0

    def test_curve_is_seawater_cp(self):
        result = pipeline_girth_weld()
        assert "SwCP" in result.sn_curve.name or "seawater" in result.sn_curve.name.lower()

    def test_longer_design_life_more_damage(self):
        r20 = pipeline_girth_weld(design_life_years=20)
        r40 = pipeline_girth_weld(design_life_years=40)
        assert r40.damage == pytest.approx(2 * r20.damage, rel=1e-3)

    def test_life_longer_than_design_life_for_low_scf(self):
        result = pipeline_girth_weld(scf=0.5, dff=1.0)
        assert result.life_years > result.design_life_years


# ---------------------------------------------------------------------------
# SCR touchdown
# ---------------------------------------------------------------------------

class TestSCRTouchdown:
    def test_smoke(self):
        result = scr_touchdown()
        assert result.damage > 0

    def test_name_set(self):
        result = scr_touchdown()
        assert result.name == "SCR Touchdown Zone"

    def test_default_dff_is_10(self):
        result = scr_touchdown()
        assert result.dff == pytest.approx(10.0)

    def test_free_corrosion_curve(self):
        result = scr_touchdown()
        # Free corrosion: no fatigue limit
        assert result.sn_curve.fatigue_limit == 0.0

    def test_scf_1_by_default(self):
        result = scr_touchdown()
        assert result.scf == pytest.approx(1.0)

    def test_scr_more_damage_than_pipeline_same_conditions(self):
        """SCR F1-free should give more damage than pipeline D-CP for same histogram."""
        from digitalmodel.structural.fatigue.worked_examples import _compute_example
        hist = pd.DataFrame({"range": [50.0, 100.0], "count": [1e5, 1e4]})
        r_pipe = _compute_example(
            "P", "", hist, _dnv_seawater_cp("D"), 1.0, 1.0, 20, [], []
        )
        r_scr = _compute_example(
            "S", "", hist, _dnv_seawater_free("F1"), 1.0, 1.0, 20, [], []
        )
        assert r_scr.damage > r_pipe.damage


# ---------------------------------------------------------------------------
# Mooring chain
# ---------------------------------------------------------------------------

class TestMooringChain:
    def test_smoke(self):
        result = mooring_chain()
        assert result.damage > 0

    def test_name_set(self):
        result = mooring_chain()
        assert result.name == "Mooring Chain Link"

    def test_default_design_life_25_years(self):
        result = mooring_chain()
        assert result.design_life_years == pytest.approx(25.0)

    def test_default_dff_5(self):
        result = mooring_chain()
        assert result.dff == pytest.approx(5.0)

    def test_chain_curve_no_fatigue_limit(self):
        result = mooring_chain()
        assert result.sn_curve.fatigue_limit == 0.0

    def test_chain_damage_positive(self):
        result = mooring_chain()
        assert result.damage > 0

    def test_life_years_positive(self):
        result = mooring_chain()
        assert result.life_years > 0


# ---------------------------------------------------------------------------
# generate_example_report
# ---------------------------------------------------------------------------

class TestGenerateExampleReport:
    @pytest.fixture()
    def pipe_result(self):
        return pipeline_girth_weld()

    def test_returns_string(self, pipe_result):
        html = generate_example_report(pipe_result)
        assert isinstance(html, str)

    def test_contains_html_tag(self, pipe_result):
        html = generate_example_report(pipe_result)
        assert "<html" in html

    def test_contains_plotly_cdn(self, pipe_result):
        html = generate_example_report(pipe_result)
        assert "plotly" in html.lower()

    def test_contains_example_name(self, pipe_result):
        html = generate_example_report(pipe_result)
        assert "Pipeline Girth Weld" in html

    def test_contains_sn_curve_name(self, pipe_result):
        html = generate_example_report(pipe_result)
        assert pipe_result.sn_curve.name in html

    def test_contains_summary_table(self, pipe_result):
        html = generate_example_report(pipe_result)
        assert "Miner Damage" in html

    def test_contains_sn_chart_div(self, pipe_result):
        html = generate_example_report(pipe_result)
        assert 'id="sn-chart"' in html

    def test_contains_hist_chart_div(self, pipe_result):
        html = generate_example_report(pipe_result)
        assert 'id="hist-chart"' in html

    def test_contains_assumptions(self, pipe_result):
        html = generate_example_report(pipe_result)
        assert "Assumptions" in html

    def test_contains_references(self, pipe_result):
        html = generate_example_report(pipe_result)
        assert "References" in html
        assert "DNV" in html

    def test_contains_pass_fail(self, pipe_result):
        html = generate_example_report(pipe_result)
        assert "PASS" in html or "FAIL" in html

    def test_writes_to_file(self, pipe_result):
        with tempfile.NamedTemporaryFile(suffix=".html", delete=False) as f:
            path = f.name
        generate_example_report(pipe_result, output_path=path)
        content = Path(path).read_text()
        assert "<html" in content
        assert len(content) > 2000

    def test_substantial_length(self, pipe_result):
        html = generate_example_report(pipe_result)
        assert len(html) > 3000

    def test_scr_report(self):
        result = scr_touchdown()
        html = generate_example_report(result)
        assert "SCR Touchdown Zone" in html
        assert "F1" in html

    def test_chain_report(self):
        result = mooring_chain()
        html = generate_example_report(result)
        assert "Mooring Chain Link" in html


# ---------------------------------------------------------------------------
# Integration
# ---------------------------------------------------------------------------

class TestIntegration:
    def test_all_three_examples_run(self):
        """All three examples produce valid ExampleResult objects."""
        results = [pipeline_girth_weld(), scr_touchdown(), mooring_chain()]
        for r in results:
            assert isinstance(r, ExampleResult)
            assert r.damage > 0
            assert r.life_years > 0
            assert isinstance(r.passes, (bool, np.bool_))

    def test_all_three_generate_reports(self):
        """All three produce HTML reports with Plotly content."""
        for example_fn in [pipeline_girth_weld, scr_touchdown, mooring_chain]:
            result = example_fn()
            html = generate_example_report(result)
            assert "<html" in html
            assert "plotly" in html.lower()

    def test_damage_order_conservative_to_unconservative(self):
        """
        SCR (F1, free-corrosion, DFF=10) should have higher DFF-adjusted damage
        than pipeline (D, CP, DFF=3) per unit design life — both default.
        """
        pipe = pipeline_girth_weld()
        scr = scr_touchdown()
        # Normalise by design life to compare rates
        pipe_rate = pipe.damage * pipe.dff / pipe.design_life_years
        scr_rate = scr.damage * scr.dff / scr.design_life_years
        # SCR with DFF=10 on F1 free-corrosion should have higher adjusted rate
        assert scr_rate > pipe_rate

    def test_custom_design_life_consistent(self):
        """Damage scales linearly with design life."""
        r20 = scr_touchdown(design_life_years=20)
        r10 = scr_touchdown(design_life_years=10)
        assert r20.damage == pytest.approx(2 * r10.damage, rel=1e-3)
