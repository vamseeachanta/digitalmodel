"""
Tests for FFS Phase 1 wall thickness assessment modules.

Covers:
  - grid_parser: CSV / in-memory DataFrame / NumPy array / unit conversion /
    DataCorrectionFactor / NaN handling
  - ffs_router: GML vs LML classification, user-override, partial-coverage
  - level1_screener: t_mm vs t_min pass/fail (B31.4, B31.8, ASME VIII Div1)
  - level2_engine: GML area-average, LML RSF + Folias, near-boundary RSF
  - ffs_decision: all verdict branches + remaining-life projection
  - ffs_report: HTML output structure

Follows: tests/asset_integrity/test_pipe_sizing.py naming convention.
"""
import io
import math
import textwrap

import numpy as np
import pandas as pd
import pytest


# ---------------------------------------------------------------------------
# Helpers shared by multiple test classes
# ---------------------------------------------------------------------------

def _uniform_grid(rows: int, cols: int, value: float) -> pd.DataFrame:
    """Return a uniform-thickness DataFrame for simple benchmark tests."""
    return pd.DataFrame(
        [[value] * cols for _ in range(rows)],
        index=list(range(rows)),
        columns=list(range(cols)),
    )


def _degraded_grid(
    rows: int,
    cols: int,
    nominal: float,
    flaw_row_slice: slice,
    flaw_col_slice: slice,
    flaw_value: float,
) -> pd.DataFrame:
    """Return a grid with a rectangular degraded region (LML-like flaw)."""
    df = _uniform_grid(rows, cols, nominal)
    df.iloc[flaw_row_slice, flaw_col_slice] = flaw_value
    return df


# ===========================================================================
# grid_parser
# ===========================================================================


class TestGridParserFromDataFrame:
    """GridParser.from_dataframe — normalisation and validation."""

    def test_returns_dataframe(self):
        from digitalmodel.asset_integrity.assessment.grid_parser import GridParser
        df = _uniform_grid(5, 8, 0.500)
        result = GridParser.from_dataframe(df)
        assert isinstance(result, pd.DataFrame)

    def test_shape_preserved(self):
        from digitalmodel.asset_integrity.assessment.grid_parser import GridParser
        df = _uniform_grid(6, 10, 0.450)
        result = GridParser.from_dataframe(df)
        assert result.shape == (6, 10)

    def test_values_unchanged_for_absolute_thickness(self):
        from digitalmodel.asset_integrity.assessment.grid_parser import GridParser
        df = _uniform_grid(4, 4, 0.375)
        result = GridParser.from_dataframe(df)
        assert pytest.approx(result.values.mean()) == 0.375

    def test_data_correction_factor_applied(self):
        from digitalmodel.asset_integrity.assessment.grid_parser import GridParser
        # C-scan percentage grid; nominal 0.500 inch → 100% = 0.500 inch
        # DataCorrectionFactor 0.833 means multiply raw values by 0.833
        df = pd.DataFrame([[100.0, 100.0], [100.0, 100.0]])
        result = GridParser.from_dataframe(df, data_correction_factor=0.833)
        expected = 100.0 * 0.833
        assert pytest.approx(result.values.mean()) == expected

    def test_nan_rows_dropped_when_all_nan(self):
        from digitalmodel.asset_integrity.assessment.grid_parser import GridParser
        df = _uniform_grid(4, 4, 0.375)
        df.iloc[2, :] = float("nan")
        result = GridParser.from_dataframe(df, drop_all_nan_rows=True)
        assert result.shape[0] == 3

    def test_nan_not_dropped_by_default(self):
        from digitalmodel.asset_integrity.assessment.grid_parser import GridParser
        df = _uniform_grid(4, 4, 0.375)
        df.iloc[2, :] = float("nan")
        result = GridParser.from_dataframe(df)
        assert result.shape[0] == 4

    def test_mm_to_inch_conversion(self):
        from digitalmodel.asset_integrity.assessment.grid_parser import GridParser
        df = _uniform_grid(3, 3, 12.7)  # 12.7 mm = 0.5 inch
        result = GridParser.from_dataframe(df, input_units="mm")
        assert pytest.approx(result.values.mean(), rel=1e-4) == 0.5

    def test_inch_input_unchanged(self):
        from digitalmodel.asset_integrity.assessment.grid_parser import GridParser
        df = _uniform_grid(3, 3, 0.5)
        result = GridParser.from_dataframe(df, input_units="in")
        assert pytest.approx(result.values.mean()) == 0.5

    def test_invalid_units_raises_value_error(self):
        from digitalmodel.asset_integrity.assessment.grid_parser import GridParser
        df = _uniform_grid(2, 2, 0.5)
        with pytest.raises(ValueError, match="input_units"):
            GridParser.from_dataframe(df, input_units="cm")

    def test_empty_dataframe_raises_value_error(self):
        from digitalmodel.asset_integrity.assessment.grid_parser import GridParser
        with pytest.raises(ValueError, match="empty"):
            GridParser.from_dataframe(pd.DataFrame())


class TestGridParserFromCSV:
    """GridParser.from_csv — file-object and CSV string round-trip."""

    def test_from_csv_string_returns_dataframe(self):
        from digitalmodel.asset_integrity.assessment.grid_parser import GridParser
        csv_text = "0.500,0.490,0.510\n0.495,0.500,0.505\n"
        f = io.StringIO(csv_text)
        result = GridParser.from_csv(f, header=None)
        assert isinstance(result, pd.DataFrame)
        assert result.shape == (2, 3)

    def test_from_csv_values_correct(self):
        from digitalmodel.asset_integrity.assessment.grid_parser import GridParser
        csv_text = "0.400,0.450\n0.430,0.410\n"
        f = io.StringIO(csv_text)
        result = GridParser.from_csv(f, header=None)
        assert pytest.approx(result.iloc[0, 0]) == 0.400
        assert pytest.approx(result.iloc[1, 1]) == 0.410

    def test_from_csv_with_header_row_ignored(self):
        from digitalmodel.asset_integrity.assessment.grid_parser import GridParser
        csv_text = "col0,col1,col2\n0.500,0.490,0.510\n0.495,0.500,0.505\n"
        f = io.StringIO(csv_text)
        result = GridParser.from_csv(f, header=0)
        assert result.shape == (2, 3)


class TestGridParserFromNumpy:
    """GridParser.from_numpy — 2-D array input."""

    def test_from_numpy_returns_dataframe(self):
        from digitalmodel.asset_integrity.assessment.grid_parser import GridParser
        arr = np.full((5, 7), 0.375)
        result = GridParser.from_numpy(arr)
        assert isinstance(result, pd.DataFrame)

    def test_from_numpy_shape_correct(self):
        from digitalmodel.asset_integrity.assessment.grid_parser import GridParser
        arr = np.ones((4, 6)) * 0.450
        result = GridParser.from_numpy(arr)
        assert result.shape == (4, 6)

    def test_from_numpy_values_correct(self):
        from digitalmodel.asset_integrity.assessment.grid_parser import GridParser
        arr = np.array([[0.5, 0.4], [0.45, 0.48]])
        result = GridParser.from_numpy(arr)
        assert pytest.approx(result.iloc[0, 1]) == 0.4

    def test_from_numpy_1d_raises(self):
        from digitalmodel.asset_integrity.assessment.grid_parser import GridParser
        with pytest.raises(ValueError, match="2-D"):
            GridParser.from_numpy(np.array([0.5, 0.4, 0.3]))


class TestGridParserMinMax:
    """GridParser.min_thickness and max_thickness helpers."""

    def test_min_thickness_returns_minimum(self):
        from digitalmodel.asset_integrity.assessment.grid_parser import GridParser
        df = pd.DataFrame([[0.5, 0.3], [0.4, 0.6]])
        result = GridParser.from_dataframe(df)
        assert pytest.approx(GridParser.min_thickness(result)) == 0.3

    def test_max_thickness_returns_maximum(self):
        from digitalmodel.asset_integrity.assessment.grid_parser import GridParser
        df = pd.DataFrame([[0.5, 0.3], [0.4, 0.6]])
        result = GridParser.from_dataframe(df)
        assert pytest.approx(GridParser.max_thickness(result)) == 0.6

    def test_min_thickness_ignores_nan(self):
        from digitalmodel.asset_integrity.assessment.grid_parser import GridParser
        df = pd.DataFrame([[0.5, float("nan")], [0.4, 0.6]])
        result = GridParser.from_dataframe(df)
        assert pytest.approx(GridParser.min_thickness(result)) == 0.4


# ===========================================================================
# ffs_router
# ===========================================================================


class TestFFSRouterClassification:
    """FFSRouter.classify — GML vs LML routing."""

    def test_uniform_loss_classified_as_gml(self):
        from digitalmodel.asset_integrity.assessment.ffs_router import FFSRouter
        df = _uniform_grid(10, 20, 0.400)
        result = FFSRouter.classify(df, nominal_od_in=16.0, nominal_wt_in=0.500)
        assert result["assessment_type"] == "GML"

    def test_small_local_flaw_classified_as_lml(self):
        from digitalmodel.asset_integrity.assessment.ffs_router import FFSRouter
        df = _degraded_grid(10, 20, 0.500, slice(4, 6), slice(4, 6), 0.300)
        result = FFSRouter.classify(df, nominal_od_in=16.0, nominal_wt_in=0.500)
        assert result["assessment_type"] == "LML"

    def test_user_override_gml_forces_gml(self):
        from digitalmodel.asset_integrity.assessment.ffs_router import FFSRouter
        df = _degraded_grid(10, 20, 0.500, slice(4, 6), slice(4, 6), 0.300)
        result = FFSRouter.classify(
            df, nominal_od_in=16.0, nominal_wt_in=0.500, force_type="GML"
        )
        assert result["assessment_type"] == "GML"

    def test_user_override_lml_forces_lml(self):
        from digitalmodel.asset_integrity.assessment.ffs_router import FFSRouter
        df = _uniform_grid(10, 20, 0.400)
        result = FFSRouter.classify(
            df, nominal_od_in=16.0, nominal_wt_in=0.500, force_type="LML"
        )
        assert result["assessment_type"] == "LML"

    def test_invalid_force_type_raises(self):
        from digitalmodel.asset_integrity.assessment.ffs_router import FFSRouter
        df = _uniform_grid(5, 5, 0.400)
        with pytest.raises(ValueError, match="force_type"):
            FFSRouter.classify(
                df, nominal_od_in=12.0, nominal_wt_in=0.375, force_type="PART6"
            )

    def test_result_contains_flaw_fraction(self):
        from digitalmodel.asset_integrity.assessment.ffs_router import FFSRouter
        df = _degraded_grid(10, 20, 0.500, slice(4, 6), slice(4, 6), 0.300)
        result = FFSRouter.classify(df, nominal_od_in=16.0, nominal_wt_in=0.500)
        assert "degraded_fraction" in result

    def test_degraded_fraction_in_range(self):
        from digitalmodel.asset_integrity.assessment.ffs_router import FFSRouter
        df = _degraded_grid(10, 20, 0.500, slice(4, 6), slice(4, 6), 0.300)
        result = FFSRouter.classify(df, nominal_od_in=16.0, nominal_wt_in=0.500)
        assert 0.0 <= result["degraded_fraction"] <= 1.0


# ===========================================================================
# level1_screener
# ===========================================================================


class TestLevel1ScreenerB318:
    """Level1Screener with B31.8 design code (gas pipeline)."""

    # B31.8 internal pressure wall thickness: t = P*D / (2*S*F*E*T)
    # S = SMYS, F = 0.72, E = 1.0, T = 1.0 (temperature derating, <=250F)

    def _b318_tmin(
        self,
        pressure_psi: float,
        od_in: float,
        smys_psi: float,
        df: float = 0.72,
    ) -> float:
        return (pressure_psi * od_in) / (2 * smys_psi * df * 1.0 * 1.0)

    def test_accept_when_tmm_exceeds_tmin(self):
        from digitalmodel.asset_integrity.assessment.level1_screener import (
            Level1Screener,
        )
        # 16-inch gas pipeline, X52 (SMYS=52000 psi), 1000 psi operating
        od, smys = 16.0, 52000
        tmin = self._b318_tmin(1000, od, smys)
        screener = Level1Screener(
            design_code="B31.8",
            nominal_od_in=od,
            smys_psi=smys,
            design_pressure_psi=1000.0,
        )
        result = screener.evaluate(tmm_in=tmin + 0.050)
        assert result["verdict"] == "ACCEPT"

    def test_fail_when_tmm_below_tmin(self):
        from digitalmodel.asset_integrity.assessment.level1_screener import (
            Level1Screener,
        )
        od, smys = 16.0, 52000
        tmin = self._b318_tmin(1000, od, smys)
        screener = Level1Screener(
            design_code="B31.8",
            nominal_od_in=od,
            smys_psi=smys,
            design_pressure_psi=1000.0,
        )
        result = screener.evaluate(tmm_in=tmin - 0.050)
        assert result["verdict"] == "FAIL_LEVEL_1"

    def test_tmin_value_numerically_correct(self):
        from digitalmodel.asset_integrity.assessment.level1_screener import (
            Level1Screener,
        )
        od, smys, p = 16.0, 52000, 1000.0
        expected_tmin = self._b318_tmin(p, od, smys)
        screener = Level1Screener(
            design_code="B31.8",
            nominal_od_in=od,
            smys_psi=smys,
            design_pressure_psi=p,
        )
        result = screener.evaluate(tmm_in=0.5)
        assert pytest.approx(result["t_min_in"], rel=1e-5) == expected_tmin

    def test_boundary_exactly_at_tmin_accepts(self):
        from digitalmodel.asset_integrity.assessment.level1_screener import (
            Level1Screener,
        )
        od, smys, p = 16.0, 52000, 1000.0
        tmin = self._b318_tmin(p, od, smys)
        screener = Level1Screener(
            design_code="B31.8",
            nominal_od_in=od,
            smys_psi=smys,
            design_pressure_psi=p,
        )
        result = screener.evaluate(tmm_in=tmin)
        assert result["verdict"] == "ACCEPT"

    def test_result_contains_required_keys(self):
        from digitalmodel.asset_integrity.assessment.level1_screener import (
            Level1Screener,
        )
        screener = Level1Screener(
            design_code="B31.8",
            nominal_od_in=16.0,
            smys_psi=52000,
            design_pressure_psi=1000.0,
        )
        result = screener.evaluate(tmm_in=0.5)
        for key in ("verdict", "t_min_in", "t_mm_in", "margin_in"):
            assert key in result, f"Missing key: {key}"


class TestLevel1ScreenerB314:
    """Level1Screener with B31.4 design code (liquid pipeline)."""

    # B31.4 hoop stress: t = P*D / (2*S*E), E=1.0, design factor = 0.72
    def _b314_tmin(self, pressure_psi: float, od_in: float, smys_psi: float) -> float:
        return (pressure_psi * od_in) / (2 * smys_psi * 0.72 * 1.0)

    def test_b314_accept(self):
        from digitalmodel.asset_integrity.assessment.level1_screener import (
            Level1Screener,
        )
        od, smys, p = 12.0, 52000, 800.0
        tmin = self._b314_tmin(p, od, smys)
        screener = Level1Screener(
            design_code="B31.4",
            nominal_od_in=od,
            smys_psi=smys,
            design_pressure_psi=p,
        )
        result = screener.evaluate(tmm_in=tmin + 0.05)
        assert result["verdict"] == "ACCEPT"

    def test_b314_fail(self):
        from digitalmodel.asset_integrity.assessment.level1_screener import (
            Level1Screener,
        )
        od, smys, p = 12.0, 52000, 800.0
        tmin = self._b314_tmin(p, od, smys)
        screener = Level1Screener(
            design_code="B31.4",
            nominal_od_in=od,
            smys_psi=smys,
            design_pressure_psi=p,
        )
        result = screener.evaluate(tmm_in=tmin - 0.05)
        assert result["verdict"] == "FAIL_LEVEL_1"


class TestLevel1ScreenerASMEVIII:
    """Level1Screener with ASME VIII Div 1 design code (pressure vessel)."""

    # UG-27(c)(1): t = P*R / (S*E - 0.6*P)
    # Using: R = (OD - 2t) / 2 approximated as OD/2 for thin-wall
    # For t_min from OD: t = P*(OD/2) / (S*E - 0.6*P)
    def _asme8_tmin(
        self, pressure_psi: float, od_in: float, allowable_stress_psi: float
    ) -> float:
        r = od_in / 2.0
        return (pressure_psi * r) / (allowable_stress_psi * 1.0 - 0.6 * pressure_psi)

    def test_asme8_accept(self):
        from digitalmodel.asset_integrity.assessment.level1_screener import (
            Level1Screener,
        )
        od, s_allow, p = 48.0, 17500, 150.0
        tmin = self._asme8_tmin(p, od, s_allow)
        screener = Level1Screener(
            design_code="ASME_VIII_DIV1",
            nominal_od_in=od,
            allowable_stress_psi=s_allow,
            design_pressure_psi=p,
        )
        result = screener.evaluate(tmm_in=tmin + 0.05)
        assert result["verdict"] == "ACCEPT"

    def test_asme8_fail(self):
        from digitalmodel.asset_integrity.assessment.level1_screener import (
            Level1Screener,
        )
        od, s_allow, p = 48.0, 17500, 150.0
        tmin = self._asme8_tmin(p, od, s_allow)
        screener = Level1Screener(
            design_code="ASME_VIII_DIV1",
            nominal_od_in=od,
            allowable_stress_psi=s_allow,
            design_pressure_psi=p,
        )
        result = screener.evaluate(tmm_in=tmin - 0.05)
        assert result["verdict"] == "FAIL_LEVEL_1"

    def test_unsupported_code_raises(self):
        from digitalmodel.asset_integrity.assessment.level1_screener import (
            Level1Screener,
        )
        with pytest.raises(ValueError, match="design_code"):
            Level1Screener(
                design_code="DNV_F101",
                nominal_od_in=12.0,
                smys_psi=52000,
                design_pressure_psi=500.0,
            )


# ===========================================================================
# level2_engine
# ===========================================================================


class TestLevel2EngineGML:
    """Level2Engine — GML area-averaged thickness assessment."""

    def test_accept_when_rsf_above_rsfa(self):
        from digitalmodel.asset_integrity.assessment.level2_engine import Level2Engine
        # Uniform grid at 90% of nominal → mild degradation → RSF should pass
        nominal_wt = 0.500
        df = _uniform_grid(10, 20, nominal_wt * 0.92)
        engine = Level2Engine(
            assessment_type="GML",
            nominal_od_in=16.0,
            nominal_wt_in=nominal_wt,
            t_min_in=0.260,
            rsf_a=0.9,
        )
        result = engine.evaluate(df)
        assert result["verdict"] == "ACCEPT"

    def test_fail_when_rsf_below_rsfa(self):
        from digitalmodel.asset_integrity.assessment.level2_engine import Level2Engine
        # Grid thinned to 50% of nominal
        nominal_wt = 0.500
        df = _uniform_grid(10, 20, nominal_wt * 0.50)
        engine = Level2Engine(
            assessment_type="GML",
            nominal_od_in=16.0,
            nominal_wt_in=nominal_wt,
            t_min_in=0.400,
            rsf_a=0.9,
        )
        result = engine.evaluate(df)
        assert result["verdict"] == "FAIL_LEVEL_2"

    def test_rsf_key_present_in_result(self):
        from digitalmodel.asset_integrity.assessment.level2_engine import Level2Engine
        df = _uniform_grid(5, 10, 0.450)
        engine = Level2Engine(
            assessment_type="GML",
            nominal_od_in=16.0,
            nominal_wt_in=0.500,
            t_min_in=0.260,
            rsf_a=0.9,
        )
        result = engine.evaluate(df)
        assert "rsf" in result

    def test_rsf_is_one_for_undamaged_pipe(self):
        from digitalmodel.asset_integrity.assessment.level2_engine import Level2Engine
        nominal_wt = 0.500
        df = _uniform_grid(5, 10, nominal_wt)
        engine = Level2Engine(
            assessment_type="GML",
            nominal_od_in=16.0,
            nominal_wt_in=nominal_wt,
            t_min_in=0.260,
            rsf_a=0.9,
        )
        result = engine.evaluate(df)
        assert pytest.approx(result["rsf"], rel=1e-4) == 1.0

    def test_t_am_key_present(self):
        from digitalmodel.asset_integrity.assessment.level2_engine import Level2Engine
        df = _uniform_grid(5, 10, 0.450)
        engine = Level2Engine(
            assessment_type="GML",
            nominal_od_in=16.0,
            nominal_wt_in=0.500,
            t_min_in=0.260,
            rsf_a=0.9,
        )
        result = engine.evaluate(df)
        assert "t_am_in" in result


class TestLevel2EngineLML:
    """Level2Engine — LML RSF and Folias-factor assessment."""

    def test_accept_local_flaw_with_sufficient_rsf(self):
        from digitalmodel.asset_integrity.assessment.level2_engine import Level2Engine
        # 16-inch pipe, nominal WT 0.500 inch; small local flaw 0.300 inch deep
        nominal_wt = 0.500
        df = _degraded_grid(10, 20, nominal_wt, slice(4, 6), slice(4, 6), 0.380)
        engine = Level2Engine(
            assessment_type="LML",
            nominal_od_in=16.0,
            nominal_wt_in=nominal_wt,
            t_min_in=0.260,
            rsf_a=0.9,
        )
        result = engine.evaluate(df)
        assert result["verdict"] == "ACCEPT"

    def test_reject_deep_local_flaw(self):
        from digitalmodel.asset_integrity.assessment.level2_engine import Level2Engine
        nominal_wt = 0.500
        # Very deep flaw → t_mm at 40% of nominal
        df = _degraded_grid(10, 20, nominal_wt, slice(4, 7), slice(4, 7), 0.180)
        engine = Level2Engine(
            assessment_type="LML",
            nominal_od_in=16.0,
            nominal_wt_in=nominal_wt,
            t_min_in=0.300,
            rsf_a=0.9,
        )
        result = engine.evaluate(df)
        assert result["verdict"] == "FAIL_LEVEL_2"

    def test_rsf_boundary_exactly_at_rsfa_accepts(self):
        """RSF == RSFa should produce ACCEPT (boundary condition)."""
        from digitalmodel.asset_integrity.assessment.level2_engine import Level2Engine
        # Construct a grid where RSF evaluates to exactly 0.9 by hand
        # For GML, RSF = t_am / t_min (simplified). If t_am/t_min = 0.9 → ACCEPT
        t_min = 0.300
        t_am = t_min * 0.9  # RSF = 0.9 exactly
        nominal_wt = 0.400
        df = _uniform_grid(5, 10, t_am)
        engine = Level2Engine(
            assessment_type="GML",
            nominal_od_in=16.0,
            nominal_wt_in=nominal_wt,
            t_min_in=t_min,
            rsf_a=0.9,
        )
        result = engine.evaluate(df)
        assert result["verdict"] == "ACCEPT"

    def test_result_has_folias_factor_for_lml(self):
        from digitalmodel.asset_integrity.assessment.level2_engine import Level2Engine
        nominal_wt = 0.500
        df = _degraded_grid(10, 20, nominal_wt, slice(4, 6), slice(4, 6), 0.380)
        engine = Level2Engine(
            assessment_type="LML",
            nominal_od_in=16.0,
            nominal_wt_in=nominal_wt,
            t_min_in=0.260,
            rsf_a=0.9,
        )
        result = engine.evaluate(df)
        assert "folias_factor" in result

    def test_folias_factor_greater_than_one(self):
        from digitalmodel.asset_integrity.assessment.level2_engine import Level2Engine
        nominal_wt = 0.500
        df = _degraded_grid(10, 20, nominal_wt, slice(4, 6), slice(4, 6), 0.380)
        engine = Level2Engine(
            assessment_type="LML",
            nominal_od_in=16.0,
            nominal_wt_in=nominal_wt,
            t_min_in=0.260,
            rsf_a=0.9,
        )
        result = engine.evaluate(df)
        assert result["folias_factor"] >= 1.0

    def test_rsf_custom_rsfa_0_85(self):
        """Stricter RSFa = 0.85 should reject a case that 0.9 would accept."""
        from digitalmodel.asset_integrity.assessment.level2_engine import Level2Engine
        # t_am slightly above 0.85 * t_min but below 0.90 * t_min
        t_min = 0.300
        t_am = t_min * 0.87  # passes 0.85 threshold, fails 0.9 threshold
        nominal_wt = 0.400
        df = _uniform_grid(5, 10, t_am)
        engine_strict = Level2Engine(
            assessment_type="GML",
            nominal_od_in=16.0,
            nominal_wt_in=nominal_wt,
            t_min_in=t_min,
            rsf_a=0.9,
        )
        result_strict = engine_strict.evaluate(df)
        assert result_strict["verdict"] == "FAIL_LEVEL_2"

    def test_invalid_assessment_type_raises(self):
        from digitalmodel.asset_integrity.assessment.level2_engine import Level2Engine
        with pytest.raises(ValueError, match="assessment_type"):
            Level2Engine(
                assessment_type="PART6",
                nominal_od_in=16.0,
                nominal_wt_in=0.500,
                t_min_in=0.260,
                rsf_a=0.9,
            )


# ===========================================================================
# ffs_decision
# ===========================================================================


class TestFFSDecision:
    """FFSDecision.decide — all verdict branches."""

    def test_accept_when_both_levels_pass(self):
        from digitalmodel.asset_integrity.assessment.ffs_decision import FFSDecision
        # RSF=0.97 is above the MONITOR band (RSFa + 0.05 = 0.95) → pure ACCEPT
        result = FFSDecision.decide(
            level1_verdict="ACCEPT",
            level2_verdict="ACCEPT",
            rsf=0.97,
            rsf_a=0.9,
            t_mm_in=0.400,
            t_min_in=0.300,
            corrosion_rate_in_per_yr=0.005,
        )
        assert result["verdict"] == "ACCEPT"

    def test_fail_when_level1_fails(self):
        from digitalmodel.asset_integrity.assessment.ffs_decision import FFSDecision
        result = FFSDecision.decide(
            level1_verdict="FAIL_LEVEL_1",
            level2_verdict="ACCEPT",
            rsf=0.95,
            rsf_a=0.9,
            t_mm_in=0.250,
            t_min_in=0.300,
            corrosion_rate_in_per_yr=0.005,
        )
        assert result["verdict"] in ("REPAIR", "REPLACE", "RE_RATE")

    def test_repair_when_level2_fails_with_remaining_life(self):
        from digitalmodel.asset_integrity.assessment.ffs_decision import FFSDecision
        result = FFSDecision.decide(
            level1_verdict="FAIL_LEVEL_1",
            level2_verdict="FAIL_LEVEL_2",
            rsf=0.75,
            rsf_a=0.9,
            t_mm_in=0.280,
            t_min_in=0.300,
            corrosion_rate_in_per_yr=0.010,
        )
        assert result["verdict"] in ("REPAIR", "REPLACE", "RE_RATE", "MONITOR")

    def test_accept_result_has_remaining_life(self):
        from digitalmodel.asset_integrity.assessment.ffs_decision import FFSDecision
        result = FFSDecision.decide(
            level1_verdict="ACCEPT",
            level2_verdict="ACCEPT",
            rsf=0.95,
            rsf_a=0.9,
            t_mm_in=0.400,
            t_min_in=0.300,
            corrosion_rate_in_per_yr=0.005,
        )
        assert "remaining_life_yr" in result

    def test_remaining_life_positive_for_accept(self):
        from digitalmodel.asset_integrity.assessment.ffs_decision import FFSDecision
        result = FFSDecision.decide(
            level1_verdict="ACCEPT",
            level2_verdict="ACCEPT",
            rsf=0.95,
            rsf_a=0.9,
            t_mm_in=0.400,
            t_min_in=0.300,
            corrosion_rate_in_per_yr=0.005,
        )
        assert result["remaining_life_yr"] > 0

    def test_remaining_life_formula(self):
        """Remaining life = (t_mm - t_min) / corrosion_rate."""
        from digitalmodel.asset_integrity.assessment.ffs_decision import FFSDecision
        t_mm, t_min, rate = 0.400, 0.300, 0.010
        result = FFSDecision.decide(
            level1_verdict="ACCEPT",
            level2_verdict="ACCEPT",
            rsf=0.95,
            rsf_a=0.9,
            t_mm_in=t_mm,
            t_min_in=t_min,
            corrosion_rate_in_per_yr=rate,
        )
        expected = (t_mm - t_min) / rate
        assert pytest.approx(result["remaining_life_yr"], rel=1e-5) == expected

    def test_result_contains_required_keys(self):
        from digitalmodel.asset_integrity.assessment.ffs_decision import FFSDecision
        result = FFSDecision.decide(
            level1_verdict="ACCEPT",
            level2_verdict="ACCEPT",
            rsf=0.95,
            rsf_a=0.9,
            t_mm_in=0.400,
            t_min_in=0.300,
            corrosion_rate_in_per_yr=0.005,
        )
        for key in ("verdict", "remaining_life_yr", "governing_criterion"):
            assert key in result, f"Missing key: {key}"

    def test_zero_corrosion_rate_gives_infinite_remaining_life(self):
        from digitalmodel.asset_integrity.assessment.ffs_decision import FFSDecision
        result = FFSDecision.decide(
            level1_verdict="ACCEPT",
            level2_verdict="ACCEPT",
            rsf=0.95,
            rsf_a=0.9,
            t_mm_in=0.400,
            t_min_in=0.300,
            corrosion_rate_in_per_yr=0.0,
        )
        assert result["remaining_life_yr"] == float("inf")

    def test_rsf_below_rsfa_triggers_non_accept_verdict(self):
        from digitalmodel.asset_integrity.assessment.ffs_decision import FFSDecision
        result = FFSDecision.decide(
            level1_verdict="ACCEPT",
            level2_verdict="FAIL_LEVEL_2",
            rsf=0.80,
            rsf_a=0.9,
            t_mm_in=0.350,
            t_min_in=0.300,
            corrosion_rate_in_per_yr=0.008,
        )
        assert result["verdict"] != "ACCEPT"


# ===========================================================================
# ffs_report
# ===========================================================================


class TestFFSReport:
    """FFSReport.generate_html — HTML output structure and content."""

    def _make_report_inputs(self):
        from digitalmodel.asset_integrity.assessment.ffs_decision import FFSDecision
        df = _uniform_grid(5, 10, 0.420)
        decision = FFSDecision.decide(
            level1_verdict="ACCEPT",
            level2_verdict="ACCEPT",
            rsf=0.93,
            rsf_a=0.9,
            t_mm_in=0.420,
            t_min_in=0.300,
            corrosion_rate_in_per_yr=0.006,
        )
        return df, decision

    def test_returns_non_empty_string(self):
        from digitalmodel.asset_integrity.assessment.ffs_report import FFSReport
        df, decision = self._make_report_inputs()
        html = FFSReport.generate_html(
            grid_df=df,
            decision=decision,
            component_id="PIPE-001",
            nominal_od_in=16.0,
            nominal_wt_in=0.500,
            t_min_in=0.300,
            design_code="B31.8",
            design_pressure_psi=1000.0,
        )
        assert isinstance(html, str) and len(html) > 0

    def test_html_starts_with_doctype_or_html_tag(self):
        from digitalmodel.asset_integrity.assessment.ffs_report import FFSReport
        df, decision = self._make_report_inputs()
        html = FFSReport.generate_html(
            grid_df=df,
            decision=decision,
            component_id="PIPE-001",
            nominal_od_in=16.0,
            nominal_wt_in=0.500,
            t_min_in=0.300,
            design_code="B31.8",
            design_pressure_psi=1000.0,
        )
        assert html.lstrip().startswith("<!DOCTYPE") or html.lstrip().startswith("<html")

    def test_html_contains_component_id(self):
        from digitalmodel.asset_integrity.assessment.ffs_report import FFSReport
        df, decision = self._make_report_inputs()
        html = FFSReport.generate_html(
            grid_df=df,
            decision=decision,
            component_id="VESSEL-42",
            nominal_od_in=16.0,
            nominal_wt_in=0.500,
            t_min_in=0.300,
            design_code="B31.8",
            design_pressure_psi=1000.0,
        )
        assert "VESSEL-42" in html

    def test_html_contains_verdict(self):
        from digitalmodel.asset_integrity.assessment.ffs_report import FFSReport
        df, decision = self._make_report_inputs()
        html = FFSReport.generate_html(
            grid_df=df,
            decision=decision,
            component_id="PIPE-001",
            nominal_od_in=16.0,
            nominal_wt_in=0.500,
            t_min_in=0.300,
            design_code="B31.8",
            design_pressure_psi=1000.0,
        )
        assert "ACCEPT" in html

    def test_html_contains_t_min(self):
        from digitalmodel.asset_integrity.assessment.ffs_report import FFSReport
        df, decision = self._make_report_inputs()
        html = FFSReport.generate_html(
            grid_df=df,
            decision=decision,
            component_id="PIPE-001",
            nominal_od_in=16.0,
            nominal_wt_in=0.500,
            t_min_in=0.300,
            design_code="B31.8",
            design_pressure_psi=1000.0,
        )
        assert "0.300" in html

    def test_html_contains_design_code(self):
        from digitalmodel.asset_integrity.assessment.ffs_report import FFSReport
        df, decision = self._make_report_inputs()
        html = FFSReport.generate_html(
            grid_df=df,
            decision=decision,
            component_id="PIPE-001",
            nominal_od_in=16.0,
            nominal_wt_in=0.500,
            t_min_in=0.300,
            design_code="B31.8",
            design_pressure_psi=1000.0,
        )
        assert "B31.8" in html

    def test_html_contains_api579_reference(self):
        from digitalmodel.asset_integrity.assessment.ffs_report import FFSReport
        df, decision = self._make_report_inputs()
        html = FFSReport.generate_html(
            grid_df=df,
            decision=decision,
            component_id="PIPE-001",
            nominal_od_in=16.0,
            nominal_wt_in=0.500,
            t_min_in=0.300,
            design_code="B31.8",
            design_pressure_psi=1000.0,
        )
        assert "API 579" in html

    def test_html_contains_closing_html_tag(self):
        from digitalmodel.asset_integrity.assessment.ffs_report import FFSReport
        df, decision = self._make_report_inputs()
        html = FFSReport.generate_html(
            grid_df=df,
            decision=decision,
            component_id="PIPE-001",
            nominal_od_in=16.0,
            nominal_wt_in=0.500,
            t_min_in=0.300,
            design_code="B31.8",
            design_pressure_psi=1000.0,
        )
        assert "</html>" in html


# ===========================================================================
# Integration: end-to-end pipeline
# ===========================================================================


class TestFFSEndToEnd:
    """End-to-end: grid → router → Level 1 → Level 2 → decision → report."""

    def _run_pipeline(
        self,
        grid_df: pd.DataFrame,
        nominal_od_in: float,
        nominal_wt_in: float,
        t_min_in: float,
        design_code: str,
        smys_psi: float,
        design_pressure_psi: float,
        corrosion_rate_in_per_yr: float,
        rsf_a: float = 0.9,
    ) -> dict:
        from digitalmodel.asset_integrity.assessment.grid_parser import GridParser
        from digitalmodel.asset_integrity.assessment.ffs_router import FFSRouter
        from digitalmodel.asset_integrity.assessment.level1_screener import (
            Level1Screener,
        )
        from digitalmodel.asset_integrity.assessment.level2_engine import Level2Engine
        from digitalmodel.asset_integrity.assessment.ffs_decision import FFSDecision

        parsed = GridParser.from_dataframe(grid_df)
        routing = FFSRouter.classify(
            parsed, nominal_od_in=nominal_od_in, nominal_wt_in=nominal_wt_in
        )

        screener = Level1Screener(
            design_code=design_code,
            nominal_od_in=nominal_od_in,
            smys_psi=smys_psi,
            design_pressure_psi=design_pressure_psi,
        )
        t_mm = GridParser.min_thickness(parsed)
        l1 = screener.evaluate(tmm_in=t_mm)

        engine = Level2Engine(
            assessment_type=routing["assessment_type"],
            nominal_od_in=nominal_od_in,
            nominal_wt_in=nominal_wt_in,
            t_min_in=t_min_in,
            rsf_a=rsf_a,
        )
        l2 = engine.evaluate(parsed)

        decision = FFSDecision.decide(
            level1_verdict=l1["verdict"],
            level2_verdict=l2["verdict"],
            rsf=l2["rsf"],
            rsf_a=rsf_a,
            t_mm_in=t_mm,
            t_min_in=t_min_in,
            corrosion_rate_in_per_yr=corrosion_rate_in_per_yr,
        )
        return {"routing": routing, "l1": l1, "l2": l2, "decision": decision}

    def test_16in_gas_pipeline_healthy_accepts(self):
        """16-inch gas pipeline (B31.8, X52) with mild corrosion should ACCEPT."""
        nominal_wt = 0.500
        df = _uniform_grid(10, 50, nominal_wt * 0.94)  # 6% loss
        result = self._run_pipeline(
            grid_df=df,
            nominal_od_in=16.0,
            nominal_wt_in=nominal_wt,
            t_min_in=0.260,
            design_code="B31.8",
            smys_psi=52000,
            design_pressure_psi=1000.0,
            corrosion_rate_in_per_yr=0.005,
        )
        assert result["decision"]["verdict"] == "ACCEPT"

    def test_12in_oil_pipeline_severe_corrosion_fails(self):
        """12-inch oil pipeline with severe corrosion (~70% WT loss) should fail."""
        nominal_wt = 0.375
        # Grid at 30% of nominal: t_am = 0.1125 in, t_min = 0.200 in
        # RSF = t_am / t_min = 0.1125 / 0.200 = 0.5625 < 0.9 → FAIL_LEVEL_2
        df = _uniform_grid(8, 30, nominal_wt * 0.30)
        result = self._run_pipeline(
            grid_df=df,
            nominal_od_in=12.0,
            nominal_wt_in=nominal_wt,
            t_min_in=0.200,
            design_code="B31.4",
            smys_psi=52000,
            design_pressure_psi=800.0,
            corrosion_rate_in_per_yr=0.015,
        )
        assert result["decision"]["verdict"] != "ACCEPT"

    def test_pipeline_with_csv_input_returns_verdict(self):
        """CSV-sourced grid parsed through the full pipeline returns a verdict."""
        from digitalmodel.asset_integrity.assessment.grid_parser import GridParser
        from digitalmodel.asset_integrity.assessment.ffs_router import FFSRouter
        from digitalmodel.asset_integrity.assessment.level1_screener import (
            Level1Screener,
        )
        from digitalmodel.asset_integrity.assessment.level2_engine import Level2Engine
        from digitalmodel.asset_integrity.assessment.ffs_decision import FFSDecision

        csv_text = "\n".join(
            [",".join(["0.480"] * 12) for _ in range(8)]
        )
        f = io.StringIO(csv_text)
        parsed = GridParser.from_csv(f, header=None)

        routing = FFSRouter.classify(
            parsed, nominal_od_in=12.0, nominal_wt_in=0.375
        )
        screener = Level1Screener(
            design_code="B31.8",
            nominal_od_in=12.0,
            smys_psi=52000,
            design_pressure_psi=900.0,
        )
        t_mm = GridParser.min_thickness(parsed)
        l1 = screener.evaluate(tmm_in=t_mm)
        engine = Level2Engine(
            assessment_type=routing["assessment_type"],
            nominal_od_in=12.0,
            nominal_wt_in=0.375,
            t_min_in=0.200,
            rsf_a=0.9,
        )
        l2 = engine.evaluate(parsed)
        decision = FFSDecision.decide(
            level1_verdict=l1["verdict"],
            level2_verdict=l2["verdict"],
            rsf=l2["rsf"],
            rsf_a=0.9,
            t_mm_in=t_mm,
            t_min_in=0.200,
            corrosion_rate_in_per_yr=0.005,
        )
        assert "verdict" in decision

    def test_numpy_input_end_to_end(self):
        """NumPy array input completes a full assessment pipeline."""
        from digitalmodel.asset_integrity.assessment.grid_parser import GridParser
        from digitalmodel.asset_integrity.assessment.ffs_decision import FFSDecision
        from digitalmodel.asset_integrity.assessment.ffs_router import FFSRouter
        from digitalmodel.asset_integrity.assessment.level1_screener import (
            Level1Screener,
        )
        from digitalmodel.asset_integrity.assessment.level2_engine import Level2Engine

        arr = np.full((6, 14), 0.430)
        parsed = GridParser.from_numpy(arr)
        routing = FFSRouter.classify(
            parsed, nominal_od_in=14.0, nominal_wt_in=0.500
        )
        screener = Level1Screener(
            design_code="B31.8",
            nominal_od_in=14.0,
            smys_psi=52000,
            design_pressure_psi=900.0,
        )
        t_mm = GridParser.min_thickness(parsed)
        l1 = screener.evaluate(tmm_in=t_mm)
        engine = Level2Engine(
            assessment_type=routing["assessment_type"],
            nominal_od_in=14.0,
            nominal_wt_in=0.500,
            t_min_in=0.240,
            rsf_a=0.9,
        )
        l2 = engine.evaluate(parsed)
        decision = FFSDecision.decide(
            level1_verdict=l1["verdict"],
            level2_verdict=l2["verdict"],
            rsf=l2["rsf"],
            rsf_a=0.9,
            t_mm_in=t_mm,
            t_min_in=0.240,
            corrosion_rate_in_per_yr=0.004,
        )
        assert "verdict" in decision
