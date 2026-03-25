# ABOUTME: Tests for unified parametric study coordinator (WRK-256)
# ABOUTME: TDD-first: coordinator dispatches wall-thickness and fatigue sweeps, aggregates results

"""Tests for the unified parametric study coordinator.

Validates dispatch to wall-thickness and fatigue engines, combined sweep,
result aggregation, and the OrcaFlex stub path.
"""

from __future__ import annotations

import pytest
import pandas as pd

from digitalmodel.structural.parametric_coordinator import (
    StudyType,
    WallThicknessParams,
    FatigueParams,
    OrcaFlexParams,
    StudySpec,
    StudyResult,
    ParametricCoordinator,
    ORCAFLEX_AVAILABLE,
)


# ---------------------------------------------------------------------------
# Minimal valid parameter fixtures
# ---------------------------------------------------------------------------

def _wt_params() -> WallThicknessParams:
    return WallThicknessParams(
        wall_thicknesses=[0.010, 0.015, 0.020],
        outer_diameters=[0.27305],
        grades=["X65"],
        internal_pressures=[20e6],
        external_pressures=[5e6],
    )


def _fatigue_params() -> FatigueParams:
    histogram = pd.DataFrame({
        "range": [50.0, 100.0, 150.0],
        "count": [1e6, 1e5, 1e4],
    })
    return FatigueParams(
        histogram=histogram,
        scf_values=[1.0, 1.5],
        curves=[("DNV", "D")],
        thickness_values=[25.0, 32.0],
        dff_values=[1.0, 2.0],
    )


def _ofx_params() -> OrcaFlexParams:
    return OrcaFlexParams(water_depths=[100.0, 200.0])


# ===========================================================================
# StudyType enum
# ===========================================================================

class TestStudyType:
    def test_wall_thickness_member_exists(self):
        assert StudyType.WALL_THICKNESS is not None

    def test_fatigue_member_exists(self):
        assert StudyType.FATIGUE is not None

    def test_orcaflex_campaign_member_exists(self):
        assert StudyType.ORCAFLEX_CAMPAIGN is not None

    def test_combined_wt_fatigue_member_exists(self):
        assert StudyType.COMBINED_WT_FATIGUE is not None

    def test_all_expected_members_present(self):
        names = {m.name for m in StudyType}
        assert {"WALL_THICKNESS", "FATIGUE", "ORCAFLEX_CAMPAIGN", "COMBINED_WT_FATIGUE"}.issubset(names)


# ===========================================================================
# Parameter dataclasses
# ===========================================================================

class TestWallThicknessParams:
    def test_required_fields_accepted(self):
        p = _wt_params()
        assert p.wall_thicknesses == [0.010, 0.015, 0.020]
        assert p.grades == ["X65"]

    def test_default_code_is_dnv(self):
        from digitalmodel.structural.analysis.wall_thickness import DesignCode
        p = _wt_params()
        assert p.code == DesignCode.DNV_ST_F101

    def test_requires_nonempty_wall_thicknesses(self):
        with pytest.raises((ValueError, TypeError)):
            WallThicknessParams(
                wall_thicknesses=[],
                outer_diameters=[0.27305],
                grades=["X65"],
                internal_pressures=[20e6],
                external_pressures=[5e6],
            )


class TestFatigueParams:
    def test_required_fields_accepted(self):
        p = _fatigue_params()
        assert p.scf_values == [1.0, 1.5]

    def test_histogram_stored_as_dataframe(self):
        p = _fatigue_params()
        assert isinstance(p.histogram, pd.DataFrame)
        assert {"range", "count"}.issubset(p.histogram.columns)

    def test_requires_nonempty_curves(self):
        with pytest.raises((ValueError, TypeError)):
            FatigueParams(
                histogram=pd.DataFrame({"range": [50.0], "count": [1e6]}),
                scf_values=[1.0],
                curves=[],
                thickness_values=[25.0],
                dff_values=[1.0],
            )


class TestOrcaFlexParams:
    def test_required_water_depths_accepted(self):
        p = _ofx_params()
        assert p.water_depths == [100.0, 200.0]

    def test_optional_fields_default_to_none(self):
        p = _ofx_params()
        assert p.route_lengths is None
        assert p.tensions is None


# ===========================================================================
# StudySpec validation
# ===========================================================================

class TestStudySpec:
    def test_wt_spec_requires_wt_params(self):
        spec = StudySpec(study_type=StudyType.WALL_THICKNESS, wt_params=_wt_params())
        assert spec.study_type == StudyType.WALL_THICKNESS
        assert spec.wt_params is not None

    def test_fatigue_spec_requires_fatigue_params(self):
        spec = StudySpec(study_type=StudyType.FATIGUE, fatigue_params=_fatigue_params())
        assert spec.fatigue_params is not None

    def test_combined_spec_requires_both_param_sets(self):
        spec = StudySpec(
            study_type=StudyType.COMBINED_WT_FATIGUE,
            wt_params=_wt_params(),
            fatigue_params=_fatigue_params(),
        )
        assert spec.wt_params is not None
        assert spec.fatigue_params is not None

    def test_wt_spec_missing_wt_params_raises(self):
        with pytest.raises(ValueError):
            StudySpec(study_type=StudyType.WALL_THICKNESS)

    def test_fatigue_spec_missing_fatigue_params_raises(self):
        with pytest.raises(ValueError):
            StudySpec(study_type=StudyType.FATIGUE)

    def test_combined_spec_missing_fatigue_params_raises(self):
        with pytest.raises(ValueError):
            StudySpec(study_type=StudyType.COMBINED_WT_FATIGUE, wt_params=_wt_params())

    def test_orcaflex_spec_missing_params_raises(self):
        with pytest.raises(ValueError):
            StudySpec(study_type=StudyType.ORCAFLEX_CAMPAIGN)


# ===========================================================================
# StudyResult
# ===========================================================================

class TestStudyResult:
    def _make_wt_result(self) -> StudyResult:
        coord = ParametricCoordinator()
        spec = StudySpec(study_type=StudyType.WALL_THICKNESS, wt_params=_wt_params())
        return coord.run(spec)

    def _make_fatigue_result(self) -> StudyResult:
        coord = ParametricCoordinator()
        spec = StudySpec(study_type=StudyType.FATIGUE, fatigue_params=_fatigue_params())
        return coord.run(spec)

    def test_wt_result_has_study_type(self):
        r = self._make_wt_result()
        assert r.study_type == StudyType.WALL_THICKNESS

    def test_wt_result_has_dataframe(self):
        r = self._make_wt_result()
        assert isinstance(r.dataframe, pd.DataFrame)
        assert not r.dataframe.empty

    def test_wt_result_dataframe_has_max_utilisation(self):
        r = self._make_wt_result()
        assert "max_utilisation" in r.dataframe.columns

    def test_fatigue_result_has_dataframe(self):
        r = self._make_fatigue_result()
        assert isinstance(r.dataframe, pd.DataFrame)
        assert not r.dataframe.empty

    def test_fatigue_result_dataframe_has_damage(self):
        r = self._make_fatigue_result()
        assert "damage" in r.dataframe.columns

    def test_result_has_summary_dict(self):
        r = self._make_wt_result()
        assert isinstance(r.summary, dict)

    def test_result_summary_has_n_combinations(self):
        r = self._make_wt_result()
        assert "n_combinations" in r.summary

    def test_result_summary_n_combinations_correct(self):
        r = self._make_wt_result()
        assert r.summary["n_combinations"] == len(r.dataframe)

    def test_result_html_report_is_string(self):
        r = self._make_wt_result()
        assert isinstance(r.html_report, str)
        assert len(r.html_report) > 0

    def test_result_html_contains_title(self):
        r = self._make_wt_result()
        assert "Wall Thickness" in r.html_report


# ===========================================================================
# ParametricCoordinator — wall thickness dispatch
# ===========================================================================

class TestCoordinatorWallThickness:
    def test_run_returns_study_result(self):
        coord = ParametricCoordinator()
        spec = StudySpec(study_type=StudyType.WALL_THICKNESS, wt_params=_wt_params())
        result = coord.run(spec)
        assert isinstance(result, StudyResult)

    def test_dispatches_all_wall_thickness_combos(self):
        params = WallThicknessParams(
            wall_thicknesses=[0.010, 0.015],
            outer_diameters=[0.27305],
            grades=["X65"],
            internal_pressures=[20e6],
            external_pressures=[5e6],
        )
        coord = ParametricCoordinator()
        spec = StudySpec(study_type=StudyType.WALL_THICKNESS, wt_params=params)
        result = coord.run(spec)
        # 2 WT x 1 OD x 1 grade x 1 Pi x 1 Pe = 2 rows
        assert len(result.dataframe) == 2

    def test_result_contains_expected_columns(self):
        coord = ParametricCoordinator()
        spec = StudySpec(study_type=StudyType.WALL_THICKNESS, wt_params=_wt_params())
        result = coord.run(spec)
        expected = {"wall_thickness_mm", "max_utilisation", "is_safe", "governing_check"}
        assert expected.issubset(result.dataframe.columns)

    def test_summary_has_study_type(self):
        coord = ParametricCoordinator()
        spec = StudySpec(study_type=StudyType.WALL_THICKNESS, wt_params=_wt_params())
        result = coord.run(spec)
        assert result.summary["study_type"] == StudyType.WALL_THICKNESS.name


# ===========================================================================
# ParametricCoordinator — fatigue dispatch
# ===========================================================================

class TestCoordinatorFatigue:
    def test_run_returns_study_result(self):
        coord = ParametricCoordinator()
        spec = StudySpec(study_type=StudyType.FATIGUE, fatigue_params=_fatigue_params())
        result = coord.run(spec)
        assert isinstance(result, StudyResult)

    def test_result_dataframe_has_expected_columns(self):
        coord = ParametricCoordinator()
        spec = StudySpec(study_type=StudyType.FATIGUE, fatigue_params=_fatigue_params())
        result = coord.run(spec)
        expected = {"scf", "curve_label", "damage", "passes_dff"}
        assert expected.issubset(result.dataframe.columns)

    def test_summary_pass_fail_counts_are_integers(self):
        coord = ParametricCoordinator()
        spec = StudySpec(study_type=StudyType.FATIGUE, fatigue_params=_fatigue_params())
        result = coord.run(spec)
        assert isinstance(result.summary.get("n_pass"), int)
        assert isinstance(result.summary.get("n_fail"), int)

    def test_summary_pass_plus_fail_equals_total(self):
        coord = ParametricCoordinator()
        spec = StudySpec(study_type=StudyType.FATIGUE, fatigue_params=_fatigue_params())
        result = coord.run(spec)
        total = result.summary["n_combinations"]
        assert result.summary["n_pass"] + result.summary["n_fail"] == total


# ===========================================================================
# ParametricCoordinator — combined WT+fatigue dispatch
# ===========================================================================

class TestCoordinatorCombined:
    def test_combined_returns_study_result(self):
        coord = ParametricCoordinator()
        spec = StudySpec(
            study_type=StudyType.COMBINED_WT_FATIGUE,
            wt_params=_wt_params(),
            fatigue_params=_fatigue_params(),
        )
        result = coord.run(spec)
        assert isinstance(result, StudyResult)

    def test_combined_dataframe_has_both_domains(self):
        coord = ParametricCoordinator()
        spec = StudySpec(
            study_type=StudyType.COMBINED_WT_FATIGUE,
            wt_params=_wt_params(),
            fatigue_params=_fatigue_params(),
        )
        result = coord.run(spec)
        assert "max_utilisation" in result.dataframe.columns
        assert "damage" in result.dataframe.columns

    def test_combined_summary_has_both_domain_keys(self):
        coord = ParametricCoordinator()
        spec = StudySpec(
            study_type=StudyType.COMBINED_WT_FATIGUE,
            wt_params=_wt_params(),
            fatigue_params=_fatigue_params(),
        )
        result = coord.run(spec)
        assert "wt_n_combinations" in result.summary
        assert "fatigue_n_combinations" in result.summary

    def test_combined_html_mentions_both_studies(self):
        coord = ParametricCoordinator()
        spec = StudySpec(
            study_type=StudyType.COMBINED_WT_FATIGUE,
            wt_params=_wt_params(),
            fatigue_params=_fatigue_params(),
        )
        result = coord.run(spec)
        assert "Wall Thickness" in result.html_report
        assert "Fatigue" in result.html_report


# ===========================================================================
# ParametricCoordinator — OrcaFlex stub / availability
# ===========================================================================

class TestCoordinatorOrcaFlex:
    def test_orcaflex_available_flag_is_bool(self):
        assert isinstance(ORCAFLEX_AVAILABLE, bool)

    def test_orcaflex_spec_without_availability_raises_or_stubs(self):
        """When OrcFxAPI is absent the coordinator must either raise
        OrcaFlexUnavailableError or return a stub StudyResult with
        a 'stubbed' flag in summary."""
        coord = ParametricCoordinator()
        spec = StudySpec(
            study_type=StudyType.ORCAFLEX_CAMPAIGN, ofx_params=_ofx_params()
        )
        if ORCAFLEX_AVAILABLE:
            # If OrcFxAPI is present the call should not crash
            result = coord.run(spec)
            assert isinstance(result, StudyResult)
        else:
            from digitalmodel.structural.parametric_coordinator import (
                OrcaFlexUnavailableError,
            )
            with pytest.raises(OrcaFlexUnavailableError):
                coord.run(spec)

    def test_stub_error_message_mentions_orcaflex(self):
        if ORCAFLEX_AVAILABLE:
            pytest.skip("OrcFxAPI is present; stub path not tested here")
        from digitalmodel.structural.parametric_coordinator import (
            OrcaFlexUnavailableError,
        )
        coord = ParametricCoordinator()
        spec = StudySpec(
            study_type=StudyType.ORCAFLEX_CAMPAIGN, ofx_params=_ofx_params()
        )
        with pytest.raises(OrcaFlexUnavailableError, match="OrcFxAPI"):
            coord.run(spec)


# ===========================================================================
# ParametricCoordinator — unknown study type
# ===========================================================================

class TestCoordinatorInvalidInput:
    def test_unknown_study_type_raises_value_error(self):
        """Passing an unrecognised study_type must fail fast."""
        coord = ParametricCoordinator()
        # Monkeypatching StudySpec to bypass validation:
        spec = StudySpec.__new__(StudySpec)
        object.__setattr__(spec, "study_type", "NONEXISTENT_TYPE")
        object.__setattr__(spec, "wt_params", None)
        object.__setattr__(spec, "fatigue_params", None)
        object.__setattr__(spec, "ofx_params", None)
        with pytest.raises((ValueError, AttributeError, KeyError)):
            coord.run(spec)
