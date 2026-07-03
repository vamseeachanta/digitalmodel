# ABOUTME: Tests for API 579-1 Part 6 pitting assessment (assessment/pitting.py)
# ABOUTME: — characterization, Level 1 screen, equivalent-LTA Level 2, routing.
"""Pitting assessment tests (#1269).

Validation labelling (repo convention, see test_ffs_published_examples.py):

* There are NO PUBLISHED-VALIDATED cases here — no verified API 579-1 Part 6
  worked example was available to this implementation and the standard's
  coupled-pit charts are deliberately NOT transcribed.
* Synthetic-grid tests are derivation-anchored: every asserted number follows
  from the documented closed forms (pit counting, thickness averages, the
  Part 5 Level 2 RSF/Folias equations already validated elsewhere).
* Real-fixture tests are REGRESSION-ANCHOR: they lock the current output of
  this implementation on the anonymized real UT scan to catch silent drift.
  They are NOT claims of agreement with an external authority.
"""

from pathlib import Path

import numpy as np
import pandas as pd
import pytest

from digitalmodel.asset_integrity.assessment.ffs_coordinator import (
    FFSComponent,
    assess_component,
)
from digitalmodel.asset_integrity.assessment.ffs_router import FFSRouter
from digitalmodel.asset_integrity.assessment.grid_parser import GridParser
from digitalmodel.asset_integrity.assessment.pitting import (
    assess_pitting,
    assess_pitting_level2_equivalent_lta,
    characterize_pit_field,
    screen_pitting_level1,
)

DATA = Path(__file__).parent / "test_data" / "real_inspection"
REAL_GRID = DATA / "ut_grid_RJ-101-boxend_worstzone_fullres.csv"

NOMINAL = 0.500  # inches, synthetic grids


def _grid(rows=30, cols=40, background=NOMINAL, pits=None):
    """Uniform grid with optional seeded pits: {(row, col): reading}."""
    g = np.full((rows, cols), background, dtype=float)
    for (r, c), v in (pits or {}).items():
        g[r, c] = v
    return pd.DataFrame(g)


# Eight well-separated single-cell pits (readings in inches)
EIGHT_PITS = {
    (3, 5): 0.30,
    (3, 20): 0.30,
    (8, 10): 0.25,
    (12, 30): 0.30,
    (15, 4): 0.30,
    (20, 22): 0.30,
    (25, 12): 0.30,
    (27, 35): 0.30,
}


# ===========================================================================
# Pit-field characterization (derivation-anchored data reduction)
# ===========================================================================


class TestCharacterization:
    def test_uniform_grid_has_no_pits(self):
        char = characterize_pit_field(_grid(), NOMINAL)
        assert char.pit_count == 0
        assert char.pitted_cell_count == 0
        assert char.pitted_cell_fraction == 0.0
        assert char.max_pit_depth_ratio == 0.0

    def test_pit_count_and_fraction(self):
        char = characterize_pit_field(_grid(pits=EIGHT_PITS), NOMINAL)
        assert char.pit_count == 8
        assert char.pitted_cell_count == 8
        assert char.pitted_cell_fraction == pytest.approx(8 / (30 * 40))

    def test_depth_statistics(self):
        char = characterize_pit_field(_grid(pits=EIGHT_PITS), NOMINAL)
        # deepest pit reads 0.25 -> w/t = (0.500-0.25)/0.500 = 0.50
        assert char.t_min_pit_in == pytest.approx(0.25)
        assert char.max_pit_depth_ratio == pytest.approx(0.50)
        t_avg = (7 * 0.30 + 0.25) / 8
        assert char.t_avg_pit_in == pytest.approx(t_avg)
        assert char.avg_pit_depth_ratio == pytest.approx((NOMINAL - t_avg) / NOMINAL)

    def test_pitted_region_extent(self):
        char = characterize_pit_field(_grid(pits=EIGHT_PITS), NOMINAL)
        # pits span rows 3..27 inclusive -> 25 rows; unit row spacing
        assert char.pitted_region_row_count == 25
        assert char.pitted_region_length_in == pytest.approx(25.0)
        # cols 4..35 inclusive
        assert char.pitted_region_col_count == 32

    def test_multicell_pit_counts_once(self):
        pits = {(5, 5): 0.30, (5, 6): 0.30, (6, 5): 0.30, (6, 6): 0.30}
        char = characterize_pit_field(_grid(pits=pits), NOMINAL)
        assert char.pit_count == 1
        assert char.largest_pit_cells == 4

    def test_pit_couple_spacing(self):
        pits = {(0, 0): 0.30, (0, 10): 0.30}
        char = characterize_pit_field(_grid(pits=pits), NOMINAL)
        assert char.pit_spacing_min_in == pytest.approx(10.0)
        assert char.pit_spacing_mean_in == pytest.approx(10.0)

    def test_spacing_none_for_single_pit(self):
        char = characterize_pit_field(_grid(pits={(5, 5): 0.30}), NOMINAL)
        assert char.pit_spacing_min_in is None
        assert char.pit_spacing_mean_in is None

    def test_threshold_is_strict(self):
        # reading exactly at 0.9*t_nom is NOT a pit
        char = characterize_pit_field(_grid(pits={(5, 5): 0.45}), NOMINAL)
        assert char.pit_count == 0

    def test_nan_cells_are_not_pits(self):
        g = _grid(pits=EIGHT_PITS)
        g.iloc[0, 0] = np.nan
        char = characterize_pit_field(g, NOMINAL)
        assert char.pit_count == 8
        assert char.total_valid_cells == 30 * 40 - 1

    def test_background_median_ignores_pits(self):
        char = characterize_pit_field(_grid(pits=EIGHT_PITS), NOMINAL)
        assert char.background_median_in == pytest.approx(NOMINAL)

    def test_to_dict_is_serialisable(self):
        d = characterize_pit_field(_grid(pits=EIGHT_PITS), NOMINAL).to_dict()
        assert d["pit_count"] == 8
        assert "pitted_region_length_in" in d


# ===========================================================================
# Level 1 conservative screen (derivation-anchored closed form)
# ===========================================================================


class TestLevel1Screen:
    def _char(self, pits):
        return characterize_pit_field(_grid(pits=pits), NOMINAL)

    def test_shallow_pits_accept(self):
        char = self._char({(5, 5): 0.40, (10, 10): 0.40})
        r = screen_pitting_level1(char, t_min_in=0.35)
        assert r["verdict"] == "ACCEPT"
        assert r["average_criterion_pass"] and r["deep_pit_criterion_pass"]

    def test_average_criterion_fails(self):
        # t_avg_pit = 0.30 < t_min = 0.35
        char = self._char({(5, 5): 0.30, (10, 10): 0.30})
        r = screen_pitting_level1(char, t_min_in=0.35)
        assert r["verdict"] == "FAIL_LEVEL_1"
        assert not r["average_criterion_pass"]

    def test_deep_pit_criterion_fails(self):
        # deepest pit Rt = 0.05/0.50 = 0.10 < 0.20 floor
        char = self._char({(5, 5): 0.45 - 1e-9, (10, 10): 0.05})
        r = screen_pitting_level1(char, t_min_in=0.10)
        assert not r["deep_pit_criterion_pass"]
        assert r["verdict"] == "FAIL_LEVEL_1"
        assert r["rt_deepest_pit"] == pytest.approx(0.10)

    def test_fca_reduces_margin(self):
        char = self._char({(5, 5): 0.40, (10, 10): 0.40})
        assert screen_pitting_level1(char, t_min_in=0.35)["verdict"] == "ACCEPT"
        r = screen_pitting_level1(char, t_min_in=0.35, fca_in=0.10)
        assert r["verdict"] == "FAIL_LEVEL_1"

    def test_no_pits_accepts(self):
        char = characterize_pit_field(_grid(), NOMINAL)
        r = screen_pitting_level1(char, t_min_in=0.45)
        assert r["verdict"] == "ACCEPT"


# ===========================================================================
# Level 2 equivalent-LTA bounding (derivation-anchored to the validated
# Part 5 engine; the bounding basis is stated on the result)
# ===========================================================================


class TestLevel2EquivalentLTA:
    OD = 16.0
    TMIN = 0.35

    def _l2(self, pits):
        char = characterize_pit_field(_grid(pits=pits), NOMINAL)
        return assess_pitting_level2_equivalent_lta(
            char,
            nominal_od_in=self.OD,
            nominal_wt_in=NOMINAL,
            t_min_in=self.TMIN,
            rsf_a=0.9,
        )

    def test_result_shape_and_basis(self):
        r = self._l2(EIGHT_PITS)
        assert r["assessment_type"] == "PITTING"
        assert "equivalent local thin area" in r["assessment_basis"]
        assert 0.0 < r["rsf"] <= 1.0
        assert r["folias_factor"] >= 1.0
        eq = r["equivalent_lta"]
        assert eq["axial_rows"] == 25
        assert eq["axial_extent_in"] == pytest.approx(25.0)
        assert eq["source_pit_count"] == 8

    def test_deeper_pits_lower_rsf(self):
        shallow = {k: 0.30 for k in EIGHT_PITS}
        deep = {k: 0.20 for k in EIGHT_PITS}
        rsf_shallow = self._l2(shallow)["rsf"]
        rsf_deep = self._l2(deep)["rsf"]
        assert rsf_deep < rsf_shallow

    def test_longer_pit_field_does_not_raise_rsf(self):
        compact = {(3, 5): 0.30, (4, 10): 0.30, (5, 20): 0.30}
        extended = {(3, 5): 0.30, (14, 10): 0.30, (27, 20): 0.30}
        assert self._l2(extended)["rsf"] <= self._l2(compact)["rsf"]

    def test_no_pits_gives_unity_rsf(self):
        char = characterize_pit_field(_grid(), NOMINAL)
        r = assess_pitting_level2_equivalent_lta(
            char, nominal_od_in=self.OD, nominal_wt_in=NOMINAL, t_min_in=self.TMIN
        )
        assert r["rsf"] == 1.0
        assert r["verdict"] == "ACCEPT"
        assert r["equivalent_lta"] is None

    def test_one_call_convenience(self):
        r = assess_pitting(
            _grid(pits=EIGHT_PITS),
            nominal_od_in=self.OD,
            nominal_wt_in=NOMINAL,
            t_min_in=self.TMIN,
        )
        assert r["characterization"].pit_count == 8
        assert r["level1"]["verdict"] in ("ACCEPT", "FAIL_LEVEL_1")
        assert r["level2"]["assessment_type"] == "PITTING"


# ===========================================================================
# Router classification (documented heuristic; opt-in-safe)
# ===========================================================================


class TestRouterPittingClassification:
    def test_many_sharp_pits_on_sound_background_route_to_pitting(self):
        result = FFSRouter.classify(
            _grid(pits=EIGHT_PITS), nominal_od_in=16.0, nominal_wt_in=NOMINAL
        )
        assert result["assessment_type"] == "PITTING"
        assert result["auto_classified"] is True
        assert result["pitting"]["detected"] is True
        assert result["pitting"]["pit_count"] == 8

    def test_uniform_loss_still_gml(self):
        df = _grid(background=0.400)
        result = FFSRouter.classify(df, nominal_od_in=16.0, nominal_wt_in=NOMINAL)
        assert result["assessment_type"] == "GML"

    def test_contiguous_patch_still_lml(self):
        pits = {(r, c): 0.30 for r in range(4, 6) for c in range(4, 6)}
        result = FFSRouter.classify(
            _grid(rows=10, cols=20, pits=pits),
            nominal_od_in=16.0,
            nominal_wt_in=NOMINAL,
        )
        assert result["assessment_type"] == "LML"

    def test_too_few_pits_not_pitting(self):
        pits = {(3, 5): 0.30, (12, 30): 0.30, (25, 12): 0.30}
        result = FFSRouter.classify(
            _grid(pits=pits), nominal_od_in=16.0, nominal_wt_in=NOMINAL
        )
        assert result["assessment_type"] == "LML"
        assert result["pitting"]["detected"] is False

    def test_pits_on_corroded_background_not_pitting(self):
        # background at 88 % of nominal -> no contrast -> contiguous-loss path
        df = _grid(background=0.44, pits=EIGHT_PITS)
        result = FFSRouter.classify(df, nominal_od_in=16.0, nominal_wt_in=NOMINAL)
        assert result["assessment_type"] == "GML"
        assert result["pitting"]["detected"] is False

    def test_force_type_pitting(self):
        result = FFSRouter.classify(
            _grid(background=0.400),
            nominal_od_in=16.0,
            nominal_wt_in=NOMINAL,
            force_type="PITTING",
        )
        assert result["assessment_type"] == "PITTING"
        assert result["auto_classified"] is False

    def test_invalid_force_type_still_raises(self):
        with pytest.raises(ValueError, match="force_type"):
            FFSRouter.classify(
                _grid(), nominal_od_in=16.0, nominal_wt_in=NOMINAL, force_type="PART7"
            )


# ===========================================================================
# Coordinator end-to-end
# ===========================================================================


def _component(**overrides):
    kwargs = dict(
        component_id="PIT-001",
        design_code="B31.8",
        nominal_od_in=16.0,
        nominal_wt_in=NOMINAL,
        design_pressure_psi=1000.0,
        smys_psi=52_000.0,
        corrosion_rate_in_per_yr=0.005,
        rsf_a=0.90,
    )
    kwargs.update(overrides)
    return FFSComponent(**kwargs)


class TestCoordinatorPitting:
    def test_force_type_pitting_end_to_end(self):
        res = assess_component(_component(), _grid(pits=EIGHT_PITS),
                               force_type="PITTING")
        assert res.assessment_type == "PITTING"
        assert res.verdict in ("ACCEPT", "MONITOR", "RE_RATE", "REPAIR", "REPLACE")
        assert 0.0 < res.rsf <= 1.0
        assert res.level2["assessment_type"] == "PITTING"
        assert "equivalent local thin area" in res.level2["assessment_basis"]
        assert res.details["pitting_characterization"]["pit_count"] == 8
        assert res.level1["pitting_screen"]["verdict"] in (
            "ACCEPT", "FAIL_LEVEL_1",
        )
        assert res.sufficiency_status in ("SUFFICIENT", "TAKE_MORE", "ESCALATE")

    def test_auto_classification_dispatches_pitting(self):
        res = assess_component(_component(), _grid(pits=EIGHT_PITS))
        assert res.assessment_type == "PITTING"
        assert res.details["router"]["pitting"]["detected"] is True

    def test_healthy_grid_unaffected(self):
        res = assess_component(_component(), _grid())
        assert res.assessment_type in ("GML", "LML")

    def test_deep_pit_field_fails_level1_screen(self):
        # deep pits: Rt = 0.05/0.50 = 0.10 < 0.20 floor
        pits = {k: 0.05 for k in EIGHT_PITS}
        res = assess_component(_component(), _grid(pits=pits),
                               force_type="PITTING")
        assert res.level1["pitting_screen"]["deep_pit_criterion_pass"] is False
        assert res.level1["verdict"] == "FAIL_LEVEL_1"


# ===========================================================================
# Real anonymized inspection fixture (REGRESSION-ANCHOR — locks the current
# implementation's output on a real pit-morphology scan; not an external
# validation)
# ===========================================================================


NOMINAL_OD_REAL = 21.25
NOMINAL_WT_REAL = 0.875  # 22.225 mm


class TestRealFixtureIntegration:
    @pytest.fixture(scope="class")
    def real_grid(self):
        return GridParser.from_csv(str(REAL_GRID), input_units="mm")

    def test_characterize_real_scan(self, real_grid):
        char = characterize_pit_field(real_grid, NOMINAL_WT_REAL)
        # REGRESSION-ANCHOR: current data reduction of the real severe scan
        # (11 isolated pits over 29 037 readings, deepest ~32 % of WT, on a
        # background whose median sits at ~98.6 % of nominal)
        assert char.pit_count == 11
        assert char.pitted_cell_count == 48
        assert char.pitted_cell_fraction == pytest.approx(48 / 29037, rel=1e-9)
        assert char.max_pit_depth_ratio == pytest.approx(0.3240, abs=1e-3)
        assert char.avg_pit_depth_ratio == pytest.approx(0.2574, abs=1e-3)
        assert char.background_median_in / NOMINAL_WT_REAL == pytest.approx(
            0.9861, abs=1e-3
        )
        # physical sanity: deepest reading matches the fixture register (~15.0 mm)
        assert char.t_min_pit_in == pytest.approx(15.02 / 25.4, abs=0.02)

    def test_assess_real_scan_as_pitting(self, real_grid):
        component = FFSComponent(
            component_id="RJ-101-boxend",
            design_code="B31.8",
            nominal_od_in=NOMINAL_OD_REAL,
            nominal_wt_in=NOMINAL_WT_REAL,
            design_pressure_psi=500.0,
            smys_psi=80_000.0,
            corrosion_rate_in_per_yr=0.25 / 25.4,
        )
        res = assess_component(component, real_grid, force_type="PITTING")
        assert res.assessment_type == "PITTING"
        assert "equivalent local thin area" in res.level2["assessment_basis"]
        # REGRESSION-ANCHOR: at 500 psi the code t_min is small, so the
        # equivalent-LTA thickness ratio caps and the RSF is unity; both
        # Level 1 screens pass.
        assert res.rsf == pytest.approx(1.0, abs=1e-9)
        assert res.verdict == "ACCEPT"
        assert res.level1["pitting_screen"]["verdict"] == "ACCEPT"
        assert res.details["pitting_characterization"]["pit_count"] == 11

    def test_real_severe_scan_auto_routes_to_pitting(self, real_grid):
        # REGRESSION-ANCHOR: the severe worst-zone scan is textbook pit
        # morphology (11 sharp separated minima, ~0.17 % of cells, background
        # median ~98.6 % of nominal) — the heuristic detects it.
        route = FFSRouter.classify(
            real_grid,
            nominal_od_in=NOMINAL_OD_REAL,
            nominal_wt_in=NOMINAL_WT_REAL,
        )
        assert route["assessment_type"] == "PITTING"
        assert route["pitting"]["detected"] is True
        assert route["pitting"]["pit_count"] == 11

    def test_real_mild_scan_stays_lml(self):
        # Opt-in safety: the mild scan has only 2 sub-threshold components —
        # below the pit-count floor — so its default classification is
        # unchanged by the pitting heuristic.
        grid = GridParser.from_csv(
            str(DATA / "ut_grid_RJ-103-pinend_ds8.csv"), input_units="mm"
        )
        route = FFSRouter.classify(
            grid, nominal_od_in=NOMINAL_OD_REAL, nominal_wt_in=NOMINAL_WT_REAL
        )
        assert route["assessment_type"] == "LML"
        assert route["pitting"]["detected"] is False
