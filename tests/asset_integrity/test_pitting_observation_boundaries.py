# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Synthetic data-boundary regressions; no physical-method qualification."""

import math

import numpy as np
import pandas as pd
import pytest

from digitalmodel.asset_integrity.assessment.pitting import (
    assess_pitting,
    assess_pitting_level2_equivalent_lta,
    characterize_pit_field,
    screen_pitting_level1,
)


def _level2(characterization):
    return assess_pitting_level2_equivalent_lta(
        characterization, nominal_od_in=24.0, nominal_wt_in=0.5,
        t_min_in=0.35,
    )


def test_pit_free_summary_retains_observed_wall_instead_of_nominal():
    char = characterize_pit_field(pd.DataFrame([[0.45, 0.46], [0.48, 0.49]]), 0.5)
    assert char.pit_count == 0
    assert char.t_min_pit_in == pytest.approx(0.45)
    assert char.t_avg_pit_in == pytest.approx(0.47)
    assert char.background_median_in == pytest.approx(0.47)
    assert char.max_pit_depth_in == 0.0
    assert char.pitted_region_length_in == 0.0


@pytest.mark.parametrize("grid", [
    pd.DataFrame(), pd.DataFrame([[np.nan]]), pd.DataFrame([[np.inf, -np.inf]]),
])
def test_missing_observations_never_become_nominal_wall(grid):
    char = characterize_pit_field(grid, 0.5)
    assert char.total_valid_cells == 0
    assert char.pit_count == 0
    assert math.isnan(char.t_min_pit_in)
    assert math.isnan(char.t_avg_pit_in)
    assert math.isnan(char.background_median_in)


def test_missing_background_population_has_no_background_median():
    char = characterize_pit_field(pd.DataFrame([[0.3] * 2] * 2), 0.5)
    assert char.pitted_cell_count == char.total_valid_cells == 4
    assert math.isnan(char.background_median_in)


@pytest.mark.parametrize("wall,minimum,fca", [
    (0.5, 0.35, 0.0), (0.46, 0.47, 0.0), (0.46, 0.38, 0.1),
])
def test_no_pit_population_cannot_pass_a_pitting_screen(wall, minimum, fca):
    char = characterize_pit_field(pd.DataFrame([[wall] * 2] * 2), 0.5)
    result = screen_pitting_level1(char, t_min_in=minimum, fca_in=fca)
    assert result["verdict"] == "NOT_EVALUATED"
    assert result["average_criterion_pass"] is None
    assert result["deep_pit_criterion_pass"] is None
    assert math.isnan(result["rt_deepest_pit"])
    assert result["t_avg_pit_in"] == pytest.approx(wall)
    assert "no pit population" in result["basis"].lower()


@pytest.mark.parametrize("wall", [0.46, np.nan])
def test_absent_pits_have_no_strength_ratio_or_folias_factor(wall):
    char = characterize_pit_field(pd.DataFrame([[wall] * 2] * 2), 0.5)
    result = _level2(char)
    assert result["verdict"] == "NOT_EVALUATED"
    assert math.isnan(result["rsf"])
    assert math.isnan(result["rt"])
    assert math.isnan(result["folias_factor"])
    assert result["equivalent_lta"] is None
    assert "no pit population" in result["assessment_basis"].lower()
    if math.isfinite(wall):
        assert result["t_mm_in"] == pytest.approx(wall)
        assert result["t_am_in"] == pytest.approx(wall)
    else:
        assert math.isnan(result["t_mm_in"])
        assert math.isnan(result["t_am_in"])


@pytest.mark.parametrize("fca", [0.0, 0.1])
def test_convenience_api_never_assigns_strength_to_pit_free_wall(fca):
    result = assess_pitting(
        pd.DataFrame([[0.46] * 2] * 2), nominal_od_in=24.0,
        nominal_wt_in=0.5, t_min_in=0.47, fca_in=fca,
    )
    assert result["characterization"].t_min_pit_in == pytest.approx(0.46)
    assert result["level1"]["verdict"] == "NOT_EVALUATED"
    assert result["level2"]["verdict"] == "NOT_EVALUATED"
    assert math.isnan(result["level2"]["rsf"])


@pytest.mark.parametrize("pitch", [1.0, 10.0])
@pytest.mark.parametrize("pitted_rows", [1, 2, 4])
def test_equivalent_lta_preserves_physical_length_for_any_row_count(pitch, pitted_rows):
    rows = [[0.5] * 3] + [[0.3, 0.5, 0.3]] * pitted_rows + [[0.5] * 3]
    grid = pd.DataFrame(rows, index=np.arange(len(rows)) * pitch)
    char = characterize_pit_field(grid, 0.5)
    result = _level2(char)
    assert char.pitted_region_row_count == pitted_rows
    assert result["flaw_length_in"] == pytest.approx(pitted_rows * pitch)
    assert result["equivalent_lta"]["axial_extent_in"] == pytest.approx(pitted_rows * pitch)


def test_explicit_singleton_pitch_controls_extent_and_pit_spacing():
    char = characterize_pit_field(pd.DataFrame([[0.3, 0.5, 0.3]]), 0.5,
                                  row_spacing_in=10.0)
    assert char.row_spacing_in == 10.0
    assert char.pitted_region_length_in == 10.0
    assert char.pit_spacing_min_in == 20.0
    assert char.to_dict()["row_spacing_source"] == "explicit"
    result = _level2(char)
    assert result["flaw_length_in"] == 10.0
    assert result["equivalent_lta"]["row_spacing_source"] == "explicit"


def test_explicit_pitch_takes_precedence_over_grid_labels():
    char = characterize_pit_field(pd.DataFrame([[0.3], [0.3]], index=[0, 1]),
                                  0.5, row_spacing_in=4.0)
    assert char.row_spacing_in == 4.0
    assert char.pitted_region_length_in == 8.0
    assert _level2(char)["flaw_length_in"] == 8.0


def test_convenience_api_preserves_explicit_singleton_pitch():
    result = assess_pitting(
        pd.DataFrame([[0.3, 0.5, 0.3]]), nominal_od_in=24.0,
        nominal_wt_in=0.5, t_min_in=0.35, row_spacing_in=10.0,
    )
    assert result["level2"]["flaw_length_in"] == 10.0
    assert result["characterization"].row_spacing_source == "explicit"


def test_multirow_index_spacing_records_its_source():
    char = characterize_pit_field(pd.DataFrame([[0.3], [0.3]], index=[0, 4]), 0.5)
    assert char.row_spacing_in == 4.0
    assert char.row_spacing_source == "index"


def test_nonuniform_index_cannot_be_presented_as_one_measured_pitch():
    char = characterize_pit_field(
        pd.DataFrame([[0.3, 0.5], [0.5, 0.5], [0.3, 0.5]], index=[0, 1, 100]),
        0.5,
    )
    assert char.row_spacing_source == "assumed_unit_spacing"
    assert "pitting.row_spacing_assumed" in _level2(char)["applicability"].flags


def test_singleton_unit_spacing_is_visible_and_blocks_a_passing_disposition():
    char = characterize_pit_field(pd.DataFrame([[0.3, 0.5, 0.3]]), 0.5)
    assert char.row_spacing_in == 1.0
    assert char.row_spacing_source == "assumed_unit_spacing"
    result = _level2(char)
    assert not result["applicability"].ok
    assert "pitting.row_spacing_assumed" in result["applicability"].flags
    assert result["equivalent_lta"]["row_spacing_source"] == "assumed_unit_spacing"


@pytest.mark.parametrize("index", [["a", "b"], [0, 0], [2, 1], [0, np.inf]])
def test_unusable_index_spacing_remains_an_assumption(index):
    char = characterize_pit_field(pd.DataFrame([[0.3], [0.3]], index=index), 0.5)
    assert char.row_spacing_in == 1.0
    assert char.row_spacing_source == "assumed_unit_spacing"
    assert "pitting.row_spacing_assumed" in _level2(char)["applicability"].flags


@pytest.mark.parametrize("pitch", [0.0, -1.0, np.nan, np.inf, True, "10"])
def test_explicit_pitch_requires_a_positive_finite_number(pitch):
    with pytest.raises(ValueError, match="row_spacing_in"):
        characterize_pit_field(pd.DataFrame([[0.3, 0.5]]), 0.5,
                               row_spacing_in=pitch)
