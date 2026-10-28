"""Wall treatment policy: synthetic examples with independent arithmetic.

Range source: Notes on Computational Fluid Dynamics: General Principles
(2022), section 7.5, typical log-law applicability: 30 < y+ < 500.
The samples are chosen test inputs, not experimental validation data.
"""

import math

import pytest

from digitalmodel.solvers.openfoam.wall_treatment import (
    YPlusEvidence,
    assess_wall_treatment,
)


def test_face_count_summary_and_selection():
    """Mean=(0+20+40+100)/4=40; two of four faces below 30 => 0.5."""
    evidence = YPlusEvidence((0, 20, 40, 100), source="synthetic snapshot")
    result = assess_wall_treatment(achieved=evidence)
    summary = result["achieved_y_plus"]
    assert summary["min"] == 0
    assert summary["max"] == 100
    assert summary["mean"] == 40
    assert summary["fraction_below_30"] == 0.5
    assert summary["weighting"] == "face_count"
    assert summary["source"] == "synthetic snapshot"
    assert result["selected"] == "nutUSpaldingWallFunction"
    assert result["achieved_check"] == "PASS"


def test_area_weighted_summary():
    """Areas 3,1: mean=(20*3+100)/4=40; below-30 area=3/4=0.75."""
    evidence = YPlusEvidence((20, 100), source="synthetic", face_areas=(3, 1))
    summary = evidence.summary()
    assert summary["mean"] == 40
    assert summary["fraction_below_30"] == 0.75
    assert summary["weighting"] == "face_area"
    assert summary["face_count"] == 2


@pytest.mark.parametrize("values", [(31, 100, 499), (31,)])
def test_only_achieved_log_layer_samples_select_nutk(values):
    """Every sample strictly inside the cited 30-to-500 range."""
    result = assess_wall_treatment(achieved=YPlusEvidence(values, source="test"))
    assert result["selected"] == "nutkWallFunction"
    assert result["achieved_check"] == "PASS"


@pytest.mark.parametrize("values", [(30,), (500,), (1, 100), (100, 501)])
def test_boundaries_and_outliers_select_continuous_profile(values):
    """Strict inequalities in section 7.5 exclude both boundary values."""
    result = assess_wall_treatment(achieved=YPlusEvidence(values, source="test"))
    assert result["selected"] == "nutUSpaldingWallFunction"


def test_no_achieved_evidence_is_not_evaluated_even_with_a_prediction():
    result = assess_wall_treatment(
        predicted=YPlusEvidence((100,), source="prediction")
    )
    assert result["selected"] == "nutUSpaldingWallFunction"
    assert result["achieved_y_plus"] is None
    assert result["achieved_check"] == "NOT_EVALUATED"
    assert result["predicted_y_plus"]["mean"] == 100


def test_explicit_inconsistent_nutk_is_recorded_and_warned():
    result = assess_wall_treatment(
        requested="nutkWallFunction",
        achieved=YPlusEvidence((20, 100), source="measured"),
        predicted=YPlusEvidence((10,), source="predicted"),
    )
    assert result["selected"] == "nutkWallFunction"
    assert result["achieved_check"] == "FAIL"
    assert result["selection_basis"] == "explicit"
    assert result["predicted_check"] == "FAIL"
    assert len(result["warnings"]) == 2


def test_prediction_cannot_override_achieved_evidence():
    result = assess_wall_treatment(
        achieved=YPlusEvidence((100,), source="achieved"),
        predicted=YPlusEvidence((20,), source="predicted"),
    )
    assert result["selected"] == "nutkWallFunction"
    assert result["achieved_check"] == "PASS"
    assert result["predicted_check"] == "FAIL"


@pytest.mark.parametrize("values", [(), (-1,), (math.nan,), (math.inf,)])
def test_reject_invalid_samples(values):
    with pytest.raises(ValueError):
        YPlusEvidence(values, source="test")


@pytest.mark.parametrize("areas", [(1,), (0, 1), (-1, 1), (math.inf, 1)])
def test_reject_invalid_areas(areas):
    with pytest.raises(ValueError):
        YPlusEvidence((20, 100), source="test", face_areas=areas)


def test_reject_missing_source_and_unsupported_function():
    with pytest.raises(ValueError, match="source"):
        YPlusEvidence((100,), source=" ")
    with pytest.raises(ValueError, match="wall function"):
        assess_wall_treatment(requested="unknown")


def test_evidence_copies_mutable_inputs():
    samples, areas = [20, 100], [3, 1]
    evidence = YPlusEvidence(samples, source="test", face_areas=areas)
    samples[0], areas[0] = 100, 1
    assert evidence.summary()["fraction_below_30"] == 0.75
