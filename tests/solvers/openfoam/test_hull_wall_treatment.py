"""Builder contracts for achieved y+ evidence, without executing a solver.

Synthetic samples and expected statistics follow test_wall_treatment's hand
calculations. Range source: Notes on Computational Fluid Dynamics: General
Principles (2022), section 7.5: 30 < y+ < 500.
"""

import json

import pytest

from digitalmodel.solvers.openfoam.hull_case import HullCaseConfig, build_hull_case
from digitalmodel.solvers.openfoam.hull_double_body import (
    DoubleBodyCaseConfig,
    build_double_body_case,
)
from digitalmodel.solvers.openfoam.hull_manifest import HullManifest
from digitalmodel.solvers.openfoam.wall_treatment import YPlusEvidence


@pytest.fixture(params=["free_surface", "double_body"])
def builder(request, manifest_dict, stl_file):
    config_type, build = (
        (HullCaseConfig, build_hull_case)
        if request.param == "free_surface"
        else (DoubleBodyCaseConfig, build_double_body_case)
    )
    config = config_type(
        manifest=HullManifest.from_dict(manifest_dict),
        stl_path=stl_file,
        velocity=2.0,  # Synthetic builder condition, not an acceptance limit.
        ranks=1,
    )
    return config, build


@pytest.mark.parametrize(
    ("samples", "selected", "check"),
    [
        (None, "nutUSpaldingWallFunction", "NOT_EVALUATED"),
        ((20, 100), "nutUSpaldingWallFunction", "PASS"),
        ((31, 100), "nutkWallFunction", "PASS"),
    ],
)
def test_written_field_and_provenance_agree(builder, tmp_path, samples, selected, check):
    config, build = builder
    if samples is not None:
        config = config.replace(
            achieved_y_plus=YPlusEvidence(samples, source="synthetic snapshot")
        )
    case = build(config, tmp_path)
    nut = (case / "0.orig/nut").read_text()
    assert f"type            {selected};" in nut
    record = json.loads((case / "case_provenance.json").read_text())["wall_treatment"]
    assert record["selected"] == selected
    assert record["achieved_check"] == check
    if samples is not None:
        assert record["achieved_y_plus"]["min"] == min(samples)
        assert record["achieved_y_plus"]["max"] == max(samples)


def test_predicted_conflict_emits_warning_and_persists_evidence(builder, tmp_path):
    """Prediction mean=(20+100)/2=60; fraction below 30=1/2=0.5."""
    config, build = builder
    with pytest.warns(UserWarning, match="Predicted y\\+"):
        config = config.replace(
            nut_wall_function="nutkWallFunction",
            predicted_y_plus=YPlusEvidence((20, 100), source="synthetic prediction"),
        )
    case = build(config, tmp_path)
    record = json.loads((case / "case_provenance.json").read_text())["wall_treatment"]
    assert record["selected"] == "nutkWallFunction"
    assert record["predicted_check"] == "FAIL"
    assert record["achieved_check"] == "NOT_EVALUATED"
    assert record["predicted_y_plus"]["mean"] == 60
    assert record["predicted_y_plus"]["fraction_below_30"] == 0.5
    assert record["warnings"]


def test_target_is_not_achieved_evidence(builder):
    config, _ = builder
    # Deliberately change the design target; no achieved samples were supplied.
    result = config.replace(y_plus_target=100).wall_treatment
    assert result["selected"] == "nutUSpaldingWallFunction"
    assert result["achieved_y_plus"] is None
    assert result["achieved_check"] == "NOT_EVALUATED"


def test_invalid_choice_is_rejected_at_configuration(builder):
    config, _ = builder
    with pytest.raises(ValueError, match="wall function"):
        config.replace(nut_wall_function="unsupported")
