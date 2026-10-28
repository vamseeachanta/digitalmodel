# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Mixed synthetic evidence survives diagnostic matrix transitions; no solves."""

from copy import deepcopy

from digitalmodel.ansys.analysis_evidence import build_package, load_package
from digitalmodel.ansys.analysis_matrix_publish import publish_matrix
from digitalmodel.ansys.analysis_records import canonical_bytes
from tests.ansys.analysis_replay_fixture import full_fixture
from tests.ansys.test_analysis_replay_publish import prepared, publication_paths


def test_mixed_historical_states_survive_replay_and_matrix_publication(tmp_path, monkeypatch):
    """Independent fixture arithmetic: six rows split as 2 failed + 2 computed
    + 2 not evaluated. Diagnostic replay replaces 64 failed target rows only;
    the historical 2 failures remain, and 3 unattempted cases remain pending.
    Computed zero is a synthetic identity value, never an engineering result.
    """
    data = full_fixture(tmp_path, monkeypatch)
    baseline = deepcopy(data["baseline"])
    baseline.pop("package_hash")
    for case in baseline["cases"]:
        case.pop("row_hash")
    states = ["failed", "computed", "not_evaluated"] * 2
    historical = baseline["cases"][0]
    for response, state in zip(historical["responses"], states, strict=True):
        response.update(
            calculation_status=state,
            value="0" if state == "computed" else None,
            limitations=["synthetic-history"],
        )
    # Case-level incompleteness is independent of individual computed responses.
    historical["assessment_status"] = "incomplete"
    data["baseline"] = build_package(baseline, data["resolver"])
    before = [canonical_bytes(case) for case in data["baseline"]["cases"]]
    replay = prepared(data)
    assert replay["coverage"]["assessment_failed_responses"] == 2
    assert replay["coverage"]["assessment_incomplete_cases"] == 1
    assert replay["coverage"]["pending_cases"] == 3
    assert replay["coverage"]["qualified_responses"] == 0

    evidence_root, manifest = publication_paths(data)
    revision = publish_matrix(
        replay, data["baseline"], manifest, evidence_root, data["resolver"],
        review_sha256=data["review_sha256"],
    )
    published = load_package(manifest)
    assert load_package(revision) == published == replay
    for index, case in enumerate(published["cases"]):
        if index != 8:
            assert canonical_bytes(case) == before[index]
    assert [r["calculation_status"] for r in published["cases"][0]["responses"]] == states
    assert published["cases"][8]["engineering_qualified"] is False
