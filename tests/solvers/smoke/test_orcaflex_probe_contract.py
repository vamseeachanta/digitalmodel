"""Numerical, saved-state and bounded-resource smoke contracts (issue 2082)."""

import gc
import json
import sys

import pytest

from digitalmodel.solvers.smoke.probes import check_orcaflex


def assert_failure(result):
    assert result["ok"] is False
    assert result["solver"] == "orcaflex"
    assert result.get("stage")
    assert result.get("error")
    json.dumps(result, allow_nan=False)


def test_success_requires_independent_readback_and_proof_fields(tmp_path, fake_api):
    result = check_orcaflex(tmp_path)
    assert result["ok"] is True
    assert ("simulation", "smoke.sim") in fake_api.loads
    assert result["thread_count_requested"] == 1
    assert result["thread_counts_observed"] == {
        "solve": 1, "data_reader": 1, "simulation_reader": 1,
    }
    for field in ("static_finite", "dynamic_finite", "simulation_complete",
                  "saved_sim_reloaded", "reloaded_simulation_complete",
                  "reloaded_dynamic_finite"):
        assert result[field] is True
    assert result["reloaded_dynamic_samples"] == result["dynamic_samples"] == 3
    assert result["sim_bytes"] > 0
    assert result["dll_version"] == "FAKE-NO-LICENCE"
    json.dumps(result, allow_nan=False)


@pytest.mark.parametrize("value", [float("nan"), float("inf"), -float("inf")])
def test_nonfinite_static_refuses_without_nonstandard_json(tmp_path, fake_api, value):
    fake_api.static = value
    assert_failure(check_orcaflex(tmp_path))


@pytest.mark.parametrize("history", [[], [1.0], [1.0, float("nan"), 2.0],
                                      [1.0, float("inf"), 2.0],
                                      [1.0, -float("inf"), 2.0]])
def test_invalid_dynamic_history_refuses(tmp_path, fake_api, history):
    fake_api.history = history
    assert_failure(check_orcaflex(tmp_path))


@pytest.mark.parametrize("state,complete", [
    ("SimulationStopped", False), ("RunningSimulation", True),
    ("SimulationPaused", True),
])
def test_incomplete_or_wrong_solve_state_refuses(tmp_path, fake_api, state, complete):
    fake_api.state, fake_api.complete = state, complete
    assert_failure(check_orcaflex(tmp_path))


@pytest.mark.parametrize("kind", ["missing", "empty", "corrupt"])
def test_invalid_saved_simulation_refuses(tmp_path, fake_api, kind):
    fake_api.sim_file = kind
    assert_failure(check_orcaflex(tmp_path))


@pytest.mark.parametrize("history", [[], [1.0], [1.0, 2.0],
                                      [1.0, float("nan"), 3.0],
                                      [1.0, float("inf"), 3.0],
                                      [1.0, -float("inf"), 3.0]])
def test_readback_is_validated_independently(tmp_path, fake_api, history):
    fake_api.reader_history = history
    assert_failure(check_orcaflex(tmp_path))
    assert ("simulation", "smoke.sim") in fake_api.loads


@pytest.mark.parametrize("state,complete", [
    ("SimulationStopped", False), ("RunningSimulation", True),
    ("SimulationPaused", True),
])
def test_readback_state_and_completion_refuse(tmp_path, fake_api, state, complete):
    fake_api.reader_state, fake_api.reader_complete = state, complete
    assert_failure(check_orcaflex(tmp_path))


def test_readback_requires_expected_line(tmp_path, fake_api):
    fake_api.missing_line = True
    assert_failure(check_orcaflex(tmp_path))


@pytest.mark.parametrize("role", ["solve", "data_reader", "simulation_reader"])
def test_observed_thread_count_after_solve_or_load_refuses(tmp_path, fake_api, role):
    fake_api.observed_threads[role] = 8
    assert_failure(check_orcaflex(tmp_path))


@pytest.mark.parametrize("failure_attempt", [None, 1, 2, 3])
def test_every_constructor_attempt_has_explicit_one_thread(tmp_path, fake_api, failure_attempt):
    fake_api.fail_constructor = failure_attempt
    result = check_orcaflex(tmp_path)
    assert fake_api.constructors
    assert all(call["threadCount"] == 1 for call in fake_api.constructors)
    if failure_attempt:
        assert_failure(result)
        assert len(fake_api.constructors) == failure_attempt
        if failure_attempt == 1:
            assert not result.get("thread_counts_observed")
    else:
        assert result["ok"] is True
        assert len(fake_api.constructors) == 3


@pytest.mark.parametrize("operation", ["statics", "dynamics", "save_data", "load_data",
                                        "save_sim", "load_sim"])
def test_api_errors_remain_structured_failures(tmp_path, fake_api, operation):
    fake_api.fail_operation = operation
    assert_failure(check_orcaflex(tmp_path))


def test_missing_api_is_structured_and_has_no_fabricated_observations(tmp_path, monkeypatch):
    monkeypatch.setitem(sys.modules, "OrcFxAPI", None)
    result = check_orcaflex(tmp_path)
    assert_failure(result)
    assert result["stage"] == "import"
    assert not result.get("thread_counts_observed")


@pytest.mark.parametrize("operation", [None, "dynamics", "load_sim"])
def test_native_handles_not_retained_after_return(tmp_path, fake_api, operation):
    fake_api.fail_operation = operation
    result = check_orcaflex(tmp_path)
    gc.collect()
    assert fake_api.references
    assert all(reference() is None for reference in fake_api.references)
    json.dumps(result, allow_nan=False)
