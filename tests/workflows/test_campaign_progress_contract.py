"""Software contract examples from issue 2298, proposed contract items 2–4.

Values are synthetic unitless counters and clock readings, not solver data.
Each numerical expectation is derived below rather than from production code.
"""
from copy import deepcopy

import pytest

from digitalmodel.workflows import campaign_state as state


def test_repeated_poll_does_not_refresh_progress_age():
    """Counter 4→5 at t=20; at t=31 age=31−20=11 exceeds input limit 10."""
    evidence = state.observe_progress({}, counter=4, now=10)
    assert evidence == {"progress_counter": 4, "progress_counter_at": None,
                        "progress_observed_at": 10}
    evidence = state.observe_progress(evidence, counter=5, now=20)
    evidence["process_alive"] = True
    original = deepcopy(evidence)
    polled = state.observe_progress(evidence, counter=5, now=30)
    assert evidence == original
    assert polled["progress_counter_at"] == 20
    assert state.classify_lane(polled, now=30, stall_after=10) == state.LaneState.SOLVING
    assert state.classify_lane(polled, now=31, stall_after=10) == state.LaneState.STALLED


def test_first_sample_is_not_evidence_of_advancement():
    """A single sample has no earlier counter for comparison (issue 2298 §2)."""
    evidence = state.observe_progress({"process_alive": True}, counter=4, now=10)
    assert state.classify_lane(evidence, 10, 10) == state.LaneState.STALLED


def test_counter_reset_requires_a_new_advance():
    """5→1 is a reset; 1→2 at t=30 is the first observed advance afterward."""
    evidence = {"progress_counter": 5, "progress_counter_at": 10, "process_alive": True}
    reset = state.observe_progress(evidence, counter=1, now=20)
    assert reset["progress_counter_at"] is None
    assert state.classify_lane(reset, 20, 10) == state.LaneState.STALLED
    advanced = state.observe_progress(reset, counter=2, now=30)
    assert advanced["progress_counter_at"] == 30
    assert state.classify_lane(advanced, 30, 10) == state.LaneState.SOLVING


@pytest.mark.parametrize("bad", [float("nan"), float("inf"), -1, True, "10", None])
@pytest.mark.parametrize("field", ["counter", "now"])
def test_observation_rejects_invalid_samples(field, bad):
    """Counters and timestamps require finite, nonnegative numbers, not flags."""
    kwargs = {"counter": 4, "now": 10, field: bad}
    with pytest.raises(ValueError, match=field):
        state.observe_progress({}, **kwargs)


def test_observation_rejects_reversed_clock():
    """Observation t=19 precedes the recorded advance t=20."""
    with pytest.raises(ValueError, match="time"):
        state.observe_progress({"progress_counter": 4, "progress_counter_at": 20},
                               counter=5, now=19)


@pytest.mark.parametrize("samples", [
    ((4, 100),),
    ((4, 10), (5, 20), (5, 100)),
    ((4, 10), (5, 20), (1, 100)),
], ids=["first-sample", "unchanged-poll", "counter-reset"])
@pytest.mark.parametrize("counter_change", [-1, 0, 1],
                         ids=["reset", "unchanged", "advance"])
def test_observation_rejects_reversal_after_latest_sample(samples, counter_change):
    """Every history ends at t=100; t=25 is rejected for every counter change."""
    evidence = {"process_alive": True}
    for counter, now in samples:
        evidence = state.observe_progress(evidence, counter=counter, now=now)
    original = deepcopy(evidence)
    assert state.classify_lane(evidence, 100, 10) == state.LaneState.STALLED
    with pytest.raises(ValueError, match="time"):
        state.observe_progress(evidence, counter=samples[-1][0] + counter_change,
                               now=25)
    assert evidence == original
    assert state.classify_lane(evidence, 100, 10) == state.LaneState.STALLED


def test_equal_observation_time_preserves_advancement_age():
    """At t=100 age=100-20=80; another t=100 poll cannot reduce that age."""
    evidence = state.observe_progress({"process_alive": True}, counter=4, now=10)
    evidence = state.observe_progress(evidence, counter=5, now=20)
    evidence = state.observe_progress(evidence, counter=5, now=100)
    polled = state.observe_progress(evidence, counter=5, now=100)
    assert polled["progress_counter_at"] == 20
    assert state.classify_lane(polled, 100, 10) == state.LaneState.STALLED


@pytest.mark.parametrize("bad", [float("nan"), float("inf"), -1, True, "10", None])
@pytest.mark.parametrize("field", ["progress_counter", "progress_counter_at"])
def test_invalid_progress_evidence_never_reports_solving(field, bad):
    """Invalid evidence cannot establish advancing native output (issue 2298 §2)."""
    evidence = {"process_alive": True, "progress_counter": 4, "progress_counter_at": 10,
                field: bad}
    assert state.classify_lane(evidence, 10, 10) == state.LaneState.STALLED


@pytest.mark.parametrize("bad", [float("nan"), float("inf"), -1, True, "10", None])
@pytest.mark.parametrize("field", ["now", "stall_after"])
def test_health_requires_valid_clock_and_explicit_limit(field, bad):
    """A bad caller-supplied age limit must not silently qualify output."""
    kwargs = {"now": 10, "stall_after": 10, field: bad}
    with pytest.raises(ValueError, match=field):
        state.classify_lane({}, **kwargs)


@pytest.mark.parametrize("bad", [None, "", " ", True, 1])
@pytest.mark.parametrize("field", ["criteria_id", "criteria_version"])
def test_governing_criteria_must_be_nonempty_strings(field, bad):
    """Unknown governing criteria cannot qualify matching unknown rows (§3)."""
    kwargs = {"criteria_id": "acceptance", "criteria_version": "v1", field: bad}
    with pytest.raises(ValueError, match=field):
        state.check_criteria([], **kwargs)


def test_campaign_status_checks_criteria_and_names_next_action():
    """3 cases: done+failed=2 attempted, 1 complete/qualified/reported (§4)."""
    cases = [dict(id="a", status="completed", qualified=True),
             dict(id="b", status="failed"), dict(id="c", status="pending")]
    for row in cases:
        row.update(criteria_id="acceptance", criteria_version="v1")
    original = deepcopy(cases)
    assert state.campaign_status(iter(cases), {"a"}, criteria_id="acceptance",
                                 criteria_version="v1", next_action="Run c") == (
        "native 2/3 attempted, 1 completed, 1 qualified; report 1/1 complete; next: Run c"
    )
    assert cases == original


def test_campaign_status_rejects_stale_pending_rows():
    """A pending row is also a consumer of the launch criteria (§3)."""
    cases = [dict(id="pending", status="pending", criteria_id="acceptance",
                  criteria_version="v0")]
    with pytest.raises(state.CriteriaVersionMismatch, match="pending"):
        state.campaign_status(cases, set(), criteria_id="acceptance",
                              criteria_version="v1", next_action="Run pending")


@pytest.mark.parametrize("bad", [None, "", " ", True])
def test_campaign_status_requires_next_action(bad):
    """The next executable action is required by issue 2298 §4."""
    with pytest.raises(ValueError, match="next_action"):
        state.campaign_status([], set(), criteria_id="acceptance",
                              criteria_version="v1", next_action=bad)
