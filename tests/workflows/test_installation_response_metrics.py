"""Engineering response metrics: synthetic fixtures, no solver required."""
import json

import pytest

from digitalmodel.workflows.installation_response_metrics import (
    classify_case, summarize_grid, tension_event_metrics,
)


def test_equality_is_low_tension_but_not_compression():
    result = tension_event_metrics([0, 1, 2], [0, 0, 0], units="kN")
    assert result["compression"]["total_duration_s"] == 0
    assert result["low_tension"]["total_duration_s"] == 2
    assert result["low_tension"]["events"][0]["right_censored"]
    json.dumps(result, allow_nan=False)


def test_interpolated_crossings_and_post_exit_peak():
    result = tension_event_metrics([0, 1, 2, 3], [2, -2, 2, 6], units="kN")
    event = result["compression"]["events"][0]
    assert event["start_s"] == pytest.approx(0.5)
    assert event["end_s"] == pytest.approx(1.5)
    assert event["duration_s"] == pytest.approx(1)
    assert event["retension_peak"] == 6
    assert event["retension_window_censored"]
    assert result["units"] == "kN"


def test_negative_all_is_censored_without_retension():
    result = tension_event_metrics([0, 1, 2], [-2, -1, -3], units="kN")
    event = result["compression"]["events"][0]
    assert event["left_censored"] and event["right_censored"]
    assert event["retension_peak"] is None
    assert result["compression"]["total_duration_s"] == 2


def test_zero_touch_separates_strict_compression_events():
    result = tension_event_metrics([0, 1, 2], [-1, 0, -1], units="kN")
    assert result["compression"]["event_count"] == 2
    assert result["low_tension"]["event_count"] == 1


def test_retension_window_interpolates_last_endpoint():
    result = tension_event_metrics([0, 1, 2, 3], [1, -1, 1, 3], units="kN",
                                   retension_window_s=0.75)
    event = result["compression"]["events"][0]
    assert event["retension_peak"] == 1.5
    assert event["retension_peak_time_s"] == 2.25
    assert not event["retension_window_censored"]


def test_no_events_and_nonzero_threshold():
    assert tension_event_metrics([0, 1], [2, 3], units="N")["low_tension"]["events"] == []
    result = tension_event_metrics([0, 1, 2], [2, 1, 2], units="N", near_zero_threshold=1.5)
    assert result["low_tension"]["total_duration_s"] == 1
    assert result["low_tension"]["threshold"] == 1.5


@pytest.mark.parametrize("times, values", [
    ([0, 1, 2], [0, float("nan"), 0]), ([0, 1, 3], [1, 2, 3]),
    ([0, 0, 1], [1, 2, 3]), ([0, 1], [1]), ([0], [1]),
])
def test_invalid_signals_fail_closed(times, values):
    with pytest.raises(ValueError):
        tension_event_metrics(times, values, units="kN")


@pytest.mark.parametrize("kwargs", [{"units": ""}, {"units": "kN", "near_zero_threshold": -1},
                                  {"units": "kN", "retension_window_s": 0}])
def test_invalid_metadata(kwargs):
    with pytest.raises(ValueError):
        tension_event_metrics([0, 1], [1, 2], **kwargs)


def test_classification_missing_limits_and_equality():
    metric = {"peak": {"value": 10, "units": "kN"}}
    limit = {"peak": {"maximum": 10, "units": "kN"}}
    assert classify_case(metric, {})["status"] == "NOT EVALUATED"
    assert classify_case(metric, limit)["status"] == "PASS"
    assert classify_case({"peak": {"value": 11, "units": "kN"}}, limit)["status"] == "FAIL"
    assert classify_case({}, limit)["status"] == "NOT EVALUATED"
    assert classify_case(metric, limit, run_status="failed")["status"] == "FAILED"
    assert classify_case(metric, limit, run_status="missing")["status"] == "MISSING"
    assert classify_case(metric, {"peak": {"maximum": 10, "units": "N"}})["status"] == "NOT EVALUATED"


def test_grid_all_pass_is_upper_censored():
    result = summarize_grid([{"hs_m": h, "tp_s": 8, "status": "PASS"} for h in [0.25, 0.5]])
    row = result["rows"][0]
    assert row["upper_censored"]
    assert row["first_pass_band_upper_hs_m"] == 0.5
    assert row["boundary_bracket_hs_m"] is None


def test_grid_running_cell_is_incomplete_not_failure():
    result = summarize_grid([{"hs_m": .25, "tp_s": 8, "status": "RUNNING"}])
    row = result['rows'][0]
    assert row['statuses'] == ['RUNNING']
    assert row['incomplete']
    assert row['first_pass_band_upper_hs_m'] is None


def test_grid_disconnected_nonmonotonic_and_missing():
    cases = [{"hs_m": h, "tp_s": 8, "status": s} for h, s in
             [(0.25, "PASS"), (0.5, "FAIL"), (0.75, "PASS")]]
    cases.append({"hs_m": 0.25, "tp_s": 9, "status": "NOT EVALUATED"})
    result = summarize_grid(cases)
    first, second = result["rows"]
    assert first["nonmonotonic"] and first["disconnected"]
    assert first["boundary_bracket_hs_m"] == [0.25, 0.5]
    assert second["statuses"] == ["NOT EVALUATED", "MISSING", "MISSING"]
    assert second["first_pass_band_upper_hs_m"] is None
