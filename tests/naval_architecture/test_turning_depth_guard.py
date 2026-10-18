"""Prevent deep-water calculations from silently accepting shallow water."""

from copy import deepcopy
import math

import pytest

from digitalmodel.naval_architecture.turning_circle import (
    simulate_nomoto_turning_circle, load_packaged_turning_circle_yaml,
    validate_turning_circle_input, run_turning_circle_sweep,
)
from digitalmodel.naval_architecture.maneuvering_envelope import imo_turning_compliance
from digitalmodel.naval_architecture.maneuvering_envelope_workflow import (
    validate_maneuvering_envelope_input, run_maneuvering_envelope_sweep,
)


TURN = dict(speed_m_s=5, rudder_angle_deg=35, K_per_s=0.12, T_s=20,
            duration_s=200, dt_s=1)


def estimate(**kwargs):
    return simulate_nomoto_turning_circle(**TURN, **kwargs)


def check_imo(**kwargs):
    return imo_turning_compliance(400, 450, 100, **kwargs)


@pytest.mark.parametrize("calculate", [estimate, check_imo])
@pytest.mark.parametrize("ratio", [1.2, 1.5, 3.0, 3.5, 4.0])
def test_refuses_depth_outside_deep_water_screen(calculate, ratio):
    with pytest.raises(ValueError, match="h/T > 4"):
        calculate(water_depth_m=ratio, draft_m=1)


@pytest.mark.parametrize("calculate", [estimate, check_imo])
@pytest.mark.parametrize("depth", [math.nextafter(4.0, math.inf), 5, math.inf])
def test_allows_depth_above_screen(calculate, depth):
    result = calculate(water_depth_m=depth, draft_m=1)
    if calculate is estimate:
        assert result["metadata"]["depth_applicability"] == "screen_passed"
        assert result["metrics"]["metric_status"] == "ok"
        assert result["warnings"] == []
    else:
        assert result.depth_applicability == "screen_passed"
        assert result.overall_pass


@pytest.mark.parametrize("calculate", [estimate, check_imo])
@pytest.mark.parametrize("depth,draft", [
    (math.nan, 10), (-math.inf, 10), (0, 10), (-1, 10),
    (50, 0), (50, -1), (50, math.nan), (50, math.inf), (math.inf, None),
])
def test_invalid_depth_inputs_raise(calculate, depth, draft):
    with pytest.raises(ValueError):
        calculate(water_depth_m=depth, draft_m=draft)


def test_unknown_depth_is_visible_in_estimator_result():
    result = estimate()
    assert result["metadata"]["depth_applicability"] == "unverified"
    assert result["metrics"]["metric_status"] == "warning"
    assert any("applicability is unverified" in w for w in result["warnings"])


def test_unknown_depth_imo_emits_warning_and_marks_verdict():
    with pytest.warns(UserWarning, match="applicability is unverified"):
        result = check_imo()
    assert result.depth_applicability == "unverified"


def test_yaml_and_sweep_do_not_drop_depth_guard():
    payload = deepcopy(load_packaged_turning_circle_yaml().raw)
    payload["environment"] = {"water_depth_m": 1.2 * payload["vessel"]["draft_m"]}
    with pytest.raises(ValueError, match="h/T > 4"):
        validate_turning_circle_input(payload)
    config = load_packaged_turning_circle_yaml()
    config.raw["environment"] = payload["environment"]
    with pytest.raises(ValueError, match="h/T > 4"):
        run_turning_circle_sweep(config)


def test_sweep_retains_unknown_depth_warning():
    result = run_turning_circle_sweep(load_packaged_turning_circle_yaml())
    assert any("applicability is unverified" in w for w in result["warnings"])
    assert all(c["depth_applicability"] == "unverified" for c in result["cases"])


def test_imo_workflow_does_not_drop_depth_guard():
    # Synthetic dimensions; no LFS data or proprietary inputs required.
    config = validate_maneuvering_envelope_input({
        "case": {"id": "synthetic"},
        "ship": {"lbp_m": 100, "beam_m": 20, "K_prime": 1},
        "rudder": {"area_m2": 20, "span_m": 5, "aspect_ratio": 1.25,
                   "x_from_cg_m": 45},
        "propulsion": {"diameter_m": 4, "kt_coeffs": [0.2],
                       "thrust_deduction": 0.1, "wake_fraction": 0.2,
                       "C_rp": 1, "ship_speed_m_s": 5, "shaft_speed_rev_s": 2},
        "environment": {"water_depth_m": 1, "wind_speed_m_s": 0, "CXYc": 0.1},
        "loading_conditions": [{"name": "synthetic", "draft_m": 5, "Cb": 0.7,
                                "rudder_effective_area_m2": 20,
                                "lateral_underwater_area_m2": 500,
                                "windage_area_m2": 500}],
        "sweep": {"current_speeds": {"units": "kn", "values": [1]}},
        "outputs": {},
    })
    with pytest.raises(ValueError, match="h/T > 4"):
        run_maneuvering_envelope_sweep(config)
