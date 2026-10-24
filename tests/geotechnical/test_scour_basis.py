# ABOUTME: Independent sediment/depth thresholds and guarded scour assessments.
# ABOUTME: Synthetic arithmetic examples are not design data or acceptance limits.
"""Basis: FHWA-RD-03-052 (2005), chapter 5, equation 3 (SI).

All inputs and comparison limits in this file are synthetic, selected for
hand arithmetic. No field validation or standard compliance is asserted.
"""

import json
import math
from dataclasses import asdict
from pathlib import Path

import pytest
import yaml

from digitalmodel.geotechnical import scour
from digitalmodel.geotechnical.cfg_workflows import ScourWorkflow


def test_critical_current_hand_calculation():
    """FHWA eq. 3: 31.08*sqrt(.04)*64**(1/6)*.001**(1/3).

    sqrt(.04)=.2, 64**(1/6)=2, .001**(1/3)=.1;
    therefore Ucrit=31.08*.2*2*.1=1.2432 m/s.
    This is a mean current velocity, not the Shields shear velocity.
    """
    assert scour.critical_current_velocity(
        sediment_d50_mm=1.0,
        water_depth_m=64.0,
        critical_shields_parameter=0.04,
    ) == pytest.approx(1.2432, rel=1e-12)


@pytest.mark.parametrize(
    "depth, diameter, expected",
    [
        (1.0, 1.0, 0.6216),  # 31.08*.2*1*.1
        (64.0, 8.0, 2.4864),  # 31.08*.2*2*.2
    ],
)
def test_depth_and_grain_size_change_critical_current(depth, diameter, expected):
    """The sixth-root depth and cube-root grain terms follow FHWA eq. 3."""
    assert scour.critical_current_velocity(
        sediment_d50_mm=diameter,
        water_depth_m=depth,
        critical_shields_parameter=0.04,
    ) == pytest.approx(expected, rel=1e-12)


@pytest.mark.parametrize(
    "name",
    [
        "sediment_d50_mm",
        "water_depth_m",
        "critical_shields_parameter",
    ],
)
@pytest.mark.parametrize("value", [0.0, -1.0, math.nan, math.inf, -math.inf])
def test_invalid_critical_current_inputs(name, value):
    values = dict(
        sediment_d50_mm=1.0, water_depth_m=64.0, critical_shields_parameter=0.04
    )
    values[name] = value
    with pytest.raises(ValueError, match=name):
        scour.critical_current_velocity(**values)


@pytest.mark.parametrize(
    "velocity, expected",
    [
        (0.5, "BELOW_THRESHOLD"),
        pytest.param(1.2432 - 1e-9, "BELOW_THRESHOLD", id="just below the hand value"),
        pytest.param(1.2432 + 1e-9, "AT_OR_ABOVE_THRESHOLD", id="just above the hand value"),
        (2.0, "AT_OR_ABOVE_THRESHOLD"),
    ],
)
def test_monopile_motion_does_not_establish_scour_depth(velocity, expected):
    """Ucrit=1.2432 m/s from the hand calculation above; no depth model."""
    result = scour.monopile_scour_depth(
        pile_diameter_m=6.0,
        current_velocity_ms=velocity,
        water_depth_m=64.0,
        sediment_d50_mm=1.0,
        critical_shields_parameter=0.04,
    )
    assert result.critical_velocity_ms == pytest.approx(1.2432, rel=1e-12)
    assert result.sediment_motion_status == expected
    assert result.scour_depth_m is None
    assert result.scour_to_diameter_ratio is None
    assert result.status == "NOT_EVALUATED"
    assert result.standard != "DNV-RP-F107"
    assert "depth model" in result.reason


def test_pipeline_exposes_current_threshold_but_does_not_infer_wave_scour():
    """Same 1.2432 m/s hand result; KC=.2*10/.5=4 and Uc/(Uc+Uw)=.8."""
    result = scour.pipeline_scour_depth(
        pipe_od_m=0.5,
        current_velocity_ms=0.8,
        wave_orbital_velocity_ms=0.2,
        sediment_d50_mm=1.0,
        wave_period_s=10.0,
        water_depth_m=64.0,
        critical_shields_parameter=0.04,
    )
    assert result.critical_velocity_ms == pytest.approx(1.2432, rel=1e-12)
    assert result.sediment_motion_status == "BELOW_THRESHOLD"
    assert result.keulegan_carpenter == pytest.approx(4.0)
    assert result.current_wave_ratio == pytest.approx(0.8)
    assert result.scour_depth_m is None
    assert result.status == "NOT_EVALUATED"
    assert "wave" in result.reason


def test_missing_motion_inputs_are_named():
    result = scour.monopile_scour_depth(6.0, 1.0, 30.0)
    assert result.critical_velocity_ms is None
    assert result.sediment_motion_status == "NOT_EVALUATED"
    assert "sediment_d50_mm" in result.reason
    assert "critical_shields_parameter" in result.reason


def test_pile_motion_uses_actual_water_depth():
    """FHWA eq. 3 gives 0.6216 m/s at 1 m, 1.2432 m/s at 64 m.

    At the supplied current of 0.8 m/s, changing only depth crosses the
    motion threshold. Neither condition establishes a local scour depth.
    """
    shallow = scour.monopile_scour_depth(
        6.0,
        0.8,
        1.0,
        sediment_d50_mm=1.0,
        critical_shields_parameter=0.04,
    )
    deep = scour.monopile_scour_depth(
        6.0,
        0.8,
        64.0,
        sediment_d50_mm=1.0,
        critical_shields_parameter=0.04,
    )
    assert shallow.sediment_motion_status == "AT_OR_ABOVE_THRESHOLD"
    assert deep.sediment_motion_status == "BELOW_THRESHOLD"


def test_pipeline_motion_uses_actual_grain_size():
    """At 1 m depth, 1 mm and 8 mm give 0.6216 and 1.2432 m/s.

    Holding current at 1 m/s crosses the motion threshold as grain size
    increases; the current-only case does not require a wave period.
    """
    fine = scour.pipeline_scour_depth(
        0.5,
        1.0,
        0.0,
        1.0,
        water_depth_m=1.0,
        critical_shields_parameter=0.04,
    )
    coarse = scour.pipeline_scour_depth(
        0.5,
        1.0,
        0.0,
        8.0,
        water_depth_m=1.0,
        critical_shields_parameter=0.04,
    )
    assert fine.sediment_motion_status == "AT_OR_ABOVE_THRESHOLD"
    assert coarse.sediment_motion_status == "BELOW_THRESHOLD"


@pytest.mark.parametrize(
    "velocity, limit, expected",
    [
        (1.0, 1.0, "PASS"),
        (2.0, 1.0, "PASS"),
        (3.0, 1.0, "FAIL"),
    ],
)
def test_armour_comparison_against_supplied_limit(velocity, limit, expected):
    """Ns=U/sqrt(g*d*(rho_r/rho_w-1))=U/sqrt(10*.4*1)=U/2.

    The supplied synthetic limit is 1.0; equality passes the explicit <=
    comparison. Nominal thickness=3 layers*.4 m=1.2 m. These are arithmetic
    checks, not a sourced recommendation for a rock grading or layer count.
    """
    result = scour.rock_armour_thickness(
        velocity,
        2000.0,
        1000.0,
        0.4,
        gravity_ms2=10.0,
        stability_limit=limit,
        layer_count=3.0,
    )
    assert result.stability_number == pytest.approx(velocity / 2.0, rel=1e-12)
    assert result.stability_status == expected
    assert result.thickness_m == pytest.approx(1.2, rel=1e-12)
    assert result.status == "NOT_EVALUATED"  # complete protection design absent


def test_armour_missing_basis_is_not_a_pass_or_two_layer_default():
    result = scour.rock_armour_thickness(1.0, 2000.0, 1000.0, 0.4)
    assert result.stability_number is None
    assert result.stability_status == "NOT_EVALUATED"
    assert result.thickness_m is None
    assert "gravity_ms2" in result.reason
    assert "stability_limit" in result.reason
    assert "layer_count" in result.reason


def test_computed_armour_number_without_limit_is_unevaluated():
    """Ns=1/sqrt(10*.4*1)=0.5; no acceptance limit is inferred."""
    result = scour.rock_armour_thickness(
        1.0,
        2000.0,
        1000.0,
        0.4,
        gravity_ms2=10.0,
    )
    assert result.stability_number == pytest.approx(0.5)
    assert result.stability_status == "NOT_EVALUATED"
    assert result.thickness_m is None


@pytest.mark.parametrize(
    "kwargs",
    [
        {"gravity_ms2": -1.0},
        {"stability_limit": math.nan},
        {"stability_limit": 0.0},
        {"layer_count": 0.0},
    ],
)
def test_armour_rejects_invalid_optional_basis_even_if_incomplete(kwargs):
    with pytest.raises(ValueError):
        scour.rock_armour_thickness(1.0, 2000.0, 1000.0, 0.4, **kwargs)


@pytest.mark.parametrize("value", [-1.0, math.nan, math.inf, -math.inf])
@pytest.mark.parametrize("calculator", ["pipeline", "monopile", "armour"])
def test_rejects_invalid_velocity_magnitudes(calculator, value):
    with pytest.raises(ValueError, match="current_velocity_ms"):
        if calculator == "pipeline":
            scour.pipeline_scour_depth(0.5, value, 0.2, 1.0)
        elif calculator == "monopile":
            scour.monopile_scour_depth(6.0, value, 30.0)
        else:
            scour.rock_armour_thickness(value, 2000.0, 1000.0, 0.4)


def test_wave_period_is_required_only_for_nonzero_wave_diagnostics():
    result = scour.pipeline_scour_depth(0.5, 1.0, 0.2, 1.0)
    assert result.keulegan_carpenter is None
    assert "wave_period_s" in result.reason
    assert scour.pipeline_scour_depth(0.5, 1.0, 0.0, 1.0).keulegan_carpenter == 0


@pytest.mark.parametrize("diameter", [5e-324, 1e308])
def test_unrepresentable_critical_current_is_rejected(diameter):
    """Underflow on SI conversion and overflow on the combined product fail."""
    with pytest.raises(ValueError):
        scour.critical_current_velocity(
            sediment_d50_mm=diameter,
            water_depth_m=diameter,
            critical_shields_parameter=diameter,
        )


@pytest.mark.parametrize(
    "kwargs",
    [
        {"wave_orbital_velocity_ms": -1.0},
        {"wave_period_s": math.nan},
        {"sediment_d50_mm": 0.0},
        {"burial_ratio": math.inf},
        {"water_depth_m": -1.0},
        {"critical_shields_parameter": math.nan},
    ],
)
def test_pipeline_zero_flow_cannot_bypass_invalid_inputs(kwargs):
    values = dict(
        pipe_od_m=0.5,
        current_velocity_ms=0.0,
        wave_orbital_velocity_ms=0.0,
        sediment_d50_mm=1.0,
    )
    values.update(kwargs)
    with pytest.raises(ValueError):
        scour.pipeline_scour_depth(**values)


def test_large_velocity_ratio_avoids_overflow():
    """For equal finite magnitudes Uc/(Uc+Uw)=1/2 even if their sum overflows."""
    result = scour.pipeline_scour_depth(1.0, 1e308, 1e308, 1.0)
    assert result.current_wave_ratio == pytest.approx(0.5)
    assert result.status == "NOT_EVALUATED"


def test_zero_flow_is_evaluated_separately_from_missing_design_basis():
    pipeline = scour.pipeline_scour_depth(0.5, 0.0, 0.0, 1.0)
    pile = scour.monopile_scour_depth(6.0, 0.0, 30.0)
    for result in (pipeline, pile):
        assert result.scour_depth_m == 0.0
        assert result.status == "NO_FLOW"
        assert result.sediment_motion_status == "NOT_EVALUATED"
        json.dumps(asdict(result), allow_nan=False)


def test_workflow_example_and_json_readback(tmp_path):
    """Hand Ucrit=1.2432; armour Ns=3/2=1.5 exceeds supplied limit 1."""
    example = (
        Path(__file__).resolve().parents[2]
        / "examples/workflows/scour-assessment/input.yml"
    )
    cfg = yaml.safe_load(example.read_text())
    cfg["scour"]["outputs"]["directory"] = str(tmp_path)
    routed = ScourWorkflow().router(cfg)
    results = routed["scour"]["results"]
    assert results["monopile"]["critical_velocity_ms"] == pytest.approx(1.2432)
    assert results["pipeline"]["scour_depth_m"] is None
    assert results["rock_armour"]["stability_status"] == "FAIL"
    saved = json.loads(Path(routed["scour"]["summary_json"]).read_text())
    assert saved["results"] == results
    assert "DNV-RP-F107" not in json.dumps(results)


@pytest.mark.parametrize("layer_count", [2.5, 0.5, True])
def test_armour_requires_whole_layer_count(layer_count):
    with pytest.raises(ValueError, match="layer_count"):
        scour.rock_armour_thickness(
            1.0, 2000.0, 1000.0, 0.4, layer_count=layer_count
        )
