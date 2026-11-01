# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Case execution checks with independent force and equilibrium arithmetic.

All numerical inputs are synthetic. Sources for equations: OpenStax University
Physics Volume 1 (2016), 12.1 and 15.1; NASA Drag Equation (2021, updated 2025).
"""

import numpy as np
import pytest

from digitalmodel.hydrodynamics.passing_ship.analysis import (
    run_analysis_cases,
    wind_force,
)
from digitalmodel.hydrodynamics.passing_ship.input_schemas import (
    AnalysisCase,
    EnvironmentSpec,
    FenderSpec,
    OutputSpec,
    PassingShipDimensions,
    PassingShipScenario,
    PassingShipSpec,
    PassingTrack,
    WindLoadSpec,
)
from tests.hydrodynamics.passing_ship.test_mooring_response import (  # noqa: F401
    settings,
    vessel,
)


@pytest.fixture
def spec(request):
    model_vessel = request.getfixturevalue("vessel")
    response_settings = request.getfixturevalue("settings")
    model_vessel.wind_load = WindLoadSpec(
        air_density_kg_m3=1,
        frontal_area_m2=4,
        lateral_area_m2=8,
        surge_coefficient=1,
        sway_coefficient=1,
        centre_of_pressure_x_m=2,
        centre_of_pressure_y_m=1,
    )
    return PassingShipSpec(
        name="spring-example",
        moored_vessel=model_vessel,
        response_settings=response_settings,
        passing_ship=PassingShipScenario(
            dimensions=PassingShipDimensions(loa=8, beam=2, draft=1),
            track=PassingTrack(speed_knots=2, separation_m=20),
        ),
        environment=EnvironmentSpec(wind_speed=10, wind_direction_deg=0),
        analysis_cases=[
            AnalysisCase(name="still", include_passing_ship=False),
            AnalysisCase(name="wind", include_passing_ship=False, include_wind=True),
        ],
    )


def test_wind_hand_calculation_and_body_direction(spec):
    """Wind from bow: vx=-10; Fx=0.5*1*4*1*(-10)*10=-200 N.

    Moment = x*Fy-y*Fx = 2*0-1*(-200)=200 Nm.
    Wind from starboard: Fy=0.5*1*8*(-10)*10=-400 N, M=-800 Nm.
    """
    np.testing.assert_allclose(wind_force(spec), [-200, 0, 200], atol=1e-10)
    spec.environment.wind_direction_deg = 90
    np.testing.assert_allclose(wind_force(spec), [0, -400, -800], atol=1e-10)


def test_wind_schema_and_sample_use_body_relative_direction(spec):
    """270 degrees is from port: Fy=0.5*1*8*10**2=400 N."""
    from pathlib import Path

    description = EnvironmentSpec.model_fields["wind_direction_deg"].description
    assert "body-relative" in description and "bow" in description
    sample = Path(__file__).resolve().parents[3] / (
        "docs/domains/orcaflex/passing_ship/sample/spec.yml"
    )
    text = sample.read_text(encoding="utf-8")
    assert "from west" not in text
    assert "from port" in text
    spec.environment.wind_direction_deg = 270
    np.testing.assert_allclose(wind_force(spec), [0, 400, 800], atol=1e-10)


def test_passage_timescale_respects_case_speed(spec):
    """m=2000 kg, Kxx=2000 N/m: T=6.283185307179586 s.

    L1+L2=10+8=18 m; U=1/2 m/s: durations 18/9 s.
    OpenStax (2016), 15.1 supplies T; duration is distance/speed.
    """
    spec.moored_vessel.hull.displacement_tonnes = 2
    spec.passing_ship.track.speed_knots = 1 / 0.514444
    spec.analysis_cases = [AnalysisCase(name="base"), AnalysisCase(
        name="faster", passing_speed_knots=2 / 0.514444,
    )]
    results = run_analysis_cases(spec, time_s=[0], start_stagger_m=0)
    assert results[0].surge_natural_period_s == pytest.approx(6.283185307179586)
    assert results[0].passage_duration_s == pytest.approx(18)
    assert results[1].passage_duration_s == pytest.approx(9)
    assert results[0].applicability_basis == pytest.approx({
        "mass_kg": 2000, "surge_stiffness_n_m": 2000,
        "moored_length_m": 10, "passing_length_m": 8, "speed_m_s": 1,
    })
    spec.analysis_cases = [AnalysisCase(name="wind", include_passing_ship=False)]
    assert run_analysis_cases(spec, time_s=[0], start_stagger_m=0)[0].passage_duration_s is None


@pytest.mark.parametrize("invalid", [True, False, "1", float("nan"), float("inf")])
@pytest.mark.parametrize("group", ["stiffness", "fender", "settings", "wind"])
def test_response_numeric_inputs_are_strict_and_finite(spec, invalid, group):
    """Every new numeric input rejects booleans, strings and nonfinite values."""
    from pydantic import ValidationError

    if group == "stiffness":
        model = type(spec.moored_vessel.mooring_lines[0])
        data = spec.moored_vessel.mooring_lines[0].model_dump()
        fields = ["horizontal_stiffness_n_m"]
    elif group == "fender":
        model = FenderSpec
        data = dict(fender_id="contact", x=0, y=0, normal_x=0, normal_y=1,
                    gap_m=0, stiffness_n_m=4000)
        fields = [name for name in data if name != "fender_id"]
    else:
        instance = spec.response_settings if group == "settings" else spec.moored_vessel.wind_load
        model, data = type(instance), instance.model_dump()
        fields = list(data)
    # Positive case: the integer and float engineering inputs remain valid.
    assert model.model_validate(data).model_dump() == data
    for field in fields:
        with pytest.raises(ValidationError):
            model.model_validate({**data, field: invalid})


def test_every_case_and_wind_response(spec):
    """Wind case q=(-200/2000,0,200/16000)=(-0.1,0,0.0125)."""
    result = run_analysis_cases(spec, time_s=[0, 1], start_stagger_m=0)
    assert [r.name for r in result] == ["still", "wind"]
    np.testing.assert_allclose(result[0].motions, np.zeros((2, 3)), atol=1e-12)
    np.testing.assert_allclose(result[1].motions, [[-0.1, 0, 0.0125]] * 2, atol=1e-12)
    assert result[0].line_tensions_n["x-"][0] == pytest.approx(10000)
    assert result[1].line_tensions_n["x-"][0] == pytest.approx(10100)


def test_speed_separation_overrides_and_force_scaling(spec):
    """Same abeam sample: quadratic force law doubles speed -> fourfold q.

    Decreasing separation increases attraction and the line resisting sway.
    This tests the real existing force calculator, without a response mock.
    """
    spec.analysis_cases = [
        AnalysisCase(name="base"),
        AnalysisCase(name="faster", passing_speed_knots=4),
        AnalysisCase(name="closer", separation_m=10),
    ]
    spec.response_settings.max_translation_m = 100
    spec.response_settings.max_yaw_rad = 1
    results = run_analysis_cases(spec, time_s=[0], start_stagger_m=0)
    base, faster, closer = results
    np.testing.assert_allclose(faster.motions, 4 * base.motions, rtol=1e-10, atol=1e-10)
    assert abs(closer.motions[0, 1]) > abs(base.motions[0, 1])
    assert max(v[0] for v in closer.line_tensions_n.values()) > max(
        v[0] for v in base.line_tensions_n.values()
    )
    assert faster.forces.speed == pytest.approx(2 * base.forces.speed)
    assert closer.forces.separation == 10
    assert spec.passing_ship.track.speed_knots == 2
    assert spec.passing_ship.track.separation_m == 20


def test_sampling_offset_and_output_flags(spec):
    """Sampling is x=x_start+offset+Ut; output flags control returned fields."""
    spec.analysis_cases = [AnalysisCase(name="forces")]
    spec.outputs = OutputSpec(line_tensions=False, vessel_motions=False)
    spec.passing_ship.track.closest_approach_offset_m = 3
    result = run_analysis_cases(spec, time_s=[0, 2], start_stagger_m=-5)[0]
    assert result.forces.stagger[0] == -2
    assert result.forces.stagger[1] == pytest.approx(-2 + 2 * result.forces.speed)
    assert result.line_tensions_n is None
    assert result.motions is None
    spec.analysis_cases = [
        AnalysisCase(name="wind", include_passing_ship=False, include_wind=True)
    ]
    spec.outputs = OutputSpec(line_tensions=False, force_time_histories=False)
    result = run_analysis_cases(spec, time_s=[0], start_stagger_m=0)[0]
    assert result.forces is None
    assert result.line_tensions_n is None
    assert result.motions is not None


@pytest.mark.parametrize(
    "defect,match",
    [
        ("angle", "parallel"),
        ("wind", "wind"),
        ("settings", "response_settings"),
        ("fender", "fender"),
        ("names", "unique"),
        ("current", "current"),
        ("nan", "finite"),
        ("overlap", "separation"),
        ("depth", "depth"),
    ],
)
def test_executor_explicit_limits(spec, defect, match):
    if defect == "angle":
        spec.analysis_cases = [AnalysisCase(name="passing")]
        spec.passing_ship.track.angle_deg = 90
    elif defect == "wind":
        spec.moored_vessel.wind_load = None
    elif defect == "settings":
        spec.response_settings = None
    elif defect == "fender":
        spec.outputs.fender_loads = True
    elif defect == "names":
        spec.analysis_cases[1].name = spec.analysis_cases[0].name
    elif defect == "current":
        spec.environment.current_speed = 1
    elif defect == "nan":
        spec.environment.wind_speed = float("nan")
    elif defect == "overlap":
        spec.analysis_cases = [AnalysisCase(name="passing", separation_m=1)]
    elif defect == "depth":
        spec.environment.water_depth = 0.5
    with pytest.raises(ValueError, match=match):
        run_analysis_cases(spec, time_s=[0, 1], start_stagger_m=0)


def test_default_case_and_forces_only_without_response_inputs(spec):
    spec.analysis_cases = []
    spec.response_settings = None
    spec.moored_vessel.mooring_lines = []
    spec.outputs = OutputSpec(line_tensions=False, vessel_motions=False)
    results = run_analysis_cases(spec, time_s=[0], start_stagger_m=0)
    assert len(results) == 1 and results[0].name == "base"
    assert results[0].forces.peak_sway > 0


def test_fender_output_flag_is_live(spec):
    """Wind from port gives Fy=400 N; gapless kf=4000 shares equally.

    Kyy=4000, so y=400/(4000+4000)=0.05 m and R=200 N.
    """
    from digitalmodel.hydrodynamics.passing_ship import FenderSpec

    spec.analysis_cases = [
        AnalysisCase(name="contact", include_passing_ship=False, include_wind=True)
    ]
    spec.environment.wind_direction_deg = 270
    spec.moored_vessel.wind_load.centre_of_pressure_x_m = 0
    spec.moored_vessel.wind_load.centre_of_pressure_y_m = 0
    spec.moored_vessel.fenders = [
        FenderSpec(
            fender_id="side",
            x=0,
            y=0,
            normal_x=0,
            normal_y=1,
            gap_m=0,
            stiffness_n_m=4000,
        )
    ]
    spec.outputs.fender_loads = True
    result = run_analysis_cases(spec, time_s=[0], start_stagger_m=0)[0]
    assert result.motions[0, 1] == pytest.approx(0.05)
    assert result.fender_loads_n["side"][0] == pytest.approx(200)
    spec.outputs.fender_loads = False
    masked = run_analysis_cases(spec, time_s=[0], start_stagger_m=0)[0]
    assert masked.fender_loads_n is None
    np.testing.assert_allclose(masked.motions, result.motions, atol=1e-12)


@pytest.mark.parametrize("times", [[], [1], [0, 0], [0, -1], [0, float("nan")]])
def test_invalid_sampling_rejected(spec, times):
    with pytest.raises(ValueError, match="time"):
        run_analysis_cases(spec, time_s=times, start_stagger_m=0)
