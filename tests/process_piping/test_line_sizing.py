"""Hydraulic benchmarks use actual volumetric flow and SI units."""

from dataclasses import replace
import math
import pytest

from digitalmodel.process_piping.line_sizing import (
    Flow,
    evaluate_line,
    size_line,
    service_velocity_limit,
)


@pytest.fixture
def water():
    return Flow(
        phase="liquid",
        flow_m3_s=50 / 3600,
        density_kg_m3=998.2,
        viscosity_pa_s=0.001002,
        length_m=100,
        roughness_m=0.000045,
    )


def test_public_water_example(water):
    # Catalog inches: ID=(4.5-2*.237)*.0254=.1022604 m.
    # A=.0082127 m², v=1.691 m/s; Re=172318; f~.01888 (Swamee–Jain).
    # Darcy dp=f*(100/.1022604)*998.2*v²/2 ~26340 Pa = .2634 bar/100m.
    result = evaluate_line(water, nps=4, schedule="40", velocity_limit_m_s=2)
    assert result.inner_diameter_m == pytest.approx(0.1022604)
    assert result.velocity_m_s == pytest.approx(1.691, abs=0.002)
    assert result.reynolds == pytest.approx(172318, rel=0.003)
    assert result.friction_factor == pytest.approx(0.01888, abs=0.0001)
    assert result.pressure_drop_pa / 1e5 == pytest.approx(0.2634, abs=0.003)
    assert result.within_velocity_limit


def test_liquid_selection_fittings_head_and_laminar(water):
    selected = size_line(
        water, schedules=("40",), velocity_limit_m_s=2, maximum_pressure_drop_pa=30000
    )
    assert selected.nps == 4
    assert selected.schedule == "40"
    bare = evaluate_line(water, nps=4, schedule="40", velocity_limit_m_s=2)
    added = evaluate_line(
        replace(water, fittings_k=3, elevation_change_m=10),
        nps=4,
        schedule="40",
        velocity_limit_m_s=2,
    )
    assert added.pressure_drop_pa - bare.pressure_drop_pa == pytest.approx(
        3 * 998.2 * bare.velocity_m_s**2 / 2 + 998.2 * 9.80665 * 10
    )
    low = evaluate_line(
        replace(water, flow_m3_s=1e-6), nps=4, schedule="40", velocity_limit_m_s=2
    )
    assert low.friction_factor == pytest.approx(64 / low.reynolds)
    assert low.regime == "laminar"
    with pytest.raises(ValueError, match="candidate"):
        size_line(
            water, schedules=("40",), velocity_limit_m_s=0.001, candidate_nps=(4, 6)
        )


def gas(**changes):
    return Flow(
        **(
            dict(
                phase="gas",
                flow_m3_s=0.05,
                density_kg_m3=10,
                viscosity_pa_s=1.1e-5,
                length_m=4000,
                roughness_m=4.5e-5,
                inlet_pressure_pa_abs=1e6,
            )
            | changes
        )
    )


def test_gas_low_drop_and_isothermal_residual():
    small = evaluate_line(gas(length_m=1), nps=4, schedule="40", velocity_limit_m_s=40)
    assert small.method == "inlet_density"
    assert small.pressure_drop_pa / 1e6 <= 0.1
    large = evaluate_line(gas(), nps=4, schedule="40", velocity_limit_m_s=40)
    assert large.method == "isothermal"
    # Independent integrated momentum: (p1²-p2²)/(G²*RT)=fL/D+2ln(p1/p2).
    p1, p2 = 1e6, large.outlet_pressure_pa_abs
    area = math.pi * large.inner_diameter_m**2 / 4
    mass_flux = 0.05 * 10 / area
    assert (p1 * p1 - p2 * p2) / (mass_flux**2 * (p1 / 10)) == pytest.approx(
        large.friction_factor * 4000 / large.inner_diameter_m + 2 * math.log(p1 / p2),
        rel=1e-10,
    )
    assert large.velocity_m_s == pytest.approx((0.05 / area) * p1 / p2)
    assert large.pressure_drop_pa > 0.1e6


def test_gas_threshold_choking_and_fittings():
    assert (
        size_line(gas(), velocity_limit_m_s=40, maximum_pressure_drop_pa=1e5).nps == 5
    )
    result = evaluate_line(gas(length_m=1), nps=4, schedule="40", velocity_limit_m_s=40)
    # Inlet-density dp scales with length; exactly 10% stays on screening branch.
    threshold_length = 0.1e6 / result.pressure_drop_pa
    below = evaluate_line(
        gas(length_m=threshold_length * 0.999),
        nps=4,
        schedule="40",
        velocity_limit_m_s=40,
    )
    above = evaluate_line(
        gas(length_m=threshold_length * 1.001),
        nps=4,
        schedule="40",
        velocity_limit_m_s=40,
    )
    assert below.method == "inlet_density"
    assert above.method == "isothermal"
    assert (
        evaluate_line(
            gas(fittings_k=3), nps=4, schedule="40", velocity_limit_m_s=40
        ).pressure_drop_pa
        > evaluate_line(
            gas(), nps=4, schedule="40", velocity_limit_m_s=40
        ).pressure_drop_pa
    )
    with pytest.raises(ValueError, match="chok"):
        evaluate_line(gas(flow_m3_s=1), nps=4, schedule="40", velocity_limit_m_s=40)
    with pytest.raises(ValueError, match="horizontal"):
        evaluate_line(
            gas(elevation_change_m=1), nps=4, schedule="40", velocity_limit_m_s=40
        )


def test_two_phase_us_customary_c_conversion():
    # rho=16.018463 kg/m³ = 1 lb/ft³; C=100 -> Ve=100 ft/s=30.48m/s.
    flow = Flow(
        phase="two_phase",
        flow_m3_s=0.1,
        density_kg_m3=16.01846337396,
        viscosity_pa_s=None,
        length_m=100,
        roughness_m=0,
        erosional_c_us=100,
        erosional_c_basis="continuous, sand-free; API RP 14E 1991 2.5",
    )
    result = evaluate_line(flow, nps=4, schedule="40", velocity_limit_m_s=40)
    assert result.erosional_velocity_m_s == pytest.approx(30.48)
    assert result.pressure_drop_pa is None
    assert result.friction_factor is None
    assert result.within_velocity_limit
    assert size_line(flow, velocity_limit_m_s=40).nps == 3
    assert evaluate_line(
        replace(flow, erosional_c_us=125), nps=4, schedule="40", velocity_limit_m_s=40
    ).erosional_velocity_m_s == pytest.approx(38.1)
    assert not evaluate_line(
        replace(flow, flow_m3_s=1), nps=4, schedule="40", velocity_limit_m_s=40
    ).within_velocity_limit
    with pytest.raises(ValueError):
        size_line(flow, velocity_limit_m_s=40, maximum_pressure_drop_pa=1e5)
    with pytest.raises(ValueError):
        evaluate_line(
            replace(flow, erosional_c_basis=""),
            nps=4,
            schedule="40",
            velocity_limit_m_s=40,
        )


def test_public_service_limit():
    # ASHRAE Fundamentals 2017 Ch22 Table22, pump suction upper bound 7ft/s.
    assert service_velocity_limit("pump_suction_water").value == pytest.approx(2.1336)
    assert service_velocity_limit("general_water").value == pytest.approx(3.048)
    with pytest.raises(ValueError):
        service_velocity_limit("gas")


@pytest.mark.parametrize(
    "changes",
    [
        dict(flow_m3_s=0),
        dict(density_kg_m3=-1),
        dict(phase="steam"),
        dict(roughness_m=math.nan),
        dict(fittings_k=-1),
        dict(viscosity_pa_s=0),
        dict(length_m=-1),
    ],
)
def test_invalid(water, changes):
    with pytest.raises(ValueError):
        evaluate_line(
            replace(water, **changes), nps=4, schedule="40", velocity_limit_m_s=2
        )


def test_wall_filter_and_transition(water):
    selected = size_line(
        water, schedules=("40", "80"), velocity_limit_m_s=2, minimum_wall_m=0.008
    )
    assert selected.wall_thickness_m >= 0.008
    re3000 = replace(water, flow_m3_s=water.flow_m3_s * 3000 / 172318)
    assert (
        evaluate_line(re3000, nps=4, schedule="40", velocity_limit_m_s=2).regime
        == "transitional"
    )
