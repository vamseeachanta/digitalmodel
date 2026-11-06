# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Pressure and hydraulic selection must use the same candidate dimensions."""

from dataclasses import replace

import pytest

from digitalmodel.process_piping import Flow, PressureDesign, size_line


@pytest.fixture
def design():
    """Synthetic inputs; the template OD deliberately represents NPS 2."""
    return PressureDesign(
        pressure_pa=8e6,
        outside_diameter_m=0.060325,
        allowable_stress_pa=138e6,
        quality_factor=1,
        weld_strength_factor=1,
        y=0.4,
        corrosion_allowance_m=0.003,
        mechanical_allowance_m=0,
        mill_tolerance_fraction=0.125,
    )


@pytest.fixture
def water():
    """Actual flow and assumed water properties, in SI units."""
    return Flow("liquid", 50 / 3600, 998.2, 0.001002, 100, 0.000045)


@pytest.mark.parametrize("phase", ["liquid", "gas", "two_phase"])
def test_pressure_constraint_applies_to_every_phase(water, design, phase):
    flow = replace(
        water,
        phase=phase,
        density_kg_m3=10 if phase == "gas" else 998.2,
        length_m=1,
        inlet_pressure_pa_abs=1e6 if phase == "gas" else None,
        erosional_c_us=100 if phase == "two_phase" else None,
        erosional_c_basis="assumed clean service" if phase == "two_phase" else None,
    )
    hydraulic_only = size_line(
        flow, candidate_nps=(4,), schedules=("40", "80"), velocity_limit_m_s=4
    )
    selected = size_line(
        flow,
        candidate_nps=(4,),
        schedules=("40", "80"),
        velocity_limit_m_s=4,
        pressure_design=design,
    )
    assert hydraulic_only.schedule == "40"
    assert hydraulic_only.pressure_requirement is None
    assert selected.schedule == "80"
    assert selected.pressure_requirement.minimum_nominal_thickness_m <= selected.wall_thickness_m
    assert {ref.code_id for ref in selected.citations} >= {"asme-b31-3", "pipe-flow"}


@pytest.mark.parametrize("nps,required", [(4, 0.0071290975), (6, 0.0088765682)])
def test_required_wall_uses_candidate_od(water, design, nps, required):
    # Existing pressure method, explicit synthetic factors:
    # t_nom=[8*D/(2*(138+8*0.4))+0.003]/0.875, with P/S in MPa.
    # D=0.114300 or 0.168275 m gives the independent expected walls above.
    selected = size_line(
        water,
        candidate_nps=(nps,),
        schedules=("40", "80"),
        velocity_limit_m_s=4,
        pressure_design=design,
    )
    assert selected.nps == nps
    assert selected.schedule == "80"
    assert selected.pressure_requirement.minimum_nominal_thickness_m == pytest.approx(required)
    assert design.outside_diameter_m == 0.060325


def test_pressure_and_supplied_wall_constraints_both_apply(water, design):
    selected = size_line(
        water,
        candidate_nps=(4, 6),
        schedules=("40", "80"),
        velocity_limit_m_s=4,
        pressure_design=design,
        minimum_wall_m=0.01,
    )
    assert (selected.nps, selected.schedule) == (6, "80")
    assert selected.wall_thickness_m >= 0.01
    assert selected.pressure_requirement.minimum_nominal_thickness_m <= selected.wall_thickness_m


def test_pressure_constraint_cannot_override_hydraulic_budget(water, design):
    selected = size_line(
        water,
        candidate_nps=(4, 6),
        schedules=("40", "80"),
        velocity_limit_m_s=4,
        maximum_pressure_drop_pa=10000,
        pressure_design=design,
    )
    assert (selected.nps, selected.schedule) == (6, "80")
    assert selected.pressure_drop_pa <= 10000


def test_no_pressure_suitable_schedule_reports_reason(water, design):
    with pytest.raises(ValueError, match="pressure-required wall"):
        size_line(
            water,
            candidate_nps=(4, 6),
            schedules=("40",),
            velocity_limit_m_s=4,
            pressure_design=design,
        )


@pytest.mark.parametrize(
    "changes,reason",
    [
        ({"quality_factor": 0}, "quality_factor"),
        ({"mill_tolerance_fraction": 1}, "mill tolerance"),
        ({"pressure_pa": 60e6}, "special high-pressure"),
        ({"corrosion_allowance_m": 0.1}, "outside this screen"),
    ],
)
def test_invalid_or_out_of_scope_pressure_design_is_not_accepted(water, design, changes, reason):
    with pytest.raises(ValueError, match=reason):
        size_line(
            water,
            candidate_nps=(4,),
            velocity_limit_m_s=4,
            pressure_design=replace(design, **changes),
        )
