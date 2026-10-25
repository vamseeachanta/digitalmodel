"""Inventory regressions for the constant-inflow comparison case (#1978).

Reference inputs are from issue #1978 (2026). Independent continuity arithmetic
uses OpenStax, University Physics Volume 1 (2016), section 14.5, Q=dV/dt:
capacity=20*6*10=1200 m3; initial liquid=headroom=1200/2=600 m3;
transfer=60 m3; T=6.2514 s; Q=120/6.2514 m3/s;
t_full=600/(120/6.2514)=31.257 s; at 50 s inflow=6000/6.2514 m3.
The selected half-cycle comparison ends at 6.2514/2=3.1257 s and adds 60 m3.
Other numbers below are synthetic test inputs, with their arithmetic stated.
"""

from dataclasses import replace
from pathlib import Path
import re

import pytest

from digitalmodel.solvers.openfoam.case_builder import OpenFOAMCaseBuilder
from digitalmodel.solvers.openfoam.case_coupling import (
    build_case_manifest,
    map_sweep_case_to_openfoam_case,
    synthetic_coupling_spec,
    synthetic_sweep_case,
    verify_coupling,
)
from digitalmodel.solvers.openfoam.models import BoundaryType, CaseType, OpenFOAMCase


def reference(*, fill=0.5, capacity=0.05, period=6.2514):
    """Issue #1978 geometry, transfer and period, independent of dispersion."""
    spec = synthetic_coupling_spec()
    sweep = replace(
        synthetic_sweep_case(),
        fill_fraction=fill,
        fill_depth=fill * 10.0,
        conduit_capacity=capacity,
        roll_period_s=period,
        roll_frequency_hz=1.0 / period,
    )
    return sweep, spec


def inlet(case):
    return next(
        bc
        for bc in case.boundary_conditions
        if bc.patch_name == "inlet" and bc.field == "U"
    )


def test_reference_default_is_one_half_cycle_and_does_not_change_plain_cases():
    sweep, spec = reference()
    case = map_sweep_case_to_openfoam_case(sweep, spec)
    assert case.solver_config.end_time == pytest.approx(3.1257)
    assert (
        OpenFOAMCase.for_case_type(CaseType.SLOSHING, "plain").solver_config.end_time
        == 50.0
    )
    result = verify_coupling(case, sweep, spec)
    assert result.planned_inflow_m3 == pytest.approx(60.0)
    assert result.headroom_m3 == pytest.approx(600.0)
    assert result.headroom_exhaustion_time_s == pytest.approx(31.257)
    assert result.checks["planned_inflow_within_headroom"]


def test_reference_old_default_exhausts_headroom_before_end_time():
    sweep, spec = reference()
    case = map_sweep_case_to_openfoam_case(sweep, spec)
    case.solver_config.end_time = 50.0
    # Independent calculation above: 6000/6.2514 > 600, exhausted at 31.257 s.
    with pytest.raises(ValueError, match=r"headroom=600.*31\.257.*endTime=50"):
        verify_coupling(case, sweep, spec)


def test_mapper_rejects_inflow_exceeding_primary_tank_headroom():
    """Synthetic fill=.975: headroom=30 m3; capacity=.05 adds 60 m3."""
    sweep, spec = reference(fill=0.975)
    with pytest.raises(ValueError, match="headroom"):
        map_sweep_case_to_openfoam_case(sweep, spec)


def test_mapper_keeps_shorter_existing_end_time():
    """Synthetic period=120 s: min(default 50, T/2=60)=50 s."""
    sweep, spec = reference(period=120.0)
    case = map_sweep_case_to_openfoam_case(sweep, spec)
    assert case.solver_config.end_time == 50.0
    assert verify_coupling(case, sweep, spec).planned_inflow_m3 == 50.0


def test_gate_uses_elapsed_time_and_actual_initial_fill():
    """Synthetic start=10, end=11, Q=120/6.2514: one second of inflow.

    A changed fill of .75 gives (1-.75)*1200=300 m3 headroom.
    """
    sweep, spec = reference()
    case = map_sweep_case_to_openfoam_case(sweep, spec)
    case.solver_config.start_time = 10.0
    case.solver_config.end_time = 11.0
    case.fill_level = 0.75
    result = verify_coupling(case, sweep, spec)
    assert result.planned_inflow_m3 == pytest.approx(120.0 / 6.2514)
    assert result.headroom_m3 == 300.0
    assert result.headroom_exhaustion_time_s == pytest.approx(25.6285)


def test_gate_accounts_for_mesh_snapped_initial_inventory():
    """Synthetic fill=.989 rounds to 40/40 wet cells, giving zero headroom."""
    sweep, spec = reference()
    case = map_sweep_case_to_openfoam_case(sweep, spec)
    case.fill_level = 0.989
    with pytest.raises(ValueError, match="headroom=0"):
        verify_coupling(case, sweep, spec)


@pytest.mark.parametrize(
    "encoded",
    [
        "constant 1000",
        "constant -1",
        "constant nan",
        "constant inf",
        "table ((0 0)(1 1))",
        "constant nope",
    ],
)
def test_gate_reads_and_validates_emitted_boundary_rate(encoded):
    """Synthetic invalid rates must fail even when metadata remains valid."""
    sweep, spec = reference()
    case = map_sweep_case_to_openfoam_case(sweep, spec)
    inlet(case).extra["volumetricFlowRate"] = encoded
    with pytest.raises(ValueError, match="inlet|headroom"):
        verify_coupling(case, sweep, spec)


def test_gate_rejects_safe_but_inconsistent_boundary_rate():
    """Synthetic Q=1 fits the tank but disagrees with the planned transfer."""
    sweep, spec = reference()
    case = map_sweep_case_to_openfoam_case(sweep, spec)
    inlet(case).extra["volumetricFlowRate"] = "constant 1"
    with pytest.raises(ValueError, match="inlet.*mean"):
        verify_coupling(case, sweep, spec)


@pytest.mark.parametrize(
    "start,end",
    [
        (0.0, float("nan")),
        (0.0, float("inf")),
        (1.0, 0.0),
        (0.0, 0.0),
        (float("nan"), 1.0),
    ],
)
def test_gate_rejects_invalid_runtime(start, end):
    sweep, spec = reference()
    case = map_sweep_case_to_openfoam_case(sweep, spec)
    case.solver_config.start_time, case.solver_config.end_time = start, end
    with pytest.raises(ValueError, match="time|Time"):
        verify_coupling(case, sweep, spec)


def test_zero_flow_never_exhausts_headroom():
    sweep, spec = reference(capacity=0.0)
    case = map_sweep_case_to_openfoam_case(sweep, spec)
    case.solver_config.end_time = 50.0
    result = verify_coupling(case, sweep, spec)
    assert result.planned_inflow_m3 == 0.0
    assert result.headroom_exhaustion_time_s is None


def test_exact_headroom_is_allowed_but_larger_volume_is_rejected():
    """Synthetic capacity=.5 moves 600 m3, exactly the 600 m3 headroom.

    Doubling duration would add 1200 m3 and must fail.
    """
    sweep, spec = reference(capacity=0.5)
    case = map_sweep_case_to_openfoam_case(sweep, spec)
    result = verify_coupling(case, sweep, spec)
    assert result.planned_inflow_m3 == result.headroom_m3 == 600.0
    case.solver_config.end_time *= 2.0
    with pytest.raises(ValueError, match="headroom"):
        verify_coupling(case, sweep, spec)


@pytest.mark.parametrize("period", [6.2514, 0.9])
def test_ninety_percent_fill_accepts_exact_headroom(period):
    """Synthetic fill=.9: 1200-1080=120 m3; capacity=.1 adds 120 m3.

    Period=.9 also rounds the computed half-cycle inflow slightly above 120.
    """
    sweep, spec = reference(fill=0.9, capacity=0.1, period=period)
    case = map_sweep_case_to_openfoam_case(sweep, spec)
    result = verify_coupling(case, sweep, spec)
    assert result.passed
    assert result.headroom_m3 == pytest.approx(120.0, rel=2e-15, abs=0.0)
    assert result.planned_inflow_m3 == pytest.approx(120.0, rel=2e-15, abs=0.0)
    assert result.headroom_exhaustion_time_s == pytest.approx(period / 2.0)


def test_ninety_percent_fill_rejects_small_physical_overflow():
    """Synthetic excess=1200*1e-12 m3, larger than arithmetic roundoff."""
    sweep, spec = reference(fill=0.9, capacity=0.1 + 1e-12)
    with pytest.raises(ValueError, match="exceeds initialized headroom"):
        map_sweep_case_to_openfoam_case(sweep, spec)


def test_gate_rejects_positive_inflow_into_zero_headroom():
    """A full tank cannot admit even a synthetic 1e-15 m3/s inlet."""
    sweep, spec = reference(capacity=0.0)
    case = map_sweep_case_to_openfoam_case(sweep, spec)
    case.fill_level = 1.0
    inlet(case).extra["volumetricFlowRate"] = "constant 1e-15"
    with pytest.raises(ValueError, match="headroom=0"):
        verify_coupling(case, sweep, spec)


@pytest.mark.parametrize("duplicate", [False, True])
def test_gate_rejects_ambiguous_or_unbounded_inlet(duplicate):
    sweep, spec = reference()
    case = map_sweep_case_to_openfoam_case(sweep, spec)
    if duplicate:
        case.boundary_conditions.append(inlet(case))
    else:
        inlet(case).bc_type = BoundaryType.FIXED_VALUE
    with pytest.raises(ValueError, match="inlet.*exactly one"):
        verify_coupling(case, sweep, spec)


def test_documented_worked_example(capsys):
    """Run the documented example; expected output is hand-calculated above."""
    doc = (
        Path(__file__).resolve().parents[3]
        / "docs/domains/openfoam/coupled_inflow_inventory.md"
    )
    example = doc.read_text().split("```python\n")[1].split("```")[0]
    exec(compile(example, str(doc), "exec"), {})
    assert capsys.readouterr().out == (
        "end time: 3.1 s\nplanned inflow: 60.0 m3\nheadroom: 600.0 m3\n"
    )


def test_built_case_and_manifest_record_guarded_duration(tmp_path):
    sweep, spec = reference()
    case = map_sweep_case_to_openfoam_case(sweep, spec)
    result = verify_coupling(case, sweep, spec)
    manifest = build_case_manifest(case, sweep, spec, result)
    assert manifest.solver["end_time_s"] == pytest.approx(3.1257)
    assert manifest.solver["planned_inflow_m3"] == pytest.approx(60.0)
    assert manifest.solver["headroom_m3"] == 600.0
    case_dir = OpenFOAMCaseBuilder(case).build(tmp_path)
    assert re.search(
        r"endTime\s+3\.1257;", (case_dir / "system/controlDict").read_text()
    )
