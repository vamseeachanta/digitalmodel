#!/usr/bin/env python3
"""
ABOUTME: Regression tests for the coupled-case inlet flux defect (#1528 slice 7
follow-up). The mapper used to impose the conduit *velocity* Q_peak/A_conduit as
a fixedValue over the whole inlet face, over-fluxing the tank by the ratio of
face area to conduit area. The fix specifies a volumetric flow rate
(flowRateInletVelocity), which is area-independent, and the case builder now
renders attached boundary conditions into the 0/ fields instead of hardcoding
them.
"""

import math
from pathlib import Path

import pytest

from digitalmodel.solvers.openfoam.models import BoundaryType, CaseType, OpenFOAMCase
from digitalmodel.solvers.openfoam.case_builder import OpenFOAMCaseBuilder
from digitalmodel.solvers.openfoam.gravity_conduit import ConduitGeometry
from digitalmodel.solvers.openfoam.case_coupling import (
    CouplingSpec,
    map_sweep_case_to_openfoam_case,
    mean_flow_rate,
    synthetic_sweep_case,
)


def _spec(conduit_area: float) -> CouplingSpec:
    return CouplingSpec(
        tank_length=20.0,
        tank_width=6.0,
        tank_height=10.0,
        roll_amplitude_deg=5.0,
        conduit=ConduitGeometry(area=conduit_area, discharge_coefficient=0.6,
                                loss_coefficient=1.5),
        dest_fill_fraction=0.30,
    )


def _inlet_u_bc(case: OpenFOAMCase):
    return next(
        bc for bc in case.boundary_conditions
        if bc.patch_name == "inlet" and bc.field == "U"
    )


# ---------------------------------------------------------------------------
# mean_flow_rate helper
# ---------------------------------------------------------------------------


def test_mean_flow_rate_integrates_to_transfer_over_half_cycle():
    # Mean half-cycle rate 2*f*V_transfer, held over T/2, transfers exactly V.
    v_transfer, period = 60.0, 6.2514
    q_mean = mean_flow_rate(v_transfer, period)
    assert q_mean == pytest.approx(2.0 * v_transfer / period)
    assert q_mean * (period / 2.0) == pytest.approx(v_transfer)


# ---------------------------------------------------------------------------
# The defect: inlet flux must be area-INDEPENDENT
# ---------------------------------------------------------------------------


def test_inlet_flux_is_volumetric_flow_rate_not_raw_velocity():
    sweep = synthetic_sweep_case()
    case = map_sweep_case_to_openfoam_case(sweep, _spec(conduit_area=0.5))
    bc = _inlet_u_bc(case)
    assert bc.bc_type is BoundaryType.FLOW_RATE_INLET_VELOCITY
    assert "volumetricFlowRate" in bc.extra
    # It must encode the mean volumetric rate, not a bare face velocity.
    q_mean = mean_flow_rate(
        case.metadata["transfer_volume_m3"], case.metadata["roll_period_s"]
    )
    assert str(q_mean)[:6] in bc.extra["volumetricFlowRate"] or \
        f"{q_mean:.10g}" in bc.extra["volumetricFlowRate"]


def test_inlet_flux_independent_of_conduit_area():
    # Same transfer volume + period, different conduit areas -> IDENTICAL inlet
    # volumetric flow rate. (The old velocity-based BC scaled with 1/area.)
    sweep = synthetic_sweep_case()
    bc_small = _inlet_u_bc(map_sweep_case_to_openfoam_case(sweep, _spec(0.25)))
    bc_large = _inlet_u_bc(map_sweep_case_to_openfoam_case(sweep, _spec(2.0)))
    assert bc_small.extra["volumetricFlowRate"] == bc_large.extra["volumetricFlowRate"]


def test_metadata_records_volumetric_flow_rate():
    sweep = synthetic_sweep_case()
    case = map_sweep_case_to_openfoam_case(sweep, _spec(0.5))
    q_mean = mean_flow_rate(
        case.metadata["transfer_volume_m3"], case.metadata["roll_period_s"]
    )
    assert case.metadata["volumetric_flow_rate_m3_s"] == pytest.approx(q_mean)


# ---------------------------------------------------------------------------
# The builder must RENDER attached BCs (previously hardcoded/ignored)
# ---------------------------------------------------------------------------


def test_builder_renders_flow_rate_inlet_into_zero_U(tmp_path):
    sweep = synthetic_sweep_case()
    case = map_sweep_case_to_openfoam_case(sweep, _spec(0.5))
    case_dir = OpenFOAMCaseBuilder(case).build(tmp_path)
    u_text = (case_dir / "0" / "U").read_text()
    assert "flowRateInletVelocity" in u_text
    assert "volumetricFlowRate" in u_text
    # The old bug: a huge bare face velocity like "uniform (60 0 0)" must be gone.
    assert "uniform (1 0 0)" not in u_text  # default inlet velocity replaced


def test_builder_keeps_defaults_without_attached_bcs(tmp_path):
    # Regression: a plain case (no attached BCs) still gets the default fields.
    case = OpenFOAMCase.for_case_type(CaseType.SLOSHING, "plain_case")
    case_dir = OpenFOAMCaseBuilder(case).build(tmp_path)
    u_text = (case_dir / "0" / "U").read_text()
    assert "boundaryField" in u_text
    assert "inlet" in u_text and "outlet" in u_text
