# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

import pytest
import numpy as np
from digitalmodel.marine_ops.artificial_lift.dynacard.models import (
    DynacardAnalysisContext, CardData, RodSection, PumpProperties, SurfaceUnit
)
from digitalmodel.marine_ops.artificial_lift.dynacard.solver import DynacardWorkflow
from digitalmodel.marine_ops.artificial_lift.dynacard.physics import DynacardPhysicsSolver

from digitalmodel.marine_ops.artificial_lift.dynacard.models import AnalysisResults
from digitalmodel.marine_ops.artificial_lift.dynacard.solver import perform_well_troubleshooting
from digitalmodel.marine_ops.artificial_lift.dynacard.finite_difference import FiniteDifferenceSolver

@pytest.fixture
def sample_context():
    """Provides a standard well context for testing."""
    t = np.linspace(0, 2 * np.pi, 100)
    pos = 100 * (1 - np.cos(t))
    # Increased base load to 25000 to ensure downhole load stays in 'NORMAL' range after buoyant weight subtraction
    load = 25000 + 5000 * np.sin(t)
    
    return DynacardAnalysisContext(
        api14="TEST-WELL-001",
        surface_card=CardData(position=pos.tolist(), load=load.tolist()),
        rod_string=[RodSection(diameter=1.0, length=5000)],
        pump=PumpProperties(diameter=1.75, depth=5000),
        surface_unit=SurfaceUnit(
            manufacturer="TestMfg", unit_type="TestUnit", 
            stroke_length=144, gear_box_rating=640000
        ),
        spm=10.0
    )

def test_physics_solver_attenuation(sample_context):
    """Verifies that the downhole card reflects expected stroke attenuation."""
    solver = DynacardPhysicsSolver(sample_context)
    results = solver.solve_wave_equation()
    
    surf_stroke = max(sample_context.surface_card.position)
    dh_stroke = max(results.downhole_card.position)
    
    # This boundary has a shorter pump stroke; exact analytical values are
    # independently checked in test_gibbs_transfer.py, without fitted scaling.
    assert dh_stroke < surf_stroke

def test_buckling_detection(sample_context):
    """Ensures buckling is detected when loads are sufficiently negative."""
    # Modify load to simulate extreme compression
    sample_context.surface_card.load = [L - 25000 for L in sample_context.surface_card.load]
    
    workflow = DynacardWorkflow(sample_context)
    results = workflow.run_full_analysis()
    
    assert results.buckling_detected is True

def test_ai_classification_produces_valid_result(sample_context):
    """Verifies that ML classifier produces a valid classification for a standard card."""
    workflow = DynacardWorkflow(sample_context)
    results = workflow.run_full_analysis()

    assert "Classification:" in results.diagnostic_message
    # The ML model should produce a valid failure mode
    from digitalmodel.marine_ops.artificial_lift.dynacard import PumpDiagnostics
    classified_mode = results.diagnostic_message.split("Classification: ")[1].split(".")[0]
    assert classified_mode in PumpDiagnostics.FAILURE_MODES

def test_invalid_config_validation():
    """Ensures Pydantic catches missing required fields."""
    with pytest.raises(ValueError):
        # Missing required surface_card
        DynacardAnalysisContext(api14="FAIL", spm=10.0)

def test_full_workflow_integration(sample_context):
    """End-to-end test of the dynacard workflow."""
    workflow = DynacardWorkflow(sample_context)
    results = workflow.run_full_analysis()

    assert results.downhole_card is not None
    assert len(results.downhole_card.position) == len(sample_context.surface_card.position)
    assert results.ctx.api14 == "TEST-WELL-001"


# --- Additional solver.py coverage tests ---

def test_workflow_init_without_context():
    """Init DynacardWorkflow with no context, verify solver is None.

    The default remains everitt_jennings. The coupled Gibbs implementation
    is tested analytically and remains unqualified for field diagnosis.
    """
    workflow = DynacardWorkflow()
    assert workflow.ctx is None
    assert workflow.solver is None
    assert workflow.solver_method == 'everitt_jennings'


def test_workflow_init_with_finite_difference(sample_context):
    """Init with solver_method='finite_difference', verify solver type."""
    workflow = DynacardWorkflow(sample_context, solver_method='finite_difference')
    assert isinstance(workflow.solver, FiniteDifferenceSolver)
    assert workflow.solver_method == 'finite_difference'


def test_router_with_well_data(sample_context):
    """Build cfg dict with 'well_data' key containing serialized context, call router(), verify results key is populated."""
    context_dict = sample_context.model_dump()
    cfg = {'well_data': context_dict}
    workflow = DynacardWorkflow()
    result_cfg = workflow.router(cfg)
    assert 'results' in result_cfg
    assert result_cfg['results'] is not None
    assert isinstance(result_cfg['results'], dict)


def test_router_with_solver_method_override(sample_context):
    """Router with solver_method='finite_difference' in cfg."""
    context_dict = sample_context.model_dump()
    cfg = {
        'well_data': context_dict,
        'solver_method': 'finite_difference',
    }
    workflow = DynacardWorkflow()
    result_cfg = workflow.router(cfg)
    assert 'results' in result_cfg
    assert result_cfg['results']['solver_method'] == 'finite_difference'


def test_compare_solvers(sample_context):
    """Disagreement requires explicit opt-in and is measured against the default."""
    from digitalmodel.marine_ops.artificial_lift.dynacard.exceptions import PhysicsError

    workflow = DynacardWorkflow(sample_context)
    with pytest.raises(PhysicsError, match="solver disagreement") as caught:
        workflow.compare_solvers()
    comparison = workflow.compare_solvers(allow_divergence=True)
    assert comparison['reference_solver'] == 'everitt_jennings'
    assert comparison['reference_results'].downhole_card is not None
    assert set(comparison['comparisons']) == {'gibbs', 'finite_difference'}
    assert comparison['within_threshold'] is False
    for metrics in comparison['comparisons'].values():
        assert np.isfinite(metrics['stroke_error_pct'])
        assert np.isfinite(metrics['load_nrmse_pct'])
    assert comparison['threshold_load_nrmse_pct'] == pytest.approx(4.4)
    assert comparison['comparisons']['gibbs']['stroke_error_pct'] < 20.0
    gibbs = comparison['comparisons']['gibbs']
    # The coupled solution now passes this synthetic comparison envelope.
    # The legacy finite-difference candidate still fails the aggregate gate.
    assert gibbs['load_nrmse_pct'] < comparison['threshold_load_nrmse_pct']
    assert gibbs['within_threshold'] is True
    assert comparison['comparisons']['finite_difference']['within_threshold'] is False
    assert caught.value.details['comparisons']['gibbs'] == gibbs


def test_perform_well_troubleshooting(sample_context):
    """Call perform_well_troubleshooting with context dict, verify returns AnalysisResults."""
    context_dict = sample_context.model_dump()
    results = perform_well_troubleshooting(context_dict)
    assert isinstance(results, AnalysisResults)
    assert results.downhole_card is not None
    assert results.ctx is not None


def test_full_workflow_with_finite_difference(sample_context):
    """Run full analysis with FD solver, verify results."""
    workflow = DynacardWorkflow(sample_context, solver_method='finite_difference')
    results = workflow.run_full_analysis()
    assert results.downhole_card is not None
    assert results.solver_method == 'finite_difference'
    assert len(results.downhole_card.position) > 0
    assert len(results.downhole_card.load) > 0


def test_workflow_p1_results_populated(sample_context):
    """Verify fluid_load, cpip, fillage, production are populated after run_full_analysis."""
    workflow = DynacardWorkflow(sample_context)
    results = workflow.run_full_analysis()
    assert results.fluid_load is not None
    assert results.cpip is not None
    assert results.fillage is not None
    assert results.production is not None
    assert results.pump_fillage == pytest.approx(results.fillage.fillage)
    assert results.inferred_production == pytest.approx(results.production.theoretical_production)
