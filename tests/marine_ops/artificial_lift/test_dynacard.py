import pytest
import numpy as np
from digitalmodel.marine_ops.artificial_lift.dynacard.models import (
    DynacardAnalysisContext, CardData, RodSection, PumpProperties, SurfaceUnit
)
from digitalmodel.marine_ops.artificial_lift.dynacard.solver import DynacardWorkflow
from digitalmodel.marine_ops.artificial_lift.dynacard.physics import DynacardPhysicsSolver

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
    
    # Updated for high-fidelity physics: 0.92 attenuation for this configuration
    assert dh_stroke < surf_stroke
    assert dh_stroke == pytest.approx(surf_stroke * 0.92, rel=1e-2)

def test_buckling_detection(sample_context):
    """Ensures buckling is detected when loads are sufficiently negative."""
    # Modify load to simulate extreme compression
    sample_context.surface_card.load = [L - 25000 for L in sample_context.surface_card.load]
    
    workflow = DynacardWorkflow(sample_context)
    results = workflow.run_full_analysis()
    
    assert results.buckling_detected is True

def test_ai_classification_normal(sample_context):
    """Verifies that a standard sinusoidal card is classified as NORMAL."""
    workflow = DynacardWorkflow(sample_context)
    results = workflow.run_full_analysis()
    
    assert "NORMAL" in results.diagnostic_message

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

from digitalmodel.marine_ops.artificial_lift.dynacard.models import AnalysisResults
from digitalmodel.marine_ops.artificial_lift.dynacard.solver import perform_well_troubleshooting
from digitalmodel.marine_ops.artificial_lift.dynacard.finite_difference import FiniteDifferenceSolver


def test_workflow_init_without_context():
    """Init DynacardWorkflow with no context, verify solver is None."""
    workflow = DynacardWorkflow()
    assert workflow.ctx is None
    assert workflow.solver is None
    assert workflow.solver_method == 'gibbs'


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
    """Call compare_solvers(), verify return has expected keys and stroke_diff_pct is reasonable."""
    workflow = DynacardWorkflow(sample_context)
    comparison = workflow.compare_solvers()
    assert 'gibbs_results' in comparison
    assert 'fd_results' in comparison
    assert 'comparison' in comparison
    comp = comparison['comparison']
    assert 'gibbs_stroke' in comp
    assert 'fd_stroke' in comp
    assert 'stroke_diff_pct' in comp
    assert 'load_rmse' in comp
    assert 'load_rmse_pct' in comp
    assert comp['stroke_diff_pct'] < 20.0


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
