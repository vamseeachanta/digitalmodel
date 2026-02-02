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
