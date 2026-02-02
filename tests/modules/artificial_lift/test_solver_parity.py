import os
import json
from pathlib import Path
import pytest
import numpy as np
from digitalmodel.artificial_lift.dynacard.models import (
    DynacardAnalysisContext, CardData, RodSection, PumpProperties, SurfaceUnit
)
from digitalmodel.artificial_lift.dynacard.solver import DynacardWorkflow

# Reference data from legacy system
REFERENCE_FILE = "/mnt/github/workspace-hub/client_projects/energy_firm_data_analytics/dynacard/Code/Oxy.Cipher.DynaCard/tests/testdata/11708328.json"


def _reference_file_available() -> bool:
    """Check if the reference file exists."""
    return Path(REFERENCE_FILE).exists()


def load_reference_data():
    with open(REFERENCE_FILE, 'r') as f:
        data = json.load(f)
    
    # Legacy Downhole Card (The "Gold Standard")
    ref_dh_pos = data['downholeCard']['Position']
    ref_dh_load = data['downholeCard']['Load']
    
    # Equipment and Surface Card for our new solver
    rods = [RodSection(diameter=r['Diameter'], length=r['TotalLength'], 
                       modulus_of_elasticity=r['ModulusOfElasticity']) 
            for r in data['equipmentData']['Rods']]
    
    ctx = DynacardAnalysisContext(
        api14=data['CardDetails']['Api14'],
        surface_card=CardData(position=data['cardData']['Position'], load=data['cardData']['Load']),
        rod_string=rods,
        pump=PumpProperties(diameter=data['equipmentData']['Pump']['Diameter'], 
                            depth=data['equipmentData']['Pump']['Depth']),
        surface_unit=SurfaceUnit(manufacturer="Ref", unit_type="C", 
                                 stroke_length=data['InputParameters']['StrokePerMinute4LiftCapacity'], 
                                 gear_box_rating=0),
        spm=data['InputParameters']['StrokesPerMinute']
    )
    
    return ctx, ref_dh_pos, ref_dh_load

@pytest.mark.skipif(
    not _reference_file_available(),
    reason=f"Reference file not available: {REFERENCE_FILE}"
)
def test_solver_parity_with_legacy_gold_standard():
    """
    Test-Driven Update: Compare new solver results with legacy outputs.
    Ensures mathematical accuracy within defined acceptance criteria.
    """
    ctx, ref_pos, ref_load = load_reference_data()
    
    # 1. Run New Solver
    workflow = DynacardWorkflow(ctx)
    results = workflow.run_full_analysis()
    
    new_pos = np.array(results.downhole_card.position)
    new_load = np.array(results.downhole_card.load)
    ref_pos = np.array(ref_pos)
    ref_load = np.array(ref_load)
    
    # 2. Compare Metrics
    # A. Stroke Length (Peak-to-Peak)
    new_stroke = np.max(new_pos) - np.min(new_pos)
    ref_stroke = np.max(ref_pos) - np.min(ref_pos)
    stroke_diff_pct = abs(new_stroke - ref_stroke) / ref_stroke * 100
    
    # B. Peak Load
    new_peak = np.max(new_load)
    ref_peak = np.max(ref_load)
    peak_diff_pct = abs(new_peak - ref_peak) / ref_peak * 100
    
    # C. Normalized RMS Error (Shape similarity)
    # Ensure arrays are same length for comparison
    min_len = min(len(new_load), len(ref_load))
    rms_error = np.sqrt(np.mean((new_load[:min_len] - ref_load[:min_len])**2))
    nrmse = (rms_error / (np.max(ref_load) - np.min(ref_load))) * 100

    # 3. Acceptance Assertions
    print(f"\n--- Parity Results for Well {ctx.api14} ---")
    print(f"Stroke Difference: {stroke_diff_pct:.2f}% (Limit: 2%)")
    print(f"Peak Load Difference: {peak_diff_pct:.2f}% (Limit: 10%)")
    print(f"Shape NRMSE: {nrmse:.2f}% (Limit: 20%)")
    
    assert stroke_diff_pct < 2.0, f"Stroke difference too high: {stroke_diff_pct:.2f}%"
    assert peak_diff_pct < 10.0, f"Peak load difference too high: {peak_diff_pct:.2f}%"
    assert nrmse < 20.0, f"Card shape error too high: {nrmse:.2f}%"