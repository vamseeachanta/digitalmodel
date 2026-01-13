import os
import json
import pytest
import glob
import numpy as np
from digitalmodel.modules.artificial_lift.dynacard.models import (
    DynacardAnalysisContext, CardData, RodSection, PumpProperties, SurfaceUnit
)
from digitalmodel.modules.artificial_lift.dynacard.solver import DynacardWorkflow

# Path to local cleansed test data
LOCAL_DATA_DIR = os.path.join(os.path.dirname(__file__), "test_data")

def get_cleansed_files():
    """Retrieves all anonymized JSON files from the local test data directory."""
    return glob.glob(os.path.join(LOCAL_DATA_DIR, "*.json"))

def map_cleansed_to_modern(file_path):
    """Maps anonymized JSON structure to modern DynacardAnalysisContext."""
    with open(file_path, 'r') as f:
        data = json.load(f)
    
    # Rods mapping
    rods = []
    for r in data.get('equipmentData', {}).get('Rods', []):
        rods.append(RodSection(
            diameter=r.get('Diameter', 1.0),
            length=r.get('TotalLength', 0.0),
            modulus_of_elasticity=r.get('ModulusOfElasticity', 30500000.0),
            density=490.0
        ))
    
    # Pump mapping
    pump_raw = data.get('equipmentData', {}).get('Pump', {})
    pump = PumpProperties(
        diameter=pump_raw.get('Diameter', 1.5),
        depth=pump_raw.get('Depth', 5000.0)
    )
    
    # Surface Unit mapping
    su_raw = data.get('equipmentData', {}).get('SurfaceUnit', {})
    surface_unit = SurfaceUnit(
        manufacturer=su_raw.get('PumpingUnitManufacturer', 'Unknown'),
        unit_type=su_raw.get('PumpingUnitGeometry', 'C'),
        stroke_length=su_raw.get('StrokeLengthSetting', 144.0),
        gear_box_rating=su_raw.get('GearBoxRating', 0.0)
    )
    
    # Card Data mapping
    card_raw = data.get('cardData', {})
    surface_card = CardData(
        position=card_raw.get('Position', []),
        load=card_raw.get('Load', [])
    )
    
    return DynacardAnalysisContext(
        api14=data.get('CardDetails', {}).get('Api14', 'ANONYMIZED'),
        surface_card=surface_card,
        rod_string=rods,
        pump=pump,
        surface_unit=surface_unit,
        spm=data.get('InputParameters', {}).get('StrokesPerMinute', 10.0)
    )

@pytest.mark.parametrize("file_path", get_cleansed_files())
def test_algorithm_robustness_with_cleansed_data(file_path):
    """
    Ensures new algorithms correctly process the permanent anonymized dataset.
    """
    ctx = map_cleansed_to_modern(file_path)
    
    if not ctx.surface_card.position or not ctx.surface_card.load:
        pytest.skip(f"Skipping {os.path.basename(file_path)}: Empty card data")
        
    workflow = DynacardWorkflow(ctx)
    results = workflow.run_full_analysis()
    
    assert results is not None
    assert results.downhole_card is not None
    assert len(results.downhole_card.position) == len(ctx.surface_card.position)
    assert results.pump_fillage > 0
    assert "Classification" in results.diagnostic_message
    
    # Verify Anonymization is maintained
    assert "API-CLEANSED" in results.ctx.api14 or "cleansed" in file_path.lower()
