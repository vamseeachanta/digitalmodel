import json
from pathlib import Path

import pytest

from digitalmodel.marine_ops.artificial_lift.dynacard.comparison import (
    compare_cards,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.data_loader import (
    get_expected_downhole_card,
    load_from_json_file,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.solver import DynacardWorkflow

LOCAL_DATA_DIR = Path(__file__).with_name("test_data")

def get_cleansed_files():
    """Retrieves all anonymized JSON files from the local test data directory."""
    return sorted(LOCAL_DATA_DIR.glob("cleansed_well_*.json"))

@pytest.mark.parametrize("file_path", get_cleansed_files())
def test_algorithm_robustness_with_cleansed_data(file_path):
    """
    Ensures new algorithms correctly process the permanent anonymized dataset.
    """
    with file_path.open() as stream:
        raw_data = json.load(stream)
    ctx = load_from_json_file(file_path)
    expected_downhole = get_expected_downhole_card(raw_data)
    assert expected_downhole is not None

    workflow = DynacardWorkflow(ctx)
    results = workflow.run_full_analysis()

    assert results is not None
    assert results.downhole_card is not None
    assert len(results.downhole_card.position) == len(ctx.surface_card.position)
    assert results.pump_fillage > 0
    assert "Classification" in results.diagnostic_message
    
    comparison = compare_cards(results.downhole_card, expected_downhole)
    # Published worst errors are 2.15% load and 0.41% position. Adding
    # 0.05 percentage point for rounding gives the regression ceilings.
    assert comparison.load_nrmse_pct < 2.20
    assert comparison.position_nrmse_pct < 0.46

    # Verify anonymization is maintained.
    assert "API-CLEANSED" in results.ctx.api14
