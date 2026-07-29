import json
from pathlib import Path

import numpy as np
from digitalmodel.marine_ops.artificial_lift.dynacard.comparison import (
    compare_cards,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.data_loader import (
    get_expected_downhole_card,
    load_from_json_file,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.solver import DynacardWorkflow

DATA_DIR = Path(__file__).with_name("test_data")
REFERENCE_FILES = tuple(
    DATA_DIR / f"cleansed_well_{well_number:03d}.json"
    for well_number in range(1, 6)
)


def test_null_fixture_runtime_uses_loader_default():
    """A nullable optional runtime must not make a required fixture unloadable."""
    context = load_from_json_file(DATA_DIR / "cleansed_well_004.json")

    assert context.runtime == 24.0
    assert context.input_params.runtime == 24.0


def test_solver_parity_with_vendor_downhole_cards():
    """Hold the default solver to its published real-card accuracy."""
    comparisons = []
    for reference_file in REFERENCE_FILES:
        # These are required repository fixtures. Opening them directly makes a
        # missing fixture fail instead of turning the only parity check into a skip.
        with reference_file.open() as stream:
            raw_data = json.load(stream)
        context = load_from_json_file(reference_file)
        expected = get_expected_downhole_card(raw_data)
        assert expected is not None

        results = DynacardWorkflow(context).run_full_analysis()
        comparisons.append(compare_cards(results.downhole_card, expected))

    load_nrmse = np.array(
        [comparison.load_nrmse_pct for comparison in comparisons]
    )
    position_nrmse = np.array(
        [comparison.position_nrmse_pct for comparison in comparisons]
    )

    # The class docstring reports median load nRMSE to one decimal place.
    # Its 0.9% claim therefore permits values below 0.9 + 0.05 = 0.95%.
    # The other bounds reproduce the issue's published 0.16% median position
    # and 2.15% worst load results, rounded upward by 0.05 percentage point.
    assert np.median(load_nrmse) < 0.95
    assert np.median(position_nrmse) < 0.2
    assert np.max(load_nrmse) < 2.20
