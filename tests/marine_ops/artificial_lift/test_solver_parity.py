import json
from pathlib import Path

import numpy as np
import pytest
from digitalmodel.marine_ops.artificial_lift.dynacard.calculations import (
    calculate_fluid_load,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.comparison import (
    compare_cards,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.data_loader import (
    get_expected_downhole_card,
    load_from_json_file,
    parse_legacy_json,
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


def test_legacy_loader_preserves_survey_station_arrays():
    """Legacy survey stations must reach the analysis context unchanged."""
    context = parse_legacy_json(
        {
            "surveyData": [
                {"MD": 0.0, "Inclination": 1.25, "Azimuth": 2.5},
                {"MD": 100.0, "Inclination": 3.75, "Azimuth": 5.0},
            ]
        }
    )

    assert context.survey is not None
    assert context.survey.measured_depth == [0.0, 100.0]
    assert context.survey.inclination == [1.25, 3.75]
    assert context.survey.azimuth == [2.5, 5.0]


def test_legacy_loader_keeps_null_survey_absent():
    """A null legacy survey must retain the vertical-fallback contract."""
    assert parse_legacy_json({"surveyData": None}).survey is None


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


def test_corner_metrics_match_available_vendor_analyses():
    """Corner-derived metrics remain close to the four usable vendor results."""
    vendor_values = {
        2: (88.37, 6127.0),
        3: (97.52, 6557.0),
        4: (97.92, 7682.0),
        5: (95.40, 3965.0),
    }
    fillage_errors = []
    fluid_load_errors_pct = []

    for well_number, (vendor_fillage, vendor_fluid_load) in vendor_values.items():
        context = load_from_json_file(
            DATA_DIR / f"cleansed_well_{well_number:03d}.json"
        )
        results = DynacardWorkflow(context).run_full_analysis()
        fluid_load = calculate_fluid_load(results.downhole_card).fluid_load
        fillage_errors.append(results.pump_fillage - vendor_fillage)
        fluid_load_errors_pct.append(
            100.0 * (fluid_load - vendor_fluid_load) / vendor_fluid_load
        )

    # The lowest-fillage fixture is the regression that exposes the old bias.
    # The two near-full wells retain their pre-fix accuracy instead of paying
    # for the improvement on wells 002 and 005.
    assert abs(fillage_errors[0]) < 3.0
    assert abs(fillage_errors[1]) < 0.7
    assert abs(fillage_errors[2]) < 1.0
    assert abs(fillage_errors[3]) < 1.0
    assert np.mean(np.abs(fillage_errors)) < 1.0
    assert np.mean(np.abs(fluid_load_errors_pct)) < 6.0

    # Honest coverage limit: these fixtures span 88.37-97.92% vendor fillage.
    # No trustworthy severe fluid-pound card is available here, so this test
    # does not establish accuracy near the unusable previous-project 54% card.
