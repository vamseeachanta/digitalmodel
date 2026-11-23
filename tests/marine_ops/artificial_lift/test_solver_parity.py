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
)
from digitalmodel.marine_ops.artificial_lift.dynacard.solver import DynacardWorkflow
from digitalmodel.marine_ops.artificial_lift.dynacard.physics import DynacardPhysicsSolver

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

    # The default solver's recorded median load nRMSE is rounded to 0.9%.
    # That regression measurement permits values below 0.9 + 0.05 = 0.95%.
    # The other bounds reproduce the issue's published 0.16% median position
    # and 2.15% worst load results, rounded upward by 0.05 percentage point.
    assert np.median(load_nrmse) < 0.95
    assert np.median(position_nrmse) < 0.2
    assert np.max(load_nrmse) < 2.20


def test_coupled_gibbs_parity_with_stored_reference_cards():
    """Check coupled transfer against five stored cards with zero rod damping."""
    load_nrmse = []
    correlations = []
    affine_residuals = []
    for reference_file in REFERENCE_FILES:
        raw = json.loads(reference_file.read_text())
        context = load_from_json_file(reference_file)
        assert all(section.damping_factor == 0.0 for section in context.rod_string)
        expected = get_expected_downhole_card(raw)
        assert expected is not None
        with pytest.warns(UserWarning, match="comparison only: not field validated"):
            results = DynacardPhysicsSolver(context).solve_wave_equation(harmonic_count=15)
        assert results.downhole_load_basis.closing_sample_dropped == (
            reference_file == REFERENCE_FILES[-1]
        )
        if results.downhole_load_basis.closing_sample_dropped:
            # The stored reference repeats the same closing sample as the input.
            assert expected.position[0] == expected.position[-1]
            assert expected.load[0] == expected.load[-1]
            expected = expected.model_copy(update={
                "position": expected.position[:-1], "load": expected.load[:-1],
            })
        card = results.downhole_card
        load_nrmse.append(compare_cards(card, expected).load_nrmse_pct)
        surface_load = np.asarray(context.surface_card.load[:len(card.load)])
        pump_load = np.asarray(card.load)
        correlations.append(np.corrcoef(surface_load, pump_load)[0, 1])
        slope, intercept = np.polyfit(surface_load, pump_load, 1)
        affine_residuals.append(
            float(np.max(np.abs(pump_load - (slope * surface_load + intercept))))
        )

    # Measured median 1.337% and maximum 2.033% are rounded up by 0.05-0.07
    # percentage point. These are regression bounds, not acceptance limits.
    # All five fixtures carry zero rod damping; field calibration is untested.
    assert np.median(load_nrmse) < 1.40
    assert np.max(load_nrmse) < 2.10
    # Fixture 001 measures r=0.991812 at the same sample indices: <0.99 is
    # not established. The measured <0.995 regression bound still excludes
    # the old affine path's r=1; this is not a field accuracy criterion.
    assert all(correlation < 0.995 for correlation in correlations), correlations
    # The #1857 defect was an exactly affine load map (residual 4.5e-12 lbf).
    # A least-squares affine fit of pump load on surface load must now leave a
    # residual far above arithmetic noise on every fixture.
    assert all(residual > 100.0 for residual in affine_residuals), affine_residuals


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
