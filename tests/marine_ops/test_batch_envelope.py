# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Allowable Hs tables checked against constant-gain and point-transfer cases."""

from dataclasses import replace

import numpy as np
import pandas as pd
import pytest

from digitalmodel.marine_ops.batch_envelope import (
    BatchOperation,
    ResponseCriterion,
    allowable_hs_table,
)
from digitalmodel.marine_ops.operation_envelope import operation_envelope
from tests.marine_ops.test_operation_envelope import _rao_set


def _operation(key="lift", limit=1.0, **kwargs):
    return BatchOperation(
        key, (ResponseCriterion("tip_heave", "heave", limit, "m", **kwargs),)
    )


def test_closed_form_batch_grid_and_csv(tmp_path):
    rao = _rao_set(heave=2.0)
    rao.heave.magnitude[:, 2] = 4.0
    output = tmp_path / "allowable.csv"
    frame = allowable_hs_table(
        rao, [_operation(), _operation("lowering", 0.5)],
        tp_range_s=[8.0, 12.0], headings=[0.0, 90.0], csv_path=output,
    )
    # s = gain * Hs / 2, so Hs = 2 * limit / gain.
    assert len(frame) == 8
    assert frame.hs_limit_m.tolist() == pytest.approx([1, 0.5, 1, 0.5, 0.5, 0.25, 0.5, 0.25])
    assert frame.operation.tolist() == ["lift"] * 4 + ["lowering"] * 4
    assert frame.tp_s.tolist() == [8, 8, 12, 12] * 2
    assert frame.heading_deg.tolist() == [0, 90] * 4
    assert set(frame.governing_criterion) == {"tip_heave"}
    assert set(frame.status) == {"evaluated"}
    pd.testing.assert_frame_equal(pd.read_csv(output), frame, check_dtype=False)


def test_governing_criterion_is_selected_per_heading():
    rao = _rao_set(heave=1.0, roll=2.0)
    rao.heave.magnitude[:, 2] = 4.0
    op = BatchOperation("transit", (
        ResponseCriterion("heave_motion", "heave", 1.0, "m"),
        ResponseCriterion("roll_motion", "roll", 1.0, "deg"),
    ))
    frame = allowable_hs_table(rao, [op], tp_range_s=[8], headings=[0, 90])
    assert frame.governing_criterion.tolist() == ["roll_motion", "heave_motion"]
    assert frame.hs_limit_m.tolist() == pytest.approx([1, 0.5])


def test_governing_criterion_can_change_with_period():
    op = BatchOperation("lift", (
        ResponseCriterion("motion", "heave", 1, "m"),
        ResponseCriterion("speed", "heave", 1, "m/s", response="velocity"),
    ))
    frame = allowable_hs_table(_rao_set(), [op], tp_range_s=[4, 16], headings=[0])
    assert frame.governing_criterion.tolist() == ["speed", "motion"]


def test_criterion_label_does_not_determine_numerical_status():
    op = BatchOperation("lift", (ResponseCriterion("cap", "heave", 1, "m"),))
    frame = allowable_hs_table(_rao_set(), [op], tp_range_s=[8], headings=[0])
    assert frame.status.iloc[0] == "evaluated"
    assert frame.hs_limit_m.iloc[0] == pytest.approx(2)


def test_matches_existing_envelope_and_alpha():
    rao = _rao_set(heave=1.0)
    op = replace(_operation(limit=3), alpha=0.9)
    frame = allowable_hs_table(rao, [op], tp_range_s=[8, 12], headings=[0])
    old = operation_envelope(rao, "transit", tp_range_s=[8, 12], headings=[0])
    assert frame.hs_limit_m.tolist() == pytest.approx([p.hs_limit_m for p in old.points])
    assert set(frame.qualification_status) == {"NOT_EVALUATED"}


@pytest.mark.parametrize("response,unit,order", [
    ("velocity", "m/s", 1), ("acceleration", "m/s^2", 2),
])
def test_derivative_response_matches_independent_quadrature(response, unit, order):
    rao = _rao_set(heave=1.0)
    op = BatchOperation("lowering", (
        ResponseCriterion(response, "heave", 0.5, unit, response=response),
    ))
    frame = allowable_hs_table(
        rao, [op], tp_range_s=[8], headings=[0], spectrum_type="pierson_moskowitz",
    )
    # Independent fine gamma=1 quadrature over the envelope's retained band.
    wp = 2 * np.pi / 8
    x = np.linspace(0.01, 8, 100001)
    shape = 5 * x**-5 * np.exp(-1.25 / x**4)
    trap = getattr(np, "trapezoid", None) or np.trapz
    moment = trap((wp * x)**(2 * order) * shape, x) / trap(shape, x) / 16
    assert frame.hs_limit_m.iloc[0] == pytest.approx(0.5 / (2 * np.sqrt(moment)), rel=1e-4)
    assert frame.governing_criterion.iloc[0] == response


def test_point_transfer_preserves_complex_phase_and_input():
    rao = _rao_set(heave=1.0, pitch=180 / np.pi)
    rao.pitch.phase[:] = 90
    before = rao.heave.magnitude.copy()
    op = _operation(point_m=(1, 0, 0))
    frame = allowable_hs_table(rao, [op], tp_range_s=[8], headings=[0])
    # Existing transfer: H_tip = 1 + i; gain sqrt(2).
    assert frame.hs_limit_m.iloc[0] == pytest.approx(np.sqrt(2))
    np.testing.assert_array_equal(rao.heave.magnitude, before)


def test_unenhanced_spectrum_equals_jonswap_with_gamma_one():
    kwargs = dict(tp_range_s=[8], headings=[0])
    unenhanced = allowable_hs_table(_rao_set(), [_operation()], spectrum_type="pierson_moskowitz", **kwargs)
    js = allowable_hs_table(_rao_set(), [_operation()], gamma=1, **kwargs)
    assert unenhanced.hs_limit_m.iloc[0] == js.hs_limit_m.iloc[0]


def test_insufficient_coverage_and_cap_are_explicit():
    rao = _rao_set()
    for component in (rao.heave, rao.roll):
        component.frequencies = replace(component.frequencies, values=np.linspace(0.5, 1, 6))
    op = BatchOperation("lift", (
        ResponseCriterion("motion", "heave", 1, "m"),
        ResponseCriterion("speed", "roll", 100, "deg/s", response="velocity"),
    ))
    frame = allowable_hs_table(rao, [op], tp_range_s=[8], headings=[0])
    assert pd.isna(frame.hs_limit_m.iloc[0])
    assert frame.status.iloc[0] == "insufficient_coverage"
    assert frame.governing_criterion.iloc[0] == "coverage"
    zero = allowable_hs_table(_rao_set(heave=0), [_operation()], tp_range_s=[8], headings=[0])
    assert zero.hs_limit_m.iloc[0] == 20
    assert zero.status.iloc[0] == "capped"
    assert zero.governing_criterion.iloc[0] == "cap"


def test_non_governing_criterion_still_gates_coverage_and_csv(tmp_path):
    rao = _rao_set(heave=1, roll=0.001)
    rao.roll.frequencies = replace(rao.roll.frequencies, values=np.linspace(0.5, 1, 6))
    op = BatchOperation("lift", (
        ResponseCriterion("motion", "heave", 1, "m"),
        ResponseCriterion("speed", "roll", 100, "deg/s", response="velocity"),
    ))
    output = tmp_path / "coverage.csv"
    frame = allowable_hs_table(rao, [op], tp_range_s=[8], headings=[0], csv_path=output)
    assert frame.status.iloc[0] == "insufficient_coverage"
    assert pd.isna(frame.hs_limit_m.iloc[0])
    assert pd.isna(pd.read_csv(output).hs_limit_m.iloc[0])


def test_requested_and_actual_heading_are_distinct_and_circular():
    frame = allowable_hs_table(_rao_set(), [_operation()], tp_range_s=[8], headings=[359])
    assert frame.heading_deg.iloc[0] == 359
    assert frame.governing_heading_deg.iloc[0] == 0


@pytest.mark.parametrize("kwargs,match", [
    ({"tp_range_s": []}, "Tp"),
    ({"tp_range_s": [0]}, "Tp"),
    ({"tp_range_s": [8, 8]}, "duplicates"),
    ({"headings": []}, "headings"),
    ({"headings": [0, 360]}, "duplicates"),
    ({"headings": [np.nan]}, "headings"),
    ({"headings": [270]}, "heading"),
    ({"gamma": 0.5}, "gamma"),
    ({"gamma": np.inf}, "gamma"),
    ({"hs_cap_m": 0}, "cap"),
])
def test_invalid_calculation_parameters_are_rejected(kwargs, match):
    parameters = dict(tp_range_s=[8], headings=[0])
    parameters.update(kwargs)
    with pytest.raises(ValueError, match=match):
        allowable_hs_table(_rao_set(), [_operation()], **parameters)


@pytest.mark.parametrize("change,match", [
    ({"unit": "deg"}, "unit"),
    ({"response": "load"}, "response"),
    ({"max_significant": 0}, "limit"),
    ({"point_m": (1, 2)}, "point"),
    ({"point_m": (0, np.nan, 0)}, "point"),
    ({"dof": "roll", "unit": "deg", "point_m": (0, 0, 0)}, "translation"),
])
def test_invalid_criteria_are_rejected(change, match):
    criterion = replace(_operation().criteria[0], **change)
    with pytest.raises(ValueError, match=match):
        allowable_hs_table(_rao_set(), [BatchOperation("lift", (criterion,))],
                           tp_range_s=[8], headings=[0])


@pytest.mark.parametrize("operations,match", [
    ([], "empty"),
    ([_operation(), _operation()], "duplicate operation"),
    ([BatchOperation("lift", ())], "criteria"),
    ([replace(_operation(), alpha=1.1)], "alpha"),
    ([BatchOperation("lift", (_operation().criteria[0],) * 2)], "duplicate criterion"),
])
def test_invalid_operations_are_rejected(operations, match):
    with pytest.raises(ValueError, match=match):
        allowable_hs_table(_rao_set(), operations, tp_range_s=[8], headings=[0])


def test_point_transfer_rejects_mismatched_grids_and_bad_phase():
    rao = _rao_set()
    rao.pitch.phase[0, 0] = np.nan
    with pytest.raises(ValueError, match="phase"):
        allowable_hs_table(rao, [_operation(point_m=(1, 0, 0))], tp_range_s=[8], headings=[0])
    rao.pitch.phase[:] = 0
    rao.pitch.headings = replace(rao.pitch.headings, values=rao.pitch.headings.values + 1)
    with pytest.raises(ValueError, match="grid"):
        allowable_hs_table(rao, [_operation(point_m=(1, 0, 0))], tp_range_s=[8], headings=[0])


def test_unsupported_spectrum_is_rejected_before_csv_write(tmp_path):
    output = tmp_path / "not-written.csv"
    with pytest.raises(ValueError, match="spectrum"):
        allowable_hs_table(_rao_set(), [_operation()], tp_range_s=[8], headings=[0],
                           spectrum_type="unknown", csv_path=output)
    assert not output.exists()
