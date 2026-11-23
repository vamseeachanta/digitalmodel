# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Operation-selection contracts from issue #2768, knowledge-pack item S10.

These are API acceptance rules, not standards-derived engineering limits.
"""

from dataclasses import replace
import importlib
from unittest.mock import Mock

import pytest

from digitalmodel.marine_ops.operation_envelope import (
    OPERATIONS,
    envelope_for_operations,
    resolve_operation,
)
from tests.marine_ops.test_operation_envelope import _rao_set


@pytest.fixture
def calculation(monkeypatch):
    """Record calls without evaluating a spectrum for selection-only checks."""
    module = importlib.import_module("digitalmodel.marine_ops.operation_envelope")
    calculation = Mock(
        side_effect=lambda rao, op, **kwargs: Mock(operation=resolve_operation(op).key)
    )
    monkeypatch.setattr(module, "operation_envelope", calculation)
    return calculation


def test_empty_operations_rejected_before_calculation(calculation):
    """S10: explicit empty selection must not look like a successful mapping."""
    with pytest.raises(ValueError, match="operations must not be empty"):
        envelope_for_operations(None, operations=[])
    calculation.assert_not_called()


@pytest.mark.parametrize(
    "selection",
    [
        ["transit", "transit"],
        [OPERATIONS["transit"], OPERATIONS["transit"]],
        ["transit", OPERATIONS["transit"]],
        [OPERATIONS["transit"], "transit"],
        [OPERATIONS["transit"], replace(OPERATIONS["transit"], basis="other basis")],
        ["transit", "heavy_lift", "transit"],
    ],
    ids=["strings", "objects", "mixed", "reverse-mixed", "different-basis", "nonadjacent"],
)
def test_repeated_operation_key_rejected_before_calculation(calculation, selection):
    """S10: resolved keys, including different criteria sources, cannot overwrite."""
    with pytest.raises(ValueError, match="duplicate operation key: transit"):
        envelope_for_operations(None, operations=selection)
    calculation.assert_not_called()


def test_unknown_operation_rejected_before_calculation(calculation):
    """Resolution of the whole selection precedes any spectrum evaluation."""
    with pytest.raises(KeyError, match="unknown operation"):
        envelope_for_operations(None, operations=["transit", "unknown"])
    calculation.assert_not_called()


@pytest.mark.parametrize("selection", [None, list(OPERATIONS)], ids=["default", "explicit"])
def test_default_catalogue_order_and_forwarded_arguments(calculation, selection):
    """None and an explicit catalogue each evaluate every key once, in order."""
    rao, periods, headings = object(), object(), object()
    results = envelope_for_operations(
        rao, operations=selection, tp_range_s=periods, headings=headings
    )
    assert list(results) == list(OPERATIONS)
    assert [
        (call.args[0], resolve_operation(call.args[1]))
        for call in calculation.call_args_list
    ] == [
        (rao, operation) for operation in OPERATIONS.values()
    ]
    assert all(
        call.kwargs == {"tp_range_s": periods, "headings": headings}
        for call in calculation.call_args_list
    )


def test_distinct_custom_keys_preserve_selection_order(calculation):
    """Identical definitions with distinct keys are intentionally distinct outputs."""
    custom = replace(OPERATIONS["transit"], key="custom")
    results = envelope_for_operations(None, operations=[custom, "transit"])
    assert list(results) == ["custom", "transit"]
    assert [
        (call.args[0], resolve_operation(call.args[1]))
        for call in calculation.call_args_list
    ] == [
        (None, custom), (None, OPERATIONS["transit"])
    ]


def test_selected_operations_keep_closed_form_limits():
    """Independent hand calculation, envelope-basis.md revision 2, B1/B2/B6.

    Unit heave gain and zero rotation give m0=Hs**2/16 and s=Hs/2.
    Hs_forecast=2*alpha*C: transit=2*0.9*3=5.4 m;
    station keeping=2*0.9*2=3.6 m. Tp=12 s is a synthetic sample;
    the formula is independent of Tp. The existing constant-gain regression's
    relative tolerance of 0.001 is retained for discrete integration.
    """
    # Inputs and tolerance: revision 2 B1/B2/B6 and the existing constant-gain test.
    custom = replace(OPERATIONS["transit"], key="custom")
    results = envelope_for_operations(
        _rao_set(), ["dp_station_keeping", custom], tp_range_s=[12.0]
    )
    assert list(results) == ["dp_station_keeping", "custom"]
    for key, expected in (("dp_station_keeping", 3.6), ("custom", 5.4)):
        result = results[key]
        result.require_evaluated()
        assert result.operation == key
        for point in result.points:
            assert point.governing_dof == "heave"
            assert point.hs_forecast_limit_m == pytest.approx(expected, rel=1e-3)
