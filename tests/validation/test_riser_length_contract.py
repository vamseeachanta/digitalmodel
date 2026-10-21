# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent arithmetic checks for a synthetic segmented riser contract."""

import pytest
from pydantic import ValidationError

from digitalmodel.contract_validation.riser_length import RiserLengthContract


def test_admitted_riser_total_is_preserved():
    """Length additivity: synthetic sections 12.5+7.5+10 = 30 m by hand.

    This is a geometric sum, with no material coefficients or design limits.
    An exact-zero comparison tolerance is supplied for binary-exact inputs.
    """
    contract = RiserLengthContract.model_validate_json(
        '{"segments_m":[12.5,7.5,10.0],"total_m":30.0,'
        '"absolute_tolerance_m":0.0,"units":"m"}'
    )
    restored = RiserLengthContract.model_validate_json(contract.model_dump_json())
    assert restored.total_m == 30.0
    assert restored.segments_m == (12.5, 7.5, 10.0)
    assert restored.units == "m"


def test_total_mismatch_is_rejected():
    with pytest.raises(ValidationError, match="total"):
        RiserLengthContract(
            segments_m=(12.5, 7.5, 10.0),
            total_m=31.0,
            absolute_tolerance_m=0.0,
            units="m",
        )


def test_caller_supplies_tolerance_in_metres():
    """Synthetic 1+2=3 m; 3.125 differs by exactly 0.125 m.

    The caller's synthetic tolerance is not a design acceptance criterion.
    """
    data = dict(segments_m=(1.0, 2.0), total_m=3.125, units="m")
    assert RiserLengthContract(**data, absolute_tolerance_m=0.125).total_m == 3.125
    with pytest.raises(ValidationError):
        RiserLengthContract(**data, absolute_tolerance_m=0.0625)
    with pytest.raises(ValidationError):
        RiserLengthContract(**data)


@pytest.mark.parametrize(
    "change",
    [
        {"segments_m": ()},
        {"segments_m": (0.0,)},
        {"segments_m": (-1.0,)},
        {"segments_m": (float("nan"),)},
        {"segments_m": (True,)},
        {"segments_m": ("1",)},
        {"segments_m": (1e308, 1e308)},
        {"total_m": float("inf")},
        {"total_m": 0.0},
        {"total_m": -1.0},
        {"total_m": True},
        {"absolute_tolerance_m": -1.0},
        {"absolute_tolerance_m": float("inf")},
        {"units": "ft"},
    ],
)
def test_invalid_length_contract_fails_closed(change):
    data = dict(segments_m=(1.0,), total_m=1.0, absolute_tolerance_m=0.0, units="m")
    data.update(change)
    with pytest.raises(ValidationError):
        RiserLengthContract(**data)
