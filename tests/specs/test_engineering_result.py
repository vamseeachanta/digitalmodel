# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Scalar-result contracts preserve engineering values across serialization."""

import pytest
from pydantic import ValidationError

from digitalmodel.specs.engineering_result import EngineeringResult


def test_pressure_round_trip_preserves_value_and_units():
    """OpenStax UP1 (2016), 14.1, eq. 14.4: gauge p = rho*g*h.

    Synthetic rho=1000 kg/m³, g=10 m/s², h=2.5 m give
    1000*10*2.5 = 25000 Pa by independent hand multiplication.
    This schema admits a result; it does not calculate or qualify that result.
    """
    result = EngineeringResult.model_validate_json(
        '{"quantity":"gauge pressure","value":25000.0,"units":"Pa",'
        '"source":"OpenStax UP1 (2016), 14.1, eq. 14.4"}'
    )
    restored = EngineeringResult.model_validate_json(result.model_dump_json())
    assert restored.value == 25000.0
    assert restored.units == "Pa"
    assert restored == result


@pytest.mark.parametrize("value", [True, "25000", float("nan"), float("inf")])
def test_result_rejects_non_numeric_or_nonfinite_values(value):
    with pytest.raises(ValidationError):
        EngineeringResult(quantity="pressure", value=value, units="Pa", source="hand")


@pytest.mark.parametrize("field", ["quantity", "units", "source"])
def test_result_requires_nonblank_metadata(field):
    data = dict(quantity="pressure", value=25000.0, units="Pa", source="hand")
    data[field] = "  "
    with pytest.raises(ValidationError):
        EngineeringResult(**data)


def test_result_rejects_unknown_fields_and_mutation():
    data = dict(quantity="pressure", value=25000.0, units="Pa", source="hand")
    with pytest.raises(ValidationError):
        EngineeringResult(**data, unit="kPa")
    result = EngineeringResult(**data)
    with pytest.raises(ValidationError):
        result.value = 25.0
