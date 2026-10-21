# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Synthetic protocol examples for issue 2226; no engineering reference values.

Expected values are exact copies of the supplied records. Zero is deliberately
used as the same raw token in every state, so state cannot be inferred from its
magnitude. No equations, conversion factors or acceptance limits are exercised.
"""

import csv
import io
import json
from copy import deepcopy

import pytest

from digitalmodel.data_systems.solver_reference_export import SolverReferenceExporter


UNITS = {"force": {"N"}}  # Caller-declared identity only; no unit conversion.


def row(**changes):
    """Return a synthetic row, with independent condition and source metadata."""
    return {
        "condition_id": "condition-a",
        "quantity": "force",
        "value": 0,
        "source_unit": "N",
        "value_state": "computed",
        "warnings": [],
        "source_id": "synthetic-source",
        "revision": "synthetic-revision",
        **changes,
    }


def exported(records):
    return json.loads(SolverReferenceExporter(records, supported_units=UNITS).to_json())


def test_json_preserves_condition_warnings_units_and_provenance():
    records = [
        row(
            warnings=["Outside method range", "Applicability not established"],
            details={"readiness": "unverified", "source_digest": "synthetic"},
        ),
        row(condition_id="condition-b", value=12.5),
    ]
    before = deepcopy(records)
    result = exported(records)
    for original, actual in zip(records, result):
        assert {key: actual[key] for key in original} == original
        assert actual["export_evaluation"] == "NOT_EVALUATED"
    assert result[0]["warnings"] != result[1]["warnings"]
    assert records == before


@pytest.mark.parametrize(
    "state",
    [
        "computed",
        "absent",
        "invalid",
        "uncomputed",
        "cached_unqualified",
        "unknown",
    ],
)
def test_zero_never_establishes_computation_or_qualification(state):
    result = exported([row(value_state=state)])[0]
    assert result["value"] == 0
    assert result["value_state"] == state
    assert result["export_value_state"] == state
    assert result["export_evaluation"] == "NOT_EVALUATED"


def test_missing_state_and_vendor_state_remain_unknown():
    missing = row()
    missing.pop("value_state")
    result = exported([missing, row(value_state="vendor-opaque")])
    assert "value_state" not in result[0]
    assert result[0]["export_value_state"] == "unknown"
    assert result[1]["value_state"] == "vendor-opaque"
    assert result[1]["export_value_state"] == "unknown"


@pytest.mark.parametrize("value", [None, "not-a-number", True, [], {}])
def test_absent_and_invalid_values_override_computed_claim(value):
    result = exported([row(value=value)])[0]
    assert result["value"] == value
    assert result["value_state"] == "computed"  # Source claim is retained.
    assert result["export_value_state"] == ("absent" if value is None else "invalid")


def test_missing_value_remains_absent_without_inserting_zero():
    source = row()
    source.pop("value")
    actual = exported([source])[0]
    assert "value" not in actual
    assert actual["export_value_state"] == "absent"


@pytest.mark.parametrize("unit", ["kN", "", None])
def test_unsupported_or_undeclared_units_are_rejected(unit):
    with pytest.raises(ValueError, match="unit"):
        exported([row(source_unit=unit)])


def test_unit_allowlist_is_quantity_specific():
    with pytest.raises(ValueError, match="unit"):
        exported([row(quantity="power")])


@pytest.mark.parametrize("warnings", ["warning", [None], {"warning": True}])
def test_malformed_warning_fields_are_rejected(warnings):
    with pytest.raises(ValueError, match="warnings"):
        exported([row(warnings=warnings)])


def test_missing_warning_evidence_is_not_replaced_by_empty_list():
    source = row()
    source.pop("warnings")
    actual = exported([source])[0]
    assert "warnings" not in actual
    assert actual["export_evaluation"] == "NOT_EVALUATED"


def test_csv_cells_are_json_encoded_to_retain_types_and_warning_boundaries():
    records = [
        row(warnings=["Warning, with comma", 'Warning "quoted"\nnext line']),
        row(condition_id="condition-b", value=None, extra={"unit": "N"}),
    ]
    exporter = SolverReferenceExporter(records, supported_units=UNITS)
    csv_rows = list(csv.DictReader(io.StringIO(exporter.to_csv())))
    decoded = [
        {key: json.loads(value) for key, value in item.items() if value != ""}
        for item in csv_rows
    ]
    assert decoded == json.loads(exporter.to_json())
    assert "extra" not in decoded[0]
    assert decoded[1]["value"] is None


def test_export_snapshot_is_not_changed_by_caller_or_allowlist_mutation():
    source, units = row(warnings=["Unqualified"]), deepcopy(UNITS)
    exporter = SolverReferenceExporter([source], supported_units=units)
    before = exporter.to_json()
    source["warnings"].clear()
    source["value"] = 12.5
    units["force"].clear()
    assert exporter.to_json() == before


@pytest.mark.parametrize("field", ["export_value_state", "export_evaluation"])
def test_source_cannot_overwrite_export_annotations(field):
    with pytest.raises(ValueError, match="reserved"):
        exported([row(**{field: "qualified"})])


@pytest.mark.parametrize("value", [float("nan"), float("inf"), -float("inf")])
def test_nonfinite_values_require_explicit_source_text(value):
    # JSON cannot represent these numeric tokens (RFC 8259, section 6, 2017).
    with pytest.raises(ValueError, match="finite"):
        exported([row(value=value)])


def test_textual_invalid_tokens_are_retained_without_qualification():
    result = exported([row(value="NaN", value_state="invalid")])[0]
    assert result["value"] == "NaN"
    assert result["export_value_state"] == "invalid"


@pytest.mark.parametrize("field", ["condition_id", "quantity"])
def test_missing_condition_or_quantity_is_rejected(field):
    source = row()
    source.pop(field)
    with pytest.raises(ValueError, match=field):
        exported([source])


def test_empty_exports_are_well_defined():
    exporter = SolverReferenceExporter([], supported_units=UNITS)
    assert json.loads(exporter.to_json()) == []
    assert exporter.to_csv() == ""


def test_successful_helper_without_output_remains_absent():
    # Issue 2226: process success and an opaque handle do not prove execution.
    result = exported(
        [
            row(
                value=None,
                execution={
                    "exit_code": 0,
                    "instance_handle": "opaque",
                    "fresh_output": False,
                },
            )
        ]
    )[0]
    assert result["execution"] == {
        "exit_code": 0,
        "instance_handle": "opaque",
        "fresh_output": False,
    }
    assert result["export_value_state"] == "absent"
    assert result["export_evaluation"] == "NOT_EVALUATED"


def test_csv_distinguishes_missing_null_and_empty_warning_evidence():
    missing = row()
    missing.pop("warnings")
    records = [missing, row(warnings=None), row(warnings=[])]
    result = list(
        csv.DictReader(
            io.StringIO(
                SolverReferenceExporter(records, supported_units=UNITS).to_csv()
            )
        )
    )
    assert result[0]["warnings"] == ""
    assert json.loads(result[1]["warnings"]) is None
    assert json.loads(result[2]["warnings"]) == []


@pytest.mark.parametrize(
    "units", [None, ["N"], {"force": "N"}, {"force": None}, {"force": {None}}]
)
def test_malformed_unit_contracts_fail_before_export(units):
    with pytest.raises(ValueError, match="supported_units"):
        SolverReferenceExporter([row()], supported_units=units)


@pytest.mark.parametrize(
    "details", [{1: "non-string key"}, ("tuple",), {"value": float("nan")}]
)
def test_nested_source_evidence_cannot_be_silently_coerced(details):
    with pytest.raises(ValueError, match="JSON"):
        exported([row(details=details)])


def test_mixed_condition_states_survive_same_export():
    states = ["computed", "uncomputed", "cached_unqualified", "unknown"]
    records = [row(condition_id=state, value_state=state) for state in states]
    result = exported(records)
    assert [item["condition_id"] for item in result] == states
    assert [item["export_value_state"] for item in result] == states
    assert all(item["value"] == 0 for item in result)
