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


def source_text(records, format):
    """Independently encode literal source records using the documented wire format."""
    if format == "json":
        return json.dumps(records)
    fields = list(dict.fromkeys(key for record in records for key in record))
    if not fields:
        return ""
    stream = io.StringIO(newline="")
    writer = csv.DictWriter(stream, fieldnames=fields, lineterminator="\n")
    writer.writeheader()
    for record in records:
        writer.writerow({key: json.dumps(value) for key, value in record.items()})
    return stream.getvalue()


def load_text(text, format, units=UNITS):
    """Load a text representation through the public reader under test."""
    reader = getattr(SolverReferenceExporter, f"from_{format}")
    return reader(text, supported_units=units)


@pytest.mark.parametrize("format", ["json", "csv"])
def test_readers_preserve_literal_condition_evidence(format):
    """By inspection: each zero keeps its declared state and each source field.

    This is an identity calculation: output[source key] = input[source key].
    Null or missing value gives absent; text gives invalid; a vendor state gives
    unknown. These are protocol outcomes, with no physical acceptance limit.
    """
    missing = row(condition_id="missing", value=None)
    missing.pop("warnings")
    records = [
        row(
            condition_id="cached",
            value_state="cached_unqualified",
            warnings=["Outside applicability range", 'Warning, quoted: "x"\nnext'],
            status={"applicability": "unknown"},
            execution={
                "exit_code": 0,
                "instance_handle": "opaque",
                "fresh_output": False,
            },
        ),
        row(condition_id="pending", value_state="uncomputed", warnings=None),
        row(condition_id="vendor", value_state="vendor-opaque", warnings=[]),
        row(condition_id="invalid", value="NaN"),
        missing,
    ]
    result = json.loads(load_text(source_text(records, format), format).to_json())
    for original, actual in zip(records, result):
        assert {key: actual[key] for key in original} == original
        assert actual["export_evaluation"] == "NOT_EVALUATED"
    assert [item["export_value_state"] for item in result] == [
        "cached_unqualified",
        "uncomputed",
        "unknown",
        "invalid",
        "absent",
    ]
    assert "warnings" not in result[-1]


@pytest.mark.parametrize("first", ["json", "csv"])
@pytest.mark.parametrize("second", ["json", "csv"])
def test_export_readback_across_formats_preserves_annotations(first, second):
    """The independently enumerated answer is the original evidence plus annotations."""
    record = row(value_state="cached_unqualified", warnings=["Unqualified"])
    expected = [
        {
            **record,
            "export_value_state": "cached_unqualified",
            "export_evaluation": "NOT_EVALUATED",
        }
    ]
    initial = SolverReferenceExporter([record], supported_units=UNITS)
    loaded = load_text(getattr(initial, f"to_{first}")(), first)
    reloaded = load_text(getattr(loaded, f"to_{second}")(), second)
    assert json.loads(reloaded.to_json()) == expected


@pytest.mark.parametrize("format", ["json", "csv"])
@pytest.mark.parametrize(
    "change",
    [
        {"export_value_state": "computed", "export_evaluation": "NOT_EVALUATED"},
        {"export_value_state": "cached_unqualified", "export_evaluation": "PASS"},
        {"export_evaluation": "NOT_EVALUATED"},
        {"export_value_state": "cached_unqualified"},
    ],
)
def test_readers_reject_inconsistent_or_incomplete_annotations(format, change):
    record = row(value_state="cached_unqualified", **change)
    with pytest.raises(ValueError, match="annotations"):
        load_text(source_text([record], format), format)


@pytest.mark.parametrize("format", ["json", "csv"])
def test_readers_revalidate_units_in_existing_exports(format):
    record = row(
        source_unit="kN",
        export_value_state="computed",
        export_evaluation="NOT_EVALUATED",
    )
    with pytest.raises(ValueError, match="unit"):
        load_text(source_text([record], format), format)


@pytest.mark.parametrize(
    "text",
    [
        '[{"warnings": ["Lost warning"], "warnings": []}]',
        '[{"execution": {"fresh_output": false, "fresh_output": true}}]',
    ],
)
def test_json_reader_rejects_duplicate_keys_before_evidence_is_lost(text):
    # RFC 8259 (2017), section 4: duplicate names have unpredictable behavior.
    with pytest.raises(ValueError, match="Duplicate JSON"):
        load_text(text, "json")


@pytest.mark.parametrize("text", ["null", "{}", '"text"', "[null]", "[[]]", "["])
def test_json_reader_requires_an_array_of_records(text):
    with pytest.raises(ValueError):
        load_text(text, "json")


@pytest.mark.parametrize("format", ["json", "csv"])
@pytest.mark.parametrize("token", ["NaN", "Infinity", "-Infinity", "1e9999"])
def test_readers_reject_nonfinite_numeric_tokens_anywhere(format, token):
    # RFC 8259 (2017), section 6; overflow is also outside finite storage.
    if format == "json":
        text = source_text([row(details="TOKEN")], format).replace('"TOKEN"', token)
    else:
        # A plain cell is an independent JSON document under the export contract.
        text = source_text([row(details="TOKEN")], format).replace('"""TOKEN"""', token)
    with pytest.raises(ValueError, match="finite"):
        load_text(text, format)


@pytest.mark.parametrize(
    "text",
    [
        "condition_id,quantity,source_unit,warnings,warnings\n",
        "condition_id,quantity\n",
        'condition_id,quantity,source_unit\n"unterminated',
        "condition_id,quantity,source_unit\n\n",
        "condition_id,quantity,source_unit\nnull,null\n",
        "condition_id,quantity,source_unit\nnull,null,null,null\n",
        "condition_id,quantity,source_unit\nplain,plain,plain\n",
    ],
)
def test_csv_reader_rejects_ambiguous_or_malformed_tables(text):
    with pytest.raises(ValueError):
        load_text(text, "csv")


def test_csv_reader_rejects_duplicate_json_keys_inside_cells():
    text = source_text([row(details={"status": "unknown"})], "csv")
    text = text.replace(
        '{""status"": ""unknown""}', '{""status"": ""unknown"", ""status"": ""ready""}'
    )
    with pytest.raises(ValueError, match="Duplicate JSON"):
        load_text(text, "csv")


@pytest.mark.parametrize("format", ["json", "csv"])
def test_empty_export_readback(format):
    initial = SolverReferenceExporter([], supported_units=UNITS)
    loaded = load_text(getattr(initial, f"to_{format}")(), format)
    assert loaded.to_json() == "[]\n"
    assert loaded.to_csv() == ""


@pytest.mark.parametrize("format", ["json", "csv"])
@pytest.mark.parametrize("text", [None, [], b"[]"])
def test_readers_require_text(format, text):
    with pytest.raises(ValueError, match="text"):
        load_text(text, format)


@pytest.mark.parametrize("format", ["json", "csv"])
def test_readback_keeps_missing_null_and_empty_warning_evidence_distinct(format):
    """Absence is no key, null is None, and an observed empty list is []."""
    missing = row()
    missing.pop("warnings")
    records = [missing, row(warnings=None), row(warnings=[])]
    loaded = load_text(source_text(records, format), format)
    for next_format in ("json", "csv"):
        actual = json.loads(
            load_text(getattr(loaded, f"to_{next_format}")(), next_format).to_json()
        )
        assert "warnings" not in actual[0]
        assert actual[1]["warnings"] is None
        assert actual[2]["warnings"] == []


@pytest.mark.parametrize("format", ["json", "csv"])
def test_claimed_computation_does_not_establish_a_fresh_qualified_run(format):
    """An exit token and a numeric value have no run-specific output proof."""
    source = row(
        execution={"exit_code": 0, "instance_handle": "opaque"},
        status="unknown",
        warnings=["Fresh output not established"],
    )
    actual = json.loads(load_text(source_text([source], format), format).to_json())[0]
    assert actual["execution"] == source["execution"]
    assert actual["status"] == "unknown"
    assert actual["export_value_state"] == "computed"  # Source claim only.
    assert actual["export_evaluation"] == "NOT_EVALUATED"


def test_documented_worked_example_has_the_enumerated_answer(capsys):
    """Expected states follow literal input tokens, independent of any solver.

    The same raw zero occurs in computed, uncomputed and cached rows. Missing
    output maps to absent; source text maps to invalid; no state maps to unknown.
    CSV -> JSON readback must leave the literal warning and unit spellings intact.
    """
    import runpy
    from pathlib import Path

    path = (
        Path(__file__).resolve().parents[2]
        / "examples"
        / "domains"
        / "data_systems"
        / "solver_reference_roundtrip.py"
    )
    runpy.run_path(str(path), run_name="__main__")
    actual = json.loads(capsys.readouterr().out)
    assert [record["export_value_state"] for record in actual] == [
        "computed",
        "uncomputed",
        "cached_unqualified",
        "absent",
        "invalid",
        "unknown",
    ]
    assert [record.get("value") for record in actual] == [0, 0, 0, None, "NaN", 0]
    assert [record["source_unit"] for record in actual] == ["N"] * len(actual)
    assert actual[2]["warnings"] == ["Applicability unknown, cached output"]
    assert "warnings" not in actual[-1]
    assert "value_state" not in actual[-1]
    assert actual[3]["execution"] == {"exit_code": 0, "instance_handle": "opaque"}
    assert all(record["export_evaluation"] == "NOT_EVALUATED" for record in actual)
