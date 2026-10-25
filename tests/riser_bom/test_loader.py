# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""External JSON contract; rates are visibly invented, not scaled real costs."""

import json
import traceback
from dataclasses import asdict
from pathlib import Path

import pytest

from digitalmodel.riser_bom import cost_bom, load_cost_basis
from tests.riser_bom.test_costing import basis, cited_root, configuration


def test_external_json_runs_end_to_end(tmp_path):
    """Same independent hand sum: 6 + 75 + 20 + 14 + 16.5 = 131.5."""
    payload = {"schema_version": 1, **asdict(basis())}
    external = tmp_path / "basis.json"
    external.write_text(json.dumps(payload), encoding="utf-8")
    loaded = load_cost_basis(external)
    result = cost_bom(configuration(), loaded, citation_root=cited_root(tmp_path))
    assert result["total"] == "131.5"


def payload():
    return {"schema_version": 1, **asdict(basis())}


@pytest.mark.parametrize(
    "change",
    [
        "version",
        "boolean_version",
        "missing_citation",
        "wrong_unit",
        "missing_component",
        "duplicate_component",
        "extra",
        "extra_rate",
        "empty_currency",
        "empty_revision",
        "nan",
        "negative",
        "invalid_kind",
        "rates_not_list",
    ],
)
def test_invalid_external_contract_rejected(tmp_path, change):
    data = payload()
    data["rates"] = list(data["rates"])
    if change == "version":
        data["schema_version"] = 2
    elif change == "boolean_version":
        data["schema_version"] = True
    elif change == "missing_citation":
        del data["rates"][0]["citation"]
    elif change == "wrong_unit":
        data["rates"][0]["unit"] = "m"
    elif change == "missing_component":
        data["rates"].pop()
    elif change == "duplicate_component":
        data["rates"].append(data["rates"][0])
    elif change == "extra":
        data["escalation"] = 2
    elif change == "extra_rate":
        data["rates"][0]["discount"] = 2
    elif change == "empty_currency":
        data["currency"] = " "
    elif change == "empty_revision":
        data["revision"] = " "
    elif change == "nan":
        data["rates"][0]["value"] = "NaN"
    elif change == "negative":
        data["rates"][0]["value"] = "-1"
    elif change == "invalid_kind":
        data["data_kind"] = "normalized"
    else:
        data["rates"] = {}
    external = tmp_path / "invalid.json"
    external.write_text(json.dumps(data), encoding="utf-8")
    with pytest.raises(ValueError, match="Invalid external cost basis"):
        load_cost_basis(external)


def test_duplicate_json_keys_fail_closed(tmp_path):
    external = tmp_path / "duplicate.json"
    external.write_text('{"schema_version": 1, "schema_version": 1}', encoding="utf-8")
    with pytest.raises(ValueError, match="Invalid external cost basis"):
        load_cost_basis(external)


def test_unavailable_basis_does_not_disclose_path(tmp_path):
    with pytest.raises(ValueError, match="External cost basis is unavailable") as error:
        load_cost_basis(tmp_path / "missing.json")
    assert str(tmp_path) not in str(error.value)


@pytest.mark.parametrize("failure", ["symlink_loop", "non_utf8"])
def test_unreadable_basis_errors_are_sanitized(tmp_path, failure):
    external = tmp_path / "unreadable.json"
    document_marker = "unreadable basis contents"
    if failure == "symlink_loop":
        external.symlink_to(external)
    else:
        external.write_bytes(document_marker.encode("ascii") + b"\xff")

    with pytest.raises(ValueError) as error:
        load_cost_basis(external)

    assert type(error.value) is ValueError
    assert str(error.value) == "External cost basis is unavailable"
    public_error = "".join(traceback.format_exception(error.value))
    assert str(external) not in public_error
    assert str(tmp_path) not in public_error
    assert document_marker not in public_error


def test_vendored_basis_rejected_before_read():
    with pytest.raises(ValueError, match="external to the public repository"):
        load_cost_basis(Path(__file__).resolve())


def test_symlink_to_vendored_file_is_rejected(tmp_path):
    external = tmp_path / "link.json"
    external.symlink_to(Path(__file__).resolve())
    with pytest.raises(ValueError, match="external to the public repository"):
        load_cost_basis(external)


def test_private_basis_keeps_privacy_marker(tmp_path):
    """Only the label is private in this test; the values remain invented."""
    data = payload()
    data["data_kind"] = "private"
    external = tmp_path / "private.json"
    external.write_text(json.dumps(data), encoding="utf-8")
    result = cost_bom(
        configuration(), load_cost_basis(external), citation_root=cited_root(tmp_path)
    )
    assert result["data_kind"] == "private"
    assert result["basis_revision"] == "1"
