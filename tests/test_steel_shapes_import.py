"""Offline workbook import tests; values are reused from the cited sample."""

import hashlib
import json
import socket
from importlib.resources import files

import pytest

from digitalmodel.steel_shapes import get_section_data
from digitalmodel.steel_shapes_import import import_workbook


@pytest.fixture(autouse=True)
def no_network(monkeypatch):
    def denied(*args, **kwargs):
        raise AssertionError("workbook import tests shall remain offline")

    monkeypatch.setattr(socket.socket, "connect", denied)


def _dimension_fields(item):
    dims = item["dimensions"]
    family = item["family"]
    fields = {"A": item["area"], "kdes": dims.get("kdes"), "tnom": dims.get("tnom")}
    if family in {"W", "S", "HP", "C"}:
        fields.update({k: dims[k] for k in ("d", "bf", "tw", "tf")})
    elif family == "L":
        fields.update({"d": dims["d"], "b": dims["b"], "t": dims["t"]})
    elif family == "HSS_RECT":
        fields.update({"Ht": dims["d"], "B": dims["b"], "tdes": dims["t"]})
    else:
        fields.update({"OD": dims["d"], "tdes": dims["t"]})
    return fields


@pytest.fixture
def workbook(tmp_path):
    from openpyxl import Workbook

    data = json.loads(
        files("digitalmodel.data_systems.data")
        .joinpath("steel_shapes_sample.json")
        .read_text()
    )
    book = Workbook()
    sheet = book.active
    sheet.title = "Database v16.0"
    # Same duplicate headings as the source: imperial block followed by SI.
    headings = [
        "Type",
        "EDI_Std_Nomenclature",
        "AISC_Manual_Label",
        "A",
        "d",
        "Ht",
        "OD",
        "bf",
        "B",
        "b",
        "tw",
        "tf",
        "t",
        "tnom",
        "tdes",
        "kdes",
        "EDI_Std_Nomenclature",
        "AISC_Manual_Label",
    ]
    sheet.append(headings)
    for item in data["sections"]:
        family = item["family"]
        fields = _dimension_fields(item)
        row = [fields.get(h) for h in headings]
        row[:3] = [
            "HSS" if family.startswith("HSS") else family,
            item["aliases"][0],
            item["name"],
        ]
        row[-2:] = [item["metric_name"], item["metric_name"]]
        sheet.append(row)
        sheet.cell(
            row=sheet.max_row, column=headings.index("A") + 1
        ).number_format = item["source"].get("area_number_format", "0.00")
    path = tmp_path / "source.xlsx"
    book.save(path)
    return path, data


def test_import_preserves_all_families_and_aliases(workbook, tmp_path):
    path, expected = workbook
    data = import_workbook(path, source_url="https://www.aisc.org/example.xlsx")
    assert len(data["sections"]) == len(expected["sections"])
    assert data["source"]["sha256"] == hashlib.sha256(path.read_bytes()).hexdigest()
    output = tmp_path / "catalog.json"
    output.write_text(json.dumps(data))
    for item in expected["sections"]:
        actual = get_section_data(item["metric_name"], catalog_path=output)
        assert actual["dimensions"] == item["dimensions"]
        assert actual["area"] == item["area"]
        assert (
            actual["source"]["area_number_format"]
            == item["source"]["area_number_format"]
        )


def test_reject_wrong_workbook_edition(workbook):
    from openpyxl import load_workbook

    path, _ = workbook
    book = load_workbook(path)
    book.active.title = "wrong edition"
    book.save(path)
    with pytest.raises(ValueError, match="Database v16.0"):
        import_workbook(path, source_url="https://www.aisc.org/example.xlsx")


def test_reject_missing_metric_heading(workbook):
    from openpyxl import load_workbook

    path, _ = workbook
    book = load_workbook(path)
    book.active.cell(row=1, column=book.active.max_column).value = "missing"
    book.save(path)
    with pytest.raises(ValueError, match="heading"):
        import_workbook(path, source_url="https://www.aisc.org/example.xlsx")


def test_reject_missing_required_shape_value(workbook):
    from openpyxl import load_workbook

    path, _ = workbook
    book = load_workbook(path)
    headings = [c.value for c in book.active[1]]
    book.active.cell(row=2, column=headings.index("tw") + 1).value = None
    book.save(path)
    with pytest.raises(ValueError, match="tw"):
        import_workbook(path, source_url="https://www.aisc.org/example.xlsx")
