"""Independent per-cell evidence for every formula in the scoped workbooks.

Text results compare exactly. Numeric results use relative 1e-9 and absolute
1e-12, with no relaxed tolerance.
"""

import importlib.util
import json
from collections import Counter
from functools import lru_cache
from pathlib import Path
import re
import shutil
import sys

import pytest

from digitalmodel.geotechnical.slope_input_labels import (
    MaterialLabel,
    MaterialLabelInput,
    PolygonLabelInput,
    ReinforcementInput,
    ReinforcementRule,
    WaterLoadInput,
    distributed_load_guidance,
    material_name,
    polygon_material_name,
    reinforcement_properties,
)


HERE = Path(__file__).parent
REPOSITORY = HERE.parents[2]
FIXTURES = tuple(HERE / "fixtures" / name for name in (
    "xslope_simple1.json", "xslope_clay_blanket.json",
))


@lru_cache(maxsize=None)
def read_live_evidence(source):
    """Use the committed extractor, preserving direct workbook comparison."""
    path = REPOSITORY / source
    if not path.is_file():
        pytest.skip("Source workbook is absent; committed JSON remains testable")
    spec = importlib.util.spec_from_file_location(
        "formula_evidence_extractor", HERE / "formula_evidence.py",
    )
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module.extract_workbook(path)


def material_rows(evidence):
    cells = evidence["constants"]["mat"]
    return tuple(
        MaterialLabel(cells[f"A{row}"], cells[f"B{row}"])
        for row in range(11, 26)
    )


def hand_material_name(key, evidence):
    """Independent hand formula: scan the referenced full-column input range.

    First matching A entry returns B. A blank B concatenated with "" is "". This
    scans the extracted source columns directly rather than calling helpers.
    """
    cells = evidence["constants"]["mat"]
    for address, identifier in cells.items():
        if address.startswith("A") and identifier == key:
            name = cells["B" + address[1:]]
            return "" if name is None else str(name)
    return ""


def formula_cases():
    cases = []
    for path in FIXTURES:
        evidence = json.loads(path.read_text(encoding="utf-8"))
        for record in evidence["formulas"]:
            cases.append(pytest.param(
                path, record["sheet"], record["cell"],
                id=f"{path.stem}-{record['sheet']}-{record['cell']}",
            ))
    return cases


def compare_formula(evidence, sheet, address):
    record = next(
        f for f in evidence["formulas"] if (f["sheet"], f["cell"]) == (sheet, address)
    )
    cells = evidence["constants"][sheet]
    if sheet == "profile":
        m = re.fullmatch(
            r'=_xlfn.XLOOKUP\(([A-Z]+5),mat!\$A:\$A, mat!\$B:\$B,""\) & ""',
            record["formula"],
        )
        assert m, record
        key = cells[m[1]]
        actual = material_name(MaterialLabelInput(key), material_rows(evidence))
        expected = hand_material_name(key, evidence)
    elif sheet == "polygon":
        m = re.fullmatch(
            r'=IF\(OR\((?P<col>[A-Z]+)5="",(?P=col)5="material"\),'
            r'_xlfn.XLOOKUP\((?P=col)6,mat!\$A:\$A, mat!\$B:\$B,""\) & "",""\)',
            record["formula"],
        )
        assert m, record
        region_type = cells[m["col"] + "5"]
        key = cells[m["col"] + "6"]
        actual = polygon_material_name(
            PolygonLabelInput(key, region_type), material_rows(evidence),
        )
        expected = (
            hand_material_name(key, evidence)
            if region_type is None or region_type.lower() in ("", "material") else ""
        )
    elif sheet == "reinforce":
        match = re.fullmatch(
            r'=IF\(\$G(\d+)="","",IFERROR\(VLOOKUP\(\$G\1,'
            r'\$([A-Z]+)\$8:\$([A-Z]+)\$11,([23]),0\),""\)\)',
            record["formula"],
        )
        assert match, record
        first_col, last_col = match[2], match[3]
        middle_col = "AA" if first_col == "Z" else "AC"
        rules = tuple(ReinforcementRule(
            cells[f"{first_col}{row}"], cells[f"{middle_col}{row}"],
            cells[f"{last_col}{row}"],
        ) for row in range(8, 12))
        selected = cells["G" + match[1]]
        output_index = int(match[4]) - 2
        actual = reinforcement_properties(ReinforcementInput(selected), rules)[output_index]
        # Independent hand VLOOKUP: inspect the actual first table column,
        # find the first exact row and offset the requested return column.
        expected = ""
        if selected is not None and selected != "":
            result_col = middle_col if output_index == 0 else last_col
            for row in range(8, 12):
                if cells[f"{first_col}{row}"].lower() == selected.lower():
                    value = cells[f"{result_col}{row}"]
                    expected = 0 if value is None else value
                    break
    elif sheet in ("dloads", "dloads (2)"):
        match = re.fullmatch(r'=IF\(main!\$D\$(23|24)="auto","([^"]*)",""\)', record["formula"])
        assert match, record
        mode = evidence["constants"]["main"]["D" + match[1]]
        actual = distributed_load_guidance(WaterLoadInput(mode))
        expected = match[2] if mode is not None and mode.lower() == "auto" else ""
    else:
        pytest.fail(f"Unmapped formula: {sheet}!{address}")
    assert_result(actual, expected, record)
    if record["cached_value"] is not None:
        assert_result(actual, record["cached_value"], record)


def assert_result(actual, expected, record):
    if isinstance(expected, (int, float)):
        assert actual == pytest.approx(expected, rel=1e-9, abs=1e-12), record
    else:
        assert actual == expected, record


@pytest.mark.parametrize("fixture,sheet,address", formula_cases())
def test_formulas_from_fixture(fixture, sheet, address):
    compare_formula(json.loads(fixture.read_text(encoding="utf-8")), sheet, address)


@pytest.mark.parametrize("fixture,sheet,address", formula_cases())
def test_formulas_from_workbook(fixture, sheet, address):
    committed = json.loads(fixture.read_text(encoding="utf-8"))
    live = read_live_evidence(committed["source"])
    assert live == committed
    compare_formula(live, sheet, address)


@pytest.mark.parametrize("fixture,total,cached", zip(FIXTURES, (72, 72), (2, 4)))
def test_formula_inventory(fixture, total, cached):
    evidence = json.loads(fixture.read_text(encoding="utf-8"))
    records = evidence["formulas"]
    assert len(records) == total
    assert len({(r["sheet"], r["cell"]) for r in records}) == total
    assert sum(r["cached_value"] is not None for r in records) == cached
    assert Counter(r["sheet"] for r in records) == {
        sheet["sheet"]: sheet["formula_count"]
        for sheet in evidence["sheets"] if sheet["formula_count"]
    }


@pytest.mark.parametrize("fixture", FIXTURES)
@pytest.mark.parametrize("selected", (
    "Geosynthetic", "Nail", "Tieback", "Anchor", "nAiL", "unknown", "", None,
))
def test_reinforcement_populated_and_failed_lookup(fixture, selected):
    """Exercise real lookup rows hidden behind blank selectors in the sources."""
    evidence = json.loads(fixture.read_text(encoding="utf-8"))
    evidence["constants"]["reinforce"]["G3"] = selected
    for address in ("H3", "I3"):
        compare_formula(evidence, "reinforce", address)


@pytest.mark.parametrize("fixture", FIXTURES)
def test_material_lookup_missing_id_blank_name_and_first_match(fixture):
    evidence = json.loads(fixture.read_text(encoding="utf-8"))
    rows = material_rows(evidence)
    assert material_name(MaterialLabelInput(1), rows) == "soil"
    assert material_name(MaterialLabelInput(2), rows) == ""
    assert material_name(MaterialLabelInput(999), rows) == ""
    duplicate = (MaterialLabel(1, "first"),) + rows
    assert material_name(MaterialLabelInput(1), duplicate) == "first"
    for region_type in (None, "", "MATERIAL"):
        assert polygon_material_name(PolygonLabelInput(1, region_type), rows) == "soil"
    assert polygon_material_name(PolygonLabelInput(1, "refinement"), rows) == ""


@pytest.mark.parametrize("mode,expected", ((None, ""), ("", ""), ("manual", "")))
def test_water_guidance_other_modes(mode, expected):
    assert distributed_load_guidance(WaterLoadInput(mode)) == expected


def test_water_guidance_is_case_insensitive():
    evidence = json.loads(FIXTURES[0].read_text(encoding="utf-8"))
    evidence["constants"]["main"]["D23"] = "AUTO"
    compare_formula(evidence, "dloads", "B2")


def test_reinforcement_first_match_and_blank_return_cells():
    rules = (
        ReinforcementRule("Nail", None, None),
        ReinforcementRule("Nail", "Axial", "Passive"),
    )
    assert reinforcement_properties(ReinforcementInput("Nail"), rules) == (0, 0)


@pytest.mark.parametrize("relative_output", [False, True])
def test_extractor_output_is_reproducible(tmp_path, monkeypatch, relative_output):
    """Regenerate real workbook evidence under Windows newline defaults."""
    spec = importlib.util.spec_from_file_location(
        "formula_evidence_output", HERE / "formula_evidence.py",
    )
    extractor = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(extractor)
    for source in extractor.SOURCES:
        destination = tmp_path / source
        destination.parent.mkdir(parents=True, exist_ok=True)
        shutil.copyfile(REPOSITORY / source, destination)
    extract = extractor.extract_workbook
    monkeypatch.setattr(extractor, "REPOSITORY", tmp_path)
    monkeypatch.setattr(
        extractor, "extract_workbook", lambda path: extract(path, repository=tmp_path),
    )
    write_text = Path.write_text

    def windows_write_text(path, data, encoding=None, errors=None, newline=None):
        return write_text(
            path, data, encoding=encoding, errors=errors,
            newline="\r\n" if newline is None else newline,
        )

    monkeypatch.setattr(Path, "write_text", windows_write_text)
    monkeypatch.chdir(tmp_path)
    output_dir = Path("generated") if relative_output else tmp_path / "generated"
    monkeypatch.setattr(sys, "argv", ["formula_evidence.py", "--output-dir", str(output_dir)])
    extractor.main()
    fixtures = (*FIXTURES, REPOSITORY / (
        "tests/specialized/rigging/fractional_inch/fixtures/anchor_shackle_sizes.json"
    ))
    for fixture in fixtures:
        assert (output_dir / fixture.name).read_bytes() == fixture.read_bytes()
