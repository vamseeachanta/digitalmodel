"""Independent per-cell evidence for fractional-inch shackle sizes.

The four fractional-inch results are binary-exact and compare exactly with
independent rational arithmetic and stored workbook values.
"""

import importlib.util
import json
import math
from collections import Counter
from fractions import Fraction
from functools import lru_cache
from pathlib import Path
import re

import pytest

from digitalmodel.asset_integrity.common.data import FromString


HERE = Path(__file__).parent
REPOSITORY = HERE.parents[3]
FIXTURES = (HERE / "fixtures/anchor_shackle_sizes.json",)


@lru_cache(maxsize=None)
def read_live_evidence(source):
    """Use the committed extractor, preserving direct workbook comparison."""
    path = REPOSITORY / source
    if not path.is_file():
        pytest.skip("Source workbook is absent; committed JSON remains testable")
    spec = importlib.util.spec_from_file_location(
        "formula_evidence_extractor",
        REPOSITORY / "tests/geotechnical/slope_input_labels/formula_evidence.py",
    )
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module.extract_workbook(path)


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
    assert sheet == "Sheet1", record
    match = re.fullmatch(r'=(\d+)/(\d+)', record["formula"])
    assert match, record
    actual = FromString().convert_fraction_to_float(record["formula"][1:])
    # Independent exact rational arithmetic, then conversion to float.
    expected = float(Fraction(record["formula"][1:]))
    assert_result(actual, expected, record)
    if record["cached_value"] is not None:
        assert_result(actual, record["cached_value"], record)


def assert_result(actual, expected, record):
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


@pytest.mark.parametrize("fixture,total,cached", zip(FIXTURES, (4,), (4,)))
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


@pytest.mark.parametrize("perturb", ("conversion", "cache"))
def test_binary_exact_fraction_rejects_adjacent_float(monkeypatch, perturb):
    evidence = json.loads(FIXTURES[0].read_text(encoding="utf-8"))
    adjacent = math.nextafter(0.1875, 1.0)
    if perturb == "conversion":
        monkeypatch.setattr(FromString, "convert_fraction_to_float", lambda *_: adjacent)
    else:
        evidence["formulas"][0]["cached_value"] = adjacent
    with pytest.raises(AssertionError):
        compare_formula(evidence, "Sheet1", "A2")
