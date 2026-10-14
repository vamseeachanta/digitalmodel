"""Offline ingestion checks against the retained CC0 workbook and manifest."""

import hashlib
import json
from dataclasses import asdict
from decimal import Decimal
from pathlib import Path
from types import SimpleNamespace

import pytest
import xlrd

from digitalmodel.marine_ops.artificial_lift.dynacard.diagnostic_plate import (
    DiagnosticPlateCurve,
    _read_pair,
    load_diagnostic_plate,
)

FIXTURES = Path(__file__).parent / "testdata"
PLATE = FIXTURES / "mendeley_plate"
MANIFEST = json.loads((PLATE / "manifest.json").read_text(encoding="utf-8"))
WORKBOOK = PLATE / MANIFEST["file"]["name"]


def test_workbook_integrity():
    raw = WORKBOOK.read_bytes()
    assert len(raw) == MANIFEST["file"]["size_bytes"]
    assert hashlib.sha256(raw).hexdigest() == MANIFEST["file"]["sha256"]


def test_classes_counts_and_axis_ranges():
    curves = load_diagnostic_plate(WORKBOOK)
    expected = MANIFEST["curves"]
    assert [(c.label, c.block_index) for c in curves] == [
        (e["label"], e["block_index"]) for e in expected
    ]
    for curve, entry in zip(curves, expected, strict=True):
        assert len(curve.position) == len(curve.values) == entry["point_count"]
        assert [min(curve.position), max(curve.position)] == entry["position_range"]
        assert [min(curve.values), max(curve.values)] == entry["ordinate_range"]
        assert curve.ordinate == entry["ordinate"]
        assert curve.fault_mode == entry["fault_mode"]
        assert curve.columns == tuple(entry["columns_zero_based"])
    # Domain is the source chart's normalised axes, not invented physical units.
    assert all(0 <= x <= 1 for c in curves for x in (*c.position, *c.values))


def test_load_blocks_match_existing_generator_reference_precision():
    references = json.loads(
        (FIXTURES / "plate_reference_cards.json").read_text(encoding="utf-8")
    )["cards"]
    cards = {c.label: c for c in load_diagnostic_plate(WORKBOOK)
             if c.ordinate == "load"}
    assert set(cards) == {e["sheet"] for e in references.values()}
    for reference in references.values():
        card = cards[reference["sheet"]]
        # Precision is derived from the existing fixture, not a new tolerance.
        precision = max(-Decimal(str(v)).as_tuple().exponent
                        for key in ("position", "load") for v in reference[key])
        assert [round(v, precision) for v in card.position] == reference["position"]
        assert [round(v, precision) for v in card.values] == reference["load"]


def test_labels_round_trip_without_translation_or_loss():
    curves = load_diagnostic_plate(WORKBOOK)
    payload = json.loads(json.dumps([asdict(c) for c in curves], ensure_ascii=False))
    restored = [DiagnosticPlateCurve(**{
        **e, "columns": tuple(e["columns"]),
        "position": tuple(e["position"]), "values": tuple(e["values"]),
    }) for e in payload]
    assert restored == curves


def test_power_and_ambiguous_classes_remain_unmapped():
    curves = load_diagnostic_plate(WORKBOOK)
    assert all(c.fault_mode is None for c in curves if c.ordinate == "power")
    ambiguous = {"供液不足", "出砂", "过平衡", "欠平衡", "减速箱故障"}
    assert {c.label for c in curves if c.fault_mode is None} == {
        c.label for c in curves if c.ordinate == "power"
    } | ambiguous
    assert all(c.fault_mode is None for c in curves if c.label in ambiguous)
    from digitalmodel.marine_ops.artificial_lift.dynacard.card_generators import (
        ALL_GENERATORS,
    )
    assert {c.fault_mode for c in curves if c.fault_mode} <= set(ALL_GENERATORS)


def _sheet_with_rows(rows):
    def cell(row, col):
        value = rows[row][col]
        if value is None:
            return SimpleNamespace(ctype=xlrd.XL_CELL_EMPTY, value="")
        kind = xlrd.XL_CELL_TEXT if isinstance(value, str) else xlrd.XL_CELL_NUMBER
        return SimpleNamespace(ctype=kind, value=value)
    return SimpleNamespace(name="工况正常", nrows=len(rows), cell=cell)


def test_pair_preserves_blanks_and_order():
    # Values are taken directly from the first curve; blanks represent padding.
    curve = MANIFEST["curves"][0]
    rows = [tuple(curve["position_range"]), (None, None),
            tuple(curve["ordinate_range"])]
    assert _read_pair(_sheet_with_rows(rows), 0) == (
        tuple(row[0] for row in rows if row[0] is not None),
        tuple(row[1] for row in rows if row[1] is not None),
    )


@pytest.mark.parametrize("bad", [None, "unexpected", float("nan"),
                                  float("inf"), -float("inf")])
def test_invalid_pairs_are_rejected(bad):
    # Negative probes are corruptions of sourced values, not engineering data.
    point = MANIFEST["curves"][0]["position_range"][0]
    with pytest.raises(ValueError):
        _read_pair(_sheet_with_rows([(point, bad)]), 0)


def test_unknown_classes_are_rejected(monkeypatch):
    book = SimpleNamespace(sheets=lambda: [SimpleNamespace(name="unknown")])
    monkeypatch.setattr(xlrd, "open_workbook", lambda **kwargs: book)
    with pytest.raises(ValueError, match="Unknown plate class"):
        load_diagnostic_plate(WORKBOOK)


def test_changed_workbook_is_rejected(tmp_path):
    raw = bytearray(WORKBOOK.read_bytes())
    raw[0] ^= 1  # Deliberate single-bit corruption of the retained source.
    changed = tmp_path / WORKBOOK.name
    changed.write_bytes(raw)
    with pytest.raises(ValueError, match="digest"):
        load_diagnostic_plate(changed)


def test_outside_source_domain_is_rejected():
    point = MANIFEST["curves"][0]["position_range"][0]
    with pytest.raises(ValueError, match="normalised"):
        _read_pair(_sheet_with_rows([(point, -point)]), 0)


def test_empty_pair_is_rejected():
    with pytest.raises(ValueError, match="Empty"):
        _read_pair(_sheet_with_rows([(None, None)]), 0)


def test_ordinate_schema_matches_embedded_chart_titles():
    # Titles live in BIFF chart records, not text cells. This checks the source
    # interpretation independently of both the loader and its numeric parsing.
    # Private xlrd attributes are used to count embedded chart titles. A library
    # change may break this test without a data change; re-derive the counts
    # another way if that happens, keeping the data and digest unchanged.
    from xlrd.compdoc import CompDoc
    raw = WORKBOOK.read_bytes()
    stream = CompDoc(raw).get_named_stream("Workbook")
    book = xlrd.open_workbook(file_contents=raw, on_demand=True)
    for index, sheet in enumerate(book.sheets()):
        start = book._sh_abs_posn[index]
        end = (book._sh_abs_posn[index + 1]
               if index + 1 < book.nsheets else len(stream))
        chunk = stream[start:end]
        entries = [e for e in MANIFEST["curves"] if e["label"] == sheet.name]
        for title, key in [("Normalized displacement", "position"),
                           ("Normalized load", "load"),
                           ("Normalized power", "power")]:
            expected = len(entries) if key == "position" else sum(
                e["ordinate"] == key for e in entries
            )
            assert chunk.count(title.encode("utf-16le")) == expected

        if any(e["ordinate"] == "load" for e in entries):
            assert chunk.find("Normalized load".encode("utf-16le")) < chunk.find(
                "Normalized power".encode("utf-16le")
            )
