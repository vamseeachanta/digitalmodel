# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Offline plate comparison; expected inventories come from the retained source."""

import json
from pathlib import Path

import pytest

from digitalmodel.marine_ops.artificial_lift.dynacard import plate_comparison
from digitalmodel.marine_ops.artificial_lift.dynacard.diagnostic_plate import (
    load_diagnostic_plate,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.models import CardData
from digitalmodel.marine_ops.artificial_lift.dynacard.plate_comparison import (
    compare_diagnostic_plate,
)

PLATE = Path(__file__).parent / "testdata" / "mendeley_plate"
MANIFEST = json.loads((PLATE / "manifest.json").read_text(encoding="utf-8"))
WORKBOOK = PLATE / MANIFEST["file"]["name"]


def test_inventory_retains_every_load_and_power_curve():
    """Expected labels, quantities and counts are independently inventoried cells."""
    report = compare_diagnostic_plate(WORKBOOK, {})
    assert [(row["label"], row["block_index"], row["ordinate"])
            for row in report["curves"]] == [
        (row["label"], row["block_index"], row["ordinate"])
        for row in MANIFEST["curves"]
    ]
    assert report["source_sha256"] == MANIFEST["file"]["sha256"]
    assert report["unscored_supplied_counts"] == {}
    assert all(row["status"] == "NOT_EVALUATED" for row in report["curves"])
    assert report["acceptance"]["status"] == "NOT_EVALUATED"
    assert report["field_accuracy"]["status"] == "NOT_EVALUATED"
    assert json.loads(json.dumps(report, allow_nan=False)) == report


@pytest.mark.parametrize("entry", [row for row in MANIFEST["curves"]
                                  if row["fault_mode"]])
def test_self_comparison_for_each_literal_mapping(entry):
    """Hand calculation: each coincident point has sqrt(0**2 + 0**2) = 0.

    Thus its mean distance, minimum over alignments and median over copies
    are all zero. This identity holds for every workbook load-curve mapping.
    The input here is a geometric oracle, not claimed synthetic or field data.
    """
    curve = next(c for c in load_diagnostic_plate(WORKBOOK)
                 if c.label == entry["label"] and c.ordinate == "load")
    card = CardData(position=list(curve.position), load=list(curve.values))
    report = compare_diagnostic_plate(WORKBOOK, {curve.fault_mode: [card, card]})
    row = next(r for r in report["curves"] if r["label"] == curve.label
               and r["ordinate"] == "load")
    assert row["status"] == "MEASURED"
    assert report["unscored_supplied_counts"] == {}
    assert row["count"] == 2  # Two identical copies supplied above.
    assert row["distances"] == pytest.approx([0, 0])
    assert row["median_distance"] == pytest.approx(0)
    assert row["nearest_load_label"] == curve.label
    assert row["median_distances_by_label"][curve.label] == pytest.approx(0)
    assert set(row["median_distances_by_label"]) == {
        c["label"] for c in MANIFEST["curves"] if c["ordinate"] == "load"
    }


def test_power_and_ambiguous_labels_are_never_scored():
    """The manifest deliberately gives no mode to power, supply or sand curves."""
    report = compare_diagnostic_plate(WORKBOOK, {})
    for row in report["curves"]:
        if row["ordinate"] == "power" or row["fault_mode"] is None:
            assert row["status"] == "NOT_EVALUATED"
            assert row["reason"]
            assert row["distances"] == []
            assert row["median_distance"] is None
            assert row["nearest_load_label"] is None


@pytest.mark.parametrize("count", [0, 1, 2])
def test_no_exemplar_modes_are_named_without_aliasing(count):
    """Stuck-pump has no literal load exemplar in the source manifest."""
    card = CardData(position=[0, 1, 1, 0], load=[0, 0, 1, 1])
    report = compare_diagnostic_plate(WORKBOOK, {"STUCK_PUMP": [card] * count})
    assert report["unscored_supplied_counts"] == {"STUCK_PUMP": count}
    assert "STUCK_PUMP" in report["modes_without_literal_exemplar"]
    assert all(row["status"] == "NOT_EVALUATED" for row in report["curves"])


def test_unscored_counts_exclude_modes_with_literal_exemplars():
    """Only supplied modes without a load exemplar need separate counts."""
    card = CardData(position=[0, 1, 1, 0], load=[0, 0, 1, 1])
    report = compare_diagnostic_plate(WORKBOOK, {
        "NORMAL": [card], "STUCK_PUMP": [card, card], "GAS_LOCK": [],
    })
    assert report["unscored_supplied_counts"] == {"GAS_LOCK": 0, "STUCK_PUMP": 2}
    normal = next(row for row in report["curves"] if row["fault_mode"] == "NORMAL")
    assert normal["status"] == "MEASURED"
    assert normal["count"] == 1


def test_unknown_input_mode_is_rejected():
    with pytest.raises(ValueError, match="Unknown generator mode"):
        compare_diagnostic_plate(WORKBOOK, {"misspelled": []})


def test_empty_sample_list_is_not_a_measurement():
    report = compare_diagnostic_plate(WORKBOOK, {"NORMAL": []})
    row = next(r for r in report["curves"] if r["fault_mode"] == "NORMAL")
    assert row["status"] == "NOT_EVALUATED"
    assert row["count"] == 0
    assert row["reason"] == "No synthetic samples supplied"


def test_bad_card_is_rejected_even_without_an_exemplar():
    """No-exemplar status must not conceal corrupt supplied card arrays."""
    card = CardData(position=[0, 1, 0], load=[0, float("nan"), 1])
    with pytest.raises(ValueError, match="finite"):
        compare_diagnostic_plate(WORKBOOK, {"STUCK_PUMP": [card]})


def test_changed_workbook_cannot_be_used(tmp_path):
    changed = tmp_path / "changed.xls"
    changed.write_bytes(b"not the retained workbook")
    with pytest.raises(ValueError, match="digest"):
        compare_diagnostic_plate(changed, {})


def test_source_digest_describes_loaded_curves_after_workbook_changes(
    tmp_path, monkeypatch,
):
    """A later replacement must not change the evidence for already loaded data."""
    workbook = tmp_path / "plate.xls"
    workbook.write_bytes(WORKBOOK.read_bytes())

    def load_then_replace(path):
        curves = load_diagnostic_plate(path)
        workbook.write_bytes(b"replacement after verified load")
        return curves

    monkeypatch.setattr(plate_comparison, "load_diagnostic_plate", load_then_replace)
    report = compare_diagnostic_plate(workbook, {})
    assert report["source_sha256"] == MANIFEST["file"]["sha256"]
    assert [(row["label"], row["ordinate"]) for row in report["curves"]] == [
        (row["label"], row["ordinate"]) for row in MANIFEST["curves"]
    ]


def test_affine_scale_and_sample_origin_do_not_define_a_fault():
    """Hand calculation on a unit square: normalisation removes x'=2x+3
    and y'=4y+5. Reversed, shifted vertices still trace the same four edges;
    the uniform perimeter grid aligns exactly, giving zero point distances.
    These constructed coordinates are algebra probes, not engineering values.
    """
    from digitalmodel.marine_ops.artificial_lift.dynacard.benchmark.measurements import (
        loop_distance,
    )

    assert loop_distance([0, 1, 1, 0], [0, 0, 1, 1],
                         [5, 5, 3, 3], [9, 5, 5, 9]) == pytest.approx(0)


def _example():
    import runpy

    root = Path(__file__).resolve().parents[4]
    return runpy.run_path(str(root / "examples" / "domains" / "artificial_lift"
                             / "diagnostic_plate_comparison.py"))


def test_worked_example_requires_explicit_sample_seeds(capsys):
    with pytest.raises(SystemExit) as error:
        _example()["main"](["--workbook", str(WORKBOOK)])
    assert error.value.code != 0
    assert "--seeds" in capsys.readouterr().err


def test_worked_example_uses_original_workbook(capsys):
    """One sample for each mapped class; coverage derives from the manifest."""
    report = _example()["main"](["--workbook", str(WORKBOOK), "--seeds", "0"])
    expected_modes = {row["fault_mode"] for row in MANIFEST["curves"]
                      if row["fault_mode"]}
    measured = [row for row in report["curves"] if row["status"] == "MEASURED"]
    assert {row["fault_mode"] for row in measured} == expected_modes
    assert all(row["count"] == 1 for row in measured)
    assert report["seeds"] == [0]  # Explicit caller-selected seed above.
    output = capsys.readouterr().out
    assert "NOT_EVALUATED" in output
    assert "GAS_INTERFERENCE" in output
    assert "distance x 100" in output
    assert all(row["label"] in output for row in report["curves"])


def test_duplicate_seeds_cannot_inflate_sample_count():
    with pytest.raises(ValueError, match="unique"):
        _example()["main"](["--workbook", str(WORKBOOK), "--seeds", "0", "0"])
