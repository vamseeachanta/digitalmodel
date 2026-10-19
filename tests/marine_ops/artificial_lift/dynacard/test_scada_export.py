# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Synthetic format tests; no restricted export rows are redistributed.

Format basis: issue #1873 correction (2026-07-28), inspected exports (2020).
All numeric fixtures below are invented inputs for independent hand arithmetic,
not field measurements, calibration constants, or engineering acceptance limits.
"""

import csv
import io
import json
import runpy
from pathlib import Path

import pytest

from digitalmodel.marine_ops.artificial_lift.dynacard.scada_export import (
    load_scada_export,
    main,
    read_scada_export,
    summarize_cards,
)


def export_text(**changes):
    """Build an original synthetic row with the observed column names."""
    row = {
        "Well Name": "SYNTHETIC",
        "Card Time": "2020-01-01T00:00:00.0000000Z",
        "Card Type": "Current",
        "Stroke Period": "6",
        "SPM": "10",
        "Stroke Length": "4",
        "Surface Positions": "0,2,4,2,0",
        "Surface Loads": "10,30,30,10,10",
        "Downhole Positions": "",
        "Downhole Loads": "",
    }
    row.update(changes)
    stream = io.StringIO(newline="")
    writer = csv.DictWriter(stream, fieldnames=list(row), delimiter="|")
    writer.writeheader()
    writer.writerow(row)
    return stream.getvalue()


def read_one(**changes):
    return read_scada_export(io.StringIO(export_text(**changes)))[0]


def test_surface_order_metadata_and_hand_calculated_ranges():
    """Hand: span = 4-0 = 4; range = 30-10 = 20; span/header = 4/4 = 1."""
    card = read_one()
    assert card.position == (0, 2, 4, 2, 0)
    assert card.load == (10, 30, 30, 10, 10)
    assert card.timestamp == "2020-01-01T00:00:00.0000000Z"
    assert card.card_type == "Current"
    assert (card.spm, card.stroke_period, card.stroke_length) == (10, 6, 4)
    assert card.surface_stroke == 4
    assert card.load_range == 20
    assert card.stroke_ratio == 1
    assert card.downhole_status == "MISSING"


@pytest.mark.parametrize("kind", ["Current", "Full", "Other"])
def test_acquisition_type_is_preserved_without_diagnosis(kind):
    card = read_one(**{"Card Type": kind})
    assert card.card_type == kind
    assert "diagnosis" not in summarize_cards([card])


@pytest.mark.parametrize(
    "position,load,status",
    [
        ("", "", "MISSING"),
        ("0,1,0", "0,1,0", "COPIED"),
        ("0,1,0", "0.0,1.0,0.0", "COPIED"),
        ("0,1,0", "", "INVALID"),
        ("", "1,2,1", "INVALID"),
        ("0,1,0", "1,2", "INVALID"),
        ("0,1,0", "1,nan,1", "INVALID"),
        ("bad", "bad", "INVALID"),
        ("0,1,0", "10,20,10", "UNVERIFIED"),
    ],
)
def test_downhole_headers_never_establish_validation(position, load, status):
    card = read_one(**{"Downhole Positions": position, "Downhole Loads": load})
    assert card.downhole_status == status
    report = summarize_cards([card])
    assert report["solver_validation"] == "NOT_EVALUATED"
    assert report["downhole_status_counts"] == {status: 1}


@pytest.mark.parametrize(
    "changes",
    [
        {"Surface Positions": "0,1"},
        {"Surface Positions": "0,,4,2,0"},
        {"Surface Loads": "1,2,3,4,"},
        {"Surface Loads": "1,2,inf,4,5"},
        {"Surface Positions": "0,nan,4,2,0"},
        {"Surface Loads": ""},
        {"Surface Positions": "1", "Surface Loads": "2"},
        {"Card Time": "yesterday"},
        {"Card Time": "2020-01-01T00:00:00"},
        {"Card Time": ""},
        {"Card Type": ""},
        {"SPM": "-1"},
        {"Stroke Period": "0"},
        {"Stroke Length": "nan"},
        {"Surface Positions": "-1e308,0,1e308,0,0"},
        {"Stroke Length": "1e-320"},
    ],
)
def test_invalid_surface_or_metadata_fails_with_row_context(changes):
    with pytest.raises(ValueError, match="row 2"):
        read_one(**changes)


def test_no_header_or_missing_or_duplicate_columns():
    for text in ["", "Surface Positions|Surface Loads\n0,1|1,2\n",
                 export_text().replace("SPM|", "Card Time|")]:
        with pytest.raises(ValueError, match="header"):
            read_scada_export(io.StringIO(text))


def test_ragged_rows_and_invalid_quoting_are_rejected():
    for text in [export_text().rstrip() + "|extra\n",
                 export_text().rsplit("|", 1)[0] + "\n",
                 export_text().splitlines()[0] + '\n"unfinished']:
        with pytest.raises(ValueError, match="row 2"):
            read_scada_export(io.StringIO(text))


def test_optional_numbers_and_downhole_columns_can_be_absent():
    row = {
        "Card Time": "2020-01-01T00:00:00+00:00", "Card Type": "Full",
        "Surface Positions": "0,1,0", "Surface Loads": "3,4,3",
    }
    text = "|".join(row) + "\n" + "|".join(row.values()) + "\n"
    card = read_scada_export(io.StringIO(text))[0]
    assert card.spm is None
    assert card.stroke_ratio is None
    assert card.downhole_status == "MISSING"


def test_declared_units_required_before_carddata_adapter():
    card = read_one()
    with pytest.raises(TypeError):
        card.to_surface_card()
    with pytest.raises(ValueError, match="units"):
        card.to_surface_card(position_unit="m", load_unit="kN")
    surface = card.to_surface_card(position_unit="in", load_unit="lbf")
    assert surface.position == list(card.position)
    assert surface.load == list(card.load)
    assert surface.timestamp == card.timestamp


def test_batch_preserves_distribution_and_per_card_values():
    """Hand: second span 8-0=8, load range 9-1=8, ratio 8/4=2.

    Two cards therefore have ratios [1,2], loads [20,8], counts [5,3].
    No mean can conceal the second card's larger stroke ratio.
    """
    first = read_one()
    second = read_one(**{"Surface Positions": "0,8,0", "Surface Loads": "1,9,1"})
    report = summarize_cards([first, second])
    assert report["card_count"] == 2
    assert report["point_count_counts"] == {5: 1, 3: 1}
    assert report["distributions"]["stroke_ratio"] == {
        "values": [1, 2], "min": 1, "max": 2,
    }
    assert report["cards"][1]["surface_load_range"] == 8
    assert report["units"] == "UNDECLARED"
    assert summarize_cards([])["distributions"]["stroke_ratio"] == {
        "values": [], "min": None, "max": None,
    }


def test_load_utf8_bom_and_cli(tmp_path, capsys):
    path = tmp_path / "synthetic.csv"
    path.write_text(export_text(), encoding="utf-8-sig")
    assert len(load_scada_export(path)) == 1
    assert main([str(path)]) == 0
    report = json.loads(capsys.readouterr().out)
    assert report["card_count"] == 1
    assert report["solver_validation"] == "NOT_EVALUATED"
    assert main([str(path), str(path)]) == 0
    assert json.loads(capsys.readouterr().out)["card_count"] == 2


def test_cli_failure_does_not_print_input_values(tmp_path, capsys):
    path = tmp_path / "bad.csv"
    path.write_text(export_text(**{"Surface Loads": "invalid"}))
    assert main([str(path)]) == 2
    captured = capsys.readouterr()
    assert not captured.out
    assert "row 2" in captured.err
    assert "invalid" not in captured.err


def test_cli_encoding_failure_does_not_echo_bytes(tmp_path, capsys):
    path = tmp_path / "encoding.csv"
    path.write_bytes(b"\xff")
    assert main([str(path)]) == 2
    captured = capsys.readouterr()
    assert not captured.out
    assert captured.err == "Cannot read export: UnicodeDecodeError\n"


def test_documented_example_matches_independent_hand_calculation():
    """Hand: 4-0=4, 30-10=20, 4/4=1; equal downhole arrays are copied."""
    root = Path(__file__).resolve().parents[4]
    example = runpy.run_path(
        str(root / "examples/domains/artificial_lift/scada_surface_export.py")
    )
    report = example["worked_example"]()
    assert report["cards"] == [{
        "point_count": 5, "surface_stroke": 4, "surface_load_range": 20,
        "surface_to_declared_stroke_ratio": 1, "downhole_status": "COPIED",
    }]
    assert report["solver_validation"] == "NOT_EVALUATED"
