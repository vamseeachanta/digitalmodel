"""MAH boundary tests; all example quantities are synthetic input assumptions.

Hand calculation: 0.02 line-break events/year times 0.5 fatalities/event
equals 0.01 expected fatalities/year. No acceptance criterion is assumed.
"""

import csv
import io
import json

import pytest
from pydantic import ValidationError

from digitalmodel.asset_integrity.safety_case import (
    EvaluationStatus,
    MAHRegister,
    MAHValidationError,
    MajorAccidentHazard,
)


def hazard_data(**changes):
    data = dict(
        hazard_id="line-break",
        description="Line break with loss of station",
        frequency_per_year=0.02,
        fatalities_per_event=0.5,
        frequency_basis="Synthetic arithmetic example, not field data",
        consequence_basis="Synthetic conditional mean, not an individual probability",
        barriers=("Inspection, including terminations", "Exclusion zone"),
        responsible_roles=("Integrity engineer",),
    )
    return data | changes


def csv_text(**changes):
    data = hazard_data(**changes)
    data["barriers"] = json.dumps(data["barriers"])
    data["responsible_roles"] = json.dumps(data["responsible_roles"])
    stream = io.StringIO()
    writer = csv.DictWriter(stream, fieldnames=list(data))
    writer.writeheader()
    writer.writerow(data)
    return stream.getvalue()


def test_csv_ingest_and_loss():
    register = MAHRegister.from_csv(io.StringIO(csv_text()))
    hazard = register.hazards[0]
    assert hazard.hazard_id == "line-break"
    assert hazard.expected_fatalities_per_year == pytest.approx(0.01)
    assert hazard.status is EvaluationStatus.EVALUATED
    assert hazard.barriers[0] == "Inspection, including terminations"


@pytest.mark.parametrize("as_string", [False, True])
@pytest.mark.parametrize("encoding", ["utf-8", "utf-8-sig"])
def test_csv_file_round_trip(tmp_path, as_string, encoding):
    original = MAHRegister(hazards=(MajorAccidentHazard(**hazard_data()),))
    path = tmp_path / "register.csv"
    path.write_text(original.to_csv(), encoding=encoding)
    assert MAHRegister.from_csv(str(path) if as_string else path) == original


@pytest.mark.parametrize("with_hazard", [False, True])
def test_bom_prefixed_text_stream(with_hazard):
    hazards = (MajorAccidentHazard(**hazard_data()),) if with_hazard else ()
    original = MAHRegister(hazards=hazards)
    stream = io.StringIO("\ufeff" + original.to_csv())
    assert MAHRegister.from_csv(stream) == original
    assert not stream.closed


def test_bom_before_quoted_header_in_text_stream():
    text = csv_text()
    quoted_header = text.replace("hazard_id,", '"hazard_id",', 1)
    expected = MAHRegister.from_csv(io.StringIO(text))
    assert MAHRegister.from_csv(io.StringIO("\ufeff" + quoted_header)) == expected


@pytest.mark.parametrize("field", ["frequency_per_year", "fatalities_per_event"])
def test_underscore_numeric_cell_rejected(field):
    with pytest.raises(MAHValidationError) as exc:
        MAHRegister.from_csv(io.StringIO(csv_text(**{field: "1_0"})))
    assert exc.value.row == 2
    assert exc.value.field == field


@pytest.mark.parametrize(
    "preceding,malformed,expected_row",
    [
        (None, 'bad,"d"x,unused\n', 2),
        ("Prior scenario", 'bad,"d"x,unused\n', 3),
        ("Prior\nscenario", 'bad,"d"x,unused\n', 4),
        (None, 'bad,"d\ncontinued"x,unused\n', 3),
        (None, 'bad,"d\ncontinued', 3),
        (None, '\nbad,"d"x,unused\n', 3),
    ],
)
def test_strict_quoting_error_reports_physical_line(preceding, malformed, expected_row):
    text = csv_text(description=preceding) if preceding is not None else (
        MAHRegister(hazards=()).to_csv()
    )
    with pytest.raises(MAHValidationError) as exc:
        MAHRegister.from_csv(io.StringIO(text + malformed))
    assert exc.value.row == expected_row
    assert exc.value.field == "row"
    assert "malformed CSV" in str(exc.value)


def test_malformed_header_reports_physical_line():
    with pytest.raises(MAHValidationError) as exc:
        MAHRegister.from_csv(io.StringIO('"hazard_id"x,description\n'))
    assert exc.value.row == 1
    assert exc.value.field == "row"
    assert "malformed CSV" in str(exc.value)


@pytest.mark.parametrize(
    "field,value",
    [
        ("frequency_per_year", "bad"),
        ("frequency_per_year", "NaN"),
        ("frequency_per_year", "inf"),
        ("frequency_per_year", -0.1),
        ("fatalities_per_event", -1),
        ("description", " "),
        ("frequency_basis", ""),
        ("barriers", [" "]),
        ("responsible_roles", [" "]),
    ],
)
def test_bad_row_reports_location(field, value):
    with pytest.raises(MAHValidationError) as exc:
        MAHRegister.from_csv(io.StringIO(csv_text(**{field: value})))
    assert exc.value.row == 2
    assert field in exc.value.field


@pytest.mark.parametrize(
    "text",
    [
        "",
        "hazard_id,description\na,b\n",
        csv_text().replace("hazard_id,", "description,", 1),
        csv_text().replace("hazard_id,", "unexpected,", 1),
        csv_text() + "extra,short\n",
        csv_text().rstrip() + ",extra\n",
        csv_text().replace('[""Integrity engineer""]', "not-json"),
        csv_text().replace('[""Integrity engineer""]', '"""a string"""'),
    ],
)
def test_malformed_csv_rejected(text):
    with pytest.raises(MAHValidationError):
        MAHRegister.from_csv(io.StringIO(text))


def test_duplicate_ids_rejected_with_csv_location():
    text = csv_text()
    with pytest.raises(MAHValidationError) as exc:
        MAHRegister.from_csv(io.StringIO(text + text.splitlines()[1] + "\n"))
    assert exc.value.row == 3
    assert exc.value.field == "hazard_id"


def test_direct_schema_validation_and_immutability():
    for changes in (
        {"unexpected": "x"},
        {"frequency_per_year": True},
        {"frequency_per_year": "0.02"},
    ):
        with pytest.raises(ValidationError):
            MajorAccidentHazard(**hazard_data(**changes))
    hazard = MajorAccidentHazard(**hazard_data())
    with pytest.raises(ValidationError):
        hazard.frequency_per_year = -1
    with pytest.raises(ValidationError):
        MAHRegister(hazards=(hazard, hazard))


def test_missing_inputs_are_not_zero_risk():
    register = MAHRegister.from_csv(io.StringIO(csv_text(fatalities_per_event="")))
    hazard = register.hazards[0]
    assert hazard.status is EvaluationStatus.NOT_EVALUATED
    assert hazard.expected_fatalities_per_year is None
    assert MAHRegister.from_csv(io.StringIO(register.to_csv())) == register


def test_true_zero_and_mean_greater_than_one_are_valid():
    """0 events/year times 2 fatalities/event equals 0 fatalities/year."""
    hazard = MajorAccidentHazard(
        **hazard_data(
            frequency_per_year=0,
            fatalities_per_event=2,
        )
    )
    assert hazard.status is EvaluationStatus.EVALUATED
    assert hazard.expected_fatalities_per_year == 0


def test_unmitigated_hazard_can_be_recorded():
    """An empty control inventory must not prevent recording a known hazard."""
    register = MAHRegister.from_csv(io.StringIO(csv_text(barriers=[])))
    assert register.hazards[0].barriers == ()
    assert register.hazards[0].expected_fatalities_per_year == pytest.approx(0.01)


def test_header_only_register_is_empty():
    register = MAHRegister(hazards=())
    assert MAHRegister.from_csv(io.StringIO(register.to_csv())) == register


def test_overflow_is_rejected():
    with pytest.raises(ValidationError):
        MajorAccidentHazard(
            **hazard_data(
                frequency_per_year=1e300,
                fatalities_per_event=1e300,
            )
        )
