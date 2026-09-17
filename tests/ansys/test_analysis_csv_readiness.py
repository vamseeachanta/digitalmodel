"""Flat result exports visibly distinguish diagnostic values from reuse."""
import csv
import io

import pytest

from tests.ansys.test_analysis_evidence import intake
from digitalmodel.ansys.analysis_evidence import build_package, response_csv, read_response_csv


def test_flat_diagnostic_value_is_labelled_and_round_trips(intake):
    study, resolver = intake
    study["cases"][0]["capture_role"] = "diagnostic_fixture"
    text = response_csv(build_package(study, resolver))
    row = next(csv.DictReader(io.StringIO(text)))
    assert row["value_type"] == "diagnostic_decimal"
    assert read_response_csv(text)[0]["response"]["value"] == "12.5"
    with pytest.raises(ValueError, match="typed columns"):
        read_response_csv(text.replace(",diagnostic_decimal,", ",decimal,"))
