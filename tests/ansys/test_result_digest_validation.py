# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Offline digest-reader contracts using synthetic records only."""

import pytest

from digitalmodel.ansys.results_extractor import ResultsExtractor
from tests.ansys.golden_acceptance import parse_digest


@pytest.fixture(params=["extractor", "golden"])
def reader(request):
    """Keep each reader's public error type and label convention explicit."""
    if request.param == "extractor":
        return ResultsExtractor().parse_result_digest, ValueError, False
    return parse_digest, AssertionError, True


@pytest.mark.parametrize(
    "text",
    [
        pytest.param("", id="empty"),
        pytest.param(" \t\n ", id="whitespace-only"),
        pytest.param("a,1,b", id="odd-token-count"),
        pytest.param(",1", id="blank-label"),
        pytest.param("a,", id="blank-value"),
        pytest.param("a,1,,", id="empty-pair"),
        pytest.param("a,1, , ", id="whitespace-pair"),
        pytest.param("a,1,,b,2", id="internal-empty-token"),
        pytest.param("a,1,", id="trailing-comma"),
        pytest.param(",a,1", id="leading-comma"),
        pytest.param("a,1\n\nb,2", id="empty-record"),
        pytest.param("a,1,a,2", id="duplicate"),
        pytest.param("a,1, a ,2", id="padded-duplicate"),
        pytest.param("a,1,A,2", id="case-variant-duplicate"),
        pytest.param("Straße,1,STRASSE,2", id="casefold-duplicate"),
        pytest.param("a,NaN", id="nan"),
        pytest.param("a,-nan", id="signed-nan"),
        pytest.param("a,Infinity", id="infinity"),
        pytest.param("a,-inf", id="negative-infinity"),
        pytest.param("a,1_000", id="numeric-separator"),
        pytest.param("a,1D2", id="fortran-exponent"),
        pytest.param("a,1.2.3", id="multiple-points"),
        pytest.param("a,1e", id="missing-exponent"),
        pytest.param("a,one", id="nonnumeric"),
        pytest.param("a,1e101", id="excessive-positive-exponent"),
        pytest.param("a,1e-101", id="excessive-negative-exponent"),
        pytest.param("a," + "1" * 129, id="overlong-literal"),
    ],
)
def test_readers_reject_malformed_digest(reader, text):
    read, error, _ = reader
    with pytest.raises(error):
        read(text)


@pytest.mark.parametrize(
    "text,extractor_expected,golden_expected",
    [
        ("Zero,0", {"Zero": 0.0}, {"zero": 0.0}),
        ("a,-2.5,b,+3", {"a": -2.5, "b": 3.0}, {"a": -2.5, "b": 3.0}),
        ("A,1.25e2,B,-2E-2", {"A": 125.0, "B": -0.02}, {"a": 125.0, "b": -0.02}),
        ("Node,22.,a,.5", {"Node": 22.0, "a": 0.5}, {"node": 22.0, "a": 0.5}),
        (" \tStress , 12.5 , UC , 0.5 \n", {"Stress": 12.5, "UC": 0.5},
         {"stress": 12.5, "uc": 0.5}),
        ("Force,0\nMoment,-4.\n", {"Force": 0.0, "Moment": -4.0},
         {"force": 0.0, "moment": -4.0}),
        ("A,1\r\nB,2\r\n", {"A": 1.0, "B": 2.0}, {"a": 1.0, "b": 2.0}),
        ("Straße,1", {"Straße": 1.0}, {"straße": 1.0}),
        ("lo,1e-100,hi,1e100", {"lo": 1e-100, "hi": 1e100},
         {"lo": 1e-100, "hi": 1e100}),
    ],
)
def test_readers_preserve_valid_values_and_label_conventions(
    reader, text, extractor_expected, golden_expected
):
    read, _, lowercase = reader
    assert read(text) == (golden_expected if lowercase else extractor_expected)
