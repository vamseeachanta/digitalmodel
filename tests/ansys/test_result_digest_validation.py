# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Digest protocol regressions; all values are synthetic parser inputs."""

import pytest

from digitalmodel.ansys.results_extractor import ResultsExtractor


@pytest.mark.parametrize(
    "text",
    [
        "",
        " \n ",
        "peak,1,",
        "peak,,uc,1",
        "peak,,1,,uc,1",
        ",1,uc,2",
        "peak,1\nuc",
        "peak,\n1,uc,2",
        "peak,1,peak,2",
        "peak,1, PEAK ,2",
        "peak,nan",
        "peak,inf",
        "peak,-inf",
        "peak,1e999",
        "peak,********",
        "peak,1_0",
        "peak,\u0661",
        "peak value,1",
    ],
)
def test_malformed_digest_is_rejected(text):
    """Missing, overwritten or nonfinite evidence cannot support a result."""
    with pytest.raises(ValueError):
        ResultsExtractor().parse_result_digest(text)


def test_multiline_digest_normalizes_labels_and_preserves_signed_values():
    """Hand reading: three fields, values 22, -300 and 400; no calculation."""
    text = " PEAK_NODE ,22.\r\nreaction_fx_n,-3.0E+02\r\nreaction_fy_n,400.\r\n"
    assert ResultsExtractor().parse_result_digest(text) == {
        "peak_node": 22.0,
        "reaction_fx_n": -300.0,
        "reaction_fy_n": 400.0,
    }


@pytest.mark.parametrize("text", ["", "peak,1,", "peak,,1,,uc,1", "peak,\n1,uc,2"])
def test_golden_reader_rejects_the_same_incomplete_evidence(text):
    """Golden and production checks must preserve the same field boundaries."""
    from tests.ansys.golden_acceptance import parse_digest

    with pytest.raises(AssertionError):
        parse_digest(text)


@pytest.mark.parametrize(
    "raw,expected", [("22.", 22.0), ("+.25", 0.25), ("-3.0E+02", -300.0)]
)
def test_ascii_fixed_and_exponential_numbers_are_read(raw, expected):
    """Direct reading of synthetic F/E-format tokens emitted by the decks."""
    assert ResultsExtractor().parse_result_digest(f"value,{raw}") == {"value": expected}
