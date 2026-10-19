"""Scalar listing audit, not qualification of unaveraged tensors.

Format basis: ANSYS Mechanical APDL Command Reference, 2024 R2, PRNSOL,
Notes and Table 223 (component labels, averaging and surface limitations):
https://ansyshelp.ansys.com/public/Views/Secured/corp/v242/en/ans_cmd/Hlp_C_PRNSOL.html
All numbers below are synthetic inputs; no engineering acceptance limit is used.
"""

import pytest

from digitalmodel.ansys.results_extractor import ResultsExtractor


def test_selects_requested_column_instead_of_last_value():
    """Hand lookup: SX is -12 and SEQV is 30, despite SX being last."""
    text = "NODE SEQV SX\n1 30 -12\n"
    extractor = ResultsExtractor()
    assert extractor.parse_prnsol_block(text, "sx")["value"].tolist() == [-12]
    assert extractor.parse_prnsol_block(text)["value"].tolist() == [30]


@pytest.mark.parametrize("number", ["1.25D+02", "1.25d+02", "1.25E+02", "+125."])
def test_explicit_exponents_preserve_the_value(number):
    """Hand conversion: 1.25 times ten squared equals 125."""
    result = ResultsExtractor().parse_prnsol_block(f"NODE SEQV\n1 {number}\n")
    assert result["value"].tolist() == [125]


@pytest.mark.parametrize(
    "text, reason",
    [
        ("NODE SX\n1 5", "component"),
        ("NODE SEQV SEQV\n1 5 6", "duplicate column"),
        ("NODE SX SEQV\n1 5", "column count"),
        ("NODE SEQV\n1 5 6", "column count"),
        ("NODE SEQV\n1 1.25-02", "numeric"),
        ("NODE SEQV\n1 1.25+02", "numeric"),
        ("NODE SEQV\n1 *****", "numeric"),
        ("NODE SEQV\n1 NaN", "numeric"),
        ("NODE SEQV\n1 inf", "numeric"),
        ("NODE SEQV\n1 1E999", "finite"),
        ("NODE SX SEQV\n1 ***** 5", "numeric"),
        ("NODE SEQV\n1 5\n1 6", "duplicate node"),
        ("NODE SEQV\n0 5", "node ID"),
        ("NODE SEQV\n1.5 5", "node ID"),
        ("NODE SEQV\n1 5\ntruncated row", "node ID"),
    ],
)
def test_rejects_ambiguous_or_incomplete_scalar_rows(text, reason):
    """Each supplied row violates its header or scalar node identity."""
    with pytest.raises(ValueError, match=reason):
        ResultsExtractor().parse_prnsol_block(text)


def test_repeated_headers_and_blank_lines_preserve_rows():
    """Hand lookup over two pages: node 1 -> 5; node 2 -> 9."""
    text = "NODE SEQV SX\n1 5 2\n\nNODE SX SEQV\n2 3 9\n"
    result = ResultsExtractor().parse_prnsol_block(text)
    assert result.to_dict("records") == [
        {"node_id": 1, "value": 5}, {"node_id": 2, "value": 9},
    ]


def test_extrema_footer_is_not_a_result_row():
    """The summary node/value lines do not add to the one data row."""
    text = "NODE SEQV\n1 5\n\nMINIMUM VALUES\nNODE 1\nVALUE 5\n"
    assert ResultsExtractor().parse_prnsol_block(text)["value"].tolist() == [5]


def test_empty_listing_keeps_legacy_empty_dataframe():
    """No listing supplies zero rows; completeness is not asserted."""
    result = ResultsExtractor().parse_prnsol_block("")
    assert result.empty
    assert list(result.columns) == ["node_id", "value"]


def test_expected_nodes_preserve_an_interior_scalar_peak():
    """Hand lookup: face values 2 and 3; interior node 2 has maximum 20.

    The expected set is supplied independently, not derived from the listing.
    This scalar example establishes coverage only, not a tensor trace check.
    """
    extractor = ResultsExtractor()
    result = extractor.parse_prnsol_block(
        "NODE SEQV\n1 2\n2 20\n3 3\n", expected_node_ids=[3, 1, 2],
    )
    assert result.loc[result["value"].idxmax(), "node_id"] == 2
    assert extractor.extract_max_values(result)["max"] == 20


@pytest.mark.parametrize(
    "text, expected, reason",
    [
        ("NODE SEQV\n1 2\n3 3", [1, 2, 3], "missing=\\[2\\]"),
        ("NODE SEQV\n1 2\n2 20\n3 3", [1, 3], "unexpected=\\[2\\]"),
        ("", [1], "missing=\\[1\\]"),
        ("NODE SEQV", [1], "missing=\\[1\\]"),
        ("NODE SEQV\n1 2", [1, 1], "duplicate expected"),
        ("NODE SEQV\n1 2", [True], "expected node IDs"),
        ("NODE SEQV\n1 2", [1.0], "expected node IDs"),
        ("NODE SEQV\n1 2", ["1"], "expected node IDs"),
        ("NODE SEQV\n1 2", [0], "expected node IDs"),
        ("NODE SEQV\n1 2", [], "unexpected=\\[1\\]"),
    ],
)
def test_expected_node_manifest_requires_exact_coverage(text, expected, reason):
    """Exact set difference gives the absent or extra synthetic node IDs."""
    with pytest.raises(ValueError, match=reason):
        ResultsExtractor().parse_prnsol_block(text, expected_node_ids=expected)
