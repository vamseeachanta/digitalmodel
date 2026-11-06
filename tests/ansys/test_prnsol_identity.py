"""Scalar listing audit, not qualification of unaveraged tensors.

Format basis: ANSYS Mechanical APDL Command Reference, 2024 R2, PRNSOL,
Notes and Table 223 (component labels, averaging and surface limitations):
https://ansyshelp.ansys.com/public/Views/Secured/corp/v242/en/ans_cmd/Hlp_C_PRNSOL.html
Inputs are synthetic except for the retained native fixture regressions;
no engineering acceptance limit is used.
"""

from pathlib import Path

import numpy as np
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


@pytest.mark.parametrize("banner", [
    "***** POST1 NODAL STRESS LISTING *****",
    "LOAD STEP= 1 SUBSTEP= 1",
    "TIME= 1.0000",
])
def test_page_banner_requires_a_fresh_node_header(banner):
    text = f"NODE SEQV\n1 5\n\n{banner}\n2 9\n"
    with pytest.raises(ValueError, match="NODE header"):
        ResultsExtractor().parse_prnsol_block(text)


def test_two_pages_with_banners_preserve_column_identity():
    text = """***** POST1 NODAL STRESS LISTING *****
LOAD STEP= 1 SUBSTEP= 1
TIME= 1.0000
NODE SEQV SX
1 5 2

***** POST1 NODAL STRESS LISTING *****
LOAD STEP= 1 SUBSTEP= 1
TIME= 1.0000
NODE SX SEQV
2 3 9
"""
    result = ResultsExtractor().parse_prnsol_block(text, expected_node_ids=[1, 2])
    assert result.to_dict("records") == [
        {"node_id": 1, "value": 5}, {"node_id": 2, "value": 9},
    ]


def test_non_row_text_after_page_banner_preserves_rows():
    text = "NODE SEQV\n1 5\nTIME= 1.0000\nunknown annotation\nNODE SEQV\n2 9"
    result = ResultsExtractor().parse_prnsol_block(text, expected_node_ids=[1, 2])
    assert result["value"].tolist() == [5, 9]


@pytest.mark.parametrize("pages", [1, 2, 9])
def test_native_page_preamble_preserves_scalar_rows(pages):
    """Retained n4_recovery page layout with synthetic space-separated rows."""
    text = "".join(
        f"""***** POST1 NODAL STRESS LISTING *****
LOAD STEP= 1 SUBSTEP= 1
TIME= 1.0000 LOAD CASE= 0
THE FOLLOWING X,Y,Z VALUES ARE IN GLOBAL COORDINATES
NODE SX SY SZ SXY SYZ SXZ
{node} -12 3 4 5 6 7

"""
        for node in range(1, pages + 1)
    )
    result = ResultsExtractor().parse_prnsol_block(
        text, "SX", expected_node_ids=range(1, pages + 1),
    )
    assert result["value"].tolist() == [-12] * pages


@pytest.mark.parametrize("row", ["2 9", "0 9", "-2 9", "+2 9", "2 *****"])
def test_row_after_page_preamble_requires_a_fresh_header(row):
    text = (
        "NODE SEQV\n1 5\nTIME= 1.0000\n"
        f"THE FOLLOWING X,Y,Z VALUES ARE IN GLOBAL COORDINATES\n{row}\n"
    )
    with pytest.raises(ValueError, match="NODE header"):
        ResultsExtractor().parse_prnsol_block(text)


@pytest.mark.parametrize("later_banner, field", [
    ("LOAD STEP= 2 SUBSTEP= 1\nTIME= 1.0000", "LOAD STEP"),
    ("LOAD STEP= 1 SUBSTEP= 2\nTIME= 1.0000", "SUBSTEP"),
    ("LOAD STEP= 1 SUBSTEP= 1\nTIME= 2.0000", "TIME"),
    ("TIME= 2.0000", "TIME"),
    ("SUBSTEP= 2", "SUBSTEP"),
])
def test_pages_with_different_step_identity_are_rejected(later_banner, field):
    text = (
        "LOAD STEP= 1 SUBSTEP= 1\nTIME= 1.0000\nNODE SEQV\n1 5\n"
        f"{later_banner}\nNODE SEQV\n2 9\n3 12\n"
    )
    with pytest.raises(ValueError, match=field):
        ResultsExtractor().parse_prnsol_block(text)


def test_page_identity_is_captured_before_the_first_node_header():
    text = "LOAD STEP= 1\nLOAD STEP= 2\nNODE SEQV\n1 5\n"
    with pytest.raises(ValueError, match="LOAD STEP"):
        ResultsExtractor().parse_prnsol_block(text)


@pytest.mark.parametrize("banner, field", [
    ("LOAD STEP= invalid", "LOAD STEP"),
    ("SUBSTEP= 1.5", "SUBSTEP"),
    ("TIME= NaN", "TIME"),
    ("TIME= inf", "TIME"),
    ("TIME= *****", "TIME"),
])
def test_invalid_explicit_page_identity_is_rejected(banner, field):
    with pytest.raises(ValueError, match=field):
        ResultsExtractor().parse_prnsol_block(f"{banner}\nNODE SEQV\n1 5\n")


def test_time_identity_comparison_preserves_decimal_precision():
    text = (
        "TIME= 1.00000000000000000001\nNODE SEQV\n1 5\n"
        "TIME= 1.00000000000000000002\nNODE SEQV\n2 9\n"
    )
    with pytest.raises(ValueError, match="TIME"):
        ResultsExtractor().parse_prnsol_block(text)


def test_equivalent_page_identity_notation_is_accepted():
    text = (
        "LOAD STEP= 01 SUBSTEP= 01\nTIME= 1.0000\nNODE SEQV\n1 5\n"
        "load step = 1 substep = 1\ntime = 1.0D+00\nNODE SEQV\n2 9\n"
    )
    result = ResultsExtractor().parse_prnsol_block(text, expected_node_ids=[1, 2])
    assert result["value"].tolist() == [5, 9]


def test_missing_page_identity_exposes_captured_metadata():
    text = (
        "NODE SEQV\n1 5\nLOAD STEP= 2 SUBSTEP= 3\nTIME= 4\nNODE SEQV\n2 9\n"
        "***** POST1 NODAL STRESS LISTING *****\nNODE SEQV\n3 12\n"
    )
    result = ResultsExtractor().parse_prnsol_block(text, expected_node_ids=[1, 2, 3])
    assert result["value"].tolist() == [5, 9, 12]
    assert result.attrs["result_identity"] == {
        "LOAD STEP": 2, "SUBSTEP": 3, "TIME": 4,
    }


@pytest.mark.parametrize("component, expected", [
    ("SX", -0.9986391738718805E+001),
    ("SZ", 0.1301786994303694E+003),
    ("SXY", -0.8493660944317938E-010),
])
def test_native_full_precision_fields_preserve_component_values(component, expected):
    text = (Path(__file__).parent / "fixtures/n4_recovery/stress.txt").read_text()
    result = ResultsExtractor().parse_prnsol_block(
        text, component, expected_node_ids=[85, 89, 93, 169, 173, 177, 253, 257, 261],
    )
    assert result.loc[result["node_id"] == 85, "value"].item() == expected


@pytest.mark.parametrize("mutation", ["truncate", "overflow", "missing_exponent"])
def test_native_full_precision_fields_reject_malformed_rows(mutation):
    text = (Path(__file__).parent / "fixtures/n4_recovery/stress.txt").read_text()
    row = next(line for line in text.splitlines() if line.split()[:1] == ["85"])
    if mutation == "truncate":
        row = row[:-1]
    elif mutation == "overflow":
        row = row[:10] + "*" * 24 + row[34:]
    else:
        row = row[:10] + "1.25-02".rjust(24) + row[34:]
    with pytest.raises(ValueError, match="column count|numeric"):
        ResultsExtractor().parse_prnsol_block(f"NODE SX SY SZ SXY SYZ SXZ\n{row}", "SX")


@pytest.mark.parametrize("dtype", [np.int32, np.int64, np.uint64])
def test_numpy_integral_node_manifest_is_accepted(dtype):
    result = ResultsExtractor().parse_prnsol_block(
        "NODE SEQV\n1 5\n2 9", expected_node_ids=np.array([1, 2], dtype=dtype),
    )
    assert result["node_id"].tolist() == [1, 2]


@pytest.mark.parametrize("expected, reason", [
    ([np.bool_(True)], "expected node IDs"),
    ([np.int64(-1)], "expected node IDs"),
    ([1, np.int64(1)], "duplicate expected"),
])
def test_numpy_node_manifest_rejects_invalid_or_duplicate_ids(expected, reason):
    with pytest.raises(ValueError, match=reason):
        ResultsExtractor().parse_prnsol_block("NODE SEQV\n1 5", expected_node_ids=expected)


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


@pytest.mark.parametrize("pages", [2, 9])
def test_complete_native_page_header_preserves_rows(pages):
    """Repeat the retained scalar page preamble with hand-valued rows."""
    fixtures = Path(__file__).parent / "fixtures/n4_recovery"
    lines = (fixtures / "stress.txt").read_text().splitlines()
    preamble = "\n".join(lines[3:17]) + "\n"
    text = "".join(
        f"{preamble}NODE SX SY SZ SXY SYZ SXZ\n{node} -12 3 4 5 6 7\n"
        for node in range(1, pages + 1)
    )
    result = ResultsExtractor().parse_prnsol_block(
        text, "SX", expected_node_ids=range(1, pages + 1),
    )
    assert result["value"].tolist() == [-12] * pages
    assert result.attrs["result_identity"]["TIME"] == 1


def test_native_stress_rows_survive_a_complete_page_break():
    fixtures = Path(__file__).parent / "fixtures/n4_recovery"
    lines = (fixtures / "stress.txt").read_text().splitlines()
    split = next(i for i, line in enumerate(lines) if line.split()[:1] == ["169"]) + 1
    text = "\n".join(lines[:split] + lines[3:19] + lines[split:])
    result = ResultsExtractor().parse_prnsol_block(
        text, "SX", expected_node_ids=[85, 89, 93, 169, 173, 177, 253, 257, 261],
    )
    assert result.loc[result["node_id"] == 85, "value"].item() == -0.9986391738718805E+001


def test_absent_page_identity_is_exposed_as_empty_metadata():
    result = ResultsExtractor().parse_prnsol_block("NODE SEQV\n1 5\n")
    assert result.attrs["result_identity"] == {}


@pytest.mark.parametrize("indicator", ["both", "banner", "element"])
def test_single_element_listing_is_rejected(indicator):
    """The retained first element block is never a scalar nodal result."""
    native = (Path(__file__).parent / "fixtures/n4_recovery/presol.txt").read_text()
    first = native.index("  ELEMENT=")
    text = native[:native.index("  ELEMENT=", first + 1)]
    if indicator == "banner":
        text = "\n".join(line for line in text.splitlines() if "ELEMENT=" not in line)
    elif indicator == "element":
        text = "\n".join(
            line for line in text.splitlines() if "ELEMENT NODAL" not in line
        )
    with pytest.raises(ValueError, match="element listing"):
        ResultsExtractor().parse_prnsol_block(text, "SX")


@pytest.mark.parametrize("prefix", [
    "", "TIME= 1\n", "NODE SX\n1 5\n", "NODE SX\n1 5\nMINIMUM VALUES\n",
])
def test_element_marker_is_rejected_in_every_parser_state(prefix):
    with pytest.raises(ValueError, match="element listing"):
        ResultsExtractor().parse_prnsol_block(prefix + " element = 1\n", "SX")


def test_older_ansys_page_banner_preserves_rows():
    """Synthetic older banner spelling with hand lookup: node 2 has SX=-12."""
    text = (
        "NODE SX\n1 5\n"
        " *** ANSYS - ENGINEERING ANALYSIS SYSTEM ***\n"
        "NODE SX\n2 -12\n"
    )
    result = ResultsExtractor().parse_prnsol_block(text, "SX", expected_node_ids=[1, 2])
    assert result["value"].tolist() == [5, -12]
