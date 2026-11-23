# ABOUTME: Tests for ResultsExtractor — ANSYS output file parsing
# ABOUTME: Verifies regex-based extraction of stress, displacement, convergence, export, comparison

"""Tests for results_extractor — ResultsExtractor output parsing, export, and comparison."""

import json
import pytest
import pandas as pd

from digitalmodel.ansys.results_extractor import (
    ComparisonRow,
    ConvergenceRecord,
    DisplacementSummary,
    ResultsExtractor,
    ResultSummary,
    StressSummary,
)


def _ext() -> ResultsExtractor:
    return ResultsExtractor()


# ---------------------------------------------------------------------------
# Summary extraction (existing)
# ---------------------------------------------------------------------------

SAMPLE_OUTPUT = """\
 ***** ANSYS - ENGINEERING ANALYSIS SYSTEM *****

 NUMBER OF NODES   =    15432
 NUMBER OF ELEMENTS=     8721

 SUBSTEP= 1  CUM. ITER.= 5
 FORCE CONVERGENCE VALUE  =  0.1234E+02  CRITERION=  0.5000E+02

 SUBSTEP= 2  CUM. ITER.= 10
 FORCE CONVERGENCE VALUE  =  0.5000E-01  CRITERION=  0.5000E+02

 SOLUTION IS CONVERGED

 MAXIMUM SEQV =  185.3  AT NODE=  4521
 MAXIMUM USUM =  0.452  AT NODE=  3201
 TOTAL VALUES:  50000.0
"""


class TestExtractSummary:
    def test_extracts_total_nodes(self):
        summary = _ext().extract_summary(SAMPLE_OUTPUT)
        assert summary.total_nodes == 15432

    def test_extracts_total_elements(self):
        summary = _ext().extract_summary(SAMPLE_OUTPUT)
        assert summary.total_elements == 8721

    def test_extracts_max_von_mises(self):
        summary = _ext().extract_summary(SAMPLE_OUTPUT)
        assert summary.max_von_mises_mpa == pytest.approx(185.3)

    def test_extracts_max_von_mises_node(self):
        summary = _ext().extract_summary(SAMPLE_OUTPUT)
        assert summary.max_von_mises_node == 4521

    def test_extracts_max_displacement(self):
        summary = _ext().extract_summary(SAMPLE_OUTPUT)
        assert summary.max_displacement_mm == pytest.approx(0.452)

    def test_detects_convergence(self):
        summary = _ext().extract_summary(SAMPLE_OUTPUT)
        assert summary.converged is True

    def test_counts_substeps(self):
        summary = _ext().extract_summary(SAMPLE_OUTPUT)
        assert summary.num_substeps == 2

    def test_extracts_reaction_force(self):
        summary = _ext().extract_summary(SAMPLE_OUTPUT)
        assert summary.total_reaction_force_n == pytest.approx(50000.0)


class TestExtractSummaryEmpty:
    def test_empty_text_returns_defaults(self):
        summary = _ext().extract_summary("")
        assert summary.total_nodes == 0
        assert summary.max_von_mises_mpa is None
        assert summary.converged is False


# ---------------------------------------------------------------------------
# PRNSOL parsing (existing)
# ---------------------------------------------------------------------------

PRNSOL_BLOCK = """\
 PRINT S    NODAL SOLUTION PER NODE

  NODE     SX          SY          SZ         SEQV
     1   100.5       -50.2        30.1       120.3
     2    80.0       -40.0        25.0       100.0
     3   150.0       -60.0        40.0       180.5

 MINIMUM VALUES
     NODE         2
     VALUE    80.000
"""


class TestParsePrnsol:
    def test_returns_dataframe(self):
        df = _ext().parse_prnsol_block(PRNSOL_BLOCK)
        assert isinstance(df, pd.DataFrame)

    def test_correct_row_count(self):
        df = _ext().parse_prnsol_block(PRNSOL_BLOCK)
        assert len(df) == 3

    def test_contains_node_id_column(self):
        df = _ext().parse_prnsol_block(PRNSOL_BLOCK)
        assert "node_id" in df.columns


# ---------------------------------------------------------------------------
# Element table parsing (existing)
# ---------------------------------------------------------------------------

PRETAB_BLOCK = """\
 ELEMENT TABLE LISTING

  ELEM    SMISES    SMAX
     1    120.5     150.2
     2     95.3     110.0
     3    200.1     250.4

"""


class TestParseElementTable:
    def test_returns_dataframe(self):
        df = _ext().parse_element_table(PRETAB_BLOCK)
        assert isinstance(df, pd.DataFrame)

    def test_correct_row_count(self):
        df = _ext().parse_element_table(PRETAB_BLOCK)
        assert len(df) == 3

    def test_element_id_column_present(self):
        df = _ext().parse_element_table(PRETAB_BLOCK)
        assert "element_id" in df.columns


# ---------------------------------------------------------------------------
# Convergence history (existing)
# ---------------------------------------------------------------------------

class TestParseConvergenceHistory:
    def test_parses_convergence_records(self):
        records = _ext().parse_convergence_history(SAMPLE_OUTPUT)
        assert len(records) >= 2

    def test_records_have_substep(self):
        records = _ext().parse_convergence_history(SAMPLE_OUTPUT)
        assert all(isinstance(r, ConvergenceRecord) for r in records)

    def test_empty_text_returns_empty(self):
        records = _ext().parse_convergence_history("")
        assert records == []


# ---------------------------------------------------------------------------
# Max values extraction (existing)
# ---------------------------------------------------------------------------

class TestExtractMaxValues:
    def test_extracts_max(self):
        df = pd.DataFrame({"value": [10.0, 20.0, 30.0]})
        stats = _ext().extract_max_values(df)
        assert stats["max"] == pytest.approx(30.0)

    def test_extracts_min(self):
        df = pd.DataFrame({"value": [-5.0, 10.0, 20.0]})
        stats = _ext().extract_max_values(df)
        assert stats["min"] == pytest.approx(-5.0)

    def test_empty_df_returns_zeros(self):
        stats = _ext().extract_max_values(pd.DataFrame())
        assert stats["max"] == 0.0


# ---------------------------------------------------------------------------
# Stress summary extraction (NEW)
# ---------------------------------------------------------------------------

class TestStressSummary:
    def test_extracts_von_mises(self):
        ss = _ext().extract_stress_summary(SAMPLE_OUTPUT)
        assert ss.max_von_mises_mpa == pytest.approx(185.3)

    def test_extracts_location_node(self):
        ss = _ext().extract_stress_summary(SAMPLE_OUTPUT)
        assert ss.location_node == 4521

    def test_empty_text_returns_zero(self):
        ss = _ext().extract_stress_summary("")
        assert ss.max_von_mises_mpa == 0.0


# ---------------------------------------------------------------------------
# Displacement summary extraction (NEW)
# ---------------------------------------------------------------------------

class TestDisplacementSummary:
    def test_extracts_total_displacement(self):
        ds = _ext().extract_displacements(SAMPLE_OUTPUT)
        assert ds.max_total_mm == pytest.approx(0.452)

    def test_extracts_displacement_node(self):
        ds = _ext().extract_displacements(SAMPLE_OUTPUT)
        assert ds.location_node == 3201

    def test_empty_text_returns_zero(self):
        ds = _ext().extract_displacements("")
        assert ds.max_total_mm == 0.0


# ---------------------------------------------------------------------------
# JSON/CSV export (NEW)
# ---------------------------------------------------------------------------

class TestExportResults:
    def test_json_export_is_valid(self):
        summary = _ext().extract_summary(SAMPLE_OUTPUT)
        result = _ext().export_results(summary, format="json")
        data = json.loads(result)
        assert data["max_von_mises_mpa"] == pytest.approx(185.3)

    def test_json_export_contains_all_fields(self):
        summary = _ext().extract_summary(SAMPLE_OUTPUT)
        result = _ext().export_results(summary, format="json")
        data = json.loads(result)
        assert "total_nodes" in data
        assert "converged" in data
        assert "num_substeps" in data

    def test_csv_export_has_header(self):
        summary = _ext().extract_summary(SAMPLE_OUTPUT)
        result = _ext().export_results(summary, format="csv")
        assert "max_von_mises_mpa" in result
        assert "total_nodes" in result

    def test_csv_export_has_data_row(self):
        summary = _ext().extract_summary(SAMPLE_OUTPUT)
        result = _ext().export_results(summary, format="csv")
        lines = [l for l in result.strip().split("\n") if l]
        assert len(lines) == 2  # header + data


# ---------------------------------------------------------------------------
# Multi-load-case comparison (NEW)
# ---------------------------------------------------------------------------

CASE_A_OUTPUT = """\
 NUMBER OF NODES   =    5000
 NUMBER OF ELEMENTS=    3000
 SOLUTION IS CONVERGED
 MAXIMUM SEQV =  120.0  AT NODE=  100
 MAXIMUM USUM =  0.300  AT NODE=  200
"""

CASE_B_OUTPUT = """\
 NUMBER OF NODES   =    5000
 NUMBER OF ELEMENTS=    3000
 SOLUTION IS CONVERGED
 MAXIMUM SEQV =  200.0  AT NODE=  300
 MAXIMUM USUM =  0.500  AT NODE=  400
"""

CASE_C_UNCONVERGED = """\
 NUMBER OF NODES   =    5000
 *** WARNING ***
"""


class TestCompareLoadCases:
    def test_returns_correct_count(self):
        cases = [
            ("Operating", CASE_A_OUTPUT),
            ("Hydrotest", CASE_B_OUTPUT),
        ]
        rows = _ext().compare_load_cases(cases)
        assert len(rows) == 2

    def test_identifies_max_stress_case(self):
        cases = [
            ("Operating", CASE_A_OUTPUT),
            ("Hydrotest", CASE_B_OUTPUT),
        ]
        rows = _ext().compare_load_cases(cases)
        stresses = {r.load_case_name: r.max_stress_mpa for r in rows}
        assert stresses["Hydrotest"] > stresses["Operating"]

    def test_identifies_unconverged(self):
        cases = [
            ("Good", CASE_A_OUTPUT),
            ("Bad", CASE_C_UNCONVERGED),
        ]
        rows = _ext().compare_load_cases(cases)
        bad_row = [r for r in rows if r.load_case_name == "Bad"][0]
        assert bad_row.converged is False

    def test_comparison_includes_nodes_elements(self):
        cases = [("A", CASE_A_OUTPUT)]
        rows = _ext().compare_load_cases(cases)
        assert rows[0].total_nodes == 5000
        assert rows[0].total_elements == 3000

    def test_export_comparison_json(self):
        cases = [
            ("A", CASE_A_OUTPUT),
            ("B", CASE_B_OUTPUT),
        ]
        rows = _ext().compare_load_cases(cases)
        result = _ext().export_comparison(rows, format="json")
        data = json.loads(result)
        assert len(data) == 2
        assert data[0]["load_case"] == "A"

    def test_export_comparison_csv(self):
        cases = [("A", CASE_A_OUTPUT)]
        rows = _ext().compare_load_cases(cases)
        result = _ext().export_comparison(rows, format="csv")
        assert "load_case" in result
        assert "max_stress_mpa" in result
