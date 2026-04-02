# ABOUTME: Tests for ResultsExtractor — ANSYS output file parsing
# ABOUTME: Verifies regex-based extraction of stress, displacement, convergence data

"""Tests for results_extractor — ResultsExtractor output parsing."""

import pytest
import pandas as pd

from digitalmodel.ansys.results_extractor import (
    ResultsExtractor,
    ResultSummary,
    ConvergenceRecord,
)


def _ext() -> ResultsExtractor:
    return ResultsExtractor()


# ---------------------------------------------------------------------------
# Summary extraction
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
# PRNSOL parsing
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
# Element table parsing
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
# Convergence history
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
# Max values extraction
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
