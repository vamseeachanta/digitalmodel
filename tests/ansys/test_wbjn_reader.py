# ABOUTME: Unit tests for WBJNReader — IronPython Workbench journal metadata extraction
# ABOUTME: TDD Red phase: tests written before implementation

"""Tests for wbjn_reader — WBJNReader.read_metadata."""

from pathlib import Path

import pytest

from digitalmodel.ansys.wbjn_reader import WBJNReader
from digitalmodel.ansys.models import WBJNJournal

FIXTURES = Path(__file__).parent / "fixtures"
JOURNAL = FIXTURES / "journal_simple.wbjn"


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def _reader() -> WBJNReader:
    return WBJNReader()


def _journal() -> WBJNJournal:
    return _reader().read_metadata(JOURNAL)


# ---------------------------------------------------------------------------
# WBJNJournal structure
# ---------------------------------------------------------------------------

class TestWBJNJournalStructure:
    def test_returns_wbjn_journal(self):
        j = _journal()
        assert isinstance(j, WBJNJournal)

    def test_filepath_recorded(self):
        j = _journal()
        assert "journal_simple.wbjn" in j.filepath

    def test_release_version_extracted(self):
        j = _journal()
        assert j.release_version == "16.0"

    def test_system_names_extracted(self):
        j = _journal()
        assert "SYS" in j.system_names
        assert "Modal" in j.system_names

    def test_system_names_count(self):
        j = _journal()
        assert len(j.system_names) == 2

    def test_design_point_deletions_extracted(self):
        j = _journal()
        assert len(j.design_point_operations) == 2

    def test_design_point_deletion_names(self):
        j = _journal()
        # Should capture "Delete DP 3" and "Delete DP 4"
        ops_str = " ".join(j.design_point_operations)
        assert "3" in ops_str
        assert "4" in ops_str

    def test_parameter_expressions_extracted(self):
        j = _journal()
        assert len(j.parameter_expressions) == 2

    def test_parameter_expression_p1(self):
        j = _journal()
        param_names = [p for p, _ in j.parameter_expressions]
        assert "P1" in param_names

    def test_parameter_expression_p2(self):
        j = _journal()
        param_names = [p for p, _ in j.parameter_expressions]
        assert "P2" in param_names


# ---------------------------------------------------------------------------
# Edge cases
# ---------------------------------------------------------------------------

class TestWBJNReaderEdgeCases:
    def test_accepts_path_object(self):
        j = _reader().read_metadata(Path(JOURNAL))
        assert isinstance(j, WBJNJournal)

    def test_accepts_string_path(self):
        j = _reader().read_metadata(str(JOURNAL))
        assert isinstance(j, WBJNJournal)

    def test_empty_file_returns_empty_fields(self, tmp_path):
        wbjn = tmp_path / "empty.wbjn"
        wbjn.write_text("# encoding: utf-8\n")
        j = _reader().read_metadata(wbjn)
        assert j.release_version == ""
        assert j.system_names == []
        assert j.design_point_operations == []
        assert j.parameter_expressions == []

    def test_no_release_comment_returns_empty_version(self, tmp_path):
        wbjn = tmp_path / "norelease.wbjn"
        wbjn.write_text('system1 = GetSystem(Name="SYS")\n')
        j = _reader().read_metadata(wbjn)
        assert j.release_version == ""
