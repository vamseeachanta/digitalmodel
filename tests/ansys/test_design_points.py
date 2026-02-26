# ABOUTME: Unit tests for DesignPointReader — ANSYS DesignXplorer CSV log parsing
# ABOUTME: TDD Red phase: tests written before implementation

"""Tests for design_points — DesignPointReader.read."""

from pathlib import Path

import pytest

from digitalmodel.ansys.design_points import DesignPointReader
from digitalmodel.ansys.models import DesignPoint, ParametricStudy

FIXTURES = Path(__file__).parent / "fixtures"
DP_LOG = FIXTURES / "design_point_log.csv"


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def _reader() -> DesignPointReader:
    return DesignPointReader()


def _study() -> ParametricStudy:
    return _reader().read(DP_LOG)


# ---------------------------------------------------------------------------
# ParametricStudy structure
# ---------------------------------------------------------------------------

class TestParametricStudyStructure:
    def test_returns_parametric_study(self):
        study = _study()
        assert isinstance(study, ParametricStudy)

    def test_project_name_extracted(self):
        study = _study()
        assert study.project_name == "SplitBody_Mesh_rev3"

    def test_parameter_labels_extracted(self):
        study = _study()
        assert "P1" in study.parameter_labels
        assert "P2" in study.parameter_labels
        assert "P3" in study.parameter_labels

    def test_p1_label_text(self):
        study = _study()
        assert "Force Z Component" in study.parameter_labels["P1"]

    def test_p2_label_text(self):
        study = _study()
        assert "Force 2 Z Component" in study.parameter_labels["P2"]


# ---------------------------------------------------------------------------
# Design points count and structure
# ---------------------------------------------------------------------------

class TestDesignPointsParsing:
    def test_returns_five_design_points(self):
        study = _study()
        assert len(study.design_points) == 5

    def test_design_points_are_design_point_instances(self):
        study = _study()
        assert all(isinstance(dp, DesignPoint) for dp in study.design_points)

    def test_dp0_name(self):
        study = _study()
        dp0 = study.design_points[0]
        assert dp0.name == "DP 0"

    def test_dp0_index(self):
        study = _study()
        dp0 = study.design_points[0]
        assert dp0.index == 0

    def test_dp0_p1_value(self):
        study = _study()
        dp0 = study.design_points[0]
        assert dp0.parameters["P1"] == pytest.approx(1700000.0)

    def test_dp0_p2_value(self):
        study = _study()
        dp0 = study.design_points[0]
        assert dp0.parameters["P2"] == pytest.approx(-1700000.0)

    def test_dp1_p1_value(self):
        study = _study()
        dp1 = study.design_points[1]
        assert dp1.parameters["P1"] == pytest.approx(1000000.0)

    def test_skips_audit_comment_lines(self):
        """Audit comment lines embedded mid-file must not create extra DPs."""
        study = _study()
        # File has one embedded audit comment between DP 2 and DP 3
        assert len(study.design_points) == 5

    def test_dp_indices_sequential(self):
        study = _study()
        indices = [dp.index for dp in study.design_points]
        assert indices == list(range(5))


# ---------------------------------------------------------------------------
# Edge cases
# ---------------------------------------------------------------------------

class TestDesignPointEdgeCases:
    def test_accepts_path_object(self):
        study = _reader().read(Path(DP_LOG))
        assert len(study.design_points) == 5

    def test_accepts_string_path(self):
        study = _reader().read(str(DP_LOG))
        assert len(study.design_points) == 5

    def test_empty_file_returns_empty_design_points(self, tmp_path):
        csv = tmp_path / "empty.csv"
        csv.write_text("# This file is written by the ANSYS DesignXplorer\n")
        study = _reader().read(csv)
        assert study.design_points == []
