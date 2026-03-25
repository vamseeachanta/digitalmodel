#!/usr/bin/env python
"""
Tests for Phase 4 extraction layer used by pipeline postprocessing scripts.

Tests the new export_rangegraph_csvs integration without requiring OrcFxAPI or sim files.
"""
import sys
from pathlib import Path
from unittest.mock import MagicMock, patch

import pytest


def _mock_pipeline(name="pipeline", n_segs=5, seg_length=10.0):
    """Return a MagicMock mimicking an OrcaFlex Line (pipeline) object."""
    line = MagicMock()
    line.Name = name
    line.NumberOfSegments = n_segs
    line.SegmentLength = [seg_length] * n_segs
    line.NodeArclengths = [i * seg_length for i in range(n_segs + 1)]

    rg = MagicMock()
    rg.X = [i * seg_length for i in range(n_segs + 1)]
    rg.Max = [1100.0] * (n_segs + 1)
    rg.Min = [900.0] * (n_segs + 1)
    rg.Mean = [1000.0] * (n_segs + 1)
    line.RangeGraph = MagicMock(return_value=rg)
    line.TimeHistory = MagicMock(return_value=[1000.0] * 10)

    return line


class TestExportRangegraphCsvsIntegration:
    """Test export_rangegraph_csvs with mock OrcaFlex objects."""

    def test_csv_written_for_pipeline(self, tmp_path):
        from digitalmodel.solvers.orcaflex.reporting.extractors.aggregator import (
            export_rangegraph_csvs,
        )

        mock_ofx = MagicMock()
        pipeline = _mock_pipeline()

        with patch(
            "digitalmodel.solvers.orcaflex.reporting.extractors.aggregator.ofx",
            mock_ofx,
        ):
            paths = export_rangegraph_csvs(
                lines=[pipeline],
                variables=["Effective Tension", "Max Bending Stress"],
                period=mock_ofx.pnLatestWave,
                output_dir=tmp_path,
            )

        assert len(paths) == 1
        assert paths[0].exists()

    def test_csv_has_required_columns(self, tmp_path):
        from digitalmodel.solvers.orcaflex.reporting.extractors.aggregator import (
            export_rangegraph_csvs,
        )

        mock_ofx = MagicMock()
        pipeline = _mock_pipeline()

        with patch(
            "digitalmodel.solvers.orcaflex.reporting.extractors.aggregator.ofx",
            mock_ofx,
        ):
            paths = export_rangegraph_csvs(
                lines=[pipeline],
                variables=["Effective Tension"],
                period=mock_ofx.pnLatestWave,
                output_dir=tmp_path,
            )

        import csv
        with open(paths[0]) as f:
            headers = csv.DictReader(f).fieldnames
        assert "ArcLength_m" in headers
        assert "Effective_Tension_Min" in headers
        assert "Effective_Tension_Max" in headers
        assert "Effective_Tension_Mean" in headers

    def test_output_dir_created_if_missing(self, tmp_path):
        from digitalmodel.solvers.orcaflex.reporting.extractors.aggregator import (
            export_rangegraph_csvs,
        )

        mock_ofx = MagicMock()
        nested = tmp_path / "rangegraphs" / "sim_001"

        with patch(
            "digitalmodel.solvers.orcaflex.reporting.extractors.aggregator.ofx",
            mock_ofx,
        ):
            export_rangegraph_csvs(
                lines=[_mock_pipeline()],
                variables=["Effective Tension"],
                period=mock_ofx.pnLatestWave,
                output_dir=nested,
            )

        assert nested.exists()

    def test_raises_without_orcfxapi(self, tmp_path):
        from digitalmodel.solvers.orcaflex.reporting.extractors.aggregator import (
            export_rangegraph_csvs,
        )

        with patch(
            "digitalmodel.solvers.orcaflex.reporting.extractors.aggregator.ofx",
            None,
        ):
            with pytest.raises(ImportError, match="OrcFxAPI"):
                export_rangegraph_csvs(
                    lines=[_mock_pipeline()],
                    variables=["Effective Tension"],
                    period=None,
                    output_dir=tmp_path,
                )


class TestPhase4ImportGuard:
    """Verify scripts remain runnable when digitalmodel package is absent."""

    def test_generate_html_report_handles_missing_phase4(self):
        """PHASE4_AVAILABLE flag prevents ImportError when package not installed."""
        import importlib
        import importlib.util

        script_path = (
            Path(__file__).parent / "generate_html_report.py"
        )
        assert script_path.exists(), "generate_html_report.py not found"

        src = script_path.read_text(encoding="utf-8")
        assert "PHASE4_AVAILABLE" in src, "Script must guard Phase 4 imports"
        assert "try:" in src, "Script must use try/except for Phase 4 imports"

    def test_generate_env_case_report_handles_missing_phase4(self):
        """PHASE4_AVAILABLE flag prevents ImportError when package not installed."""
        script_path = (
            Path(__file__).parent / "generate_env_case_report.py"
        )
        assert script_path.exists(), "generate_env_case_report.py not found"

        src = script_path.read_text(encoding="utf-8")
        assert "PHASE4_AVAILABLE" in src, "Script must guard Phase 4 imports"
        assert "try:" in src, "Script must use try/except for Phase 4 imports"
