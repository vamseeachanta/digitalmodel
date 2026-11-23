"""Tests for ResultExtractor (WRK-030)."""
from __future__ import annotations

from pathlib import Path
from unittest.mock import MagicMock, patch

import pytest

from digitalmodel.hydrodynamics.diffraction.orcawave_runner import RunResult, RunStatus
from digitalmodel.hydrodynamics.diffraction.result_extractor import (
    ExtractionResult,
    ResultExtractor,
)


class TestExtractionResultDefaults:
    """Test ExtractionResult dataclass defaults."""

    def test_default_success_false(self) -> None:
        er = ExtractionResult(success=False)
        assert er.success is False
        assert er.results is None
        assert er.model_file is None
        assert er.error_message is None

    def test_success_with_results(self, mock_diffraction_results) -> None:
        er = ExtractionResult(success=True, results=mock_diffraction_results)
        assert er.success is True
        assert er.results is not None


class TestFindModelFile:
    """Test model file discovery."""

    def test_finds_sim_file(self, tmp_path: Path) -> None:
        sim = tmp_path / "model.sim"
        sim.touch()
        extractor = ResultExtractor()
        found = extractor.find_model_file(tmp_path)
        assert found == sim

    def test_finds_dat_file(self, tmp_path: Path) -> None:
        dat = tmp_path / "results.dat"
        dat.touch()
        extractor = ResultExtractor()
        found = extractor.find_model_file(tmp_path)
        assert found == dat

    def test_prefers_sim_over_dat(self, tmp_path: Path) -> None:
        (tmp_path / "model.sim").touch()
        (tmp_path / "model.dat").touch()
        extractor = ResultExtractor()
        found = extractor.find_model_file(tmp_path)
        assert found.suffix == ".sim"

    def test_returns_none_when_no_model(self, tmp_path: Path) -> None:
        (tmp_path / "readme.txt").touch()
        extractor = ResultExtractor()
        found = extractor.find_model_file(tmp_path)
        assert found is None


class TestExtractHappyPath:
    """Test extract() with mocked converter."""

    def test_successful_extraction(
        self, tmp_path: Path, mock_diffraction_results
    ) -> None:
        sim = tmp_path / "output.sim"
        sim.touch()
        run_result = RunResult(
            status=RunStatus.COMPLETED,
            output_dir=tmp_path,
        )

        mock_converter = MagicMock()
        mock_converter.convert_to_unified_schema.return_value = mock_diffraction_results

        with patch(
            "digitalmodel.hydrodynamics.diffraction.result_extractor.OrcaWaveConverter",
            return_value=mock_converter,
        ):
            extractor = ResultExtractor(vessel_name="TestVessel", water_depth=100.0)
            result = extractor.extract(run_result)

        assert result.success is True
        assert result.results is not None
        assert result.model_file == sim


class TestExtractErrorCases:
    """Test extract() error handling."""

    def test_no_output_dir(self) -> None:
        run_result = RunResult(status=RunStatus.COMPLETED, output_dir=None)
        extractor = ResultExtractor()
        result = extractor.extract(run_result)
        assert result.success is False
        assert "output_dir" in result.error_message.lower()

    def test_no_model_file_found(self, tmp_path: Path) -> None:
        run_result = RunResult(status=RunStatus.COMPLETED, output_dir=tmp_path)
        extractor = ResultExtractor()
        result = extractor.extract(run_result)
        assert result.success is False
        assert "model file" in result.error_message.lower()

    def test_failed_run_result(self, tmp_path: Path) -> None:
        run_result = RunResult(
            status=RunStatus.FAILED,
            output_dir=tmp_path,
            error_message="Solver crashed",
        )
        extractor = ResultExtractor()
        result = extractor.extract(run_result)
        assert result.success is False

    def test_converter_exception(self, tmp_path: Path) -> None:
        sim = tmp_path / "model.sim"
        sim.touch()
        run_result = RunResult(status=RunStatus.COMPLETED, output_dir=tmp_path)

        with patch(
            "digitalmodel.hydrodynamics.diffraction.result_extractor.OrcaWaveConverter",
            side_effect=Exception("OrcFxAPI error"),
        ):
            extractor = ResultExtractor(water_depth=100.0)
            result = extractor.extract(run_result)

        assert result.success is False
        assert "OrcFxAPI error" in result.error_message
