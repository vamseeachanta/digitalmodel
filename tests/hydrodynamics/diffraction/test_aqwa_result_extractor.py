"""Tests for AQWAResultExtractor (WRK-027)."""
from __future__ import annotations

from dataclasses import fields as dataclass_fields
from pathlib import Path
from unittest.mock import MagicMock, patch

import pytest

from digitalmodel.hydrodynamics.diffraction.aqwa_runner import (
    AQWARunResult,
    AQWARunStatus,
)
from digitalmodel.hydrodynamics.diffraction.aqwa_result_extractor import (
    AQWAExtractionResult,
    AQWAResultExtractor,
)


# ---------------------------------------------------------------------------
# TestAQWAExtractionResult
# ---------------------------------------------------------------------------


class TestAQWAExtractionResult:
    """Test AQWAExtractionResult dataclass defaults and structure."""

    def test_defaults(self) -> None:
        er = AQWAExtractionResult(success=False)
        assert er.success is False
        assert er.results is None
        assert er.lis_file is None
        assert er.error_message is None

    def test_success_state(self, mock_diffraction_results) -> None:
        lis = Path("/tmp/analysis/output.LIS")
        er = AQWAExtractionResult(
            success=True,
            results=mock_diffraction_results,
            lis_file=lis,
        )
        assert er.success is True
        assert er.results is not None
        assert er.lis_file == lis

    def test_is_dataclass(self) -> None:
        field_names = {f.name for f in dataclass_fields(AQWAExtractionResult)}
        assert "success" in field_names
        assert "results" in field_names
        assert "lis_file" in field_names
        assert "error_message" in field_names


# ---------------------------------------------------------------------------
# TestAQWAResultExtractor
# ---------------------------------------------------------------------------


class TestAQWAResultExtractor:
    """Test AQWAResultExtractor.find_lis_file and extract methods."""

    # -- find_lis_file -------------------------------------------------------

    def test_find_lis_file_found(self, tmp_path: Path) -> None:
        lis = tmp_path / "results.LIS"
        lis.touch()
        extractor = AQWAResultExtractor(
            vessel_name="TestVessel", water_depth=100.0
        )
        found = extractor.find_lis_file(tmp_path)
        assert found == lis

    def test_find_lis_file_not_found(self, tmp_path: Path) -> None:
        (tmp_path / "readme.txt").touch()
        extractor = AQWAResultExtractor(
            vessel_name="TestVessel", water_depth=100.0
        )
        found = extractor.find_lis_file(tmp_path)
        assert found is None

    def test_find_lis_file_prefers_lis_extension(self, tmp_path: Path) -> None:
        (tmp_path / "alpha.LIS").touch()
        (tmp_path / "beta.lis").touch()
        extractor = AQWAResultExtractor(
            vessel_name="TestVessel", water_depth=100.0
        )
        found = extractor.find_lis_file(tmp_path)
        # Should return first sorted match from the first matching pattern
        assert found is not None
        assert found.suffix in (".LIS", ".lis")

    # -- extract error cases -------------------------------------------------

    def test_extract_failed_run_returns_error(self, tmp_path: Path) -> None:
        run_result = AQWARunResult(
            status=AQWARunStatus.FAILED,
            output_dir=tmp_path,
            error_message="Solver crashed",
        )
        extractor = AQWAResultExtractor(
            vessel_name="TestVessel", water_depth=100.0
        )
        result = extractor.extract(run_result)
        assert result.success is False
        assert result.error_message is not None

    def test_extract_no_output_dir(self) -> None:
        run_result = AQWARunResult(
            status=AQWARunStatus.COMPLETED, output_dir=None
        )
        extractor = AQWAResultExtractor(
            vessel_name="TestVessel", water_depth=100.0
        )
        result = extractor.extract(run_result)
        assert result.success is False
        assert "output_dir" in result.error_message.lower()

    def test_extract_no_lis_file(self, tmp_path: Path) -> None:
        (tmp_path / "readme.txt").touch()
        run_result = AQWARunResult(
            status=AQWARunStatus.COMPLETED, output_dir=tmp_path
        )
        extractor = AQWAResultExtractor(
            vessel_name="TestVessel", water_depth=100.0
        )
        result = extractor.extract(run_result)
        assert result.success is False
        assert "lis" in result.error_message.lower()

    # -- extract happy path --------------------------------------------------

    def test_extract_happy_path_mocked(
        self, tmp_path: Path, mock_diffraction_results
    ) -> None:
        lis = tmp_path / "output.LIS"
        lis.touch()
        run_result = AQWARunResult(
            status=AQWARunStatus.COMPLETED,
            output_dir=tmp_path,
        )

        mock_converter = MagicMock()
        mock_converter.convert_to_unified_schema.return_value = (
            mock_diffraction_results
        )

        with patch(
            "digitalmodel.hydrodynamics.diffraction.aqwa_result_extractor.AQWAConverter",
            return_value=mock_converter,
        ) as mock_cls:
            extractor = AQWAResultExtractor(
                vessel_name="TestVessel", water_depth=100.0
            )
            result = extractor.extract(run_result)

        mock_cls.assert_called_once_with(
            analysis_folder=tmp_path, vessel_name="TestVessel"
        )
        mock_converter.convert_to_unified_schema.assert_called_once_with(
            water_depth=100.0
        )
        assert result.success is True
        assert result.results is mock_diffraction_results
        assert result.lis_file == lis

    # -- extract converter exception -----------------------------------------

    def test_extract_converter_exception(self, tmp_path: Path) -> None:
        lis = tmp_path / "output.LIS"
        lis.touch()
        run_result = AQWARunResult(
            status=AQWARunStatus.COMPLETED, output_dir=tmp_path
        )

        with patch(
            "digitalmodel.hydrodynamics.diffraction.aqwa_result_extractor.AQWAConverter",
            side_effect=Exception("LIS parse error"),
        ):
            extractor = AQWAResultExtractor(
                vessel_name="TestVessel", water_depth=100.0
            )
            result = extractor.extract(run_result)

        assert result.success is False
        assert "LIS parse error" in result.error_message
