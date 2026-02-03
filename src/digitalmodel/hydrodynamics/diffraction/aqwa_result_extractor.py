"""Bridge AQWARunResult -> DiffractionResults via AQWAConverter (WRK-028).

Finds AQWA ``.LIS`` output files in the run output directory and delegates
to ``AQWAConverter`` for result extraction.
"""
from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path

from digitalmodel.hydrodynamics.diffraction.aqwa_converter import AQWAConverter
from digitalmodel.hydrodynamics.diffraction.aqwa_runner import (
    AQWARunResult,
    AQWARunStatus,
)
from digitalmodel.hydrodynamics.diffraction.output_schemas import DiffractionResults

LIS_FILE_PATTERNS = ["*.LIS", "*.lis"]


@dataclass
class AQWAExtractionResult:
    """Result of bridging an AQWARunResult to DiffractionResults."""

    success: bool
    results: DiffractionResults | None = None
    lis_file: Path | None = None
    error_message: str | None = None


class AQWAResultExtractor:
    """Extract DiffractionResults from an AQWA AQWARunResult."""

    def __init__(
        self,
        vessel_name: str | None = None,
        water_depth: float | None = None,
    ) -> None:
        self._vessel_name = vessel_name
        self._water_depth = water_depth

    def extract(self, run_result: AQWARunResult) -> AQWAExtractionResult:
        """Convert an AQWARunResult into DiffractionResults.

        Returns an AQWAExtractionResult with success=False and a descriptive
        error_message if any step fails.
        """
        if run_result.status == AQWARunStatus.FAILED:
            return AQWAExtractionResult(
                success=False,
                error_message=f"Run failed: {run_result.error_message or 'unknown'}",
            )

        if run_result.output_dir is None:
            return AQWAExtractionResult(
                success=False,
                error_message="AQWARunResult has no output_dir set",
            )

        lis_file = self.find_lis_file(run_result.output_dir)
        if lis_file is None:
            return AQWAExtractionResult(
                success=False,
                error_message=f"No .LIS file found in {run_result.output_dir}",
            )

        water_depth = self._extract_water_depth(run_result)
        vessel_name = self._vessel_name or run_result.spec_name or "AQWA_Vessel"

        try:
            converter = AQWAConverter(
                analysis_folder=run_result.output_dir,
                vessel_name=vessel_name,
            )
            results = converter.convert_to_unified_schema(water_depth=water_depth)
        except Exception as exc:
            return AQWAExtractionResult(
                success=False,
                lis_file=lis_file,
                error_message=str(exc),
            )

        return AQWAExtractionResult(
            success=True,
            results=results,
            lis_file=lis_file,
        )

    def find_lis_file(self, output_dir: Path) -> Path | None:
        """Locate the primary .LIS output file in *output_dir*.

        Searches for both upper- and lower-case extensions.
        """
        for pattern in LIS_FILE_PATTERNS:
            matches = sorted(output_dir.glob(pattern))
            if matches:
                return matches[0]
        return None

    def _extract_water_depth(self, run_result: AQWARunResult) -> float:
        """Determine water depth from constructor arg or a sensible default."""
        if self._water_depth is not None:
            return self._water_depth
        return 0.0
