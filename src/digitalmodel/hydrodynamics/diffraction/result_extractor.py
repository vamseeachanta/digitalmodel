"""Bridge RunResult -> DiffractionResults via OrcaWaveConverter (WRK-030).

Finds model output files (``.sim`` / ``.dat``) in the run output directory
and delegates to ``OrcaWaveConverter`` for result extraction.
"""
from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import Any

from digitalmodel.hydrodynamics.diffraction.orcawave_runner import RunResult, RunStatus
from digitalmodel.hydrodynamics.diffraction.output_schemas import DiffractionResults

try:
    from digitalmodel.hydrodynamics.diffraction.orcawave_converter import (
        OrcaWaveConverter,
    )

    CONVERTER_AVAILABLE = True
except ImportError:  # pragma: no cover
    OrcaWaveConverter = None  # type: ignore[assignment,misc]
    CONVERTER_AVAILABLE = False

MODEL_FILE_PATTERNS = ["*.sim", "*.dat"]


@dataclass
class ExtractionResult:
    """Result of bridging a RunResult to DiffractionResults."""

    success: bool
    results: DiffractionResults | None = None
    model_file: Path | None = None
    error_message: str | None = None


class ResultExtractor:
    """Extract DiffractionResults from an OrcaWave RunResult."""

    def __init__(
        self,
        vessel_name: str | None = None,
        water_depth: float | None = None,
    ) -> None:
        self._vessel_name = vessel_name
        self._water_depth = water_depth

    def extract(self, run_result: RunResult) -> ExtractionResult:
        """Convert a RunResult into DiffractionResults.

        Returns an ExtractionResult with success=False and a descriptive
        error_message if any step fails.
        """
        if run_result.status == RunStatus.FAILED:
            return ExtractionResult(
                success=False,
                error_message=f"Run failed: {run_result.error_message or 'unknown'}",
            )

        if run_result.output_dir is None:
            return ExtractionResult(
                success=False,
                error_message="RunResult has no output_dir set",
            )

        model_file = self.find_model_file(run_result.output_dir)
        if model_file is None:
            return ExtractionResult(
                success=False,
                error_message=f"No model file found in {run_result.output_dir}",
            )

        water_depth = self._extract_water_depth(run_result)

        try:
            converter = OrcaWaveConverter(
                model_file=model_file,
                vessel_name=self._vessel_name,
            )
            results = converter.convert_to_unified_schema(water_depth=water_depth)
        except Exception as exc:
            return ExtractionResult(
                success=False,
                model_file=model_file,
                error_message=str(exc),
            )

        return ExtractionResult(
            success=True,
            results=results,
            model_file=model_file,
        )

    def find_model_file(self, output_dir: Path) -> Path | None:
        """Locate the primary model output file in *output_dir*.

        Prefers ``.sim`` over ``.dat``.
        """
        for pattern in MODEL_FILE_PATTERNS:
            matches = sorted(output_dir.glob(pattern))
            if matches:
                return matches[0]
        return None

    def _extract_water_depth(self, run_result: RunResult) -> float:
        """Determine water depth from constructor arg or a sensible default."""
        if self._water_depth is not None:
            return self._water_depth
        return 0.0
