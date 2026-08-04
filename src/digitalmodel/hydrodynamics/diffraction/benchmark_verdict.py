"""Derive fail-closed benchmark statuses from comparison evidence."""

from __future__ import annotations

from dataclasses import dataclass
import math
from numbers import Real
from typing import Iterable, Literal, Optional

from digitalmodel.hydrodynamics.diffraction.output_schemas import (
    HydrodynamicMatrix,
)


@dataclass(frozen=True)
class BenchmarkVerdict:
    """A derived benchmark status and its operator-readable reason."""

    status: Literal["pass", "fail", "suspect", "incomplete"]
    reason: str


def derive_status(
    *,
    matrices: Iterable[HydrodynamicMatrix],
    correlation: Optional[float] | Iterable[Optional[float]],
    quality: str | Iterable[str],
    consensus: str,
) -> BenchmarkVerdict:
    """Derive status from provenance, statistic quality, and consensus.

    ``pass`` is deliberately allow-listed: it requires solver provenance,
    usable non-identical statistics, and full consensus. A caller cannot
    override the result with a hand-authored status.
    """
    matrix_list = tuple(matrices)
    correlations = _as_correlations(correlation)
    qualities = _as_qualities(quality)
    refusal = _evidence_refusal(correlations, qualities)
    if refusal is not None:
        return refusal
    if not matrix_list:
        return BenchmarkVerdict("incomplete", "comparison has no matrix inputs")

    unproven_sources = sorted(
        {matrix.source for matrix in matrix_list if matrix.source != "solver"}
    )
    if unproven_sources:
        return BenchmarkVerdict(
            "suspect",
            f"matrix provenance must be solver; found {', '.join(unproven_sources)}",
        )
    if len(matrix_list) < 2:
        return BenchmarkVerdict(
            "incomplete", "comparison requires at least two matrix inputs"
        )
    if "IDENTICAL" in qualities:
        return BenchmarkVerdict("suspect", "comparison inputs are identical")
    if consensus == "NO_CONSENSUS":
        return BenchmarkVerdict("fail", "comparison consensus is NO_CONSENSUS")
    usable_qualities = {"COMPARED", "NULL_RESPONSE"}
    if all(item in usable_qualities for item in qualities) and consensus == "FULL":
        return BenchmarkVerdict(
            "pass",
            "solver-sourced comparison has usable statistics and FULL consensus",
        )
    return BenchmarkVerdict(
        "incomplete",
        f"comparison cannot derive pass from quality={qualities}, consensus={consensus}",
    )


def _as_correlations(
    value: Optional[float] | Iterable[Optional[float]],
) -> tuple[Optional[float], ...]:
    if value is None or isinstance(value, Real):
        return (value,)
    return tuple(value)


def _as_qualities(value: str | Iterable[str]) -> tuple[str, ...]:
    return (value,) if isinstance(value, str) else tuple(value)


def _evidence_refusal(
    correlations: tuple[Optional[float], ...],
    qualities: tuple[str, ...],
) -> Optional[BenchmarkVerdict]:
    refused = {"INSUFFICIENT_DATA", "INSUFFICIENT_SAMPLING"}
    refused_quality = next((item for item in qualities if item in refused), None)
    if refused_quality is not None:
        return BenchmarkVerdict(
            "incomplete", f"comparison refused: {refused_quality}"
        )
    if any(item is None for item in correlations):
        return BenchmarkVerdict(
            "incomplete", "comparison refused: correlation unavailable"
        )
    invalid = _invalid_correlation_reason(correlations)
    if invalid is not None:
        return BenchmarkVerdict("incomplete", f"comparison refused: {invalid}")
    if not correlations or len(correlations) != len(qualities):
        return BenchmarkVerdict(
            "incomplete", "comparison evidence is empty or misaligned"
        )
    return None


def _invalid_correlation_reason(
    correlations: tuple[Optional[float], ...],
) -> Optional[str]:
    numeric = [item for item in correlations if item is not None]
    if any(not isinstance(item, Real) for item in numeric):
        return "correlation must be numeric"
    if any(not math.isfinite(float(item)) for item in numeric):
        return "correlation must be finite"
    if any(float(item) < -1.0 or float(item) > 1.0 for item in numeric):
        return "correlation must be within [-1, 1]"
    return None
