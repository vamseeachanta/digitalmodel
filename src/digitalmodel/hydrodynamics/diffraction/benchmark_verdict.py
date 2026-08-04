"""Derive fail-closed benchmark statuses from comparison evidence."""

from __future__ import annotations

from dataclasses import dataclass
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
    correlation: Optional[float],
    quality: str,
    consensus: str,
) -> BenchmarkVerdict:
    """Derive status from provenance, statistic quality, and consensus.

    ``pass`` is deliberately allow-listed: it requires solver provenance,
    usable non-identical statistics, and full consensus. A caller cannot
    override the result with a hand-authored status.
    """
    matrix_list = list(matrices)
    refused_qualities = {"INSUFFICIENT_DATA", "INSUFFICIENT_SAMPLING"}

    if correlation is None or quality in refused_qualities:
        detail = quality if quality in refused_qualities else "correlation unavailable"
        return BenchmarkVerdict("incomplete", f"comparison refused: {detail}")
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
    if quality == "IDENTICAL":
        return BenchmarkVerdict("suspect", "comparison inputs are identical")
    if consensus == "NO_CONSENSUS":
        return BenchmarkVerdict("fail", "comparison consensus is NO_CONSENSUS")
    if quality == "COMPARED" and consensus == "FULL":
        return BenchmarkVerdict(
            "pass",
            "solver-sourced comparison has usable statistics and FULL consensus",
        )
    return BenchmarkVerdict(
        "incomplete",
        f"comparison cannot derive pass from quality={quality}, consensus={consensus}",
    )
