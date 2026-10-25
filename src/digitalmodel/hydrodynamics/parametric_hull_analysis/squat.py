# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Unrestricted-channel Römisch squat and an explicit clearance budget.

Method: PIANC On Course 142 (2011), Appendix A4, printed p. 22, eqs. (7)-(10),
reporting Römisch (1989). The original 1989 publication is not verified here.
https://pianc-jp.org/news/images/On%20Course%20142.pdf

Critical speed and the empirical h/T envelope require caller-supplied cited
values; there is no default geometry qualification or navigation criterion.
Speed is relative to water. Depth includes the applicable water level; draft
is static even-keel draft. Waves, heel, static trim and bathymetric uncertainty
are not modelled. Citation metadata resolution is not technical validation.
"""

from __future__ import annotations

from dataclasses import dataclass
from math import isfinite, sqrt
from pathlib import Path
from types import MappingProxyType
from typing import Iterable, Mapping

from digitalmodel.citations.registry import get_pianc_depth_band_reference
from digitalmodel.citations.schema import Citation, CitedValue, validate_citation

from .models import DepthClassification, classify_depth

ROMISCH_REFERENCE = Citation(
    code_id="PIANC-ROMISCH-SQUAT", publisher="PIANC", revision="2011",
    section="On Course 142, Appendix A4, p. 22, equations (7)-(10)",
    wiki_path="wikis/marine-engineering/wiki/standards/romisch-squat.md",
    source_sibling="generic",
    note="Römisch (1989) as reported in https://pianc-jp.org/news/images/On%20Course%20142.pdf",
)


@dataclass(frozen=True)
class SquatResult:
    """Metre-valued screening predictions with input and coefficient provenance."""

    bow_m: float
    stern_m: float
    draft_m: float
    water_depth_m: float
    speed_ms: float
    length_bp_m: float
    beam_m: float
    block_coefficient: float
    depth_class: DepthClassification
    coefficients: Mapping[str, CitedValue]
    critical_speed: CitedValue
    minimum_depth_ratio: CitedValue
    maximum_depth_ratio: CitedValue
    depth_reference: CitedValue

    @property
    def maximum_m(self) -> float:
        """Return the governing end displacement, not mean sinkage."""
        return max(self.bow_m, self.stern_m)

    @property
    def navigation_status(self) -> str:
        return "NOT_EVALUATED"


@dataclass(frozen=True)
class ClearanceResult:
    """Budget comparison only; a positive margin is not navigation approval."""

    squat: SquatResult
    static_m: float
    allowance_m: float
    residual_m: float
    minimum_clearance: CitedValue | None
    margin_m: float | None
    status: str

    @property
    def budget_exhausted(self) -> bool:
        """Nonpositive residual; includes allowances, not proof of contact."""
        return self.residual_m <= 0

    @property
    def navigation_status(self) -> str:
        return "NOT_EVALUATED"


def _finite(name: str, value: float, *, positive: bool = False) -> None:
    if isinstance(value, bool) or not isfinite(value):
        raise ValueError(f"{name} must be finite")
    if value < 0 or (positive and value == 0):
        raise ValueError(f"{name} must be {'positive' if positive else 'nonnegative'}")


def _cited(name: str, item: CitedValue, units: str, root: Path) -> float:
    if not isinstance(item, CitedValue):
        raise ValueError(f"{name} requires a CitedValue")
    if item.units != units:
        raise ValueError(f"{name} units must be {units}")
    _finite(name, item.value)
    validate_citation(item.citation, repo_root=root)
    return item.value


def romisch_squat(
    *, length_bp_m: float, beam_m: float, block_coefficient: float,
    draft_m: float, water_depth_m: float, speed_ms: float,
    critical_speed: CitedValue, minimum_depth_ratio: CitedValue,
    maximum_depth_ratio: CitedValue, citation_root: Path,
) -> SquatResult:
    """Evaluate eqs. (7)-(10) within an explicitly supplied h/T envelope.

    ``critical_speed`` is in m/s; both envelope bounds are dimensionless.
    Their citations must identify the basis applicable to this hull/channel.
    Bounds are inclusive; the physical guards require h>T and 0<=V<Vcr.
    These guards are not a claim that all subcritical geometries are validated.
    ``citation_root`` is explicit to avoid implicit discovery of private data.
    Missing method, depth-band or input records fail closed. No solver is used.
    """
    if citation_root is None:
        raise ValueError("citation_root must be explicit")
    for name, value in (("length_bp_m", length_bp_m), ("beam_m", beam_m),
                        ("draft_m", draft_m), ("water_depth_m", water_depth_m),
                        ("block_coefficient", block_coefficient)):
        _finite(name, value, positive=True)
    _finite("speed_ms", speed_ms)
    if block_coefficient > 1:
        raise ValueError("block_coefficient must not exceed unity")
    critical = _cited("critical_speed", critical_speed, "m/s", citation_root)
    lower = _cited("minimum_depth_ratio", minimum_depth_ratio, "dimensionless", citation_root)
    upper = _cited("maximum_depth_ratio", maximum_depth_ratio, "dimensionless", citation_root)
    if lower <= 1 or upper < lower:
        raise ValueError("depth-ratio envelope requires 1 < minimum <= maximum")
    if water_depth_m <= draft_m or not lower <= water_depth_m / draft_m <= upper:
        raise ValueError("h/T is grounded or outside the supplied applicability envelope")
    if critical <= 0 or speed_ms >= critical:
        raise ValueError("speed must be below a positive critical_speed")
    validate_citation(ROMISCH_REFERENCE, repo_root=citation_root)
    depth_reference = get_pianc_depth_band_reference(repo_root=citation_root)
    ratio = speed_ms / critical
    try:
        # PIANC On Course 142 (2011), App. A4, p. 22, eq. (8): all literals.
        cv = 8 * ratio**2 * ((ratio - 0.5)**4 + 0.0625)
        # Same source, eq. (9): factor 10, square, and stern identity 1.
        cf = (10 * (beam_m / length_bp_m) * block_coefficient)**2
        # Same source, eq. (10): coefficient 0.155 and square-root dependence.
        k_depth = 0.155 * sqrt(water_depth_m / draft_m)
        # Same source, eq. (7): end squat = Cv * Cf * K * T.
        stern = cv * k_depth * draft_m
        bow = stern * cf
    except OverflowError as exc:
        raise ValueError("squat calculation overflow") from exc
    for name, value in (("bow", bow), ("stern", stern), ("shape", cf)):
        _finite(name, value)
    coefficients = MappingProxyType({
        name: CitedValue(value, ROMISCH_REFERENCE, "dimensionless")
        for name, value in {"speed": cv, "bow_shape": cf,
                            "stern_shape": 1.0, "depth": k_depth}.items()
    })
    return SquatResult(
        bow, stern, draft_m, water_depth_m, speed_ms, length_bp_m, beam_m,
        block_coefficient, classify_depth(water_depth_m, draft_m), coefficients,
        critical_speed, minimum_depth_ratio, maximum_depth_ratio, depth_reference,
    )


def clearance_budget(
    squat: SquatResult, *, allowance_m: float,
    minimum_clearance: CitedValue | None, citation_root: Path,
) -> ClearanceResult:
    """Subtract squat and explicit nonnegative allowances from static UKC.

    Geometric accounting identity: residual = h - T - max(Sb, Ss) - allowance.
    No empirical coefficient is introduced. Allowance is a caller's aggregate
    downward deduction for effects not already represented by h, T and squat;
    its completeness is NOT_EVALUATED. A zero allowance must be explicit.
    ``minimum_clearance`` is a cited metre-valued criterion, or None when absent.
    Equality meets that supplied minimum. Negative residuals are preserved.
    """
    if citation_root is None:
        raise ValueError("citation_root must be explicit")
    _finite("allowance_m", allowance_m)
    # Re-evaluate provenance and geometry, including when results were stored
    # or constructed outside this module. Never accept stale citation metadata.
    checked = romisch_squat(
        length_bp_m=squat.length_bp_m, beam_m=squat.beam_m,
        block_coefficient=squat.block_coefficient, draft_m=squat.draft_m,
        water_depth_m=squat.water_depth_m, speed_ms=squat.speed_ms,
        critical_speed=squat.critical_speed,
        minimum_depth_ratio=squat.minimum_depth_ratio,
        maximum_depth_ratio=squat.maximum_depth_ratio, citation_root=citation_root,
    )
    if checked != squat:
        raise ValueError("squat result does not match its inputs and citations")
    static = squat.water_depth_m - squat.draft_m
    residual = static - squat.maximum_m - allowance_m
    if not isfinite(residual):
        raise ValueError("clearance calculation overflow")
    margin = None
    status = "NOT_EVALUATED"
    if minimum_clearance is not None:
        minimum = _cited("minimum_clearance", minimum_clearance, "m", citation_root)
        margin = residual - minimum
        if not isfinite(margin):
            raise ValueError("clearance margin overflow")
        status = "MEETS_INPUT_MINIMUM" if margin >= 0 else "BELOW_INPUT_MINIMUM"
    return ClearanceResult(squat, static, allowance_m, residual, minimum_clearance, margin, status)


def squat_speed_sweep(speeds_ms: Iterable[float], **inputs) -> list[SquatResult]:
    """Evaluate a finite supplied speed grid in order; any invalid row fails.

    No interpolation, optimisation or maximum permitted speed is inferred.
    Other keyword inputs are those of ``romisch_squat`` except ``speed_ms``.
    """
    if "speed_ms" in inputs:
        raise ValueError("supply speeds_ms, not a separate speed_ms")
    rows = [romisch_squat(speed_ms=speed, **inputs) for speed in speeds_ms]
    if not rows:
        raise ValueError("speeds_ms must not be empty")
    return rows
