# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Bounded piping geometry checks, API 579-1/ASME FFS-1 June 2016 Part 8.

Table 8.4 supplies individual purchased-pipe ovality and angular weld limits.
PASS applies only to the assessed geometric feature, not the entire component
or continued operation. Other weld dimensions and original design requirements
require separate verification. Units: millimetres, degrees, dimensionless ratios.
"""

from dataclasses import dataclass
import math
from pathlib import Path

from digitalmodel.citations.schema import Citation, validate_citation


TABLE_CITATION = Citation(
    code_id="api-std-579-asme-ffs-1",
    publisher="API + ASME (joint)",
    revision="2016",
    section="Part 8, clauses 8.2.4, 8.2.5.1, 8.4.1, 8.4.2; Table 8.4",
    wiki_path="wikis/engineering-standards/wiki/standards/api-std-579.md",
    source_sibling="generic",
    note="June 2016; individual piping fabrication criteria, printed page 8-24",
)
# API 579-1/ASME FFS-1 June 2016 Table 8.4; citation emitted with each result.
WELDED_PIPE_OVALITY_LIMIT = 0.01
THIN_PIPE_OVALITY_LIMIT = 0.015
THIN_WALL_RATIO_LIMIT = 0.03
ANGULAR_LIMIT_DEGREES = 3.0
_WELDED_PURCHASE_STANDARDS = frozenset(
    {"ASTM A358", "ASTM A671", "ASTM A672", "ASTM A691"}
)


@dataclass(frozen=True)
class PipingContext:
    """Inspector-confirmed scope per 2016 clauses 8.2 and 8.4.1(a).

    Confirmations require original design, operating history and material
    evidence; these functions do not establish cyclic or creep eligibility.
    No confirmation defaults to true. Local distortions and coexisting damage
    are excluded from this bounded implementation.
    """

    construction_code: str
    component_type: str
    original_tolerance_confirmed: bool
    noncyclic_service_confirmed: bool
    outside_creep_range_confirmed: bool
    original_fatigue_analysis: bool
    other_damage_present: bool
    local_shell_distortion_present: bool


@dataclass(frozen=True)
class GeometryScreenResult:
    """Feature-only result with a fail-closed edition-specific citation sidecar."""

    governing_value: float
    limit: float | None
    units: str
    utilisation: float | None
    verdict: str
    applicable: bool
    applicability_flags: tuple[str, ...]
    citations: tuple[Citation, ...]


def _check_context(context: PipingContext) -> tuple[str, ...]:
    """Apply the bounded scope of 2016 clauses 8.2 and 8.4.1(a)."""
    if not isinstance(context, PipingContext):
        raise ValueError("context must be a PipingContext")
    flags = []
    if context.construction_code != "ASME B31.3":
        flags.append("construction code is outside the Table 8.4 screen")
    if context.component_type != "straight_pipe":
        flags.append("component geometry is outside the straight-pipe screen")
    expected = {
        "original_tolerance_confirmed": True,
        "noncyclic_service_confirmed": True,
        "outside_creep_range_confirmed": True,
        "original_fatigue_analysis": False,
        "other_damage_present": False,
        "local_shell_distortion_present": False,
    }
    for name, required in expected.items():
        value = getattr(context, name)
        if type(value) is not bool:
            raise ValueError(f"{name} must be a boolean confirmation")
        if value != required:
            flags.append(f"{name} is outside the bounded screen")
    return tuple(flags)


def _finite(value: float, name: str, *, positive: bool = True) -> None:
    """Reject nonphysical values before evaluating a geometric criterion."""
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        raise ValueError(f"{name} must be a finite number")
    if not math.isfinite(value) or value < 0 or (positive and value == 0):
        raise ValueError(f"{name} must be finite and {'positive' if positive else 'nonnegative'}")


def _result(
    value: float, limit: float | None, units: str, flags: tuple[str, ...],
    citation_root: Path | None,
) -> GeometryScreenResult:
    """Compare an individual Table 8.4 value after citation validation."""
    validate_citation(TABLE_CITATION, repo_root=citation_root)
    applicable = not flags
    utilisation = value / limit if applicable and limit is not None else None
    if utilisation is not None and not math.isfinite(utilisation):
        raise ValueError("utilisation must remain finite")
    verdict = "NOT_APPLICABLE"
    if applicable and limit is not None:
        verdict = "PASS" if value <= limit else "NEEDS_ASSESSMENT"
    return GeometryScreenResult(
        governing_value=value, limit=limit, units=units, utilisation=utilisation,
        verdict=verdict, applicable=applicable, applicability_flags=flags,
        citations=(TABLE_CITATION,),
    )


def assess_piping_ovality(
    *, specified_od_mm: float, minimum_od_mm: float, maximum_od_mm: float,
    thickness_mm: float, purchase_standard: str, context: PipingContext,
    citation_root: Path | None = None,
) -> GeometryScreenResult:
    """Compare extreme outside diameters with 2016 Table 8.4 limits.

    The governing ratio is (maximum minus minimum) / specified outside
    diameter. ASTM A530 is supported only within the table's thin-wall range;
    the four listed welded-pipe specifications use their individual limit.
    Purchase standard and original construction tolerance require records.
    Fittings, elbows, centreline offsets and full component approval are excluded.
    """
    for name, value in (
        ("specified_od_mm", specified_od_mm), ("minimum_od_mm", minimum_od_mm),
        ("maximum_od_mm", maximum_od_mm), ("thickness_mm", thickness_mm),
    ):
        _finite(value, name)
    if maximum_od_mm < minimum_od_mm:
        raise ValueError("maximum outside diameter is less than minimum")
    if 2 * thickness_mm >= minimum_od_mm:
        raise ValueError("wall thickness leaves no internal pipe diameter")
    flags = _check_context(context)
    limit = None
    if purchase_standard in _WELDED_PURCHASE_STANDARDS:
        limit = WELDED_PIPE_OVALITY_LIMIT
    elif purchase_standard == "ASTM A530":
        limit = THIN_PIPE_OVALITY_LIMIT
        if thickness_mm / specified_od_mm > THIN_WALL_RATIO_LIMIT:
            flags += ("ASTM A530 thin-wall range is exceeded",)
    else:
        flags += ("purchase specification requires a separate verified tolerance",)
    ratio = (maximum_od_mm - minimum_od_mm) / specified_od_mm
    if not math.isfinite(ratio):
        raise ValueError("diameter ratio must remain finite")
    return _result(ratio, limit, "dimensionless", flags, citation_root)


def assess_angular_misalignment(
    *, angle_degrees: float, context: PipingContext,
    citation_root: Path | None = None,
) -> GeometryScreenResult:
    """Compare weld angular magnitude with 2016 Table 8.4, B31.3 row.

    Measured angle is a nonnegative magnitude in degrees. Passing this
    criterion does not evaluate centreline offset, ovality or other weld flaws.
    """
    _finite(angle_degrees, "angle_degrees", positive=False)
    flags = _check_context(context)
    return _result(angle_degrees, ANGULAR_LIMIT_DEGREES, "degrees", flags, citation_root)
