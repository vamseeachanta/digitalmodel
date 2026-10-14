# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Bounded CSR hull-wastage checks; no overall classification disposition.

Source: IACS CSR, NR606 mirror, footer 1 July 2025, Part 1 Chapter 13.
Lengths are metres for section geometry and millimetres for thicknesses.
Only horizontal and vertical flat plates are supported. Stiffeners, inclined
plates, effective breadth and automatic survey-zone selection remain gaps.
"""

from __future__ import annotations

import math
from dataclasses import dataclass, replace
from pathlib import Path

from digitalmodel.citations.schema import Citation, validate_citation
from digitalmodel.naval_architecture.hull_girder_screening import (
    SectionElement,
    section_properties,
)


@dataclass(frozen=True)
class SurveyScope:
    """Explicit applicability inputs; CSR Chapter 13 Sections 1 and 2."""

    classed_under_csr: bool
    in_operation: bool
    special_survey: bool
    age_years: float
    simple_section_confirmed: bool = False


@dataclass(frozen=True)
class HullPlate:
    """A contributing flat plate with approved-plan zone and gross thickness.

    CSR Chapter 13 Section 2, 1.2.1–1.2.3 and 2.2.2. Width denotes breadth
    for a horizontal plate and height for a vertical plate. Neutral-zone
    plates are vertical, so their area is already the vertical projection.
    Gross offered thickness is distinct from the local as-built thickness.
    """

    name: str
    zone: str
    orientation: str
    width_m: float
    centroid_m: float
    gross_mm: float
    measured_mm: float
    corrosion_addition_mm: float


@dataclass(frozen=True)
class WastageResult:
    """Component verdict, governing check, numeric evidence and citation sidecar."""

    verdict: str
    governing_check: str | None
    values: dict[str, float]
    utilizations: dict[str, float]
    applicability_flags: tuple[str, ...]
    citations: tuple[Citation, ...]


def _number(name: str, value: float, *, positive: bool = False) -> None:
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        raise ValueError(f"{name} must be a finite number")
    if not math.isfinite(value) or value < 0 or (positive and value == 0):
        raise ValueError(f"{name} must be finite and {'positive' if positive else 'nonnegative'}")


def _scope_flags(scope: SurveyScope, *, global_check: bool) -> tuple[str, ...]:
    _number("age_years", scope.age_years)
    for name in ("classed_under_csr", "in_operation", "special_survey",
                 "simple_section_confirmed"):
        if type(getattr(scope, name)) is not bool:
            raise ValueError(f"{name} must be boolean")
    flags = []
    if not scope.classed_under_csr:
        flags.append("not_classed_under_csr")
    if not scope.in_operation:
        flags.append("not_in_operation")
    if global_check and not scope.special_survey:
        flags.append("not_special_survey")
    if global_check and scope.age_years <= 10:
        flags.append("not_over_ten_years")
    if global_check and not scope.simple_section_confirmed:
        flags.append("section_outside_supported_idealization")
    return tuple(flags)


def _citations(citation: Citation, sections: tuple[str, ...],
               wiki_root: Path | None) -> tuple[Citation, ...]:
    if (citation.code_id, citation.publisher, citation.revision) != (
        "IACS-CSR", "IACS", "2025-07-01",
    ) or citation.source_sibling != "generic" or citation.source_project is not None:
        raise ValueError("A generic IACS-CSR citation for 2025-07-01 is required")
    sidecar = tuple(replace(citation, section=s) for s in sections)
    for item in sidecar:
        validate_citation(item, repo_root=wiki_root)
    return sidecar


def screen_local_corrosion(
    *, as_built_mm: float, measured_mm: float, corrosion_addition_mm: float,
    voluntary_addition_mm: float, scope: SurveyScope, citation: Citation,
    wiki_root: Path | None = None, reserve_mm: float = 0.5,
) -> WastageResult:
    """Compare a reading with renewal and reserve boundaries, in millimetres.

    CSR 2025 Chapter 13 Section 2 symbols, 2.1.1–2.1.3, pages 715–717.
    Renewal subtracts corrosion and voluntary additions from as-built
    thickness. Repair subtracts voluntary addition only. MONITOR_RESERVE
    identifies the alternative-solution band; it does not grant survey
    approval. ACCEPT_LOCAL does not establish global strength or class status.
    Reserve is an explicit input fixed to the opened rule value of 0.5.
    """
    for name, value in (("as_built_mm", as_built_mm), ("measured_mm", measured_mm),
                        ("corrosion_addition_mm", corrosion_addition_mm),
                        ("voluntary_addition_mm", voluntary_addition_mm),
                        ("reserve_mm", reserve_mm)):
        _number(name, value, positive=name == "as_built_mm")
    if reserve_mm != 0.5:
        raise ValueError("CSR Section 2 symbols fixes reserve_mm to 0.5")
    renewal = as_built_mm - corrosion_addition_mm - voluntary_addition_mm
    if renewal <= 0:
        raise ValueError("renewal thickness must be positive")
    citations = _citations(citation, (
        "Part 1 Chapter 13 Section 2 symbols",
        "Part 1 Chapter 13 Section 2 paragraphs 2.1.1 and 2.1.2",
        "Part 1 Chapter 13 Section 2 paragraph 2.1.3",
    ), wiki_root)
    flags = _scope_flags(scope, global_check=False)
    if flags:
        return WastageResult("NOT_APPLICABLE", None, {}, {}, flags, citations)
    verdict = "RENEW" if measured_mm < renewal else (
        "MONITOR_RESERVE" if measured_mm < renewal + reserve_mm else "ACCEPT_LOCAL"
    )
    values = dict(renewal_thickness_mm=renewal,
                  repair_thickness_mm=as_built_mm - voluntary_addition_mm,
                  reserve_upper_mm=renewal + reserve_mm,
                  measured_thickness_mm=measured_mm,
                  diminution_mm=as_built_mm - measured_mm,
                  renewal_margin_mm=measured_mm - renewal)
    return WastageResult(verdict, "renewal_margin_mm", values, {}, (), citations)


def _validate_plates(plates: tuple[HullPlate, ...], depth_m: float) -> None:
    _number("depth_m", depth_m, positive=True)
    if not plates or {p.zone for p in plates} != {"deck", "bottom", "neutral"}:
        raise ValueError("deck, bottom and neutral contributing plates are required")
    if len({p.name for p in plates}) != len(plates) or any(not p.name for p in plates):
        raise ValueError("plate names must be nonempty and unique")
    for p in plates:
        for name in ("width_m", "centroid_m", "gross_mm", "measured_mm",
                     "corrosion_addition_mm"):
            _number(name, getattr(p, name), positive=name in (
                "width_m", "gross_mm", "measured_mm"))
        if p.orientation not in ("horizontal", "vertical"):
            raise ValueError("only horizontal and vertical plates are supported")
        if p.centroid_m > depth_m or p.corrosion_addition_mm >= p.gross_mm:
            raise ValueError("centroid or corrosion addition is outside the section")
        if p.zone == "neutral" and p.orientation != "vertical":
            raise ValueError("neutral-zone plates must have a defined vertical projection")
        if p.orientation == "vertical" and (
            p.centroid_m - p.width_m / 2 < 0 or
            p.centroid_m + p.width_m / 2 > depth_m
        ):
            raise ValueError("vertical plate extends beyond the section depth")


def _properties(plates: tuple[HullPlate, ...], depth_m: float,
                thickness_field: str) -> dict[str, float]:
    elements = []
    for p in plates:
        factory = (SectionElement.horizontal if p.orientation == "horizontal"
                   else SectionElement.vertical)
        elements.append(factory(p.name, p.width_m,
                                getattr(p, thickness_field) / 1000, p.centroid_m))
    return section_properties(elements, depth_m=depth_m)


def _global_evidence(plates: tuple[HullPlate, ...], depth_m: float,
                     retained_fraction: float) -> tuple[dict[str, float], dict[str, float]]:
    gross = _properties(plates, depth_m, "gross_mm")
    current = _properties(plates, depth_m, "measured_mm")
    neutral = tuple(p for p in plates if p.zone == "neutral")
    target = sum(p.width_m * (p.gross_mm - 0.5 * p.corrosion_addition_mm)
                 / 1000 for p in neutral)
    area = sum(p.width_m * p.measured_mm / 1000 for p in neutral)
    values = dict(gross_sm_deck_m3=gross["sm_deck_m3"],
                  gross_sm_bottom_m3=gross["sm_keel_m3"],
                  measured_sm_deck_m3=current["sm_deck_m3"],
                  measured_sm_bottom_m3=current["sm_keel_m3"],
                  measured_neutral_axis_m=current["neutral_axis_m"],
                  neutral_target_area_m2=target, measured_neutral_area_m2=area)
    utilizations = dict(
        deck_modulus=retained_fraction * gross["sm_deck_m3"] / current["sm_deck_m3"],
        bottom_modulus=retained_fraction * gross["sm_keel_m3"] / current["sm_keel_m3"],
        neutral_area=target / area,
    )
    return values, utilizations


def screen_global_corrosion(
    plates: tuple[HullPlate, ...], *, depth_m: float, zones_confirmed: bool,
    scope: SurveyScope, citation: Citation, wiki_root: Path | None = None,
    retained_modulus_fraction: float = 0.9,
) -> WastageResult:
    """CSR gross-offered-reference checks through the existing section API.

    CSR 2025 Chapter 13 Section 2, 2.2.1–2.2.2(a,b), page 717.
    Required deck and bottom modulus is 0.9 times gross offered modulus.
    Neutral-zone target area uses gross offered thickness minus half the
    corrosion addition. Ratios are required/current; equality passes.
    The modulus route is used; the alternative flange-area route and the
    per-item shortcut are not implemented. Full contributing section and
    approved zone membership are caller responsibilities. The caller shall
    confirm the simple steel section, with no effective members above the
    strength deck requiring the equivalent-deck correction in Chapter 5
    Section 1, 1.4.3. PASS_GLOBAL
    does not establish local renewal, pitting or classification compliance.
    """
    _validate_plates(plates, depth_m)
    _number("retained_modulus_fraction", retained_modulus_fraction, positive=True)
    if retained_modulus_fraction != 0.9:
        raise ValueError("CSR 2.2.2(a) fixes retained_modulus_fraction to 0.9")
    if type(zones_confirmed) is not bool:
        raise ValueError("zones_confirmed must be boolean")
    citations = _citations(citation, (
        "Part 1 Chapter 13 Section 2 paragraphs 1.2.1–1.2.3 and 2.2.1",
        "Part 1 Chapter 13 Section 2 paragraph 2.2.2(a)",
        "Part 1 Chapter 13 Section 2 paragraph 2.2.2(b)",
        "Part 1 Chapter 5 Section 1 paragraphs 1.4.2–1.5.1; section_properties API",
    ), wiki_root)
    flags = _scope_flags(scope, global_check=True)
    if not zones_confirmed:
        flags += ("zone_membership_unconfirmed",)
    if flags:
        return WastageResult("NOT_APPLICABLE", None, {}, {}, flags, citations)
    values, utilizations = _global_evidence(plates, depth_m, retained_modulus_fraction)
    governing = max(utilizations, key=lambda key: utilizations[key])
    verdict = "PASS_GLOBAL" if utilizations[governing] <= 1 else "RENEW"
    return WastageResult(verdict, governing, values, utilizations, (), citations)
