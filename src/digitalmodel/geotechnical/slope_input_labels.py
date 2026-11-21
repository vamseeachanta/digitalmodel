"""Slope input-template labels, independent of Excel and the optional solver.

Sources: tests/fixtures/geotechnical/slope/xslope_simple1.xlsx and
tests/fixtures/geotechnical/slope/xslope_clay_blanket.xlsx.
All inputs here are dimensionless IDs or text. The workbooks declare Imperial
and SI respectively; these formulas do not operate on dimensional quantities.
"""

from __future__ import annotations

from dataclasses import dataclass


@dataclass(frozen=True)
class MaterialLabel:
    """One lookup row: dimensionless ID and optional name (blank stays None)."""

    material_id_dimensionless: int
    name_text: str | None


@dataclass(frozen=True)
class MaterialLabelInput:
    """Dimensionless material ID selected by a profile line."""

    material_id_dimensionless: int


@dataclass(frozen=True)
class PolygonLabelInput:
    """Dimensionless material ID and optional region-type text."""

    material_id_dimensionless: int
    region_type_text: str | None


def material_name(
    selection: MaterialLabelInput, materials: tuple[MaterialLabel, ...],
) -> str:
    """Return the first exact XLOOKUP name followed by Excel text coercion.

    profile row 6 in both slope workbooks. XLOOKUP's not-found argument is
    empty text. A matched blank return cell concatenated with empty text yields
    empty text, following documented Excel coercion; no cached value in the
    source covers this case. No engineering standard or clause is cited.
    """
    for material in materials:
        if material.material_id_dimensionless == selection.material_id_dimensionless:
            return "" if material.name_text is None else material.name_text
    return ""


def polygon_material_name(
    polygon: PolygonLabelInput, materials: tuple[MaterialLabel, ...],
) -> str:
    """Return a material label only for a blank or material polygon type.

    polygon row 7 in both slope workbooks: IF/OR gates the same exact lookup
    used by profile row 6. Excel text comparisons ignore letter case.
    """
    if (polygon.region_type_text or "").casefold() in ("", "material"):
        return material_name(MaterialLabelInput(polygon.material_id_dimensionless), materials)
    return ""


@dataclass(frozen=True)
class ReinforcementRule:
    """A text-only row in the reinforcement direction/application table."""

    reinforcement_type_text: str
    direction_text: str | None
    application_text: str | None


@dataclass(frozen=True)
class ReinforcementInput:
    """Reinforcement type text; None denotes an unused input row."""

    reinforcement_type_text: str | None


def reinforcement_properties(
    reinforcement: ReinforcementInput, rules: tuple[ReinforcementRule, ...],
) -> tuple[str | int, str | int]:
    """Return direction and application with IF/IFERROR exact-lookup behavior.

    reinforce H3:I22 in both slope workbooks, using Z8:AB11 in the simple case
    and AB8:AD11 in the clay case. An unused or unmatched selector returns
    empty text. A matched blank return cell gives Excel numeric zero. Inputs
    are literal reinforcement type names from the workbook table.
    """
    selected = reinforcement.reinforcement_type_text
    if selected is None or selected == "":
        return "", ""
    for rule in rules:
        if selected.casefold() == rule.reinforcement_type_text.casefold():
            return (
                0 if rule.direction_text is None else rule.direction_text,
                0 if rule.application_text is None else rule.application_text,
            )
    return "", ""


@dataclass(frozen=True)
class WaterLoadInput:
    """Water-load mode text; None denotes a blank cell (no physical unit).

    Source: main!D23 in the simple case, main!D24 in the clay case.
    """

    mode_text: str | None


def distributed_load_guidance(water_load: WaterLoadInput) -> str:
    """Return the dloads and dloads (2) B2 instruction for automatic water loads.

    This is entry guidance only, not a water-load or seepage calculation.
    Both slope workbooks use the same case-insensitive IF comparison.
    """
    if (water_load.mode_text or "").casefold() == "auto":
        return (
            "Enter non-water loads only — water loads derive automatically from "
            "the piezometric line / seepage boundary conditions."
        )
    return ""
