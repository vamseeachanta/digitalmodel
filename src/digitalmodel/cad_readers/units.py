# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Conservative length-unit resolution shared by all readers."""

from .models import LengthUnit, UnitInfo, UnitsError

# NIST SP 811 (2008), Appendix B.8, inch and foot conversion entries;
# SI prefixes milli and centi. These are exact definitions, not tolerances.
# https://www.nist.gov/pml/special-publication-811/nist-guide-si-appendix-b-conversion-factors/nist-guide-si-appendix-b8
METRES_PER_UNIT: dict[LengthUnit, float] = {
    "m": 1.0,
    "mm": 0.001,
    "cm": 0.01,
    "in": 0.0254,
    "ft": 0.3048,
}


def validate_unit(unit: str | None) -> None:
    """Reject spelling errors before reading or invoking a converter."""
    if unit is not None and unit not in METRES_PER_UNIT:
        raise UnitsError("Unsupported units; use m, mm, cm, in or ft")


def resolve_units(
    declared: tuple[LengthUnit, ...],
    caller: LengthUnit | None,
    coordinate_unit: LengthUnit | None = None,
) -> UnitInfo:
    """A caller declaration fills a gap; it never silently overrides metadata."""
    validate_unit(caller)
    if not declared and caller is None:
        raise UnitsError("Units declaration required for a file without units")
    if declared and caller is not None and any(u != caller for u in declared):
        raise UnitsError("Caller units conflict with file units")
    unit = coordinate_unit or (declared[0] if declared else caller)
    assert unit is not None
    return UnitInfo(
        unit, METRES_PER_UNIT[unit], declared, "file" if declared else "caller"
    )
