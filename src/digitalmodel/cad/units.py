# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Resolve declared units without inferring them from geometry extents."""

from .models import LengthUnit, UnitBasis, UnitDecision, UnitsError

# NIST SI Prefixes, prefix table, updated 2025-08-13: centi=10^-2, milli=10^-3.
# https://www.nist.gov/pml/owm/metric-si-prefixes
# NIST SP 811 (2008), Appendix B.8, foot and inch entries (exact factors).
# https://www.nist.gov/pml/special-publication-811/nist-guide-si-appendix-b-conversion-factors/nist-guide-si-appendix-b8
# The metre factor is the identity conversion, not a fitted coefficient.
METRES_PER_UNIT: dict[LengthUnit, float] = {
    "m": 1.0,
    "mm": 0.001,
    "cm": 0.01,
    "in": 0.0254,
    "ft": 0.3048,
}


def resolve_units(
    declared: LengthUnit | None, supplied: LengthUnit | None
) -> UnitDecision:
    """Require a supported declaration and refuse any caller/file conflict.

    Caller input fills a missing declaration; it never replaces a known one.
    An unsupported file declaration is an error handled by the format reader.
    """
    for value in (declared, supplied):
        if value is not None and (
            not isinstance(value, str) or value not in METRES_PER_UNIT
        ):
            raise UnitsError("Unsupported units; use m, mm, cm, in or ft")
    if declared is not None and supplied is not None and declared != supplied:
        raise UnitsError("File and caller units conflict")
    unit = declared if declared is not None else supplied
    if unit is None:
        raise UnitsError("State units explicitly when the file has no length units")
    basis: UnitBasis = "caller" if declared is None else "file"
    if declared is not None and supplied is not None:
        basis = "file_and_caller"
    return UnitDecision(unit, METRES_PER_UNIT[unit], basis)
