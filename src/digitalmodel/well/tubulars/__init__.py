# ABOUTME: Tubular design sub-package — design envelopes, connection ratings, wear
# ABOUTME: Public API for casing/tubing string design checks (WRK-376)

"""Tubular design tools — triaxial stress envelope, VME checks, API limits."""

from digitalmodel.well.tubulars.design_envelope import (
    AnisotropicVmeEnvelope,
    ApiEllipseEnvelope,
    IsotropicVmeEnvelope,
    TubularGeometry,
    compute_hoop_stress,
    compute_vme_stress,
    design_envelope_points,
)
from digitalmodel.well.tubulars.casing import (
    API_5CT_GRADES,
    CASING_SIZES,
    Casing,
    CasingGrade,
    casing,
    casing_grade,
    collapse_pressure,
    internal_yield_pressure,
    list_sizes,
    pipe_body_yield_strength,
    wall_for_size,
)
from digitalmodel.well.tubulars.sucker_rod import (
    API_11B_ROD_GRADES,
    ROD_SIZES_IN,
    RodGrade,
    RodTaperSection,
    SuckerRod,
    modified_goodman_allowable,
    rod_area,
    rod_grade,
    rod_string_goodman,
    sucker_rod,
)

__all__ = [
    "AnisotropicVmeEnvelope",
    "ApiEllipseEnvelope",
    "IsotropicVmeEnvelope",
    "TubularGeometry",
    "compute_hoop_stress",
    "compute_vme_stress",
    "design_envelope_points",
    # casing (API 5CT / API 5C3)
    "API_5CT_GRADES",
    "CASING_SIZES",
    "Casing",
    "CasingGrade",
    "casing",
    "casing_grade",
    "collapse_pressure",
    "internal_yield_pressure",
    "list_sizes",
    "pipe_body_yield_strength",
    "wall_for_size",
    # sucker rod (API 11B / modified Goodman)
    "API_11B_ROD_GRADES",
    "ROD_SIZES_IN",
    "RodGrade",
    "RodTaperSection",
    "SuckerRod",
    "modified_goodman_allowable",
    "rod_area",
    "rod_grade",
    "rod_string_goodman",
    "sucker_rod",
]
