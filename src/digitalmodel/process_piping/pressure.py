"""Straight metallic process pipe: internal pressure and hydrostatic screening.

ASME B31.3-2024 304.1.2(a) eq. (3a), 304.1.1 and 345.4.2 eq. (24).
Public equation corroboration: LANL VAR-2010-001 section 3.3 (B31.3-2008):
https://engstandards.lanl.gov/esm/pressure_safety/var_2010_001.pdf
USBR public test-pressure reference (historical B31.3-2004 excerpt):
https://www.usbr.gov/lc/yuma/facilities/ydp/appendixpart1.pdf
No material allowables, quality factors or temperature tables are supplied.
"""

from dataclasses import dataclass

from digitalmodel.citations.schema import Citation
from ._basis import citations, positive, nonnegative


@dataclass(frozen=True)
class PressureDesign:
    """Required design inputs in Pa, m and dimensionless factors.

    E (quality_factor) is the applicable longitudinal weld/casting quality
    factor, sometimes named Ej; it is NOT an additional multiplier on Ej.
    S: Table A-1/A-1C; E: 302.3.3/302.3.4; W: 302.3.5; Y: Table 304.1.1-1.
    Values and mill undertolerance require the governing material/product
    specification. No default engineering values are silently selected.
    """

    pressure_pa: float
    outside_diameter_m: float
    allowable_stress_pa: float
    quality_factor: float
    weld_strength_factor: float
    y: float
    corrosion_allowance_m: float
    mechanical_allowance_m: float
    mill_tolerance_fraction: float


@dataclass(frozen=True)
class ThicknessResult:
    pressure_thickness_m: float
    minimum_finished_thickness_m: float
    minimum_nominal_thickness_m: float
    citations: tuple[Citation, ...]


def pressure_thickness(design: PressureDesign) -> ThicknessResult:
    """t=PD/[2(SEW+PY)]; tm=t+c; ordered min=tm/(1-mill_fraction).

    ASME B31.3-2024 304.1.2(a) eq. (3a), 304.1.1. The 304.1.2(b)
    special-consideration boundary is P/(SE)>0.385 or t>=D/6.
    This function rejects special-consideration cases and, as a conservative
    project rule, an ordered wall >=D/6 (the code limit applies to t).
    Positive internal gauge pressure only; no branches, bending, external
    pressure, sustained loads, cyclic service or high-pressure Chapter IX.
    """
    refs = citations("pressure")
    positive(
        pressure_pa=design.pressure_pa,
        outside_diameter_m=design.outside_diameter_m,
        allowable_stress_pa=design.allowable_stress_pa,
        quality_factor=design.quality_factor,
        weld_strength_factor=design.weld_strength_factor,
    )
    nonnegative(
        y=design.y,
        corrosion_allowance_m=design.corrosion_allowance_m,
        mechanical_allowance_m=design.mechanical_allowance_m,
        mill_tolerance_fraction=design.mill_tolerance_fraction,
    )
    if (
        design.quality_factor > 1
        or design.weld_strength_factor > 1
        or design.y > 1
        or design.mill_tolerance_fraction >= 1
    ):
        raise ValueError("E and W must be <=1, Y<=1, and mill tolerance <1")
    p, d, s = design.pressure_pa, design.outside_diameter_m, design.allowable_stress_pa
    if p / (s * design.quality_factor) > 0.385:
        raise ValueError(
            "B31.3 304.1.2(b) special high-pressure consideration required"
        )
    t = (
        p
        * d
        / (2 * (s * design.quality_factor * design.weld_strength_factor + p * design.y))
    )
    finished = t + design.corrosion_allowance_m + design.mechanical_allowance_m
    nominal = finished / (1 - design.mill_tolerance_fraction)
    if nominal >= d / 6:
        raise ValueError(
            "ordered wall >= D/6: outside this screen (project rule; "
            "B31.3 304.1.2 applies D/6 to t)")
    return ThicknessResult(t, finished, nominal, refs)


@dataclass(frozen=True)
class HydrotestResult:
    required_pressure_pa: float
    yield_pressure_limit_pa: float
    target_pressure_pa: float
    reduced_for_yield: bool
    component_limit_satisfied: bool
    citations: tuple[Citation, ...]


def hydrotest_pressure(
    *,
    design_pressure_pa: float,
    design_allowable_pa: float,
    test_allowable_pa: float,
    design_temperature_k: float,
    test_temperature_k: float,
    outside_diameter_m: float,
    minimum_test_wall_m: float,
    test_yield_pa: float,
    other_longitudinal_stress_pa: float,
    component_test_limit_pa: float,
) -> HydrotestResult:
    """Pointwise hydrostatic target, Pa gauge; B31.3-2024 345.4.2 eq. (24).

    Required pressure=max(1.5P,1.5P*St/S) when Td>Tt, otherwise 1.5P.
    For dissimilar materials supply the governing minimum St/S ratio via
    the corresponding S and St. The 6.5 ratio cap of B31.3-2012 345.4.2 is
    NOT applied; its removal in the 2024 edition is ASSUMED, to be confirmed
    against B31.3-2024 345.4.2 (without the cap the target is not lower). 345.2.1(a) permits reduction to avoid test yielding.

    An elastic cylinder screen bounds hoop stress by the inner-surface
    Lame value P*(ro²+ri²)/(ro²-ri²), and longitudinal stress by
    P*ri²/(ro²-ri²)+abs(other longitudinal stress). Both are limited to
    test yield. These are an explicit elastic screen, not a full piping
    stress analysis or the code's nominal-pressure-stress calculation.
    Lame/closed-end equilibrium: Portland State University, Strength of
    Materials and Failure Theories (2014), thick-walled pressure vessels,
    https://web.cecs.pdx.edu/~far/me437/Fall%202014/Strength/Strength-PE-Hormoz.pdf

    Supply minimum available wall AT TEST (including mill tolerance and any
    existing loss), not the future corroded wall unless that is the test
    condition. Component test limit includes flange/valve/gasket limits at
    test temperature. A component limit below target is INFEASIBLE, never
    automatically justified by the yield-reduction provision. Elevation
    head, supports, test-fluid weight and brittle fracture require separate
    checks. No leak inspection, hold-time or test certificate is produced.
    """
    refs = citations("hydro")
    positive(
        design_pressure_pa=design_pressure_pa,
        design_allowable_pa=design_allowable_pa,
        test_allowable_pa=test_allowable_pa,
        design_temperature_k=design_temperature_k,
        test_temperature_k=test_temperature_k,
        outside_diameter_m=outside_diameter_m,
        minimum_test_wall_m=minimum_test_wall_m,
        test_yield_pa=test_yield_pa,
        component_test_limit_pa=component_test_limit_pa,
    )
    if isinstance(other_longitudinal_stress_pa, bool) or not isinstance(
        other_longitudinal_stress_pa, (float, int)
    ):
        raise ValueError("other longitudinal stress must be finite")
    nonnegative(longitudinal_magnitude=abs(other_longitudinal_stress_pa))
    if minimum_test_wall_m >= outside_diameter_m / 6:
        raise ValueError("hydro screen requires wall < D/6")
    remaining = test_yield_pa - abs(other_longitudinal_stress_pa)
    if remaining <= 0:
        raise ValueError("longitudinal stress leaves no yield margin")
    ratio = (
        max(1.0, test_allowable_pa / design_allowable_pa)
        if design_temperature_k > test_temperature_k
        else 1.0
    )
    required = 1.5 * design_pressure_pa * ratio
    ro = outside_diameter_m / 2
    ri = ro - minimum_test_wall_m
    metal = ro * ro - ri * ri
    hoop_limit = test_yield_pa * metal / (ro * ro + ri * ri)
    axial_limit = remaining * metal / (ri * ri)
    limit = min(hoop_limit, axial_limit)
    target = min(required, limit)
    return HydrotestResult(
        required,
        limit,
        target,
        target < required,
        target <= component_test_limit_pa,
        refs,
    )
