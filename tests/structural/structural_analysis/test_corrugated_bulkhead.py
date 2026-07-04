# ABOUTME: Tests for corrugated-bulkhead buckling — hand-derived trapezoidal
# ABOUTME: section properties (Z, I per corrugation & per unit width) + mode checks.
import math

import pytest

from digitalmodel.structural.structural_analysis.buckling import (
    PlateBucklingAnalyzer,
)
from digitalmodel.structural.structural_analysis.corrugated_bulkhead import (
    CODE_REFERENCE,
    CorrugatedBulkheadGeometry,
    check_corrugated_bulkhead,
)
from digitalmodel.structural.structural_analysis.models import (
    STEEL_AH36,
    PlateGeometry,
)


# A clean trapezoidal corrugation used for the golden hand calcs:
#   a (flange) = 300 mm, c (web slant) = 200 mm, t = 10 mm, phi = 30 deg
#   => d = c*sin(30) = 200*0.5 = 100 mm (exact)
#      s = a + c*cos(30) = 300 + 200*0.8660254038 = 473.2050808 mm
def _geom(**kw):
    base = dict(
        flange_width_mm=300.0,
        web_width_mm=200.0,
        thickness_mm=10.0,
        corrugation_angle_deg=30.0,
        span_mm=3000.0,
    )
    base.update(kw)
    return CorrugatedBulkheadGeometry(**base)


# --- derived geometry --------------------------------------------------------
def test_derived_depth_and_pitch():
    g = _geom()
    assert math.isclose(g.corrugation_depth_mm, 100.0, rel_tol=1e-12)
    assert math.isclose(
        g.half_pitch_mm, 300.0 + 200.0 * math.cos(math.radians(30)), rel_tol=1e-12
    )
    assert math.isclose(g.neutral_axis_mm, 50.0, rel_tol=1e-12)


# --- GOLDEN 1: equal-thickness section properties ----------------------------
# By hand (a=300, c=200, t=10, d=100):
#   A = t*(a+c)            = 10*500            = 5000      mm^2
#   I = t*d^2*(3a+c)/12    = 10*1e4*1100/12    = 9,166,666.6667 mm^4
#   Z = t*d*(3a+c)/6       = 10*100*1100/6     = 183,333.3333   mm^3
#   per unit width (/s, s = 473.2050808 mm):
#     Z/s = 387.4327 mm^2 (=mm^3/mm) ,  I/s = 19,371.6 mm^3 (=mm^4/mm)
def test_golden_section_properties_equal_thickness():
    g = _geom()
    assert math.isclose(g.area_mm2, 5000.0, rel_tol=1e-12)
    assert math.isclose(g.moment_of_inertia_mm4, 9_166_666.66667, rel_tol=1e-9)
    assert math.isclose(g.section_modulus_mm3, 183_333.33333, rel_tol=1e-9)

    # consistency: Z = I / (d/2)
    assert math.isclose(
        g.section_modulus_mm3,
        g.moment_of_inertia_mm4 / g.neutral_axis_mm,
        rel_tol=1e-12,
    )

    # per-unit-width golden values
    s = g.half_pitch_mm
    assert math.isclose(
        g.section_modulus_per_unit_width_mm2, 183_333.33333 / s, rel_tol=1e-9
    )
    assert math.isclose(g.section_modulus_per_unit_width_mm2, 387.4327, rel_tol=1e-4)
    assert math.isclose(
        g.moment_of_inertia_per_unit_width_mm3, 9_166_666.66667 / s, rel_tol=1e-9
    )
    assert math.isclose(g.moment_of_inertia_per_unit_width_mm3, 19_371.64, rel_tol=1e-4)


# --- GOLDEN 2: unequal flange/web thickness (general IACS form) --------------
# t_f=12, t_w=8, a=300, c=200, d=100:
#   Z = d*(3 a t_f + c t_w)/6 = 100*(10800+1600)/6 = 206,666.6667 mm^3
#   I = a t_f (d/2)^2 + c t_w d^2/12 = 9,000,000 + 1,333,333.33 = 10,333,333.33 mm^4
def test_golden_section_properties_unequal_thickness():
    g = _geom(thickness_mm=12.0, web_thickness_mm=8.0)
    assert math.isclose(g.section_modulus_mm3, 206_666.66667, rel_tol=1e-9)
    assert math.isclose(g.moment_of_inertia_mm4, 10_333_333.33333, rel_tol=1e-9)
    assert math.isclose(g.area_mm2, 300.0 * 12.0 + 200.0 * 8.0, rel_tol=1e-12)


# --- GOLDEN 3: symmetric 45-deg corrugation (independent cross-check) --------
# a = c = 800 mm, phi = 45 deg, t = 15 mm:
#   d = c*sin(45) = 800*0.7071068 = 565.685 mm ; d^2 = 320000
#   A = t*(a+c)            = 15*1600              = 24000     mm^2
#   I = t*d^2*(3a+c)/12    = 15*320000*3200/12    = 1.280e9   mm^4
#   Z = t*d*(3a+c)/6       = 15*565.685*3200/6    = 4.525e6   mm^3
#   flange elastic plate buckling: sigma_E = 4 pi^2 E/[12(1-nu^2)] (t/a)^2
#     = 4 pi^2 * 206000 / (12*0.91) * (15/800)^2 = 261.82 MPa
def test_golden_symmetric_45deg():
    g = CorrugatedBulkheadGeometry(
        flange_width_mm=800.0,
        web_width_mm=800.0,
        thickness_mm=15.0,
        corrugation_angle_deg=45.0,
        span_mm=4000.0,
    )
    assert math.isclose(
        g.corrugation_depth_mm, 800.0 * math.sin(math.radians(45)), rel_tol=1e-12
    )
    assert math.isclose(g.area_mm2, 24000.0, rel_tol=1e-12)
    assert math.isclose(g.moment_of_inertia_mm4, 1.280e9, rel_tol=1e-3)
    assert math.isclose(g.section_modulus_mm3, 4.525e6, rel_tol=1e-3)

    # full pitch is exactly twice the half-pitch unit
    assert math.isclose(g.full_pitch_mm, 2.0 * g.half_pitch_mm, rel_tol=1e-12)

    # flange elastic plate-buckling stress (reused DNV-RP-C201 plate solver)
    plate = PlateBucklingAnalyzer(STEEL_AH36)
    flange = PlateGeometry(length=g.span_mm, width=g.flange_width_mm, thickness=g.t_f)
    assert math.isclose(plate.elastic_buckling_stress(flange), 261.82, rel_tol=1e-3)


def test_result_is_flagged_preliminary():
    res = check_corrugated_bulkhead(_geom(), STEEL_AH36, axial_stress_mpa=60.0)
    assert res.preliminary is True


# --- buckling: flange local mode reuses the validated plate solver ----------
def test_flange_buckling_matches_plate_solver():
    g = _geom()
    sigma = 120.0
    res = check_corrugated_bulkhead(g, STEEL_AH36, axial_stress_mpa=sigma)

    # Independent re-computation with the plate solver for the flange element.
    plate = PlateBucklingAnalyzer(STEEL_AH36)
    flange = PlateGeometry(length=g.span_mm, width=g.flange_width_mm, thickness=g.t_f)
    direct = plate.check_plate_buckling(flange, sigma_x=sigma, gamma_m=1.15)
    assert math.isclose(res.flange_buckling_util, direct.utilization, rel_tol=1e-12)
    assert res.code_reference == CODE_REFERENCE


# --- buckling: wider (more slender) flange raises local utilisation ---------
def test_wider_flange_more_slender():
    narrow = check_corrugated_bulkhead(
        _geom(flange_width_mm=200.0), STEEL_AH36, axial_stress_mpa=120.0
    )
    wide = check_corrugated_bulkhead(
        _geom(flange_width_mm=500.0), STEEL_AH36, axial_stress_mpa=120.0
    )
    assert wide.flange_buckling_util > narrow.flange_buckling_util


# --- buckling: governing-mode selection -------------------------------------
def test_slender_flange_governs_and_fails():
    # Wide, thin flange under high axial -> local flange buckling governs & fails.
    g = _geom(flange_width_mm=900.0, thickness_mm=8.0)
    res = check_corrugated_bulkhead(g, STEEL_AH36, axial_stress_mpa=260.0)
    assert res.governing_mode == "flange_plate_buckling"
    assert res.passes is False
    assert res.utilization > 1.0


def test_high_shear_governs():
    g = _geom()
    res = check_corrugated_bulkhead(
        g, STEEL_AH36, axial_stress_mpa=30.0, shear_stress_mpa=140.0
    )
    assert res.governing_mode == "web_shear_buckling"
    assert res.shear_buckling_util > res.flange_buckling_util


def test_robust_corrugation_passes():
    # Compact corrugation, modest load -> all modes well within capacity.
    g = _geom(flange_width_mm=250.0, thickness_mm=14.0)
    res = check_corrugated_bulkhead(
        g, STEEL_AH36, axial_stress_mpa=60.0, shear_stress_mpa=20.0
    )
    assert res.passes is True
    assert res.utilization < 1.0


# --- validation --------------------------------------------------------------
def test_geometry_validation():
    with pytest.raises(ValueError):
        _geom(flange_width_mm=-1.0)
    with pytest.raises(ValueError):
        _geom(corrugation_angle_deg=0.0)
    with pytest.raises(ValueError):
        _geom(corrugation_angle_deg=90.0)
