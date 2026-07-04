# ABOUTME: Tests for the API 11B sucker-rod catalog — grades/sizes, the
# ABOUTME: modified-Goodman allowable-stress check, and dynacard tie-in.
import math

import pytest

from digitalmodel.well.tubulars import (
    RodGrade,
    RodTaperSection,
    SuckerRod,
    modified_goodman_allowable,
    rod_area,
    rod_grade,
    rod_string_goodman,
    sucker_rod,
)


# --- modified-Goodman allowable (API 11BR): SA = (T/4 + 0.5625*Smin)*SF -------
def test_modified_goodman_allowable_formula():
    # Grade D, T = 115,000, Smin = 10,000, SF = 1.0 -> 28,750 + 5,625 = 34,375.
    assert modified_goodman_allowable(115_000, 10_000, 1.0) == 34_375.0
    # Grade C, T = 90,000, Smin = 0 -> 22,500.
    assert modified_goodman_allowable(90_000, 0.0) == 22_500.0
    # Service factor derates linearly.
    assert modified_goodman_allowable(115_000, 10_000, 0.9) == \
        pytest.approx(34_375.0 * 0.9)


# --- grades & sizes ----------------------------------------------------------
def test_grades():
    g = rod_grade("D")
    assert isinstance(g, RodGrade)
    assert g.min_tensile_psi == 115_000
    assert rod_grade("c").min_tensile_psi == 90_000  # case-insensitive
    with pytest.raises(KeyError):
        rod_grade("Z")


def test_rod_area():
    assert math.isclose(rod_area(0.875), math.pi / 4 * 0.875 ** 2, rel_tol=1e-12)
    assert math.isclose(rod_area(0.875), 0.6013, rel_tol=1e-3)  # 7/8" rod


# --- Goodman check on a load cycle -------------------------------------------
def test_goodman_check_pass_and_loading():
    rod = sucker_rod("D", 0.875)            # 7/8" Grade D, area 0.6013 in^2
    res = rod.goodman_check(peak_load_lbf=15_000.0, min_load_lbf=5_000.0)
    area = rod.area_in2
    s_max = 15_000.0 / area
    s_min = 5_000.0 / area
    s_allow = 115_000 / 4 + 0.5625 * s_min
    assert math.isclose(res["max_stress_psi"], s_max, rel_tol=1e-12)
    assert math.isclose(res["allowable_stress_psi"], s_allow, rel_tol=1e-12)
    assert math.isclose(res["loading"], s_max / s_allow, rel_tol=1e-12)
    assert res["passes"] is True
    assert res["code_reference"] == "API 11B / modified Goodman"


def test_goodman_check_fails_when_overloaded():
    rod = sucker_rod("C", 0.625)            # 5/8" Grade C
    res = rod.goodman_check(peak_load_lbf=20_000.0, min_load_lbf=0.0)
    assert res["passes"] is False
    assert res["loading"] > 1.0


def test_higher_min_stress_lifts_allowable():
    # Same peak, higher minimum -> higher Goodman allowable (the diagram line).
    rod = sucker_rod("D", 0.875)
    low = rod.goodman_check(30_000.0, 0.0)["allowable_stress_psi"]
    high = rod.goodman_check(30_000.0, 8_000.0)["allowable_stress_psi"]
    assert high > low


def test_service_factor_validation():
    with pytest.raises(ValueError):
        SuckerRod(nominal_in=0.875, grade=rod_grade("D"), service_factor=1.5)
    with pytest.raises(ValueError):
        SuckerRod(nominal_in=-1.0, grade=rod_grade("D"))


# --- tapered rod string ------------------------------------------------------
def test_rod_string_goodman_per_section():
    top = RodTaperSection(sucker_rod("D", 0.875), length_ft=2000.0)
    bot = RodTaperSection(sucker_rod("D", 0.750), length_ft=3000.0)
    results = rod_string_goodman([
        (top, 25_000.0, 8_000.0),
        (bot, 18_000.0, 6_000.0),
    ])
    assert len(results) == 2
    assert results[0]["nominal_in"] == 0.875
    assert results[1]["length_ft"] == 3000.0
    assert all("passes" in r for r in results)


# --- dynacard tie-in ---------------------------------------------------------
def test_to_rod_section_carries_grade_tensile():
    section = sucker_rod("D", 0.875).to_rod_section(length_ft=2000.0, count=80)
    assert section.diameter == 0.875
    assert section.minimum_tensile == 115_000
    assert section.grade == "D"
    assert section.length == 2000.0
