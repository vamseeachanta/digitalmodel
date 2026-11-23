# ABOUTME: Golden tests for class-rule scantling checks — minimum plate
# ABOUTME: thickness, plate thickness for pressure, stiffener section modulus.
import math

import pytest

from digitalmodel.naval_architecture.scantlings import (
    check_minimum_plate_thickness,
    check_plate_thickness_pressure,
    check_stiffener_section_modulus,
    material_factor_f1,
    minimum_plate_thickness,
    plate_aspect_factor,
    plate_thickness_for_pressure,
    required_section_modulus,
)


# --- aspect factor ka = (1.1 - 0.25 s/l)^2, capped at 1.0 --------------------
def test_plate_aspect_factor():
    assert math.isclose(plate_aspect_factor(1.0, 1.0), 0.7225, rel_tol=1e-9)
    assert math.isclose(plate_aspect_factor(2.0, 2.5), 0.81, rel_tol=1e-9)
    assert plate_aspect_factor(0.8, 2.5) == 1.0  # (1.02)^2 -> capped


# --- plate thickness for lateral pressure -----------------------------------
def test_plate_thickness_for_pressure():
    # s=0.8, l=2.5 (ka=1.0), p=100 kN/m^2, sigma=160 -> 9.992 mm.
    t = plate_thickness_for_pressure(0.8, 2.5, 100.0, 160.0)
    assert math.isclose(t, 15.8 * 0.8 * 10.0 / math.sqrt(160.0), rel_tol=1e-9)
    assert math.isclose(t, 9.9926, rel_tol=1e-4)


def test_plate_thickness_adds_corrosion():
    base = plate_thickness_for_pressure(0.8, 2.5, 100.0, 160.0)
    with_corr = plate_thickness_for_pressure(0.8, 2.5, 100.0, 160.0,
                                             corrosion_mm=1.5)
    assert math.isclose(with_corr - base, 1.5, rel_tol=1e-9)


# --- required stiffener section modulus -------------------------------------
def test_required_section_modulus():
    # s=0.8, l=2.5, p=100, sigma=160, m=12 -> 260.42 cm^3.
    z = required_section_modulus(0.8, 2.5, 100.0, 160.0)
    assert math.isclose(z, 1000 * 100 * 0.8 * 2.5 ** 2 / (12 * 160), rel_tol=1e-9)
    assert math.isclose(z, 260.4167, rel_tol=1e-4)


def test_section_modulus_simply_supported_is_larger():
    clamped = required_section_modulus(0.8, 2.5, 100.0, 160.0, m_factor=12.0)
    ss = required_section_modulus(0.8, 2.5, 100.0, 160.0, m_factor=8.0)
    assert ss > clamped
    assert math.isclose(ss / clamped, 12.0 / 8.0, rel_tol=1e-9)


# --- minimum plate thickness by location ------------------------------------
def test_minimum_plate_thickness():
    # L=150, bottom (5.0 + 0.04L), mild steel -> 11.0 mm.
    assert math.isclose(minimum_plate_thickness(150.0, "bottom"), 11.0,
                        rel_tol=1e-9)
    # Higher-strength steel reduces it by sqrt(235/ReH).
    ah36 = minimum_plate_thickness(150.0, "bottom", yield_mpa=355.0)
    assert math.isclose(ah36, 11.0 * math.sqrt(235.0 / 355.0), rel_tol=1e-9)
    assert ah36 < 11.0
    with pytest.raises(KeyError):
        minimum_plate_thickness(150.0, "funnel")


def test_material_factor_f1():
    assert material_factor_f1(235.0) == 1.0
    assert math.isclose(material_factor_f1(355.0), 355.0 / 235.0, rel_tol=1e-9)


# --- compliance-style pass/fail checks --------------------------------------
def test_check_dicts_shape_and_pass():
    chk = check_plate_thickness_pressure(12.0, 0.8, 2.5, 100.0, 160.0)
    assert set(chk) == {"name", "code", "pass", "value", "required", "unit"}
    assert chk["code"] == "DNV-RU-SHIP / IACS CSR"
    assert chk["pass"] is True            # 12 >= 9.99
    assert chk["unit"] == "mm"

    fail = check_plate_thickness_pressure(8.0, 0.8, 2.5, 100.0, 160.0)
    assert fail["pass"] is False          # 8 < 9.99


def test_check_section_modulus_and_min_thickness():
    z = check_stiffener_section_modulus(300.0, 0.8, 2.5, 100.0, 160.0)
    assert z["pass"] is True              # 300 >= 260.4
    assert z["unit"] == "cm³"

    t = check_minimum_plate_thickness(10.0, 150.0, "bottom")
    assert t["pass"] is False             # 10 < 11.0
    assert t["required"] == 11.0
