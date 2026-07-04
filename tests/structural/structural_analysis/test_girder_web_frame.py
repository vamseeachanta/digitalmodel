# ABOUTME: Tests for girder & web-frame buckling — web-panel shear buckling
# ABOUTME: (k_tau + Johnson-Ostenfeld) and girder tripping bracket-spacing effect.
import math

import pytest

from digitalmodel.structural.structural_analysis.girder_web_frame import (
    WebPanel,
    check_girder_tripping,
    check_web_frame_shear,
    girder_tripping_capacity,
    web_shear_buckling_coefficient,
    web_shear_buckling_stress,
)
from digitalmodel.structural.structural_analysis.models import STEEL_AH36
from digitalmodel.structural.structural_analysis.panel_buckling import (
    StiffenedPanelGeometry,
    StiffenerGeometry,
)


def _girder():
    return StiffenedPanelGeometry(
        plate_length=2400.0, plate_thickness=12.0,
        stiffener=StiffenerGeometry(
            web_height=250.0, web_thickness=10.0, flange_width=90.0,
            flange_thickness=12.0, spacing=800.0, section_type="tee"),
    )


# --- shear buckling coefficient k_tau (two branches) -------------------------
def test_k_tau_branches():
    # a/d = 2 (long): 5.34 + 4*(0.5)^2 = 6.34
    assert math.isclose(web_shear_buckling_coefficient(1000.0, 2000.0), 6.34,
                        rel_tol=1e-9)
    # a/d = 0.5 (short): 4 + 5.34*(2)^2 = 25.36
    assert math.isclose(web_shear_buckling_coefficient(1000.0, 500.0), 25.36,
                        rel_tol=1e-9)
    # square a/d = 1: 5.34 + 4 = 9.34
    assert math.isclose(web_shear_buckling_coefficient(1000.0, 1000.0), 9.34,
                        rel_tol=1e-9)


# --- web shear buckling stress (formula + JO reuse) --------------------------
def test_web_shear_buckling_stress_elastic():
    web = WebPanel(depth_mm=1000.0, thickness_mm=12.0, panel_length_mm=2000.0)
    tau_cr = web_shear_buckling_stress(web, STEEL_AH36)
    # Recompute tau_e (k_tau=6.34) and confirm it is the elastic value (<0.5 fy).
    k = 6.34
    tau_e = (k * math.pi ** 2 * 206000.0 / (12.0 * (1 - 0.3 ** 2))
             * (12.0 / 1000.0) ** 2)
    assert tau_e <= 0.5 * STEEL_AH36.yield_strength
    assert math.isclose(tau_cr, tau_e, rel_tol=1e-9)   # elastic: no knockdown


def test_web_shear_buckling_stress_inelastic_knockdown():
    # Thick web -> tau_e > 0.5 fy -> Johnson-Ostenfeld reduces it below tau_e.
    web = WebPanel(depth_mm=1000.0, thickness_mm=25.0, panel_length_mm=2000.0)
    tau_cr = web_shear_buckling_stress(web, STEEL_AH36)
    k = 6.34
    tau_e = (k * math.pi ** 2 * 206000.0 / (12.0 * (1 - 0.3 ** 2))
             * (25.0 / 1000.0) ** 2)
    assert tau_e > 0.5 * STEEL_AH36.yield_strength
    assert tau_cr < tau_e                               # knocked down
    assert tau_cr < STEEL_AH36.yield_strength


# --- web frame shear check ---------------------------------------------------
def test_check_web_frame_shear():
    web = WebPanel(1000.0, 12.0, 2000.0)
    res = check_web_frame_shear(web, STEEL_AH36, applied_shear_mpa=50.0)
    assert math.isclose(res.k_tau, 6.34, rel_tol=1e-9)
    assert res.passes is True
    assert math.isclose(
        res.utilization, 50.0 / (res.critical_shear_stress_mpa / 1.15),
        rel_tol=1e-9)
    assert res.code_reference == "DNV-RP-C201 / DNV CN 30.1"

    overloaded = check_web_frame_shear(web, STEEL_AH36, applied_shear_mpa=300.0)
    assert overloaded.passes is False


# --- girder tripping: bracket-spacing effect ---------------------------------
def test_closer_brackets_raise_tripping_capacity():
    girder = _girder()
    sigma = 150.0
    wide = girder_tripping_capacity(girder, STEEL_AH36, sigma,
                                    bracket_spacing_mm=2400.0)
    tight = girder_tripping_capacity(girder, STEEL_AH36, sigma,
                                     bracket_spacing_mm=800.0)
    assert tight > wide                                 # closer brackets help
    assert wide > 0


def test_check_girder_tripping():
    girder = _girder()
    res = check_girder_tripping(girder, STEEL_AH36, sigma_x=150.0,
                                bracket_spacing_mm=1200.0)
    assert res.bracket_spacing_mm == 1200.0
    assert res.tripping_capacity_mpa > 0
    assert isinstance(res.passes, bool)
    assert math.isclose(
        res.utilization, 150.0 / (res.tripping_capacity_mpa / 1.15),
        rel_tol=1e-9)


def test_web_panel_validation():
    with pytest.raises(ValueError):
        WebPanel(depth_mm=-1.0, thickness_mm=12.0, panel_length_mm=2000.0)
