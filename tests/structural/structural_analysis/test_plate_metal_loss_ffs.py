# ABOUTME: Tests for the metal-loss FFS layer (plate_metal_loss_ffs.py): monotonicity,
# ABOUTME: zero-loss == nominal buckling, capacity-retained bounds, max-acceptable-loss, panel modes.
"""Tests for digitalmodel.structural.structural_analysis.plate_metal_loss_ffs.

The FFS layer wraps the VALIDATED DNV-RP-C201 buckling solvers, so these tests
assert relationships and consistency rather than re-deriving buckling numbers:
monotonic worsening with loss, zero-loss equalling the bare buckling check, the
capacity-retained fraction staying in (0, 1] and decreasing, the
max-acceptable-loss bisection bracketing utilisation = 1, the perforation guard,
and a valid panel governing mode.
"""

import pytest

from digitalmodel.structural.structural_analysis.models import (
    STEEL_AH36,
    PlateGeometry,
)
from digitalmodel.structural.structural_analysis.buckling import (
    PlateBucklingAnalyzer,
)
from digitalmodel.structural.structural_analysis.panel_buckling import (
    StiffenerGeometry,
    StiffenedPanelGeometry,
)
from digitalmodel.structural.structural_analysis.plate_metal_loss_ffs import (
    MetalLossFFSResult,
    assess_plate_uniform_loss,
    assess_panel_uniform_loss,
    assess_plate_local_loss,
    max_acceptable_loss,
)


# --- Representative ship structure ----------------------------------------
SIGMA_X = 150.0  # MPa axial compression


def ship_plate():
    # 2400 x 800 x 16 mm hull plate, AH36.
    return PlateGeometry(length=2400.0, width=800.0, thickness=16.0)


def ship_panel():
    # spacing 700, tee web 400x12 / flange 120x16, span 2400, AH36.
    stiff = StiffenerGeometry(
        web_height=400.0, web_thickness=12.0,
        flange_width=120.0, flange_thickness=16.0,
        spacing=700.0, section_type="tee",
    )
    return StiffenedPanelGeometry(
        plate_length=2400.0, plate_thickness=16.0, stiffener=stiff,
    )


# --------------------------------------------------------------------------
# Zero loss == bare buckling check
# --------------------------------------------------------------------------
def test_plate_zero_loss_matches_nominal_buckling():
    geo = ship_plate()
    res = assess_plate_uniform_loss(geo, STEEL_AH36, 0.0, fca_mm=0.0, sigma_x=SIGMA_X)

    bare = PlateBucklingAnalyzer(STEEL_AH36).check_plate_buckling(geo, SIGMA_X)

    assert res.remaining_thickness_mm == pytest.approx(16.0)
    assert res.utilization == pytest.approx(float(bare.utilization), rel=1e-9)
    assert res.utilization == pytest.approx(res.utilization_original, rel=1e-9)
    assert res.capacity_retained_frac == pytest.approx(1.0, rel=1e-9)
    assert isinstance(res, MetalLossFFSResult)
    assert res.level == 1


def test_panel_zero_loss_capacity_retained_unity():
    panel = ship_panel()
    res = assess_panel_uniform_loss(panel, STEEL_AH36, 0.0, sigma_x=SIGMA_X)
    assert res.utilization == pytest.approx(res.utilization_original, rel=1e-9)
    assert res.capacity_retained_frac == pytest.approx(1.0, rel=1e-9)


# --------------------------------------------------------------------------
# Monotonicity: more loss -> higher utilisation, lower capacity retained
# --------------------------------------------------------------------------
def test_plate_monotonic_with_loss():
    geo = ship_plate()
    losses = [0.0, 1.0, 2.0, 4.0, 6.0]
    utils, caps = [], []
    for ml in losses:
        r = assess_plate_uniform_loss(geo, STEEL_AH36, ml, sigma_x=SIGMA_X)
        utils.append(r.utilization)
        caps.append(r.capacity_retained_frac)
    assert all(utils[i] < utils[i + 1] for i in range(len(utils) - 1))
    assert all(caps[i] > caps[i + 1] for i in range(len(caps) - 1))
    # capacity_retained in (0, 1]
    assert all(0.0 < c <= 1.0 for c in caps)


def test_panel_monotonic_with_loss():
    panel = ship_panel()
    losses = [0.0, 1.0, 2.0, 4.0, 6.0]
    utils, caps = [], []
    for ml in losses:
        r = assess_panel_uniform_loss(panel, STEEL_AH36, ml, sigma_x=SIGMA_X)
        utils.append(r.utilization)
        caps.append(r.capacity_retained_frac)
    assert all(utils[i] < utils[i + 1] for i in range(len(utils) - 1))
    assert all(caps[i] > caps[i + 1] for i in range(len(caps) - 1))
    assert all(0.0 < c <= 1.0 for c in caps)


# --------------------------------------------------------------------------
# FCA reduces the effective wall just like metal loss
# --------------------------------------------------------------------------
def test_fca_worsens_like_metal_loss():
    geo = ship_plate()
    only_loss = assess_plate_uniform_loss(geo, STEEL_AH36, 3.0, fca_mm=0.0, sigma_x=SIGMA_X)
    with_fca = assess_plate_uniform_loss(geo, STEEL_AH36, 1.0, fca_mm=2.0, sigma_x=SIGMA_X)
    # Same total wall removed (3 mm) -> same remaining thickness & utilisation.
    assert with_fca.remaining_thickness_mm == pytest.approx(only_loss.remaining_thickness_mm)
    assert with_fca.utilization == pytest.approx(only_loss.utilization, rel=1e-9)


# --------------------------------------------------------------------------
# max_acceptable_loss brackets utilisation = 1
# --------------------------------------------------------------------------
def test_max_acceptable_loss_brackets_unity_plate():
    geo = ship_plate()
    info = max_acceptable_loss(geo, STEEL_AH36, SIGMA_X, is_panel=False)
    ml = info["metal_loss_mm"]
    assert info["limited_by"] == "buckling"
    assert 0.0 < ml < 16.0

    # At the reported loss it just passes; a bit deeper fails, a bit shallower passes.
    at = assess_plate_uniform_loss(geo, STEEL_AH36, ml, sigma_x=SIGMA_X)
    deeper = assess_plate_uniform_loss(geo, STEEL_AH36, ml + 0.5, sigma_x=SIGMA_X)
    shallower = assess_plate_uniform_loss(geo, STEEL_AH36, max(0.0, ml - 0.5), sigma_x=SIGMA_X)

    assert at.utilization == pytest.approx(1.0, abs=2e-3)
    assert bool(at.passes) is True
    assert bool(deeper.passes) is False
    assert deeper.utilization > 1.0
    assert bool(shallower.passes) is True
    assert shallower.utilization < 1.0

    # Percentage is consistent with the mm value vs the 16 mm nominal.
    assert info["metal_loss_pct"] == pytest.approx(100.0 * ml / 16.0, rel=1e-6)


def test_max_acceptable_loss_panel():
    panel = ship_panel()
    info = max_acceptable_loss(panel, STEEL_AH36, SIGMA_X, is_panel=True)
    ml = info["metal_loss_mm"]
    assert 0.0 < ml < 16.0
    at = assess_panel_uniform_loss(panel, STEEL_AH36, ml, sigma_x=SIGMA_X)
    deeper = assess_panel_uniform_loss(panel, STEEL_AH36, ml + 0.5, sigma_x=SIGMA_X)
    assert bool(at.passes) is True
    assert bool(deeper.passes) is False


# --------------------------------------------------------------------------
# Perforation guard: loss + FCA >= thickness raises
# --------------------------------------------------------------------------
def test_full_wall_loss_raises():
    geo = ship_plate()
    with pytest.raises(ValueError):
        assess_plate_uniform_loss(geo, STEEL_AH36, 16.0, sigma_x=SIGMA_X)
    with pytest.raises(ValueError):
        assess_plate_uniform_loss(geo, STEEL_AH36, 14.0, fca_mm=3.0, sigma_x=SIGMA_X)


def test_negative_inputs_raise():
    geo = ship_plate()
    with pytest.raises(ValueError):
        assess_plate_uniform_loss(geo, STEEL_AH36, -1.0, sigma_x=SIGMA_X)
    with pytest.raises(ValueError):
        assess_plate_uniform_loss(geo, STEEL_AH36, 1.0, fca_mm=-1.0, sigma_x=SIGMA_X)


# --------------------------------------------------------------------------
# Panel governing mode reported and valid; mode-change tracked
# --------------------------------------------------------------------------
def test_panel_governing_mode_valid():
    panel = ship_panel()
    res = assess_panel_uniform_loss(panel, STEEL_AH36, 4.0, sigma_x=SIGMA_X)
    assert res.governing_mode in {"plate_induced", "column", "torsional"}
    assert res.mode is None
    assert "governing_mode_changed" in res.details
    assert isinstance(res.details["governing_mode_changed"], bool)
    assert res.details["governing_mode_nominal"] in {
        "plate_induced", "column", "torsional"}


# --------------------------------------------------------------------------
# Level 2 -- local metal-loss patch
# --------------------------------------------------------------------------
def test_local_patch_level2_basic_and_monotonic():
    geo = ship_plate()
    r1 = assess_plate_local_loss(
        geo, STEEL_AH36, metal_loss_mm=2.0,
        patch_length_mm=400.0, patch_width_mm=300.0, sigma_x=SIGMA_X)
    r2 = assess_plate_local_loss(
        geo, STEEL_AH36, metal_loss_mm=4.0,
        patch_length_mm=400.0, patch_width_mm=300.0, sigma_x=SIGMA_X)
    assert r1.level == 2
    assert r2.utilization > r1.utilization
    assert r2.capacity_retained_frac < r1.capacity_retained_frac
    assert 0.0 < r1.capacity_retained_frac <= 1.0
    # patch larger than plate -> error
    with pytest.raises(ValueError):
        assess_plate_local_loss(
            geo, STEEL_AH36, metal_loss_mm=2.0,
            patch_length_mm=9999.0, patch_width_mm=300.0, sigma_x=SIGMA_X)
