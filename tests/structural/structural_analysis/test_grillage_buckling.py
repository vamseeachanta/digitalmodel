# ABOUTME: Golden + property tests for the cross-stiffened grillage buckling
# ABOUTME: solver (DNV-RP-C201 Sec. 7.3-7.4): orthotropic mode + governing pick.
"""Tests for ``grillage_buckling``.

Golden values (derivations in
``docs/domains/grillage-buckling-validation-2026-06-28.md``):

* Isotropic collapse: an orthotropic plate with ``Dx = Dy = H = E t^3 /
  (12 (1 - nu^2))`` reproduces the DNV plate-buckling stress with ``k = 4``
  (223.11 MPa for ``t = 12, b = 700, E = 210000, nu = 0.3``).
* Reduces to the single stiffener: a one-bay grillage suppresses the overall
  mode and its longitudinal utilisation equals the standalone
  ``StiffenedPanelBucklingAnalyzer`` result exactly.
* Closer transverse frames raise the overall capacity (monotone) and the
  governing mode switches as the layout changes.
"""

import math

import pytest

from digitalmodel.structural.structural_analysis.models import (
    STEEL_AH36,
    MaterialProperties,
)
from digitalmodel.structural.structural_analysis.panel_buckling import (
    StiffenerGeometry,
)
from digitalmodel.structural.structural_analysis.grillage_buckling import (
    GRILLAGE_CODE_REFERENCE,
    GrillageGeometry,
    GrillageBucklingAnalyzer,
)


# ---------------------------------------------------------------------------
# Fixtures / helpers
# ---------------------------------------------------------------------------
def _longitudinal():
    return StiffenerGeometry(
        web_height=150,
        web_thickness=10,
        flange_width=0,
        flange_thickness=0,
        spacing=600,
        section_type="flatbar",
    )


def _transverse(spacing=2400):
    return StiffenerGeometry(
        web_height=250,
        web_thickness=12,
        flange_width=0,
        flange_thickness=0,
        spacing=spacing,
        section_type="flatbar",
    )


def _canonical_grillage(s_T=2400, length_x=9600):
    """Light, wide AH36 grillage where the orthotropic mode is meaningful."""
    return GrillageGeometry(
        length_x=length_x,
        width_y=3600,
        plate_thickness=10,
        longitudinal=_longitudinal(),
        transverse=_transverse(s_T),
    )


# ---------------------------------------------------------------------------
# Golden 1: isotropic collapse to the DNV k = 4 plate-buckling stress
# ---------------------------------------------------------------------------
def test_orthotropic_collapses_to_isotropic_k4_plate():
    """With Dx = Dy = H = plate rigidity Dp, the long-panel orthotropic minimum
    equals the DNV plate-buckling force 4*pi^2*Dp/b^2, i.e. k = 4."""
    E, nu, t, b = 210000.0, 0.3, 12.0, 700.0
    Dp = E * t**3 / (12.0 * (1.0 - nu**2))

    # Long panel (a = 3b) so the integer minimum reaches the continuous k = 4.
    res = GrillageBucklingAnalyzer.orthotropic_critical_force(
        Dp, Dp, Dp, a=3.0 * b, b=b
    )
    sigma_e = res["Nx_cr"] / t

    k4_plate = 4.0 * math.pi**2 * E / (12.0 * (1.0 - nu**2)) * (t / b) ** 2
    assert sigma_e == pytest.approx(223.11, abs=0.05)
    assert sigma_e == pytest.approx(k4_plate, rel=1e-9)


def test_orthotropic_euler_limit_single_longitudinal():
    """Dy = H = 0 (no transverse stiffness) -> N_x,cr = pi^2 Dx / a^2 at m = 1,
    i.e. Euler buckling of the longitudinal direction over the full length."""
    Dx, a, b = 5.0e10, 9600.0, 3600.0
    res = GrillageBucklingAnalyzer.orthotropic_critical_force(Dx, 0.0, 0.0, a, b)
    assert res["m"] == 1
    assert res["n"] == 1
    assert res["Nx_cr"] == pytest.approx(math.pi**2 * Dx / a**2, rel=1e-9)


# ---------------------------------------------------------------------------
# Golden 2: overall orthotropic mode for a defined grillage
# ---------------------------------------------------------------------------
def test_overall_mode_golden_values():
    """Defined AH36 grillage -> hand-checked overall elastic/critical stress."""
    g = _canonical_grillage()
    A = GrillageBucklingAnalyzer(STEEL_AH36)
    ob = A.overall_buckling(g, sigma_x=150.0)

    assert ob["applicable"] is True
    assert ob["num_frame_bays"] == 4
    assert ob["m"] == 3
    assert ob["n"] == 1
    # Orthotropic elastic buckling stress (pre Johnson-Ostenfeld).
    assert ob["sigma_e"] == pytest.approx(927.9, abs=1.0)
    # Inelastic (Johnson-Ostenfeld) characteristic capacity, capped near fy.
    assert ob["sigma_cr"] == pytest.approx(321.0, abs=1.0)
    assert ob["sigma_cr"] < STEEL_AH36.yield_strength
    # Effective-width reduction was applied to the wide transverse plate.
    assert 0.0 < ob["Cxs_transverse"] < 1.0


# ---------------------------------------------------------------------------
# Golden 3 / monotonicity: closer transverse frames raise the overall capacity
# ---------------------------------------------------------------------------
def test_closer_transverse_frames_raise_overall_capacity():
    A = GrillageBucklingAnalyzer(STEEL_AH36)
    spacings = [4800, 3200, 2400, 1600, 1200]
    sigma_e = []
    for s_T in spacings:
        ob = A.overall_buckling(_canonical_grillage(s_T=s_T), sigma_x=150.0)
        sigma_e.append(ob["sigma_e"])
    # Strictly increasing as transverse spacing decreases.
    assert all(b > a for a, b in zip(sigma_e, sigma_e[1:]))


# ---------------------------------------------------------------------------
# Reduces to the single stiffener (acceptance criterion)
# ---------------------------------------------------------------------------
def test_single_bay_reduces_to_single_stiffener():
    """One-bay grillage: overall mode is suppressed and the longitudinal
    utilisation equals the standalone single-stiffener panel result."""
    A = GrillageBucklingAnalyzer(STEEL_AH36)
    g = _canonical_grillage(s_T=9600, length_x=9600)  # frames only at the ends
    assert g.num_frame_bays() == 1

    r = A.check_grillage(g, sigma_x=80.0)
    assert r.overall_mode_applicable is False
    assert r.overall_utilization == 0.0

    # Standalone validated single-stiffener panel over the same span.
    panel = A.longitudinal_panel(g)
    standalone = A.panel_analyzer.check_panel(panel, 80.0)
    standalone_util = max(
        standalone.details["column_result"].utilization,
        standalone.details["torsional"]["utilization"],
    )
    assert r.longitudinal_utilization == pytest.approx(standalone_util, rel=1e-12)
    assert r.governing_mode == "longitudinal_stiffener"


# ---------------------------------------------------------------------------
# Governing-mode selection switches correctly
# ---------------------------------------------------------------------------
def test_governing_mode_switches_with_layout():
    """Widely spaced frames -> long, slender longitudinal columns govern;
    closely spaced frames -> the plate field governs."""
    A = GrillageBucklingAnalyzer(STEEL_AH36)

    wide = A.check_grillage(_canonical_grillage(s_T=4800, length_x=14400), 150.0)
    assert wide.governing_mode == "longitudinal_stiffener"
    assert wide.utilization > 1.0  # slender columns -> fails

    tight = A.check_grillage(_canonical_grillage(s_T=1800, length_x=14400), 150.0)
    assert tight.governing_mode == "plate_field"
    assert tight.governing_mode != wide.governing_mode


def test_transverse_frame_can_govern_under_biaxial_load():
    """Transverse compression sigma_y loads the transverse frame, which can then
    become the governing mode."""
    A = GrillageBucklingAnalyzer(STEEL_AH36)
    g = _canonical_grillage(s_T=1800, length_x=14400)
    r = A.check_grillage(g, sigma_x=120.0, sigma_y=120.0)
    assert r.governing_mode == "transverse_frame"
    assert r.transverse_utilization > 0.0


# ---------------------------------------------------------------------------
# Plumbing / contract
# ---------------------------------------------------------------------------
def test_result_contract_and_code_reference():
    A = GrillageBucklingAnalyzer(STEEL_AH36)
    r = A.check_grillage(_canonical_grillage(), sigma_x=150.0)
    assert r.code_reference == GRILLAGE_CODE_REFERENCE == "DNV-RP-C201"
    # Governing utilisation is the max across the four modes.
    assert r.utilization == pytest.approx(
        max(
            r.plate_utilization,
            r.longitudinal_utilization,
            r.transverse_utilization,
            r.overall_utilization,
        )
    )
    assert isinstance(r.passes, bool)
    assert isinstance(r.overall_half_waves, int)
    assert isinstance(r.overall_mode_applicable, bool)


def test_passes_flag_consistent_with_utilization():
    A = GrillageBucklingAnalyzer(STEEL_AH36)
    # Low stress -> passes.
    r_low = A.check_grillage(_canonical_grillage(), sigma_x=40.0)
    assert r_low.passes == (r_low.utilization <= 1.0)
    assert r_low.passes is True
    # High stress -> fails.
    r_high = A.check_grillage(_canonical_grillage(), sigma_x=400.0)
    assert r_high.passes is False


def test_custom_material_runs():
    mat = MaterialProperties(
        yield_strength=355,
        ultimate_strength=490,
        youngs_modulus=210000,
        poissons_ratio=0.3,
        density=7850,
        name="S355",
    )
    A = GrillageBucklingAnalyzer(mat)
    r = A.check_grillage(_canonical_grillage(), sigma_x=150.0)
    assert r.governing_mode in (
        "plate_field",
        "longitudinal_stiffener",
        "transverse_frame",
        "overall_grillage",
    )
