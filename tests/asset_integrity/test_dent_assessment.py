# ABOUTME: Tests for dent / dent-gouge screening — depth-threshold behaviour,
# ABOUTME: Appendix R strain derivation anchor, monotonicity, routing, validation.
"""Tests for digitalmodel.asset_integrity.dent_assessment.

Validation basis
----------------
* DERIVATION-ANCHOR -- the strain expectations are computed inside the test
  from the same written-out ASME B31.8 Appendix R formulation documented in
  the module docstring (parabolic apex radii, reversed-curvature screening
  assumption).  No published worked dent-strain example with numeric output
  was embedded; the anchors guard the implemented formula, not a standard's
  table value.
* The 7 % OD plain-dent and 6 % strain thresholds are published values
  (EPRG plain-dent guideline; ASME B31.8 strain practice); the tests only
  exercise threshold *behaviour* around them.
"""

import math

import pytest

from digitalmodel.asset_integrity.dent_assessment import (
    PLAIN_DENT_DEPTH_LIMIT,
    PLAIN_DENT_STRAIN_LIMIT,
    VERDICT_ORDER,
    WELD_DENT_DEPTH_LIMIT,
    DentAssessment,
    assess_dent,
    dent_strain_b31_8,
)

# Reference pipe: 24 in OD x 0.500 in WT (depth-only screens unless noted).
OD, WT = 24.0, 0.500


# --- Plain-dent depth screen around 7 % OD ----------------------------------
def test_defaults_are_the_published_values():
    assert PLAIN_DENT_DEPTH_LIMIT == pytest.approx(0.07)
    assert WELD_DENT_DEPTH_LIMIT == pytest.approx(0.02)
    assert PLAIN_DENT_STRAIN_LIMIT == pytest.approx(0.06)


def test_depth_just_below_7pct_accepts():
    r = assess_dent(OD, WT, 0.069 * OD)
    assert isinstance(r, DentAssessment)
    assert r.verdict == "ACCEPT"
    assert r.depth_ratio == pytest.approx(0.069)
    assert "depth screen" in r.governing_criterion


def test_depth_just_above_7pct_needs_assessment_without_strain():
    r = assess_dent(OD, WT, 0.071 * OD)
    assert r.verdict == "NEEDS_ASSESSMENT"
    assert "exceeds" in r.governing_criterion
    # No lengths given -> no strain estimate, and the result says so.
    assert r.strain is None
    assert any("strain" in n for n in r.validity_notes)


def test_depth_exactly_at_limit_accepts():
    r = assess_dent(OD, WT, PLAIN_DENT_DEPTH_LIMIT * OD)
    assert r.verdict == "ACCEPT"


def test_threshold_is_parameterized():
    # ASME B31.8 operational practice (6 % OD) via the parameter.
    r = assess_dent(OD, WT, 0.065 * OD, plain_dent_depth_limit=0.06)
    assert r.verdict == "NEEDS_ASSESSMENT"
    assert assess_dent(OD, WT, 0.065 * OD).verdict == "ACCEPT"


# --- Appendix R strain estimate (DERIVATION-ANCHOR) --------------------------
def test_strain_hand_check_derivation_anchor():
    """DERIVATION-ANCHOR: expected strain computed here from the written-out
    Appendix R formulation (module docstring), independently of the module."""
    d, ll, lc = 1.2, 12.0, 10.0
    t, r0 = WT, OD / 2.0
    eps1 = (t / 2.0) * (1.0 / r0 + 8.0 * d / lc**2)   # 1/R1 = -8d/Lc^2
    eps2 = (t / 2.0) * (8.0 * d / ll**2)              # R2 = -LL^2/(8d)
    eps3 = 0.5 * (d / ll) ** 2
    y_i = eps2 + eps3
    eps_i = math.sqrt(eps1**2 - eps1 * y_i + y_i**2)
    y_o = eps3 - eps2
    eps_o = math.sqrt(eps1**2 + eps1 * y_o + y_o**2)

    s = dent_strain_b31_8(OD, WT, d, ll, lc)
    assert s.eps_circ_bending == pytest.approx(eps1, rel=1e-12)
    assert s.eps_long_bending == pytest.approx(eps2, rel=1e-12)
    assert s.eps_long_membrane == pytest.approx(eps3, rel=1e-12)
    assert s.eps_inside == pytest.approx(eps_i, rel=1e-12)
    assert s.eps_outside == pytest.approx(eps_o, rel=1e-12)
    assert s.eps_max == pytest.approx(max(eps_i, eps_o), rel=1e-12)
    # Signed radii carry the reversed-curvature screening assumption.
    assert s.r1_in == pytest.approx(-(lc**2) / (8 * d))
    assert s.r2_in == pytest.approx(-(ll**2) / (8 * d))
    assert s.r0_in == pytest.approx(12.0)


def test_strain_numeric_spot_value():
    # OD=24, t=0.5, d=1.68 (7 % OD), LL=Lc=12:
    # eps1 = 0.25*(2/24 + 8*1.68/144) = 0.0441667
    # eps2 = 0.25*(8*1.68/144)        = 0.0233333
    # eps3 = 0.5*(1.68/12)^2          = 0.0098
    s = dent_strain_b31_8(OD, WT, 1.68, 12.0, 12.0)
    assert s.eps_circ_bending == pytest.approx(0.0441667, rel=1e-5)
    assert s.eps_long_bending == pytest.approx(0.0233333, rel=1e-5)
    assert s.eps_long_membrane == pytest.approx(0.0098, rel=1e-5)
    assert s.eps_inside == pytest.approx(
        math.sqrt(0.0441667**2 - 0.0441667 * 0.0331333 + 0.0331333**2),
        rel=1e-4)


def test_strain_increases_with_depth():
    strains = [dent_strain_b31_8(OD, WT, d, 12.0, 12.0).eps_max
               for d in (0.3, 0.8, 1.5, 2.4, 3.0)]
    assert all(b > a for a, b in zip(strains, strains[1:]))


def test_strain_governs_when_depth_screen_passes():
    # Sharp, short dent: depth 1.5 in (6.25 % OD, passes depth) but strains
    # over 6 % because the dent is only 5 in long in each direction.
    r = assess_dent(OD, WT, 1.5, 5.0, 5.0)
    assert r.strain is not None and r.strain.eps_max > PLAIN_DENT_STRAIN_LIMIT
    assert r.verdict == "NEEDS_ASSESSMENT"
    assert "strain" in r.governing_criterion


def test_deep_but_strain_qualified_dent_is_monitor():
    # Long smooth dent on a large pipe: 7.5 % OD (over the depth screen) but
    # estimated strain well under 6 % -> strain-qualified MONITOR.
    r = assess_dent(42.0, 0.5, 0.075 * 42.0, 60.0, 60.0)
    assert r.strain is not None and r.strain.eps_max < PLAIN_DENT_STRAIN_LIMIT
    assert r.verdict == "MONITOR"
    assert "strain-qualified" in r.governing_criterion


# --- Monotonicity: deeper is never better ------------------------------------
def test_verdict_monotonic_in_depth_with_strain():
    depths = [0.2, 0.5, 1.0, 1.5, 1.9, 2.4, 3.0, 3.6]
    ranks = [VERDICT_ORDER[assess_dent(OD, WT, d, 12.0, 12.0).verdict]
             for d in depths]
    assert ranks == sorted(ranks)
    assert ranks[0] == VERDICT_ORDER["ACCEPT"]
    assert ranks[-1] >= VERDICT_ORDER["NEEDS_ASSESSMENT"]


def test_verdict_monotonic_in_depth_depth_only():
    depths = [0.01 * OD, 0.05 * OD, 0.08 * OD, 0.15 * OD]
    ranks = [VERDICT_ORDER[assess_dent(OD, WT, d).verdict] for d in depths]
    assert ranks == sorted(ranks)


# --- Dent-gouge routing -------------------------------------------------------
def test_dent_gouge_always_needs_assessment():
    for depth in (0.005 * OD, 0.05 * OD, 0.12 * OD):
        r = assess_dent(OD, WT, depth, with_gouge_or_metal_loss=True)
        assert r.verdict == "NEEDS_ASSESSMENT"
        assert "fracture-controlled" in r.governing_criterion


def test_dent_gouge_overrides_weld_and_strain_paths():
    r = assess_dent(OD, WT, 0.10 * OD, 12.0, 12.0,
                    on_weld=True, with_gouge_or_metal_loss=True)
    assert r.verdict == "NEEDS_ASSESSMENT"
    assert "dent-gouge" in r.governing_criterion


# --- Weld and restraint modifiers --------------------------------------------
def test_on_weld_is_stricter_than_body():
    depth = 0.05 * OD  # ACCEPT on the body, over the 2 % OD weld screen
    body = assess_dent(OD, WT, depth)
    weld = assess_dent(OD, WT, depth, on_weld=True)
    assert body.verdict == "ACCEPT"
    assert weld.verdict == "REJECT"
    assert VERDICT_ORDER[weld.verdict] > VERDICT_ORDER[body.verdict]


def test_shallow_dent_on_weld_is_monitor_not_accept():
    r = assess_dent(OD, WT, 0.015 * OD, on_weld=True)
    assert r.verdict == "MONITOR"
    assert "weld" in r.governing_criterion


def test_weld_threshold_parameterized():
    r = assess_dent(OD, WT, 0.015 * OD, on_weld=True,
                    weld_dent_depth_limit=0.01)
    assert r.verdict == "REJECT"


def test_restrained_caps_accept_at_monitor():
    depth = 0.04 * OD
    assert assess_dent(OD, WT, depth).verdict == "ACCEPT"
    r = assess_dent(OD, WT, depth, restrained=True)
    assert r.verdict == "MONITOR"
    assert any("restrained" in n for n in r.validity_notes)


# --- Result metadata ----------------------------------------------------------
def test_citations_and_code_reference_present():
    r = assess_dent(OD, WT, 0.5, smys_psi=52_000.0)
    assert r.code_reference.startswith("API 579-1/ASME FFS-1")
    assert any("Appendix R" in c for c in r.citations)
    assert any("EPRG" in c for c in r.citations)
    assert r.thresholds["smys_psi"] == 52_000.0
    assert r.thresholds["plain_dent_depth_limit"] == pytest.approx(0.07)


def test_sharp_dent_gets_profile_reliability_note():
    r = assess_dent(OD, WT, 3.0, 5.0, 12.0)  # d/LL = 0.6 > 0.5
    assert any("half the axial length" in n for n in r.validity_notes)


def test_zero_depth_accepts_with_no_strain():
    r = assess_dent(OD, WT, 0.0)
    assert r.verdict == "ACCEPT"
    assert r.strain is None
    assert r.depth_ratio == 0.0


# --- Validation ---------------------------------------------------------------
def test_validity_errors():
    with pytest.raises(ValueError):
        assess_dent(-24.0, WT, 0.5)                      # bad OD
    with pytest.raises(ValueError):
        assess_dent(OD, 0.0, 0.5)                        # bad WT
    with pytest.raises(ValueError):
        assess_dent(OD, 13.0, 0.5)                       # WT >= OD/2
    with pytest.raises(ValueError):
        assess_dent(OD, WT, -0.1)                        # negative depth
    with pytest.raises(ValueError):
        assess_dent(OD, WT, 25.0)                        # depth >= OD
    with pytest.raises(ValueError):
        assess_dent(OD, WT, 0.5, -1.0, 6.0)              # bad axial length
    with pytest.raises(ValueError):
        assess_dent(OD, WT, 0.5, 6.0, 0.0)               # bad circ length
    with pytest.raises(ValueError):
        assess_dent(OD, WT, 0.5, weld_dent_depth_limit=0.5)  # weld > plain


def test_strain_function_validates():
    with pytest.raises(ValueError):
        dent_strain_b31_8(OD, WT, 0.0, 12.0, 12.0)       # zero depth
    with pytest.raises(ValueError):
        dent_strain_b31_8(OD, WT, 0.5, 12.0, None)       # missing length
