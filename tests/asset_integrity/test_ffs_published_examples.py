# ABOUTME: Published-example golden tests for the FFS corroded-pipe strength
# ABOUTME: methods (ASME B31G / Kiefner-Vieth and DNV-RP-F101), issue #1094.
"""Golden tests against PUBLISHED worked examples (validation-only, #1094).

Each test encodes the exact published INPUTS of a recognised worked example and
asserts our implementation reproduces the published OUTPUT within a stated
tolerance.  Tests are explicitly labelled:

* PUBLISHED-VALIDATED  -- the asserted number is sourced from a published
  worked example (the standard's own appendix example or a published reference
  implementation) AND independently hand-verified here.
* REGRESSION-ANCHOR    -- no external published numeric output could be sourced
  for these exact inputs, so we anchor our own current output to catch silent
  drift.  These are NOT claims of agreement with an external authority.

Sources
-------
* ASME B31G-1991 Nonmandatory Appendix A, worked Example #1, as reproduced by
  the Kiefner & Vieth ``CRVL.BAS`` program and the CRAN ``pipenostics`` package
  (``b31crvl`` reference example).
* DNV-RP-F101 (Oct 2010) Section 8.2 single-defect capacity equation, worked
  examples as reproduced by ``pipenostics`` (``dnvpf`` reference examples 1 & 2),
  each also hand-verified against the closed-form capacity equation.

See docs/domains/ffs-published-examples-2026-06-28.md for the source register
and the validated-vs-regression status of every case below.
"""

import math

import pytest

from digitalmodel.asset_integrity.corroded_pipe import (
    b31g_original,
    modified_b31g,
    rstreng_effective_area,
)
from digitalmodel.asset_integrity.dnv_rp_f101 import dnv_f101_single_defect

# ---------------------------------------------------------------------------
# Example A -- ASME B31G-1991 Appendix A / CRVL.BAS Example #1 (Original B31G)
#
#   D = 30 in, t = 0.438 in, SMYS = 52,000 psi (API 5L X52),
#   defect depth d = 0.100 in, axial length L = 7.5 in,
#   design factor F = 0.72, MAOP = 910 psi.
#
# Published output (CRVL.BAS / pipenostics b31crvl):
#   Design pressure = 910-psi-safe; "Design pressure = Safe pressure = 1093 PSI";
#   "maximum allowed corrosion depth = 0.2490 inch" at the 910 psi MAOP.
# ---------------------------------------------------------------------------
B31G_D, B31G_T, B31G_SMYS = 30.0, 0.438, 52_000.0
B31G_L, B31G_F, B31G_MAOP = 7.5, 0.72, 910.0


def test_b31g_original_crvl_example1_design_safe_pressure_published():
    """PUBLISHED-VALIDATED: CRVL.BAS Example #1 design/safe pressure = 1093 psi.

    CRVL reports the safe operating pressure as the B31G estimated failure
    pressure factored by the design factor F, capped at the intact design
    pressure ``2*SMYS*t/D*F``.  For this shallow defect the cap governs, so the
    published "Design pressure = Safe pressure = 1093 PSI".
    """
    r = b31g_original(B31G_D, B31G_T, 0.100, B31G_L, B31G_SMYS)
    intact_design = 2.0 * B31G_SMYS * B31G_T / B31G_D * B31G_F
    safe_crvl = min(r.failure_pressure_psi * B31G_F, intact_design)
    # Published value rounded to 1093 psi; we reproduce 1093.25 psi.
    assert safe_crvl == pytest.approx(1093.0, abs=1.0)
    # The B31G intermediate factor A = 0.893*L/sqrt(D*t) = 1.847 (published).
    A = 0.893 * B31G_L / math.sqrt(B31G_D * B31G_T)
    assert A == pytest.approx(1.847, abs=0.005)


def test_b31g_original_crvl_example1_allowed_depth_published():
    """PUBLISHED-VALIDATED: CRVL Example #1 max allowed depth 0.2490 in @ 910 psi.

    At the published maximum allowable corrosion depth (0.2490 in) the B31G
    failure pressure factored by F must equal the MAOP (910 psi).  This exercises
    the corroded failure-pressure formula at a non-trivial (deeper) defect.
    """
    r = b31g_original(B31G_D, B31G_T, 0.2490, B31G_L, B31G_SMYS)
    safe_at_allowed_depth = r.failure_pressure_psi * B31G_F
    # We reproduce 910.9 psi vs the published 910 psi MAOP (0.1 % high; the
    # published depth is itself rounded to 4 dp).
    assert safe_at_allowed_depth == pytest.approx(B31G_MAOP, abs=3.0)


# ---------------------------------------------------------------------------
# Example B -- DNV-RP-F101 (Oct 2010) Section 8.2 single-defect capacity.
#
# The capacity equation is unit-consistent, so these SI examples are run in
# mm / N-per-mm^2 (MPa); the returned ``capacity_pressure_psi`` field then
# carries MPa for these two cases (documented below).
#
# Source: DNV-RP-F101 Sec 8.2 worked examples, reproduced by pipenostics dnvpf;
# both also hand-verified here against P = (2 t f_u/(D-t))(1-d/t)/(1-(d/t)/Q),
# Q = sqrt(1 + 0.31 (L/sqrt(D t))^2).
# ---------------------------------------------------------------------------
def test_dnv_f101_single_defect_example1_published_mpa():
    """PUBLISHED-VALIDATED: DNV-RP-F101 Sec 8.2 Example 1 -> 15.86626 MPa.

    D=812.8 mm, t=19.1 mm, d=13.4 mm, L=203.2 mm, f_u(SMTS)=530.9 MPa.
    NOTE: inputs/outputs are in mm / MPa for this SI example; the
    ``*_psi``-named fields therefore carry MPa here (formula is unit-agnostic).
    """
    res = dnv_f101_single_defect(812.8, 19.1, 13.4, 203.2, 530.9)
    assert res.capacity_pressure_psi == pytest.approx(15.86626, rel=1e-5)


def test_dnv_f101_single_defect_example2_published_mpa():
    """PUBLISHED-VALIDATED: DNV-RP-F101 Sec 8.2 Example 2 -> 34.01183 MPa.

    D=219.0 mm, t=14.5 mm, d=9.0 mm, L=200.0 mm, f_u(SMTS)=455.1 MPa.
    Inputs/outputs in mm / MPa (see Example 1 note).
    """
    res = dnv_f101_single_defect(219.0, 14.5, 9.0, 200.0, 455.1)
    assert res.capacity_pressure_psi == pytest.approx(34.01183, rel=1e-5)


# ---------------------------------------------------------------------------
# Regression anchors -- same published geometry, methods for which no external
# published numeric output for these exact inputs could be sourced.  These pin
# our CURRENT output so a behaviour change is caught; they are NOT external
# validations.
# ---------------------------------------------------------------------------
def test_modified_b31g_crvl_geometry_regression_anchor():
    """REGRESSION-ANCHOR: Modified B31G on the CRVL Example #1 geometry.

    No external published Modified-B31G (0.85 dL) output was sourced for these
    exact inputs; value is our own current output (hand-traceable: flow=62 ksi,
    A/A0=0.85 d/t, two-part Folias).
    """
    r = modified_b31g(B31G_D, B31G_T, 0.100, B31G_L, B31G_SMYS)
    assert r.failure_pressure_psi == pytest.approx(1624.68, rel=1e-4)


def test_rstreng_uniform_profile_regression_anchor():
    """REGRESSION-ANCHOR: RSTRENG, uniform-depth profile, CRVL Example #1 geom.

    A uniform (rectangular) metal-loss profile gives effective A/A0 = d/t; no
    external published RSTRENG output was sourced for these exact inputs.
    """
    r = rstreng_effective_area(B31G_D, B31G_T, [0.0, B31G_L], [0.100, 0.100], B31G_SMYS)
    assert r.area_ratio == pytest.approx(0.100 / B31G_T, rel=1e-9)
    assert r.failure_pressure_psi == pytest.approx(1587.44, rel=1e-4)
