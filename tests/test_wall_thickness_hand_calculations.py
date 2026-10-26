# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
# ABOUTME: Independent first-principles stress points for four screening envelopes
# ABOUTME: Checks report inputs, both fibres, utilisation and a bending boundary

"""Hand calculations for the implemented moment-tension screening envelopes.

Primary basis: the independent derivations below, with synthetic inputs only.
No standard document or earlier worked example supplies the expected values.

Common specimen: D = 500.0 mm, t = 10.0 mm, d = 480.0 mm, yield stress
300.0 MPa, ultimate stress 450.0 MPa, internal pressure 6.0 MPa and external
pressure 2.0 MPa. Corrosion allowance and fabrication tolerance are zero;
the material is seamless. All four points therefore use nominal wall.

First-principles derivation (SI units unless a stress is marked MPa):

* Integrate dA = r dr dtheta over the annulus, 0.24 <= r <= 0.25:
  A = pi*(0.25**2 - 0.24**2) = 49*pi/10000 m**2.
* Integrate y**2 dA, with y = r*sin(theta), over that annulus:
  I = pi*(0.25**4 - 0.24**4)/4; Z = I/0.25
  = 58849*pi/100000000 m**3.
* In the thin-wall membrane approximation, balance the diametral pressure
  force on a unit length: 2*t*h = (p_i-p_e)*d. Thus h = 96.0 MPa.
  Radial stress and shear are omitted. The pressure difference acts on the
  bore projection; this is not a thick-cylinder solution.
* Uniform axial stress gives T = integral(a dA) = a*A. Linear elastic
  bending gives M = integral(k*y**2 dA) = k*I, so the extreme-fibre stress
  is b = k*D/2 = M/Z. The two longitudinal stresses are l = a +/- b.
  T here is the prescribed wall axial resultant, with no extra end-cap
  force; conversion from system effective tension is outside this check.
* For principal stresses (h, l, 0), subtract their mean to obtain the
  deviatoric stresses s_i. Normalising distortional energy to uniaxial
  yield gives v**2 = (3/2)*sum(s_i**2) = h**2-h*l+l**2
  = (l-h/2)**2 + 3*h**2/4. Hence, in MPa,
  v_plus = hypot(102+b, 48*sqrt(3)) and
  v_minus = hypot(102-b, 48*sqrt(3)) when a = 150.0 MPa.
  The larger fibre value divided by the stated allowable is utilisation.
* For that fixed axial stress, solving v_plus = S for positive bending
  gives b_limit = sqrt(S**2 - 6912) - 102 MPa and
  M_limit = Z*b_limit*1e6 N.m. The other fibre is also checked below.

Independent teaching reference: "Yield and Plastic Flow", dated 2001-10-15,
MIT OpenCourseWare, Mechanics of Materials, "Multiaxial stress states",
page 5, equation (2) and the uniaxial-yield reduction on the same page:
https://ocw.mit.edu/courses/3-11-mechanics-of-materials-fall-1999/resources/mit3_11f99_yield/
This supports the deviatoric-energy criterion and its tensile-yield
normalisation for ductile material. It supplies none of the four screening
multipliers below.

Allowable-coefficient provenance remains OPEN for each report route:

* RP 1111: 0.72, hence S = 216.0 MPa, is a regression input from the
  report's Normal Operating setting. Independent applicability to this
  equivalent-stress screening criterion has not been established.
* RP 2RD: 0.667, hence S = 200.1 MPa, is a regression input from the
  report's Operating setting. Neither its rounding from 2/3 nor its use
  for equivalent stress has an independently confirmed basis here.
* STD 2RD: 0.8, hence S = 240.0 MPa, is a regression input from the
  report's SLS setting. A pressure or linear/cosine interaction factor
  does not by itself establish applicability to a von Mises allowable.
* F101: 0.831, hence S = 249.3 MPa, is a regression input from the
  report's Low Safety Class setting. The label alone does not establish
  an independent basis for its use as an equivalent-stress multiplier.

Writing S = yield/n gives n = 1/f for a multiplier f; that identity cannot
select or justify f. The teaching reference and the first-principles stress
derivation therefore do not close these coefficient-provenance gaps. An
independent reference with the applicable loading criterion and condition
is needed for each multiplier before claiming allowable validation.

Public applicability cross-checks (inspected 2026-10-26):

* RP 1111: the public code-check description's "API RP 1111 CLD" and
  "API RP 1111 BEP" sections describe load interactions, rather than this
  report's equivalent-stress allowable:
  https://www.orcina.com/webhelp/OrcaFlex/Content/html/Codechecks,APIRP1111.htm
* RP 2RD: the public description's "Results" section includes radial
  stress and evaluates longitudinal stress at mid-wall. Its allowable
  expression does not establish this report's rounded multiplier with
  outer-fibre bending and omitted radial stress:
  https://www.orcina.com/webhelp/OrcaFlex/Content/html/Codechecks,APIRP2RD.htm
* STD 2RD: the public description's "API STD 2RD method 1" and "method 2"
  sections define load interactions. Their design factors do not establish
  an equivalent-stress multiplier for this screening calculation:
  https://www.orcina.com/webhelp/OrcaFlex/Content/html/Codechecks,APISTD2RD.htm
* F101: the public description's "Results" section distinguishes load-
  and displacement-controlled combined-load criteria. Neither establishes
  this report's single equivalent-stress multiplier:
  https://www.orcina.com/webhelp/OrcaFlex/Content/html/Codechecks,DNVSTF101.htm

These are public implementation descriptions, not inspected standards.
The four coefficient-applicability gaps above therefore remain OPEN.

The report's load fractions and factors identify the result of the
implementation, not a requirement of a design code. The RP 1111 route is
screening-only while its combined check is unregistered. These calculations
do not reconcile the report with the strategy checks: in particular, the
STD 2RD linear/cosine interactions and the F101 simplified combined form
are different criteria and are not validated here. No report edition is asserted.
"""

import math

import pytest

from digitalmodel.structural.analysis import wall_thickness_mt_report as report
from digitalmodel.structural.analysis.wall_thickness import (
    DesignCode,
    PipeGeometry,
    PipeMaterial,
)


def _assert_hand_calculation(monkeypatch, code, moment_nm, factor, bending_mpa):
    """Observe the real report calculation without substituting its result."""
    observed = {}
    original = report._compute_key_points

    def observe(T_y, M_p, A, Z, sigma_h, sigma_allow):
        points = original(T_y, M_p, A, Z, sigma_h, sigma_allow)
        observed.update(A=A, Z=Z, h=sigma_h, allowable=sigma_allow, points=points)
        return points

    monkeypatch.setattr(report, "_compute_key_points", observe)
    html = report.generate_mt_report(
        geometry=PipeGeometry(
            outer_diameter=0.5,
            wall_thickness=0.01,
            corrosion_allowance=0.0,
            fabrication_tolerance=0.0,
        ),
        material=PipeMaterial(grade="synthetic", smys=300e6, smts=450e6),
        internal_pressure=6e6,
        external_pressure=2e6,
        code=code,
    )

    # Independent annulus integrals and force balance from the derivation.
    area = 49 * math.pi / 10000
    modulus = 58849 * math.pi / 100000000
    tension_n = 735000 * math.pi
    allowable_mpa = 300 * factor
    assert observed["A"] == pytest.approx(area, rel=1e-12)
    assert observed["Z"] == pytest.approx(modulus, rel=1e-12)
    assert observed["h"] == pytest.approx(96e6, rel=1e-12)
    assert observed["allowable"] == pytest.approx(allowable_mpa * 1e6, rel=1e-12)

    label = "T = 0.5 T_y + M = 0.5 M_p"
    point, = (p for p in observed["points"] if p["label"] == label)
    assert point["T_kN"] == pytest.approx(tension_n / 1e3, rel=1e-12)
    assert point["M_kNm"] == pytest.approx(moment_nm / 1e3, rel=1e-12)
    assert point["T_kN"] * 1e3 / observed["A"] == pytest.approx(150e6, rel=1e-12)
    assert point["M_kNm"] * 1e3 / observed["Z"] == pytest.approx(
        bending_mpa * 1e6, rel=1e-12,
    )

    # Use the independently reduced deviatoric-energy form, not report output.
    vm_plus = math.hypot(102 + bending_mpa, 48 * math.sqrt(3))
    vm_minus = math.hypot(102 - bending_mpa, 48 * math.sqrt(3))
    expected_util = max(vm_plus, vm_minus) / allowable_mpa
    assert point["util"] == pytest.approx(expected_util, rel=1e-12)
    assert point["status"] == "FAIL"  # Each chosen stress point exceeds its limit.
    assert (
        f"<td>{label}</td><td>{tension_n / 1e3:.1f}</td>"
        f"<td>{moment_nm / 1e3:.1f}</td><td>0.500</td><td>0.500</td>"
        f"<td>{expected_util:.3f}</td>"
    ) in html

    # Check the same point with reversed tension, so the other fibre governs.
    # For a = -150.0 MPa, l-h/2 = -198 +/- b, independently of the report.
    reversed_util = math.hypot(198 + bending_mpa, 48 * math.sqrt(3)) / allowable_mpa
    for moment_sign in (-1, 1):
        assert report._von_mises_util_single(
            -tension_n, moment_sign * moment_nm, observed["A"], observed["Z"],
            observed["h"], observed["allowable"],
        ) == pytest.approx(reversed_util, rel=1e-12)

    # An independent analytical boundary also anchors the traced envelope.
    limit_bending_mpa = math.sqrt(allowable_mpa**2 - 6912) - 102
    expected_limit_nm = modulus * limit_bending_mpa * 1e6
    tensions, moments = report._trace_envelope(
        observed["A"], observed["Z"], observed["h"], 300e6, factor,
        tension_n - 1e5, tension_n + 1e5, 2 * moment_nm, n_points=2,
    )
    assert tensions[1] == pytest.approx(tension_n, rel=1e-12)
    # Bisection stops at a bracket width below 100 N.m: midpoint error < 50 N.m.
    assert moments[1] == pytest.approx(expected_limit_nm, abs=50, rel=0)
    boundary_vm_plus = math.hypot(102 + limit_bending_mpa, 48 * math.sqrt(3))
    boundary_vm_minus = math.hypot(102 - limit_bending_mpa, 48 * math.sqrt(3))
    assert max(boundary_vm_plus, boundary_vm_minus) == pytest.approx(
        allowable_mpa, rel=1e-12,
    )


def test_rp_1111_screening_hand_calculation(monkeypatch):
    """RP 1111 screening point, derived using the common specimen above.

    Axial yield gives T_y = 300e6*A, so T = T_y/2 = 735000*pi N
    (2309.1 kN) and a = 150.0 MPa. Integrating a fully yielded annulus,
    M_p = yield*integral(abs(y) dA) = yield*4*(R_o**3-R_i**3)/3
    = 720400.0 N.m; the selected M = M_p/2 = 360.2 kN.m.
    Thus b = 360200/Z/1e6 = 36020000/(58849*pi) = 194.8 MPa.
    Hoop h = 96.0 MPa; l_plus = 344.8 and l_minus = -44.8 MPa.
    The deviatoric-energy calculation gives v_plus = 308.3 and
    v_minus = 124.6 MPa. S = 0.72*300 = 216.0 MPa; u = v_plus/S.
    At this T, the independently solved bending boundary is 180.0 kN.m.
    Stress/utilisation relative tolerance is 1e-12; boundary tolerance is
    50.0 N.m. This is a screening result, not a registered combined check.
    """
    _assert_hand_calculation(
        monkeypatch, DesignCode.API_RP_1111, 360200, 0.72,
        36020000 / (58849 * math.pi),
    )


def test_rp_2rd_screening_hand_calculation(monkeypatch):
    """RP 2RD screening point, derived using the common specimen above.

    T = (300e6*A)/2 = 735000*pi N (2309.1 kN), so a = 150.0 MPa.
    The fully yielded annulus integral gives M_p = 300e6*4*
    (0.25**3-0.24**3)/3 = 720400.0 N.m; M = M_p/2 = 360.2 kN.m.
    Therefore b = 36020000/(58849*pi) = 194.8 MPa and h = 96.0 MPa.
    The fibres have l_plus = 344.8 and l_minus = -44.8 MPa;
    v_plus = 308.3 and v_minus = 124.6 MPa from deviatoric energy.
    S = 0.667*300 = 200.1 MPa; u = v_plus/S. The 0.667 factor
    identifies the report's screening calculation, not the exact 2/3
    factor in the pressure strategy. The bending boundary is 147.9 kN.m.
    Stress/utilisation relative tolerance is 1e-12; boundary tolerance is
    50.0 N.m. No registered equivalent-stress check is inferred.
    """
    _assert_hand_calculation(
        monkeypatch, DesignCode.API_RP_2RD, 360200, 0.667,
        36020000 / (58849 * math.pi),
    )


def test_std_2rd_screening_hand_calculation(monkeypatch):
    """STD 2RD von Mises screening point, separate from linear/cosine checks.

    T = (300e6*A)/2 = 735000*pi N (2309.1 kN), so a = 150.0 MPa.
    For a thin ring at R = (D-t)/2 = 0.245 m, integrating abs(y) dA
    with dA = t*R dtheta gives M_p = 4*yield*t*R**2
    = 720300.0 N.m. Thus M = M_p/2 = 360.2 kN.m (rounded), and
    b = 360150/Z/1e6 = 36015000/(58849*pi) = 194.8 MPa.
    Hoop h = 96.0 MPa; l_plus = 344.8 and l_minus = -44.8 MPa.
    Deviatoric energy gives v_plus = 308.2 and v_minus = 124.6 MPa.
    S = 0.8*300 = 240.0 MPa; u = v_plus/S. The independently solved
    von Mises bending boundary is 227.7 kN.m at this T.
    Stress/utilisation relative tolerance is 1e-12; boundary tolerance is
    50.0 N.m. The linear/cosine strategy criteria are outside this check.
    """
    _assert_hand_calculation(
        monkeypatch, DesignCode.API_STD_2RD, 360150, 0.8,
        36015000 / (58849 * math.pi),
    )


def test_f101_screening_hand_calculation(monkeypatch):
    """F101 von Mises screening point, separate from the simplified strategy.

    With zero corrosion allowance and seamless material, the wall is
    10.0 mm and the fabrication strength multiplier is unity.
    T = (300e6*A)/2 = 735000*pi N (2309.1 kN), so a = 150.0 MPa.
    The thin-ring integral of abs(y) dA gives M_p = 4*300e6*0.01*
    0.245**2 = 720300.0 N.m; M = M_p/2 = 360.2 kN.m (rounded).
    Thus b = 36015000/(58849*pi) = 194.8 MPa, h = 96.0 MPa,
    l_plus = 344.8 and l_minus = -44.8 MPa. Deviatoric energy gives
    v_plus = 308.2 and v_minus = 124.6 MPa. S = 0.831*300 = 249.3 MPa;
    u = v_plus/S. The bending boundary is independently 245.9 kN.m.
    Stress/utilisation relative tolerance is 1e-12; boundary tolerance is
    50.0 N.m. This checks the screening envelope only; the simplified
    combined form remains not checked against the standard's text.
    """
    _assert_hand_calculation(
        monkeypatch, DesignCode.DNV_ST_F101, 360150, 0.831,
        36015000 / (58849 * math.pi),
    )
