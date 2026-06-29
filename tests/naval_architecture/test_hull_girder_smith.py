# ABOUTME: Golden tests for the Smith incremental-iterative hull-girder ultimate
# ABOUTME: method — stocky->plastic/first-yield limit, buckling reduction, convergence.
import math

from digitalmodel.naval_architecture.hull_girder_smith import (
    CODE_REFERENCE,
    HullGirderElement,
    section_centroid_m,
    smith_ultimate,
    stiffened_plate_element,
)
from digitalmodel.structural.structural_analysis.buckling import (
    PlateBucklingAnalyzer,
)
from digitalmodel.structural.structural_analysis.models import (
    PlateGeometry,
    STEEL_AH36,
    STEEL_GRADE_A,
)
from digitalmodel.structural.structural_analysis.panel_buckling import (
    StiffenedPanelBucklingAnalyzer,
    StiffenedPanelGeometry,
    StiffenerGeometry,
)

FY = 355.0  # AH36 yield (MPa)
E = 206000.0  # MPa
DEPTH = 12.0  # box-girder depth (m)


# ---------------------------------------------------------------------------
# Box-girder test section (doubly symmetric -> centroid = plastic NA = D/2)
# ---------------------------------------------------------------------------
def build_box(sigma_u_deck=None, residual=0.5):
    """A symmetric box-girder cross-section as a list of Smith elements.

    Deck + bottom flanges (equal area) plus two side walls discretised over
    symmetric heights. With ``sigma_u_deck`` the deck flange becomes
    buckling-governed; everything else stays non-buckling (sigma_u = fy).
    """
    su_deck = FY if sigma_u_deck is None else sigma_u_deck
    a_flange_each = 0.15  # m^2 (deck/bottom split in two)
    side_positions = [1.0, 3.0, 5.0, 7.0, 9.0, 11.0]
    a_side_each = 0.20 / (2 * len(side_positions))  # two walls

    elements = []
    for k in range(2):
        elements.append(
            HullGirderElement(
                area_m2=a_flange_each,
                z_m=DEPTH,
                fy_mpa=FY,
                sigma_u_mpa=su_deck,
                youngs_modulus_mpa=E,
                post_buckling_residual=residual,
                name=f"deck{k}",
            )
        )
    for k in range(2):
        elements.append(
            HullGirderElement(
                area_m2=a_flange_each,
                z_m=0.0,
                fy_mpa=FY,
                sigma_u_mpa=FY,
                youngs_modulus_mpa=E,
                name=f"bottom{k}",
            )
        )
    for wall in range(2):
        for z in side_positions:
            elements.append(
                HullGirderElement(
                    area_m2=a_side_each,
                    z_m=z,
                    fy_mpa=FY,
                    sigma_u_mpa=FY,
                    youngs_modulus_mpa=E,
                    name=f"side{wall}",
                )
            )
    return elements


# --- Independent analytic references (computed from the element list) --------
def analytic_centroid(els):
    a = sum(e.area_m2 for e in els)
    return sum(e.area_m2 * e.z_m for e in els) / a


def analytic_inertia(els):
    zc = analytic_centroid(els)
    return sum(e.area_m2 * (e.z_m - zc) ** 2 for e in els)


def analytic_My(els):
    """First-yield moment fy*Z (kN.m), Z = I/c (lumped)."""
    zc = analytic_centroid(els)
    inertia = analytic_inertia(els)
    c = max(abs(e.z_m - zc) for e in els)
    return FY * (inertia / c) * 1.0e3


def analytic_Mp(els):
    """Fully-plastic moment fy*sum(A|z-z_pna|) (kN.m). z_pna = centroid here
    (uniform fy, doubly symmetric section)."""
    zc = analytic_centroid(els)
    return FY * sum(e.area_m2 * abs(e.z_m - zc) for e in els) * 1.0e3


def kappa_yield():
    """First-yield curvature of the extreme fibre (1/m)."""
    return FY / (E * (DEPTH / 2.0))


# ---------------------------------------------------------------------------
# GOLDEN 1a — elastic march reproduces M = E*I*kappa, NA at centroid
# ---------------------------------------------------------------------------
def test_elastic_march_matches_beam_theory():
    els = build_box()
    inertia = analytic_inertia(els)
    kappa = 0.1 * kappa_yield()  # well below first yield -> elastic
    res = smith_ultimate(els, max_curvature=kappa, num_steps=1)
    pt = res.hogging_curve[-1]
    assert math.isclose(pt.curvature, kappa, rel_tol=1e-12)
    assert math.isclose(pt.neutral_axis_m, DEPTH / 2.0, abs_tol=1e-6)
    assert math.isclose(pt.moment_kn_m, E * inertia * kappa * 1.0e3, rel_tol=1e-6)


# ---------------------------------------------------------------------------
# GOLDEN 1b — first-yield moment equals fy*Z
# ---------------------------------------------------------------------------
def test_first_yield_moment_matches_section_modulus():
    els = build_box()
    res = smith_ultimate(els, max_curvature=kappa_yield(), num_steps=1)
    pt = res.hogging_curve[-1]
    assert math.isclose(pt.moment_kn_m, analytic_My(els), rel_tol=1e-6)


# ---------------------------------------------------------------------------
# GOLDEN 1c (MAKE-OR-BREAK) — stocky section reduces to the fully-plastic moment
# ---------------------------------------------------------------------------
def test_stocky_section_reduces_to_plastic_moment():
    els = build_box()  # no buckling: sigma_u = fy everywhere
    mp = analytic_Mp(els)
    res = smith_ultimate(els, max_curvature=50.0 * kappa_yield(), num_steps=300)
    # Hogging and sagging both converge to the analytic plastic moment.
    assert math.isclose(res.ultimate_moment_hogging_kn_m, mp, rel_tol=1e-4)
    assert math.isclose(res.ultimate_moment_sagging_kn_m, mp, rel_tol=1e-4)
    # Symmetric section -> hog == sag.
    assert math.isclose(
        res.ultimate_moment_hogging_kn_m, res.ultimate_moment_sagging_kn_m, rel_tol=1e-6
    )
    # Plastic moment exceeds first yield (shape factor > 1) -> march captured
    # plastification, not just the elastic response.
    assert mp > analytic_My(els)


# ---------------------------------------------------------------------------
# GOLDEN 2 — deck buckling reduces the SAGGING ultimate (deck in compression)
# ---------------------------------------------------------------------------
def test_deck_buckling_reduces_sagging_ultimate():
    stocky_mp = analytic_Mp(build_box())
    els = build_box(sigma_u_deck=0.6 * FY, residual=0.6)
    res = smith_ultimate(els, max_curvature=10.0 * kappa_yield(), num_steps=400)

    # Sagging (deck in compression) is reduced below the stocky plastic moment.
    assert res.ultimate_moment_sagging_kn_m < stocky_mp
    # Hogging (deck in tension, keel compression unbuckled) ~ stocky plastic.
    assert math.isclose(res.ultimate_moment_hogging_kn_m, stocky_mp, rel_tol=2e-3)
    # Reduction is magnitude-sane (deck carries ~40% of Mp; cut to 0.6*fy +
    # post-buckling softening -> sagging lands well inside these bounds).
    assert 0.5 * stocky_mp < res.ultimate_moment_sagging_kn_m < 0.97 * stocky_mp
    # Sagging now strictly weaker than hogging (the physical asymmetry).
    assert res.ultimate_moment_sagging_kn_m < res.ultimate_moment_hogging_kn_m


# ---------------------------------------------------------------------------
# GOLDEN 3 — curvature-step refinement converges M_U
# ---------------------------------------------------------------------------
def test_curvature_refinement_converges():
    kmax = 10.0 * kappa_yield()
    coarse = smith_ultimate(
        build_box(sigma_u_deck=0.6 * FY, residual=0.6), max_curvature=kmax, num_steps=50
    )
    fine = smith_ultimate(
        build_box(sigma_u_deck=0.6 * FY, residual=0.6),
        max_curvature=kmax,
        num_steps=200,
    )
    finer = smith_ultimate(
        build_box(sigma_u_deck=0.6 * FY, residual=0.6),
        max_curvature=kmax,
        num_steps=800,
    )

    s_coarse = coarse.ultimate_moment_sagging_kn_m
    s_fine = fine.ultimate_moment_sagging_kn_m
    s_finer = finer.ultimate_moment_sagging_kn_m

    d_coarse = abs(s_coarse - s_finer) / s_finer
    d_fine = abs(s_fine - s_finer) / s_finer
    assert d_fine <= d_coarse  # refinement converges
    assert d_fine < 2e-3  # fine grid is within 0.2% of the finest


# ---------------------------------------------------------------------------
# Element load-shortening curve unit checks
# ---------------------------------------------------------------------------
def test_load_shortening_curve_branches():
    el = HullGirderElement(
        area_m2=1.0,
        z_m=0.0,
        fy_mpa=300.0,
        sigma_u_mpa=180.0,
        youngs_modulus_mpa=200000.0,
        post_buckling_residual=0.5,
    )
    eps_y = 300.0 / 200000.0
    # Tension: elastic then capped at +fy.
    assert math.isclose(el.stress(0.5 * eps_y), 200000.0 * 0.5 * eps_y, rel_tol=1e-12)
    assert math.isclose(el.stress(0.1), 300.0, rel_tol=1e-12)
    # Compression elastic (below buckling capacity 180 MPa).
    assert math.isclose(el.stress(-1.0e-4), -200000.0 * 1.0e-4, rel_tol=1e-12)
    # Compression post-buckling: magnitude between residual*cap and cap.
    s = el.stress(-0.1)
    assert -180.0 < s < -90.0
    assert el.buckles is True

    # Non-buckling element: compression plateaus at -fy (perfectly plastic).
    hard = HullGirderElement(
        area_m2=1.0,
        z_m=0.0,
        fy_mpa=300.0,
        sigma_u_mpa=300.0,
        youngs_modulus_mpa=200000.0,
    )
    assert hard.buckles is False
    assert math.isclose(hard.stress(-0.1), -300.0, rel_tol=1e-12)


# ---------------------------------------------------------------------------
# Factory reuse — sigma_u comes from the validated panel solver (no reimpl.)
# ---------------------------------------------------------------------------
def test_stiffened_plate_element_uses_panel_solver():
    stiff = StiffenerGeometry(
        web_height=300.0,
        web_thickness=12.0,
        flange_width=150.0,
        flange_thickness=15.0,
        spacing=800.0,
        section_type="tee",
    )
    panel = StiffenedPanelGeometry(
        plate_length=3200.0, plate_thickness=14.0, stiffener=stiff
    )
    analyzer = StiffenedPanelBucklingAnalyzer(STEEL_AH36)
    expected = analyzer.check_panel(panel, sigma_x=STEEL_AH36.yield_strength)

    el = stiffened_plate_element(panel, STEEL_AH36, z_m=15.0)
    assert math.isclose(el.sigma_u_mpa, expected.critical_stress, rel_tol=1e-12)
    assert math.isclose(el.fy_mpa, STEEL_AH36.yield_strength, rel_tol=1e-12)
    # sigma_u is buckling-reduced below yield for this slender panel.
    assert el.sigma_u_mpa < STEEL_AH36.yield_strength
    assert el.area_m2 > 0.0


# ---------------------------------------------------------------------------
# Result metadata
# ---------------------------------------------------------------------------
def test_result_carries_code_reference_and_na_migration():
    els = build_box(sigma_u_deck=0.6 * FY, residual=0.6)
    res = smith_ultimate(els, max_curvature=10.0 * kappa_yield(), num_steps=100)
    assert res.code_reference == CODE_REFERENCE == "IACS UR S11A / Smith method"
    # Neutral axis starts at the centroid and migrates away from the buckled
    # deck on the sagging branch (toward the still-effective keel).
    centroid = section_centroid_m(els)
    na_zero = res.sagging_curve[0].neutral_axis_m
    na_peak = res.sagging_curve[-1].neutral_axis_m
    assert math.isclose(na_zero, centroid, abs_tol=1e-6)
    assert na_peak < centroid  # NA drops toward the keel (tension side)


# ===========================================================================
# Independently-verified golden cases (Grade A: fy=235 MPa, E=206 GPa).
# Reproduced by an external check of the Smith method; high confidence.
# ===========================================================================
GA_FY = 235.0
GA_E = 206000.0
GA_D = 10.0  # flange separation / depth (m)
GA_AF = 0.25  # flange area B*tf = 10 m * 25 mm (m^2)


def _ga_kappa_yield():
    return GA_FY / (GA_E * (GA_D / 2.0))


# CASE 1 — stocky two-flange box reduces to first-yield = fully-plastic.
# Shape factor exactly 1.0 (webs shear-only -> no bending element): Z = A_f*D =
# 2.5 m^3, M_U = fy*Z = 587,500 kN.m. THE make-or-break correctness check.
def test_case1_two_flange_box_reduces_to_first_yield():
    els = [
        HullGirderElement(GA_AF, 0.0, GA_FY, GA_FY, GA_E),
        HullGirderElement(GA_AF, GA_D, GA_FY, GA_FY, GA_E),
    ]
    res = smith_ultimate(els, max_curvature=50.0 * _ga_kappa_yield(), num_steps=300)
    assert math.isclose(res.ultimate_moment_hogging_kn_m, 587_500.0, rel_tol=1e-5)
    assert math.isclose(res.ultimate_moment_sagging_kn_m, 587_500.0, rel_tol=1e-5)


# CASE 2 — stocky single-cell box with webs: M-kappa rises from the first-yield
# moment fy*Z = 783,333 kN.m to a plateau at the fully-plastic moment
# fy*Zp = 881,250 kN.m (shape factor 1.125). Webs discretised into point areas;
# the lumped section converges to the continuous I=16.6667 / Zp=3.75.
def test_case2_single_cell_box_shape_factor_reserve():
    n_per_web = 100
    tw, t_f, breadth = 0.025, 0.025, 10.0
    els = [
        HullGirderElement(breadth * t_f, 0.0, GA_FY, GA_FY, GA_E),
        HullGirderElement(breadth * t_f, GA_D, GA_FY, GA_FY, GA_E),
    ]
    dz = GA_D / n_per_web
    a_web = tw * dz
    for _wall in range(2):
        for i in range(n_per_web):
            els.append(HullGirderElement(a_web, (i + 0.5) * dz, GA_FY, GA_FY, GA_E))
    # First yield at kappa_y -> fy*Z.
    res_y = smith_ultimate(els, max_curvature=_ga_kappa_yield(), num_steps=1)
    assert math.isclose(res_y.hogging_curve[-1].moment_kn_m, 783_333.0, rel_tol=1e-3)
    # Fully-plastic plateau -> fy*Zp, with shape-factor reserve > first yield.
    res_p = smith_ultimate(els, max_curvature=50.0 * _ga_kappa_yield(), num_steps=300)
    assert math.isclose(res_p.ultimate_moment_hogging_kn_m, 881_250.0, rel_tol=1e-3)
    assert res_p.ultimate_moment_hogging_kn_m > res_y.hogging_curve[-1].moment_kn_m


# CASE 3 — slender compression flange (plate b=800, t=20, a=2400, k=4) buckles:
# Johnson-Ostenfeld sigma_u = 205.34 MPa (0.874*fy) -> sagging M_U = sigma_u*Z =
# 513,347 kN.m, a 12.6% knockdown below fy*Z. (Published-formula-derived, not
# published-validated.) sigma_u taken from the validated DNV-RP-C201 plate
# solver; the pure buckling cap (residual=1) gives the first-collapse plateau.
def test_case3_slender_flange_buckling_knockdown():
    analyzer = PlateBucklingAnalyzer(STEEL_GRADE_A)
    plate = PlateGeometry(length=2400.0, width=800.0, thickness=20.0)
    sigma_u = analyzer.johnson_ostenfeld(analyzer.elastic_buckling_stress(plate))
    assert math.isclose(sigma_u, 205.34, rel_tol=1e-3)
    assert math.isclose(sigma_u / GA_FY, 0.874, rel_tol=2e-3)

    # Deck (z=D) is the compression flange in sagging; keel stays at fy.
    els = [
        HullGirderElement(GA_AF, 0.0, GA_FY, GA_FY, GA_E),
        HullGirderElement(
            GA_AF, GA_D, GA_FY, sigma_u, GA_E, post_buckling_residual=1.0
        ),
    ]
    res = smith_ultimate(els, max_curvature=10.0 * _ga_kappa_yield(), num_steps=400)
    # Hogging (deck in tension) keeps the full fy*Z.
    assert math.isclose(res.ultimate_moment_hogging_kn_m, 587_500.0, rel_tol=1e-5)
    # Sagging knocked down to sigma_u*Z = 513,347 kN.m.
    assert math.isclose(res.ultimate_moment_sagging_kn_m, 513_347.0, rel_tol=1e-3)
    assert res.ultimate_moment_sagging_kn_m < 587_500.0
