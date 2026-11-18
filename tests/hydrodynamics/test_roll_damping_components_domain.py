"""Domain, regression and provenance tests for roll_damping_components.

These cover the second-provider review of PR #3925: the diagnostic
``as_printed`` coefficient, validation of optional keel inputs, geometric
limits on explicit bilge radii, rejection of inputs that drive a dissipation
coefficient negative, sign and factor regressions at non-zero OG, and the
structured citation sidecar. Equation numbers refer to Chakrabarti (2001),
Ocean Engineering 28, 915-932.
"""

import math
from pathlib import Path

import pytest

from digitalmodel.citations.schema import Citation, CitationResolutionError
from digitalmodel.hydrodynamics import roll_damping_components as rdc
from digitalmodel.hydrodynamics.roll_damping_components import (
    FLAG_AMPLITUDE_ABOVE_PAPER_EXAMPLES,
    FLAG_EQ35_WITH_ROUNDED_BILGE,
    FLAG_STRIP_STATIONS_EXCLUDED,
    SOURCE_CITATION,
    OutsideEmpiricalDomainError,
    RollDampingComponentError,
    RollDampingComponents,
    barge_eddy_damping,
    barge_roll_damping,
    bilge_keel_damping_section,
    bilge_keel_hull_pressure_section,
    bilge_keel_lever,
    bilge_radius_from_area_coefficient,
    drag_route_eddy_damping,
    HullSection,
    eddy_damping_section,
    eddy_damping_sections,
    eddy_damping_strip,
    eddy_pressure_ratio,
    friction_radius,
    lift_damping,
    ship_wetted_area,
    validate_citations,
)

RHO = 1025.0
NU = 1.19e-6
# Vendored frontmatter-only wiki fixtures (shared with tests/citations/).
CITATION_ROOT = Path(__file__).resolve().parents[1] / "citations" / "fixtures"
BARGE = dict(
    rho=RHO,
    nu=NU,
    length=120.0,
    beam=36.0,
    draft=5.5,
    kg=8.0,
    omega=0.6,
    citation_root=CITATION_ROOT,
)


# --- P2-1: the diagnostic as_printed coefficient keeps C_p on both terms -------


def test_as_printed_keeps_cp_on_the_second_term():
    """Eq. 18 multiplies all of eq. 25 by C_p; only the (r_max/d)**2 moves.

    Reference values from the review's independent evaluation of
    C_p ((r_max/d)**2 t1 + t2) for the two sections named there.
    """
    small = eddy_pressure_ratio(30.0, 10.0, 0.95, 2.0, 2.0, variant="as_printed")
    assert small == pytest.approx(0.521016558273, rel=1e-9)
    barge = eddy_pressure_ratio(120.0, 19.0, 0.999, 0.0, 1.97, variant="as_printed")
    assert barge == pytest.approx(21.324597992973, rel=1e-9)


# --- P2-2: optional keel inputs are validated before use -----------------------


@pytest.mark.parametrize(
    "keel",
    [
        dict(bilge_keel_breadth=0.5, bilge_keel_length=math.inf),
        dict(bilge_keel_breadth=math.nan, bilge_keel_length=60.0),
        dict(bilge_keel_breadth=-0.5, bilge_keel_length=60.0),
        dict(bilge_keel_breadth=0.5, bilge_keel_length=-60.0),
        dict(bilge_keel_breadth=0.5, bilge_keel_length=0.0),
        dict(bilge_keel_breadth=0.0, bilge_keel_length=60.0),
        dict(bilge_radius=-0.1),
        dict(bilge_radius=math.nan),
    ],
)
def test_barge_rejects_invalid_keel_and_radius_inputs(keel):
    with pytest.raises(RollDampingComponentError):
        barge_roll_damping(amplitude=math.radians(10.0), **BARGE, **keel)


@pytest.mark.parametrize(
    "parts",
    [
        dict(friction=1.0, eddy=math.inf, lift=0.0, wave=0.0, bilge_keel=0.0),
        dict(friction=1.0, eddy=1.0, lift=0.0, wave=0.0, bilge_keel=-1.0),
        dict(friction=math.nan, eddy=1.0, lift=0.0, wave=0.0, bilge_keel=0.0),
    ],
)
def test_components_reject_non_finite_or_negative_parts(parts):
    with pytest.raises(RollDampingComponentError):
        RollDampingComponents(**parts)


# --- P2-3: explicit bilge radii are limited to the section ---------------------


def test_explicit_bilge_radius_beyond_the_section_is_rejected():
    with pytest.raises(RollDampingComponentError):
        eddy_pressure_ratio(20.0, 8.0, 0.98, 0.0, 9.0)
    with pytest.raises(RollDampingComponentError):
        eddy_pressure_ratio(20.0, 8.0, 0.98, 0.0, 40.0)
    with pytest.raises(RollDampingComponentError):
        bilge_keel_lever(20.0, 8.0, 0.0, 9.0)
    with pytest.raises(RollDampingComponentError):
        bilge_keel_hull_pressure_section(
            RHO, 20.0, 8.0, 0.98, 0.0, 0.4, 0.17, 0.6, bilge_radius=8.0 / 0.215
        )
    with pytest.raises(RollDampingComponentError):
        barge_roll_damping(
            amplitude=math.radians(10.0),
            bilge_keel_breadth=0.5,
            bilge_keel_length=60.0,
            bilge_radius=6.0,
            **BARGE,
        )


def test_bilge_radius_at_the_geometric_limit_is_accepted():
    lever = bilge_keel_lever(20.0, 8.0, 0.0, 8.0)
    assert lever > 0.0


# --- P2-4: physical and empirical domain ---------------------------------------


def test_barge_formula_rejects_a_negative_coefficient():
    """B/T 0.5 with OG/T 0.9 and the 3/2 factor gives a negative bracket."""
    with pytest.raises(RollDampingComponentError):
        barge_eddy_damping(RHO, 100.0, 2.5, 5.0, 4.5, 0.1, 0.5, og_factor=1.5)


def test_bilge_keel_rejects_amplitudes_where_its_total_turns_negative():
    """At 180 deg the hull-pressure part outweighs the keel force; 45 deg and up
    is rejected before the sum is formed, and a non-positive sum raises too."""
    radius = bilge_radius_from_area_coefficient(20.0, 8.0, 0.98)
    with pytest.raises(RollDampingComponentError):
        bilge_keel_damping_section(RHO, 20.0, 8.0, 0.98, 0.0, 0.4, math.pi, 0.6, radius)


@pytest.mark.parametrize(
    "call",
    [
        lambda: ship_wetted_area(100.0, 5.0, 20.0, 1.2),
        lambda: friction_radius(1.2, 2000.0, 100.0, 0.0),
        lambda: lift_damping(RHO, 5.0, 100.0, 5.0, 20.0, 0.0, 1.2),
        lambda: barge_roll_damping(
            amplitude=math.radians(10.0), block_coefficient=1.2, **BARGE
        ),
        lambda: barge_roll_damping(amplitude=math.pi / 2.0, **BARGE),
    ],
)
def test_coefficients_above_one_and_meaningless_amplitudes_are_rejected(call):
    with pytest.raises(RollDampingComponentError):
        call()


def test_barge_result_flags_extrapolation_explicitly():
    plain = barge_roll_damping(amplitude=math.radians(8.0), **BARGE)
    assert plain.flags == ()
    large = barge_roll_damping(amplitude=math.radians(15.0), **BARGE)
    assert FLAG_AMPLITUDE_ABOVE_PAPER_EXAMPLES in large.flags
    rounded = barge_roll_damping(
        amplitude=math.radians(8.0),
        bilge_keel_breadth=0.4,
        bilge_keel_length=60.0,
        bilge_radius=0.5,
        **BARGE,
    )
    assert FLAG_EQ35_WITH_ROUNDED_BILGE in rounded.flags


# --- P2-5: sign and factor regressions -----------------------------------------


def test_eddy_variants_differ_by_their_og_terms_at_nonzero_og():
    """With K = C_p (r_max/d)**2, o = OG/d and b = f1 R/d:

    2001 - 2024 = K (1 - b)(0.5 o) and 2011 - 2001 = K (1 - b) b, so the ratio
    of the two differences is 0.5 o / b, independent of K.
    """
    beam, draft, sigma, og, radius = 20.0, 8.0, 0.95, 1.5, 1.5
    paper = eddy_pressure_ratio(beam, draft, sigma, og, radius)
    ittc2024 = eddy_pressure_ratio(beam, draft, sigma, og, radius, variant="ittc2024")
    ittc2011 = eddy_pressure_ratio(beam, draft, sigma, og, radius, variant="ittc2011")
    ittc2021 = eddy_pressure_ratio(beam, draft, sigma, og, radius, variant="ittc2021")
    f1 = 0.5 * (1.0 + math.tanh(20.0 * (sigma - 0.7)))
    o, b = og / draft, f1 * radius / draft
    ratio = (paper - ittc2024) / (ittc2011 - paper)
    assert ratio == pytest.approx(0.5 * o / b, rel=1e-9)
    assert ittc2021 == ittc2011


def test_barge_og_factor_scales_the_first_bracket_only():
    rho, length, beam, draft, og = RHO, 100.0, 30.0, 5.0, -2.0
    k1 = barge_eddy_damping(rho, length, beam, draft, og, 0.1, 0.5)
    k15 = barge_eddy_damping(rho, length, beam, draft, og, 0.1, 0.5, og_factor=1.5)
    h0, o = beam / (2 * draft), og / draft
    assert k15 / k1 == pytest.approx((h0**2 + 1 - 1.5 * o) / (h0**2 + 1 - o), rel=1e-12)


def _reference_hull_pressure(rho, beam, draft, sigma, og, b_bk, amp, omega, radius):
    """Eqs. 49-60 written out independently of the module, term by term."""
    h0 = beam / (2 * draft)
    f = 1 + 0.3 * math.exp(-160 * (1 - sigma))
    r_cb = draft * math.sqrt(
        (h0 - 0.293 * radius / draft) ** 2
        + (1 - og / draft - 0.293 * radius / draft) ** 2
    )
    m1, m2 = radius / draft, og / draft
    m3, m4 = 1 - m1 - m2, h0 - m1
    den = (h0 - 0.215 * m1) * (1 - 0.215 * m1)
    m5 = (0.414 * h0 + 0.0651 * m1**2 - (0.382 * h0 + 0.0106) * m1) / den
    m6 = (0.414 * h0 + 0.0651 * m1**2 - (0.382 + 0.0106 * h0) * m1) / den
    s0 = 0.3 * math.pi * f * r_cb * amp + 1.95 * b_bk
    if s0 > 0.25 * math.pi * radius:
        m7 = s0 / draft - 0.25 * math.pi * m1
        m8 = m7 + 0.414 * m1
        branch = "long"
    else:
        m7 = 0.0
        m8 = m7 + math.sqrt(2) * (1 - math.cos(s0 / radius)) * m1
        branch = "short"
    a2 = (m3 + m4) * m8 - m7**2
    b2 = (
        m4**3 / (3 * (h0 - 0.215 * m1))
        + (1 - m1) ** 2 * (2 * m3 - m2) / (6 * (1 - 0.215 * m1))
        + (m3 * m5 + m4 * m6) * m1
    )
    brace = -(-22.5 * b_bk / (math.pi * r_cb * f * amp) - 1.2) * a2 + 1.2 * b2
    value = 4 / (3 * math.pi) * rho * r_cb**2 * draft**2 * omega * amp * f**2 * brace
    return value, branch


@pytest.mark.parametrize("radius, branch", [(0.5, "long"), (4.0, "short")])
def test_hull_pressure_matches_an_independent_evaluation_on_both_s0_branches(
    radius, branch
):
    args = (RHO, 20.0, 8.0, 0.97, -0.5, 0.2, math.radians(3.0), 0.6)
    expected, got_branch = _reference_hull_pressure(*args, radius)
    assert got_branch == branch
    assert bilge_keel_hull_pressure_section(
        *args, bilge_radius=radius
    ) == pytest.approx(expected, rel=1e-12)


def test_drag_route_dissipates_the_energy_of_its_quadratic_moment():
    """Moment 1/2 C_e rho S r**3 |v| v per cycle equals pi omega phi_a**2 B_eq."""
    cd, rho, area, radius, amp, omega = 0.6, RHO, 40.0, 7.0, math.radians(8.0), 0.5
    b_eq = drag_route_eddy_damping(cd, rho, area, radius, amp, omega)
    n, period = 20000, 2.0 * math.pi / omega
    energy = 0.0
    for k in range(n):
        v = -amp * omega * math.sin(omega * (k + 0.5) * period / n)
        energy += 0.5 * cd * rho * area * radius**3 * abs(v) * v * v * period / n
    assert b_eq * math.pi * omega * amp**2 == pytest.approx(energy, rel=1e-6)


# --- P3-7: structured provenance -----------------------------------------------


def test_barge_result_carries_a_citation_sidecar():
    parts = barge_roll_damping(
        amplitude=math.radians(8.0),
        bilge_keel_breadth=0.4,
        bilge_keel_length=60.0,
        **BARGE,
    )
    sections = {c.section for c in parts.citations}
    assert all(isinstance(c, Citation) for c in parts.citations)
    assert {c.code_id for c in parts.citations} == {SOURCE_CITATION.code_id}
    for locator in ("eqs. 4-5", "eqs. 8-10, 12", "eqs. 35-36", "eqs. 46-60"):
        assert any(s.startswith(locator) for s in sections), locator
    assert "total" in parts.as_dict()


def _wiki(tmp_path, code_id, publisher, revision):
    page = tmp_path / SOURCE_CITATION.wiki_path
    page.parent.mkdir(parents=True)
    page.write_text(
        f"---\ncode_id: {code_id}\npublisher: {publisher}\nrevision: {revision}\n---\n",
        encoding="utf-8",
    )
    return tmp_path


def test_citations_resolve_fail_closed_against_the_wiki_page(tmp_path):
    parts = barge_roll_damping(amplitude=math.radians(8.0), **BARGE)
    good = _wiki(
        tmp_path / "good",
        SOURCE_CITATION.code_id,
        SOURCE_CITATION.publisher,
        SOURCE_CITATION.revision,
    )
    validate_citations(parts, wiki_root=good)
    bad = _wiki(tmp_path / "bad", SOURCE_CITATION.code_id, "Someone else", "1999")
    with pytest.raises(CitationResolutionError):
        validate_citations(parts, wiki_root=bad)
    with pytest.raises(CitationResolutionError):
        validate_citations(parts, wiki_root=tmp_path / "missing")


def _stations_with_a_null_section():
    """Three stations; the middle one (B/d 2.07, sigma 0.762) lies where the
    sectional C_R fit passes through zero and is slightly negative at OG -1 m."""
    return [
        HullSection(x=0.0, beam=25.4, draft=9.5, sigma=0.97),
        HullSection(x=10.0, beam=19.696, draft=9.5, sigma=0.7624),
        HullSection(x=20.0, beam=8.8, draft=9.5, sigma=0.5),
    ]


def test_strip_sum_rejects_a_station_outside_the_domain_by_default():
    with pytest.raises(RollDampingComponentError):
        eddy_damping_sections(_stations_with_a_null_section(), RHO, -1.0, 0.1, 0.6)


def _trapezoid(xs, values):
    return sum(
        0.5 * (values[i] + values[i + 1]) * (xs[i + 1] - xs[i])
        for i in range(len(xs) - 1)
    )


def test_strip_record_without_exclusions_matches_the_strict_sum():
    st = _stations_with_a_null_section()[::2]
    record = eddy_damping_strip(st, RHO, -1.0, 0.1, 0.6)
    assert record.damping == eddy_damping_sections(st, RHO, -1.0, 0.1, 0.6)
    assert record.excluded == ()
    assert record.flags == ()
    assert record.excluded_length_share == 0.0


def test_strip_record_names_each_excluded_station_and_its_weight():
    st = _stations_with_a_null_section()
    record = eddy_damping_strip(st, RHO, -1.0, 0.1, 0.6, exclude_non_positive=True)
    v0, v2 = (
        eddy_damping_section(RHO, s.beam, s.draft, s.sigma, -1.0, None, 0.1, 0.6)
        for s in (st[0], st[2])
    )
    assert record.damping == pytest.approx(
        _trapezoid([0.0, 10.0, 20.0], [v0, 0.0, v2]), rel=1e-12
    )
    assert record.flags == (FLAG_STRIP_STATIONS_EXCLUDED,)
    (station,) = record.excluded
    assert (station.index, station.x) == (1, 10.0)
    assert station.integration_weight == pytest.approx(10.0)
    assert station.length_share == pytest.approx(0.5)
    assert -1.0e-3 < station.c_r < 0.0
    assert record.excluded_length_share == pytest.approx(0.5)


def test_strip_with_every_station_excluded_raises_rather_than_returning_zero():
    null = _stations_with_a_null_section()[1]
    st = [null, HullSection(x=20.0, beam=null.beam, draft=null.draft, sigma=null.sigma)]
    with pytest.raises(OutsideEmpiricalDomainError):
        eddy_damping_strip(st, RHO, -1.0, 0.1, 0.6, exclude_non_positive=True)


def test_strip_exclusion_does_not_cover_non_finite_values(monkeypatch):
    monkeypatch.setattr(rdc, "eddy_pressure_ratio", lambda *a, **k: float("nan"))
    with pytest.raises(RollDampingComponentError) as info:
        eddy_damping_strip(
            _stations_with_a_null_section(),
            RHO,
            -1.0,
            0.1,
            0.6,
            exclude_non_positive=True,
        )
    assert not isinstance(info.value, OutsideEmpiricalDomainError)


def test_non_finite_coefficient_is_not_classed_as_outside_the_domain():
    with pytest.raises(RollDampingComponentError) as info:
        rdc._dissipative("C_R", float("nan"))
    assert not isinstance(info.value, OutsideEmpiricalDomainError)
    with pytest.raises(OutsideEmpiricalDomainError) as info:
        rdc._dissipative("C_R", -1.0e-4)
    assert info.value.value == -1.0e-4


# --- R2-1: citations resolve at calculation time, fail-closed ---------------


def test_barge_calculation_resolves_its_citations_against_the_fixture():
    parts = barge_roll_damping(amplitude=math.radians(8.0), **BARGE)
    assert parts.citations
    validate_citations(parts, wiki_root=CITATION_ROOT)


def test_barge_calculation_aborts_when_the_source_page_is_missing(tmp_path):
    (tmp_path / "wikis").mkdir()
    kwargs = dict(BARGE, citation_root=tmp_path)
    with pytest.raises(CitationResolutionError, match=SOURCE_CITATION.code_id):
        barge_roll_damping(amplitude=math.radians(8.0), **kwargs)


@pytest.mark.parametrize("field", ["code_id", "publisher", "revision"])
def test_barge_calculation_aborts_on_each_mismatched_identity_field(tmp_path, field):
    identity = {
        "code_id": SOURCE_CITATION.code_id,
        "publisher": SOURCE_CITATION.publisher,
        "revision": SOURCE_CITATION.revision,
    }
    identity[field] = "mismatch"
    root = _wiki(tmp_path, **identity)
    kwargs = dict(BARGE, citation_root=root)
    with pytest.raises(CitationResolutionError, match=f"frontmatter_mismatch:{field}"):
        barge_roll_damping(amplitude=math.radians(8.0), **kwargs)
