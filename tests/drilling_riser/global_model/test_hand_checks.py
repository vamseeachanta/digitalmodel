"""Hand checks behind the W2 qualification gates: submerged weight, effective
tension chain and tensioned-beam natural periods (synthetic data only)."""

from __future__ import annotations

import math

import pytest

from digitalmodel.drilling_riser.global_model.hand_checks import (
    BeamSegment,
    beam_model,
    effective_tension_chain,
    ring_weight_n,
    submerged_length_m,
    submerged_weight_n,
    tension_references,
    tensioned_beam_periods,
    tensioner_lateral_stiffness_n_m,
    wkb_reference_periods,
    wkb_string_periods,
)
from digitalmodel.drilling_riser.global_model.spec import LineSection

G = 9.80665  # NIST 2022 CODATA, standard acceleration of gravity (exact convention).
RHO_W = 1025.0


def _sec(name, length, mass=500.0, vol=0.3, bore=0.0, top_z=None):
    return LineSection(
        name=name, length_m=length, segment_length_m=1.0, mass_per_m_kg=mass,
        displaced_volume_per_m_m3=vol, bore_id_m=bore, ei_nm2=1.0e8, ea_n=1.0e10,
        drag_diameter_m=0.8, cd_normal=1.0, ca_normal=1.0,
    )


def test_submerged_weight_fully_submerged_section_includes_contents_and_buoyancy():
    s = _sec("a", 10.0, mass=500.0, vol=0.3, bore=0.4)
    rho_c = 1500.0
    w = submerged_weight_n(s, top_z_m=-5.0, rho_water=RHO_W, rho_contents=rho_c)
    a_bore = math.pi / 4 * 0.4**2
    expected = 10.0 * (500.0 + rho_c * a_bore - RHO_W * 0.3) * G
    assert w == pytest.approx(expected, rel=1e-12)


def test_submerged_weight_counts_buoyancy_only_below_msl():
    s = _sec("a", 10.0, mass=500.0, vol=0.3)
    # 4 m above MSL, 6 m below
    w = submerged_weight_n(s, top_z_m=4.0, rho_water=RHO_W, rho_contents=0.0)
    assert w == pytest.approx((10.0 * 500.0 - 6.0 * RHO_W * 0.3) * G, rel=1e-12)


def test_effective_tension_chain_top_and_bottom():
    secs = [_sec("a", 10.0), _sec("b", 20.0, mass=800.0, vol=0.1)]
    chain = effective_tension_chain(
        secs, top_z_m=-1.0, top_tension_n=1.0e6, rho_water=RHO_W, rho_contents=0.0,
    )
    w_a = 10.0 * (500.0 - RHO_W * 0.3) * G
    w_b = 20.0 * (800.0 - RHO_W * 0.1) * G
    assert chain[0]["te_top_n"] == pytest.approx(1.0e6)
    assert chain[0]["te_bottom_n"] == pytest.approx(1.0e6 - w_a)
    assert chain[-1]["te_bottom_n"] == pytest.approx(1.0e6 - w_a - w_b)
    assert chain[-1]["bottom_z_m"] == pytest.approx(-31.0)


def _uniform(n_el, length, ei, te, m):
    le = length / n_el
    return [BeamSegment(length_m=le, ei_nm2=ei, te_top_n=te, te_bottom_n=te, mass_per_m_kg=m)
            for _ in range(n_el)]


def test_tensioned_string_limit_matches_closed_form():
    """Independent hand calculation for the synthetic uniform-string fixture.

    Source: OpenStax, University Physics Volume 1 (2016), section 16.6,
    equation 16.16 (normal-mode frequencies of a fixed-end string).
    Substituting sin(n*pi*x/L) into m*w_tt - Te*w_xx = 0 gives
    f_n = n*sqrt(Te/m)/(2*L). The fixture gives sqrt(Te/m) = 50 m/s,
    so T_n = 12/n s: periods decrease as mode number increases.
    """
    length, te, m = 300.0, 2.0e6, 800.0
    periods = tensioned_beam_periods(_uniform(300, length, 1.0, te, m), n_modes=5)
    assert all(first > second for first, second in zip(periods, periods[1:]))
    for n, t in enumerate(periods, start=1):
        f = n / (2 * length) * math.sqrt(te / m)
        assert t == pytest.approx(1.0 / f, rel=5e-3)


def test_untensioned_pinned_beam_matches_closed_form():
    """Independent hand calculation for the synthetic pinned-beam fixture.

    Substituting sin(n*pi*x/L) into EI*w_xxxx + m*w_tt = 0 gives
    omega_n = (n*pi/L)**2*sqrt(EI/m), hence T_n = T_1/n**2.
    Thus the existing closed-form comparison also fixes mode identity.
    """
    length, ei, m = 50.0, 2.0e8, 400.0
    periods = tensioned_beam_periods(_uniform(100, length, ei, 0.0, m), n_modes=3)
    assert all(first > second for first, second in zip(periods, periods[1:]))
    for n, t in enumerate(periods, start=1):
        omega = (n * math.pi / length) ** 2 * math.sqrt(ei / m)
        assert t == pytest.approx(2 * math.pi / omega, rel=5e-3)


def test_beam_period_order_is_documented():
    """The public contract must describe the order established by the hand checks."""
    doc = " ".join(tensioned_beam_periods.__doc__.split())
    assert "decreasing periods" in doc
    assert "increasing natural frequency" in doc


def test_stiff_end_springs_approach_clamped_beam():
    """Independent fixed-fixed boundary solution, not a tabulated root.

    Source: Euphonics, 3.2.1 Bending beams and free-free modes (undated,
    accessed 2026), equations (7), (12), (13):
    https://euphonics.org/3-2-1-bending-beams-and-free-free-modes/
    The source's free-free determinant is NOT assumed to be clamped.

    With xi=x/L, its general solution and u(0)=u'(0)=0 give
    u=A*(cosh(beta*xi)-cos(beta*xi))
      +B*(sinh(beta*xi)-sin(beta*xi)).
    The remaining u(1)=u'(1)=0 determinant reduces to
    2*(1-cosh(beta)*cos(beta))=0. Bisection on (3*pi/2, 2*pi)
    excludes the zero root; B/A=-(cosh(beta)-cos(beta))/(sinh(beta)-sin(beta)).
    Both clamped endpoint conditions are checked below. omega=beta**2/L**2
    *sqrt(EI/mu); period=2*pi/omega. All physical inputs are synthetic.
    The relative tolerance is a chosen FE/finite-spring approximation check,
    not a standard acceptance criterion. The legacy six-decimal root is
    compared to the independently solved root at its rounding precision.
    """
    lo, hi = 1.5 * math.pi, 2 * math.pi
    for _ in range(60):  # bisection iterations exceed double-precision resolution
        mid = (lo + hi) / 2
        if math.cos(mid) * math.cosh(mid) - 1 > 0:
            hi = mid
        else:
            lo = mid
    beta = (lo + hi) / 2
    assert 4.730041 == pytest.approx(beta, abs=0.5e-6)  # six-place rounding
    ratio = -(math.cosh(beta) - math.cos(beta)) / (math.sinh(beta) - math.sin(beta))
    for xi in (0.0, 1.0):
        b = beta * xi
        u = math.cosh(b) - math.cos(b) + ratio * (math.sinh(b) - math.sin(b))
        du = math.sinh(b) + math.sin(b) + ratio * (math.cosh(b) - math.cos(b))
        assert u == pytest.approx(0, abs=1e-12)  # chosen roundoff check
        assert du == pytest.approx(0, abs=1e-12)
    length, ei, m = 50.0, 2.0e8, 400.0
    periods = tensioned_beam_periods(_uniform(200, length, ei, 0.0, m), n_modes=1,
                                     top_rot_stiffness_nm_per_rad=1e14,
                                     bottom_rot_stiffness_nm_per_rad=1e14)
    omega1 = beta**2 / length**2 * math.sqrt(ei / m)
    assert periods[0] == pytest.approx(2 * math.pi / omega1, rel=5e-3)


def test_point_mass_lengthens_first_period():
    segs = _uniform(100, 100.0, 1.0, 1.0e6, 500.0)
    base = tensioned_beam_periods(segs, n_modes=1)[0]
    heavy = tensioned_beam_periods(segs, n_modes=1, point_masses={50: 2.0e4})[0]
    assert heavy > base


def test_wkb_uniform_string_is_exact():
    length, te, m = 300.0, 2.0e6, 800.0
    periods = wkb_string_periods(_uniform(30, length, 0.0, te, m), n_modes=3)
    for n, t in enumerate(periods, start=1):
        assert t == pytest.approx(2 * length / (n * math.sqrt(te / m)), rel=1e-9)


def test_negative_tension_rejected_in_wkb():
    with pytest.raises(ValueError):
        wkb_string_periods(_uniform(3, 10.0, 0.0, -1.0, 1.0), n_modes=1)


def test_worked_partial_immersion_example():
    """Independent arithmetic for the method note's synthetic worked example.

    A 10 m section spans z=4 to -6 m, so 6 m is wet. Its structure has
    mass 500*10=5000 kg and it displaces 1000*0.3*6=1800 kg of water.
    Net mass 3200 kg times NIST 2022 conventional g gives 31381.28 N.
    At 100000 N top effective tension, equilibrium gives 68618.72 N below.
    """
    section = _sec("worked", 10.0, mass=500.0, vol=0.3)
    chain = effective_tension_chain(
        [section], top_z_m=4.0, top_tension_n=100000.0,
        rho_water=1000.0, rho_contents=0.0,
    )
    assert submerged_length_m(4.0, 10.0) == 6.0
    assert submerged_length_m(14.0, 10.0) == 0.0
    assert submerged_length_m(-4.0, 10.0) == 10.0
    assert chain[0]["submerged_weight_n"] == pytest.approx(31381.28)
    assert chain[0]["te_bottom_n"] == pytest.approx(68618.72)


@pytest.mark.parametrize("z, expected", [(0.0, 980.665), (-1.0, 490.3325)])
def test_ring_immersion_convention(spec, z, expected):
    """Independent lumped-ring balance, with synthetic inputs.

    Mr=100 kg, Vr=0.05 m^3, rho=1000 kg/m^3. Dry weight is 100*g;
    wet weight is (100-50)*g, using NIST 2022 conventional g. A centre
    exactly at mean sea level is dry by the implementation convention.
    """
    spec.environment.water_density_kg_m3 = 1000.0
    spec.tension_ring.mass_kg = 100.0
    spec.tension_ring.volume_m3 = 0.05
    spec.tension_ring.z_static_m = z
    assert ring_weight_n(spec) == pytest.approx(expected)


def test_vertical_tensioner_derivative_limit(spec):
    """Independent derivative: F*x/sqrt(h^2+x^2) has slope F/h at x=0.

    For synthetic vertical lines, total force 100000 N and separation
    10 m give 10000 N/m, independent of azimuth or line count. No axial
    spring stiffness is included because line force is held constant.
    """
    t = spec.tensioners
    t.ring_attach_radius_m = t.sheave_radius_m
    t.sheave_z_m = spec.tension_ring.z_static_m + 10.0
    t.total_vertical_tension_n = 100000.0
    assert tensioner_lateral_stiffness_n_m(spec) == pytest.approx(10000.0)


def test_uniform_neutral_riser_assembly_and_string_reference(spec):
    """Independent synthetic mass/equilibrium/travel-time assembly check.

    Riser L=10 m, structure 300 kg/m, displacement 0.3 m^3/m and water
    density 1000 kg/m^3 give zero submerged weight. With zero bore and
    unit added-mass coefficient, dynamic mass is 600 kg/m. A massless,
    volume-free ring leaves Te=240000 N throughout. Speed=sqrt(240000/600)
    =20 m/s, so the ring-to-bottom string periods are 1/n s. The dry 2 m
    inner barrel contributes two elements and is excluded from this travel
    time. These are assembly identities, not hydrodynamic qualification.
    """
    spec.environment.water_density_kg_m3 = 1000.0
    spec.contents.density_kg_m3 = 0.0
    spec.upper_flex_joint.pivot_z_m = 1.0
    spec.tension_ring.z_static_m = -1.0
    spec.tension_ring.mass_kg = spec.tension_ring.volume_m3 = 0.0
    spec.tensioners.total_vertical_tension_n = 240000.0
    spec.inner_barrel = [_sec("barrel", 2.0, mass=4.0, vol=0.0)]
    spec.riser = [_sec("neutral", 10.0, mass=300.0, vol=0.3)]
    spec.stack = [_sec("stack", 2.0, mass=0.0, vol=0.0)]
    refs = tension_references(spec)
    assert refs["riser_top_n"] == refs["riser_bottom_n"] == refs["stack_bottom_n"] == 240000.0
    segments, ring_node = beam_model(spec)
    assert ring_node == 2
    assert len(segments) == 12
    assert [s.mass_per_m_kg for s in segments] == [4.0] * 2 + [600.0] * 10
    assert wkb_reference_periods(spec, n_modes=3) == pytest.approx([1.0, 0.5, 1 / 3])
