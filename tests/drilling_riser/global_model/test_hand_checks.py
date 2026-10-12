"""Hand checks behind the W2 qualification gates: submerged weight, effective
tension chain and tensioned-beam natural periods (synthetic data only)."""

from __future__ import annotations

import math

import pytest

from digitalmodel.drilling_riser.global_model.hand_checks import (
    BeamSegment,
    effective_tension_chain,
    submerged_weight_n,
    tensioned_beam_periods,
    wkb_string_periods,
)
from digitalmodel.drilling_riser.global_model.spec import LineSection

G = 9.80665
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
    length, te, m = 300.0, 2.0e6, 800.0
    periods = tensioned_beam_periods(_uniform(300, length, 1.0, te, m), n_modes=5)
    for n, t in enumerate(periods, start=1):
        f = n / (2 * length) * math.sqrt(te / m)
        assert t == pytest.approx(1.0 / f, rel=5e-3)


def test_untensioned_pinned_beam_matches_closed_form():
    length, ei, m = 50.0, 2.0e8, 400.0
    periods = tensioned_beam_periods(_uniform(100, length, ei, 0.0, m), n_modes=3)
    for n, t in enumerate(periods, start=1):
        omega = (n * math.pi / length) ** 2 * math.sqrt(ei / m)
        assert t == pytest.approx(2 * math.pi / omega, rel=5e-3)


def test_stiff_end_springs_approach_clamped_beam():
    length, ei, m = 50.0, 2.0e8, 400.0
    periods = tensioned_beam_periods(_uniform(200, length, ei, 0.0, m), n_modes=1,
                                     top_rot_stiffness_nm_per_rad=1e14,
                                     bottom_rot_stiffness_nm_per_rad=1e14)
    omega1 = 4.730041**2 / length**2 * math.sqrt(ei / m)
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
