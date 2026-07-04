"""Tests for the parametric floater archetype models (issue #1023).

Closed-form / licence-free: no OrcaFlex or OrcaWave required. Assertions check
physical relationships and plausible ranges rather than exact magic numbers,
since the models are coefficient-based concept-screening estimates.
"""

import math

import pytest

from digitalmodel.floating_wind import (
    Barge,
    FloaterArchetype,
    IEA_15MW_RNA,
    SemiSubmersible,
    Spar,
    TLP,
    TurbineTopside,
    build_floater,
)


@pytest.fixture
def topside() -> TurbineTopside:
    return IEA_15MW_RNA


def test_topside_cg_between_base_and_hub():
    t = IEA_15MW_RNA
    assert t.total_mass_t == pytest.approx(2280.0)
    cg = t.cg_above_waterline_m()
    assert t.tower_base_above_swl_m < cg < t.hub_height_m


def test_semi_is_stable_and_in_plausible_period_band(topside):
    semi = SemiSubmersible(
        n_columns=3,
        column_diameter=12.5,
        column_radius=51.75,
        draft=20.0,
        pontoon_height=7.0,
        pontoon_volume=10000.0,
        steel_mass_t=4000.0,
    )
    p = semi.properties(topside)
    assert p.archetype is FloaterArchetype.SEMI
    assert p.feasible
    assert p.GM_m > 0
    # GM = KB + BM - KG identity holds.
    assert p.GM_m == pytest.approx(p.KB_m + p.BM_m - p.KG_m)
    # Mass balance closes: steel + ballast + topside == total == displacement.
    assert p.total_mass_t == pytest.approx(
        p.steel_mass_t + p.ballast_mass_t + p.topside_mass_t
    )
    assert p.total_mass_t == pytest.approx(p.displacement_t)
    # Column-stabilised semis sit above the wave band in heave and pitch.
    assert 15.0 < p.heave_natural_period_s < 28.0
    assert 18.0 < p.pitch_natural_period_s < 40.0


def test_well_sized_spar_is_stable_with_long_heave(topside):
    spar = Spar(diameter=20.0, draft=120.0, steel_mass_t=6000.0)
    p = spar.properties(topside)
    assert p.feasible
    assert p.GM_m > 0
    # Spar BM is small (= D^2 / 16T); stability comes from KG far below KB.
    assert p.BM_m == pytest.approx(20.0**2 / (16 * 120.0), rel=1e-6)
    assert p.KG_m < p.KB_m
    assert p.heave_natural_period_s > 20.0


def test_slim_spar_with_heavy_topside_is_statically_unstable(topside):
    # A too-slim spar cannot carry a 15 MW topside: the screen must flag it.
    spar = Spar(diameter=14.0, draft=90.0, steel_mass_t=3500.0)
    p = spar.properties(topside)
    assert p.GM_m < 0
    assert math.isinf(p.pitch_natural_period_s)
    assert any("non-positive GM" in n for n in p.notes)


def test_tlp_is_tendon_stabilised_below_wave_band(topside):
    tlp = TLP(
        n_columns=4,
        column_diameter=10.0,
        column_radius=30.0,
        draft=30.0,
        pontoon_height=8.0,
        pontoon_volume=6000.0,
        steel_mass_t=3000.0,
        n_tendons=4,
        tendon_EA_N=1.2e9,
        tendon_length_m=170.0,
    )
    p = tlp.properties(topside)
    assert p.tendon_stabilised
    # Tendon stiffness places heave and pitch well below the wave-energy band.
    assert 0.0 < p.heave_natural_period_s < 6.0
    assert 0.0 < p.pitch_natural_period_s < 6.0
    assert any("tendon-stabilised" in n for n in p.notes)


def test_barge_is_stiff_with_large_gm(topside):
    barge = Barge(length=60.0, beam=60.0, draft=12.0, steel_mass_t=5000.0)
    p = barge.properties(topside)
    assert p.feasible
    # Large waterplane -> large BM -> large, very stiff GM.
    assert p.GM_m > 10.0
    # And a livelier (shorter) heave than a semi.
    assert p.heave_natural_period_s < 12.0


def test_infeasible_when_lightweight_exceeds_buoyancy(topside):
    # Tiny barge cannot float a heavy topside: ballast would be negative.
    barge = Barge(length=20.0, beam=20.0, draft=4.0, steel_mass_t=3000.0)
    p = barge.properties(topside)
    assert not p.feasible
    assert p.ballast_mass_t == 0.0
    assert any("infeasible" in n for n in p.notes)


def test_pontoon_height_must_be_less_than_draft():
    with pytest.raises(ValueError):
        SemiSubmersible(
            n_columns=3,
            column_diameter=12.5,
            column_radius=51.75,
            draft=8.0,
            pontoon_height=8.0,
            pontoon_volume=10000.0,
            steel_mass_t=4000.0,
        )


def test_build_floater_factory(topside):
    f = build_floater("spar", diameter=20.0, draft=120.0, steel_mass_t=6000.0)
    p = f.properties(topside)
    assert p.archetype is FloaterArchetype.SPAR
    # Unknown archetype raises.
    with pytest.raises(ValueError):
        build_floater("monohull", foo=1)
