# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Synthetic box-barge arithmetic independent of the loading implementation.

Source S20: *Principles of Ship Performance*, Summer 2020 edition, sections
1.7.7 (moments), 3.3 (hydrostatics), 4.7.1 (lost buoyancy), 6.2 (load curves).
Source IS08: IMO MSC.267(85), 2008 IS Code, part A section 2.2.4.
See the accompanying value-examples document for public source locators.
All geometry, weights, densities and discretisation choices are explicit test
inputs, not recommended physical properties or acceptance limits.
Tolerance 1e-10 m/t allows binary arithmetic only; mesh tolerances are stated
separately. The wall-sided textbook edition could not be opened: nonzero-heel
formula values remain NOT_EVALUATED; only exact limits and invariants are tested.
"""

from dataclasses import replace

import pytest

from digitalmodel.naval_architecture.loading_computer import (
    BoxHull,
    DamageCase,
    WeightItem,
    damage_stability,
    gz_wall_sided,
    intact_stability,
    longitudinal_strength,
    run_loading_computer,
    solve_equilibrium,
    total_weight_and_centroids,
)


@pytest.fixture
def uniform_box():
    """S20 sec.3.3 example: W=800 t, rho=1 t/m3, L=40 m, B=10 m.

    V=800/1=800 m3; T=800/(40*10)=2 m. KG=2 m, LCG=20 m.
    Density is a stipulated input; no default seawater property is relied on.
    """
    hull = BoxHull(40.0, 10.0, 8.0)
    items = [WeightItem("uniform mass", 800.0, 20.0, 2.0, length_m=40.0)]
    return hull, items, solve_equilibrium(hull, items, rho_t_m3=1.0)


def test_total_weight_and_centroids_values():
    """S20 sec.1.7.7, first moments: W=100+200+300=600 t.

    Longitudinal moment=100*2+200*5+300*8=3600 t.m; LCG=6 m.
    Vertical moment=100*1+200*2+300*3=1400 t.m; KG=7/3 m.
    """
    items = [
        WeightItem("first", 100.0, 2.0, 1.0),
        WeightItem("second", 200.0, 5.0, 2.0),
        WeightItem("third", 300.0, 8.0, 3.0),
    ]
    assert total_weight_and_centroids(items) == pytest.approx(
        (600.0, 6.0, 2.333333333333333), abs=1e-10, rel=0
    )


def test_total_weight_and_centroids_empty_refusal():
    """A centroid cannot be defined for zero total mass (S20 sec.1.7.7)."""
    with pytest.raises(ValueError, match="total weight must be positive"):
        total_weight_and_centroids([])


def test_solve_equilibrium_box_values(uniform_box):
    """S20 sec.3.3: KB=T/2=1; BMT=B^2/(12T)=100/24=25/6.

    KMT=31/6; GMT=31/6-2=19/6. BML=1600/24=200/3;
    GML=1+200/3-2=197/3. LCG=LCB=20 => zero trimming moment.
    """
    _, _, eq = uniform_box
    assert (
        eq.displacement_t,
        eq.volume_m3,
        eq.draft_m,
        eq.kb_m,
        eq.bm_t_m,
        eq.km_m,
        eq.gm_t_m,
        eq.bm_l_m,
        eq.gm_l_m,
    ) == pytest.approx(
        (
            800.0,
            800.0,
            2.0,
            1.0,
            4.166666666666667,
            5.166666666666667,
            3.166666666666667,
            66.66666666666667,
            65.66666666666667,
        ),
        abs=1e-10,
        rel=0,
    )
    assert (eq.lcg_m, eq.lcb_m, eq.kg_m, eq.trim_m) == pytest.approx(
        (20.0, 20.0, 2.0, 0.0), abs=1e-10, rel=0
    )
    assert eq.trim_direction == "even keel"


def test_gz_wall_sided_zero_limit(uniform_box):
    """Exact limit: upright symmetric geometry has zero righting lever.

    For the documented GZ=sin(phi)*(GM+.5 BM tan(phi)^2), phi=0 gives
    sin(0)=tan(0)=0, hence GZ=0 regardless of the finite GM and BM.
    Nonzero-angle formula qualification is NOT_EVALUATED.
    """
    _, _, eq = uniform_box
    assert gz_wall_sided(eq.gm_t_m, eq.bm_t_m, 0.0) == pytest.approx(
        0.0,
        abs=1e-12,
        rel=0,  # zero-limit tolerance, round-off only
    )


@pytest.mark.parametrize("heel", [10.0, 30.0])
def test_gz_wall_sided_sign_symmetry(uniform_box, heel):
    """Reflection invariant: a symmetric hull has GZ(-phi)=-GZ(phi).

    The selected 10/30 degree inputs exercise finite heels. Nonzero numeric
    values are NOT_EVALUATED without an accessible edition of the cited text.
    """
    _, _, eq = uniform_box
    positive = gz_wall_sided(eq.gm_t_m, eq.bm_t_m, heel)
    negative = gz_wall_sided(eq.gm_t_m, eq.bm_t_m, -heel)
    assert positive > 0.0
    assert negative == pytest.approx(-positive, abs=1e-12, rel=0)


def test_gz_wall_sided_monotonic_positive_heel(uniform_box):
    """Positive GM and BM give increasing GZ over these small positive heels.

    This is an invariant check, not a numerical formula qualification.
    """
    _, _, eq = uniform_box
    values = [gz_wall_sided(eq.gm_t_m, eq.bm_t_m, a) for a in (0.0, 10.0, 30.0)]
    assert values[0] < values[1] < values[2]


def test_intact_stability_box_values(uniform_box):
    """S20 sec.3.3 gives GMT=19/6 m, carried into the intact result.

    The heel grid is stipulated: 0,10,20,30,40 degrees; upright GZ=0.
    Nonzero GZ values and the integrated areas remain NOT_EVALUATED.
    """
    _, _, eq = uniform_box
    result = intact_stability(eq, max_heel_deg=40.0, step_deg=10.0)
    assert result.gm_m == pytest.approx(3.166666666666667, abs=1e-10, rel=0)
    assert result.angles_deg == [0.0, 10.0, 20.0, 30.0, 40.0]
    assert result.gz_m[0] == pytest.approx(0.0, abs=1e-12, rel=0)


@pytest.mark.parametrize("gm, passes", [(0.14, False), (0.15, True), (0.16, True)])
def test_intact_stability_initial_gm_criterion(uniform_box, gm, passes):
    """IS08 part A 2.2.4: initial GM shall not be less than 0.15 m.

    Explicit 0.14,0.15,0.16 m inputs isolate below/at/above the one criterion.
    Other criteria are not asserted; no aggregate acceptance claim is made.
    """
    _, _, eq = uniform_box
    result = intact_stability(replace(eq, gm_t_m=gm), max_heel_deg=40.0)
    criterion = next(
        c for c in result.imo_check["criteria"] if c["name"] == "Initial GM"
    )
    assert criterion["required"] == pytest.approx(0.15, abs=1e-12, rel=0)
    assert criterion["value"] == pytest.approx(gm, abs=1e-12, rel=0)
    assert criterion["pass"] is passes


def test_damage_stability_zero_permeability_limit(uniform_box):
    """S20 sec.4.7.1: no floodable volume => no lost buoyancy or inertia.

    With permeability=0, T'=2 m and GM'=19/6 m exactly. This avoids
    treating the module's finite-flooding approximation as an exact solution.
    """
    hull, _, eq = uniform_box
    result = damage_stability(
        hull, eq, DamageCase("sealed", 4.0, 20.0, 0.0), rho_t_m3=1.0
    )
    assert (
        result.sinkage_m,
        result.damaged_draft_m,
        result.lost_waterplane_inertia_m4,
        result.damaged_gm_m,
        result.gm_reduction_m,
    ) == pytest.approx((0.0, 2.0, 0.0, 3.166666666666667, 0.0), abs=1e-10, rel=0)


@pytest.mark.xfail(
    strict=True,
    raises=AssertionError,
    reason="parallel sinkage uses the full intact waterplane",
)
@pytest.mark.parametrize(
    "field, expected",
    [
        ("damaged_draft_m", 2.222222222222222),
        ("damaged_gm_m", 2.861111111111111),
    ],
)
def test_damage_stability_exact_lost_buoyancy_values(uniform_box, field, expected):
    """S20 sec.4.7.1: regain the lost volume at unchanged mass and KG.

    A centred full-breadth, 4 m long compartment is fully permeable.
    Intact waterplane=400 m2; damaged effective waterplane=400-4*10=360 m2.
    At the final draft, (400-40)*T'=800 => T'=20/9=2.222222222222222 m.
    Sinkage=2/9 m. The approximation currently returns 2.2 m, not 20/9 m.
    The compartment is centred and extends through the final waterline.
    S20 sec.3.3.2.1: KB'=10/9; I'=36*10^3/12=3000 m4;
    BM'=3000/800=15/4; GM'=10/9+15/4-2=103/36=2.861111111111111 m.
    The approximate draft instead gives GM'=2.85 m.
    """
    hull, _, eq = uniform_box
    result = damage_stability(
        hull, eq, DamageCase("flooded", 4.0, 20.0, 1.0), rho_t_m3=1.0
    )
    assert getattr(result, field) == pytest.approx(expected, abs=1e-10, rel=0)


def test_damage_stability_finite_lost_inertia_value(uniform_box):
    """S20 Appendix C: fully permeable 4 m by 10 m waterplane loses
    transverse inertia l*b^3/12=4*1000/12=1000/3 m4.

    This geometry check is independent of the approximate sinkage calculation.
    """
    hull, _, eq = uniform_box
    result = damage_stability(
        hull, eq, DamageCase("flooded", 4.0, 20.0, 1.0), rho_t_m3=1.0
    )
    assert result.lost_waterplane_inertia_m4 == pytest.approx(
        333.3333333333333, abs=1e-10, rel=0
    )


def test_longitudinal_strength_uniform_zero(uniform_box):
    """S20 sec.6.2: w=b=800/40=20 t/m => q=0, SF=0, BM=0 throughout.

    These are free ends of a floating box, not simply supported end reactions.
    """
    hull, items, eq = uniform_box
    result = longitudinal_strength(hull, items, eq, n_stations=41)
    assert result.weight_per_m_t == pytest.approx([20.0] * 41, abs=1e-10, rel=0)
    assert result.buoyancy_per_m_t == pytest.approx([20.0] * 41, abs=1e-10, rel=0)
    assert result.shear_force_t == pytest.approx([0.0] * 41, abs=1e-10, rel=0)
    assert result.bending_moment_t_m == pytest.approx([0.0] * 41, abs=1e-10, rel=0)


@pytest.mark.parametrize("stations, tolerance", [(101, 0.021), (201, 0.011)])
def test_longitudinal_strength_floating_point_mass_values(stations, tolerance):
    """S20 sec.6.2, integrate net load for a synthetic central mass W=800 t.

    L=40 m, uniform buoyancy W/L=20 t/m. Before x=20: SF=20x;
    after x=20: SF=20x-800. Hence SF(10)=200, SF(30)=-200 t.
    Integrating the triangles gives BM(10)=BM(30)=1000 t.m,
    max BM=W L/8=4000 t.m. There are no end supports in this model.
    Spreading a point over one station cell rounds the central cusp;
    tolerate 2.1%/1.1% at 100/200 intervals (O(dx/L) local error).
    Residual absolute tolerances allow accumulated binary rounding across
    station integrations (1e-9 t and 1e-8 t.m).
    The simply supported W L/4 case is NOT_EVALUATED by this API.
    """
    hull = BoxHull(40.0, 10.0, 8.0)
    items = [WeightItem("central mass", 800.0, 20.0, 2.0)]
    eq = solve_equilibrium(hull, items, rho_t_m3=1.0)
    result = longitudinal_strength(hull, items, eq, n_stations=stations)
    quarter = (stations - 1) // 4
    assert result.shear_force_t[quarter] == pytest.approx(200.0, abs=1e-9, rel=0)
    assert result.shear_force_t[3 * quarter] == pytest.approx(-200.0, abs=1e-9, rel=0)
    assert result.bending_moment_t_m[quarter] == pytest.approx(1000.0, abs=1e-8, rel=0)
    assert result.bending_moment_t_m[3 * quarter] == pytest.approx(
        1000.0, abs=1e-8, rel=0
    )
    assert abs(result.max_bending_t_m) == pytest.approx(4000.0, rel=tolerance)
    assert result.shear_end_residual_t == pytest.approx(0.0, abs=1e-9, rel=0)
    assert result.moment_end_residual_t_m == pytest.approx(0.0, abs=1e-8, rel=0)


def test_run_loading_computer_settings_values():
    """End-to-end version of the S20 box calculation through explicit settings.

    W=800 t, rho=1, L*B=400 m2 => T=2, KB=1, BM=25/6, GM=19/6 m.
    Uniform w=b=20 t/m => zero still-water shear and moment.
    A sealed compartment preserves the draft; output heel grid uses 10 degrees.
    """
    result = run_loading_computer(
        {
            "hull": {"length_m": 40.0, "beam_m": 10.0, "depth_m": 8.0},
            "water_density_t_m3": 1.0,
            "weight_items": [
                {
                    "name": "uniform mass",
                    "weight_t": 800.0,
                    "lcg_m": 20.0,
                    "vcg_m": 2.0,
                    "length_m": 40.0,
                }
            ],
            "max_heel_deg": 40.0,
            "heel_step_deg": 10.0,
            "longitudinal_strength": {"n_stations": 41, "allowable_bending_t_m": 1.0},
            "damage_cases": [
                {
                    "name": "sealed",
                    "length_m": 4.0,
                    "x_center_m": 20.0,
                    "permeability": 0.0,
                }
            ],
        }
    )
    assert result["equilibrium"]["draft_m"] == pytest.approx(2.0, abs=1e-10, rel=0)
    assert result["equilibrium"]["gm_t_m"] == pytest.approx(
        3.166666666666667, abs=1e-10, rel=0
    )
    assert result["intact_stability"]["angles_deg"] == [0.0, 10.0, 20.0, 30.0, 40.0]
    assert result["damage_stability"][0]["damaged_draft_m"] == pytest.approx(
        2.0, abs=1e-10, rel=0
    )
    assert result["longitudinal_strength"]["max_shear_t"] == pytest.approx(
        0.0, abs=1e-10, rel=0
    )
    assert result["longitudinal_strength"]["max_bending_t_m"] == pytest.approx(
        0.0, abs=1e-10, rel=0
    )
    assert result["longitudinal_strength"]["screening_status"] == "pass"
