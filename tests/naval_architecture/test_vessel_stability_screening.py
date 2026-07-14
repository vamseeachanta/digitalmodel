# ABOUTME: Core tests for vessel_stability_screening — synthetic box-barge
# ABOUTME: fixture with closed-form hand checks (Barrass & Derrett / PNA form).
"""Core tests for :mod:`digitalmodel.naval_architecture.vessel_stability_screening`.

Synthetic box-barge golden fixture (public textbook closed forms — Barrass &
Derrett "Ship Stability for Masters and Mates" box-barge examples, PNA Vol. I
hydrostatics; no client-derived values):

* Barge L = 100 m, B = 20 m, depth D = 10 m, seawater rho = 1.025 t/m3.
* Hydrostatics at draft T: displacement = rho*L*B*T = 2050*T;
  KB = T/2; BMt = B^2/(12*T); KM = KB + BMt; LCB = LCF = 50.0 m (from AP);
  MCT1cm = displacement * BML / (100*L) with BML = L^2/(12*T)
  = 2050*T * (10000/(12*T)) / 10000 = 170.8333 t*m/cm (draft-independent);
  TPC = rho*L*B/100 = 20.5.
* Loading condition (chosen for exact hand arithmetic):
    lightship 4000 t @ vcg 5.0, lcg 50.0
    cargo     3000 t @ vcg 6.0, lcg 52.0
    fuel      1000 t @ vcg 2.0, lcg 40.0, FSM 410 t*m
    ballast    200 t @ vcg 1.0, lcg 50.0
  Totals: W = 8200 t -> T = 4.0 m exactly; KM = 2 + 400/48 = 10.333333 m;
  KG = 40200/8200 = 4.9024390 m; FSC = 410/8200 = 0.0500000 m exactly;
  KG_fluid = 4.9524390 m; GM_solid = 5.4308943 m; GM_fluid = 5.3808943 m;
  LCG = 406000/8200 = 49.5121951 m; trimming moment = 8200*(50 - LCG)
  = 4000.0 t*m -> trim = 4000/170.8333 = 23.414634 cm = 0.2341463 m by the
  stern; T_aft = 4 + 0.5*trim = 4.1170732 m, T_fwd = 3.8829268 m (LCF at
  midship).
* KN cross-curves from the wall-sided formula (synthetic fixture, valid to
  deck-edge immersion atan((D-T)/(B/2)) = 30.96 deg, tabulated to 40 deg as
  a smooth synthetic extension):
    KN(phi) = KM*sin(phi) + 0.5*BMt*tan^2(phi)*sin(phi),   BMt = 8.3333333
  so GZ = KN - KG_f*sin(phi) = GM_f*sin(phi) + 0.5*BMt*tan^2(phi)*sin(phi).
  Hand values: GZ(30) = 5.3808943*0.5 + 4.1666667*(1/3)*0.5 = 3.3848905 m;
  analytic areas (integral of the wall-sided GZ):
    area(0-a) = GM_f*(1-cos a) + 0.5*BMt*[(sec a - 1) - (1 - cos a)]
    area(0-30) = 0.8072556 m*rad; area(0-40) = 1.5566685 m*rad;
    area(30-40) = 0.7494129 m*rad (trapezoid on the 5-deg table agrees to
    ~0.3%).
* 46 CFR 170.170 weather criterion with config P = 0.055 t/m2, A = 600 m2,
  H = 5.0 m, T = min(14 deg, deck edge 30.96 deg) = 14 deg:
    GM_required = 0.055*600*5 / (8200*tan 14) = 165/2044.4896 = 0.0807046 m.
* Crane lift 100 t at 15 m outreach -> HM = 1500 t*m, HA(0) = 0.1829268 m.
  On the 5-deg tabulated curve GZ is linear 0-5 deg with slope
  GZ(5)/5 = (KN(5) - KG_f*sin 5)/5 = 0.4717565/5 = 0.0943513 m/deg, so the
  static equilibrium solves 0.0943513*phi = 0.1829268*cos(phi)
  -> phi = 1.9377 deg.
* Max-KG at W = 8200 t: GM0 >= 0.15 gives KG_limit = KM - 0.15
  = 10.1833333 m (governing); weather gives KM - 0.0807046 = 10.2526287 m;
  the GZ-curve criteria still pass at KG = KM (wall-sided term), so they
  report limited_by_km.
"""

import math

import pytest

from digitalmodel.naval_architecture.vessel_stability_screening import (
    Citation,
    CraneHeelingCase,
    GZCurve,
    HydroRow,
    HydrostaticTable,
    ImoIntactCriteria,
    LiftingCriteria,
    WeatherCriterion,
    WeightItem,
    build_loading_condition,
    equilibrium_heel_deg,
    governing_kg_limit,
    gz_from_kn,
    imo_intact_criteria,
    kn_at_displacement,
    lifting_check,
    max_kg_screening,
    rectangular_fsm_t_m,
    solve_equilibrium,
    weather_criterion_cfr_170_170,
)

L, B, D = 100.0, 20.0, 10.0
RHO = 1.025
MCT = 170.8333333  # t*m/cm, draft-independent for the box barge
KM_4 = 2.0 + 400.0 / 48.0  # 10.3333333 at T = 4 m
BMT_4 = 400.0 / 48.0  # 8.3333333 at T = 4 m
W = 8200.0
KG_FLUID = 4.9524390
GM_FLUID = KM_4 - KG_FLUID
HEELS = [0.0, 5.0, 10.0, 15.0, 20.0, 25.0, 30.0, 35.0, 40.0]


def _hydro_table() -> HydrostaticTable:
    rows = []
    for t in (2.0, 3.0, 4.0, 5.0, 6.0, 8.0):
        rows.append(
            HydroRow(
                draft_m=t,
                displacement_t=RHO * L * B * t,
                km_m=t / 2.0 + B**2 / (12.0 * t),
                lcb_m=50.0,
                lcf_m=50.0,
                mct_t_m_per_cm=MCT,
                tpc_t_per_cm=20.5,
            )
        )
    return HydrostaticTable(rows=tuple(rows))


def _items() -> list[WeightItem]:
    return [
        WeightItem("lightship", 4000.0, 5.0, 50.0),
        WeightItem("cargo", 3000.0, 6.0, 52.0),
        WeightItem("fuel", 1000.0, 2.0, 40.0, fsm_t_m=410.0),
        WeightItem("ballast", 200.0, 1.0, 50.0),
    ]


def _kn(heel_deg: list[float], km: float = KM_4, bmt: float = BMT_4) -> list[float]:
    """Wall-sided KN: KM*sin + 0.5*BMt*tan^2*sin (synthetic cross-curve)."""
    out = []
    for phi in heel_deg:
        r = math.radians(phi)
        out.append(km * math.sin(r) + 0.5 * bmt * math.tan(r) ** 2 * math.sin(r))
    return out


def _curve(kg_fluid: float = KG_FLUID) -> GZCurve:
    return gz_from_kn(HEELS, _kn(HEELS), kg_fluid)


def _weather() -> WeatherCriterion:
    return WeatherCriterion(
        wind_pressure_t_m2=0.055,
        windage_area_m2=600.0,
        windage_lever_m=5.0,
        max_heel_deg=14.0,
        citation=Citation("46 CFR 170.170", "2024", "170.170(a)"),
    )


def _lifting_criteria(**overrides) -> LiftingCriteria:
    kwargs = dict(
        max_equilibrium_heel_deg=5.0,
        residual_area_ratio_min=1.4,
        residual_range_limit_deg=40.0,
        citation=Citation("46 CFR Part 173 Subpart B", "2024", "173.005-series"),
    )
    kwargs.update(overrides)
    return LiftingCriteria(**kwargs)


# -- citations -----------------------------------------------------------------


def test_citation_requires_standard_edition_clause():
    with pytest.raises(ValueError, match="citation.edition"):
        Citation("46 CFR 170.170", "", "170.170(a)")


def test_citation_label_includes_note():
    cite = Citation("IMO IS Code", "2008", "2.2.1", note="general criteria")
    assert cite.label() == "IMO IS Code (2008) 2.2.1 — general criteria"


# -- hydrostatic table -----------------------------------------------------------


def test_table_exact_node_lookup():
    props = _hydro_table().at_displacement(8200.0)
    assert props.draft_m == pytest.approx(4.0)
    assert props.km_m == pytest.approx(10.3333333, abs=1e-6)
    assert props.mct_t_m_per_cm == pytest.approx(MCT, abs=1e-6)


def test_table_interpolates_between_drafts():
    # Midway between T=4 (8200 t) and T=5 (10250 t): W = 9225 t.
    props = _hydro_table().at_displacement(9225.0)
    assert props.draft_m == pytest.approx(4.5)
    # Linear-in-displacement KM: (10.3333333 + 9.1666667)/2 = 9.75
    assert props.km_m == pytest.approx(9.75, abs=1e-6)


def test_table_rejects_out_of_range():
    with pytest.raises(ValueError, match="outside hydrostatic table range"):
        _hydro_table().at_displacement(100.0)
    with pytest.raises(ValueError, match="outside hydrostatic table range"):
        _hydro_table().at_draft(9.0)


def test_table_rejects_non_monotonic_displacement():
    rows = (
        HydroRow(draft_m=2.0, displacement_t=5000.0, km_m=17.0),
        HydroRow(draft_m=3.0, displacement_t=4000.0, km_m=13.0),
    )
    with pytest.raises(ValueError, match="displacements must increase"):
        HydrostaticTable(rows=rows)


def test_table_needs_two_rows():
    with pytest.raises(ValueError, match=">= 2 rows"):
        HydrostaticTable(rows=(HydroRow(draft_m=2.0, displacement_t=4100.0, km_m=17.0),))


# -- loading condition -----------------------------------------------------------


def test_loading_condition_hand_totals():
    cond = build_loading_condition("departure", _items())
    assert cond.displacement_t == pytest.approx(8200.0)
    assert cond.kg_m == pytest.approx(40200.0 / 8200.0, abs=1e-9)
    assert cond.lcg_m == pytest.approx(406000.0 / 8200.0, abs=1e-9)
    assert cond.fsm_total_t_m == pytest.approx(410.0)
    assert cond.fsc_m == pytest.approx(0.05, abs=1e-12)
    assert cond.kg_fluid_m == pytest.approx(cond.kg_m + 0.05, abs=1e-12)


def test_rectangular_fsm_hand_value():
    # rho*l*b^3/12 = 0.85 * 12 * 8^3 / 12 = 435.2 t*m
    assert rectangular_fsm_t_m(12.0, 8.0, 0.85) == pytest.approx(435.2)


def test_weight_item_rejects_nonpositive_weight():
    with pytest.raises(ValueError, match="weight_t must be > 0"):
        WeightItem("bad", 0.0, 1.0, 1.0)


def test_empty_condition_rejected():
    with pytest.raises(ValueError, match="at least one weight item"):
        build_loading_condition("empty", [])


# -- equilibrium (draft / GM / trim) ----------------------------------------------


def test_equilibrium_hand_values():
    cond = build_loading_condition("departure", _items())
    eq = solve_equilibrium(cond, _hydro_table(), lbp_m=100.0)
    assert eq.draft_m == pytest.approx(4.0)
    assert eq.km_m == pytest.approx(10.3333333, abs=1e-6)
    assert eq.gm_solid_m == pytest.approx(5.4308943, abs=1e-6)
    assert eq.gm_fluid_m == pytest.approx(5.3808943, abs=1e-6)
    # trim = W*(LCB-LCG)/MCT = 4000/170.8333 cm = 0.2341463 m by the stern
    assert eq.trim_m == pytest.approx(0.2341463, abs=1e-6)
    assert eq.draft_aft_m == pytest.approx(4.1170732, abs=1e-6)
    assert eq.draft_fwd_m == pytest.approx(3.8829268, abs=1e-6)


def test_equilibrium_without_lcb_mct_skips_trim():
    rows = tuple(
        HydroRow(draft_m=t, displacement_t=2050.0 * t, km_m=t / 2 + 400.0 / (12 * t))
        for t in (2.0, 4.0, 6.0)
    )
    cond = build_loading_condition("departure", _items())
    eq = solve_equilibrium(cond, HydrostaticTable(rows=rows))
    assert eq.trim_m is None
    assert eq.draft_aft_m is None
    assert eq.gm_fluid_m == pytest.approx(5.3808943, abs=1e-6)


# -- GZ curve ---------------------------------------------------------------------


def test_gz_from_kn_hand_value_at_30():
    curve = _curve()
    # GZ(30) = KN(30) - KG_f*sin30 = 5.8611111 - 2.4762195 = 3.3848916
    assert curve.gz_at(30.0) == pytest.approx(3.3848916, abs=1e-5)


def test_gz_areas_match_wall_sided_analytic():
    curve = _curve()
    assert curve.area_m_rad(0.0, 30.0) == pytest.approx(0.8072556, rel=5e-3)
    assert curve.area_m_rad(0.0, 40.0) == pytest.approx(1.5566685, rel=5e-3)
    assert curve.area_m_rad(30.0, 40.0) == pytest.approx(0.7494129, rel=5e-3)


def test_gz_curve_validation():
    with pytest.raises(ValueError, match="strictly increasing"):
        GZCurve(heel_deg=(0.0, 10.0, 10.0), gz_m=(0.0, 1.0, 2.0))
    with pytest.raises(ValueError, match="start at 0"):
        GZCurve(heel_deg=(5.0, 10.0, 15.0), gz_m=(0.0, 1.0, 2.0))
    with pytest.raises(ValueError, match="lengths differ"):
        gz_from_kn([0.0, 10.0], [0.0], 5.0)


def test_kn_grid_interpolation_in_displacement():
    heels = [0.0, 10.0, 20.0]
    grid = [[0.0, 1.0, 2.0], [0.0, 3.0, 4.0]]
    kn = kn_at_displacement(heels, [4100.0, 8200.0], grid, 6150.0)
    assert kn == pytest.approx([0.0, 2.0, 3.0])
    with pytest.raises(ValueError, match="outside KN grid range"):
        kn_at_displacement(heels, [4100.0, 8200.0], grid, 9000.0)


# -- IMO IS Code 2008 intact criteria -----------------------------------------------


def test_imo_intact_all_pass_with_hand_values():
    results = imo_intact_criteria(_curve(), GM_FLUID)
    by_key = {r.key: r for r in results}
    assert len(results) == 6
    assert all(r.passed for r in results)
    assert by_key["imo_area_0_30"].value == pytest.approx(0.8072556, rel=5e-3)
    assert by_key["imo_area_0_30"].required == 0.055
    assert by_key["imo_gz_30"].value == pytest.approx(3.3848916, abs=1e-5)
    assert by_key["imo_angle_max_gz"].value == pytest.approx(40.0)
    assert by_key["imo_gm0"].value == pytest.approx(GM_FLUID, abs=1e-6)
    assert "IMO IS Code 2008" in by_key["imo_gm0"].citation
    assert "MSC.267(85)" in by_key["imo_gm0"].citation


def test_imo_intact_caps_areas_at_downflooding_angle():
    results = imo_intact_criteria(_curve(), GM_FLUID, downflooding_angle_deg=35.0)
    by_key = {r.key: r for r in results}
    # area(0-35) analytic = GM_f*(1-cos35) + 4.1666667*((sec35-1)-(1-cos35))
    #                     = 5.3808943*0.1808479 + 4.1666667*(0.2207745-0.1808479)
    #                     = 0.9731287 + 0.1663608 = 1.1394895 m*rad
    assert by_key["imo_area_0_40"].value == pytest.approx(1.1394895, rel=5e-3)
    assert "35" in by_key["imo_area_0_40"].description


def test_imo_intact_negative_gm_fails_gm0_and_overall():
    # FSM-dominant condition: KG_fluid pushed above KM (free-surface loss).
    results = imo_intact_criteria(_curve(kg_fluid=10.6), -0.2666667)
    by_key = {r.key: r for r in results}
    assert not by_key["imo_gm0"].passed
    assert by_key["imo_gm0"].margin == pytest.approx(-0.4166667, abs=1e-6)
    assert not all(r.passed for r in results)


def test_imo_intact_criteria_overridable_with_new_citation():
    custom = ImoIntactCriteria(
        gm0_min_m=6.0,
        citation=Citation("Flag criteria", "2020", "annex 1"),
    )
    results = imo_intact_criteria(_curve(), GM_FLUID, criteria=custom)
    by_key = {r.key: r for r in results}
    assert not by_key["imo_gm0"].passed  # 5.38 < 6.0
    assert by_key["imo_gm0"].citation == "Flag criteria (2020) annex 1"


# -- 46 CFR 170.170 weather criterion ------------------------------------------------


def test_weather_criterion_hand_value():
    result = weather_criterion_cfr_170_170(
        _weather(), W, GM_FLUID, deck_edge_immersion_deg=30.9637565
    )
    # GM_required = 0.055*600*5/(8200*tan14) = 165/2044.4896 = 0.0807046 m
    assert result.required == pytest.approx(0.0807046, abs=1e-6)
    assert result.value == pytest.approx(GM_FLUID, abs=1e-6)
    assert result.passed
    assert "46 CFR 170.170" in result.citation


def test_weather_criterion_deck_edge_caps_heel_angle():
    # Deck edge at 10 deg < 14 deg: T = 10 deg -> required grows by
    # tan14/tan10 = 0.2493280/0.1763270 = 1.4140...
    result = weather_criterion_cfr_170_170(
        _weather(), W, GM_FLUID, deck_edge_immersion_deg=10.0
    )
    assert result.required == pytest.approx(
        165.0 / (8200.0 * math.tan(math.radians(10.0))), abs=1e-6
    )


def test_weather_criterion_fails_low_gm():
    result = weather_criterion_cfr_170_170(_weather(), W, 0.05)
    assert not result.passed
    assert result.margin == pytest.approx(0.05 - 0.0807046, abs=1e-6)


def test_weather_criterion_requires_positive_inputs():
    with pytest.raises(ValueError, match="wind_pressure_t_m2 must be > 0"):
        WeatherCriterion(
            wind_pressure_t_m2=0.0,
            windage_area_m2=600.0,
            windage_lever_m=5.0,
            citation=Citation("46 CFR 170.170", "2024", "170.170(a)"),
        )


# -- lifting / crane heel -------------------------------------------------------------


def test_crane_case_from_hook_load():
    case = CraneHeelingCase.from_hook_load("main crane", 100.0, 15.0)
    assert case.heeling_moment_t_m == pytest.approx(1500.0)
    assert case.heeling_arm_m(W, 0.0) == pytest.approx(0.1829268, abs=1e-6)
    # cosine decay: HA(30) = HA(0)*cos30
    assert case.heeling_arm_m(W, 30.0) == pytest.approx(
        0.1829268 * math.cos(math.radians(30.0)), abs=1e-6
    )


def test_equilibrium_heel_hand_value():
    # Linear GZ 0-5 deg with slope 0.0943513 m/deg (see module docstring):
    # 0.0943513*phi = 0.1829268*cos(phi) -> phi = 1.9377 deg
    case = CraneHeelingCase.from_hook_load("main crane", 100.0, 15.0)
    eq = equilibrium_heel_deg(_curve(), case, W)
    assert eq == pytest.approx(1.9377, abs=5e-3)


def test_lifting_check_passes_criteria():
    case = CraneHeelingCase.from_hook_load("main crane", 100.0, 15.0)
    result = lifting_check(
        _curve(), case, W, _lifting_criteria(), downflooding_angle_deg=None
    )
    assert result.passed
    assert result.equilibrium_heel_deg == pytest.approx(1.9377, abs=5e-3)
    by_key = {c.key: c for c in result.criteria}
    assert by_key["lifting_equilibrium_heel"].required == pytest.approx(5.0)
    # Heeling energy (exact): (HM/W)*sin(eq) = 0.1829268*sin(1.9377 deg)
    assert result.heeling_area_m_rad == pytest.approx(
        0.1829268 * math.sin(math.radians(result.equilibrium_heel_deg)), rel=1e-6
    )
    assert result.residual_area_ratio > 100.0  # barge GZ dwarfs the lift
    assert "46 CFR Part 173" in by_key["lifting_residual_area_ratio"].citation


def test_lifting_equilibrium_heel_exceeding_limit_fails():
    # 10x the hook load: HA(0) = 1.8293 m; solve on the tabulated curve.
    case = CraneHeelingCase.from_hook_load("heavy lift", 1000.0, 15.0)
    result = lifting_check(_curve(), case, W, _lifting_criteria())
    by_key = {c.key: c for c in result.criteria}
    assert result.equilibrium_heel_deg > 5.0
    assert not by_key["lifting_equilibrium_heel"].passed
    assert not result.passed


def test_lifting_no_static_equilibrium_when_arm_exceeds_gz():
    # HM/W = 8 m: HA exceeds GZ over the whole tabulated range (max GZ
    # = 5.344 m at 40 deg < 8*cos40 = 6.13 m) -> no equilibrium, hard fail.
    case = CraneHeelingCase(name="capsizing lift", heeling_moment_t_m=8.0 * W)
    result = lifting_check(_curve(), case, W, _lifting_criteria())
    assert result.equilibrium_heel_deg is None
    assert not result.passed
    by_key = {c.key: c for c in result.criteria}
    assert by_key["lifting_equilibrium_heel"].value is None
    assert "no static equilibrium" in by_key["lifting_equilibrium_heel"].description


def test_lifting_deck_edge_caps_heel_limit():
    case = CraneHeelingCase.from_hook_load("main crane", 100.0, 15.0)
    result = lifting_check(
        _curve(), case, W, _lifting_criteria(), deck_edge_immersion_deg=1.0
    )
    by_key = {c.key: c for c in result.criteria}
    assert by_key["lifting_equilibrium_heel"].required == pytest.approx(1.0)
    assert not by_key["lifting_equilibrium_heel"].passed


def test_lifting_criteria_require_citation():
    with pytest.raises(TypeError):
        LiftingCriteria(max_equilibrium_heel_deg=5.0)  # citation is required


# -- max-KG screening -----------------------------------------------------------------


def test_max_kg_gm0_governs_with_hand_value():
    limits = max_kg_screening(HEELS, _kn(HEELS), KM_4, W, weather=_weather())
    by_key = {limit.key: limit for limit in limits}
    # GM0 >= 0.15 -> KG_limit = KM - 0.15 = 10.1833333 m
    assert by_key["imo_gm0"].kg_limit_m == pytest.approx(10.1833333, abs=1e-3)
    assert not by_key["imo_gm0"].limited_by_km
    # Weather: KG_limit = KM - 0.0807046 = 10.2526287 m
    assert by_key["cfr_170_170_weather_gm"].kg_limit_m == pytest.approx(
        10.2526287, abs=1e-3
    )
    # Wall-sided GZ criteria still pass at KG = KM -> limited_by_km
    for key in ("imo_area_0_30", "imo_area_0_40", "imo_area_30_40",
                "imo_gz_30", "imo_angle_max_gz"):
        assert by_key[key].limited_by_km, key
        assert by_key[key].kg_limit_m == pytest.approx(KM_4, abs=1e-9)
    governing = governing_kg_limit(limits)
    assert governing.key == "imo_gm0"
    assert governing.kg_limit_m == pytest.approx(10.1833333, abs=1e-3)


def test_max_kg_with_lifting_hand_value():
    # Equilibrium heel hits the 5-deg limit when GZ(5) = HA(5):
    # KN(5) - KG*sin5 = 0.1829268*cos5 -> KG = (0.9033889 - 0.1822313)
    # / 0.0871557 = 8.2744 m -> lifting governs over GM0 (10.18 m).
    case = CraneHeelingCase.from_hook_load("main crane", 100.0, 15.0)
    limits = max_kg_screening(
        HEELS, _kn(HEELS), KM_4, W,
        lifting_case=case, lifting=_lifting_criteria(),
    )
    by_key = {limit.key: limit for limit in limits}
    assert by_key["lifting_equilibrium_heel"].kg_limit_m == pytest.approx(
        8.2744, abs=5e-3
    )
    governing = governing_kg_limit(limits)
    assert governing.key == "lifting_equilibrium_heel"


def test_max_kg_criterion_failing_at_zero_kg_reports_none():
    strict = ImoIntactCriteria(
        gz_30_min_m=100.0,
        citation=Citation("synthetic", "2026", "edge case"),
    )
    limits = max_kg_screening(HEELS, _kn(HEELS), KM_4, W, imo=strict)
    by_key = {limit.key: limit for limit in limits}
    assert by_key["imo_gz_30"].kg_limit_m is None
    governing = governing_kg_limit(limits)
    assert governing.key == "imo_gz_30"
    assert governing.kg_limit_m is None
