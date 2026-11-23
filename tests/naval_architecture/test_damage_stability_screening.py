# ABOUTME: Core tests for damage_stability_screening — synthetic box-barge
# ABOUTME: flooding fixture with closed-form hand checks (added-weight method).
"""Core tests for :mod:`digitalmodel.naval_architecture.damage_stability_screening`.

Synthetic box-barge damage fixture (public textbook closed forms — Barrass &
Derrett box-barge hydrostatics, PNA Vol. I, wall-sided GZ; extends the
slice-1 intact fixture; no client-derived values):

* Same barge and intact condition as the slice-1 tests: L = 100 m, B = 20 m,
  D = 10 m, rho = 1.025 t/m3; intact W = 8200 t -> T = 4.0 m,
  KM = 10.3333333 m, KG = 4.9024390 m, FSC = 0.05 m, KG_f = 4.9524390 m,
  LCG = 49.5121951 m; hydrostatic table rows at T = 2..8 m with
  MCT1cm = 170.8333333 t*m/cm, LCB = LCF = 50 m.
* Flooded wing compartment (chosen for exact hand arithmetic):
  volume 500 m3, permeability 0.8 -> floodwater w = 1.025*0.8*500 = 410 t
  exactly, at vcg 2.0 m, lcg 30.0 m, tcg 4.0 m, floodwater FSM = 451 t*m
  (free communication).
* Added-weight equilibrium:
    W' = 8610 t -> table draft T' = 4.0 + 0.2*(5.0-4.0) = 4.2 m and
    KM' = 10.3333333 + 0.2*(9.1666667 - 10.3333333) = 10.1 m exactly
    (linear interpolation in displacement between the T = 4 and T = 5 rows).
    KG' = (8200*4.9024390 + 410*2.0)/8610 = 41020/8610 = 4.7642276 m;
    FSC' = (410 + 451)/8610 = 861/8610 = 0.1 m exactly;
    KG'_f = 4.8642276 m; GM'_f = 10.1 - 41881/8610 = 45080/8610
    = 5.2357724 m.
    Transverse flooding moment M_t = 410*4.0 = 1640 t*m and
    W'*GM'_f = 8610*(45080/8610) = 45080 exactly, so
    tan(heel) = 1640/45080 = 0.0363798 -> heel = 2.0835 deg.
    LCG' = (406000 + 12300)/8610 = 48.5830430 m; trimming moment
    = 430500 - 418300 = 12200 t*m -> trim = 12200/170.8333333
    = 71.4146341 cm = 0.7141463 m by the stern; T_aft = 4.2 + 0.3570732
    = 4.5570732 m, T_fwd = 3.8429268 m (LCF at midship, LBP = 100 m).
* Damaged KN cross-curve (synthetic wall-sided form at the flooded
  waterline, consistent with the table KM' = 10.1 m and
  BMt' = 400/(12*4.2) = 7.9365079 m):
    KN(phi) = KM'*sin(phi) + 0.5*BMt'*tan^2(phi)*sin(phi)
  so GZ = KN - KG'_f*sin(phi) = GM'_f*sin(phi)
  + 0.5*BMt'*tan^2(phi)*sin(phi):
    GZ(30) = 5.2357724*0.5 + 3.9682540*(1/3)*0.5 = 3.2792619 m.
* Flooding heeling arm HA(phi) = M_t*cos(phi)/W' = 0.1904762*cos(phi).
  On the 5-deg tabulated curve GZ is linear 0-5 deg with slope
  GZ(5)/5 = 0.4589749/5 = 0.0917950 m/deg, so the curve equilibrium
  solves 0.0917950*phi = 0.1904762*cos(phi) -> phi = 2.0737 deg
  (small-angle moment value above: 2.0835 deg — both are hand values).
* With downflooding at 30 deg: margin = 30 - 2.0737 = 27.9263 deg; the net
  arm stays positive to 30 deg, so range beyond equilibrium = 27.9263 deg.
  Residual area (analytic wall-sided GZ integral minus the exact heeling
  integral (M_t/W')*sin):
    area_GZ(0-30) = GM'_f*(1-cos30) + 0.5*BMt'*((sec30-1)-(1-cos30))
                  = 0.7014600 + 0.0822455 = 0.7837055 m*rad
    residual = 0.7837055 - 0.0034447 - 0.0883458 = 0.6919 m*rad (the 5-deg
    trapezoidal table integration agrees to ~0.4%).
  Heeling energy = (M_t/W')*sin(2.0737 deg) = 0.0068922 m*rad.
* Direct (licensed-tool supplied) path, no heeling arm: the supplied static
  heel 2.0835 deg anchors the analysis; range = 30 - 2.0835 = 27.9165 deg;
  residual area = area_GZ(2.0835-30) = 0.7825 m*rad (trapezoid, rel 1%).
* Constant wind arm only (no flooding moment), arm = 0.1904762 m:
  equilibrium solves 0.0917950*phi = 0.1904762 -> phi = 2.0750 deg.
* Negative post-damage GM edge case: floodwater FSM = 90000 t*m ->
  FSC' = 90410/8610 = 10.5006 m -> KG'_f = 15.2648 m > KM' -> GM'_f
  = -5.1648 m < 0 -> static heel is None and the case fails.
* Vanishing-angle fixture (piecewise-linear synthetic curve): heel
  (0, 10, 20, 30) deg, GZ (0, 0.5, 0.2, -0.3) m -> GZ crosses zero at
  20 + 0.2/0.5*10 = 24 deg; range (no arm) = 24 deg; positive area
  = 0.5*0.5*rad(10) + 0.5*0.7*rad(10) + 0.5*0.2*rad(4) = 0.1117010 m*rad.
"""

import math

import pytest

from digitalmodel.naval_architecture.damage_stability_screening import (
    DamageCriteria,
    FloodedCompartment,
    added_weight_equilibrium,
    damage_gz_analysis,
    evaluate_damage_case,
)
from digitalmodel.naval_architecture.vessel_stability_screening import (
    Citation,
    GZCurve,
    HydroRow,
    HydrostaticTable,
    WeightItem,
    build_loading_condition,
    gz_from_kn,
)

L, B = 100.0, 20.0
RHO = 1.025
MCT = 170.8333333
HEELS = [0.0, 5.0, 10.0, 15.0, 20.0, 25.0, 30.0, 35.0, 40.0]

W_DAMAGED = 8610.0
KM_DAMAGED = 10.1
BMT_DAMAGED = 400.0 / (12.0 * 4.2)  # 7.9365079
KG_FLUID_DAMAGED = 4.8642276
GM_FLUID_DAMAGED = 45080.0 / 8610.0  # 5.2357724
MOMENT = 1640.0


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


def _intact_condition():
    return build_loading_condition(
        "departure",
        [
            WeightItem("lightship", 4000.0, 5.0, 50.0),
            WeightItem("cargo", 3000.0, 6.0, 52.0),
            WeightItem("fuel", 1000.0, 2.0, 40.0, fsm_t_m=410.0),
            WeightItem("ballast", 200.0, 1.0, 50.0),
        ],
    )


def _compartment(**overrides) -> FloodedCompartment:
    kwargs = dict(
        name="wing tank",
        volume_m3=500.0,
        permeability=0.8,
        vcg_m=2.0,
        lcg_m=30.0,
        tcg_m=4.0,
        fsm_t_m=451.0,
        free_communication=True,
    )
    kwargs.update(overrides)
    return FloodedCompartment(**kwargs)


def _damaged_kn(heel_deg):
    out = []
    for phi in heel_deg:
        r = math.radians(phi)
        out.append(
            KM_DAMAGED * math.sin(r)
            + 0.5 * BMT_DAMAGED * math.tan(r) ** 2 * math.sin(r)
        )
    return out


def _damaged_curve() -> GZCurve:
    return gz_from_kn(HEELS, _damaged_kn(HEELS), KG_FLUID_DAMAGED)


def _criteria(**overrides) -> DamageCriteria:
    kwargs = dict(
        max_equilibrium_heel_deg=15.0,
        min_gm_m=0.05,
        min_downflooding_margin_deg=7.0,
        min_range_beyond_equilibrium_deg=7.0,
        min_residual_area_m_rad=0.05,
        citation=Citation(
            "IMO MODU Code 2009 (Res. A.1023(26))", "2009", "3.6"
        ),
    )
    kwargs.update(overrides)
    return DamageCriteria(**kwargs)


# -- damage-case definition ----------------------------------------------------


def test_floodwater_weight_hand_value():
    assert _compartment().floodwater_t(RHO) == pytest.approx(410.0)


def test_compartment_validation():
    with pytest.raises(ValueError, match="volume_m3 must be > 0"):
        _compartment(volume_m3=0.0)
    with pytest.raises(ValueError, match="permeability must be in"):
        _compartment(permeability=1.2)
    with pytest.raises(ValueError, match="permeability must be in"):
        _compartment(permeability=0.0)
    with pytest.raises(ValueError, match="fsm_t_m must be >= 0"):
        _compartment(fsm_t_m=-1.0)


def test_free_communication_requires_floodwater_fsm():
    with pytest.raises(ValueError, match="free-communication"):
        _compartment(fsm_t_m=0.0, free_communication=True)
    # Not free-communicating: zero FSM is acceptable (e.g. pressed-full).
    assert _compartment(fsm_t_m=0.0, free_communication=False).fsm_t_m == 0.0


# -- post-damage equilibrium (added-weight) --------------------------------------


def test_added_weight_equilibrium_hand_values():
    eq = added_weight_equilibrium(
        _intact_condition(), _hydro_table(), [_compartment()], RHO, lbp_m=100.0
    )
    assert eq.displacement_t == pytest.approx(8610.0)
    assert eq.draft_m == pytest.approx(4.2, abs=1e-9)
    assert eq.km_m == pytest.approx(10.1, abs=1e-9)
    assert eq.kg_m == pytest.approx(4.7642276, abs=1e-6)
    assert eq.fsc_m == pytest.approx(0.1, abs=1e-12)
    assert eq.kg_fluid_m == pytest.approx(4.8642276, abs=1e-6)
    assert eq.gm_fluid_m == pytest.approx(5.2357724, abs=1e-6)
    assert eq.floodwater_t == pytest.approx(410.0)
    assert eq.transverse_moment_t_m == pytest.approx(1640.0)
    # tan(heel) = 1640/45080 -> 2.0835 deg
    assert eq.heel_deg == pytest.approx(2.0835, abs=5e-4)
    assert eq.trim_m == pytest.approx(0.7141463, abs=1e-6)
    assert eq.draft_aft_m == pytest.approx(4.5570732, abs=1e-6)
    assert eq.draft_fwd_m == pytest.approx(3.8429268, abs=1e-6)


def test_added_weight_heel_uses_moment_magnitude():
    port = added_weight_equilibrium(
        _intact_condition(), _hydro_table(), [_compartment(tcg_m=-4.0)], RHO
    )
    assert port.transverse_moment_t_m == pytest.approx(1640.0)
    assert port.heel_deg == pytest.approx(2.0835, abs=5e-4)


def test_added_weight_negative_gm_reports_no_heel():
    # FSM 90000 t*m: FSC' = 90410/8610 = 10.5006 m -> GM'_f = -5.1648 m.
    eq = added_weight_equilibrium(
        _intact_condition(), _hydro_table(), [_compartment(fsm_t_m=90000.0)], RHO
    )
    assert eq.gm_fluid_m == pytest.approx(10.1 - (4.7642276 + 90410.0 / 8610.0), abs=1e-4)
    assert eq.gm_fluid_m < 0.0
    assert eq.heel_deg is None


def test_added_weight_requires_compartments_and_table_range():
    with pytest.raises(ValueError, match=">= 1 flooded compartment"):
        added_weight_equilibrium(_intact_condition(), _hydro_table(), [])
    huge = _compartment(volume_m3=10000.0, permeability=1.0, fsm_t_m=451.0)
    with pytest.raises(ValueError, match="outside hydrostatic table range"):
        added_weight_equilibrium(_intact_condition(), _hydro_table(), [huge])


# -- damaged-GZ analysis -----------------------------------------------------------


def test_damaged_gz_hand_value_at_30():
    assert _damaged_curve().gz_at(30.0) == pytest.approx(3.2792619, abs=1e-5)


def test_gz_analysis_flooding_moment_hand_values():
    analysis = damage_gz_analysis(
        _damaged_curve(),
        W_DAMAGED,
        transverse_moment_t_m=MOMENT,
        limit_angle_deg=30.0,
    )
    assert analysis.has_heeling_arm
    # Curve equilibrium: 0.0917950*phi = 0.1904762*cos(phi) -> 2.0737 deg
    assert analysis.equilibrium_heel_deg == pytest.approx(2.0737, abs=5e-3)
    assert analysis.range_end_deg == pytest.approx(30.0)
    assert analysis.range_beyond_equilibrium_deg == pytest.approx(27.9263, abs=5e-3)
    assert analysis.residual_area_m_rad == pytest.approx(0.6919, rel=1e-2)
    assert analysis.heeling_area_m_rad == pytest.approx(0.0068922, abs=1e-5)
    assert analysis.area_ratio == pytest.approx(
        analysis.residual_area_m_rad / analysis.heeling_area_m_rad
    )


def test_gz_analysis_direct_static_heel_no_arm():
    analysis = damage_gz_analysis(
        _damaged_curve(), W_DAMAGED, limit_angle_deg=30.0, static_heel_deg=2.0835
    )
    assert not analysis.has_heeling_arm
    assert analysis.equilibrium_heel_deg == pytest.approx(2.0835)
    assert analysis.range_beyond_equilibrium_deg == pytest.approx(27.9165, abs=1e-6)
    assert analysis.residual_area_m_rad == pytest.approx(0.7825, rel=1e-2)
    assert analysis.heeling_area_m_rad is None
    assert analysis.area_ratio is None


def test_gz_analysis_constant_wind_arm_hand_value():
    # GZ(phi) = 0.1904762 (constant arm) -> phi = 0.1904762/0.0917950 = 2.0750
    analysis = damage_gz_analysis(
        _damaged_curve(), W_DAMAGED, wind_heeling_arm_m=1640.0 / 8610.0
    )
    assert analysis.equilibrium_heel_deg == pytest.approx(2.0750, abs=5e-3)
    # Constant-arm heeling energy: arm * eq_rad
    assert analysis.heeling_area_m_rad == pytest.approx(
        (1640.0 / 8610.0) * math.radians(analysis.equilibrium_heel_deg), rel=1e-6
    )


def test_gz_analysis_vanishing_angle_caps_range():
    curve = GZCurve(heel_deg=(0.0, 10.0, 20.0, 30.0), gz_m=(0.0, 0.5, 0.2, -0.3))
    analysis = damage_gz_analysis(curve, 1000.0)
    assert analysis.equilibrium_heel_deg == pytest.approx(0.0)
    assert analysis.range_end_deg == pytest.approx(24.0, abs=1e-2)
    assert analysis.range_beyond_equilibrium_deg == pytest.approx(24.0, abs=1e-2)
    assert analysis.residual_area_m_rad == pytest.approx(0.1117010, rel=1e-3)


def test_gz_analysis_no_static_equilibrium():
    # Constant 6 m arm exceeds GZ everywhere (max GZ ~ 5.16 m at 40 deg).
    analysis = damage_gz_analysis(
        _damaged_curve(), W_DAMAGED, wind_heeling_arm_m=6.0
    )
    assert analysis.equilibrium_heel_deg is None
    assert analysis.range_beyond_equilibrium_deg is None
    assert analysis.residual_area_m_rad is None
    assert analysis.area_ratio is None


def test_gz_analysis_equilibrium_beyond_limit_zeroes_range():
    analysis = damage_gz_analysis(
        _damaged_curve(),
        W_DAMAGED,
        transverse_moment_t_m=MOMENT,
        limit_angle_deg=1.5,
    )
    assert analysis.equilibrium_heel_deg == pytest.approx(2.0737, abs=5e-3)
    assert analysis.range_beyond_equilibrium_deg == 0.0
    assert analysis.residual_area_m_rad == 0.0


def test_gz_analysis_input_validation():
    with pytest.raises(ValueError, match="displacement_t must be > 0"):
        damage_gz_analysis(_damaged_curve(), 0.0)
    with pytest.raises(ValueError, match="transverse_moment_t_m must be >= 0"):
        damage_gz_analysis(_damaged_curve(), W_DAMAGED, transverse_moment_t_m=-1.0)
    with pytest.raises(ValueError, match="wind_heeling_arm_m must be >= 0"):
        damage_gz_analysis(_damaged_curve(), W_DAMAGED, wind_heeling_arm_m=-0.1)
    with pytest.raises(ValueError, match="outside the tabulated GZ range"):
        damage_gz_analysis(_damaged_curve(), W_DAMAGED, static_heel_deg=60.0)


# -- cited survival criteria ---------------------------------------------------------


def test_damage_criteria_require_threshold_and_citation():
    with pytest.raises(TypeError):
        DamageCriteria(max_equilibrium_heel_deg=15.0)  # citation is required
    with pytest.raises(ValueError, match="at least one threshold"):
        DamageCriteria(citation=Citation("46 CFR Part 174", "2024", "subpart C"))
    with pytest.raises(ValueError, match="max_equilibrium_heel_deg must be > 0"):
        _criteria(max_equilibrium_heel_deg=0.0)
    with pytest.raises(ValueError, match="min_downflooding_margin_deg"):
        _criteria(min_downflooding_margin_deg=-1.0)


def test_evaluate_damage_case_passes_with_hand_margins():
    analysis = damage_gz_analysis(
        _damaged_curve(),
        W_DAMAGED,
        transverse_moment_t_m=MOMENT,
        limit_angle_deg=30.0,
    )
    result = evaluate_damage_case(
        "wing tank flooded",
        "added_weight",
        W_DAMAGED,
        GM_FLUID_DAMAGED,
        _criteria(min_area_ratio=1.0),
        heel_deg=2.0835,
        limit_angle_deg=30.0,
        analysis=analysis,
    )
    assert result.passed
    by_key = {c.key: c for c in result.criteria}
    assert set(by_key) == {
        "damage_equilibrium_heel",
        "damage_gm",
        "damage_downflooding_margin",
        "damage_range",
        "damage_residual_area",
        "damage_area_ratio",
    }
    # Curve equilibrium supersedes the small-angle moment heel.
    assert by_key["damage_equilibrium_heel"].value == pytest.approx(2.0737, abs=5e-3)
    assert by_key["damage_downflooding_margin"].value == pytest.approx(
        27.9263, abs=5e-3
    )
    assert by_key["damage_gm"].value == pytest.approx(5.2357724, abs=1e-6)
    assert by_key["damage_range"].value == pytest.approx(27.9263, abs=5e-3)
    assert all("A.1023(26)" in c.citation for c in result.criteria)


def test_evaluate_damage_case_negative_gm_fails():
    result = evaluate_damage_case(
        "flooded, unstable",
        "added_weight",
        W_DAMAGED,
        -5.1648,
        _criteria(
            min_range_beyond_equilibrium_deg=None, min_residual_area_m_rad=None
        ),
        heel_deg=None,
        limit_angle_deg=30.0,
    )
    assert not result.passed
    by_key = {c.key: c for c in result.criteria}
    assert not by_key["damage_gm"].passed
    assert by_key["damage_equilibrium_heel"].value is None
    assert not by_key["damage_equilibrium_heel"].passed
    assert "no static equilibrium" in by_key["damage_equilibrium_heel"].description
    assert by_key["damage_downflooding_margin"].value is None
    assert not by_key["damage_downflooding_margin"].passed


def test_evaluate_damage_case_equilibrium_beyond_downflooding_fails():
    analysis = damage_gz_analysis(
        _damaged_curve(),
        W_DAMAGED,
        transverse_moment_t_m=MOMENT,
        limit_angle_deg=1.5,
    )
    result = evaluate_damage_case(
        "wing tank flooded",
        "added_weight",
        W_DAMAGED,
        GM_FLUID_DAMAGED,
        _criteria(),
        heel_deg=2.0835,
        limit_angle_deg=1.5,
        analysis=analysis,
    )
    by_key = {c.key: c for c in result.criteria}
    assert by_key["damage_downflooding_margin"].value == pytest.approx(
        1.5 - 2.0737, abs=5e-3
    )
    assert not by_key["damage_downflooding_margin"].passed
    assert by_key["damage_range"].value == 0.0
    assert not by_key["damage_range"].passed
    assert not result.passed


def test_evaluate_damage_case_missing_inputs_raise():
    with pytest.raises(ValueError, match="no downflooding_angle_deg"):
        evaluate_damage_case(
            "case",
            "direct",
            W_DAMAGED,
            1.0,
            _criteria(
                min_range_beyond_equilibrium_deg=None,
                min_residual_area_m_rad=None,
            ),
            heel_deg=2.0,
            limit_angle_deg=None,
        )
    with pytest.raises(ValueError, match="require damaged GZ or KN data"):
        evaluate_damage_case(
            "case",
            "direct",
            W_DAMAGED,
            1.0,
            _criteria(min_downflooding_margin_deg=None),
            heel_deg=2.0,
            limit_angle_deg=30.0,
            analysis=None,
        )
    no_arm = damage_gz_analysis(
        _damaged_curve(), W_DAMAGED, limit_angle_deg=30.0, static_heel_deg=2.0835
    )
    with pytest.raises(ValueError, match="min_area_ratio requires a heeling arm"):
        evaluate_damage_case(
            "case",
            "direct",
            W_DAMAGED,
            GM_FLUID_DAMAGED,
            _criteria(min_area_ratio=1.0),
            heel_deg=2.0835,
            limit_angle_deg=30.0,
            analysis=no_arm,
        )
    with pytest.raises(ValueError, match="method must be"):
        evaluate_damage_case(
            "case", "lost_buoyancy", W_DAMAGED, 1.0, _criteria(), heel_deg=1.0,
            limit_angle_deg=30.0,
            analysis=damage_gz_analysis(_damaged_curve(), W_DAMAGED),
        )


def test_evaluate_damage_case_no_static_equilibrium_fails_gz_criteria():
    analysis = damage_gz_analysis(
        _damaged_curve(), W_DAMAGED, wind_heeling_arm_m=6.0, limit_angle_deg=30.0
    )
    result = evaluate_damage_case(
        "capsizing flood + wind",
        "direct",
        W_DAMAGED,
        GM_FLUID_DAMAGED,
        _criteria(min_area_ratio=1.0),
        heel_deg=None,
        limit_angle_deg=30.0,
        analysis=analysis,
    )
    assert not result.passed
    by_key = {c.key: c for c in result.criteria}
    for key in (
        "damage_equilibrium_heel",
        "damage_range",
        "damage_residual_area",
        "damage_area_ratio",
    ):
        assert by_key[key].value is None, key
        assert not by_key[key].passed, key
