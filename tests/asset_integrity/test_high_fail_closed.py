# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Regression probes for the high-severity input and disposition findings."""

from copy import deepcopy
from math import inf, nan
from types import SimpleNamespace
from pathlib import Path
import json

import pandas as pd
import pytest

from digitalmodel.asset_integrity import composite_repair as repair
from digitalmodel.asset_integrity import corroded_pipe as pipe
from digitalmodel.asset_integrity import dnv_rp_f101 as dnv
from digitalmodel.asset_integrity.circumferential_defect import (
    api579_part5_circumferential_netsection,
    api579_part5_longitudinal_rsf,
)
from digitalmodel.asset_integrity.dent_assessment import assess_dent
from digitalmodel.asset_integrity.ffs_acceptance_curves import pipe_acceptance_curve
from digitalmodel.asset_integrity.ffs_lookup import (
    build_lookup, evaluate_pipe_corroded, evaluate_plate_metal_loss,
    query_index, write_lookup,
)
from digitalmodel.asset_integrity.inspection_planning import plan_inspection
from digitalmodel.asset_integrity.pipeline_skill import PipelineInput, pipeline_integrity
from digitalmodel.asset_integrity.rbi_screening import (
    pof_category, rbi_rank_register, screen_ffs_result,
)
from digitalmodel.asset_integrity.riser_joint_ffs import (
    RiserJointFFSWorkflow, level1_flaw_envelope, place_joint,
)
from digitalmodel.asset_integrity.rsf_calculations import (
    calculate_rsf, check_ffs_level1, process_ut_grid,
)
from digitalmodel.asset_integrity.rstreng_2d import rstreng_2d_river_bottom


def _context(**changes):
    values = dict(active_leak=False, through_wall_now=False,
                  through_wall_within_repair_life=False, axial_load=False,
                  defect_type="external_metal_loss", service="oil",
                  pressure_psi=1000.0, temperature_f=100.0, repair_life_yr=5.0)
    return {**values, **changes}


def _source(**changes):
    values = dict(component_id="component", t_measured_min_in=.5, t_min_in=.25,
                  remaining_life_yr=10.0, rsf=1.0, rsf_a=.9, verdict="ACCEPT",
                  sufficiency_status="SUFFICIENT",
                  applicability=SimpleNamespace(ok=True))
    return SimpleNamespace(**{**values, **changes})


def _lookup(**changes):
    return evaluate_pipe_corroded(**{
        **dict(method="modified", grade="X52", D_in=12., t_in=.5,
               d_in=.2, L_in=10., maop_psi=1000.), **changes})


def test_A30_dent_flags_preserve_worst_action():
    result = assess_dent(12, .5, .2, 1, 1, on_weld=True)
    assert result.strain.eps_max > .06
    assert result.verdict == "NEEDS_ASSESSMENT"
    result = assess_dent(12, .5, .4, 1, 1, on_weld=True,
                         with_gouge_or_metal_loss=True)
    assert result.verdict == "REJECT"
    assert any("gouge" in note for note in result.validity_notes)


def test_A32_missing_repair_context_is_not_evaluated():
    result = repair.recommend_composite_repair().to_dict()
    assert result["evaluation_status"] == "NOT_EVALUATED"
    assert result["candidate_classes"] == []
    assert result["reinspection_interval_yr"] is None
    assert "service" in result["missing_fields"]
    result = repair.recommend_from_ffs_result(SimpleNamespace(verdict="REPAIR"))
    assert result.to_dict()["evaluation_status"] == "NOT_EVALUATED"
    from digitalmodel.asset_integrity.assessment.ffs_coordinator import FFSComponent, assess_component

    component = FFSComponent("component", "B31.8", 12.75, .5, 1000,
                             smys_psi=52000, corrosion_rate_in_per_yr=.005)
    grid = pd.DataFrame([[.5] * 6 for _ in range(6)])
    grid.iloc[2, 3] = .12
    result = assess_component(component, grid)
    assert result.verdict == "REPAIR"
    assert result.repair_recommendation["evaluation_status"] == "NOT_EVALUATED"
    assert result.repair_recommendation["candidate_classes"] == []


def test_A33_repair_context_and_limits_must_be_finite():
    for name in ("pressure_psi", "temperature_f", "repair_life_yr"):
        for value in (nan, inf, -1.):
            with pytest.raises(ValueError, match=name):
                repair.recommend_composite_repair(_context(**{name: value}))
    with pytest.raises(ValueError, match="pressure_psi"):
        repair.recommend_composite_repair({"pressure_psi": nan, "temperature_f": nan})
    with pytest.raises(ValueError):
        repair.CompositeRepairParameters(max_pressure_psi_by_class=dict.fromkeys("ABCDE", nan))
    with pytest.raises(ValueError):
        repair.CompositeRepairParameters(default_repair_life_yr=nan)
    params = repair.CompositeRepairParameters()
    params.max_pressure_psi_by_class["C"] = nan
    with pytest.raises(ValueError):
        repair.recommend_composite_repair(_context(), params=params)


def test_A34_inspection_inputs_and_derived_life_must_be_finite():
    for values in ((nan, 5, 10, .1), (10, nan, 10, .1),
                   (10, 5, nan, .1), (10, 5, 10, nan)):
        with pytest.raises(ValueError):
            plan_inspection(*values[:3], corrosion_rate=values[3])
    with pytest.raises(ValueError):
        plan_inspection(10, 5, 10, corrosion_rate=.1, required_remaining_life_years=nan)
    with pytest.raises(ValueError):
        plan_inspection(10, 5, 10, corrosion_rate=1e-320)


def test_A35_unknown_source_life_never_implies_zero_corrosion():
    for life in (None, nan, inf, 0., -1.):
        with pytest.raises(ValueError, match="life"):
            screen_ffs_result(_source(remaining_life_yr=life), "A")
    explicit = screen_ffs_result(_source(remaining_life_yr=nan), "A",
                                 corrosion_rate_in_per_yr=.01)
    assert explicit.inputs["corrosion_rate_mm_yr"] == pytest.approx(.254)


def test_A36_rbi_requires_eligible_source_assessment():
    for changes in (dict(verdict="ESCALATE", sufficiency_status="ESCALATE"),
                    dict(verdict="REPAIR"), dict(sufficiency_status="TAKE_MORE"),
                    dict(applicability=SimpleNamespace(ok=False)),
                    dict(verdict=None), dict(applicability=None)):
        with pytest.raises(ValueError, match="source"):
            screen_ffs_result(_source(**changes), "A")


def test_A37_unknown_corrosion_trend_requires_evidence():
    for rate in (None, nan, inf, -1.):
        with pytest.raises(ValueError, match="corrosion"):
            pof_category(rsf=1., rsf_a=.9, corrosion_rate_mm_yr=rate)


def test_A38_register_requires_mapped_consequence_evidence():
    register = pd.DataFrame([dict(component="pipe", joint_id="joint",
        measured_avg_wt_mm=12., min_required_wt_mm=6., corrosion_rate_mm_per_year=.1)])
    for fallback in ("C", "E"):
        with pytest.raises(ValueError, match="consequence"):
            rbi_rank_register(register, default_cof=fallback)
    assert rbi_rank_register(register, cof_by_joint={"joint": "E"}).iloc[0].cof_category == 5


def test_A39_pipe_strength_factor_and_demand_validation():
    for method in (pipe.b31g_original, pipe.modified_b31g):
        for strength, factor, demand in ((inf, 1.39, 1000), (0, 1.39, 1000),
            (52000, nan, 1000), (52000, .5, 1000), (52000, 1.39, 0),
            (52000, 1.39, nan)):
            with pytest.raises(ValueError):
                method(12, .5, .2, 10, strength, safety_factor=factor, maop_psi=demand)
    with pytest.raises(ValueError):
        pipe.rstreng_effective_area(12, .5, [0, 1], [.1, .1], inf, maop_psi=1000)


def test_A40_real_flaw_requires_positive_length():
    for method in (pipe.modified_b31g, pipe.b31g_original, dnv.dnv_f101_single_defect):
        with pytest.raises(ValueError, match="length"):
            method(12, .5, .4, 0, 60000, maop_psi=2000)


def test_A41_acceptance_requires_method_applicability():
    results = [pipe.modified_b31g(12, .5, .45, 1, 52000, maop_psi=10),
               pipe.b31g_original(12, .5, .45, 1, 52000, maop_psi=10),
               pipe.rstreng_effective_area(12, .5, [0, 1], [.45, .45], 52000, maop_psi=10),
               dnv.dnv_f101_single_defect(12, .5, .45, 1, 60000, maop_psi=10),
               dnv.dnv_f101_psf(12, .5, .45, 1, 60000, maop_psi=10),
               dnv.dnv_f101_interacting(12, .5, [(0, 1, .45)], 60000, maop_psi=10)]
    for result in results:
        assert not result.applicability.ok
        assert result.acceptable is None


def test_A42_every_profile_point_must_be_finite():
    for x, depths in (([0, 1, 2], [.1, .1, nan]), ([0, 1, inf], [.1, .1, .1])):
        with pytest.raises(ValueError, match="finite"):
            pipe.rstreng_effective_area(12, .5, x, depths, 52000, maop_psi=1000)
    with pytest.raises(ValueError, match="finite"):
        rstreng_2d_river_bottom(12, .5, [0, 1], [[.1, .1], [.1, nan]], 52000)


def test_A43_raw_depth_applicability_survives_projection():
    result = rstreng_2d_river_bottom(12, .5, [0, 1], [[.49, 0], [.49, 0]],
        52000, projection="area_weighted", maop_psi=10)
    assert not result.within_applicability
    assert not result.applicability.ok
    assert result.result.acceptable is None
    assert result.details["max_d_over_t"] == pytest.approx(.98)
    sensitivity = rstreng_2d_river_bottom(12, .5, [0, 1], [[.2, 0], [.2, 0]],
        52000, projection="area_weighted", maop_psi=10)
    assert "AREA_WEIGHTED_REPRESENTATION_UNQUALIFIED" in sensitivity.applicability.flags
    assert sensitivity.result.acceptable is None
    assert sensitivity.safe_pressure_psi > 0


def test_A44_dnv_strength_and_factor_domains():
    with pytest.raises(ValueError):
        dnv.dnv_f101_single_defect(12, .5, .2, 10, inf, maop_psi=1000)
    for factor in (0, -1, inf, nan, 1.1):
        with pytest.raises(ValueError):
            dnv.dnv_f101_single_defect(12, .5, .2, 10, 60000, usage_factor=factor)
    with pytest.raises(ValueError):
        dnv.dnv_f101_psf(12, .5, .4, 10, 60000, maop_psi=3000,
                         gamma_d=0, epsilon_d=0, std_rel_depth=0)
    with pytest.raises(ValueError):
        dnv.dnv_f101_interacting(12, .5, [(0, 1, .1)], inf)
    consumed = dnv.dnv_f101_psf(12, .5, .4, .01, 60000, maop_psi=3000)
    assert not consumed.details["factored_ligament_positive"]
    assert consumed.allowable_pressure_psi == 0 and consumed.acceptable is False


def test_A45_dnv_uncertainty_and_overrides_require_qualified_domain():
    for uncertainty in (-1., nan, inf, .17):
        with pytest.raises(ValueError):
            dnv.dnv_f101_psf(12, .5, .4, 10, 60000, std_rel_depth=uncertainty)
    for override in (dict(gamma_m=nan), dict(gamma_d=inf), dict(epsilon_d=-1),
                     dict(gamma_d=1., std_rel_depth=.08)):
        with pytest.raises(ValueError):
            dnv.dnv_f101_psf(12, .5, .2, 10, 60000, **override)


def test_A46_longitudinal_requires_positive_effective_ligament():
    for overrides in (dict(s=0, tmm=0, FCA=.1), dict(s=0, tmm=.4),
                      dict(s=1, tmm=.4, rsf_allowable=0), dict(s=1, tmm=nan)):
        with pytest.raises(ValueError):
            api579_part5_longitudinal_rsf(12, .5, **overrides)


def test_A47_circumferential_requires_measured_extents():
    with pytest.raises(ValueError):
        api579_part5_circumferential_netsection(12, .5, .4, 0, 52000)
    with pytest.raises(ValueError):
        api579_part5_circumferential_netsection(inf, .5, .4, 1, 52000, s=1)
    result = api579_part5_circumferential_netsection(12, .5, .4, 1, 52000)
    assert not result.applicability.ok
    assert result.allowable_axial_stress_psi is None
    assert "axial" in " ".join(result.applicability.notes).lower()


def test_A48_envelopes_validate_demand_and_preserve_applicability():
    for demand in (0, nan, inf):
        with pytest.raises(ValueError):
            pipe_acceptance_curve(12, .5, "X52", maop_psi=demand, depth_fracs=[.8])
    with pytest.raises(ValueError):
        pipe_acceptance_curve(12, .5, "X52", maop_psi=1, max_length_in=0)
    result = pipe_acceptance_curve(12, .5, "X52", maop_psi=1, depth_fracs=[.9])
    assert result["max_acceptable_length_in"] == [None]
    assert not result["applicability"][0]["ok"]
    result = level1_flaw_envelope(12, .5, "X52", 1, depth_fracs=[.9])
    assert result["max_acceptable_length_in"] == [None]
    assert not result["applicability"][0]["ok"]


def test_A49_lookup_identity_covers_criteria_and_full_precision():
    records = [_lookup(maop_psi=p) for p in (100, 10000)]
    assert records[0]["key"] != records[1]["key"]
    assert _lookup(safety_factor=2)["key"] != _lookup()["key"]
    assert _lookup(d_in=.20000001)["key"] != _lookup(d_in=.20000002)["key"]
    assert _lookup(D_in=2**53)["key"] != _lookup(D_in=2**53 + 1)["key"]
    plate = dict(grade="AH36", length_mm=2400, width_mm=800, thickness_mm=16,
                 sigma_x_mpa=150, metal_loss_mm=2)
    assert evaluate_plate_metal_loss(**plate)["key"] != evaluate_plate_metal_loss(**plate, fca_mm=1)["key"]
    assert evaluate_plate_metal_loss(**plate)["key"] != evaluate_plate_metal_loss(**plate, gamma_m=1.2)["key"]
    lookup = build_lookup(records)
    assert len(lookup["index"]) == 2
    conflicting = deepcopy(records[0])
    conflicting["acceptable"] = not conflicting["acceptable"]
    with pytest.raises(ValueError, match="duplicate"):
        build_lookup([records[0], conflicting])


def test_A50_lookup_retains_applicability_in_index_and_exports(tmp_path):
    record = _lookup(d_in=.45, L_in=1, maop_psi=10)
    assert record["verdict"] == "UNKNOWN" and record["acceptable"] is None
    assert not record["applicability"]["ok"]
    lookup = build_lookup([record])
    hit = query_index(lookup, record["domain"], **record["inputs"])
    assert hit["applicability"] == record["applicability"]
    paths = write_lookup(lookup, tmp_path)
    assert "applicability" in pd.read_csv(paths["csv"]).columns
    exported = json.loads(Path(paths["json"]).read_text(encoding="utf-8"))
    assert exported["index"][record["key"]]["applicability"] == record["applicability"]


def test_A52_rsf_requires_valid_required_wall_and_criterion():
    with pytest.raises(ValueError):
        calculate_rsf(.5, nan, .5)
    assert not calculate_rsf(.5, .6, .5)["ffs_acceptable"]


def test_A53_level1_requires_material_and_pressure_capacity():
    with pytest.raises(ValueError):
        check_ffs_level1(.5, .25, .5, 1000, 0, 12)
    result = check_ffs_level1(.5, .25, .5, 1000, 100, 12)
    assert result["mawp_reduced"] and not result["acceptable"]


def test_A55_ut_grid_preserves_missing_and_invalid_measurements(tmp_path):
    path = tmp_path / "grid.csv"
    path.write_text("NaN,,\n", encoding="utf-8", newline="\n")
    result = process_ut_grid(path, .5, .25)
    assert result["evaluation_status"] == "NOT_EVALUATED"
    assert result["missing_readings"] == 2 and result["invalid_readings"] == 1
    assert result["critical_points"] is None and result["below_required"] is None
    with pytest.raises(ValueError):
        process_ut_grid(path, 0, .25)
    path.write_text("0.4,0.4\n0.4\n", encoding="utf-8", newline="\n")
    assert process_ut_grid(path, .5, .25)["missing_readings"] == 1


def test_A56_pipeline_requires_explicit_corrosion_and_material_evidence():
    for inputs in (dict(corrosion_depth_mm=nan), dict(material_grade="X52"),
                   dict(corrosion_depth_mm=0)):
        with pytest.raises(ValueError):
            pipeline_integrity(PipelineInput(300, 10, 5, **inputs))
    assert pipeline_integrity(PipelineInput(300, 10, 5, material_grade="X52",
                                           corrosion_depth_mm=0)).wall_thickness_ok


def test_A58_campaign_envelope_requires_growth_load_and_toughness_basis():
    for changes in ({}, dict(corrosion_rate_in_per_yr=0, weld_sigma_b_mpa=0)):
        result = level1_flaw_envelope(12, .5, "X52", 1000, region="weld",
                                      campaign_years=3, **changes)
        assert result["evaluation_status"] == "NOT_EVALUATED"
        assert "basis" in result["reason"]
        assert all(value is None for value in result["max_acceptable_length_in"])
    cfg = dict(basename="riser_joint_ffs", riser_joint_ffs=dict(mode="envelope",
        envelope=dict(od_in=12, wt_in=.5, grade="X52", design_pressure_psi=1000,
                      region="weld", campaign_years=3)))
    assert RiserJointFFSWorkflow().router(cfg)["riser_joint_ffs"]["evaluation_status"] == "NOT_EVALUATED"
    from digitalmodel.asset_integrity.riser_joint_report import _reported_lengths

    result = level1_flaw_envelope(12, .5, "X52", 1000, region="weld", campaign_years=3)
    assert all("basis missing" in value for value in _reported_lengths(result))


def test_A59_placement_requires_positive_campaign_and_supported_margins():
    basis = dict(od_in=12, grade="X52", campaign_water_depth_ft=1000, campaign_years=3)
    for changes in (dict(campaign_water_depth_ft=0, campaign_years=0),
                    dict(zone_margins=dict(top=0, mid=0, bottom=0)),
                    dict(zone_margins=dict(top=.5, mid=.5, bottom=.5)),
                    dict(zone_margins={}), dict(campaign_years=nan)):
        with pytest.raises(ValueError):
            place_joint("joint", 0, .5, **{**basis, **changes})
