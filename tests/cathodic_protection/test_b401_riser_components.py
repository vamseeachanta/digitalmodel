"""Multi-component B401 riser design tests for issue 2261."""

from __future__ import annotations

from pathlib import Path

import pytest
import yaml

from digitalmodel.cathodic_protection import _kernels as kernel
from digitalmodel.cathodic_protection.engine_adapter import run_cathodic_protection

FIXTURE = (
    Path(__file__).resolve().parents[1]
    / "fixtures"
    / "cathodic_protection"
    / "workflow_inputs"
    / "multi_component_riser.yml"
)


def _cfg() -> dict:
    with FIXTURE.open(encoding="utf-8") as stream:
        return yaml.safe_load(stream)


def test_anode_mass_from_current_years_is_the_cited_eq2_kernel() -> None:
    # B401 Sec. 7.7 Eq. 2: M = (1.2 A*yr) * 8760 / (2000 Ah/kg * 0.90).
    assert kernel.anode_mass_from_current_years(1.2, 2000.0, 0.90) == pytest.approx(
        1.2 * 8760 / (2000 * 0.90)
    )
    with pytest.raises(ValueError, match="current_years_A_year"):
        kernel.anode_mass_from_current_years(float("nan"), 2000.0, 0.90)


def test_hybrid_components_use_local_life_coating_and_environment() -> None:
    cfg = run_cathodic_protection(_cfg())
    results = cfg["results"]
    assert results["mode"] == "b401_components"
    assert set(results["components"]) == {
        "bottom_assembly",
        "buoyancy_tank",
        "flexible_end_fittings",
        "foundation",
        "riser_joints",
        "top_assembly",
    }

    buoyancy = results["components"]["buoyancy_tank"]
    # B401:2021 Tables 8-1/8-2, Arctic >300 m: i=0.220/0.110/0.170 A/m2.
    # Table 8-4 Cat III >30 m: a=0.020, b=0.008/yr; at 20 yr,
    # f_ci/f_cm/f_cf=0.020/0.100/0.180. Eq. 1 on 100 m2 follows.
    assert buoyancy["current_demand_A"] == pytest.approx(
        {
            "initial": 100 * 0.220 * 0.020,
            "mean": 100 * 0.110 * 0.100,
            "final": 100 * 0.170 * 0.180,
        }
    )

    joints = results["components"]["riser_joints"]
    # F103:2010 Table A.1 prints 3LPP a*100=0.1, b*100=0.003, hence
    # a=0.001, b=0.00003/yr. At 15 yr: mean=0.001225, final=0.00145.
    assert joints["current_demand_A"] == pytest.approx(
        {
            "initial": 200 * 0.220 * 0.001,
            "mean": 200 * 0.110 * (0.001 + 0.00003 * 15 / 2),
            "final": 200 * 0.170 * (0.001 + 0.00003 * 15),
        }
    )
    assert any("2010 Table A.1" in c for c in results["citations"])
    assert (
        results["components"]["top_assembly"]["zones"]["atmospheric"]["disposition"]
        == "no_cp_demand"
    )


def test_shared_family_reconciles_pool_allocations_and_hosted_count_once() -> None:
    results = run_cathodic_protection(_cfg())["results"]
    family = results["anode_families"]["upper_standoff"]
    # Eq. 2 ampere-years: buoyancy 1.1*20 plus joints 0.02695*15.
    expected_ay = 1.1 * 20 + (200 * 0.110 * 0.001225) * 15
    assert family["mean_current_years_A_year"] == pytest.approx(expected_ay)
    assert family["required_mass_kg"] == pytest.approx(
        expected_ay * 8760 / (2000 * 0.90)
    )
    assert family["protected_components"] == ["buoyancy_tank", "riser_joints"]
    assert family["continuity_paths"]["buoyancy_tank"] == [
        "top_assembly",
        "buoyancy_tank",
    ]
    assert results["components"]["top_assembly"]["hosted_anode_count"] == 4
    assert results["anode_requirements"]["installed_anode_count"] == 24
    assert results["reconciliation"]["allocated_fraction_by_family"] == pytest.approx(
        {"upper_standoff": 1.0, "foundation_flush": 1.0, "lower_bracelet": 1.0}
    )
    # Overall Eq. 1 totals: buoyancy + F103-coated joints + 50 m2 buried
    # foundation + 20 m2 bottom assembly + 10 m2 end fittings.
    assert results["current_demand_A"] == pytest.approx(
        {"initial": 8.084, "mean": 5.42695, "final": 9.2093}
    )
    assert results["reconciliation"]["allocated_mass_kg_by_family"] == pytest.approx(
        {"upper_standoff": 200.0, "foundation_flush": 250.0, "lower_bracelet": 200.0}
    )
    for family_name, family in results["anode_families"].items():
        assert results["reconciliation"]["allocated_initial_output_A_by_family"][
            family_name
        ] == pytest.approx(family["anode_count"] * family["current_output_initial_A"])
        assert results["reconciliation"]["allocated_final_output_A_by_family"][
            family_name
        ] == pytest.approx(family["anode_count"] * family["current_output_final_A"])

    buoyancy = results["components"]["buoyancy_tank"]
    # Allocated mass is 0.90 * (4*50)=180 kg; Eq. 2 requires
    # 1.1*20*8760/(2000*0.90)=107.0667 kg.
    mass_check = buoyancy["allocation_checks"]["upper_standoff"]
    assert mass_check["allocated_mass_kg"] == pytest.approx(180.0)
    assert mass_check["adequacy_ratios"]["mass"] == pytest.approx(
        (1.1 * 20 * 8760 / (2000 * 0.90)) / 180.0
    )


def test_pool_surplus_does_not_mask_an_underallocated_component() -> None:
    cfg = _cfg()
    allocations = cfg["inputs"]["allocations"]
    allocations[0]["capacity_fraction"] = 0.01
    allocations[1]["capacity_fraction"] = 0.99
    results = run_cathodic_protection(cfg)["results"]
    assert results["anode_families"]["upper_standoff"]["checks"]["mass"] is True
    assert results["components"]["buoyancy_tank"]["status"]["result"] == "FAIL"
    assert results["status"]["result"] == "FAIL"
    assert results["status"]["governing_component"] == "buoyancy_tank"


@pytest.mark.parametrize(
    ("mutate", "message"),
    [
        (
            lambda c: c["inputs"]["electrical_continuity"].pop(0),
            "electrical continuity",
        ),
        (
            lambda c: c["inputs"]["allocations"].pop(0),
            "reciprocal allocation",
        ),
        (
            lambda c: c["inputs"]["allocations"][0].update(
                capacity_fraction=float("nan")
            ),
            "capacity_fraction",
        ),
        (
            lambda c: (
                c["inputs"]["allocations"][0].update(capacity_fraction=0.6),
                c["inputs"]["allocations"][1].update(capacity_fraction=0.5),
            ),
            "sum to 1",
        ),
        (
            lambda c: c["inputs"]["electrical_continuity"].append(
                {
                    "components": ["buoyancy_tank", "top_assembly"],
                    "available_for_years": 25.0,
                }
            ),
            "duplicate electrical continuity",
        ),
        (
            lambda c: c["inputs"].update(structure={"zones": []}),
            "mixed flat and component",
        ),
    ],
)
def test_component_schema_fails_closed(mutate, message: str) -> None:
    cfg = _cfg()
    mutate(cfg)
    with pytest.raises(ValueError, match=message):
        run_cathodic_protection(cfg)


def test_not_evaluated_zone_overrides_adequacy_status() -> None:
    cfg = _cfg()
    zone = cfg["inputs"]["components"][2]["zones"][0]
    zone.update(disposition="not_evaluated", rationale="coating evidence unavailable")
    results = run_cathodic_protection(cfg)["results"]
    assert results["status"]["result"] == "FAIL"
    assert results["status"]["governing_case"] == "coverage:not_evaluated"
    assert results["status"]["governing_ratio"] is None
    assert results["components"]["top_assembly"]["status"]["result"] == "FAIL"
    assert results["status"]["checks"]["top_assembly"] is False


def test_free_standing_riser_preserves_installed_shortfall_and_tsa_exclusion() -> None:
    cfg = _cfg()
    cfg["inputs"]["design_data"]["structure_type"] = "free_standing_riser"
    cfg["inputs"]["anode_families"][1]["count"] = 1
    atmospheric = cfg["inputs"]["components"][2]["zones"][0]
    atmospheric.update(
        disposition="excluded_self_protected",
        rationale="synthetic TSA exclusion; no TSA performance model is claimed",
    )
    results = run_cathodic_protection(cfg)["results"]
    assert results["components"]["foundation"]["status"]["result"] == "FAIL"
    assert results["status"]["result"] == "FAIL"
    assert (
        results["components"]["top_assembly"]["zones"]["atmospheric"]["disposition"]
        == "excluded_self_protected"
    )


def test_life_filter_uses_valid_alternate_continuity_path() -> None:
    cfg = _cfg()
    edges = cfg["inputs"]["electrical_continuity"]
    edges[1]["available_for_years"] = 10.0
    edges.append(
        {"components": ["buoyancy_tank", "riser_joints"], "available_for_years": 20.0}
    )
    family = run_cathodic_protection(cfg)["results"]["anode_families"]["upper_standoff"]
    assert family["continuity_paths"]["riser_joints"] == [
        "top_assembly",
        "buoyancy_tank",
        "riser_joints",
    ]


def test_f103_field_joint_coating_is_edition_cited() -> None:
    cfg = _cfg()
    coating = cfg["inputs"]["components"][1]["zones"][0]["coating"]
    coating.update(
        basis="f103_field_joint",
        edition="2019",
        system="3A",
        infill="none",
        operating_temperature_C=60.0,
    )
    results = run_cathodic_protection(cfg)["results"]
    row = results["components"]["riser_joints"]["zones"]["submerged"]
    # F103:2019 Table A-2, FJC 3A without infill: a=0.10, b=0.010/yr.
    assert row["coating_breakdown"]["f_cm"] == pytest.approx(0.10 + 0.010 * 15 / 2)
    assert any("2019-09 Table A-2" in c for c in results["citations"])
    # Compatibility is tabulated as prose but no qualified linepipe-pair model exists.
    assert row["coating_breakdown"]["applicability"] == "NOT_EVALUATED"
    assert results["components"]["riser_joints"]["status"]["result"] == "FAIL"
    assert results["status"]["result"] == "FAIL"


@pytest.mark.parametrize(
    ("mutate", "message"),
    [
        (
            lambda c: c["inputs"]["components"][1]["zones"][0].update(
                coating_category="III"
            ),
            "mutually exclusive",
        ),
        (
            lambda c: (
                c["inputs"]["components"][0]["zones"][0].pop("coating_category"),
                c["inputs"]["components"][0]["zones"][0].update(
                    coating=c["inputs"]["components"][1]["zones"][0]["coating"]
                ),
            ),
            "riser pipe joints",
        ),
        (
            lambda c: c["inputs"]["components"][1]["zones"][0]["coating"].update(
                concrete_weight_coating=True
            ),
            "concrete_weight_coating",
        ),
        (
            lambda c: c["inputs"]["components"][1]["zones"][0]["coating"].update(
                concrete_weight_coating="false"
            ),
            "must be Boolean",
        ),
        (
            lambda c: c["inputs"]["components"][1]["zones"][0].update(
                base_zone="atmospheric"
            ),
            "submerged linepipe zone",
        ),
        (
            lambda c: c["inputs"]["components"][1]["zones"][0]["coating"].update(
                operating_temperature_C=200.0
            ),
            "exceeds tabulated maximum",
        ),
        (
            lambda c: c["inputs"]["components"][1]["zones"][0]["coating"].pop(
                "edition"
            ),
            "edition",
        ),
        (
            lambda c: c["inputs"]["components"][2]["zones"][0].update(area_m2=-1.0),
            "area",
        ),
        (lambda c: c["inputs"].update(allocations=["bad"]), "mappings"),
        (
            lambda c: c["inputs"]["components"][0]["environment"].pop(
                "electrolyte_resistivity_ohm_m"
            ),
            "buoyancy_tank environment electrolyte_resistivity_ohm_m",
        ),
    ],
)
def test_component_applicability_and_container_validation(mutate, message: str) -> None:
    cfg = _cfg()
    mutate(cfg)
    with pytest.raises(ValueError, match=message):
        run_cathodic_protection(cfg)


def test_f103_2019_linepipe_concrete_rows_remain_distinct_and_cited() -> None:
    means = {}
    for concrete in (False, True):
        cfg = _cfg()
        coating = cfg["inputs"]["components"][1]["zones"][0]["coating"]
        coating.update(
            edition="2019",
            system="single_or_dual_layer_fbe",
            concrete_weight_coating=concrete,
        )
        row = run_cathodic_protection(cfg)["results"]["components"]["riser_joints"][
            "zones"
        ]["submerged"]["coating_breakdown"]
        means[concrete] = row["f_cm"]
        assert row["concrete_weight_coating"] is concrete
        assert any("2019-09 Table A-1" in label for label in row["citations"])
    # F103:2019 Table A-1 FBE: a=0.030; b=0.0010/yr without concrete
    # and 0.0003/yr with concrete. Mean f_c = a + b*t/2 at t=15 yr.
    assert means[False] == pytest.approx(0.030 + 0.0010 * 15 / 2)
    assert means[True] == pytest.approx(0.030 + 0.0003 * 15 / 2)


def test_reconciliation_failure_is_reported_as_the_governing_case() -> None:
    cfg = _cfg()
    # Fractions differ from unity by 5e-10: inside the schema tolerance but the
    # 200 kg allocation differs by 1e-7 kg, above reconciliation's 1e-9 kg tolerance.
    cfg["inputs"]["allocations"][1]["capacity_fraction"] = 0.1000000005
    results = run_cathodic_protection(cfg)["results"]
    assert results["status"]["result"] == "FAIL"
    assert results["status"]["checks"]["allocation_reconciliation"] is False
    assert results["status"]["governing_case"] == "allocation_reconciliation"
    assert results["status"]["governing_ratio"] is None


def test_self_continuity_reports_no_fabricated_edge_duration() -> None:
    family = run_cathodic_protection(_cfg())["results"]["anode_families"][
        "foundation_flush"
    ]
    check = family["continuity_checks"]["foundation"]
    assert check["minimum_edge_availability_years"] is None
    assert check["host_life_check"] is True
