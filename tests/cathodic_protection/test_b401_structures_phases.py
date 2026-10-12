"""B401 riser-base compositions and phased assessments for issue 2263."""

from __future__ import annotations

import copy
from pathlib import Path
from typing import Any, cast

import pytest
import yaml  # type: ignore[import-untyped]

from digitalmodel.cathodic_protection.engine_adapter import run_cathodic_protection

FIXTURE = (
    Path(__file__).resolve().parents[1]
    / "fixtures"
    / "cathodic_protection"
    / "workflow_inputs"
    / "riser_base_phased.yml"
)


def _cfg() -> dict[str, Any]:
    with FIXTURE.open(encoding="utf-8") as stream:
        return cast(dict[str, Any], yaml.safe_load(stream))


def test_components_flatten_to_cited_b401_zones_and_local_families() -> None:
    result = run_cathodic_protection(_cfg())["results"]
    assessment = result["riser_base_assessment"]

    assert set(assessment["components"]) == {"base", "foundation", "mudmat", "hatch"}
    assert assessment["components"]["hatch"]["anode_families"] == ["hatch"]
    assert assessment["components"]["mudmat"]["anode_families"] == ["mud"]
    assert "base::frame" in result["current_demand_A"]
    assert "foundation::reinforcement" in result["current_demand_A"]
    assert "mudmat::underside" in result["current_demand_A"]
    assert any("[3.9.3]" in item for item in assessment["rule_citations"])
    assert any("[3.3.12]" in item for item in assessment["rule_citations"])


def test_wet_storage_reduces_family_mass_available_to_operation() -> None:
    phases = run_cathodic_protection(_cfg())["results"]["riser_base_assessment"][
        "phases"
    ]
    storage = phases[0]["families"]["sea"]
    operation = phases[1]["families"]["sea"]

    # B401:2021 Sec. 4.7.1 algebra: 1 A * 2 y * 8760 / 2000 = 8.76 kg.
    assert storage["consumed_mass_kg"] == pytest.approx(8.76)
    assert storage["physical_mass_end_kg"] == pytest.approx(91.24)
    # Installed usable mass is 5 * 20 * 0.90 = 90 kg; 90 - 8.76 = 81.24 kg.
    assert storage["usable_mass_end_kg"] == pytest.approx(81.24)
    # Operation consumes 1 * 25 * 8760 / 2000 = 109.50 usable kg.
    # Shortfall = 109.50 - 81.24 = 28.26 kg; gross = 28.26 / 0.90 = 31.40 kg.
    assert operation["mass_shortfall_kg"] == pytest.approx(28.26)
    assert operation["additional_gross_mass_kg"] == pytest.approx(31.40)
    assert operation["additional_anode_count"] == 2


def test_temporary_phase_has_its_own_design_life() -> None:
    cfg = _cfg()
    temporary = cfg["inputs"]["riser_base_assessment"]["phases"][0]
    temporary.update(name="installation", type="temporary", life_years=0.5)
    temporary["families"][0]["mean_current_A"] = 2.0
    phase = run_cathodic_protection(cfg)["results"]["riser_base_assessment"][
        "phases"
    ][0]["families"]["sea"]
    # Explicit 0.5 y temporary life: 2 A * 0.5 y * 8760 / 2000 = 4.38 kg.
    assert phase["consumed_mass_kg"] == pytest.approx(4.38)


def test_overconsumption_is_reported_without_retroactive_future_sizing() -> None:
    cfg = _cfg()
    storage = cfg["inputs"]["riser_base_assessment"]["phases"][0]["families"][0]
    storage["mean_current_A"] = 20.0
    phases = run_cathodic_protection(cfg)["results"]["riser_base_assessment"][
        "phases"
    ]
    exhausted = phases[0]["families"]["sea"]
    operation = phases[1]["families"]["sea"]
    # 20 A * 2 y * 8760 / 2000 = 175.20 kg versus 90 kg usable installed.
    assert exhausted["raw_usable_balance_kg"] == pytest.approx(-85.20)
    assert exhausted["usable_mass_end_kg"] == 0.0
    # Future shortfall is only its 109.50 kg demand, not 109.50 + historical 85.20.
    assert operation["mass_shortfall_kg"] == pytest.approx(109.50)


@pytest.mark.parametrize(
    ("edition", "expected_clauses"),
    [
        ("2005", ("Sec. 6.2.1", "Sec. 7.8.1-7.8.5")),
        ("2010", ("Sec. 6.3.8", "Sec. 7.13.3")),
        ("2017", ("[6.9.2]", "[7.9.2]")),
        ("2021", ("[3.9.3]", "[7.4.1], [7.5.3]")),
    ],
)
def test_rule_citations_are_edition_specific(
    edition: str, expected_clauses: tuple[str, str]
) -> None:
    cfg = _cfg()
    cfg["inputs"]["design_data"]["edition"] = edition
    citations = run_cathodic_protection(cfg)["results"]["riser_base_assessment"][
        "rule_citations"
    ]
    assert all(any(clause in item for item in citations) for clause in expected_clauses)


def test_retrofit_remaining_mass_and_additional_anodes_are_hand_derived() -> None:
    retrofit = run_cathodic_protection(_cfg())["results"]["riser_base_assessment"][
        "retrofit"
    ]

    # Historical consumption = 1 A * 10 y * 8760 / 2000 = 43.80 kg.
    assert retrofit["consumed_mass_kg"] == pytest.approx(43.80)
    assert retrofit["physical_remaining_mass_kg"] == pytest.approx(56.20)
    # Usable balance = 100 * 0.90 - 43.80 = 46.20 kg.
    assert retrofit["usable_remaining_mass_kg"] == pytest.approx(46.20)
    # Future use = 1.5 * 10 * 8760 / 2000 = 65.70 kg; deficit 19.50 kg.
    assert retrofit["mass_shortfall_kg"] == pytest.approx(19.50)
    assert retrofit["additional_gross_mass_kg"] == pytest.approx(19.50 / 0.90)
    assert retrofit["count_by_mass"] == 2
    assert retrofit["recommended_additional_count"] == 2
    assert "potential_history" in retrofit["limitations"]


def test_accepted_output_shortfall_is_scoped_and_does_not_turn_fail_to_pass() -> None:
    cfg = _cfg()
    calculated = run_cathodic_protection(copy.deepcopy(cfg))["results"]
    # This record echoes the exact computed Sec. 4.8.4 initial-output failure;
    # numeric resistance/output behavior is independently pinned by #2262 tests.
    check = calculated["riser_base_assessment"]["phases"][1]["families"]["sea"][
        "output_checks"
    ]["initial_output"]
    cfg["inputs"]["riser_base_assessment"]["accepted_output_shortfalls"] = [
        {
            "family": "sea",
            "phase": "operation",
            "criterion": "initial_output",
            "demand_A": 20.0,
            "output_A": check["output_A"],
            "ratio": check["ratio"],
            "owner_reason": "Temporary service exception with stated monitoring controls.",
            "basis": {
                "source": "project_practice",
                "reason": "Owner disposition; not a B401 compliance result.",
            },
        }
    ]
    result = run_cathodic_protection(cfg)["results"]
    record = result["riser_base_assessment"]["accepted_output_shortfalls"][0]
    assert record["disposition"] == "accepted_output_shortfall"
    assert record["engineering_result"] == "FAIL"
    assert result["status"]["result"] == "FAIL"


def test_acceptance_without_reason_and_cross_component_family_fail_closed() -> None:
    cfg = _cfg()
    assessment = cfg["inputs"]["riser_base_assessment"]
    assessment["accepted_output_shortfalls"] = [
        {
            "family": "sea",
            "phase": "operation",
            "criterion": "initial_output",
            "demand_A": 20.0,
            "output_A": 1.0,
            "ratio": 20.0,
            "owner_reason": "",
            "basis": {"source": "project_practice", "reason": "test"},
        }
    ]
    with pytest.raises(ValueError, match="owner_reason"):
        run_cathodic_protection(cfg)

    cfg = _cfg()
    cfg["inputs"]["riser_base_assessment"]["components"][3]["zones"][0][
        "anode_family"
    ] = "sea"
    with pytest.raises(ValueError, match="exactly one component"):
        run_cathodic_protection(cfg)


def test_phase_assessment_requires_input_installed_count() -> None:
    cfg = _cfg()
    cfg["inputs"]["anode_families"][0].pop("count")
    with pytest.raises(ValueError, match="installed count"):
        run_cathodic_protection(cfg)


def test_retrofit_project_practice_and_2021_condition_inputs_fail_closed() -> None:
    cfg = _cfg()
    cfg["inputs"]["riser_base_assessment"]["retrofit"]["remaining_mass_reason"] = ""
    with pytest.raises(ValueError, match="remaining_mass_reason"):
        run_cathodic_protection(cfg)

    cfg = _cfg()
    cfg["inputs"]["riser_base_assessment"]["retrofit"]["proposed_anode"].pop(
        "output_basis"
    )
    with pytest.raises(ValueError, match="output_basis"):
        run_cathodic_protection(cfg)

    cfg = _cfg()
    cfg["inputs"]["riser_base_assessment"]["retrofit"]["condition"].pop(
        "potential_history"
    )
    with pytest.raises(ValueError, match="potential_history"):
        run_cathodic_protection(cfg)


def test_retrofit_missing_anode_is_removed_from_mass_and_output_inventory() -> None:
    cfg = _cfg()
    cfg["inputs"]["riser_base_assessment"]["retrofit"]["missing_anode_count"] = 1
    retrofit = run_cathodic_protection(cfg)["results"]["riser_base_assessment"][
        "retrofit"
    ]
    # Four present 20 kg anodes: 80 - 43.80 = 36.20 kg physical remaining.
    assert retrofit["physical_remaining_mass_kg"] == pytest.approx(36.20)
    # Present usable inventory is 80 * 0.90 - 43.80 = 28.20 kg.
    assert retrofit["usable_remaining_mass_kg"] == pytest.approx(28.20)
    assert retrofit["remaining_anode_count"] == 4


def test_duplicate_component_zone_and_phase_family_fail_closed() -> None:
    cfg = _cfg()
    zones = cfg["inputs"]["riser_base_assessment"]["components"][0]["zones"]
    zones.append(copy.deepcopy(zones[0]))
    with pytest.raises(ValueError, match="duplicate zone"):
        run_cathodic_protection(cfg)

    cfg = _cfg()
    families = cfg["inputs"]["riser_base_assessment"]["phases"][0]["families"]
    families.append(copy.deepcopy(families[0]))
    with pytest.raises(ValueError, match="duplicate family"):
        run_cathodic_protection(cfg)


def test_phase_and_retrofit_deficits_govern_top_level_status() -> None:
    cfg = _cfg()
    for family in cfg["inputs"]["anode_families"][1:]:
        family["count"] = 100
    operation = cfg["inputs"]["riser_base_assessment"]["phases"][1]["families"][0]
    operation["initial_current_A"] = 0.0
    operation["final_current_A"] = 0.0
    result = run_cathodic_protection(cfg)["results"]
    # 28.26 kg phase deficit is an engineering failure even when output passes.
    assert result["status"]["result"] == "FAIL"
    assert result["status"]["governing_case"] == "phase_mass"

    cfg = _cfg()
    for family in cfg["inputs"]["anode_families"][1:]:
        family["count"] = 100
    for phase in cfg["inputs"]["riser_base_assessment"]["phases"]:
        phase["families"][0].update(
            mean_current_A=0.0, initial_current_A=0.0, final_current_A=0.0
        )
    result = run_cathodic_protection(cfg)["results"]
    assert result["riser_base_assessment"]["retrofit"][
        "recommended_additional_count"
    ] == 2
    assert result["status"]["governing_case"] == "retrofit_additions"


def test_route_default_conflicts_and_retrofit_bounds_fail_closed() -> None:
    cfg = _cfg()
    cfg["inputs"]["design_data"].pop("edition")
    result = run_cathodic_protection(cfg)["results"]
    assert result["edition"] == "2021"

    cfg = _cfg()
    cfg["inputs"]["structure"] = {"zones": []}
    with pytest.raises(ValueError, match="cannot be combined"):
        run_cathodic_protection(cfg)

    cfg = _cfg()
    cfg["inputs"]["riser_base_assessment"]["phases"][0]["families"] = []
    with pytest.raises(ValueError, match="families must be non-empty"):
        run_cathodic_protection(cfg)

    cfg = _cfg()
    proposed = cfg["inputs"]["riser_base_assessment"]["retrofit"]["proposed_anode"]
    proposed["utilization_factor"] = 2.0
    with pytest.raises(ValueError, match="utilization_factor"):
        run_cathodic_protection(cfg)

    cfg = _cfg()
    retrofit = cfg["inputs"]["riser_base_assessment"]["retrofit"]
    retrofit.update(
        measured_remaining_mass_kg=1000.0,
        remaining_mass_basis="measured_remaining_mass",
        remaining_mass_source="measurement",
    )
    with pytest.raises(ValueError, match="measured remaining mass"):
        run_cathodic_protection(cfg)


def test_retrofit_reports_combined_existing_and_proposed_output() -> None:
    retrofit = run_cathodic_protection(_cfg())["results"][
        "riser_base_assessment"
    ]["retrofit"]
    checks = retrofit["combined_output_checks"]
    assert checks["initial"]["total_output_A"] == pytest.approx(
        checks["initial"]["existing_output_A"]
        + checks["initial"]["proposed_output_A"]
    )
    assert checks["initial"]["result"] == "PASS"
    assert checks["final"]["result"] == "PASS"


def test_exhausted_retrofit_anodes_receive_no_existing_output_credit() -> None:
    cfg = _cfg()
    retrofit = cfg["inputs"]["riser_base_assessment"]["retrofit"]
    retrofit.update(
        measured_remaining_mass_kg=0.0,
        remaining_mass_basis="measured_remaining_mass",
        remaining_mass_source="measurement",
        future_mean_current_A=0.01,
        future_initial_current_A=12.0,
        future_final_current_A=12.0,
    )
    result = run_cathodic_protection(cfg)["results"]["riser_base_assessment"][
        "retrofit"
    ]
    assert result["combined_output_checks"]["initial"]["existing_output_A"] == 0.0
    # Exhausted existing output is zero; 12 A / 1 A per proposed anode = 12.
    assert result["recommended_additional_count"] == 12


def test_retrofit_mass_mode_and_existing_output_evidence_are_preserved() -> None:
    cfg = _cfg()
    retrofit = cfg["inputs"]["riser_base_assessment"]["retrofit"]
    retrofit["remaining_mass_basis"] = "measured_remaining_mass"
    retrofit["remaining_mass_source"] = "measurement"
    with pytest.raises(ValueError, match="requires measured_remaining_mass_kg"):
        run_cathodic_protection(cfg)

    cfg = _cfg()
    retrofit = cfg["inputs"]["riser_base_assessment"]["retrofit"]
    retrofit["existing_output_per_anode_A"] = 0.5
    retrofit["existing_output_basis"] = {
        "source": "project_practice",
        "reason": "Inspection-based conservative output assessment.",
    }
    result = run_cathodic_protection(cfg)["results"]["riser_base_assessment"][
        "retrofit"
    ]
    assert result["existing_output_basis"]["source"] == "project_practice"
    assert "Inspection-based" in result["existing_output_basis"]["reason"]
    assert result["remaining_mass_provenance"]["source"] == "project_practice"


def test_project_practice_provenance_cannot_claim_the_standard() -> None:
    cfg = _cfg()
    cfg["inputs"]["riser_base_assessment"]["components"][0][
        "composition_basis"
    ] = "dnv-rp-b401"
    with pytest.raises(ValueError, match="composition_basis"):
        run_cathodic_protection(cfg)

    cfg = _cfg()
    cfg["inputs"]["riser_base_assessment"]["phases"][0]["basis"][
        "source"
    ] = "dnv-rp-b401"
    with pytest.raises(ValueError, match="source"):
        run_cathodic_protection(cfg)


def test_dispatch_precedence_preserves_flat_family_route() -> None:
    cfg = _cfg()
    assessment = cfg["inputs"].pop("riser_base_assessment")
    cfg["inputs"]["structure"] = {"zones": assessment["components"][0]["zones"]}
    cfg["inputs"]["anode_families"] = [cfg["inputs"]["anode_families"][0]]
    flat = run_cathodic_protection(cfg)["results"]
    assert "riser_base_assessment" not in flat
    assert flat["anode_families"]["sea"]["count_source"] == "input"
