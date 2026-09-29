"""ABS ships rebuild tests with hand-derived expected values (#2259)."""

from __future__ import annotations

import copy
import csv
import math
from pathlib import Path

import pytest

from digitalmodel.cathodic_protection.abs_ships_tables import (
    aluminium_properties,
    average_coated_current_density,
    coating_breakdown_range,
    design_current_density,
    protection_potential,
    zinc_properties,
)
from digitalmodel.cathodic_protection.engine_adapter import run_cathodic_protection
from digitalmodel.infrastructure.base_solvers.hydrodynamics.cathodic_protection import (
    CathodicProtection,
)

DATASET = (
    Path(__file__).resolve().parents[1]
    / "fixtures/test_vectors/cathodic_protection/datasets/abs-gn-ships/2017-12"
)


def _csv_rows(name: str) -> list[dict[str, str]]:
    with (DATASET / name).open(encoding="utf-8", newline="") as handle:
        return list(csv.DictReader(handle))


def _cfg() -> dict:
    return {
        "inputs": {
            "calculation_type": "ABS_gn_ships_2018",
            "design_data": {"design_life": 5},
            "environment": {"seawater": {"resistivity": {"input": 0.325}}},
            "structure": {
                "steel_total_area": 10000.0,
                "steel_coated_area": 10000.0,
                "area_coverage": 100.0,
                "coating_initial_breakdown_factor": 1.0,
                "coating_breakdown_factor_max": 2.05,
            },
            "design_current": {
                "dynamic_bare_steel_mA_m2": 1350.0,
                "static_bare_steel_mA_m2": 1350.0,
                "dynamic_time_fraction": 0.5,
                "coated_steel_mA_m2": 13.5,
                "uncoated_steel_mA_m2": 200.0,
            },
            "anode": {
                "material": "aluminium",
                "alloy": "A1",
                "protection_potential": -0.80,
                "anode_Utilisation_factor": 0.825,
                "anode_density": 2750.0,
                "physical_properties": {
                    "net_weight": 29.0,
                    "mean_length": 0.65,
                    "width": 0.125,
                    "height": 0.13,
                    "core_cross_section_m2": 0.01625,
                },
                "geometry": {"type": "long_flush", "length_m": 0.65, "width_m": 0.125},
            },
            "layout": {
                "actual_max_spacing_m": 8.0,
                "selected_locations": 200,
                "anodes_per_location": 2,
                "high_current_or_low_resistivity": False,
                "mechanical_damage_risk": False,
                "uniform_distribution_confirmed": True,
                "bilge_damage_avoided": True,
                "bilge_keel_fitted": False,
            },
        }
    }


def test_abs_table_values_are_cited() -> None:
    # ABS Sec. 3 Table 4, alloy A1: -1.09 V and 2500 Ah/kg.
    potential, capacity = aluminium_properties("A1")
    assert potential.value == pytest.approx(-1.09)
    assert capacity.value == pytest.approx(2500.0)
    assert potential.citation.section == "Section 3, Table 4"
    # ABS Sec. 2 Table 4, high durability: 0.5-1.0 %/yr.
    low, high = coating_breakdown_range("high")
    assert (low.value, high.value) == pytest.approx((0.5, 1.0))
    # ABS Sec. 2 Table 3, 1 < V < 10 m/s bare-steel range: 220-350 mA/m2.
    current_low, current_high = design_current_density("1_to_10_m_s", "bare")
    assert (current_low.value, current_high.value) == pytest.approx((220.0, 350.0))
    # ABS Sec. 2 Table 5, 37-60 months: 46-75 mA/m2.
    avg_low, avg_high = average_coated_current_density("37_to_60_months")
    assert (avg_low.value, avg_high.value) == pytest.approx((46.0, 75.0))
    # ABS Sec. 3 Table 2 has a distinct Z4 row at 60-80 C.
    z4_potential, z4_capacity = zinc_properties("Z4", temperature_c=60.0)
    assert (z4_potential.value, z4_capacity.value) == pytest.approx((-0.97, 690.0))


def test_abs_rebuild_hand_derived_demand_mass_and_count() -> None:
    out = run_cathodic_protection(_cfg())
    result = out["results"]
    # ABS 2/4.4 percentage conversion: fi=.0100, fm=.01525, ff=.0205.
    # ABS 2/4.5 with Jbd=Jbs=1350 mA/m2 gives 13.5/20.5875/27.675.
    assert result["current_demand_A"]["totals"] == pytest.approx(
        {"initial": 135.0, "mean": 205.875, "final": 276.75}
    )
    # ABS Sec. 2/7.3: W=205.875*5*8760/(2500*0.825)=4372.036364 kg.
    assert result["anode_requirements"]["total_mass_kg"] == pytest.approx(4372.036364)
    assert result["anode_requirements"]["mass_count"] == math.ceil(4372.036364 / 29.0)
    assert result["coating_breakdown_factors"]["initial"] == pytest.approx(0.01)
    assert result["layout"]["selected_locations"] == 200
    assert result["status"]["use_status"] == "cited-pending-review"
    assert result["status"]["result"] == "PASS"
    assert result["status"]["governing_case"] == "final_current_output"
    assert result["anode_requirements"]["recommended_anode_count"] == 364
    assert any("Table 3" in item for item in result["limitations"])
    joined = " | ".join(result["citations"])
    for locator in (
        "Table 3",
        "Table 4",
        "2/4.5",
        "2/5",
        "2/6.1-2/6.3",
        "2/7.3",
        "2/7.5.1-2/7.5.2",
        "3/5.2",
    ):
        assert locator in joined


def test_abs_mean_uses_dynamic_static_weighting_but_maximum_uses_dynamic() -> None:
    cfg = _cfg()
    current = cfg["inputs"]["design_current"]
    current["dynamic_bare_steel_mA_m2"] = 400.0
    current["static_bare_steel_mA_m2"] = 200.0
    current["dynamic_time_fraction"] = 0.5
    totals = run_cathodic_protection(cfg)["results"]["current_demand_A"]["totals"]
    # Sec. 2/4.5 project initial: 10000*.010*400/1000 = 40 A.
    # Eq. (7) mean: 10000*.01525*(.5*400+.5*200)/1000 = 45.75 A.
    # Eq. (6) maximum: 10000*.0205*400/1000 = 82 A.
    assert totals == pytest.approx({"initial": 40.0, "mean": 45.75, "final": 82.0})


def test_abs_rebuild_depleted_long_flush_geometry_and_output() -> None:
    result = run_cathodic_protection(_cfg())["results"]
    geom = result["anode_performance"]["depleted_geometry"]
    # Sec. 2/7.5.1: Wf=29*(1-.825)=5.075 kg.
    assert geom["net_mass_kg"] == pytest.approx(5.075)
    # Sec. 2/7.5.2: Lf=.65*(1-.1*.825)=.596375 m.
    assert geom["length_m"] == pytest.approx(0.596375)
    # ABS 2/7.5.2: Xf=pi*Wf/(Lf*rho_a)+Xcore, rf=sqrt(2Xf/pi).
    # The de-identified value is the project-derived core-area interpretation.
    x_final = math.pi * 5.075 / (0.596375 * 2750.0) + 0.01625
    radius = math.sqrt(2.0 * x_final / math.pi)
    # Sec. 2/6.2 is selected as the relevant flush formula using W=2r.
    resistance = 0.325 / (0.596375 + 2.0 * radius)
    assert geom["radius_m"] == pytest.approx(radius)
    assert result["anode_performance"]["resistance_ohm"]["final"] == pytest.approx(
        resistance
    )
    # Sec. 2/5: Delta E=(-.80)-(-1.09)=.29 V, I=.29/R.
    assert result["anode_performance"]["current_output_A"][
        "final_per_anode"
    ] == pytest.approx(0.29 / resistance)


def test_abs_main_route_no_longer_requires_experimental_and_legacy_warns() -> None:
    cfg = _cfg()
    cfg["inputs"]["design_data"].pop("experimental", None)
    run_cathodic_protection(cfg)

    legacy = _cfg()
    legacy["inputs"]["calculation_type"] = "ABS_gn_ships_2018_legacy"
    legacy["inputs"]["design_data"]["experimental"] = True
    with pytest.warns(DeprecationWarning, match="ABS_gn_ships_2018"):
        run_cathodic_protection(legacy)
    assert legacy["results"]["status"]["use_status"].startswith("legacy-uncited")

    direct = _cfg()
    direct["inputs"]["calculation_type"] = "ABS_gn_ships_2018_legacy"
    with pytest.warns(DeprecationWarning, match="ABS_gn_ships_2018"):
        CathodicProtection().router(direct)
    assert direct["results"]["status"]["use_status"].startswith("legacy-uncited")


def test_abs_short_flush_uses_square_root_area() -> None:
    cfg = _cfg()
    geom = cfg["inputs"]["anode"]["geometry"]
    geom.clear()
    geom.update({"type": "short_flush", "exposed_area_m2": 0.25})
    result = run_cathodic_protection(cfg)["results"]
    # ABS Sec. 2/6.3: R=0.315*rho/sqrt(A)=0.20475 ohm.
    assert result["anode_performance"]["resistance_ohm"]["initial"] == pytest.approx(
        0.315 * 0.325 / math.sqrt(0.25)
    )
    assert result["anode_performance"]["resistance_ohm"]["final"] == pytest.approx(
        result["anode_performance"]["resistance_ohm"]["initial"]
    )


def test_abs_invalid_layout_is_not_silently_accepted() -> None:
    cfg = _cfg()
    cfg["inputs"]["layout"] = {"actual_max_spacing_m": 8.0}
    result = run_cathodic_protection(cfg)["results"]
    assert result["layout"]["status"] == "NOT_EVALUATED"
    assert result["status"]["result"] == "FAIL"

    cfg = _cfg()
    cfg["inputs"]["layout"]["uniform_distribution_confirmed"] = False
    result = run_cathodic_protection(cfg)["results"]
    assert result["layout"]["status"] == "FAIL"
    assert result["status"]["result"] == "FAIL"


def test_abs_rejects_percentage_as_multiplier_and_missing_core() -> None:
    cfg = _cfg()
    cfg["inputs"]["structure"]["coating_breakdown_factor_max"] = 205.0
    with pytest.raises(ValueError, match="percentage"):
        run_cathodic_protection(cfg)

    cfg = _cfg()
    del cfg["inputs"]["anode"]["physical_properties"]["core_cross_section_m2"]
    with pytest.raises(ValueError, match="core_cross_section_m2"):
        run_cathodic_protection(cfg)


def test_abs_selected_count_can_fail_output_check() -> None:
    cfg = _cfg()
    cfg["inputs"]["layout"]["anodes_per_location"] = 1
    result = run_cathodic_protection(cfg)["results"]
    assert result["anode_requirements"]["selected_anode_count"] == 200
    assert result["status"]["result"] == "FAIL"


def test_abs_layout_counts_must_be_integral() -> None:
    cfg = _cfg()
    cfg["inputs"]["layout"]["selected_locations"] = 2.9
    with pytest.raises(ValueError, match="positive integer"):
        run_cathodic_protection(cfg)


@pytest.mark.parametrize(
    "field",
    [
        "high_current_or_low_resistivity",
        "mechanical_damage_risk",
        "uniform_distribution_confirmed",
        "bilge_damage_avoided",
        "bilge_keel_fitted",
    ],
)
def test_abs_layout_flags_require_actual_booleans(field: str) -> None:
    cfg = _cfg()
    cfg["inputs"]["layout"][field] = "false"
    with pytest.raises(ValueError, match=f"{field} must be a boolean"):
        run_cathodic_protection(cfg)


def test_abs_layout_requires_explicit_bilge_keel_applicability() -> None:
    cfg = _cfg()
    del cfg["inputs"]["layout"]["bilge_keel_fitted"]
    result = run_cathodic_protection(cfg)["results"]
    assert result["layout"]["status"] == "NOT_EVALUATED"
    assert "bilge_keel_fitted" in result["layout"]["reason"]
    assert result["status"]["result"] == "FAIL"

    cfg = _cfg()
    cfg["inputs"]["layout"]["minimum_anodes_per_location"] = 1.9
    with pytest.raises(ValueError, match="positive integer"):
        run_cathodic_protection(cfg)


@pytest.mark.parametrize(
    "missing", ["static_bare_steel_mA_m2", "dynamic_time_fraction"]
)
def test_abs_eq7_inputs_are_required(missing: str) -> None:
    cfg = _cfg()
    del cfg["inputs"]["design_current"][missing]
    with pytest.raises(ValueError, match="static bare density|dynamic time fraction"):
        run_cathodic_protection(cfg)


def test_abs_dataset_records_are_parseable_and_private_path_is_withheld() -> None:
    root = (
        Path(__file__).resolve().parents[1]
        / "fixtures/test_vectors/cathodic_protection/datasets/abs-gn-ships/2017-12"
    )
    csv_files = sorted(root.glob("*.csv"))
    assert {path.name for path in csv_files} == {
        "section-2-table-1.csv",
        "section-2-table-3.csv",
        "section-2-table-4.csv",
        "section-2-table-5.csv",
        "section-3-table-2.csv",
        "section-3-table-4.csv",
        "section-3-table-6.csv",
    }
    for path in csv_files:
        with path.open(encoding="utf-8", newline="") as handle:
            rows = list(csv.reader(handle))
        assert len(rows) >= 2
        assert rows[0][0].startswith("Section ")
    provenance = (root / "PROVENANCE.md").read_text(encoding="utf-8")
    assert "private standards archive" in provenance
    assert "/mnt/" not in provenance


def test_abs_all_coating_and_average_density_lookups_match_csv() -> None:
    coating_rows = {
        row["durability"]: row
        for row in _csv_rows("section-2-table-4.csv")
        if row["durability"] != "all"
    }
    for durability, row in coating_rows.items():
        low, high = coating_breakdown_range(durability)
        assert (low.value, high.value) == pytest.approx(
            (float(row["low_percent"]), float(row["high_percent"]))
        )
    periods = {
        "<=18": "up_to_18_months",
        "19-36": "19_to_36_months",
        "37-60": "37_to_60_months",
    }
    for row in _csv_rows("section-2-table-5.csv"):
        low, high = average_coated_current_density(
            periods[row["docking_period_months"]]
        )
        assert (low.value, high.value) == pytest.approx(
            (float(row["current_low_mA_m2"]), float(row["current_high_mA_m2"]))
        )


def test_abs_all_table3_lookup_rows_match_csv() -> None:
    situations = {
        "V <= 1 m/s without tidal influence": "up_to_1_m_s_no_tide",
        "V <= 1 m/s with tidal influence": "up_to_1_m_s_with_tide",
        "1 < V < 10 m/s": "1_to_10_m_s",
        "V >= 10 m/s": "at_least_10_m_s",
        "Vessels in ice": "ice",
    }
    for row in _csv_rows("section-2-table-3.csv"):
        if row["situation"] not in situations:
            continue
        for condition in ("bare", "coated"):
            low, high = design_current_density(situations[row["situation"]], condition)
            assert (low.value, high.value) == pytest.approx(
                (
                    float(row[f"{condition}_low_mA_m2"]),
                    float(row[f"{condition}_high_mA_m2"]),
                )
            )


def test_abs_all_supported_alloy_lookups_match_csv() -> None:
    aluminium = [
        row
        for row in _csv_rows("section-3-table-4.csv")
        if row["alloy"] in {"A1", "A2", "A3", "A4"}
    ]
    for row in aluminium:
        potential, capacity = aluminium_properties(row["alloy"])
        assert (potential.value, capacity.value) == pytest.approx(
            (float(row["closed_circuit_potential_V"]), float(row["capacity_Ah_kg"]))
        )


def test_abs_supported_protection_potentials_match_csv() -> None:
    rows = _csv_rows("section-2-table-1.csv")[:2]
    for anaerobic, row in zip((False, True), rows, strict=True):
        value = protection_potential(anaerobic)
        assert value.value == pytest.approx(float(row["minimum_negative_potential_V"]))
    for row in _csv_rows("section-3-table-2.csv"):
        temperature = float(row["temperature_C"].split("-")[0])
        potential, capacity = zinc_properties(row["alloy"], temperature_c=temperature)
        assert (potential.value, capacity.value) == pytest.approx(
            (float(row["closed_circuit_potential_V"]), float(row["capacity_Ah_kg"]))
        )


def test_abs_calculation_does_not_mutate_input_sections() -> None:
    cfg = _cfg()
    before = copy.deepcopy(cfg["inputs"])
    run_cathodic_protection(cfg)
    assert cfg["inputs"] == before
