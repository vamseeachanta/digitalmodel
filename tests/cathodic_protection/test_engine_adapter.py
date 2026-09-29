"""Engine adapter regression tests (issue #2210).

Each retired demo input (owner decision D5) now lives under
``tests/fixtures/cathodic_protection/workflow_inputs/`` and runs through
``run_cathodic_protection``. The pinned numbers are re-derived from the
DNV-RP-B401 (2021) / DNV-RP-F103 (2010) tables in the plan
``docs/plans/2026-09-25-issue-2210-cp-engine-adapter-demo-retirement.md``;
the pipeline fixture (no edition given) follows the DNV-RP-F103 default,
2019 since 2026-09-27, and is re-derived from the 2019 tables in its test.
"""

from __future__ import annotations

import copy
import math
import warnings
from pathlib import Path
from typing import Any

import pytest
import yaml  # type: ignore[import-untyped]
from loguru import logger

from digitalmodel.cathodic_protection import engine_adapter
from digitalmodel.cathodic_protection._experimental import ExperimentalModelError
from digitalmodel.cathodic_protection.engine_adapter import (
    CALCULATION_TYPES,
    KEY_B401_LEGACY,
    KEY_F103,
    KEY_F103_2010,
    KEY_F103_ANODE_BANK,
    KEY_F103_LEGACY,
    STATUS_FAIL,
    STATUS_PASS,
    USE_STATUS_CLIENT_EOR,
    USE_STATUS_ENGINEERING_VALIDATION,
    USE_STATUS_LEGACY_UNCITED,
    run_cathodic_protection,
)


def test_anode_bank_route_has_distinct_validation_status() -> None:
    cfg = _run("anode_bank")
    assert cfg["results"]["status"]["result"] == STATUS_PASS
    assert cfg["results"]["status"]["use_status"] == USE_STATUS_ENGINEERING_VALIDATION
    assert KEY_F103_ANODE_BANK in CALCULATION_TYPES
    assert cfg["results"]["banks"][0]["sides"][0]["protection_ok"] is True

FIXTURE_DIR = (
    Path(__file__).resolve().parents[1] / "fixtures" / "cathodic_protection" / "workflow_inputs"
)
FIXTURES = ("jacket", "manifold", "monopile", "pipeline", "ships", "fpso")
# Owner decision 2026-09-27 (epic #2206): B401 offshore and F103 bracelet are
# for client use with an engineer-of-record check; ABS offshore remains legacy.
# Benchmark 2026-09-27 (#2259): ABS ships requires experimental opt-in.
EXPECTED_USE_STATUS = {
    "jacket": USE_STATUS_CLIENT_EOR,
    "manifold": USE_STATUS_CLIENT_EOR,
    "monopile": USE_STATUS_CLIENT_EOR,
    "pipeline": USE_STATUS_CLIENT_EOR,
    "ships": "experimental-known-understatement",
    "fpso": USE_STATUS_LEGACY_UNCITED,
}

B401_KEYS = {
    "standard",
    "edition",
    "provenance",
    "citations",
    "design_life_years",
    "surface_areas_m2",
    "coating_breakdown",
    "current_densities_A_m2",
    "current_demand_A",
    "anode_resistance_ohm",
    "anode_requirements",
    "current_output_verification",
    "status",
}
F103_KEYS = {
    "standard",
    "edition",
    "provenance",
    "citations",
    "design_life_years",
    "pipeline_geometry_m",
    "coating_breakdown_factors",
    "current_densities_A_m2",
    "current_demand_A",
    "anode_requirements",
    "anode_spacing_m",
    "attenuation_analysis",
    "status",
}


def _load(name: str) -> dict[str, Any]:
    with (FIXTURE_DIR / f"{name}.yml").open(encoding="utf-8") as stream:
        cfg: dict[str, Any] = yaml.safe_load(stream)
    assert cfg["basename"] == "cathodic_protection"
    return cfg


def _run(name: str) -> dict[str, Any]:
    return run_cathodic_protection(_load(name))


def _assert_status(status: dict[str, Any]) -> None:
    assert status["result"] in {STATUS_PASS, STATUS_FAIL}
    assert status["governing_case"] in {"mass", "initial", "final"}
    assert isinstance(status["reason"], str) and status["reason"]
    assert isinstance(status["checks"], dict)


# ---------------------------------------------------------------------------
# Schema across every fixture
# ---------------------------------------------------------------------------


@pytest.mark.parametrize("name", FIXTURES)
def test_fixture_runs_and_carries_status(name: str) -> None:
    cfg = _run(name)
    results = cfg["results"]
    _assert_status(results["status"])
    assert results["status"]["use_status"] == EXPECTED_USE_STATUS[name]
    if name in {"jacket", "manifold", "monopile"}:
        assert B401_KEYS <= set(results)
        demand = results["current_demand_A"]
        assert {"total_initial_A", "total_mean_A", "total_final_A"} <= set(demand)
        verification = results["current_output_verification"]
        assert {"adequate", "governing_case", "recommended_anode_count"} <= set(verification)
        assert isinstance(results["anode_requirements"]["anode_count"], int)
        assert isinstance(verification["recommended_anode_count"], int)
        assert results["citations"]
    elif name == "pipeline":
        assert F103_KEYS <= set(results)
        assert isinstance(results["anode_requirements"]["anode_count"], int)
        assert isinstance(results["attenuation_analysis"]["protection_adequate"], bool)
        assert results["citations"]
    elif name == "ships":
        assert results is cfg["cathodic_protection"]
        assert isinstance(results["anode_requirements"]["anode_count"], int)
    else:
        assert isinstance(results["anode_requirements"]["anode_count"], int)


@pytest.mark.parametrize("name", FIXTURES)
def test_fail_status_is_logged_as_warning_and_never_raises(name: str) -> None:
    records: list[str] = []
    sink_id = logger.add(lambda message: records.append(message.record["level"].name), level="INFO")
    try:
        cfg = _run(name)
    finally:
        logger.remove(sink_id)
    status = cfg["results"]["status"]
    if status["result"] == STATUS_FAIL:
        assert "WARNING" in records
    else:
        assert "WARNING" not in records


# ---------------------------------------------------------------------------
# DNV-RP-B401 (new package): numbers re-derived from Tables 10-1/10-2/10-4/10-6/10-7
# ---------------------------------------------------------------------------


def test_jacket_b401_sec7_loop() -> None:
    # Temperate (10 C), 0-30 m, Cat III (a = 0.02, b = 0.012), 25 yr, 5000 m2:
    # f_ci/f_cm/f_cf = 0.02/0.17/0.32
    # I_initial = 5000 * 0.200 * 0.02 = 20.0 A
    # I_mean    = 5000 * 0.100 * 0.17 = 85.0 A
    # I_final   = 5000 * 0.130 * 0.32 = 208.0 A
    # M = 85 * 25 * 8760 / (2000 * 0.85) = 10 950 kg -> 55 x 200 kg (mass)
    # Fresh stand-off (rho 0.30, L 1.0, r 0.05): R = 0.30/(2 pi)(ln 80 - 1) = 0.16148 ohm,
    # I_a = 0.25 / 0.16148 = 1.548 A -> N_initial = ceil(20 / 1.548) = 13
    # Depleted (u 0.85 -> r_f = 0.05 sqrt(0.15) = 0.019365 m): R_f = 0.20677 ohm,
    # I_a = 1.209 A -> N_final = ceil(208 / 1.209) = 173 -> final governs, FAIL.
    results = _run("jacket")["results"]
    assert results["standard"] == "DNV-RP-B401 (May 2021)"
    assert results["edition"] == "2021"
    demand = results["current_demand_A"]
    assert demand["total_initial_A"] == pytest.approx(20.0)
    assert demand["total_mean_A"] == pytest.approx(85.0)
    assert demand["total_final_A"] == pytest.approx(208.0)
    assert demand["splash"]["I_final_A"] == 0.0
    assert demand["atmospheric"]["I_final_A"] == 0.0
    breakdown = results["coating_breakdown"]["submerged"]
    assert (breakdown["a"], breakdown["b_per_yr"]) == (0.02, 0.012)
    assert breakdown["f_cf"] == pytest.approx(0.32)
    densities = results["current_densities_A_m2"]["submerged"]
    assert densities["climate"] == "temperate"
    assert densities["i_initial_A_m2"] == pytest.approx(0.200)
    req = results["anode_requirements"]
    assert req["total_mass_kg"] == pytest.approx(10950.0)
    assert req["anode_count"] == 55
    assert req["electrochemical_capacity_Ah_kg"] == pytest.approx(2000.0)
    verification = results["current_output_verification"]
    assert verification["driving_voltage_V"] == pytest.approx(0.25)
    assert results["anode_resistance_ohm"] == pytest.approx(0.16148, abs=1e-5)
    assert verification["anode_resistance_final_ohm"] == pytest.approx(0.20677, abs=1e-5)
    assert verification["equivalent_radius_final_m"] == pytest.approx(0.05 * math.sqrt(0.15), abs=1e-6)
    assert verification["count_by_mass"] == 55
    assert verification["count_by_initial_current"] == 13
    assert verification["count_by_final_current"] == 173
    assert verification["recommended_anode_count"] == 173
    assert verification["governing_case"] == "final"
    assert verification["adequate"] is False
    status = results["status"]
    assert status["result"] == STATUS_FAIL
    assert status["governing_case"] == "final"
    assert "173" in status["reason"]
    assert "dnv-rp-b401 2021-05 Table 8-4" in results["citations"]


def test_manifold_b401_sec7_loop() -> None:
    # Arctic (6 C), 0-30 m, Cat III, 25 yr, 850 m2: I_mean = 850 * 0.120 * 0.17 = 17.34 A,
    # I_initial = 850 * 0.250 * 0.02 = 4.25 A, I_final = 850 * 0.170 * 0.32 = 46.24 A,
    # M = 2233.8 kg -> 28 x 80 kg; R (rho 0.33, L 0.6, r 0.045) = 0.26055 ohm -> N_initial 5;
    # R_f (r_f = 0.045 sqrt(0.15)) = 0.34359 ohm -> N_final 64.
    results = _run("manifold")["results"]
    demand = results["current_demand_A"]
    assert demand["total_initial_A"] == pytest.approx(4.25)
    assert demand["total_mean_A"] == pytest.approx(17.34)
    assert demand["total_final_A"] == pytest.approx(46.24)
    assert results["anode_requirements"]["total_mass_kg"] == pytest.approx(2233.8)
    assert results["anode_requirements"]["anode_count"] == 28
    verification = results["current_output_verification"]
    assert verification["count_by_initial_current"] == 5
    assert verification["count_by_final_current"] == 64
    assert verification["recommended_anode_count"] == 64
    assert results["status"]["result"] == STATUS_FAIL
    assert results["status"]["governing_case"] == "final"


def test_monopile_b401_sec7_loop() -> None:
    # Temperate (8 C), Cat III, 30 yr (f_cm 0.20, f_cf 0.38), 1200 m2: I_mean = 24.0 A,
    # I_initial = 4.8 A, I_final = 1200 * 0.130 * 0.38 = 59.28 A; M = 3710.12 kg -> 25 x 150 kg;
    # R (rho 0.28, L 0.9, r 0.06) = 0.15322 ohm -> N_initial 3; R_f = 0.20018 ohm -> N_final 48.
    results = _run("monopile")["results"]
    demand = results["current_demand_A"]
    assert demand["total_initial_A"] == pytest.approx(4.8)
    assert demand["total_mean_A"] == pytest.approx(24.0)
    assert demand["total_final_A"] == pytest.approx(59.28)
    assert results["anode_requirements"]["total_mass_kg"] == pytest.approx(3710.12)
    assert results["anode_requirements"]["anode_count"] == 25
    verification = results["current_output_verification"]
    assert verification["count_by_initial_current"] == 3
    assert verification["count_by_final_current"] == 48
    assert verification["recommended_anode_count"] == 48
    assert results["status"]["result"] == STATUS_FAIL
    assert results["status"]["governing_case"] == "final"


def test_jacket_installed_count_override_passes() -> None:
    cfg = _load("jacket")
    cfg["inputs"]["anode"]["count"] = 173
    results = run_cathodic_protection(cfg)["results"]
    verification = results["current_output_verification"]
    assert verification["anode_count"] == 173
    assert verification["anode_count_source"] == "input"
    assert verification["adequate"] is True
    assert results["status"]["result"] == STATUS_PASS
    assert results["status"]["governing_case"] == "final"
    # the mass-based count is still reported unchanged
    assert results["anode_requirements"]["anode_count"] == 55


def test_b401_mass_based_radius_when_radius_omitted() -> None:
    cfg = _load("jacket")
    del cfg["inputs"]["anode"]["radius_m"]
    verification = run_cathodic_protection(cfg)["results"]["current_output_verification"]
    assert verification["radius_source"] == "mass"
    # r = sqrt(m / (pi L rho)) = sqrt(200 / (pi * 1.0 * 2750))
    assert verification["equivalent_radius_initial_m"] == pytest.approx(
        math.sqrt(200.0 / (math.pi * 2750.0)), abs=1e-6
    )
    assert verification["equivalent_radius_final_m"] == pytest.approx(
        math.sqrt(0.15 * 200.0 / (math.pi * 2750.0)), abs=1e-6
    )


def test_b401_requires_anode_length() -> None:
    cfg = _load("jacket")
    del cfg["inputs"]["anode"]["length_m"]
    with pytest.raises(ValueError, match="length_m"):
        run_cathodic_protection(cfg)


def test_b401_rejects_unknown_zone_and_coating() -> None:
    cfg = _load("manifold")
    cfg["inputs"]["structure"]["zones"][0]["zone"] = "hull"
    with pytest.raises(ValueError, match="zone"):
        run_cathodic_protection(cfg)
    cfg = _load("manifold")
    cfg["inputs"]["structure"]["zones"][0]["coating_category"] = "V"
    with pytest.raises(ValueError, match="coating_category"):
        run_cathodic_protection(cfg)


# ---------------------------------------------------------------------------
# DNV-RP-F103 (new package)
# ---------------------------------------------------------------------------


def test_pipeline_f103_bracelet_design() -> None:
    # No edition in the fixture -> DNV-RP-F103 default, 2019 (owner decision 2026-09-27).
    # Non-buried, 60 C -> Table 6-2 ">50-80" column: i_cm = 0.075 A/m2 (2010 Table 5-1: 0.060).
    # FBE without concrete weight coating, Table A-1: a = 0.030, b = 0.0010 (2010 Table A.1:
    # a = 0.010, b = 0.0003). T = 30 yr: f_cm = 0.030 + 0.0010 * 15 = 0.045,
    # f_cf = 0.030 + 0.0010 * 30 = 0.060. A = pi * 0.3239 * 1500 = 1526.343 m2;
    # I_cm = 1526.343 * 0.075 * 0.045 = 5.1514 A, I_cf = 1526.343 * 0.075 * 0.060 = 6.8685 A.
    # Table 6-3 Al at ambient: 2000 Ah/kg, -1.05 V; [6.4.2] u = 0.80; [6.7.11] E_c = -0.80 V
    # -> driving voltage 0.25 V. M = 5.1514 * 30 * 8760 / (2000 * 0.80) = 846.12 kg -> 34 x 25 kg.
    # Bracelet 0.2 m x 0.04 m: A_a = pi (0.3239 + 0.08) 0.2 = 0.2538 m2, R = 0.315 * 0.30 / sqrt(A_a)
    # = 0.18759 ohm, I_a = 0.25 / 0.18759 = 1.3327 A -> N_final = ceil(6.8685 / 1.3327) = 6.
    # Spacing 1500 / 34 = 44.118 m <= 2 PL; Eq. 14 PL = sqrt(0.15 * 0.0159 * 0.308 /
    # (0.2e-6 * 0.3239 * 0.060 * 0.075)) = 1587.4 m.
    results = _run("pipeline")["results"]
    assert results["standard"] == "DNVGL-RP-F103 (September 2019, amended May 2021)"
    assert results["edition"] == "2019"
    densities = results["current_densities_A_m2"]
    assert densities["mean_current_density_A_m2"] == pytest.approx(0.075)
    assert densities["temperature_band"] == ">50-80"
    coating = results["coating_breakdown_factors"]
    assert coating["mean_factor"] == pytest.approx(0.045)
    assert coating["final_factor"] == pytest.approx(0.060)
    geometry = results["pipeline_geometry_m"]
    assert geometry["outer_surface_area_m2"] == pytest.approx(math.pi * 0.3239 * 1500.0, abs=1e-3)
    demand = results["current_demand_A"]
    assert demand["mean_current_demand_A"] == pytest.approx(1526.343 * 0.075 * 0.045, abs=1e-3)
    assert demand["final_current_demand_A"] == pytest.approx(1526.343 * 0.075 * 0.060, abs=1e-3)
    anodes = results["anode_requirements"]
    assert anodes["anode_capacity_Ah_kg"] == pytest.approx(2000.0)
    assert anodes["driving_voltage_V"] == pytest.approx(0.25)
    assert anodes["total_anode_mass_kg"] == pytest.approx(
        1526.343 * 0.075 * 0.045 * 30 * 8760 / (2000 * 0.80), abs=1e-2
    )
    assert anodes["anode_count"] == 34
    assert anodes["anode_count_by_mass"] == 34
    assert anodes["anode_count_by_final_current"] == 6
    assert anodes["utilization_factor"] == pytest.approx(0.80)
    assert anodes["anode_resistance_ohm"] == pytest.approx(0.18759, abs=1e-4)
    spacing = results["anode_spacing_m"]
    assert spacing["spacing_m"] == pytest.approx(1500.0 / 34.0, abs=1e-3)
    assert spacing["spacing_ok"] is True
    attenuation = results["attenuation_analysis"]
    expected_pl = math.sqrt(
        0.15 * 0.0159 * (0.3239 - 0.0159) / (0.2e-6 * 0.3239 * 0.060 * 0.075)
    )
    assert attenuation["protected_length_m"] == pytest.approx(expected_pl, rel=1e-4)
    assert attenuation["protection_adequate"] is True
    assert "dnv-rp-f103 2019-09 Table 6-2" in results["citations"]
    assert "dnv-rp-f103 2019-09 Table A-1" in results["citations"]
    status = results["status"]
    assert status["result"] == STATUS_PASS
    assert status["governing_case"] == "mass"
    assert status["use_status"] == USE_STATUS_CLIENT_EOR


def test_pipeline_f103_explicit_2010_edition_reproduces_earlier_results() -> None:
    # edition "2010" reproduces the pre-2026-09-27 default: Table 5-1 i_cm = 0.060 A/m2,
    # Table A.1 FBE a = 0.010, b = 0.0003 -> f_cm = 0.0145, f_cf = 0.019; I_cm = 1.328 A,
    # M = 1.328 * 30 * 8760 / (2000 * 0.80) = 218.1 kg -> 9 x 25 kg; spacing 166.7 m.
    cfg = _load("pipeline")
    cfg["inputs"]["design_data"]["edition"] = "2010"
    _assert_f103_2010_pipeline(run_cathodic_protection(cfg)["results"])


def _assert_f103_2010_pipeline(results: dict[str, Any]) -> None:
    assert results["standard"] == "DNV-RP-F103 (October 2010)"
    assert results["edition"] == "2010"
    assert results["current_densities_A_m2"]["mean_current_density_A_m2"] == pytest.approx(0.06)
    assert results["current_densities_A_m2"]["temperature_band"] == ">50-80"
    assert results["coating_breakdown_factors"]["mean_factor"] == pytest.approx(0.0145)
    assert results["coating_breakdown_factors"]["final_factor"] == pytest.approx(0.019)
    assert results["current_demand_A"]["mean_current_demand_A"] == pytest.approx(1.3279, abs=1e-3)
    assert results["anode_requirements"]["total_anode_mass_kg"] == pytest.approx(218.111, abs=1e-2)
    assert results["anode_requirements"]["anode_count"] == 9
    assert results["anode_requirements"]["anode_count_by_final_current"] == 2
    assert results["anode_spacing_m"]["spacing_m"] == pytest.approx(1500.0 / 9.0, abs=1e-3)
    assert "dnv-rp-f103 2010 Table 5-1" in results["citations"]
    assert "dnv-rp-f103 2010 Table A.1" in results["citations"]


def test_neutral_f103_key_without_edition_runs_the_2019_default() -> None:
    cfg = _load("pipeline")
    assert cfg["inputs"]["calculation_type"] == KEY_F103 == "DNV_RP_F103"
    assert "edition" not in cfg["inputs"]["design_data"]
    with warnings.catch_warnings():
        warnings.simplefilter("error", DeprecationWarning)
        results = run_cathodic_protection(cfg)["results"]
    assert results["edition"] == "2019"
    assert results["standard"] == "DNVGL-RP-F103 (September 2019, amended May 2021)"
    assert results["current_densities_A_m2"]["mean_current_density_A_m2"] == pytest.approx(0.075)
    assert results["anode_requirements"]["anode_count"] == 34
    assert results["status"]["use_status"] == USE_STATUS_CLIENT_EOR


def test_f103_2010_alias_pins_2010_with_deprecation() -> None:
    # Owner decision 2026-09-27: the historical key reproduces its pre-2019-default
    # results exactly (the pinned 2010 numbers above) and points at DNV_RP_F103.
    cfg = _load("pipeline")
    cfg["inputs"]["calculation_type"] = KEY_F103_2010
    with pytest.warns(DeprecationWarning, match="'DNV_RP_F103'"):
        results = run_cathodic_protection(cfg)["results"]
    _assert_f103_2010_pipeline(results)
    assert results["status"]["use_status"] == USE_STATUS_CLIENT_EOR


@pytest.mark.parametrize("edition", ["2010", "f103-2010"])
def test_f103_2010_alias_accepts_a_matching_edition(edition: str) -> None:
    cfg = _load("pipeline")
    cfg["inputs"]["calculation_type"] = KEY_F103_2010
    cfg["inputs"]["design_data"]["edition"] = edition
    with pytest.warns(DeprecationWarning):
        results = run_cathodic_protection(cfg)["results"]
    _assert_f103_2010_pipeline(results)


@pytest.mark.parametrize("edition", ["2019", "2021"])
def test_f103_2010_alias_rejects_a_conflicting_edition(edition: str) -> None:
    cfg = _load("pipeline")
    cfg["inputs"]["calculation_type"] = KEY_F103_2010
    cfg["inputs"]["design_data"]["edition"] = edition
    with pytest.warns(DeprecationWarning), pytest.raises(ValueError, match="use calculation_type 'DNV_RP_F103'"):
        run_cathodic_protection(cfg)


def test_pipeline_temperature_band_follows_the_edition() -> None:
    # 40 C is the 2010 Table 5-1 "<=50" column but the 2019 Table 6-2 ">25-50" column.
    bands = {}
    for edition in ("2010", "2019"):
        cfg = _load("pipeline")
        cfg["inputs"]["design_data"]["edition"] = edition
        cfg["inputs"]["pipeline"]["internal_fluid_temperature_C"] = 40.0
        results = run_cathodic_protection(cfg)["results"]
        bands[edition] = results["current_densities_A_m2"]["temperature_band"]
    assert bands == {"2010": "<=50", "2019": ">25-50"}


def test_pipeline_f103_spacing_failure_reports_fail() -> None:
    cfg = _load("pipeline")
    # 100 km buried 3LPP line (f_cf ~ 0.0019, i_cm 0.020 A/m2) with 400 kg bracelets:
    # both the mass and the final-current counts give 2 anodes, so the spacing is
    # 50 km while Eq. 14 with WT 6 mm gives PL ~ 10.8 km (2 PL ~ 21.6 km).
    cfg["inputs"]["pipeline"].update(
        length_m=100000.0,
        wall_thickness_m=0.006,
        coating_type="3LPP",
        burial_condition="buried",
        internal_fluid_temperature_C=20.0,
    )
    cfg["inputs"]["anode"].update(individual_anode_mass_kg=400.0, length_m=1.0, thickness_m=0.1)
    results = run_cathodic_protection(cfg)["results"]
    assert results["anode_requirements"]["anode_count"] == 2
    assert results["anode_spacing_m"]["spacing_m"] == pytest.approx(50000.0)
    assert results["anode_spacing_m"]["spacing_ok"] is False
    assert results["attenuation_analysis"]["protected_length_m"] == pytest.approx(10780.9, abs=0.5)
    assert results["attenuation_analysis"]["protection_adequate"] is False
    assert results["status"]["result"] == STATUS_FAIL


def test_pipeline_f103_requires_bracelet_dimensions() -> None:
    cfg = _load("pipeline")
    del cfg["inputs"]["anode"]["thickness_m"]
    with pytest.raises(ValueError, match="bracelet"):
        run_cathodic_protection(cfg)



# Issue #2256: 2019 field-joint coating ids (DNVGL-RP-F102 (2011) numbering).
# De-identified benchmark shape: F103 2019, FBE linepipe, 245 field joints of
# 0.4 m, FBE field joints (3A) with and without 4E(2) moulded PU infill.
FJC_JOINTS = 245
FJC_JOINT_LENGTH_M = 0.4


def _f103_2019_field_joint_cfg(fjc: str | None, infill: str | None) -> dict[str, Any]:
    cfg = _load("pipeline")
    cfg["inputs"]["design_data"]["edition"] = "2019"
    pipeline = cfg["inputs"]["pipeline"]
    pipeline.update(
        length_m=3000.0,
        field_joint_count=FJC_JOINTS,
        field_joint_length_m=FJC_JOINT_LENGTH_M,
    )
    if fjc is not None:
        pipeline["field_joint_coating"] = fjc
    if infill is not None:
        pipeline["field_joint_infill"] = infill
    return cfg


@pytest.mark.parametrize(
    ("fjc", "infill", "a", "b", "member", "infill_row"),
    [
        # Table A-2 (2019) row "3A FBE", infill none: a = 0.10, b = 0.010.
        ("3A", "none", 0.10, 0.010, "3A", "none"),
        # Same row with 4E(2) moulded PU on top: a = 0.03, b = 0.003.
        ("3A", "4E(2)", 0.03, 0.003, "3A+4E(2)", "4E(2) moulded PU on top"),
        # Case/whitespace-insensitive, and the May 2021 amended name.
        (" 3a ", "None", 0.10, 0.010, "3A", "none"),
        ("17A", "4e(2)", 0.03, 0.003, "3A+4E(2)", "4E(2) moulded PU on top"),
    ],
)
def test_f103_2019_fbe_field_joints_resolve_by_infill(
    fjc: str, infill: str, a: float, b: float, member: str, infill_row: str
) -> None:
    results = run_cathodic_protection(_f103_2019_field_joint_cfg(fjc, infill))["results"]
    assert results["edition"] == "2019"
    coating = results["coating_breakdown_factors"]
    assert coating["field_joint_coating"] == member
    assert coating["field_joint_infill"] == infill_row
    assert coating["field_joint_a"] == pytest.approx(a)
    assert coating["field_joint_b_per_yr"] == pytest.approx(b)
    # T = 30 yr: f_cm = a + b * 15, f_cf = a + b * 30.
    assert coating["mean_factor_field_joint"] == pytest.approx(a + b * 15.0)
    assert coating["final_factor_field_joint"] == pytest.approx(a + b * 30.0)
    area = results["pipeline_geometry_m"]["field_joint_area_m2"]
    assert area == pytest.approx(math.pi * 0.3239 * FJC_JOINTS * FJC_JOINT_LENGTH_M, abs=1e-3)
    assert "dnv-rp-f103 2019-09 Table A-2" in results["citations"]
    assert isinstance(results["anode_requirements"]["anode_count"], int)
    _assert_status(results["status"])


@pytest.mark.parametrize("fjc", ["2B(1)", "2c(2)", "5A/B/C(1)", "14B_LE"])
def test_f103_2019_accepts_other_f102_ids_without_infill(fjc: str) -> None:
    results = run_cathodic_protection(_f103_2019_field_joint_cfg(fjc, None))["results"]
    assert results["coating_breakdown_factors"]["field_joint_a"] > 0.0


def test_f103_2019_3a_without_infill_is_ambiguous() -> None:
    cfg = _f103_2019_field_joint_cfg("3A", None)
    with pytest.raises(ValueError, match=r"field_joint_infill.*\['none', '4E\(2\)'\]"):
        run_cathodic_protection(cfg)


def test_f103_2019_rejects_an_invalid_infill() -> None:
    cfg = _f103_2019_field_joint_cfg("2C(2)", "4E(2)")
    with pytest.raises(ValueError, match=r"valid choices: \['none'\]"):
        run_cathodic_protection(cfg)


def test_f103_2019_unknown_field_joint_id_lists_valid_ids() -> None:
    cfg = _f103_2019_field_joint_cfg("3Z", "none")
    with pytest.raises(ValueError, match=r"Unknown field-joint coating '3Z'.*'2B\(1\)'.*'5A/B/C\(1\)'"):
        run_cathodic_protection(cfg)


def test_f103_2010_field_joint_ids_unchanged() -> None:
    # 2010 Table A.2 row "3A FBE": a = 3/100 = 0.03, b = 0.3/100 = 0.003.
    cfg = _f103_2019_field_joint_cfg("3A", None)
    cfg["inputs"]["design_data"]["edition"] = "2010"
    coating = run_cathodic_protection(cfg)["results"]["coating_breakdown_factors"]
    assert coating["field_joint_coating"] == "3A"
    assert coating["field_joint_infill"] == "none"
    assert coating["field_joint_a"] == pytest.approx(0.03)
    assert coating["field_joint_b_per_yr"] == pytest.approx(0.003)
    assert coating["mean_factor_field_joint"] == pytest.approx(0.03 + 0.003 * 15.0)
    cfg["inputs"]["pipeline"]["field_joint_coating"] = "2B(1)"
    with pytest.raises(ValueError, match=r"DNV-RP-F103 \(2010\) Table A.2; valid ids"):
        run_cathodic_protection(cfg)


# ---------------------------------------------------------------------------
# ABS routes (legacy implementation, wrapped)
# ---------------------------------------------------------------------------


@pytest.mark.parametrize("flag", [None, False, "true", "false", 1, [], {}])
def test_ships_requires_explicit_experimental_boolean(
    flag: Any, monkeypatch: pytest.MonkeyPatch
) -> None:
    cfg = _load("ships")
    cfg["inputs"]["design_data"]["experimental"] = flag
    before = copy.deepcopy(cfg)

    def forbidden_solver() -> Any:
        pytest.fail("quarantined ships route must reject before constructing the solver")

    monkeypatch.setattr(engine_adapter, "_legacy_solver", forbidden_solver)
    with pytest.raises(ExperimentalModelError) as caught:
        run_cathodic_protection(cfg)
    message = str(caught.value)
    for wording in (
        "ABS_gn_ships_2018",
        "mean demand by about a third",
        "final demand by about half",
        "2026-09-27",
        "#2259",
        "#1852",
        "inputs.design_data.experimental",
    ):
        assert wording in message
    assert cfg == before


@pytest.mark.parametrize("design_data", [{}, None, "invalid"])
def test_ships_without_design_data_is_quarantined(design_data: Any) -> None:
    cfg = {"inputs": {"calculation_type": "ABS_gn_ships_2018"}}
    if design_data is not None:
        cfg["inputs"]["design_data"] = design_data
    with pytest.raises(ExperimentalModelError):
        run_cathodic_protection(cfg)


def test_ships_without_experimental_flag_is_quarantined() -> None:
    cfg = _load("ships")
    cfg["inputs"]["design_data"].pop("experimental", None)
    with pytest.raises(ExperimentalModelError):
        run_cathodic_protection(cfg)


def test_ships_abs_2018_wrapped_with_int_count_and_status() -> None:
    cfg = _run("ships")
    cp = cfg["cathodic_protection"]
    assert cfg["results"] is cp
    assert cp["current_demand_A"]["totals"]["mean"] == pytest.approx(196.667146)
    req = cp["anode_requirements"]
    assert req["total_mass_kg"] == pytest.approx(5067.071173)
    assert req["anode_count_raw"] == pytest.approx(184.257134)
    assert req["anode_count"] == 185
    checks = cp["anode_performance"]["checks"]
    assert checks["initial_meets_demand"] is False
    assert checks["final_meets_demand"] is False
    status = cp["status"]
    assert status["result"] == STATUS_FAIL
    assert status["governing_case"] == "final"
    assert status["use_status"] == "experimental-known-understatement"
    assert status["checks"] == {
        "initial_current_output": False,
        "final_current_output": False,
    }


def test_fpso_abs_offshore_2018_wrapped_mass_only() -> None:
    results = _run("fpso")["results"]
    assert results["current_demand_A"]["mean"] == pytest.approx(96.0)
    assert results["current_demand_A"]["final"] == pytest.approx(248.0)
    assert results["anode_current_capacity_Ah_kg"] == pytest.approx(1865.0)
    assert results["anode_mass_kg"] == pytest.approx(14091.153)
    req = results["anode_requirements"]
    assert req["anode_count"] == 79 == math.ceil(14091.153 / 180.0)
    status = results["status"]
    assert status["result"] == STATUS_PASS
    assert status["governing_case"] == "mass"
    assert status["checks"]["current_output"] is None
    assert "no current-output check" in status["reason"]
    assert status["use_status"] == USE_STATUS_LEGACY_UNCITED


# ---------------------------------------------------------------------------
# Legacy keys and dispatch
# ---------------------------------------------------------------------------


def test_legacy_b401_key_runs_old_solver_with_deprecation() -> None:
    cfg = _load("jacket")
    cfg["inputs"]["calculation_type"] = KEY_B401_LEGACY
    with pytest.warns(DeprecationWarning, match="DNV_RP_B401_offshore"):
        results = run_cathodic_protection(cfg)["results"]
    # The legacy route keeps its own (fresh-anode only) verification and no
    # PASS/FAIL verdict; its status block carries the use status only.
    assert results["current_demand_A"]["total_mean_A"] == pytest.approx(85.0)
    assert results["current_output_verification"]["recommended_anode_count"] == 135
    assert results["status"] == {"use_status": USE_STATUS_LEGACY_UNCITED}


def test_legacy_f103_key_runs_old_solver_with_deprecation(monkeypatch: pytest.MonkeyPatch) -> None:
    monkeypatch.delenv("LLM_WIKI_PATH", raising=False)
    monkeypatch.delenv("DIGITALMODEL_REPO_ROOT", raising=False)
    cfg = _load("pipeline")
    cfg["inputs"]["calculation_type"] = KEY_F103_LEGACY
    with pytest.warns(DeprecationWarning, match="DNV_RP_F103_2010"):
        results = run_cathodic_protection(cfg)["results"]
    assert results["current_demand_A"]["mean_current_demand_A"] == pytest.approx(1.328)
    assert "attenuation_analysis" in results
    assert results["status"] == {"use_status": USE_STATUS_LEGACY_UNCITED}


def test_unknown_calculation_type_lists_accepted_keys() -> None:
    cfg = _load("jacket")
    cfg["inputs"]["calculation_type"] = "DNV_rp_b401_2011"
    with pytest.raises(ValueError) as excinfo:
        run_cathodic_protection(cfg)
    message = str(excinfo.value)
    for key in CALCULATION_TYPES:
        assert key in message


def test_adapter_returns_same_cfg_object() -> None:
    cfg = _load("monopile")
    snapshot = copy.deepcopy(cfg["inputs"])
    with warnings.catch_warnings():
        warnings.simplefilter("error")
        out = run_cathodic_protection(cfg)
    assert out is cfg
    assert cfg["inputs"] == snapshot


def test_engine_dispatches_cathodic_protection_to_adapter(monkeypatch: pytest.MonkeyPatch) -> None:
    from digitalmodel import engine as engine_module

    seen: list[dict[str, Any]] = []

    def fake(cfg: dict[str, Any]) -> dict[str, Any]:
        seen.append(cfg)
        cfg["results"] = {"status": {"result": STATUS_PASS}}
        return cfg

    monkeypatch.setattr(engine_module, "run_cathodic_protection", fake)
    monkeypatch.setattr(engine_module.app_manager, "save_cfg", lambda cfg_base: None)
    cfg = {"basename": "cathodic_protection", "inputs": {"calculation_type": "x"}}
    out = engine_module.engine(cfg=cfg, config_flag=False)
    assert seen == [cfg]
    assert out["results"]["status"]["result"] == STATUS_PASS
    assert engine_adapter.run_cathodic_protection is not fake
