"""B401 concrete-zone and per-family anode design tests for issue 2262."""

from __future__ import annotations

import math

import pytest
from pydantic import ValidationError

from digitalmodel.cathodic_protection.b401_tables import Climate, DesignPhase
from digitalmodel.cathodic_protection.engine_adapter import run_cathodic_protection
from digitalmodel.cathodic_protection.marine_structure_cp import (
    ExposureZone,
    StructuralZone,
    marine_structure_current_demand,
    zone_current_density,
)
from digitalmodel.cathodic_protection.report_adapters import anode_design_report
from digitalmodel.reporting import TableBlock


def _mixed_family_cfg() -> dict:
    """Rounded, standards-derived mixed seawater/sediment design."""
    return {
        "basename": "cathodic_protection",
        "inputs": {
            "calculation_type": "DNV_RP_B401_offshore",
            "design_data": {"edition": "2021", "design_life": 20.0},
            "environment": {"seawater_temperature_C": 5.0},
            "structure": {
                "zones": [
                    {
                        "zone": "clamp_steel",
                        "base_zone": "submerged",
                        "depth_m": 1000.0,
                        "area_m2": 10.0,
                        "coating_category": "bare",
                        "anode_family": "sea",
                    },
                    {
                        "zone": "buried_frame",
                        "base_zone": "buried",
                        "area_m2": 20.0,
                        "coating_category": "bare",
                        "anode_family": "mud",
                    },
                    {
                        "zone": "mattress_reinforcement",
                        "base_zone": "concrete_embedded",
                        "depth_m": 1000.0,
                        "reinforcement_area_m2": 100.0,
                        "anode_family": "mud",
                    },
                ]
            },
            "anode_families": [
                {
                    "name": "sea",
                    "environment": "seawater",
                    "material": "aluminium",
                    "type": "stand_off",
                    "individual_anode_mass_kg": 50.0,
                    "length_m": 2.0,
                    "electrolyte_resistivity_ohm_m": 0.30,
                    "utilization_factor": 0.85,
                },
                {
                    "name": "mud",
                    "environment": "sediments",
                    "material": "aluminium",
                    "type": "flush_mounted",
                    "individual_anode_mass_kg": 25.0,
                    "length_m": 1.20,
                    "width_m": 0.09,
                    "thickness_m": 0.07,
                    "final_length_m": 1.00,
                    "final_width_m": 0.06,
                    "final_thickness_m": 0.05,
                    "electrolyte_resistivity_ohm_m": 1.0,
                    "anode_surface_temperature_C": 60.0,
                    "utilization_factor": 0.85,
                },
            ],
        },
    }


def _single_anode_cfg() -> dict:
    cfg = _mixed_family_cfg()
    family = cfg["inputs"].pop("anode_families")[0]
    cfg["inputs"]["anode"] = family
    cfg["inputs"]["environment"]["seawater_resistivity_ohm_m"] = 0.30
    for zone in cfg["inputs"]["structure"]["zones"]:
        zone.pop("anode_family")
    return cfg


def test_concrete_zone_requires_reinforcement_area() -> None:
    with pytest.raises(ValidationError, match="reinforcement_area_m2"):
        StructuralZone(
            zone_name="mat",
            exposure_zone=ExposureZone.CONCRETE_EMBEDDED,
            surface_area_m2=100.0,
            depth_m=1000.0,
        )

    zone = StructuralZone(
        zone_name="mat",
        exposure_zone=ExposureZone.CONCRETE_EMBEDDED,
        reinforcement_area_m2=12.0,
        depth_m=1000.0,
    )
    assert zone.design_area_m2 == 12.0
    assert zone.area_basis == "reinforcement_steel"

    with pytest.raises(ValidationError, match="coating_breakdown_factor"):
        StructuralZone(
            zone_name="mat",
            exposure_zone=ExposureZone.CONCRETE_EMBEDDED,
            reinforcement_area_m2=12.0,
            coating_breakdown_factor=0.1,
        )


@pytest.mark.parametrize(
    ("edition", "section"),
    [
        ("2005", "Table 10-3"),
        ("2010", "Table 10-3"),
        ("2017", "Table A-3"),
        ("2021", "Table 8-3"),
    ],
)
def test_concrete_density_is_constant_and_edition_cited(
    edition: str, section: str
) -> None:
    values = [
        zone_current_density(
            ExposureZone.CONCRETE_EMBEDDED,
            Climate.ARCTIC,
            1000.0,
            phase,
            edition,  # type: ignore[arg-type]
        )
        for phase in DesignPhase
    ]
    # Table x-3, Arctic >100 m: 0.0006 A/m2 of reinforcement steel.
    assert all(value is not None and value.value == 0.0006 for value in values)
    assert all(
        value is not None and value.citation.section == section for value in values
    )


def test_two_anode_families_keep_demand_and_electrochemistry_separate() -> None:
    results = run_cathodic_protection(_mixed_family_cfg())["results"]
    demand = results["current_demand_A"]

    # Table 8-1/8-2, Arctic >300 m: 10 m2 x 0.220/0.110/0.170.
    assert demand["clamp_steel"]["I_initial_A"] == pytest.approx(2.2)
    assert demand["clamp_steel"]["I_mean_A"] == pytest.approx(1.1)
    assert demand["clamp_steel"]["I_final_A"] == pytest.approx(1.7)
    # Buried: 20 x 0.020 = 0.400 A; concrete: 100 x 0.0006 = 0.060 A.
    assert demand["buried_frame"]["I_mean_A"] == pytest.approx(0.4)
    assert demand["mattress_reinforcement"]["I_mean_A"] == pytest.approx(0.06)

    sea = results["anode_families"]["sea"]
    mud = results["anode_families"]["mud"]
    assert sea["current_demand_A"] == pytest.approx(
        {"initial": 2.2, "mean": 1.1, "final": 1.7}
    )
    assert mud["current_demand_A"] == pytest.approx(
        {"initial": 0.46, "mean": 0.46, "final": 0.46}
    )

    # Table 8-6: Al seawater <=30 C gives 2000 Ah/kg, -1.05 V, delta E 0.25 V.
    assert (sea["capacity_Ah_kg"], sea["closed_circuit_potential_V"]) == (
        2000.0,
        -1.05,
    )
    assert sea["driving_voltage_V"] == pytest.approx(0.25)
    # Table 8-6: Al sediment 60 C gives 680 Ah/kg, -1.00 V, delta E 0.20 V.
    assert (mud["capacity_Ah_kg"], mud["closed_circuit_potential_V"]) == (
        680.0,
        -1.0,
    )
    assert mud["driving_voltage_V"] == pytest.approx(0.20)
    assert any("Table 8-7" in citation for citation in mud["citations"])

    # Eq. 2: M = I_mean * years * 8760 / (capacity * utilization).
    assert sea["required_mass_kg"] == pytest.approx(1.1 * 20 * 8760 / (2000 * 0.85))
    assert mud["required_mass_kg"] == pytest.approx(0.46 * 20 * 8760 / (680 * 0.85))
    assert sea["count_by_mass"] == math.ceil(sea["required_mass_kg"] / 50.0)
    assert mud["count_by_mass"] == math.ceil(mud["required_mass_kg"] / 25.0)

    # Table 8-7 long flush: R = rho / (2S), S=(L+W)/2 -> rho/(L+W).
    assert mud["resistance_initial_ohm"] == pytest.approx(1.0 / (1.20 + 0.09))
    assert mud["resistance_final_ohm"] == pytest.approx(1.0 / (1.00 + 0.06))
    assert mud["current_output_initial_A"] == pytest.approx(
        0.20 / mud["resistance_initial_ohm"]
    )
    assert mud["current_output_final_A"] == pytest.approx(
        0.20 / mud["resistance_final_ohm"]
    )

    # Sec. 7.8: ceil(I/Ia) gives sea 1/1 and mud 2/3 for initial/final;
    # Eq. 2 mass counts of 3 and 6 therefore govern each family.
    assert (
        sea["count_by_initial_current"],
        sea["count_by_final_current"],
        sea["recommended_anode_count"],
        sea["governing_case"],
    ) == (1, 1, 3, "mass")
    assert (
        mud["count_by_initial_current"],
        mud["count_by_final_current"],
        mud["recommended_anode_count"],
        mud["governing_case"],
    ) == (2, 3, 6, "mass")

    assert sea["count_source"] == mud["count_source"] == "recommended"
    assert sea["checks"] == {"mass": True, "initial": True, "final": True}
    assert mud["checks"] == {"mass": True, "initial": True, "final": True}
    assert results["status"]["result"] == "PASS"
    assert results["status"]["governing_family"] == "mud"
    assert results["status"]["governing_case"] == "mass"
    assert results["status"]["governing_ratio"] == pytest.approx(
        mud["required_mass_kg"] / (6 * 25.0)
    )


def test_installed_count_checks_fail_per_family_and_overall() -> None:
    cfg = _mixed_family_cfg()
    for family in cfg["inputs"]["anode_families"]:
        family["count"] = 1

    results = run_cathodic_protection(cfg)["results"]
    sea = results["anode_families"]["sea"]
    mud = results["anode_families"]["mud"]

    # Eq. 2 gives 113.36 kg seawater and 139.42 kg sediment demand, so one
    # 50 kg or 25 kg anode respectively fails each family's mass check.
    assert sea["checks"]["mass"] is False
    assert mud["checks"]["mass"] is False
    assert sea["count_source"] == mud["count_source"] == "input"
    assert results["status"]["result"] == "FAIL"
    assert results["status"]["governing_family"] == "mud"
    assert results["status"]["governing_case"] == "mass"


@pytest.mark.parametrize(("length_m", "expected_u"), [(0.399, 0.80), (0.400, 0.85)])
def test_flush_utilization_uses_fresh_geometry(
    length_m: float, expected_u: float
) -> None:
    cfg = _mixed_family_cfg()
    family = cfg["inputs"]["anode_families"][0]
    family.pop("utilization_factor")
    family.update(
        type="flush_mounted",
        length_m=length_m,
        width_m=0.1,
        thickness_m=0.1,
        final_length_m=0.3,
        final_width_m=0.08,
        final_thickness_m=0.08,
    )
    # Table 8-8: L < 4W is short/other (u=0.80); the L=4W boundary is long
    # flush-mounted (u=0.85).
    sea = run_cathodic_protection(cfg)["results"]["anode_families"]["sea"]
    assert sea["utilization_factor"] == expected_u
    assert sea["utilization_factor_source"] == "Table 8-8"


def test_explicit_standoff_radius_controls_final_geometry() -> None:
    cfg = _mixed_family_cfg()
    family = cfg["inputs"]["anode_families"][0]
    family["radius_m"] = 0.2
    # Constant-length depletion gives r_final = r_initial * sqrt(1-u).
    sea = run_cathodic_protection(cfg)["results"]["anode_families"]["sea"]
    assert sea["equivalent_radius_initial_m"] == pytest.approx(0.2)
    assert sea["equivalent_radius_final_m"] == pytest.approx(0.2 * math.sqrt(0.15))


@pytest.mark.parametrize(("radius_m", "expected_u"), [(0.251, 0.85), (0.250, 0.90)])
def test_standoff_utilization_uses_fresh_geometry(
    radius_m: float, expected_u: float
) -> None:
    cfg = _mixed_family_cfg()
    family = cfg["inputs"]["anode_families"][0]
    family.pop("utilization_factor")
    family.update(length_m=1.0, radius_m=radius_m)
    # Table 8-8: L < 4r is short slender (u=0.85); L=4r is long (u=0.90).
    sea = run_cathodic_protection(cfg)["results"]["anode_families"]["sea"]
    assert sea["utilization_factor"] == expected_u


def test_sub_microamp_zone_is_not_rounded_before_family_sizing() -> None:
    cfg = _mixed_family_cfg()
    tiny = cfg["inputs"]["structure"]["zones"][2]
    tiny["reinforcement_area_m2"] = 0.0001
    cfg["inputs"]["structure"]["zones"] = [tiny]
    cfg["inputs"]["anode_families"] = [cfg["inputs"]["anode_families"][1]]

    mud = run_cathodic_protection(cfg)["results"]["anode_families"]["mud"]
    # Table 8-3: 0.0001 m2 x 0.0006 A/m2 = 6e-8 A; Eq. 2 uses that
    # unrounded current rather than treating the family as zero demand.
    assert mud["current_demand_A"]["mean"] == pytest.approx(6e-8)
    assert mud["required_mass_kg"] == pytest.approx(6e-8 * 20 * 8760 / (680 * 0.85))


@pytest.mark.parametrize(
    ("mutator", "message"),
    [
        (lambda cfg: cfg["inputs"].update(anode={"type": "stand_off"}), "mixed"),
        (
            lambda cfg: cfg["inputs"]["anode_families"].append(
                dict(cfg["inputs"]["anode_families"][0])
            ),
            "duplicate",
        ),
        (
            lambda cfg: cfg["inputs"]["structure"]["zones"][0].update(
                anode_family="missing"
            ),
            "missing",
        ),
        (
            lambda cfg: cfg["inputs"]["structure"]["zones"][2].update(area_m2=999.0),
            "surface_area_m2",
        ),
        (
            lambda cfg: cfg["inputs"]["anode_families"][1].update(final_length_m=2.0),
            "final flush dimensions",
        ),
    ],
)
def test_family_schema_fails_closed(mutator, message: str) -> None:
    cfg = _mixed_family_cfg()
    mutator(cfg)
    with pytest.raises(ValueError, match=message):
        run_cathodic_protection(cfg)


def test_single_anode_mode_rejects_zone_family_assignments() -> None:
    cfg = _mixed_family_cfg()
    cfg["inputs"].pop("anode_families")
    cfg["inputs"]["anode"] = {"type": "stand_off"}
    with pytest.raises(ValueError, match="require inputs.anode_families"):
        run_cathodic_protection(cfg)


def test_direct_single_anode_api_rejects_zone_family_assignments() -> None:
    zone = StructuralZone(
        zone_name="steel",
        exposure_zone=ExposureZone.SUBMERGED,
        surface_area_m2=1.0,
        anode_family="sea",
    )
    with pytest.raises(ValueError, match="does not size anode families"):
        marine_structure_current_demand([zone], edition="2021")


def test_family_report_exposes_area_basis_and_family_checks() -> None:
    cfg = run_cathodic_protection(_mixed_family_cfg())
    spec = anode_design_report(cfg)
    tables = {
        block.title: block
        for section in spec.sections
        for block in section.blocks
        if isinstance(block, TableBlock)
    }
    zones = tables["Current density, area basis and demand by zone"]
    rows = {row[0]: row for row in zones.rows}
    assert "reinforcement steel" in rows["mattress_reinforcement"]
    families = tables["Anode family mass and count checks"]
    assert {row[0] for row in families.rows} == {"sea", "mud"}
    output = tables["Anode family current-output checks"]
    assert {row[0] for row in output.rows} == {"sea", "mud"}
    assert "Initial demand" in output.columns
    assert "Total final output" in output.columns
    references = tables["Where each cited table was used"]
    cited = " ".join(str(cell) for row in references.rows for cell in row)
    assert "Table 8-3" in cited
    assert "Table 8-6" in cited


def test_single_anode_report_retains_reinforcement_area_basis() -> None:
    cfg = run_cathodic_protection(_single_anode_cfg())
    concrete = cfg["results"]["current_demand_A"]["mattress_reinforcement"]
    assert concrete["area_basis"] == "reinforcement_steel"
    spec = anode_design_report(cfg)
    tables = [
        block
        for section in spec.sections
        for block in section.blocks
        if isinstance(block, TableBlock)
    ]
    demand_table = next(
        table
        for table in tables
        if table.title == "Current density, coating breakdown and demand by zone"
    )
    assert "Area basis" in demand_table.columns
    assert "reinforcement_steel" in next(
        row for row in demand_table.rows if row[0] == "mattress_reinforcement"
    )
    assert any("Table 8-4" in table.title for table in tables)
    output_section = next(section for section in spec.sections if section.key == "adequacy")
    assert "Table 8-7" in (output_section.subtitle or "")
