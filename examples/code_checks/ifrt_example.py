"""Synthetic 30 m x 15 m IFRT example; print masses and citation sidecars.

Run with PYTHONPATH=src uv run python examples/code_checks/ifrt_example.py.
No data is downloaded or saved. Vent inputs are illustrative, not API factors.
"""

from dataclasses import asdict, replace
import json
import math
from pathlib import Path

from digitalmodel.citations.schema import Citation
from digitalmodel.code_checks.storage_tanks import ifrt


def worked_example() -> dict:
    """Assumed input provenance: bundled ifrt-example.md, revision 1."""
    case = ifrt.IFRTCase(
        tank=ifrt.Tank(30.0, 15.0, (math.pi * 0.3,) * 6),
        liquid=ifrt.Liquid(35.0, 101.325, 62.0, 740.0),
        rim_seal=ifrt.public_seal_factor("vapor_mounted"),
        fittings=tuple(
            ifrt.DeckFitting(name, count, ifrt.public_fitting_factor(name))
            for name, count in (
                ("access_hatch_bolted_gasketed", 1),
                ("gauge_hatch_gasketed", 1),
                ("vacuum_breaker_gasketed", 1),
                ("unslotted_guidepole_gasketed_sleeve", 1),
                ("column_well_fabric_sleeve", 6),
                ("ifrt_leg_adjustable", 20),
            )
        ),
        deck_type="bolted",
        annual_net_throughput_m3=100_000,
        clingage=ifrt.gasoline_clingage(),
    )
    baseline = ifrt.estimate_losses(case)
    comparisons = ifrt.compare_seals(
        case,
        {
            "vapor_primary_average": ifrt.public_seal_factor("vapor_mounted"),
            "liquid_primary_average": ifrt.public_seal_factor("liquid_mounted"),
            "shoe_primary_average": ifrt.public_seal_factor("mechanical_shoe"),
            "liquid_rim_secondary_average": ifrt.public_seal_factor(
                "liquid_mounted", secondary="rim_mounted"
            ),
        },
    )
    vent_source = Citation(
        "ILLUSTRATIVE-IFRT",
        "Digitalmodel",
        "1",
        "Independent vent illustration",
        "wikis/engineering-standards/ifrt-example.md",
        source_sibling="generic",
        note="Synthetic duties and rated curves; no API 2000 qualification",
    )
    vent = ifrt.check_normal_venting(
        fill_m3_h=500,
        withdrawal_m3_h=750,
        basis=ifrt.NormalVentingBasis(1, 2, 1000, 1000, vent_source),
        pressure_vent=ifrt.RatedVent(2200, 1.5, 0.2, "Synthetic pressure curve point"),
        vacuum_vent=ifrt.RatedVent(2000, 0.3, 0.1, "Synthetic vacuum curve point"),
        tank_pressure_limit_kpag=2,
        tank_vacuum_limit_kpa=0.5,
        wiki_root=Path(ifrt.__file__).parent,
    )
    return {
        "input_status": "SYNTHETIC_NOT_DESIGN_DATA",
        "inputs": asdict(case),
        "gross_capacity_m3": case.tank.volume_m3,
        "baseline_kg_year": {
            "rim": baseline.rim_kg_year,
            "fittings": baseline.fittings_kg_year,
            "seams": baseline.seams_kg_year,
            "withdrawal": baseline.withdrawal_kg_year,
            "standing": baseline.standing_kg_year,
            "total": baseline.total_kg_year,
        },
        "seal_total_kg_year": {
            name: result.total_kg_year for name, result in comparisons.items()
        },
        "welded_deck_total_kg_year": ifrt.estimate_losses(
            replace(case, deck_type="welded")
        ).total_kg_year,
        "emissions_sources": baseline.citation_sidecar(),
        "independent_illustrative_normal_vent": {
            **asdict(vent),
            "normal_check_passed": vent.normal_check_passed,
            "limits": "Emergency relief and API qualification are not evaluated",
        },
    }


if __name__ == "__main__":
    print(json.dumps(worked_example(), indent=2, allow_nan=False))
