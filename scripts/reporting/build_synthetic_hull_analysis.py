# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Build an assumed external hull analysis with the existing ABS ships route."""

from __future__ import annotations

import argparse
import copy
import hashlib
import json
import math
from pathlib import Path
from typing import Any

from digitalmodel.cathodic_protection.engine_adapter import run_cathodic_protection

ROOT = Path(__file__).resolve().parents[2]
FIXTURE = ROOT / "tests/fixtures/cathodic_protection/workflow_inputs/synthetic_hull.json"
DESTINATION = ROOT / "docs/domains/cathodic_protection/reports/synthetic-hull"
CITATION_ROOT = ROOT / "tests/citations/fixtures"
NAME = "cathodic-protection-analysis"


def _arrangement(result: dict[str, Any], inputs: dict[str, Any]) -> dict[str, Any]:
    req = result["anode_requirements"]
    count = req["selected_anode_count"]
    demand = result["current_demand_A"]["totals"]
    performance = result["anode_performance"]["current_output_A"]
    values = [
        ("Mean-current lifetime mass", req["total_mass_kg"],
         count * req["individual_mass_kg"], "kg"),
        ("Initial current output", demand["initial"], performance["initial_total"], "A"),
        ("Mean current output", demand["mean"],
         count * min(performance["initial_per_anode"], performance["final_per_anode"]), "A"),
        ("Final current output", demand["final"], performance["final_total"], "A"),
    ]
    return {
        "anode_count": count,
        "checks": [
            {"quantity": name, "required": required, "provided": provided,
             "unit": unit, "margin_percent": 100 * (provided / required - 1),
             "passes": provided >= required}
            for name, required, provided, unit in values
        ],
        "count_by_check": {
            "mass": req["mass_count"], "initial_current_output": req["initial_output_count"],
            "mean_current_output": req["mean_output_count"],
            "final_current_output": req["final_output_count"],
            "layout_distribution": result["layout"]["minimum_layout_count"],
        },
        "recommended_anode_count": req["recommended_anode_count"],
        "governing_case": result["status"]["governing_case"],
        "status": result["status"]["result"],
        "layout": result["layout"],
        "layout_input_class": "ASSUMED",
        "net_mass_per_anode_kg": inputs["anode"]["physical_properties"]["net_weight"],
    }


def calculate(cfg: dict[str, Any] | None = None, *,
              citation_repo_root: Path = CITATION_ROOT) -> dict[str, Any]:
    """Calculate both arrangements; retain full precision in the evidence file.

    Public citation fixtures check locator metadata only. They do not establish
    source access, standards compliance or the applicability of assumed values.
    """
    cfg = copy.deepcopy(cfg) if cfg is not None else json.loads(FIXTURE.read_text())
    inputs = cfg["inputs"]
    geometry = cfg["hull_geometry"]
    length, beam, draught = (geometry[key] for key in ("length_m", "beam_m", "draught_m"))
    area = length * beam + 2 * length * draught + 2 * beam * draught
    if not math.isclose(area, inputs["structure"]["steel_total_area"]):
        raise ValueError("hull area differs from assumed rectangular geometry")
    anode = inputs["anode"]
    physical, shape = anode["physical_properties"], anode["geometry"]
    net_mass = (shape["width_m"] * physical["height"] - physical["core_cross_section_m2"]) * shape["length_m"] * anode["anode_density"]
    if not math.isclose(net_mass, physical["net_weight"]):
        raise ValueError("anode net mass differs from assumed rectangular geometry")
    cfg["citation_repo_root"] = str(citation_repo_root)
    stated_result = run_cathodic_protection(copy.deepcopy(cfg))["results"]
    if stated_result["anode_current_capacity"] != cfg["route_lookup_basis"]["capacity_Ah_kg"]:
        raise ValueError("capacity lookup differs from declared assumed performance basis")
    required = stated_result["anode_requirements"]["recommended_anode_count"]
    candidate = copy.deepcopy(cfg)
    per_location = inputs["layout"]["anodes_per_location"]
    candidate["inputs"]["layout"]["selected_locations"] = math.ceil(required / per_location)
    rechecked_result = run_cathodic_protection(candidate)["results"]
    stated = _arrangement(stated_result, inputs)
    rechecked = _arrangement(rechecked_result, candidate["inputs"])
    added = rechecked["anode_count"] - stated["anode_count"]
    result = {
        "status": "Issued for information; synthetic assumed basis",
        "case_kind": cfg["case_kind"],
        "input_sha256": hashlib.sha256(
            # The digest describes the actual calculation input, excluding only
            # the runtime citation-root path. File bytes are used for default runs.
            FIXTURE.read_bytes() if cfg_without_root(cfg) == json.loads(FIXTURE.read_text())
            else (json.dumps(cfg_without_root(cfg), sort_keys=True) + "\n").encode()
        ).hexdigest(),
        "arrangements": {"stated": stated, "rechecked": rechecked},
        "zone_demand_A": {"external_hull": {
            f"I_{phase}_A": value
            for phase, value in stated_result["current_demand_A"]["totals"].items()
        }},
        "citations": [{"code_id": "abs-gn-ships", "publisher": "ABS", "revision": "2017-12",
                       "section": label, "note": "Existing route locator; applicability remains assumed.",
                       "source_path": "src/digitalmodel/cathodic_protection/abs_ships.py"}
                      for label in stated_result["citations"]],
        "geometry_sources": [{"path": FIXTURE.relative_to(ROOT).as_posix(),
                              "input_class": "ASSUMED", "basis": cfg["description"]}],
        "input_basis": cfg["input_basis"],
        "route_lookup_basis": cfg["route_lookup_basis"],
        "reporting_standard_revision": None,
        "sensitivity": [],
        "local_potential_assessment": "NOT_EVALUATED",
        "marketing_cleared": False,
        "recommendations": {"design_additional_anodes": added,
                            "design_additional_mass_kg": added * physical["net_weight"],
                            "basis": "Retain the assumed paired layout; confirm placement before acceptance."},
        "monitoring": {"status": "NOT_EVALUATED", "frequency": "NOT_EVALUATED",
                       "trigger_potential": "NOT_EVALUATED", "retrofit_quantity": "NOT_EVALUATED",
                       "intervention_date": "NOT_EVALUATED",
                       "positions": "deepest bottom, remote bow and stern, appendages and shielded positions",
                       "required_inputs": "qualified criterion and electrode, layout, baseline survey, operating exposure, depletion and access dates"},
    }
    result["report_sha256"] = hashlib.sha256(render_note(result).encode()).hexdigest()
    return result


def cfg_without_root(cfg: dict[str, Any]) -> dict[str, Any]:
    """Exclude the runtime locator root from portable input evidence."""
    return {key: value for key, value in cfg.items() if key != "citation_repo_root"}


def render_note(result: dict[str, Any]) -> str:
    """Render the short case note from unrounded results."""
    stated, sized = (result["arrangements"][key] for key in ("stated", "rechecked"))
    rec = result["recommendations"]
    lines = [
        "# Synthetic hull cathodic protection analysis", "", "## Summary", "",
        f"The {stated['anode_count']}-anode trial does not meet the final-current criterion. "
        f"Final current governs the aggregate requirement of {stated['recommended_anode_count']} anodes. "
        f"The {sized['anode_count']}-anode re-sized arrangement meets the calculated aggregate comparisons on the assumed basis.",
        "", "## Revision history", "",
        "| Revision | Date | What changed and why |", "|---|---|---|",
        "| 0 | 2026-10-25 | Initial synthetic case for reproducible hull screening. |", "",
        "## Inputs and basis", "",
        "All case inputs are ASSUMED, based on previous experience. No actual hull, site or supplier is represented. "
        "The [input file](../../../../../tests/fixtures/cathodic_protection/workflow_inputs/synthetic_hull.json) "
        "maps every input to its unit and assumption basis. No public hull source is claimed.", "",
        "The rectangular submerged shell includes the bottom, both sides and both ends. "
        "It excludes the waterplane and appendages. Coated and bare areas are counted separately. "
        "The ideal anode net mass deducts its through core. Geometry is an idealisation, not a product drawing.", "",
        "## Method and criteria", "",
        "The existing `ABS_gn_ships_2018` key selects ABS Guidance Notes on Cathodic Protection of Ships, December 2017. "
        "The key date is historical. Clause subjects: Current demand; Anode current output; Anode resistance; "
        "Minimum net anode mass; Final dimensions; Hull anode distribution and spacing. "
        "The implementation is in `src/digitalmodel/cathodic_protection/abs_ships.py` and its table, layout and result modules.", "",
        "Initial and final demand use the dynamic bare-steel density. Mean demand weights dynamic and static exposure, "
        "with the arithmetic mean of the initial and final coating fractions. Long-flush final resistance uses "
        "depleted length plus equivalent width. These are existing route interpretations, not a new assessment of the standard. "
        "Mean-current mass uses a non-leap design year. Counts round upward; the maximum mass, initial, mean, final and layout requirement governs.", "",
        "The protective and anode potentials and alloy capacity are assumed performance inputs constrained by existing route lookups. "
        "The public citation fixtures check metadata only; no standards document or private material was opened. "
        "Source qualification and applicability remain open.", "",
        "## Results", "",
        "The [results file](cathodic-protection-analysis.citations.json) retains unrounded calculation evidence. "
        "Its `arrangements`, `checks`, `zone_demand_A` and evidence keys follow the reference-jacket sidecar. "
        "No sensitivity sweep or reporting-standard assessment is supplied.", "",
        "| Arrangement | Anodes | Check | Required | Provided | Unit | Margin (%) | Result |",
        "|---|---:|---|---:|---:|---|---:|---|",
    ]
    for name, arrangement in result["arrangements"].items():
        for row in arrangement["checks"]:
            lines.append(f"| {name} | {arrangement['anode_count']} | {row['quantity']} | "
                         f"{row['required']:.1f} | {row['provided']:.1f} | {row['unit']} | "
                         f"{row['margin_percent']:+.1f} | {'PASS' if row['passes'] else 'FAIL'} |")
    lines += ["", "## Distribution and limits", "",
              "General spacing and layout flags pass on their ASSUMED declarations. Port and starboard pairing is retained during re-sizing. "
              "No coordinate schedule demonstrates those declarations. Deepest bottom and remote bow and stern positions require assessment. "
              "Special stern, propeller and opening geometry and local potentials are NOT_EVALUATED. "
              "Aggregate PASS does not establish protection at any position. Only external sacrificial-anode screening is covered.", "",
              "## Recommendations and monitoring plan", "",
              f"At design, add {rec['design_additional_anodes']} anodes "
              f"({rec['design_additional_mass_kg']:.1f} kg net alloy) for the re-sized aggregate arrangement. "
              "Confirm coating, environment, qualified anode performance and placement before acceptance. "
              "If monitoring or later retrofit cannot be supported, resolve these uncertainties in the initial design.", "",
              "In service, measure the deepest bottom and remote bow and stern positions first, then appendages and shielded areas. "
              "Establish a qualified protective criterion and reference electrode, baseline survey and survey interval. "
              "Less negative readings need assessment against that criterion. Frequency, trigger potential, retrofit quantity and "
              "intervention date are NOT_EVALUATED: operating exposure, depletion, layout, survey and access dates are missing. "
              "The design increment is not a remaining-life retrofit prediction. An engineer must establish the monitoring and intervention plan.", "",
              "## Reproduction and checks", "", "```sh",
              "PYTHONPATH=src uv run --no-project --with pydantic --with loguru --with scipy python scripts/reporting/build_synthetic_hull_analysis.py",
              "```", "",
              "The builder uses only the public input and citation-metadata fixtures. "
              "Tests independently calculate shell area, weighted demand, net anode mass, lifetime mass, "
              "initial and depleted output and the governing count. Reproduction checks compare all evidence and report bytes. "
              "This synthetic example has no marketing clearance record; outward use remains subject to the maintainers' review.", ""]
    return "\n".join(lines)


def build(destination: Path, *, citation_repo_root: Path = CITATION_ROOT) -> Path:
    """Write deterministic evidence and its case note."""
    result = calculate(citation_repo_root=citation_repo_root)
    destination.mkdir(parents=True, exist_ok=True)
    (destination / f"{NAME}.md").write_text(render_note(result), encoding="utf-8")
    output = destination / f"{NAME}.citations.json"
    output.write_text(json.dumps(result, indent=2, sort_keys=True, allow_nan=False) + "\n", encoding="utf-8")
    return output


def main() -> None:
    """Run the public synthetic case without consulting private material."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-dir", type=Path, default=DESTINATION)
    parser.add_argument("--citation-repo-root", type=Path, default=CITATION_ROOT)
    args = parser.parse_args()
    build(args.output_dir, citation_repo_root=args.citation_repo_root)


if __name__ == "__main__":
    main()
