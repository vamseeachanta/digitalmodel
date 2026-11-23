"""Frozen S09/S10 coverage requirements, independent of emitted children.

These checks establish evidence completeness, not numerical correctness or EOR
acceptance. S09's phase ledger intentionally covers sea only; the other three
families have component-local calculations, not invented phase assessments.
"""
from __future__ import annotations

import math
from typing import Any

COMPONENTS = (
    ("base", "riser_base", "frame", "sea"),
    ("foundation", "foundation", "reinforcement", "concrete"),
    ("mudmat", "mudmat", "underside", "mud"),
    ("hatch", "hatch_cover", "cover", "hatch"),
)
PHASES = {"S09": (("storage", "wet_storage", 2.0),
                   ("operation", "operating", 25.0)),
          "S10": (("installation", "temporary", 0.5),)}
ASSESSMENT = "riser_base_assessment"
MASS_FIELDS = (
    "consumed_mass_kg", "physical_mass_start_kg", "physical_mass_end_kg",
    "usable_mass_start_kg", "usable_mass_end_kg", "mass_shortfall_kg",
    "additional_gross_mass_kg", "additional_anode_count",
)
RETROFIT_MASS = (
    "consumed_mass_kg", "physical_remaining_mass_kg", "usable_remaining_mass_kg",
    "future_usable_mass_required_kg", "mass_shortfall_kg", "additional_gross_mass_kg",
    "recommended_additional_count", "count_by_mass", "count_by_initial_output",
    "count_by_final_output", "remaining_anode_count",
)


def _at(cfg: dict[str, Any], path: str) -> Any:
    node: Any = cfg
    try:
        for part in path.split("/"):
            node = node[int(part)] if isinstance(node, list) else node[part]
    except (KeyError, IndexError, TypeError, ValueError) as exc:
        raise ValueError(f"mode coverage missing {path}") from exc
    if node is None or node == "" or node == {} or node == []:
        raise ValueError(f"mode coverage empty {path}")
    return node


def _equal(cfg: dict[str, Any], path: str, expected: Any) -> None:
    if _at(cfg, path) != expected:
        raise ValueError(f"mode coverage mismatch {path}")


def _numbers(cfg: dict[str, Any], path: str, fields: tuple[str, ...]) -> None:
    for field in fields:
        value = _at(cfg, f"{path}/{field}")
        if isinstance(value, bool) or not isinstance(value, (float, int)):
            raise ValueError(f"mode coverage requires numeric {path}/{field}")
        if not math.isfinite(value):
            raise ValueError(f"mode coverage requires finite {path}/{field}")


def _components(case: str, cfg: dict[str, Any]) -> None:
    expected = COMPONENTS if case == "S09" else COMPONENTS[:1]
    inputs = f"inputs/{ASSESSMENT}/components"
    results = f"results/{ASSESSMENT}/components"
    if len(_at(cfg, inputs)) != len(expected):
        raise ValueError("mode coverage component count mismatch")
    if set(_at(cfg, results)) != {row[0] for row in expected}:
        raise ValueError("mode coverage result component set mismatch")
    for index, (name, kind, zone, family) in enumerate(expected):
        source, target = f"{inputs}/{index}", f"{results}/{name}"
        for base in (source, target):
            _equal(cfg, f"{base}/type", kind)
        _equal(cfg, f"{source}/name", name)
        _equal(cfg, f"{source}/zones/0/zone", zone)
        _equal(cfg, f"{source}/zones/0/anode_family", family)
        _equal(cfg, f"{target}/zones", [f"{name}::{zone}"])
        _equal(cfg, f"{target}/anode_families", [family])
        _numbers(cfg, f"{target}/current_demand_A", ("initial", "mean", "final"))
        _numbers(cfg, f"results/current_demand_A/{name}::{zone}",
                 ("I_initial_A", "I_mean_A", "I_final_A"))
        _numbers(cfg, f"results/anode_families/{family}/current_demand_A",
                 ("initial", "mean", "final"))


def _phase_family(cfg: dict[str, Any], source: str, target: str) -> None:
    _equal(cfg, f"{source}/families/0/family", "sea")
    if len(_at(cfg, f"{source}/families")) != 1:
        raise ValueError("mode coverage input phase family set mismatch")
    if set(_at(cfg, f"{target}/families")) != {"sea"}:
        raise ValueError("mode coverage result phase family set mismatch")
    demand, row = f"{source}/families/0", f"{target}/families/sea"
    _numbers(cfg, demand, ("mean_current_A", "initial_current_A", "final_current_A"))
    _numbers(cfg, row, MASS_FIELDS + ("mean_current_A",))
    _equal(cfg, f"{row}/mean_current_A", _at(cfg, f"{demand}/mean_current_A"))
    for phase in ("initial", "final"):
        check = f"{row}/output_checks/{phase}_output"
        _numbers(cfg, check, ("demand_A", "output_A", "ratio"))
        _equal(cfg, f"{check}/demand_A", _at(cfg, f"{demand}/{phase}_current_A"))
        if _at(cfg, f"{check}/result") not in ("PASS", "FAIL"):
            raise ValueError("mode coverage invalid phase output disposition")


def _phases(case: str, cfg: dict[str, Any]) -> None:
    expected = PHASES[case]
    for side in ("inputs", "results"):
        base = f"{side}/{ASSESSMENT}/phases"
        if len(_at(cfg, base)) != len(expected):
            raise ValueError("mode coverage phase count mismatch")
        for index, (name, kind, life) in enumerate(expected):
            for key, value in (("name", name), ("type", kind), ("life_years", life)):
                _equal(cfg, f"{base}/{index}/{key}", value)
    for index in range(len(expected)):
        _phase_family(cfg, f"inputs/{ASSESSMENT}/phases/{index}",
                      f"results/{ASSESSMENT}/phases/{index}")


def _families(case: str, cfg: dict[str, Any]) -> None:
    names = ("sea", "mud", "hatch", "concrete") if case == "S09" else ("sea",)
    rows = _at(cfg, "inputs/anode_families")
    if not isinstance(rows, list) or len(rows) != len(names):
        raise ValueError("mode coverage installed family count mismatch")
    if set(_at(cfg, "results/anode_families")) != set(names):
        raise ValueError("mode coverage result family set mismatch")
    for index, name in enumerate(names):
        path = f"inputs/anode_families/{index}"
        _equal(cfg, f"{path}/name", name)
        _numbers(cfg, path, ("count", "individual_anode_mass_kg", "utilization_factor"))


def _retrofit(cfg: dict[str, Any]) -> None:
    source, target = f"inputs/{ASSESSMENT}/retrofit", f"results/{ASSESSMENT}/retrofit"
    for base in (source, target):
        _equal(cfg, f"{base}/family", "sea")
        _equal(cfg, f"{base}/remaining_mass_basis", "inferred_from_age_and_demand")
    _numbers(cfg, source, ("age_years", "historical_mean_current_A", "future_life_years",
                           "future_mean_current_A", "future_initial_current_A",
                           "future_final_current_A"))
    _numbers(cfg, f"{source}/proposed_anode", ("individual_mass_kg", "utilization_factor",
                                             "current_output_initial_A",
                                             "current_output_final_A"))
    _numbers(cfg, target, RETROFIT_MASS)
    _at(cfg, f"{target}/remaining_mass_provenance")
    for phase in ("initial", "final"):
        check = f"{target}/combined_output_checks/{phase}"
        _numbers(cfg, check, ("demand_A", "existing_output_A", "proposed_output_A",
                              "total_output_A"))
        _equal(cfg, f"{check}/demand_A", _at(cfg, f"{source}/future_{phase}_current_A"))
        if _at(cfg, f"{check}/result") not in ("PASS", "FAIL"):
            raise ValueError("mode coverage invalid retrofit output disposition")


def validate_mode_coverage(case_id: str, cfg: dict[str, Any]) -> None:
    """Reject missing fixed children even when dynamic output enumeration passes."""
    if case_id not in PHASES:
        return
    _families(case_id, cfg)
    _components(case_id, cfg)
    _phases(case_id, cfg)
    if case_id == "S09":
        _retrofit(cfg)
