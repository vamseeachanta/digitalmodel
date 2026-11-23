"""A partial phase report must never satisfy the fixed portfolio contract."""
from __future__ import annotations

import copy
import importlib
import sys
from pathlib import Path
from typing import Any

import pytest
import yaml  # type: ignore[import-untyped]

from digitalmodel.cathodic_protection.engine_adapter import run_cathodic_protection

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / "scripts/reporting"))


@pytest.fixture(scope="module")
def cases() -> dict[str, Any]:
    folder = ROOT / "tests/fixtures/cathodic_protection/workflow_inputs"
    return {
        name: run_cathodic_protection(yaml.safe_load((folder / filename).read_text("utf-8")))
        for name, filename in [("S09", "riser_base_phased.yml"),
                               ("S10", "temporary_service.yml")]
    }


def _validate(case: str, cfg: dict[str, Any]) -> None:
    module = importlib.import_module("cp_portfolio_coverage")
    module.validate_mode_coverage(case, cfg)


@pytest.mark.parametrize("case", ["S09", "S10"])
def test_complete_mode_has_required_children(case: str, cases: dict[str, Any]) -> None:
    _validate(case, cases[case])


@pytest.mark.parametrize("pointer", [
    "inputs/riser_base_assessment/components/3",
    "inputs/anode_families/0",
    "inputs/riser_base_assessment/phases/1",
    "inputs/riser_base_assessment/phases/0/families/0",
    "inputs/riser_base_assessment/retrofit/proposed_anode",
    "results/riser_base_assessment/components/hatch",
    "results/current_demand_A/foundation::reinforcement",
    "results/anode_families/concrete",
    "results/riser_base_assessment/phases/1",
    "results/riser_base_assessment/phases/0/families/sea",
    "results/riser_base_assessment/phases/1/families/sea/consumed_mass_kg",
    "results/riser_base_assessment/phases/1/families/sea/output_checks/final_output",
    "results/riser_base_assessment/retrofit",
    "results/riser_base_assessment/retrofit/usable_remaining_mass_kg",
    "results/riser_base_assessment/retrofit/combined_output_checks/initial",
    "results/riser_base_assessment/retrofit/combined_output_checks/final/result",
])
def test_missing_expected_child_fails(pointer: str, cases: dict[str, Any]) -> None:
    cfg = copy.deepcopy(cases["S09"])
    parts = pointer.split("/")
    node = cfg
    for part in parts[:-1]:
        node = node[int(part)] if isinstance(node, list) else node[part]
    del node[int(parts[-1]) if isinstance(node, list) else parts[-1]]
    with pytest.raises(ValueError, match="coverage"):
        _validate("S09", cfg)


@pytest.mark.parametrize("side", ["inputs", "results"])
def test_temporary_phase_cannot_be_replaced(side: str, cases: dict[str, Any]) -> None:
    cfg = copy.deepcopy(cases["S10"])
    cfg[side]["riser_base_assessment"]["phases"][0]["type"] = "operating"
    with pytest.raises(ValueError, match="coverage"):
        _validate("S10", cfg)


def test_unrelated_case_is_outside_mode_contract() -> None:
    _validate("S01", {})
