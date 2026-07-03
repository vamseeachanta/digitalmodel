"""#1280 in-context golden: the API RP 16Q minimum-top-tension chain vs the
RSU-0007 calc contract (machine-readable YAML block on the PRIVATE registry
page — dm carries only key names; values resolve wiki-side at test time).

Loud skip standalone. Tolerance: abs <= 0.5 t AND rel <= 0.15%, derived from
the contract inputs' quantization. This CANNOT mask a wrong formula: omitting
the fleet-angle factor alone produces ~1.48% error (10x the tolerance);
efficiency or n-fail mistakes are larger still.
"""
from __future__ import annotations

import os
import re
from pathlib import Path

import pytest
import yaml

from digitalmodel.drilling_riser.assembly import (
    minimum_top_tension_16q,
    tensioner_system_factor,
)

REGISTRY_REL = "wikis/riser-projects/wiki/datasets/stackup-registry.md"
_KNOWN_WIKI_CLONES = (
    Path("/mnt/local-analysis/llm-wiki"),  # abs-path-allowed
    Path.home() / "workspace-hub" / "llm-wiki",
)

_CONTRACT_KEYS = {
    "rsu_id",
    "calc",
    "factored_submerged_steel_t",
    "factored_buoyancy_t",
    "n_wires",
    "n_fail",
    "efficiency",
    "fleet_angle_factor",
    "min_top_tension_t",
    "connector_pull_t",
    "tension_setting_t",
}


def _calc_contract(rsu_id: str, calc: str) -> dict:
    candidates = []
    env = os.environ.get("LLM_WIKI_PATH")
    if env:
        candidates.append(Path(env))
    candidates.extend(_KNOWN_WIKI_CLONES)
    for base in candidates:
        page = base / REGISTRY_REL
        if not page.is_file():
            continue
        blocks = re.findall(r"```yaml\n(.*?)```", page.read_text(), re.DOTALL)
        for block in blocks:
            doc = yaml.safe_load(block)
            if not isinstance(doc, dict) or "calc_contracts" not in doc:
                continue
            for contract in doc["calc_contracts"]:
                if contract.get("rsu_id") == rsu_id and contract.get("calc") == calc:
                    missing = _CONTRACT_KEYS - set(contract)
                    assert not missing, (
                        f"calc contract schema drift — missing keys: {missing}"
                    )
                    return contract
        pytest.fail(
            f"registry page found but no {calc} contract for {rsu_id} — "
            "the paired llm-wiki calc-contract change is not on this clone"
        )
    pytest.skip(
        "llm-wiki registry not available (standalone mode) — the 16Q golden "
        "is part of the in-context merge gate"
    )


def test_16q_min_top_tension_golden():
    c = _calc_contract("RSU-0007", "16q-min-top-tension")
    factored_net_t = c["factored_submerged_steel_t"] + c["factored_buoyancy_t"]
    system = tensioner_system_factor(c["n_wires"], c["n_fail"], c["efficiency"])
    result_t = minimum_top_tension_16q(
        factored_net_t,
        n_units=c["n_wires"],
        n_fail=c["n_fail"],
        efficiency=c["efficiency"],
        fleet_angle_factor=c["fleet_angle_factor"],
    )
    documented_t = c["min_top_tension_t"]
    assert abs(result_t - documented_t) <= 0.5  # tonnes
    assert abs(result_t - documented_t) / documented_t <= 0.0015
    # The chain's own consistency: system factor from the contract's inputs
    # must equal the composition the engine applied.
    assert result_t == pytest.approx(factored_net_t * system * c["fleet_angle_factor"])


def test_setting_side_values_present_but_not_asserted():
    """The contract carries setting-side values as DATA (composition not yet
    reconciled against the underlying workbook — explicitly out of #1280
    scope). This test only pins their presence so the contract can't silently
    shed them before the follow-up slice."""
    c = _calc_contract("RSU-0007", "16q-min-top-tension")
    assert c["connector_pull_t"] > 0
    assert c["tension_setting_t"] > c["min_top_tension_t"]
