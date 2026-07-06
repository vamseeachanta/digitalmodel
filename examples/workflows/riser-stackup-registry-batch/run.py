#!/usr/bin/env python
"""Registry batch run: API RP 16Q required top tension across the riser
stack-up registry (#1453, epic #1279).

Iterates every ``riser_stackup`` row with ``stackup_type =
as-planned-stackup`` and ``operation = drilling`` (the public dm-side
handles), resolves each row's wiki-side ``16q-min-top-tension`` calc
contract (private llm-wiki registry + source pages, llm-wiki#826), and where
the contract carries the chain inputs computes the 16Q minimum vertical top
tension through ``digitalmodel.drilling_riser.assembly``. Rows without a
golden contract are reported ``missing-inputs`` with a reason
(``fields_unknown`` taxonomy).

Run (in-context; the wiki clone is required for values)::

    LLM_WIKI_PATH=/path/to/llm-wiki \
        uv run python examples/workflows/riser-stackup-registry-batch/run.py

Writes ``results/registry_batch_16q.csv`` next to this script and prints the
table. The results directory is GITIGNORED by design: computed tensions and
string weights are wiki-side VALUES (dm carries key names + engine only —
the #1280 calc-contract discipline), so the output must not be committed to
this public repo.
"""
from __future__ import annotations

import csv
import os
import sys
from dataclasses import asdict, dataclass
from pathlib import Path
from typing import Optional

from digitalmodel.drilling_riser.assembly import minimum_top_tension_16q
from digitalmodel.drilling_riser.calc_contracts import (
    contract_referenced_rsu_ids,
    load_calc_contracts,
    resolve_wiki_root,
)
from digitalmodel.riser_database.loader import RiserDatabase

HERE = Path(__file__).resolve().parent
OUTPUT_CSV = HERE / "results" / "registry_batch_16q.csv"

CALC = "16q-min-top-tension"
#: fields_unknown taxonomy (issue #1453 scope 3).
REASON_WIKI_UNAVAILABLE = "llm-wiki clone unavailable (values live wiki-side)"
REASON_RESULT_DIALECT_ONLY = (
    "fields_unknown: wiki carries result-dialect contracts only "
    "(no golden 16Q chain yet)"
)
REASON_NO_CONTRACT = "fields_unknown: no calc contract wiki-side"
REASON_NO_TSRMIN = "fields_unknown: 16Q contract lacks a TSRmin / factored-weight input"


@dataclass(frozen=True)
class BatchRow:
    rsu_id: str
    water_depth_band: str
    topology_class: str
    string_dry_weight: str
    string_submerged_weight: str
    buoyancy_uplift: str
    tmin_vert: str
    units: str
    status: str  # runnable | missing-inputs
    reason: str


def _fmt(value: Optional[float]) -> str:
    return "" if value is None else f"{value:.2f}"


def _chain_inputs(contract: dict) -> Optional[tuple[float, str]]:
    """(TSRmin, unit) from whichever golden-dialect keys the contract has."""
    if "min_slip_ring_tension_kips" in contract:
        return float(contract["min_slip_ring_tension_kips"]), "kips"
    if "tsr_min_kips" in contract:
        return float(contract["tsr_min_kips"]), "kips"
    if "factored_submerged_steel_t" in contract and "factored_buoyancy_t" in contract:
        return (
            float(contract["factored_submerged_steel_t"])
            + float(contract["factored_buoyancy_t"]),
            "t",
        )
    return None


def _assemble_row(
    stackup, contract: Optional[dict], reason_no_contract: str
) -> BatchRow:
    base = dict(
        rsu_id=stackup.rsu_id,
        water_depth_band=stackup.water_depth_band,
        topology_class=stackup.topology_class,
        string_dry_weight="",
        string_submerged_weight="",
        buoyancy_uplift="",
        tmin_vert="",
        units="",
    )
    if contract is None:
        return BatchRow(**base, status="missing-inputs", reason=reason_no_contract)
    chain = _chain_inputs(contract)
    if chain is None:
        return BatchRow(**base, status="missing-inputs", reason=REASON_NO_TSRMIN)
    t_srmin, unit = chain
    n_units = contract.get("n_wires", contract.get("n_tensioners"))
    tmin = minimum_top_tension_16q(
        t_srmin,
        n_units=n_units,
        n_fail=contract["n_fail"],
        # These workbook chains fold no reduction factor unless the contract
        # says otherwise (RSU-0007 carries explicit efficiency/fleet-angle).
        efficiency=contract.get("efficiency", 1.0),
        fleet_angle_factor=contract.get("fleet_angle_factor", 1.0),
    )
    base.update(
        # Weight decomposition only where the contract carries it (RSU-0023
        # style ws/bn lines); dry string weight is not in any 16Q contract yet.
        string_submerged_weight=_fmt(contract.get("ws_submerged_steel_kips")),
        buoyancy_uplift=_fmt(contract.get("bn_buoyancy_kips")),
        tmin_vert=_fmt(tmin),
        units=unit,
    )
    return BatchRow(**base, status="runnable", reason="")


def run(output_csv: Path = OUTPUT_CSV) -> list[BatchRow]:
    stackups = [
        row
        for row in RiserDatabase.load().stackups
        if row.stackup_type == "as-planned-stackup" and row.operation == "drilling"
    ]
    wiki_root = resolve_wiki_root()
    if wiki_root is None:
        rows = [_assemble_row(s, None, REASON_WIKI_UNAVAILABLE) for s in stackups]
    else:
        goldens = {
            c["rsu_id"]: c for c in load_calc_contracts(wiki_root) if c["calc"] == CALC
        }
        any_contract = contract_referenced_rsu_ids(wiki_root)
        rows = [
            _assemble_row(
                s,
                goldens.get(s.rsu_id),
                (
                    REASON_RESULT_DIALECT_ONLY
                    if s.rsu_id in any_contract
                    else REASON_NO_CONTRACT
                ),
            )
            for s in stackups
        ]

    output_csv.parent.mkdir(parents=True, exist_ok=True)
    with output_csv.open("w", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=list(asdict(rows[0])))
        writer.writeheader()
        writer.writerows(asdict(r) for r in rows)
    return rows


def main() -> int:
    rows = run()
    runnable = sum(1 for r in rows if r.status == "runnable")
    header = (
        "rsu_id",
        "band",
        "topology",
        "Ws_sub",
        "Bn",
        "Tmin_vert",
        "units",
        "status",
        "reason",
    )
    print(" | ".join(header))
    for r in rows:
        print(
            " | ".join(
                (
                    r.rsu_id,
                    r.water_depth_band,
                    r.topology_class,
                    r.string_submerged_weight,
                    r.buoyancy_uplift,
                    r.tmin_vert,
                    r.units,
                    r.status,
                    r.reason,
                )
            )
        )
    print(
        f"\n{runnable}/{len(rows)} as-planned drilling RSUs runnable; "
        f"CSV: {OUTPUT_CSV.relative_to(HERE)}"
    )
    if os.environ.get("LLM_WIKI_PATH") is None and resolve_wiki_root() is None:
        print("NOTE: standalone mode — set LLM_WIKI_PATH for wiki-side values")
    return 0


if __name__ == "__main__":
    sys.exit(main())
