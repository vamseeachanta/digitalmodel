#!/usr/bin/env python
"""Registry batch run: API RP 16Q required top tension across the riser
stack-up registry (#1453, epic #1279).

Iterates every ``riser_stackup`` row with ``stackup_type =
as-planned-stackup`` and ``operation = drilling`` (the public dm-side
handles), resolves each row's wiki-side ``16q-min-top-tension`` calc
contract (private llm-wiki registry + source pages, llm-wiki#826), and where
the contract carries the chain inputs computes the 16Q minimum vertical top
tension through ``digitalmodel.drilling_riser.assembly``. Rows whose source
page carries a text-extractable joint schedule + joint library instead run
the #1458 schedule-assembly path (``drilling_riser.schedule_assembly``):
string weight assembled from the wiki schedule, 16Q minimum computed at the
heaviest documented mud weight, and the reason column reports the delta
band against the wave-3 ``16q-min-tension-endpoints`` contract
(llm-wiki#828) — status ``runnable-schedule``. Remaining rows are
``missing-inputs`` with a reason (``fields_unknown`` taxonomy).

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

from digitalmodel.drilling_riser.adapter import KIPS_TO_KN
from digitalmodel.drilling_riser.assembly import minimum_top_tension_16q
from digitalmodel.drilling_riser.calc_contracts import (
    contract_referenced_rsu_ids,
    load_calc_contracts,
    resolve_wiki_root,
)
from digitalmodel.drilling_riser.schedule_assembly import (
    SCHEDULE_RSU_IDS,
    ScheduleAssemblyError,
    documented_endpoints,
    load_schedule_assembly,
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
REASON_ENDPOINTS_NO_SCHEDULE = (
    "fields_unknown: 16Q endpoint contract exists but the joint schedule is "
    "not text-extractable (raster-only counts)"
)
ENDPOINT_CALC = "16q-min-tension-endpoints"


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
    status: str  # runnable | runnable-schedule | missing-inputs
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


def _schedule_row(stackup, wiki_root) -> Optional[BatchRow]:
    """#1458 schedule-assembly path: assemble the string from the wiki joint
    schedule + library, compute the 16Q minimum at the heaviest documented
    mud weight, and report the validation delta band against the wave-3
    endpoint contract. ``None`` when this RSU has no text-extractable
    schedule (the caller falls back to missing-inputs)."""
    if stackup.rsu_id not in SCHEDULE_RSU_IDS:
        return None
    try:
        assembly = load_schedule_assembly(stackup.rsu_id, wiki_root)
        endpoints = documented_endpoints(stackup.rsu_id, wiki_root)
    except ScheduleAssemblyError:
        return None
    mud_kg_m3, documented_kn = max(endpoints)
    tmin_kn = assembly.minimum_top_tension_16q_kn(mud_kg_m3)
    deltas = [
        (assembly.minimum_top_tension_16q_kn(mud) - doc_kn) / doc_kn
        for mud, doc_kn in endpoints
    ]
    band = f"{min(deltas):+.1%}..{max(deltas):+.1%}"
    verdict = "PASS" if max(abs(d) for d in deltas) <= 0.03 else "FINDING"
    return BatchRow(
        rsu_id=stackup.rsu_id,
        water_depth_band=stackup.water_depth_band,
        topology_class=stackup.topology_class,
        string_dry_weight=_fmt(assembly.model.total_dry_weight_kn() / KIPS_TO_KN),
        # NET submerged steel weight + net lift of buoyed joints (the sources
        # document no gross-Ws/uplift split — see schedule_assembly header).
        string_submerged_weight=_fmt(
            assembly.model.total_submerged_weight_kn() / KIPS_TO_KN
        ),
        buoyancy_uplift=_fmt(assembly.buoyancy_net_lift_kn / KIPS_TO_KN),
        tmin_vert=_fmt(tmin_kn / KIPS_TO_KN),
        units="kips",
        status="runnable-schedule",
        reason=(
            f"schedule-assembly at heaviest documented mud; vs "
            f"16q-min-tension-endpoints: {verdict} (delta {band})"
        ),
    )


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
        contracts = load_calc_contracts(wiki_root)
        goldens = {c["rsu_id"]: c for c in contracts if c["calc"] == CALC}
        endpoint_ids = {c["rsu_id"] for c in contracts if c["calc"] == ENDPOINT_CALC}
        any_contract = contract_referenced_rsu_ids(wiki_root)
        rows = []
        for s in stackups:
            golden = goldens.get(s.rsu_id)
            schedule = None if golden else _schedule_row(s, wiki_root)
            if s.rsu_id in endpoint_ids:
                reason = REASON_ENDPOINTS_NO_SCHEDULE
            elif s.rsu_id in any_contract:
                reason = REASON_RESULT_DIALECT_ONLY
            else:
                reason = REASON_NO_CONTRACT
            rows.append(schedule or _assemble_row(s, golden, reason))

    output_csv.parent.mkdir(parents=True, exist_ok=True)
    with output_csv.open("w", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=list(asdict(rows[0])))
        writer.writeheader()
        writer.writerows(asdict(r) for r in rows)
    return rows


def main() -> int:
    rows = run()
    runnable = sum(1 for r in rows if r.status.startswith("runnable"))
    by_status = {
        status: sum(1 for r in rows if r.status == status)
        for status in ("runnable", "runnable-schedule", "missing-inputs")
    }
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
        f"\n{runnable}/{len(rows)} as-planned drilling RSUs runnable "
        f"({by_status['runnable']} contract-chain + "
        f"{by_status['runnable-schedule']} schedule-assembly; "
        f"{by_status['missing-inputs']} missing-inputs); "
        f"CSV: {OUTPUT_CSV.relative_to(HERE)}"
    )
    if os.environ.get("LLM_WIKI_PATH") is None and resolve_wiki_root() is None:
        print("NOTE: standalone mode — set LLM_WIKI_PATH for wiki-side values")
    return 0


if __name__ == "__main__":
    sys.exit(main())
