"""Pure, deterministic calculations for the installation-vessel pamphlet.

Every function here is a composable "workflow" step. They are deterministic:
no ``datetime.now``, no randomness, no network. The only file IO permitted is
through the explicit ``load_*`` helpers, which take an explicit path argument.

Physics / numbers are NOT invented here — they are produced by validated
digitalmodel engines (``vessel_suitability``, ``operation_envelope``,
``weather_window``, parametric RAO database). This module only orchestrates and
shapes those engine outputs into the pamphlet data structure.
"""

from __future__ import annotations

import json
import math
from pathlib import Path
from typing import Any

# ----------------------------------------------------------------------------
# RAO basis vocabulary (lever upgraded as licensed runs complete)
#   barge      -> initial pessimistic 100 m tender-barge proxy
#   drillship  -> ship-shaped proxy, justified by a completed ship diffraction
#   fpso       -> alternate ship-shaped proxy
#   vessel     -> vessel-specific OrcaWave diffraction (actual)
# ----------------------------------------------------------------------------
BASIS_MAP = {
    "barge": "Barge",
    "drillship": "Drillship",
    "fpso": "FPSO",
    "vessel": "Drillship",  # placeholder until a vessel-specific mesh exists
}
BASIS_PROVENANCE = {
    "barge": "proxy",
    "drillship": "ship_proxy",
    "fpso": "ship_proxy",
    "vessel": "actual",
}
BASIS_LABEL = {
    "barge": "Generic 100 m barge parametric RAO (initial proxy — pessimistic)",
    "drillship": (
        "Ship-shaped (drillship) parametric RAO — adapted basis, justified by "
        "completed FPSO diffraction run lr_acma_8de9c018adbd"
    ),
    "fpso": "Ship-shaped (FPSO 250 m) parametric RAO — adapted basis",
    "vessel": "Vessel-specific BokaLift 2 RAO (OrcaWave diffraction)",
}
BASIS_HUMAN = {
    "barge": "100 m barge proxy (pessimistic)",
    "drillship": "ship-shaped drillship proxy (justified by completed FPSO diffraction)",
    "fpso": "ship-shaped FPSO proxy",
    "vessel": "vessel-specific OrcaWave RAO",
}

DEFAULT_OPERATIONS = ["transit", "dp_station_keeping", "heavy_lift"]
DEFAULT_TP_GRID_S = [6.0, 8.0, 10.0, 12.0, 14.0, 16.0]


# ---------------------------------------------------------------- loaders
def load_mudmats(path: str | Path) -> dict[str, dict[str, Any]]:
    """Load the mudmat/structure catalog keyed by structure id."""
    with Path(path).open("r", encoding="utf-8") as stream:
        data = json.load(stream)
    return {m["id"]: m for m in data["structures"]}


def load_jumpers(path: str | Path) -> dict[str, dict[str, Any]]:
    """Load the rigid-jumper catalog keyed by jumper id."""
    with Path(path).open("r", encoding="utf-8") as stream:
        data = json.load(stream)
    return {j["id"]: j for j in data["jumpers"]}


# ---------------------------------------------------------------- lift suitability
def assess_lifts(
    vessel_info: dict[str, Any],
    items: list[dict[str, Any]],
    radius_m: float,
    daf: float,
) -> list[dict[str, Any]]:
    """Crane lift suitability per item via the vessel_suitability engine.

    ``vessel_info`` must carry ``"name"`` (the vessel display name used by the
    engine lookup). ``items`` is a list of ``{"item","kind","mass_air_te"}``.
    Returns one row dict per lift item. Deterministic.
    """
    from digitalmodel.marine_ops.installation import vessel_suitability as vs

    vessel_name = vessel_info["name"]
    rows: list[dict[str, Any]] = []
    for it in items:
        te = float(it["mass_air_te"])
        req = vs.LiftRequirement(weight_te=te, radius_m=float(radius_m), daf=float(daf))
        res = vs.assess_named(vessel_name, req)
        swl = getattr(res, "swl_at_radius_te", None)
        rows.append(
            {
                "item": it["item"],
                "kind": it.get("kind"),
                "mass_air_te": te,
                "hook_load_te": round(te * float(daf), 1),
                "swl_at_radius_te": swl,
                "utilization_pct": round(100 * te * float(daf) / swl, 1) if swl else None,
                "defensible": getattr(res, "defensible", None),
                "score": getattr(res, "score", None),
                "limiting": list(getattr(res, "limiting_factors", []) or []),
            }
        )
    return rows


# ---------------------------------------------------------------- envelopes
def _build_rao_set(rao_basis: str):
    """Construct an RAOSet from the parametric vessel database for ``rao_basis``.

    Ported verbatim from the validated prototype ``compute_pamphlet.py``.
    """
    import numpy as np

    from digitalmodel.orcawave.vessel_database import get_representative_raos
    from digitalmodel.hydrodynamics.diffraction.output_schemas import (
        DOF,
        FrequencyData,
        HeadingData,
        RAOComponent,
        RAOSet,
    )

    vessel_type = BASIS_MAP[rao_basis]
    rao_param = get_representative_raos(vessel_type)
    periods = np.array(rao_param.periods, dtype=float)
    freqs = 2 * math.pi / periods  # rad/s
    heads = np.array(rao_param.headings_deg, dtype=float)
    # engine wants [n_freq x n_head]; parametric stores [n_head x n_freq] -> transpose
    heave = np.array(rao_param.heave_rao).T
    roll = np.array(rao_param.roll_rao).T
    pitch = np.array(rao_param.pitch_rao).T
    zero = np.zeros_like(heave)

    def comp(dof, mag, unit):
        return RAOComponent(
            dof=dof,
            magnitude=mag,
            phase=np.zeros_like(mag),
            frequencies=FrequencyData(
                values=freqs,
                periods=periods,
                count=len(freqs),
                min_freq=float(freqs.min()),
                max_freq=float(freqs.max()),
            ),
            headings=HeadingData(
                values=heads,
                count=len(heads),
                min_heading=float(heads.min()),
                max_heading=float(heads.max()),
            ),
            unit=unit,
        )

    return RAOSet(
        vessel_name=f"Installation vessel ({vessel_type} proxy RAO)",
        analysis_tool="parametric",
        water_depth=1200.0,
        surge=comp(DOF.SURGE, zero.copy(), "m/m"),
        sway=comp(DOF.SWAY, zero.copy(), "m/m"),
        heave=comp(DOF.HEAVE, heave, "m/m"),
        roll=comp(DOF.ROLL, roll, "deg/m"),
        pitch=comp(DOF.PITCH, pitch, "deg/m"),
        yaw=comp(DOF.YAW, zero.copy(), "deg/m"),
        created_date="2026-06-28",
    )


def compute_envelopes(
    rao_basis: str,
    operations: list[str],
    tp_grid_s: list[float],
) -> dict[str, Any]:
    """Operation envelopes (limiting Hs vs Tp) for each operation.

    Returns an ordered dict keyed by operation name, each value being
    ``{"alpha", "hs_by_tp", "gov_by_tp"}``. ``hs_by_tp`` / ``gov_by_tp`` keys are
    one-decimal Tp strings (e.g. ``"6.0"``) so JSON round-trips stably.
    """
    from digitalmodel.marine_ops.operation_envelope import operation_envelope

    rset = _build_rao_set(rao_basis)
    tp_grid = [float(t) for t in tp_grid_s]
    env_out: dict[str, Any] = {}
    for op in operations:
        env = operation_envelope(rset, op, tp_range_s=tp_grid)
        hs_by_tp = {f"{round(p.tp_s, 1):.1f}": round(p.hs_limit_m, 2) for p in env.points}
        gov_by_tp = {f"{round(p.tp_s, 1):.1f}": p.governing_dof for p in env.points}
        env_out[op] = {
            "alpha": env.alpha,
            "hs_by_tp": hs_by_tp,
            "gov_by_tp": gov_by_tp,
        }
    return env_out


# ---------------------------------------------------------------- operability
def compute_operability(hs_scatter_limits: list[float]) -> list[dict[str, Any]]:
    """Weather-window operability table (% time below each Hs limit)."""
    from digitalmodel.orcaflex.weather_window import ScatterDiagram, OperabilityTable

    limits = [float(h) for h in hs_scatter_limits]
    return OperabilityTable(hs_limits=limits, scatter=ScatterDiagram()).generate_table()


# ---------------------------------------------------------------- RAO basis selection
def _is_diffraction(run: dict[str, Any]) -> bool:
    wf = (run.get("workflow") or "").lower()
    inp = (run.get("input") or "").lower()
    return (
        "diffraction" in wf
        or "diffraction" in inp
        or "orcawave" in wf
        or "orcawave" in inp
    )


def select_rao_basis(completed_runs: list[dict[str, Any]]) -> str:
    """Pure RAO-basis selection from a list of completed-run dicts.

    Rules (ported from the prototype adapt.py):
      * any run input contains "bokalift"                       -> "vessel"
      * a diffraction/orcawave run input contains "fpso"/"ship" -> "drillship"
      * any diffraction/orcawave run exists                     -> "drillship"
      * otherwise                                               -> "barge"
    """
    runs = list(completed_runs or [])
    diffraction = [r for r in runs if _is_diffraction(r)]
    if any("bokalift" in (r.get("input") or "").lower() for r in runs):
        return "vessel"
    if any(
        ("fpso" in (r.get("input") or "").lower() or "ship" in (r.get("input") or "").lower())
        for r in diffraction
    ):
        return "drillship"
    if diffraction:
        return "drillship"
    return "barge"


def select_basis_run_id(
    completed_runs: list[dict[str, Any]],
    rao_basis: str,
) -> str | None:
    """Run id that justifies the selected basis (None for the barge default)."""
    runs = list(completed_runs or [])
    diffraction = [r for r in runs if _is_diffraction(r)]
    if rao_basis == "vessel":
        for r in runs:
            if "bokalift" in (r.get("input") or "").lower():
                return r.get("run_id")
        return None
    if rao_basis in ("drillship", "fpso"):
        for r in diffraction:
            inp = (r.get("input") or "").lower()
            if "fpso" in inp or "ship" in inp:
                return r.get("run_id")
        if diffraction:
            return diffraction[0].get("run_id")
    return None


# ---------------------------------------------------------------- provenance manifest
def build_provenance(
    completed_runs: list[dict[str, Any]],
    rao_basis: str,
    vessel_name: str = "BokaLift 2",
) -> dict[str, Any]:
    """Run-state provenance manifest (T1..T8). Pure.

    Returns the full run-state object (vessel, basis, provenance, basis run id,
    completed-run count, and the ordered task list) used to render the analysis
    progress panel and the per-section provenance badges.
    """
    runs = list(completed_runs or [])
    prov = BASIS_PROVENANCE[rao_basis]
    basis_run_id = select_basis_run_id(runs, rao_basis)
    basis_note = BASIS_HUMAN[rao_basis]
    rao_source = "OrcaWave diffraction" if prov == "actual" else "parametric ship RAO"

    def task(tid, section, title, status, basis, source, run_id=None):
        return {
            "id": tid,
            "section": section,
            "title": title,
            "status": status,
            "basis": basis,
            "source": source,
            "run_id": run_id,
        }

    tasks = [
        task(
            "T1", 1, "Vessel particulars & crane curve", "actual",
            "Cited vessel database", "vessel_db.loader (curated + worldenergydata)",
        ),
        task(
            "T2", 2, "Crane lift suitability (6 items)", "actual",
            "Interpolated crane curve + DNV DAF", "vessel_suitability.assess_named",
        ),
        task(
            "T3", 3, "Vessel motion RAO", prov,
            basis_note, rao_source, basis_run_id,
        ),
        task(
            "T4", 3, "Operation envelopes (Hs/Tp)", prov,
            "operation_envelope on T3 RAO", "operation_envelope", basis_run_id,
        ),
        task(
            "T5", 3, "Per-structure splash-zone envelope", "pending",
            "Needs OrcaFlex installation run per structure",
            "OrcaFlex (licensed) — not yet queued",
        ),
        task(
            "T6", 5, "Weather-window operability", prov,
            "weather_window engine on T4 limits", "weather_window.OperabilityTable",
        ),
        task(
            "T7", 7, "DP / mooring statistical risk", "actual",
            "IMCA + HSE RR444 + DNV statistics", "industry datasets",
        ),
        task(
            "T8", 4, "Manifold / PLET structure catalog", "proxy",
            "Proxy entry — real catalog pending", "examples/demos/gtm (mudmat only)",
        ),
    ]
    return {
        "vessel": vessel_name,
        "rao_basis": rao_basis,
        "rao_provenance": prov,
        "basis_run_id": basis_run_id,
        "n_completed_runs": len(runs),
        "tasks": tasks,
    }


# ---------------------------------------------------------------- assembly
def assemble_result(
    vessel_info: dict[str, Any],
    lifts: list[dict[str, Any]],
    radius_m: float,
    daf: float,
    rao_basis: str,
    operations: list[str],
    tp_grid_s: list[float],
    hs_scatter_limits: list[float],
    completed_runs: list[dict[str, Any]],
) -> dict[str, Any]:
    """Compose the full deterministic pamphlet ``result`` payload (pure)."""
    lift_rows = assess_lifts(vessel_info, lifts, radius_m, daf)
    envelopes = compute_envelopes(rao_basis, operations, tp_grid_s)
    op_table = compute_operability(hs_scatter_limits)
    provenance = build_provenance(
        completed_runs, rao_basis, vessel_name=vessel_info.get("name", "Installation vessel")
    )
    return {
        "vessel": vessel_info,
        "lift_radius_m": float(radius_m),
        "daf": float(daf),
        "lifts": lift_rows,
        "envelopes": envelopes,
        "tp_grid_s": [float(t) for t in tp_grid_s],
        "rao_basis": rao_basis,
        "rao_provenance": BASIS_PROVENANCE[rao_basis],
        "envelope_proxy": BASIS_LABEL[rao_basis],
        "op_table": op_table,
        "provenance": provenance,
    }
