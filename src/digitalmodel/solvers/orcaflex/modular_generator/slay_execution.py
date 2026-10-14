# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Write solver-free S-lay canary inputs for the approval-gated campaign queue."""

from __future__ import annotations

import hashlib
import json
import math
from pathlib import Path
import re
from typing import Any


def read_contract(campaign: Path, expected_hours: float, max_hours: float) -> dict[str, Any]:
    """Require finite scheduling budgets and sourced criteria before generation.

    Args:
        campaign: Directory holding criteria and the timed-case definition.
        expected_hours: Scheduling estimate supplied for queue enrollment.
        max_hours: Scheduling ceiling; not a prediction of native run time.

    Returns:
        Validated criteria and timed-case records.

    Raises:
        ValueError: A budget, source, revision or canary definition is invalid.
    """
    if not (math.isfinite(expected_hours) and math.isfinite(max_hours)
            and 0 < expected_hours <= max_hours):
        raise ValueError("Finite positive queue budgets with expected <= maximum are required")
    raw = {name: (campaign / name).read_bytes() for name in (
        "case-table.yml", "criteria.json", "first-timed-case.json", "manifest.template.json")}
    criteria = json.loads(raw["criteria.json"])
    first = json.loads(raw["first-timed-case.json"])
    template = json.loads(raw["manifest.template.json"])
    if (criteria["revision"] != first["criteria_revision"]
            or criteria["revision"] != template["criteria_revision"]):
        raise ValueError("Criteria revisions must agree")
    required = {"source_qualification", "geometry", "force_balance", "native_convergence",
                "mesh_convergence", "catenary_relative_tolerance", "overbend_strain_limit",
                "installation_acceptance"}
    if set(criteria["criteria"]) != required:
        raise ValueError("Complete native and installation criteria are required")
    for criterion in criteria["criteria"].values():
        source = criterion.get("source", {})
        if (criterion.get("status") not in {"defined", "not established"}
                or not source.get("title") or not source.get("locator")):
            raise ValueError("Each criterion requires a status, source title and locator")
    if first["case_id"] != template["first_timed_case"] or template["launch_enabled"] is not False:
        raise ValueError("A disabled template and consistent first timed case are required")
    _validate_canary(first)
    return {"criteria": criteria, "first": first, "expected_hours": expected_hours,
            "max_hours": max_hours,
            "source_byte_sha256": {name: hashlib.sha256(content).hexdigest() for name, content in raw.items()}}


def _validate_canary(first: dict[str, Any]) -> None:
    """Keep a complete, single-threaded canary within its prepared model tree."""
    case_id = first["case_id"]
    if (not re.fullmatch(r"[a-z0-9]+(?:-[a-z0-9]+)*", case_id)
            or first["model_locator"] != f"models/{case_id}/master.yml"):
        raise ValueError("Canary model must be its prepared master")
    if first["workers"] != 1 or first["solver_threads"] != 1 or first["analysis_type"] != "both":
        raise ValueError("Timed canary requires one worker and thread, statics and dynamics")
    required = {"native_version", "static_convergence", "endpoint_positions", "endpoint_forces",
                "effective_tension", "bending_moment", "curvature", "steel_bending_strain",
                "roller_reaction", "touchdown_position", "exit_angle", "force_balance"}
    if not required <= first["readback"].keys() or not all(first["readback"].values()):
        raise ValueError("Complete canary readback quantities are required")


def _digest(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def _save(path: Path, data: dict[str, Any]) -> None:
    with path.open("x", encoding="utf-8") as stream:
        stream.write(json.dumps(data, indent=2, allow_nan=False) + "\n")
    if json.loads(path.read_text(encoding="utf-8")) != data:
        raise ValueError("Saved run manifest readback mismatch")


def write_queue(campaign: Path, output: Path, repo: Path, revision: str,
                contract: dict[str, Any]) -> dict[str, Any]:
    """Write a single-case batch input, queue manifest and separate evidence record.

    The external runner accepts only its own fixed field set. Approval metadata,
    criteria and input byte digests therefore belong in the separate run record.
    This function never imports or calls the execution workflow.

    Args:
        campaign: Source campaign directory.
        output: Newly prepared model directory.
        repo: Checkout to be pinned by the queue runner.
        revision: Full checkout commit id.
        contract: Validated criteria, timed case and scheduling budgets.

    Returns:
        Read-back run record with the approval digest and retained input hashes.

    Raises:
        ValueError: The canary or scheduling revision is inconsistent.
    """
    if any(_digest(campaign / name) != digest
           for name, digest in contract["source_byte_sha256"].items()):
        raise ValueError("Campaign source changed during preparation")
    first = contract["first"]
    prepared = json.loads((output / "preparation.json").read_text(encoding="utf-8"))
    if first["case_id"] not in {row["id"] for row in prepared["cases"]}:
        raise ValueError("First timed case is absent from prepared cases")
    if (len(revision) != 40 or any(c not in "0123456789abcdef" for c in revision)
            or prepared["criteria_revision"] != contract["criteria"]["revision"]):
        raise ValueError("Full commit and matching preparation criteria revision are required")
    output, repo = output.resolve(), repo.resolve()
    queue_dir = output / "queue"
    queue_dir.mkdir(exist_ok=False)
    case_id = first["case_id"]
    batch = {"basename": "orcaflex_run_batch", "orcaflex_run_batch": {
        "models": {"files": [str(output / first["model_locator"])]},
        "analysis": {"type": first["analysis_type"]},
        "run_batch": {"workers": first["workers"], "solver_threads": first["solver_threads"],
                      "mock": False, "save_sim": True,
                      "work_dir": str(output / "native/work"),
                      "output_dir": str(output / "native/results")}},
        "default": {"log_level": "INFO"}}
    batch_path = output / "native-input.json"
    _save(batch_path, batch)
    queue = {"id": f"slay-{case_id}", "title": "Generic S-lay timed canary",
             "repo_dir": str(repo), "ref": revision,
             "driver": ["python", "-m", "digitalmodel", str(batch_path)],
             "cwd": str(repo), "env": {"PYTHONPATH": str(repo / "src")},
             "expected_hours": contract["expected_hours"], "max_hours": contract["max_hours"],
             "progress": {"glob": str(output / "native/readback/*.qualified.json"), "total": 1},
             "licence_exit_codes": [], "retry": {"max_attempts": 1, "backoff_minutes": 0},
             "priority": 40, "after": []}
    queue_path = queue_dir / f"{queue['id']}.json"
    _save(queue_path, queue)
    inputs = {str(path.relative_to(output)): _digest(path)
              for path in sorted((output / "models").rglob("*.yml"))}
    inputs[batch_path.name] = _digest(batch_path)
    record = {"schema": "slay-run-manifest-1", "launch_enabled": False,
              "input_revision": revision, "criteria_revision": contract["criteria"]["revision"],
              "criteria": contract["criteria"]["criteria"], "first_timed_case": first,
              "native_attempted": 0, "native_completed": 0, "qualified_results": 0,
              "approval_required": {"record": "approvals.tsv",
                  "campaign_id": queue["id"], "manifest_sha256": _digest(queue_path),
                  "requirement": "Digest-matched approval reference and date before external queue execution; no approval is created by preparation."},
              "input_byte_sha256": inputs,
              "source_byte_sha256": contract["source_byte_sha256"],
              "remaining_cases_status": "held pending canary readback and criteria",
              "completion": "The native/readback qualified marker is written only after saved-state readback against the criteria; this adapter never creates it.",
              "queue_policy": "One attempt, no automatic retry, one native worker and one thread. Queue completion remains held until qualified readback. Priority follows the prepared drilling-riser campaign pattern."}
    _save(output / "run-manifest.json", record)
    return record
