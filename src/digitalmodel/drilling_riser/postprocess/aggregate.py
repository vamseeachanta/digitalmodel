"""Aggregator: campaign results by case, each verified against the SHA-256 in its run ledger.

A run directory holds ``run.json`` (the case list: ``case_id``, ``matrix_case``, ``params``), ``ledger.jsonl``
(one record per attempt: ``case_id``, ``status``, ``finished_utc``, ``results_path``, ``results_sha256``) and
``results/<case>.json``. Several runs may hold the same case (re-runs); the record with the latest
``finished_utc`` is the case's final record. A case whose final record is not ``ok`` has no result, and a case
declared in a ``run.json`` with no ledger record in any run (never attempted: the campaign was interrupted before
it, or its ledger is absent) is reported with status ``missing`` - it is never dropped silently.
"""

from __future__ import annotations

import hashlib
import json
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any, Iterable

KEY_FIELDS = ("mode", "configuration", "metocean", "heading_deg", "offset_pct_wd", "mud_weight_ppg", "top_tension",
              "tensioner")


class DigestMismatch(ValueError):
    """A results file does not match the SHA-256 recorded in its ledger."""


@dataclass(frozen=True)
class CaseRecord:
    case_id: str
    run: str
    run_dir: Path
    path: Path
    sha256: str
    finished_utc: str
    matrix_case: dict[str, Any] = field(default_factory=dict)
    params: dict[str, Any] = field(default_factory=dict)
    analysis: str = ""
    run_type: str = ""

    @property
    def group(self) -> str:
        return str(self.matrix_case.get("group", ""))

    @property
    def seed(self) -> int | None:
        s = self.matrix_case.get("seed")
        return int(s) if s is not None else None

    @property
    def base_id(self) -> str:
        return str(self.matrix_case.get("case_id", self.case_id)) if self.seed is not None else self.case_id


def sha256_file(path: Path) -> str:
    h = hashlib.sha256()
    with open(path, "rb") as f:
        for chunk in iter(lambda: f.read(1 << 20), b""):
            h.update(chunk)
    return h.hexdigest()


MISSING = "missing"


def _ledger(run_dir: Path) -> list[dict[str, Any]]:
    out = []
    path = run_dir / "ledger.jsonl"
    if not path.exists():  # interrupted before the first case finished: every declared case is missing
        return out
    for line in path.read_text(encoding="utf-8").splitlines():
        if line.strip():
            out.append(json.loads(line))
    return out


def collect(run_dirs: Iterable[Path]) -> tuple[dict[str, CaseRecord], list[dict[str, Any]]]:
    """Final record per case over ``run_dirs``. Returns (ok records by case id, cases without a result).

    Cases without a result are those whose final ledger record is not ``ok`` and those declared in a ``run.json``
    that no ledger records at all (status ``missing``)."""
    latest: dict[str, tuple[str, int, Path, dict, dict]] = {}
    declared: dict[str, Path] = {}
    order = 0
    for run_dir in run_dirs:
        run_dir = Path(run_dir)
        run = json.loads((run_dir / "run.json").read_text(encoding="utf-8"))
        cases = {c["case_id"]: c for c in run.get("cases", [])}
        for cid in cases:
            declared.setdefault(cid, run_dir)
        for rec in _ledger(run_dir):
            order += 1
            key = (rec.get("finished_utc") or "", order)
            cur = latest.get(rec["case_id"])
            if cur is None or key > (cur[0], cur[1]):
                latest[rec["case_id"]] = (key[0], key[1], run_dir, rec, cases.get(rec["case_id"], {}))
    records: dict[str, CaseRecord] = {}
    issues: list[dict[str, Any]] = []
    for cid, (fin, _, run_dir, rec, case) in sorted(latest.items()):
        if rec.get("status") != "ok":
            issues.append({"case_id": cid, "status": rec.get("status"), "run": run_dir.name,
                           "message": rec.get("message", "")})
            continue
        records[cid] = CaseRecord(case_id=cid, run=run_dir.name, run_dir=run_dir, path=run_dir / rec["results_path"],
                                  sha256=rec["results_sha256"], finished_utc=fin,
                                  matrix_case=case.get("matrix_case", {}), params=case.get("params", {}),
                                  analysis=case.get("analysis", rec.get("analysis", "")),
                                  run_type=case.get("run_type", ""))
    for cid in sorted(set(declared) - set(latest)):
        issues.append({"case_id": cid, "status": MISSING, "run": declared[cid].name,
                       "message": "declared in run.json but no ledger record in any run (never attempted)"})
    return records, issues


def load_verified(record: CaseRecord) -> dict[str, Any]:
    """The results document, after its SHA-256 is checked against the ledger."""
    data = record.path.read_bytes()
    got = hashlib.sha256(data).hexdigest()
    if got != record.sha256:
        raise DigestMismatch(f"{record.case_id}: {record.path.name} sha256 {got} != ledger {record.sha256}")
    return json.loads(data.decode("utf-8"))


def seed_groups(records: Iterable[CaseRecord]) -> dict[str, list[CaseRecord]]:
    """Seeded records grouped by their matrix case, seeds in order."""
    out: dict[str, list[CaseRecord]] = {}
    for r in records:
        if r.seed is not None:
            out.setdefault(r.base_id, []).append(r)
    return {k: sorted(v, key=lambda r: r.seed) for k, v in sorted(out.items())}


def case_key(matrix_case: dict[str, Any]) -> dict[str, Any]:
    """Grouping key: mode, configuration, metocean condition, heading, offset, mud weight, tension, tensioner state."""
    return {k: matrix_case.get(k) for k in KEY_FIELDS}
