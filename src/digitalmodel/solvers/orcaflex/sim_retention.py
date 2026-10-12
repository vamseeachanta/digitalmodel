"""Time-bound deletion of reproducible OrcaFlex ``.sim`` files (owner decision W403: keep none).

A ``.sim`` written by :mod:`digitalmodel.solvers.orcaflex.parallel_runner` is *reproducible* once its results are
extracted and verified: the run ledger holds the input digest (every text-model file plus the case JSON) and the
OrcaFlex DLL version, so the case can be re-solved exactly. Per run directory (one holding ``ledger.jsonl``):

* :func:`check` lists the kept ``.sim`` files that cannot be marked yet, with the reason;
* :func:`mark` appends a ``marked`` event to ``retention.jsonl`` for each kept ``.sim`` whose latest successful
  ledger record passes: results file present, its SHA-256 equal to the ledger, the required channel keys present,
  input digest and DLL version recorded, and the ``.sim`` SHA-256 equal to the ledger. The event carries
  ``delete_after_utc`` = mark time + the retention window (default 72 h);
* :func:`sweep` deletes each marked ``.sim`` whose window has passed, after re-checking its digest (a file that
  changed since marking is kept and logged ``kept_changed``), and logs ``deleted`` with the bytes freed.

:func:`cycle` runs mark then sweep over every run directory below the given roots. The command line wraps it::

    python -m digitalmodel.solvers.orcaflex.sim_retention cycle --root RUNS [--window-hours 72]
    python -m digitalmodel.solvers.orcaflex.sim_retention loop  --root RUNS --interval-min 60 --until-empty

``loop`` repeats the cycle until no marked ``.sim`` waits for its window (``--until-empty``; start it after the
batch, and an unverifiable file never keeps it running) or forever. The events file is the state, so a loop that
stops (host restart) is resumed by starting it again.
"""

from __future__ import annotations

import argparse
import datetime as dt
import hashlib
import json
import sys
import time
from pathlib import Path
from typing import Iterable

RETENTION_FILE = "retention.jsonl"
LEDGER = "ledger.jsonl"
DEFAULT_WINDOW_H = 72.0
DEFAULT_REQUIRED = ("w5",)
_FMT = "%Y-%m-%dT%H:%M:%SZ"


def _utc(t: dt.datetime) -> str:
    return t.astimezone(dt.timezone.utc).strftime(_FMT)


def _parse(s: str) -> dt.datetime:
    return dt.datetime.strptime(s, _FMT).replace(tzinfo=dt.timezone.utc)


def _sha256(path: Path) -> str:
    h = hashlib.sha256()
    with open(path, "rb") as fh:
        for chunk in iter(lambda: fh.read(1 << 20), b""):
            h.update(chunk)
    return h.hexdigest()


def _jsonl(path: Path) -> list[dict]:
    if not path.exists():
        return []
    return [json.loads(x) for x in path.read_text(encoding="utf-8").splitlines() if x.strip()]


def _append(run_dir: Path, event: dict) -> None:
    with (run_dir / RETENTION_FILE).open("a", encoding="utf-8") as fh:
        fh.write(json.dumps(event, sort_keys=True) + "\n")


def latest_ok(run_dir: Path) -> dict[str, dict]:
    """Latest successful ledger record per case (a re-extraction appended later wins)."""
    out: dict[str, dict] = {}
    for r in _jsonl(Path(run_dir) / LEDGER):
        if r.get("status") == "ok":
            out[r["case_id"]] = r
    return out


def _state(run_dir: Path) -> dict[str, dict]:
    """sim path -> its latest retention event."""
    out: dict[str, dict] = {}
    for e in _jsonl(Path(run_dir) / RETENTION_FILE):
        out[e["sim"]] = e
    return out


def _reason(run_dir: Path, rec: dict, required: Iterable[str], *, hash_sim: bool = True) -> str | None:
    """None when the record's extraction is verified and its .sim is reproducible; else why not."""
    if not rec.get("input_sha256"):
        return "no input digest in the ledger"
    if not rec.get("orcaflex_dll"):
        return "no solver version in the ledger"
    rp = rec.get("results_path")
    if not rp or not (run_dir / rp).exists():
        return "results file missing"
    if _sha256(run_dir / rp) != rec.get("results_sha256"):
        return "results digest differs from the ledger"
    try:
        ch = json.loads((run_dir / rp).read_text(encoding="utf-8")).get("channels", {})
    except (json.JSONDecodeError, OSError) as exc:
        return f"results file unreadable: {exc}"
    miss = [k for k in required if k not in ch]
    if miss:
        return f"missing channels {miss}"
    if hash_sim and _sha256(run_dir / rec["sim_kept"]) != rec.get("sim_sha256"):
        return "sim digest differs from the ledger"
    return None


def _candidates(run_dir: Path):
    state = _state(run_dir)
    for cid, rec in latest_ok(run_dir).items():
        sim = rec.get("sim_kept")
        if not sim or not (run_dir / sim).exists() or sim in state:
            continue
        yield cid, rec


def check(run_dir: Path | str, *, required: Iterable[str] = DEFAULT_REQUIRED) -> list[dict]:
    """Kept .sim files that cannot be marked, with the reason."""
    run_dir = Path(run_dir)
    out = []
    for cid, rec in _candidates(run_dir):
        why = _reason(run_dir, rec, required)
        if why:
            out.append({"case_id": cid, "sim": rec["sim_kept"], "reason": why})
    return out


def mark(run_dir: Path | str, *, now: dt.datetime | None = None, window_h: float = DEFAULT_WINDOW_H,
         required: Iterable[str] = DEFAULT_REQUIRED) -> list[dict]:
    """Mark each verified, reproducible kept .sim for deletion after ``window_h`` hours; returns the new marks."""
    run_dir = Path(run_dir)
    now = now or dt.datetime.now(dt.timezone.utc)
    out = []
    for cid, rec in _candidates(run_dir):
        if _reason(run_dir, rec, required):
            continue
        ev = {"event": "marked", "case_id": cid, "sim": rec["sim_kept"], "sim_sha256": rec["sim_sha256"],
              "sim_bytes": (run_dir / rec["sim_kept"]).stat().st_size, "input_sha256": rec["input_sha256"],
              "orcaflex_dll": rec["orcaflex_dll"], "results_path": rec["results_path"],
              "results_sha256": rec["results_sha256"], "window_h": window_h, "marked_utc": _utc(now),
              "delete_after_utc": _utc(now + dt.timedelta(hours=window_h))}
        _append(run_dir, ev)
        out.append(ev)
    return out


def sweep(run_dir: Path | str, *, now: dt.datetime | None = None) -> list[dict]:
    """Delete marked .sim files whose window has passed; returns the deletions."""
    run_dir = Path(run_dir)
    now = now or dt.datetime.now(dt.timezone.utc)
    out = []
    for sim, ev in _state(run_dir).items():
        if ev["event"] != "marked" or _parse(ev["delete_after_utc"]) > now:
            continue
        p = run_dir / sim
        if not p.exists():
            _append(run_dir, {**ev, "event": "already_absent", "at_utc": _utc(now)})
            continue
        if _sha256(p) != ev["sim_sha256"]:
            _append(run_dir, {**ev, "event": "kept_changed", "at_utc": _utc(now)})
            continue
        size = p.stat().st_size
        p.unlink()
        d = {"event": "deleted", "case_id": ev["case_id"], "sim": sim, "sim_sha256": ev["sim_sha256"],
             "bytes": size, "deleted_utc": _utc(now)}
        _append(run_dir, d)
        out.append(d)
    return out


def run_dirs(roots: Iterable[Path | str]) -> list[Path]:
    found = set()
    for r in roots:
        r = Path(r)
        if (r / LEDGER).exists():
            found.add(r)
        found.update(p.parent for p in r.rglob(LEDGER))
    return sorted(found)


def pending_bytes(run_dir: Path) -> int:
    """Bytes of kept .sim files still on disk (marked or not)."""
    total = 0
    for rec in latest_ok(run_dir).values():
        sim = rec.get("sim_kept")
        if sim and (run_dir / sim).exists():
            total += (run_dir / sim).stat().st_size
    return total


def _now() -> dt.datetime:
    return dt.datetime.now(dt.timezone.utc)


def marked_pending_bytes(run_dir: Path) -> int:
    """Bytes of marked .sim files still on disk (waiting for their window)."""
    return sum((run_dir / s).stat().st_size for s, ev in _state(run_dir).items()
               if ev["event"] == "marked" and (run_dir / s).exists())


def cycle(roots: Iterable[Path | str], *, now: dt.datetime | None = None, window_h: float = DEFAULT_WINDOW_H,
          required: Iterable[str] = DEFAULT_REQUIRED) -> dict:
    """Mark, then sweep, every run directory below ``roots``; returns totals and the unmarkable files."""
    now = now or _now()
    marked = deleted = freed = pend = mpend = 0
    blocked = []
    for d in run_dirs(roots):
        marked += len(mark(d, now=now, window_h=window_h, required=required))
        gone = sweep(d, now=now)
        deleted += len(gone)
        freed += sum(g["bytes"] for g in gone)
        pend += pending_bytes(d)
        mpend += marked_pending_bytes(d)
        blocked += [{"run_dir": str(d), **b} for b in check(d, required=required)]
    return {"at_utc": _utc(now), "marked": marked, "deleted": deleted, "freed_bytes": freed,
            "pending_bytes": pend, "marked_pending_bytes": mpend, "blocked": blocked}


def main(argv: list[str] | None = None) -> int:
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("command", choices=("check", "cycle", "loop"))
    ap.add_argument("--root", action="append", required=True, help="folder holding run directories (repeatable)")
    ap.add_argument("--window-hours", type=float, default=DEFAULT_WINDOW_H)
    ap.add_argument("--required", default=",".join(DEFAULT_REQUIRED), help="channel keys a verified result must hold")
    ap.add_argument("--interval-min", type=float, default=60.0)
    ap.add_argument("--until-empty", action="store_true")
    a = ap.parse_args(argv)
    req = tuple(k for k in a.required.split(",") if k)
    if a.command == "check":
        print(json.dumps([c for d in run_dirs(a.root) for c in check(d, required=req)], indent=1))
        return 0
    while True:
        s = cycle(a.root, now=_now(), window_h=a.window_hours, required=req)
        print(json.dumps({k: v for k, v in s.items() if k != "blocked"} | {"blocked": len(s["blocked"])}), flush=True)
        # --until-empty: stop once no marked file waits for its window (an unverifiable file never keeps it running)
        if a.command == "cycle" or (a.until_empty and s["marked_pending_bytes"] == 0):
            return 0
        time.sleep(a.interval_min * 60.0)


if __name__ == "__main__":
    sys.exit(main())
