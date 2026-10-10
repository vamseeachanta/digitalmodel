"""Compare a benchmark receipt against a baseline receipt.

Fingerprints say whether the machine still gets the same answer; solve-time
ratios say whether it is still as fast. A solver version change is reported
rather than failed, but never excuses a run that did not complete.
"""

from __future__ import annotations

import math

SLOWER_RATIO = 1.20


def _positive(value) -> bool:
    return isinstance(value, (int, float)) and math.isfinite(value) and value > 0


def _key(entry: dict) -> tuple:
    return entry["case"], entry["variant"]


def _matches(new: dict, base: dict, rel: float, ab: float) -> bool:
    if new.keys() != base.keys():
        return False
    for key in base:
        xs = new[key] if isinstance(new[key], list) else [new[key]]
        ys = base[key] if isinstance(base[key], list) else [base[key]]
        if len(xs) != len(ys) or not all(
            math.isclose(x, y, rel_tol=rel, abs_tol=ab) for x, y in zip(xs, ys)
        ):
            return False
    return True


def tolerances(new: dict, base: dict) -> tuple[float, float]:
    """Relative and absolute tolerance for a pair of entries (the looser of each)."""
    return (max(base.get("rel_tol", 1e-6), new.get("rel_tol", 1e-6)),
            max(base.get("abs_tol", 0.0), new.get("abs_tol", 0.0)))


def differing_keys(new: dict, base: dict) -> list:
    """Fingerprint keys that are missing on one side or differ beyond tolerance."""
    rel, ab = tolerances(new, base)
    fn, fb = new.get("fingerprint") or {}, base.get("fingerprint") or {}
    return [key for key in sorted(set(fn) | set(fb), key=str)
            if key not in fn or key not in fb
            or not _matches({key: fn[key]}, {key: fb[key]}, rel, ab)]


def fingerprint_status(new: dict, base: dict) -> str:
    """FAILED, INPUT_CHANGED, MATCH, VERSION_CHANGED or MISMATCH for two entries."""
    if (not new.get("n_ok") or not new.get("fingerprint")
            or not new.get("fingerprint_consistent", True)
            or not new.get("complete", True)):
        return "FAILED"
    if new.get("input_sha256") != base.get("input_sha256"):
        return "INPUT_CHANGED"
    rel, ab = tolerances(new, base)
    if _matches(new["fingerprint"], base["fingerprint"], rel, ab):
        return "MATCH"
    if new.get("solver_version") != base.get("solver_version"):
        return "VERSION_CHANGED"
    return "MISMATCH"


def compare(new: dict, base: dict, slower_ratio: float = SLOWER_RATIO) -> dict:
    if new.get("pack_version") != base.get("pack_version"):
        raise ValueError(
            f"pack version {new.get('pack_version')} cannot be compared with "
            f"baseline pack version {base.get('pack_version')}"
        )
    ineligible = [_key(e) for e in base["results"] if not e.get("baseline_eligible")]
    if ineligible:
        raise ValueError(f"baseline entries are not baseline-eligible: {ineligible}")

    found = {_key(e): e for e in new["results"]}
    rows = []
    for b in base["results"]:
        n = found.get(_key(b))
        row = {"case": b["case"], "variant": b["variant"]}
        if n is None:
            row.update(fingerprint="MISSING", timing="-")
        elif (status := fingerprint_status(n, b)) in ("FAILED", "INPUT_CHANGED"):
            row.update(fingerprint=status, timing="-")
        else:
            row["fingerprint"] = status
            new_t = (n.get("solve_s") or {}).get("median")
            base_t = (b.get("solve_s") or {}).get("median")
            same_basis = n.get("timing_basis", "solver") == b.get("timing_basis", "solver")
            if not (same_basis and _positive(new_t) and _positive(base_t)):
                row["timing"] = "NOT_COMPARABLE"
            else:
                ratio = new_t / base_t
                row["time_ratio"] = round(ratio, 3)
                row["timing"] = ("SLOWER" if ratio > slower_ratio
                                 else "FASTER" if ratio < 1 / slower_ratio else "OK")
        rows.append(row)
    ok = all(r["fingerprint"] in ("MATCH", "VERSION_CHANGED") for r in rows)
    return {"baseline_machine": base.get("machine_label"),
            "machine": new.get("machine_label"), "rows": rows, "ok": ok}
