"""Publication and coverage contracts, separate from numerical CP engines."""
from __future__ import annotations

import json
import re
from typing import Any

REQUIRED_IDS = tuple(f"S{i:02d}" for i in range(1, 12)) + tuple("ABCDE")


def validate_registry(rows: list[dict[str, Any]]) -> None:
    """Reject incomplete registries and silent duplicate replacements."""
    ids = [row["id"] for row in rows]
    if len(ids) != len(set(ids)):
        raise ValueError("duplicate coverage row")
    if set(ids) != set(REQUIRED_IDS):
        raise ValueError("coverage must enumerate all eleven modes and A-E")


def validate_release(payload: Any, allowed: Any) -> None:
    """Require the exact reviewed field/value tree, including nested plot data."""
    if json.dumps(payload, sort_keys=True, allow_nan=False) != json.dumps(
        allowed, sort_keys=True, allow_nan=False
    ):
        raise ValueError("release payload differs from the reviewed allowlist")


def review_document(case_id: str, title: str) -> dict[str, Any]:
    """Assign internal module identities; never impersonate client/job numbers."""
    if case_id not in REQUIRED_IDS:
        raise ValueError("unknown case identity")
    sequence = REQUIRED_IDS.index(case_id) + 1
    return {
        "number": f"R2281-CP-{sequence:03d}-00", "revision": "00",
        "title": f"CP module review: {title}",
        "project": "Digitalmodel CP regression portfolio",
        "client": "Internal module review - no client issuance",
        "date": "2026-10-03", "prepared_by": "Automated regression report",
        "checked_by": "Pending", "approved_by": "Pending",
        "revision_history": [{"rev": "00", "date": "2026-10-03",
                              "description": "Internal review draft",
                              "checked": "Pending", "approved": "Pending"}],
    }


def resolve_pointer(value: Any, pointer: str) -> Any:
    """Resolve a JSON pointer without evaluating code or filesystem paths."""
    if not pointer.startswith("/"):
        raise ValueError("child result pointer must start with /")
    try:
        for token in pointer[1:].split("/"):
            token = token.replace("~1", "/").replace("~0", "~")
            value = value[int(token)] if isinstance(value, list) else value[token]
    except (KeyError, ValueError, IndexError, TypeError) as exc:
        raise ValueError("child result pointer does not resolve") from exc
    return value


def validate_child(child: dict[str, Any], results: Any, html: str) -> None:
    """Check each child names its own actual result and stable rendered anchor."""
    resolve_pointer(results, child["result_pointer"])
    anchor = re.escape(child["anchor"])
    if not re.search(r'id=[\"\']' + anchor + r'[\"\']', html):
        raise ValueError("child anchor does not resolve")


def coverage_summary(rows: list[dict[str, Any]]) -> dict[str, Any]:
    """Use successful verification receipts, never folder presence or PASS."""
    validate_registry(rows)
    verified = sum(row.get("pack_state") == "verified"
                   and row.get("verification", {}).get("passed") is True
                   for row in rows)
    return {"required": len(REQUIRED_IDS), "verified": verified,
            "complete": verified == len(REQUIRED_IDS),
            "manual_visual_review": "deferred_by_owner",
            "engineering_acceptance": "pending"}
