"""Strict lock records and Linux process-liveness evidence."""

import math
import os
import re
from pathlib import Path

LOCK_SCHEMA = 1
_BOOT_ID = re.compile(
    r"[0-9a-f]{8}-[0-9a-f]{4}-[1-5][0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}\Z"
)


def boot_id() -> str:
    try:
        value = Path("/proc/sys/kernel/random/boot_id").read_text().strip()
        return value if _BOOT_ID.fullmatch(value) else "unknown"
    except OSError:
        return "unknown"


def process_start_token(pid: int | None = None) -> str:
    process = os.getpid() if pid is None else pid
    text = Path(f"/proc/{process}/stat").read_text()
    closing = text.rfind(")")
    if closing < 0:
        raise ValueError("malformed proc stat")
    fields = text[closing + 1 :].split()
    if len(fields) < 20:
        raise ValueError("malformed proc stat")
    return fields[19]


def process_state(record: dict) -> str:
    try:
        actual = process_start_token(record["pid"])
    except FileNotFoundError:
        return "dead"
    except (OSError, ValueError, KeyError, IndexError, TypeError):
        return "unknown"
    return "alive-match" if actual == record.get("process_start_token") else "alive-mismatch"


def valid_record(record: dict) -> bool:
    heartbeat = record.get("heartbeat")
    return all((
        type(record.get("schema_version")) is int
        and record["schema_version"] == LOCK_SCHEMA,
        type(record.get("pid")) is int and record["pid"] > 0,
        isinstance(record.get("owner_token"), str) and bool(record["owner_token"]),
        isinstance(record.get("boot_id"), str)
        and bool(_BOOT_ID.fullmatch(record["boot_id"])),
        isinstance(record.get("process_start_token"), str) and bool(record["process_start_token"]),
        type(heartbeat) in {int, float} and math.isfinite(heartbeat),
    ))


def lock_reclaimable(
    record: dict, *, owner_token: str, now: float, current_boot_id: str,
    process_state: str, stale_after: float,
) -> bool:
    if not valid_record(record) or record["owner_token"] != owner_token:
        return False
    if not math.isfinite(now) or now - record["heartbeat"] <= stale_after:
        return False
    if process_state == "unknown":
        return False
    prior_boot = current_boot_id != "unknown" and record["boot_id"] != current_boot_id
    return prior_boot or process_state in {"dead", "alive-mismatch"}
