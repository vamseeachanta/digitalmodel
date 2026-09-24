"""Pressure-absence-only Windows process collection with retained raw evidence."""
import base64
import calendar
from copy import deepcopy
from datetime import datetime
from fractions import Fraction
import hashlib
import json
import os
from pathlib import Path
import re
import shutil
import socket
import subprocess
import time

import psutil

from .cylinder_absence_population import PopulationError, reconcile_observed_inventories
from .cylinder_diagnostic_snapshot import _details, _seed
from .cylinder_parent_map import _initial_identity, read_windows_parent_map

QUERY_TIMEOUT_SECONDS = 2
MAX_QUERY_PIDS = 64
RAW_FIELDS = {"ProcessId", "ParentProcessId", "Name", "CreationDate", "ExecutablePath"}
QUERY_SCRIPT = r"""
$ErrorActionPreference = 'Stop'
$ProgressPreference = 'SilentlyContinue'
[Console]::OutputEncoding = [Text.UTF8Encoding]::new($false)
$raw = [Console]::In.ReadToEnd()
if ($raw -notmatch '^(0|[1-9][0-9]*)(,(0|[1-9][0-9]*))*$') { throw 'invalid PID input' }
$ids = @($raw.Split(',') | ForEach-Object { [uint32]::Parse($_) })
$filter = ($ids | ForEach-Object { "ProcessId=$_" }) -join ' OR '
$rows = @(Get-WmiObject Win32_Process -Filter $filter | ForEach-Object {
  [ordered]@{ProcessId=$_.ProcessId; ParentProcessId=$_.ParentProcessId;
    Name=$_.Name; CreationDate=$_.CreationDate; ExecutablePath=$_.ExecutablePath}
})
ConvertTo-Json -Compress -InputObject $rows
""".strip()


class CollectionError(ValueError):
    """Fail closed while retaining JSON-safe subprocess observation evidence."""

    def __init__(self, message, evidence):
        super().__init__(message)
        self.evidence = evidence


class PopulationReadError(ValueError):
    """Identity-read refusal retaining its parent map and completed rows."""

    def __init__(self, message, evidence):
        super().__init__(message)
        self.evidence = evidence


def _b64(value):
    if value is None:
        return ""
    if isinstance(value, str):
        value = value.encode("utf-8", errors="replace")
    return base64.b64encode(value).decode("ascii")


def _powershell_identity():
    discovered = shutil.which("powershell.exe")
    system_root = os.environ.get("SystemRoot") or os.environ.get("WINDIR")
    if not discovered or not system_root:
        raise ValueError("Windows PowerShell executable unavailable")
    source = Path(discovered)
    expected = Path(system_root) / "System32/WindowsPowerShell/v1.0/powershell.exe"
    if (not source.is_absolute() or source != expected
            or source.name.casefold() != "powershell.exe"):
        raise ValueError("Windows PowerShell executable identity invalid")
    for item in (source, *source.parents[:-1]):
        try:
            stat = item.lstat()
        except OSError as exc:
            raise ValueError("Windows PowerShell executable identity unavailable") from exc
        if stat.st_file_attributes & 0x400:
            raise ValueError("Windows PowerShell executable reparse path refused")
    if not source.is_file():
        raise ValueError("Windows PowerShell executable identity invalid")
    try:
        before = source.stat()
        content = source.read_bytes()
        after = source.stat()
    except OSError as exc:
        raise ValueError("Windows PowerShell executable unreadable") from exc
    before_id = (before.st_dev, before.st_ino, before.st_size, before.st_mtime_ns)
    after_id = (after.st_dev, after.st_ino, after.st_size, after.st_mtime_ns)
    if before_id != after_id or len(content) != before.st_size:
        raise ValueError("Windows PowerShell executable changed during hashing")
    return str(source), hashlib.sha256(content).hexdigest()


def _validate_pids(pids):
    if (not isinstance(pids, list) or not 1 <= len(pids) <= MAX_QUERY_PIDS
            or any(type(pid) is not int or not 0 <= pid < 2**32 for pid in pids)
            or len(set(pids)) != len(pids)):
        raise ValueError("Invalid bounded PID query")
    return sorted(pids)


def _parse_query_output(output):
    try:
        value = json.loads(output.decode("utf-8"))
    except (UnicodeDecodeError, json.JSONDecodeError) as exc:
        raise ValueError("Windows process query returned invalid JSON") from exc
    if not isinstance(value, list) or len(value) > MAX_QUERY_PIDS:
        raise ValueError("Windows process query returned invalid JSON shape")
    for row in value:
        if not isinstance(row, dict) or set(row) != RAW_FIELDS:
            raise ValueError("Windows process query returned invalid fields")
        if (type(row["ProcessId"]) is not int or type(row["ParentProcessId"]) is not int
                or not isinstance(row["Name"], str)
                or not isinstance(row["CreationDate"], str)
                or (row["ExecutablePath"] is not None
                    and not isinstance(row["ExecutablePath"], str))):
            raise ValueError("Windows process query returned invalid values")
    return value


def _query_evidence(argv, stdin, digest, elapsed, completed=None, error=None):
    stdout = error.output if error else completed.stdout
    stderr = error.stderr if error else completed.stderr
    return {
        "performed": True,
        "query": "Win32_Process identity fields filtered by the exact stdin PID set",
        "argv": argv,
        "stdin_base64": _b64(stdin),
        "script_sha256": hashlib.sha256(QUERY_SCRIPT.encode()).hexdigest(),
        "powershell_sha256": digest,
        "elapsed_ns": elapsed,
        "return_code": None if error else completed.returncode,
        "stdout_base64": _b64(stdout),
        "stderr_base64": _b64(stderr),
        "raw_rows": [],
        "dependency_pins": {"powershell_path": argv[0],
                            "powershell_sha256": digest,
                            "script_sha256": hashlib.sha256(
                                QUERY_SCRIPT.encode()).hexdigest()},
        "provider_metadata": {"provider": "Win32_Process via Get-WmiObject"},
    }


def _query_blank_names(pids):
    pids = _validate_pids(pids)
    executable, digest = _powershell_identity()
    encoded = base64.b64encode(QUERY_SCRIPT.encode("utf-16le")).decode("ascii")
    argv = [executable, "-NoProfile", "-NonInteractive", "-EncodedCommand", encoded]
    stdin = ",".join(map(str, pids)).encode("ascii")
    started = time.perf_counter_ns()
    try:
        completed = subprocess.run(
            argv, input=stdin, capture_output=True, timeout=QUERY_TIMEOUT_SECONDS,
            shell=False, creationflags=subprocess.CREATE_NO_WINDOW,
        )
    except subprocess.TimeoutExpired as exc:
        elapsed = time.perf_counter_ns() - started
        evidence = _query_evidence(argv, stdin, digest, elapsed, error=exc)
        raise CollectionError("Windows process query timed out", evidence) from exc
    elapsed = time.perf_counter_ns() - started
    evidence = _query_evidence(argv, stdin, digest, elapsed, completed=completed)
    if completed.returncode != 0:
        raise CollectionError("Windows process query failed", evidence)
    if completed.stderr:
        raise CollectionError("Windows process query returned error output", evidence)
    try:
        evidence["raw_rows"] = _parse_query_output(completed.stdout)
    except ValueError as exc:
        raise CollectionError(str(exc), evidence) from exc
    return evidence


def _dmtf_epoch(value):
    match = re.fullmatch(r"(\d{14})\.(\d{6})([+-])(\d{3})", value)
    if not match:
        raise ValueError("Invalid DMTF creation date")
    try:
        instant = datetime.strptime(match.group(1), "%Y%m%d%H%M%S")
    except ValueError as exc:
        raise ValueError("Invalid DMTF creation date") from exc
    local = Fraction(calendar.timegm(instant.timetuple()))
    local += Fraction(int(match.group(2)), 1_000_000)
    offset = int(match.group(4)) * 60 * (1 if match.group(3) == "+" else -1)
    return local - offset


def _psutil_epoch(value):
    if not isinstance(value, str) or not re.fullmatch(r"\d+(?:\.\d+)?", value):
        raise ValueError("Invalid psutil creation time")
    return Fraction(value)


def _reconcile(original, evidence):
    unresolved = {row["pid"]: row for row in original if not row["name"].strip()}
    raw_rows = evidence["raw_rows"]
    if len(raw_rows) != len(unresolved):
        raise CollectionError("Windows name result PID set differs", evidence)
    indexed = {}
    for raw in raw_rows:
        pid = raw["ProcessId"]
        if pid in indexed or pid not in unresolved:
            raise CollectionError("Windows name result PID set differs", evidence)
        if not raw["Name"].strip() or len(raw["Name"]) > 4096:
            raise CollectionError("Windows process name remains unresolved", evidence)
        initial = unresolved[pid]
        if raw["ParentProcessId"] != initial["parent_pid"]:
            raise CollectionError("Windows process parent identity differs", evidence)
        if abs(_dmtf_epoch(raw["CreationDate"]) - _psutil_epoch(
                initial["creation_time"])) > Fraction(1, 1_000_000):
            raise CollectionError("Windows process creation identity differs", evidence)
        indexed[pid] = raw
    return indexed


def _normalize(original, resolved):
    rows = deepcopy(original)
    for row in rows:
        if not row["name"].strip():
            raw = resolved.get(row["pid"])
            if raw is None:
                raise ValueError("Process name remained unresolved after the single query")
            if abs(_dmtf_epoch(raw["CreationDate"]) - _psutil_epoch(
                    row["creation_time"])) > Fraction(1, 1_000_000):
                raise ValueError("Resolved process creation identity differs")
            row["name"] = raw["Name"]
        if row["executable_path"] == "":
            row["executable_path"] = None
    return rows


def _read_population(parent_map):
    rows = []
    for pid in sorted(parent_map):
        try:
            process = psutil.Process(pid)
            rows.append(_initial_identity(process, parent_map))
        except (psutil.Error, OSError, ValueError) as exc:
            evidence = {"failed_pid": pid, "rows": deepcopy(rows),
                        "parent_map": deepcopy(parent_map)}
            raise PopulationReadError(
                f"Process {pid} identity unreadable", evidence,
            ) from exc
    return rows


def _collect_initial_populations():
    map_a = read_windows_parent_map()
    try:
        rows_a = _read_population(map_a)
    except PopulationReadError as exc:
        exc.evidence["stage"] = "A"
        raise
    map_b = read_windows_parent_map()
    try:
        rows_b = _read_population(map_b)
    except PopulationReadError as exc:
        exc.evidence.update(stage="B", prior_rows=[deepcopy(rows_a)],
                            prior_maps=[deepcopy(map_a)])
        raise
    return [rows_a, rows_b], [map_a, map_b]


def _collect_final_population():
    map_c = read_windows_parent_map()
    try:
        return _read_population(map_c), map_c
    except PopulationReadError as exc:
        exc.evidence["stage"] = "C"
        raise


def _query_targets(raw_inventories):
    indexed = {}
    blank_pids = set()
    for rows in raw_inventories:
        for row in rows:
            previous = indexed.get(row["pid"])
            if previous and previous["creation_time"] != row["creation_time"]:
                raise ValueError("Process identifier reused before name resolution")
            indexed[row["pid"]] = row
            if not row["name"].strip():
                blank_pids.add(row["pid"])
    return [indexed[pid] for pid in sorted(blank_pids)]


def _parent_map_evidence(parent_maps):
    return [{"stage": stage, "rows": [
        {"pid": pid, "parent_pid": values[pid]} for pid in sorted(values)]}
        for stage, values in zip(("A", "B", "C"), parent_maps)]


def _selected(initial):
    family = {row["pid"] for row in initial if _seed(row)}
    while True:
        expanded = family | {row["pid"] for row in initial
                             if row["parent_pid"] in family}
        if expanded == family:
            return [row for row in initial if row["pid"] in family]
        family = expanded


def _no_query_evidence():
    return {"performed": False, "query": None, "argv": [], "stdin_base64": "",
            "script_sha256": hashlib.sha256(QUERY_SCRIPT.encode()).hexdigest(),
            "powershell_sha256": None, "elapsed_ns": 0, "return_code": None,
            "stdout_base64": "", "stderr_base64": "", "raw_rows": [],
            "dependency_pins": {},
            "provider_metadata": {"provider": "Win32_Process via Get-WmiObject"}}


def _decimal_nanoseconds(value):
    if type(value) is not int or value < 0:
        raise ValueError("Invalid observation clock")
    seconds, remainder = divmod(value, 1_000_000_000)
    if not remainder:
        return str(seconds)
    return f"{seconds}.{remainder:09d}".rstrip("0")


def _collection_failure(error, raw, normalized, maps, name_resolution,
                        population_check=None):
    if isinstance(error, PopulationReadError):
        partial = deepcopy(error.evidence)
        if partial["stage"] == "C":
            raw = [*raw, partial["rows"]]
            maps = [*maps, partial["parent_map"]]
        else:
            raw = [*partial.get("prior_rows", []), partial["rows"]]
            maps = [*partial.get("prior_maps", []), partial["parent_map"]]
    evidence = {"original_initial_inventory": deepcopy(raw[0]) if raw else [],
                "original_observed_inventories": deepcopy(raw),
                "observed_inventories": deepcopy(normalized),
                "parent_maps": _parent_map_evidence(maps) if maps else [],
                "name_resolution": deepcopy(name_resolution)}
    if isinstance(error, PopulationReadError):
        failure = deepcopy(error.evidence)
        failure["parent_map"] = _parent_map_evidence([failure["parent_map"]])[0]["rows"]
        if "prior_maps" in failure:
            failure["prior_maps"] = _parent_map_evidence(failure["prior_maps"])
        evidence["identity_read_failure"] = failure
    if isinstance(error, CollectionError):
        evidence["name_resolution"] = deepcopy(error.evidence)
    elif isinstance(error, PopulationError):
        evidence["population_check"] = deepcopy(error.evidence)
    elif population_check is not None:
        evidence["population_check"] = deepcopy(population_check)
    return CollectionError(str(error), evidence)


def collect_absence_snapshot():
    """Collect one fail-closed pressure-absence process snapshot."""
    raw, normalized, parent_maps = [], [], []
    evidence, population_check = None, None
    try:
        raw, parent_maps = _collect_initial_populations()
        targets = _query_targets(raw)
        evidence = (_query_blank_names([row["pid"] for row in targets])
                    if targets else _no_query_evidence())
        resolved = _reconcile(targets, evidence) if targets else {}
        normalized = [_normalize(rows, resolved) for rows in raw]
        final_raw, final_map = _collect_final_population()
        raw.append(final_raw)
        parent_maps.append(final_map)
        normalized.append(_normalize(final_raw, resolved))
        population_check = reconcile_observed_inventories(normalized, parent_maps)
        population_check["parent_maps"] = _parent_map_evidence(parent_maps)
        initial = normalized[-1]
        selected = _selected(initial)
        cache = {}
        rows = [_details(row, cache) for row in selected]
    except (ValueError, OSError, subprocess.SubprocessError) as exc:
        raise _collection_failure(
            exc, raw, normalized, parent_maps, evidence, population_check,
        ) from exc
    coverage = {"selector": "ansys-mpi-lineage-v1",
                "enumerated_count": len(initial), "selected_count": len(rows),
                "excluded_count": len(initial) - len(rows),
                "selected_details_complete": True,
                "limitation": "Nonselected protected image paths may be unavailable."}
    return {"schema": "process-snapshot-1", "host": socket.gethostname(),
            "observed_at": _decimal_nanoseconds(time.time_ns()),
            "enumeration_complete": True, "rows": rows,
            "initial_inventory": initial, "coverage": coverage,
            "original_initial_inventory": deepcopy(raw[0]),
            "original_observed_inventories": deepcopy(raw),
            "observed_inventories": deepcopy(normalized),
            "name_resolution": evidence, "population_check": population_check}
