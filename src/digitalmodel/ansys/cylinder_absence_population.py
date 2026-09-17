"""Pure A/B/C population reconciliation for pressure-resource absence."""
from copy import deepcopy

from .cylinder_diagnostic_snapshot import _seed

STAGES = ("A", "B", "C")
FIELDS = {"pid", "parent_pid", "name", "executable_path", "creation_time"}


class PopulationError(ValueError):
    """Population refusal with JSON-safe observed change evidence."""

    def __init__(self, message, evidence):
        super().__init__(message)
        self.evidence = evidence


def _bounded_text(value, *, optional=False):
    if optional and value is None:
        return
    if not isinstance(value, str) or not value or len(value) > 4096:
        raise ValueError("Invalid bounded population text")


def _index(rows, parent_map):
    if not isinstance(rows, list) or not isinstance(parent_map, dict):
        raise ValueError("Invalid observed population")
    indexed = {}
    for row in rows:
        if not isinstance(row, dict) or set(row) != FIELDS:
            raise ValueError("Invalid observed process fields")
        pid, parent = row["pid"], row["parent_pid"]
        if (type(pid) is not int or type(parent) is not int
                or not 0 <= pid < 2**32 or not 0 <= parent < 2**32
                or pid in indexed):
            raise ValueError("Invalid observed process identity")
        _bounded_text(row["name"])
        _bounded_text(row["creation_time"])
        _bounded_text(row["executable_path"], optional=True)
        indexed[pid] = row
    if set(parent_map) != set(indexed):
        raise ValueError("Observed parent map membership differs")
    if any(type(pid) is not int or parent_map[pid] != row["parent_pid"]
           for pid, row in indexed.items()):
        raise ValueError("Observed parent map identity differs")
    return indexed


def _relevant(indexed, required=()):
    relevant = set(required) | {pid for pid, row in indexed.items()
                if _seed(row) or row["name"].casefold() == "interfoam.exe"}
    while True:
        expanded = relevant | {pid for pid, row in indexed.items()
                               if row["parent_pid"] in relevant}
        if expanded == relevant:
            return relevant
        relevant = expanded


def _complete(row):
    return (isinstance(row["name"], str) and bool(row["name"].strip())
            and isinstance(row["executable_path"], str)
            and bool(row["executable_path"].strip())
            and isinstance(row["creation_time"], str)
            and bool(row["creation_time"]))


def _identity(row):
    return row["creation_time"], row["name"], row["executable_path"]


def _refuse(message, evidence, event, disposition):
    event["disposition"] = disposition
    evidence["events"].append(event)
    raise PopulationError(message, deepcopy(evidence))


def _transition(before, after, before_stage, after_stage, relevant, evidence):
    for pid in sorted(set(before) | set(after)):
        left, right = before.get(pid), after.get(pid)
        event = {"from_stage": before_stage, "to_stage": after_stage,
                 "pid": pid}
        if left is None or right is None:
            event["change"] = "added" if left is None else "removed"
            observed = right if left is None else left
            if pid in relevant[before_stage] or pid in relevant[after_stage]:
                _refuse("Observed relevant population change", evidence, event,
                        "REFUSED_RELEVANT")
            if not _complete(observed):
                _refuse("Observed churn identity is not complete", evidence, event,
                        "REFUSED_INCOMPLETE")
            event["disposition"] = "RETAINED_UNRELATED"
            evidence["events"].append(event)
            continue
        if _identity(left) != _identity(right):
            event["change"] = "identity_changed"
            _refuse("Observed process identity changed", evidence, event,
                    "REFUSED_IDENTITY_CHANGE")
        if left["parent_pid"] != right["parent_pid"]:
            event["change"] = "parent_changed"
            if pid in relevant[before_stage] or pid in relevant[after_stage]:
                _refuse("Observed relevant parent transition", evidence, event,
                        "REFUSED_RELEVANT")
            if not _complete(right):
                _refuse("Parent-changed identity is not complete", evidence, event,
                        "REFUSED_INCOMPLETE")
            event["disposition"] = "RETAINED_UNRELATED"
            evidence["events"].append(event)


def reconcile_observed_inventories(inventories, parent_maps, *, required_relevant_pids=()):
    """Retain verified unrelated churn and refuse relevant or unknown churn."""
    if (not isinstance(inventories, list) or len(inventories) != 3
            or not isinstance(parent_maps, list) or len(parent_maps) != 3):
        raise ValueError("Exactly three observed populations are required")
    if (not isinstance(required_relevant_pids, (list, tuple))
            or len(required_relevant_pids) > 65536
            or any(type(pid) is not int or not 0 < pid < 2**32
                   for pid in required_relevant_pids)
            or list(required_relevant_pids) != sorted(set(required_relevant_pids))):
        raise ValueError('Sorted unique bounded relevant process identifiers required')
    indexed = [_index(rows, parents)
               for rows, parents in zip(inventories, parent_maps)]
    relevant = {stage: _relevant(values, required_relevant_pids)
                for stage, values in zip(STAGES, indexed)}
    evidence = {"events": [],
                "relevant_pids": {stage: sorted(relevant[stage])
                                  for stage in STAGES}}
    for pid in sorted(set(indexed[0]) & set(indexed[2])):
        if _identity(indexed[0][pid]) != _identity(indexed[2][pid]):
            _refuse('Observed endpoint process identity changed', evidence,
                    dict(from_stage='A', to_stage='C', pid=pid, change='identity_changed'),
                    'REFUSED_IDENTITY_CHANGE')
        if pid not in indexed[1] and indexed[0][pid]['parent_pid'] != indexed[2][pid]['parent_pid']:
            _transition({pid: indexed[0][pid]}, {pid: indexed[2][pid]},
                        'A', 'C', relevant, evidence)
            evidence['events'][-1]['basis'] = 'endpoint_gap'
    _transition(indexed[0], indexed[1], "A", "B", relevant, evidence)
    _transition(indexed[1], indexed[2], "B", "C", relevant, evidence)
    return evidence
