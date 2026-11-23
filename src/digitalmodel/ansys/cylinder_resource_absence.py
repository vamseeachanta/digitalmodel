"""Pure pressure resource-absence validation; no host observation or control."""
from digitalmodel.ansys.cylinder_process_inventory import (
    _pid,
    _text,
    _time,
    classify_process_inventory,
)
from digitalmodel.ansys.cylinder_diagnostic_snapshot import _seed

SCHEMA = "cfd-resource-absence-1"
SELECTOR = "ansys-mpi-lineage-v1"
INITIAL_FIELDS = {
    "pid", "parent_pid", "name", "executable_path", "creation_time",
}


def validate_absence_descriptor(value, host):
    """Recognize this schema; reject malformed instances without claiming resources."""
    if not isinstance(value, dict) or value.get("schema") != SCHEMA:
        return False
    if set(value) != {
        "schema", "host", "selector", "required_selected_state",
    }:
        raise ValueError("Invalid resource-absence descriptor fields")
    expected_host = _text(host, 255)
    if (value["host"] != expected_host or type(value["host"]) is not str
            or value["selector"] != SELECTOR
            or value["required_selected_state"] != "empty"):
        raise ValueError("Resource-absence descriptor value differs")
    return True


def _initial_rows(snapshot):
    values = snapshot.get("initial_inventory")
    coverage = snapshot.get("coverage")
    if not isinstance(values, list) or len(values) > 1000000:
        raise ValueError("Invalid bounded initial inventory")
    if (not isinstance(coverage, dict)
            or type(coverage.get("enumerated_count")) is not int
            or coverage["enumerated_count"] != len(values)):
        raise ValueError("Initial inventory enumeration count differs")
    indexed = {}
    for row in values:
        if not isinstance(row, dict) or set(row) != INITIAL_FIELDS:
            raise ValueError("Invalid initial process fields")
        pid = _pid(row["pid"], parent=True)
        _pid(row["parent_pid"], parent=True)
        _time(row["creation_time"])
        _text(row["name"])
        if row["executable_path"] is not None:
            _text(row["executable_path"])
        if pid in indexed:
            raise ValueError("Duplicate initial process identity")
        indexed[pid] = row
    return indexed


def _cross_link(snapshot, indexed):
    rows, coverage = snapshot.get("rows"), snapshot.get("coverage")
    if not isinstance(rows, list) or not isinstance(coverage, dict):
        raise ValueError("Selected process rows or coverage unavailable")
    if (type(coverage.get("selected_count")) is not int
            or type(coverage.get("excluded_count")) is not int
            or coverage["selected_count"] != len(rows)
            or coverage["excluded_count"] != len(indexed) - len(rows)):
        raise ValueError("Selected and excluded process counts differ")
    for row in rows:
        if not isinstance(row, dict) or type(row.get("pid")) is not int:
            raise ValueError("Invalid selected process identity")
        initial = indexed.get(row["pid"])
        if initial is None or any(
                type(row.get(key)) is not type(initial[key])
                or row.get(key) != initial[key] for key in INITIAL_FIELDS):
            raise ValueError("Selected process differs from initial inventory")
    return {row["pid"] for row in rows}


def _historical_absence(snapshot):
    if 'observed_inventories' not in snapshot:
        return  # Legacy snapshots have no multi-observation claim.
    histories = snapshot['observed_inventories']
    if not isinstance(histories, list) or len(histories) != 3:
        raise ValueError('three historical process inventories required')
    for rows in histories:
        if not isinstance(rows, list):
            raise ValueError('invalid historical process inventory')
        indexed = _initial_rows(dict(initial_inventory=rows,
                                     coverage={'enumerated_count': len(rows)}))
        if any(_seed(row) or row['name'].casefold() == 'interfoam.exe'
               for row in indexed.values()):
            raise ValueError('historical competing process prevents resource absence')
    if histories[-1] != snapshot['initial_inventory']:
        raise ValueError('final historical process inventory differs')


def verify_absence_snapshot(snapshot, host, now):
    """Return an unchanged unbound CLEAR classification or refuse."""
    if not isinstance(snapshot, dict) or snapshot.get("schema") != "process-snapshot-1":
        raise ValueError("Resource absence requires process-snapshot-1")
    indexed = _initial_rows(snapshot)
    _historical_absence(snapshot)
    selected = _cross_link(snapshot, indexed)
    if any(_seed(row) and pid not in selected for pid, row in indexed.items()):
        raise ValueError("Selector seed omitted from selected process rows")
    if any(row["name"].casefold() == "interfoam.exe" for row in indexed.values()):
        raise ValueError("Orphan interFoam process prevents resource absence")
    result = classify_process_inventory(
        snapshot, expected_host=host, cfd_binding=None, now=now,
        maximum_age_seconds="30",
    )
    empty_keys = ("dispositions", "conflicts", "unknowns", "process_inventory")
    if result.get("status") != "CLEAR":
        raise ValueError(f"Competing process classification {result.get('status')}")
    if snapshot["rows"] or any(result.get(key) != [] for key in empty_keys):
        raise ValueError("Resource absence requires an empty selected process set")
    return result
