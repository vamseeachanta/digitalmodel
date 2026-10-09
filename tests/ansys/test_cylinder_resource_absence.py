"""RED contract tests for the pressure-only resource-absence descriptor."""
from copy import deepcopy
import importlib

import pytest


HOST = "TEST-HOST"
EMPTY_DESCRIPTOR = {
    "schema": "cfd-resource-absence-1",
    "host": HOST,
    "selector": "ansys-mpi-lineage-v1",
    "required_selected_state": "empty",
}


def module():
    return importlib.import_module("digitalmodel.ansys.cylinder_resource_absence")


def initial(pid, parent, name, executable=None, created="99"):
    return {
        "pid": pid,
        "parent_pid": parent,
        "name": name,
        "executable_path": executable,
        "creation_time": created,
    }


def detail(pid, parent, name, executable, created="99"):
    return {
        **initial(pid, parent, name, executable, created),
        "executable_sha256": "a" * 64,
        "argv": [executable],
        "script_sources": [],
    }


def snapshot(initial_inventory=(), rows=(), *, observed="100", complete=True):
    initial_inventory, rows = list(initial_inventory), list(rows)
    return {
        "schema": "process-snapshot-1",
        "host": HOST,
        "observed_at": observed,
        "enumeration_complete": complete,
        "initial_inventory": initial_inventory,
        "rows": rows,
        "coverage": {
            "selector": "ansys-mpi-lineage-v1",
            "enumerated_count": len(initial_inventory),
            "selected_count": len(rows),
            "excluded_count": len(initial_inventory) - len(rows),
            "selected_details_complete": True,
            "limitation": "synthetic contract fixture",
        },
    }


def test_exact_absence_descriptor_is_recognized():
    assert module().validate_absence_descriptor(EMPTY_DESCRIPTOR, HOST) is True


@pytest.mark.parametrize("value", [None, {}, [], {"schema": "cfd-process-binding-2"}])
def test_unrelated_value_is_not_this_schema(value):
    assert module().validate_absence_descriptor(value, HOST) is False


@pytest.mark.parametrize(
    "mutation",
    [
        lambda value: value.pop("selector"),
        lambda value: value.update(extra=True),
        lambda value: value.update(host="OTHER"),
        lambda value: value.update(selector="other-selector"),
        lambda value: value.update(required_selected_state="unknown"),
        lambda value: value.update(host=True),
    ],
)
def test_malformed_absence_descriptor_refuses(mutation):
    value = deepcopy(EMPTY_DESCRIPTOR)
    mutation(value)
    with pytest.raises(ValueError):
        module().validate_absence_descriptor(value, HOST)


def test_empty_complete_snapshot_returns_plain_unbound_clear():
    result = module().verify_absence_snapshot(snapshot(), HOST, "100")
    assert result["status"] == "CLEAR"
    assert result["dispositions"] == []
    assert result["conflicts"] == []
    assert result["unknowns"] == []
    assert result["process_inventory"] == []
    assert "PRESERVED_CFD" not in repr(result)


def test_unrelated_python_controller_is_outside_selected_scope():
    controller = initial(27200, 8592, "python.exe", r"C:\Python\python.exe")
    result = module().verify_absence_snapshot(snapshot([controller]), HOST, "100")
    assert result["status"] == "CLEAR"
    assert result["dispositions"] == []


def test_unselected_windows_system_idle_pid_zero_is_permitted():
    idle = initial(0, 0, "System Idle Process")
    result = module().verify_absence_snapshot(snapshot([idle]), HOST, "100")
    assert result["status"] == "CLEAR"


def test_ansys_process_remains_a_conflict():
    base = initial(40, 4, "ANSYS261.exe", r"C:\ANSYS\bin\ANSYS261.exe")
    selected = detail(40, 4, "ANSYS261.exe", r"C:\ANSYS\bin\ANSYS261.exe")
    with pytest.raises(ValueError, match="CONFLICT"):
        module().verify_absence_snapshot(snapshot([base], [selected]), HOST, "100")


def test_mpi_process_remains_unknown_without_family_exemption():
    base = initial(50, 5, "mpiexec.exe", r"C:\MPI\mpiexec.exe")
    selected = detail(50, 5, "mpiexec.exe", r"C:\MPI\mpiexec.exe")
    with pytest.raises(ValueError, match="UNKNOWN"):
        module().verify_absence_snapshot(snapshot([base], [selected]), HOST, "100")


@pytest.mark.parametrize("name", ["interFoam.exe", "INTERFOAM.EXE"])
def test_orphan_interfoam_in_complete_inventory_refuses(name):
    orphan = initial(60, 6, name, r"C:\OpenFOAM\interFoam.exe")
    with pytest.raises(ValueError, match="interFoam"):
        module().verify_absence_snapshot(snapshot([orphan]), HOST, "100")


@pytest.mark.parametrize(
    "name,path",
    [
        ("ansys261.exe", r"C:\ANSYS\bin\ansys261.exe"),
        ("mpiexec.exe", r"C:\MPI\mpiexec.exe"),
    ],
)
def test_seed_present_only_in_initial_inventory_refuses(name, path):
    omitted = initial(65, 6, name, path)
    with pytest.raises(ValueError, match="seed"):
        module().verify_absence_snapshot(snapshot([omitted]), HOST, "100")


@pytest.mark.parametrize(
    "fault",
    [
        lambda value: value["initial_inventory"].append(
            deepcopy(value["initial_inventory"][0])
        ),
        lambda value: value["initial_inventory"][0].update(pid=True),
        lambda value: value["initial_inventory"][0].update(parent_pid=-1),
        lambda value: value["initial_inventory"][0].update(name=""),
        lambda value: value["initial_inventory"][0].update(extra="field"),
        lambda value: value["coverage"].update(enumerated_count=2),
    ],
)
def test_invalid_or_contradictory_initial_inventory_refuses(fault):
    value = snapshot([initial(70, 7, "python.exe", r"C:\Python\python.exe")])
    fault(value)
    with pytest.raises(ValueError):
        module().verify_absence_snapshot(value, HOST, "100")


def test_selected_row_must_exist_with_same_identity_in_initial_inventory():
    base = initial(80, 8, "mpiexec.exe", r"C:\MPI\mpiexec.exe")
    selected = detail(80, 9, "mpiexec.exe", r"C:\MPI\mpiexec.exe")
    with pytest.raises(ValueError):
        module().verify_absence_snapshot(snapshot([base], [selected]), HOST, "100")


def test_version_two_snapshot_cannot_dispatch_around_absence_checks():
    value = snapshot()
    value["schema"] = "process-snapshot-2"
    value["forwarders"] = []
    value["errors"] = []
    with pytest.raises(ValueError, match="process-snapshot-1"):
        module().verify_absence_snapshot(value, HOST, "100")


@pytest.mark.parametrize(
    "fault,now",
    [
        (lambda value: value.update(enumeration_complete=False), "100"),
        (lambda value: value["coverage"].update(selected_details_complete=False), "100"),
        (lambda value: value["coverage"].update(selector="other"), "100"),
        (lambda value: None, "131"),
        (lambda value: value.update(observed_at="101"), "100"),
    ],
)
def test_incomplete_changed_or_stale_snapshot_refuses(fault, now):
    value = snapshot()
    fault(value)
    with pytest.raises(ValueError):
        module().verify_absence_snapshot(value, HOST, now)


def test_verifier_calls_existing_classifier_with_none_binding(monkeypatch):
    seen = {}
    clear = {
        "status": "CLEAR",
        "dispositions": [],
        "conflicts": [],
        "unknowns": [],
        "process_inventory": [],
    }

    def classify(value, **kwargs):
        seen.update(kwargs)
        return deepcopy(clear)

    monkeypatch.setattr(module(), "classify_process_inventory", classify)
    assert module().verify_absence_snapshot(snapshot(), HOST, "100") == clear
    assert seen == {
        "expected_host": HOST,
        "cfd_binding": None,
        "now": "100",
        "maximum_age_seconds": "30",
    }
