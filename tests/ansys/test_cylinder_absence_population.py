"""RED tests for pressure-absence A/B/C population reconciliation."""
import importlib

import pytest


def module():
    return importlib.import_module("digitalmodel.ansys.cylinder_absence_population")


def row(pid, parent=4, name="python.exe", executable=r"C:\Python\python.exe",
        created="100"):
    return {"pid": pid, "parent_pid": parent, "name": name,
            "executable_path": executable, "creation_time": created}


def reconcile(a, b, c):
    inventories = [a, b, c]
    maps = [{item["pid"]: item["parent_pid"] for item in values}
            for values in inventories]
    return module().reconcile_observed_inventories(inventories, maps)


def test_verified_unrelated_addition_is_retained():
    result = reconcile([row(10)], [row(10)], [row(10), row(20)])
    assert result["events"] == [{
        "from_stage": "B", "to_stage": "C", "pid": 20,
        "change": "added", "disposition": "RETAINED_UNRELATED",
    }]


def test_verified_unrelated_exit_is_retained():
    result = reconcile([row(10), row(20)], [row(10)], [row(10)])
    assert result["events"][0]["pid"] == 20
    assert result["events"][0]["change"] == "removed"
    assert result["events"][0]["disposition"] == "RETAINED_UNRELATED"


def test_transient_unrelated_process_is_retained_in_both_events():
    result = reconcile([row(10)], [row(10), row(20)], [row(10)])
    assert [(item["change"], item["pid"]) for item in result["events"]] == [
        ("added", 20), ("removed", 20),
    ]


def test_verified_unrelated_parent_change_is_retained():
    result = reconcile([row(10, 4)], [row(10, 5)], [row(10, 5)])
    assert result["events"][0]["change"] == "parent_changed"
    assert result["events"][0]["disposition"] == "RETAINED_UNRELATED"


@pytest.mark.parametrize("name", ["ANSYS261.exe", "mpiexec.exe", "smpd.exe", "interFoam.exe"])
def test_relevant_addition_refuses(name):
    with pytest.raises(module().PopulationError, match="relevant") as caught:
        reconcile([row(10)], [row(10)], [row(10), row(20, name=name)])
    assert caught.value.evidence["events"][-1]["disposition"] == "REFUSED_RELEVANT"


def test_transient_b_seed_and_descendant_refuse():
    b = [row(10), row(20, name="ANSYS261.exe"), row(21, parent=20, name="worker.exe")]
    with pytest.raises(module().PopulationError, match="relevant") as caught:
        reconcile([row(10)], b, [row(10)])
    assert 20 in caught.value.evidence["relevant_pids"]["B"]
    assert 21 in caught.value.evidence["relevant_pids"]["B"]


def test_parent_transition_into_relevant_lineage_refuses():
    seed = row(30, name="ANSYS261.exe")
    with pytest.raises(module().PopulationError, match="relevant"):
        reconcile([seed, row(20, 4)], [seed, row(20, 30)], [seed, row(20, 30)])


@pytest.mark.parametrize(
    "changed",
    [
        row(10, created="101"),
        row(10, name="other.exe"),
        row(10, executable=r"C:\Other\other.exe"),
    ],
)
def test_reused_or_changed_identity_refuses(changed):
    with pytest.raises(module().PopulationError, match="identity"):
        reconcile([row(10)], [row(10)], [changed])


@pytest.mark.parametrize("incomplete", [row(20, name=" "), row(20, executable=None)])
def test_new_incomplete_identity_refuses(incomplete):
    with pytest.raises(module().PopulationError, match="complete"):
        reconcile([row(10)], [row(10)], [row(10), incomplete])


def test_unchanged_protected_missing_executable_remains_permitted():
    protected = row(820, name="Secure System", executable=None)
    result = reconcile([protected], [protected], [protected])
    assert result["events"] == []


def test_exited_row_requires_complete_unrelated_identity():
    protected = row(820, name="Secure System", executable=None)
    with pytest.raises(module().PopulationError, match="complete"):
        reconcile([protected], [], [])


def test_duplicate_pid_or_parent_map_mismatch_refuses():
    with pytest.raises(ValueError):
        reconcile([row(10), row(10)], [row(10)], [row(10)])
    with pytest.raises(ValueError):
        module().reconcile_observed_inventories(
            [[row(10)], [row(10)], [row(10)]], [{10: 4}, {10: 5}, {10: 4}],
        )
