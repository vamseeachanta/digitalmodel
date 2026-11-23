"""Collector population-retention integration; no native calls."""
from copy import deepcopy
import pytest
from .test_cylinder_absence_collection import initial, install_observation, module


def test_final_identity_read_failure_retains_map_and_partial_rows(monkeypatch):
    rows = [initial(10, 4, "python.exe", r"C:\Python\python.exe")]
    install_observation(monkeypatch, rows)
    failure = {
        "stage": "C", "failed_pid": 20, "rows": deepcopy(rows),
        "parent_map": {10: 4, 20: 4},
    }

    def fail():
        raise module().PopulationReadError("Process 20 identity unreadable", failure)

    monkeypatch.setattr(module(), "_collect_final_population", fail)
    with pytest.raises(module().CollectionError) as caught:
        module().collect_absence_snapshot()
    expected = dict(failure, parent_map=[
        {"pid": 10, "parent_pid": 4}, {"pid": 20, "parent_pid": 4}])
    assert caught.value.evidence["identity_read_failure"] == expected
    assert failure["parent_map"] == {10: 4, 20: 4}
    from digitalmodel.ansys.analysis_records import canonical_bytes
    assert canonical_bytes(caught.value.evidence)
    assert caught.value.evidence["original_observed_inventories"][-1] == rows
    assert caught.value.evidence["parent_maps"][-1]["stage"] == "C"
    assert caught.value.evidence["parent_maps"][-1]["rows"][-1] == {
        "pid": 20, "parent_pid": 4,
    }


def test_detail_failure_retains_completed_population_dispositions(monkeypatch):
    seed = initial(40, 4, "ANSYS261.exe", r"C:\ANSYS\bin\ANSYS261.exe")
    unrelated = initial(90, 4, "calc.exe", r"C:\Tools\calc.exe")
    install_observation(monkeypatch, [seed])
    monkeypatch.setattr(
        module(), "_collect_final_population",
        lambda: ([deepcopy(seed), deepcopy(unrelated)], {40: 4, 90: 4}),
    )
    monkeypatch.setattr(
        module(), "_details",
        lambda row, cache: (_ for _ in ()).throw(ValueError("detail reread failed")),
    )
    with pytest.raises(module().CollectionError, match="detail reread failed") as caught:
        module().collect_absence_snapshot()
    assert caught.value.evidence["population_check"]["events"] == [{
        "from_stage": "B", "to_stage": "C", "pid": 90,
        "change": "added", "disposition": "RETAINED_UNRELATED",
    }]


def test_final_population_drives_inventory_selection_and_coverage(monkeypatch):
    a = [initial(10, 4, "python.exe", r"C:\Python\python.exe")]
    c = a + [initial(20, 4, "calc.exe", r"C:\Tools\calc.exe")]
    install_observation(monkeypatch, a)
    monkeypatch.setattr(module(), "_collect_final_population", lambda: (deepcopy(c), {10: 4, 20: 4}))
    value = module().collect_absence_snapshot()
    assert value["original_initial_inventory"] == a
    assert value["initial_inventory"] == c
    assert value["observed_inventories"] == [a, a, c]
    assert value["coverage"]["enumerated_count"] == 2
    assert value["coverage"]["selected_count"] == 0
    assert value["coverage"]["excluded_count"] == 2


def test_absence_collector_does_not_call_legacy_v2_enumerator(monkeypatch):
    rows = [initial(10, 4, "python.exe", r"C:\Python\python.exe")]
    install_observation(monkeypatch, rows)
    monkeypatch.setattr(
        module(), "enumerate_windows_v2", lambda: pytest.fail("legacy active-v2 path called"),
        raising=False,
    )
    assert module().collect_absence_snapshot()["coverage"]["enumerated_count"] == 1


def test_powershell_identity_requires_regular_executable_and_records_digest(monkeypatch, tmp_path):
    executable = tmp_path / "System32/WindowsPowerShell/v1.0/powershell.exe"
    executable.parent.mkdir(parents=True)
    executable.write_bytes(b"pinned executable")
    monkeypatch.setenv("SystemRoot", str(tmp_path))
    monkeypatch.setattr(module().shutil, "which", lambda name: str(executable))
    path, digest = module()._powershell_identity()
    assert path == str(executable)
    assert digest == module().hashlib.sha256(b"pinned executable").hexdigest()

    monkeypatch.setattr(module().shutil, "which", lambda name: str(tmp_path))
    with pytest.raises(ValueError, match="executable"):
        module()._powershell_identity()
