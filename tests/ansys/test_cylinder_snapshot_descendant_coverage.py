"""Unpinned children must remain visible to resource classification."""
from digitalmodel.ansys.cylinder_diagnostic_snapshot_v2 import _selection
from digitalmodel.ansys import cylinder_diagnostic_snapshot as legacy


def test_pinned_controller_selection_includes_unpinned_descendants():
    rows = [
        dict(pid=1, parent_pid=0, name='python.exe', executable_path='C:/python.exe'),
        dict(pid=2, parent_pid=1, name='python.exe', executable_path='C:/python.exe'),
        dict(pid=3, parent_pid=2, name='worker.exe', executable_path='C:/worker.exe'),
        dict(pid=4, parent_pid=0, name='python.exe', executable_path='C:/python.exe'),
    ]
    selected = _selection(rows, {1: '1'})
    assert [row['pid'] for row in selected] == [1, 2, 3]


def test_legacy_controller_selection_includes_unpinned_descendants(monkeypatch):
    rows = [
        dict(pid=1, parent_pid=0, name='python.exe', executable_path='C:/python.exe'),
        dict(pid=2, parent_pid=1, name='python.exe', executable_path='C:/python.exe'),
        dict(pid=3, parent_pid=2, name='worker.exe', executable_path='C:/worker.exe'),
        dict(pid=4, parent_pid=0, name='python.exe', executable_path='C:/python.exe'),
    ]
    monkeypatch.setattr(legacy, '_enumerate', lambda: rows)
    monkeypatch.setattr(legacy, '_details', lambda row, cache: row.copy())
    result = legacy.collect_process_snapshot({'processes': [{'pid': 1}]})
    assert [row['pid'] for row in result['rows']] == [1, 2, 3]


def test_v2_duplicate_forwarded_child_refuses_before_collection(monkeypatch):
    from digitalmodel.ansys import cylinder_diagnostic_snapshot_v2 as module
    import pytest

    monkeypatch.setattr(module.socket, 'gethostname', lambda: 'synthetic')
    seed = dict(schema='process-discovery-seed-2', host='synthetic',
                selected_processes=[dict(pid=i, creation_time=str(i)) for i in (1, 2, 3)],
                forwarder_candidates=[dict(parent_pid=i, parent_creation_time=str(i),
                    child_pid=3, child_creation_time='3', declared_target_alias='C:/alias/python.exe')
                    for i in (1, 2)])
    with pytest.raises(ValueError, match='child'):
        module._configuration(None, seed)
