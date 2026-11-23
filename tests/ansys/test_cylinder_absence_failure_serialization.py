"""Refused absence observations remain serializable; no process query is run."""
import pytest
from digitalmodel.ansys.analysis_records import canonical_bytes
from . import test_cylinder_absence_collection as fixtures


@pytest.mark.parametrize('phase', [0, 1, 2])
def test_unreadable_identity_failure_is_canonical(monkeypatch, phase):
    module = fixtures.module()
    count = []
    monkeypatch.setattr(module, 'read_windows_parent_map', lambda: {1: 0})
    monkeypatch.setattr(module.psutil, 'Process', lambda pid: pid)
    def identity(process, parents):
        count.append(process)
        if len(count) == phase + 1:
            raise ValueError('synthetic unreadable identity')
        return fixtures.initial(1, 0, 'ordinary.exe', 'C:/ordinary.exe', '0')
    monkeypatch.setattr(module, '_initial_identity', identity)
    monkeypatch.setattr(module, '_query_blank_names', lambda _: pytest.fail('query'))
    with pytest.raises(module.CollectionError) as caught:
        module.collect_absence_snapshot()
    value = caught.value.evidence
    assert canonical_bytes(value)
    failure = value['identity_read_failure']
    assert failure['stage'] == ('A', 'B', 'C')[phase]
    assert failure['failed_pid'] == 1
    assert failure['parent_map'] == [{'pid': 1, 'parent_pid': 0}]
    assert failure.get('prior_maps', []) == ([{'stage': 'A', 'rows': [{'pid': 1, 'parent_pid': 0}]}] if phase == 1 else [])
    assert len(value['parent_maps']) == phase + 1
