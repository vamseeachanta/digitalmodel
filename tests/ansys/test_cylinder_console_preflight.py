"""Owner and classifier agreement is required before licence observation."""
import pytest

from digitalmodel.ansys import cylinder_diagnostic_preflight as module
from digitalmodel.ansys import cylinder_cfd_owner_evidence as owner
from tests.ansys.test_cylinder_diagnostic_preflight import prepared, _set_cfd_binding


@pytest.mark.parametrize('binding', [None, {'schema': 'cfd-process-binding-1'}])
def test_operation_supplement_requires_v2_binding(prepared, binding):
    config, approval, reservation, _, _ = prepared
    config['cfd_wrapper_console_evidence'] = {'id': 'source'}
    _set_cfd_binding(config, binding)
    with pytest.raises(ValueError, match='supplement'):
        module.ProductionPreflight(config, approval, reservation)._bindings()


@pytest.mark.parametrize('changed', ['owner', 'classification', 'missing'])
def test_console_disagreement_refuses_before_licence(prepared, monkeypatch, changed):
    config, approval, reservation, _, _ = prepared
    binding = {'schema': 'cfd-process-binding-2',
               'wrapper_console_supplement': {'id': 'source', 'sha256': 'a'*64, 'pids': [20, 21]}}
    _set_cfd_binding(config, binding)
    config['cfd_wrapper_console_evidence'] = {'id': 'source', 'path': 'source', 'sha256': 'a'*64}
    evidence = {'wrapper_console_pids': [20, 21]}
    classification = {'status': 'CLEAR', 'process_inventory': [], 'wrapper_console_pids': [20, 21]}
    if changed == 'owner':
        evidence['wrapper_console_pids'] = [20, 22]
    elif changed == 'classification':
        classification['wrapper_console_pids'] = [20, 22]
    else:
        classification.pop('wrapper_console_pids')
    monkeypatch.setattr(owner, 'resolve_owner_evidence', lambda *a: evidence.copy())
    monkeypatch.setattr(module, 'classify_process_inventory', lambda *a, **k: classification)
    monkeypatch.setattr(module, '_license_query', lambda *a: pytest.fail('licence before agreement'))
    collector = module.ProductionPreflight(config, approval, reservation)
    with pytest.raises(ValueError, match='console'):
        collector(approval)
    assert collector._ready_at is None


def test_console_owner_disagreement_on_recheck_refuses(prepared, monkeypatch):
    config, approval, reservation, _, _ = prepared
    binding = {'schema': 'cfd-process-binding-2',
               'wrapper_console_supplement': {'id': 'source', 'sha256': 'a'*64, 'pids': [20, 21]}}
    _set_cfd_binding(config, binding)
    config['cfd_wrapper_console_evidence'] = {'id': 'source', 'path': 'source', 'sha256': 'a'*64}
    evidence = {'wrapper_console_pids': [20, 21]}
    monkeypatch.setattr(owner, 'resolve_owner_evidence', lambda *a: evidence.copy())
    monkeypatch.setattr(module, 'classify_process_inventory', lambda *a, **k:
                        {'status': 'CLEAR', 'process_inventory': [], 'wrapper_console_pids': [20, 21]})
    collector = module.ProductionPreflight(config, approval, reservation)
    collector(approval)
    evidence['wrapper_console_pids'] = [20, 22]
    monkeypatch.setattr(module, '_license_query', lambda *a: pytest.fail('licence after mismatch'))
    with pytest.raises(ValueError, match='console'):
        collector.before_launch()
