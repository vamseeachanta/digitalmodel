"""Synthetic immutable-bundle tests; no native/provider/process invocation."""
import json
import sys
from pathlib import Path
from types import SimpleNamespace

import pytest

from digitalmodel.ansys import cylinder_runtime_bundle as module
from digitalmodel.ansys.analysis_records import canonical_bytes, digest_bytes
from digitalmodel.ansys.cylinder_canary import ORDER, _manifest


@pytest.fixture
def setup_bundle(tmp_path, monkeypatch):
    (tmp_path / '.git').mkdir()
    source = tmp_path / 'examples/ansys/cylinder-benchmark'
    source.mkdir(parents=True)
    names = ['checker_brief.txt', 'checker_capture.json', 'checker_contract.json',
             'checker_invocation.json', 'checker_process_stderr.txt',
             'checker_response.json', 'checker_transport.json', 'producer_derivation.json',
             'reference.json', 'reference_comparison.json', 'prepared/basis-criteria.json',
             'prepared/preparation.json', 'prepared/proposal.json']
    names += [f'prepared/{case}.{ext}' for case in ORDER for ext in ('inp', 'json')]
    artifacts = []
    for name in names:
        path = source / name
        path.parent.mkdir(exist_ok=True)
        raw = name.encode()
        path.write_bytes(raw)
        artifacts.append({'path': name, 'sha256': digest_bytes(raw), 'bytes': len(raw)})
    runtime = tmp_path / 'src/digitalmodel/ansys/cylinder_fake.py'
    runtime.parent.mkdir(parents=True)
    runtime.write_bytes(b'# synthetic source\n')
    inventory = [{'path': 'src/digitalmodel/ansys/cylinder_fake.py',
                  'sha256': digest_bytes(runtime.read_bytes())}]
    manifest = {'schema': 'cylinder-b1-1', 'case_order': list(ORDER),
                'cases': [{'case_id': case, 'deck': f'prepared/{case}.inp',
                           'metadata': f'prepared/{case}.json'} for case in ORDER],
                'artifacts': artifacts, 'reference': 'reference.json',
                'runtime_sources': [{**inventory[0], 'sha256': '0' * 64}]}
    (source / 'manifest.json').write_bytes(canonical_bytes(manifest))
    monkeypatch.setattr(module, '_owning_root', lambda: tmp_path)
    monkeypatch.setattr(module, 'runtime_sources', lambda: inventory)
    monkeypatch.setattr(module, '_git_blob', lambda *args: b'# synthetic source\n')
    pins = {'original_manifest_sha256': digest_bytes((source / 'manifest.json').read_bytes()),
            'runtime_inventory_sha256': digest_bytes(canonical_bytes(inventory)),
            'source_revision': 'a' * 40}
    return tmp_path, source, inventory, pins


def test_success_preserves_original_and_all_artifacts(setup_bundle, monkeypatch):
    root, source, inventory, pins = setup_bundle
    original = (source / 'manifest.json').read_bytes()
    result = module.prepare_runtime_bundle('reviewed-1', **pins)
    output = root / result['bundle_path']
    assert (source / 'manifest.json').read_bytes() == original
    successor = json.loads((output / 'manifest.json').read_bytes())
    baseline = json.loads(original)
    assert successor == {**baseline, 'runtime_sources': inventory,
                         'runtime_lineage': pins}
    assert len(successor['artifacts']) == 21
    for row in successor['artifacts']:
        assert (output / row['path']).read_bytes() == (source / row['path']).read_bytes()
    monkeypatch.setattr('digitalmodel.ansys.cylinder_canary.runtime_sources', lambda: inventory)
    assert _manifest(output, {'manifest_sha256': result['manifest_sha256']}) == successor
    with pytest.raises(ValueError, match='differs from B2 approval'):
        _manifest(output, {'manifest_sha256': pins['original_manifest_sha256']})


@pytest.mark.parametrize('label', ['', '../escape', 'a/b', 'a\\b', '.', 'CON', 'nul', 'a.', '/root'])
def test_label_refusal_before_copy(setup_bundle, label):
    root, _, _, pins = setup_bundle
    with pytest.raises(ValueError):
        module.prepare_runtime_bundle(label, **pins)
    assert not (root / 'examples/ansys/cylinder-runtime').exists()


@pytest.mark.parametrize('pin', ['original_manifest_sha256', 'runtime_inventory_sha256'])
def test_wrong_pins_refuse(setup_bundle, pin):
    root, _, _, pins = setup_bundle
    pins[pin] = '0' * 64
    with pytest.raises(ValueError):
        module.prepare_runtime_bundle('wrong', **pins)
    assert not (root / 'examples/ansys/cylinder-runtime/wrong/manifest.json').exists()


def test_source_revision_bytes_must_match(setup_bundle, monkeypatch):
    root, _, _, pins = setup_bundle
    monkeypatch.setattr(module, '_git_blob', lambda *args: b'# different')
    with pytest.raises(ValueError, match='Git'):
        module.prepare_runtime_bundle('drift', **pins)
    assert not (root / 'examples/ansys/cylinder-runtime/drift/manifest.json').exists()


@pytest.mark.parametrize('fault', ['missing_capture', 'duplicate', 'traversal', 'wrong_digest'])
def test_malformed_original_refuses(setup_bundle, fault):
    _, source, _, pins = setup_bundle
    path = source / 'manifest.json'
    manifest = json.loads(path.read_bytes())
    if fault == 'missing_capture':
        manifest['artifacts'] = [r for r in manifest['artifacts'] if r['path'] != 'checker_capture.json']
    elif fault == 'duplicate':
        manifest['artifacts'][-1] = manifest['artifacts'][0]
    elif fault == 'traversal':
        manifest['artifacts'][-1]['path'] = '../escape'
    else:
        manifest['artifacts'][0]['sha256'] = '0' * 64
    path.write_bytes(canonical_bytes(manifest))
    pins['original_manifest_sha256'] = digest_bytes(path.read_bytes())
    with pytest.raises(ValueError):
        module.prepare_runtime_bundle('invalid', **pins)


def test_existing_destination_never_overwritten(setup_bundle):
    root, _, _, pins = setup_bundle
    target = root / 'examples/ansys/cylinder-runtime/used'
    target.mkdir(parents=True)
    with pytest.raises(FileExistsError):
        module.prepare_runtime_bundle('used', **pins)


def test_copy_time_drift_preserves_partial_without_manifest(setup_bundle, monkeypatch):
    root, source, _, pins = setup_bundle
    original_write = module._write
    def drift(path, raw):
        result = original_write(path, raw)
        if path.name == 'checker_brief.txt':
            (source / 'checker_capture.json').write_bytes(b'changed')
        return result
    monkeypatch.setattr(module, '_write', drift)
    with pytest.raises(ValueError):
        module.prepare_runtime_bundle('partial', **pins)
    target = root / 'examples/ansys/cylinder-runtime/partial'
    assert not (target / 'manifest.json').exists()
    assert (target / 'preparation-failure.json').exists()


def test_inventory_hash_mutation_has_explicit_refusal(setup_bundle):
    _, _, inventory, pins = setup_bundle
    inventory[0]['sha256'] = '0' * 64
    with pytest.raises(ValueError, match='Runtime inventory differs from reviewed pin'):
        module.prepare_runtime_bundle('changed-inventory', **pins)


@pytest.mark.parametrize('predicate', ['is_symlink', 'is_junction'])
@pytest.mark.parametrize('location', ['source', 'destination'])
def test_redirected_ancestor_refuses_without_copy(setup_bundle, monkeypatch, predicate, location):
    root, source, _, pins = setup_bundle
    target = source if location == 'source' else root / 'examples/ansys/cylinder-runtime'
    original_check = getattr(Path, predicate)
    monkeypatch.setattr(Path, predicate,
                        lambda path: path == target or original_check(path))
    with pytest.raises(ValueError, match='Redirected'):
        module.prepare_runtime_bundle('redirected', **pins)
    assert not (root / 'examples/ansys/cylinder-runtime/redirected').exists()


@pytest.mark.parametrize('changed', ['runtime', 'manifest', 'artifact'])
def test_final_copy_drift_prevents_manifest_publication(setup_bundle, monkeypatch, changed):
    root, source, inventory, pins = setup_bundle
    original = (source / 'manifest.json').read_bytes()
    last = json.loads(original)['artifacts'][-1]['path']
    writer = module._write
    def mutate_after_last_copy(path, raw):
        writer(path, raw)
        if str(path).replace('\\', '/').endswith('/' + last):
            target = {'runtime': root / inventory[0]['path'],
                      'manifest': source / 'manifest.json',
                      'artifact': source / 'checker_capture.json'}[changed]
            target.write_bytes(target.read_bytes() + b'\n')
    monkeypatch.setattr(module, '_write', mutate_after_last_copy)
    with pytest.raises(ValueError):
        module.prepare_runtime_bundle('last-copy-drift', **pins)
    target = root / 'examples/ansys/cylinder-runtime/last-copy-drift'
    assert not (target / 'manifest.json').exists()
    assert (target / 'preparation-failure.json').exists()
    if changed != 'manifest':
        assert (source / 'manifest.json').read_bytes() == original


def test_output_root_redirection_after_first_copy_refuses(setup_bundle, monkeypatch):
    root, _, _, pins = setup_bundle
    target = root / 'examples/ansys/cylinder-runtime/redirected-late'
    state = {'redirected': False}
    writer = module._write
    original_check = Path.is_junction
    monkeypatch.setattr(Path, 'is_junction', lambda p: (p == target and state['redirected']) or original_check(p))
    def redirect_after_first(path, raw):
        writer(path, raw)
        state['redirected'] = True
    monkeypatch.setattr(module, '_write', redirect_after_first)
    with pytest.raises(ValueError, match='Redirected'):
        module.prepare_runtime_bundle('redirected-late', **pins)
    assert not (target / 'manifest.json').exists()


def test_publication_never_replaces_existing_manifest(tmp_path):
    pending = tmp_path / 'manifest.pending.json'
    pending.write_bytes(b'new')
    final = tmp_path / 'manifest.json'
    final.write_bytes(b'preserved')
    with pytest.raises(FileExistsError):
        module._publish_manifest(tmp_path, pending)
    assert final.read_bytes() == b'preserved'
    assert pending.read_bytes() == b'new'


@pytest.mark.parametrize('kind_code,kind,show_code', [(1, b'', 0), (0, b'tree', 0), (0, b'commit', 1), (0, b'commit', 0)])
def test_git_binding_subprocess_contract(tmp_path, monkeypatch, kind_code, kind, show_code):
    calls = []
    def read_only_git(argv, **kwargs):
        calls.append(argv)
        assert argv[:3] == ['git', '-C', str(tmp_path)]
        return (SimpleNamespace(returncode=kind_code, stdout=kind) if argv[3] == 'cat-file'
                else SimpleNamespace(returncode=show_code, stdout=b'exact-source'))
    monkeypatch.setattr(module.subprocess, 'run', read_only_git)
    if kind_code or kind != b'commit' or show_code:
        with pytest.raises(ValueError):
            module._git_blob(tmp_path, 'a' * 40, 'src/example.py')
    else:
        assert module._git_blob(tmp_path, 'a' * 40, 'src/example.py') == b'exact-source'
    assert len(calls) == (1 if kind_code or kind != b'commit' else 2)


def test_pending_alias_unlink_failure_preserves_published_bundle(setup_bundle, monkeypatch):
    root, source, inventory, pins = setup_bundle
    original = (source / 'manifest.json').read_bytes()
    baseline = json.loads(original)
    target = root / 'examples/ansys/cylinder-runtime/retained-alias'
    pending = target / 'manifest.pending.json'
    final = target / 'manifest.json'
    unlink = Path.unlink
    observed = []

    def fail_only_published_alias(path, *args, **kwargs):
        if path == pending:
            assert final.exists()
            assert path.samefile(final)
            observed.append(path.read_bytes())
            raise PermissionError('injected pending-alias cleanup failure')
        return unlink(path, *args, **kwargs)

    monkeypatch.setattr(Path, 'unlink', fail_only_published_alias)
    result = module.prepare_runtime_bundle('retained-alias', **pins)
    expected = canonical_bytes({**baseline, 'runtime_sources': inventory,
                                'runtime_lineage': pins})
    assert observed == [expected]
    assert result['pending_manifest_alias_retained'] is True
    assert result['manifest_sha256'] == digest_bytes(expected)
    assert final.read_bytes() == pending.read_bytes() == expected
    assert (source / 'manifest.json').read_bytes() == original
    assert len(baseline['artifacts']) == 21
    for row in baseline['artifacts']:
        raw = (target / row['path']).read_bytes()
        assert raw == (source / row['path']).read_bytes()
        assert digest_bytes(raw) == row['sha256']
    assert not (target / 'preparation-failure.json').exists()


def test_runtime_inventory_covers_imported_base_runner():
    from digitalmodel.ansys import cylinder_runner

    root = Path(cylinder_runner.__file__).resolve().parents[3]
    runner = root / 'src/digitalmodel/ansys/runner.py'
    assert cylinder_runner.ANSYSRunner.__module__ == 'digitalmodel.ansys.runner'
    inventory = module.runtime_sources()
    rows = [row for row in inventory
            if row['path'] == 'src/digitalmodel/ansys/runner.py']
    assert rows == [{'path': 'src/digitalmodel/ansys/runner.py',
                     'sha256': digest_bytes(runner.read_bytes())}]


def test_runtime_inventory_covers_every_immediate_package_source():
    from digitalmodel.ansys import cylinder_canary

    directory = Path(cylinder_canary.__file__).resolve().parent
    expected = [{'path': 'src/digitalmodel/ansys/' + path.name,
                 'sha256': digest_bytes(path.read_bytes())}
                for path in sorted(directory.glob('*.py'))]
    assert module.runtime_sources() == expected
    assert any(row['path'].endswith('/__init__.py') for row in expected)


@pytest.mark.parametrize('name,origin', [
    ('digitalmodel.ansys.unlisted_extension', 'unlisted.py'),
    ('digitalmodel.ansys.runner', 'runner.py'),
    ('digitalmodel.ansys', '__init__.py'),
    ('digitalmodel.ansys.nested.runner', 'runner.py'),
])
def test_loaded_package_module_outside_inventory_refuses(tmp_path, monkeypatch, name, origin):
    other = tmp_path / origin
    other.write_bytes(b'# untrusted source')
    monkeypatch.setitem(sys.modules, name, SimpleNamespace(__file__=str(other)))
    with pytest.raises(ValueError, match='inventory|runtime|source'):
        module.runtime_sources()


def test_unlisted_module_cannot_borrow_inventoried_file_origin(monkeypatch):
    from digitalmodel.ansys import cylinder_canary

    monkeypatch.setitem(sys.modules, 'digitalmodel.ansys.unlisted_extension',
                        SimpleNamespace(__file__=cylinder_canary.__file__))
    with pytest.raises(ValueError, match='inventory|runtime|source'):
        module.runtime_sources()
