"""Synthetic retained-preparation accounting; no live solver or resource queries."""
from copy import deepcopy
import importlib
import json

import pytest

from digitalmodel.ansys.analysis_records import canonical_bytes, digest_bytes
from digitalmodel.ansys.cylinder_pressure_scope import INTERMEDIATE_SCOPE


def module():
    return importlib.import_module('digitalmodel.ansys.cylinder_preparation_history')


def pin(path, value):
    raw = canonical_bytes(value) if not isinstance(value, bytes) else value
    path.write_bytes(raw)
    return dict(path=str(path), sha256=digest_bytes(raw), bytes=len(raw))


@pytest.fixture
def history(tmp_path):
    runs = tmp_path/'runs'; runs.mkdir()
    streams = tmp_path/'_coordination'; streams.mkdir()
    old = runs/'ansys-2121-pressure-intermediate-20260917-r1'; old.mkdir()
    output = runs/'ansys-2121-pressure-intermediate-20260917-r2'
    ledger = runs/'ledger'; ledger.mkdir()
    parent = pin(ledger/('a'*64+'.json'), {'synthetic': 'parent'})
    config = dict(scope=deepcopy(INTERMEDIATE_SCOPE), operational=dict(output_directory=str(output)),
                  lineage=dict(original_root=str(runs/'zero'),
                               parent_claim={k: parent[k] for k in ('path', 'sha256')}))
    previous = deepcopy(config); previous['operational']['output_directory'] = str(old)
    original = pin(tmp_path/'previous.json', previous)
    refusal = dict(case_id='ocv-t60-p10-n8', consumed_count=2, native_launch_count=0,
        launch_adapter_calls=0, terminal_reason='PRECLAIM_REFUSAL', reservation_released=True,
        no_owned_processes_established=True, engineering_qualified=False,
        preparation_binding=dict(config_sha256=original['sha256'], ordinal=3,
                                 streams=dict(stdout=str(streams/'SOLVERS-intermediate-driver-20260917-r1.stdout.json'),
                                              stderr=str(streams/'SOLVERS-intermediate-driver-20260917-r1.stderr.txt')),
                                 output=str(old), parent_claim_sha256=parent['sha256']))
    files = [pin(old/'preparation-refusal.json', refusal), pin(old/'prefix-replay.json', b'123')]
    records = [pin(streams/'SOLVERS-intermediate-driver-20260917-r1.stdout.json', refusal),
               pin(streams/'SOLVERS-intermediate-driver-20260917-r1.stderr.txt', b'')]
    config['preparation_history'] = dict(schema='cylinder-preparation-history-1',
        stream_directory=str(streams), preparations=[dict(root=str(old), config=original,
                                                          files=files, streams=records)])
    return config


def test_complete_history_returns_storage_declarations(history):
    result = module().validate_preparation_history(history)
    entry = history['preparation_history']['preparations'][0]
    assert result['prior_capture_roots'] == [entry['root']]
    assert result['supplemental_files'] == [x['path'] for x in entry['streams']]
    assert result['retained_bytes'] == sum(x['bytes'] for x in entry['files']+entry['streams'])


@pytest.mark.parametrize('fault', ['missing_history', 'omitted_root', 'omitted_file',
    'omitted_stream', 'extra_file', 'changed_file', 'changed_config', 'extra_stream',
    'boolean_launch', 'nonzero_launch', 'unreleased', 'ledger', 'wrong_output'])
def test_incomplete_or_unsettled_history_refuses(history, fault):
    from pathlib import Path
    descriptor = history['preparation_history']; entry = descriptor['preparations'][0]
    if fault == 'missing_history':
        del history['preparation_history']
    elif fault == 'omitted_root':
        descriptor['preparations'] = []
    elif fault == 'omitted_file':
        entry['files'].pop()
    elif fault == 'omitted_stream':
        entry['streams'].pop()
    elif fault == 'extra_file':
        (Path(entry['root'])/'unlisted').write_bytes(b'x')
    elif fault == 'changed_file':
        Path(entry['files'][1]['path']).write_bytes(b'456')
    elif fault == 'changed_config':
        Path(entry['config']['path']).write_bytes(b'{}')
    elif fault == 'extra_stream':
        (Path(descriptor['stream_directory'])/'SOLVERS-intermediate-driver-orphan.stderr.txt').write_bytes(b'x')
    elif fault == 'ledger':
        parent = Path(history['lineage']['parent_claim']['path'])
        parent.with_name(parent.stem+'.ordinal-3.json').write_bytes(b'{}')
    elif fault == 'wrong_output':
        previous = json.loads(Path(entry['config']['path']).read_bytes())
        previous['operational']['output_directory'] = str(Path(entry['root']).parent/'wrong')
        entry['config'] = pin(Path(entry['config']['path']), previous)
    else:
        value = json.loads(Path(entry['files'][0]['path']).read_bytes())
        if fault == 'boolean_launch': value['native_launch_count'] = False
        elif fault == 'nonzero_launch': value['native_launch_count'] = 1
        else: value['reservation_released'] = False
        entry['files'][0] = pin(Path(entry['files'][0]['path']), value)
        entry['streams'][0] = pin(Path(entry['streams'][0]['path']), value)
    with pytest.raises(ValueError):
        module().validate_preparation_history(history)


def test_membership_is_rechecked_after_initial_validation(history):
    from pathlib import Path
    module().validate_preparation_history(history)
    entry = history['preparation_history']['preparations'][0]
    (Path(entry['root'])/'late-file').write_bytes(b'x')
    with pytest.raises(ValueError):
        module().validate_preparation_history(history)


def test_storage_supplemental_files_are_counted(tmp_path):
    from .test_cylinder_pressure_resources import storage, module as resources
    args = storage(tmp_path); extra = tmp_path/'driver-output'; extra.write_bytes(b'12345')
    result = resources().cumulative_storage(*args, free_bytes=4*1024**3,
                                           supplemental_files=[extra])
    assert result['total_bytes'] == 10174192+15
    assert any(r['path'].startswith('supplemental-') for r in result['files'])


def test_storage_supplemental_alias_refuses(tmp_path):
    from .test_cylinder_pressure_resources import storage, module as resources
    args = storage(tmp_path)
    with pytest.raises(ValueError):
        resources().cumulative_storage(*args, free_bytes=4*1024**3,
                                       supplemental_files=[args[1]])


def test_postclaim_history_recheck_preserves_owned_ledger(history):
    from pathlib import Path
    parent = Path(history['lineage']['parent_claim']['path'])
    path = parent.with_name(parent.stem+'.ordinal-3.json'); path.write_bytes(b'{}')
    assert module().validate_preparation_history(history, require_unclaimed=False)['retained_bytes'] > 0
    assert path.read_bytes() == b'{}'


def test_pinned_hardlink_is_refused(history, tmp_path):
    import os
    from pathlib import Path
    original = Path(history['preparation_history']['preparations'][0]['files'][1]['path'])
    os.link(original, tmp_path/'alias')
    with pytest.raises(ValueError): module().validate_preparation_history(history)


def test_oversized_metadata_is_refused_before_read(history, monkeypatch):
    from pathlib import Path
    entry = history['preparation_history']['preparations'][0]
    with Path(entry['files'][1]['path']).open('wb') as out:
        out.truncate(16*1024**2+1)
    entry['files'][1]['bytes'] = 16*1024**2+1
    with pytest.raises(ValueError, match='bound'):
        module().validate_preparation_history(history)


def test_output_relocation_cannot_hide_preparation_history(history, tmp_path):
    other = tmp_path/'other'; other.mkdir()
    history['operational']['output_directory'] = str(other/'fresh')
    del history['preparation_history']
    with pytest.raises(ValueError, match='namespace'):
        module().validate_preparation_history(history)


def test_empty_history_still_requires_unclaimed_ordinal(tmp_path):
    parent = tmp_path/('a'*64+'.json'); parent.write_bytes(b'{}')
    parent.with_name(parent.stem+'.ordinal-3.json').write_bytes(b'{}')
    config = dict(scope=deepcopy(INTERMEDIATE_SCOPE),
        operational=dict(output_directory=str(tmp_path/'fresh')),
        lineage=dict(original_root=str(tmp_path/'zero'),
                     parent_claim=dict(path=str(parent), sha256=digest_bytes(b'{}'))))
    with pytest.raises(ValueError, match='ledger'):
        module().validate_preparation_history(config)


def test_replica_stream_directory_cannot_replace_originals(history, tmp_path):
    import shutil
    from pathlib import Path
    descriptor = history['preparation_history']; entry = descriptor['preparations'][0]
    replica = tmp_path/'replica'; replica.mkdir()
    for row in entry['streams']:
        target = replica/Path(row['path']).name
        shutil.copyfile(row['path'], target); row['path'] = str(target)
    descriptor['stream_directory'] = str(replica)
    with pytest.raises(ValueError, match='stream.*location'):
        module().validate_preparation_history(history)


def test_orphan_streams_require_history_even_without_preparation_roots(tmp_path):
    runs = tmp_path/'runs'; runs.mkdir()
    streams = tmp_path/'_coordination'; streams.mkdir()
    (streams/'SOLVERS-intermediate-driver-orphan.stdout.json').write_bytes(b'{}')
    parent = runs/('a'*64+'.json'); parent.write_bytes(b'{}')
    config = dict(scope=deepcopy(INTERMEDIATE_SCOPE),
        operational=dict(output_directory=str(runs/'fresh')),
        lineage=dict(original_root=str(runs/'zero'),
                     parent_claim=dict(path=str(parent), sha256=digest_bytes(b'{}'))))
    with pytest.raises(ValueError, match='stream'):
        module().validate_preparation_history(config)


def test_case_variant_orphan_stream_is_detected(history):
    from pathlib import Path
    root = Path(history['preparation_history']['stream_directory'])
    (root/'Solvers-Intermediate-Driver-orphan.STDOUT.JSON').write_bytes(b'{}')
    with pytest.raises(ValueError, match='stream'):
        module().validate_preparation_history(history)


def test_transplanted_refusal_does_not_match_configuration(history):
    from pathlib import Path
    entry = history['preparation_history']['preparations'][0]
    value = json.loads(Path(entry['files'][0]['path']).read_bytes())
    value['preparation_binding']['config_sha256'] = 'b'*64
    entry['files'][0] = pin(Path(entry['files'][0]['path']), value)
    entry['streams'][0] = pin(Path(entry['streams'][0]['path']), value)
    with pytest.raises(ValueError, match='binding'):
        module().validate_preparation_history(history)


def test_unrecognized_legacy_refusal_is_rejected(history):
    from pathlib import Path
    entry = history['preparation_history']['preparations'][0]
    value = json.loads(Path(entry['files'][0]['path']).read_bytes())
    del value['preparation_binding']
    entry['files'][0] = pin(Path(entry['files'][0]['path']), value)
    entry['streams'][0] = pin(Path(entry['streams'][0]['path']), value)
    with pytest.raises(ValueError, match='legacy'):
        module().validate_preparation_history(history)


@pytest.mark.parametrize('fault', ['stderr', 'secondary_reason', 'accounting_error', 'stream_binding'])
def test_incomplete_terminal_evidence_refuses(history, fault):
    from pathlib import Path
    entry = history['preparation_history']['preparations'][0]
    value = json.loads(Path(entry['files'][0]['path']).read_bytes())
    if fault == 'stderr':
        entry['streams'][1] = pin(Path(entry['streams'][1]['path']), b'traceback')
    else:
        if fault == 'secondary_reason':
            value['terminal_reasons'] = ['PRECLAIM_REFUSAL', 'FINAL_ACCOUNTING_INCOMPLETE']
        elif fault == 'accounting_error':
            value['final_accounting_error'] = 'failed'
        else:
            value['preparation_binding']['streams'] = {'stdout': 'replica', 'stderr': 'replica'}
        entry['files'][0] = pin(Path(entry['files'][0]['path']), value)
        entry['streams'][0] = pin(Path(entry['streams'][0]['path']), value)
    with pytest.raises(ValueError):
        module().validate_preparation_history(history)
