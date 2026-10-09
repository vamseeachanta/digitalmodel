"""Synthetic intermediate mesh (N8)/coarse mesh (N4) journal and storage tests."""
import os
from pathlib import Path
import pytest
from digitalmodel.ansys.analysis_records import canonical_bytes, digest_bytes
from digitalmodel.ansys.cylinder_pressure_journal import PressureJournal
from digitalmodel.ansys import cylinder_pressure_resources as storage


@pytest.fixture
def lineage(tmp_path):
    parent = tmp_path / ('a' * 64 + '.json')
    parent.write_bytes(b'original')
    sha = digest_bytes(parent.read_bytes())
    prior = tmp_path / ('a' * 64 + '.ordinal-2.json')
    prior.write_bytes(canonical_bytes(dict(ordinal=2, state='attempt_consumed',
        parent_sha256=sha, case_id='ocv-t60-p10-n4')))
    return parent, sha, dict(path=str(prior), sha256=digest_bytes(prior.read_bytes()))


def journal(lineage):
    parent, sha, prior = lineage
    return PressureJournal(parent, sha, ordinal=3, predecessor=prior)


def test_intermediate_names_and_no_restart(lineage):
    parent, sha, prior = lineage
    before = parent.read_bytes(), open(prior['path'], 'rb').read()
    item = journal(lineage)
    assert item.paths['claim'].name == parent.stem + '.ordinal-3.json'
    item.start({'ordinal': 3}); item.claim({'ordinal': 3})
    assert item.consumed
    item.terminal({'settled': True})
    with pytest.raises(FileExistsError): journal(lineage)
    assert before == (parent.read_bytes(), open(prior['path'], 'rb').read())


@pytest.mark.parametrize('ordinal', [True, False, 1, 4, '3', 3.0])
def test_unknown_or_untyped_ordinal_refused(lineage, ordinal):
    with pytest.raises(ValueError):
        PressureJournal(lineage[0], lineage[1], ordinal=ordinal)


@pytest.mark.parametrize('fault', ['missing', 'wrong_path', 'digest', 'state', 'ordinal', 'parent', 'case', 'hardlink'])
def test_predecessor_refusal(lineage, tmp_path, fault):
    parent, sha, prior = lineage
    path = Path(prior['path'])
    if fault == 'missing': prior = None
    elif fault == 'wrong_path': prior['path'] = str(parent)
    elif fault == 'digest': prior['sha256'] = '0' * 64
    elif fault == 'hardlink': os.link(path, tmp_path / 'alias')
    else:
        import json
        row = json.loads(path.read_bytes())
        key = {'state':'state','ordinal':'ordinal','parent':'parent_sha256','case':'case_id'}[fault]
        row[key] = 'wrong'
        path.write_bytes(canonical_bytes(row)); prior['sha256'] = digest_bytes(path.read_bytes())
    with pytest.raises(ValueError): PressureJournal(parent, sha, ordinal=3, predecessor=prior)


@pytest.mark.parametrize('transition', ['start', 'claim', 'terminal'])
def test_predecessor_rechecked_each_transition(lineage, transition):
    item = journal(lineage)
    if transition != 'start': item.start({})
    if transition == 'terminal': item.claim({})
    Path(lineage[2]['path']).write_bytes(b'changed')
    with pytest.raises(ValueError): getattr(item, transition)({})


@pytest.mark.parametrize('suffix', ['.invocation.json', '.json', '.terminal.json', '.partial'])
def test_any_intermediate_residue_refuses(lineage, suffix):
    parent = lineage[0]
    parent.with_name(parent.stem + '.ordinal-3' + suffix).write_bytes(b'partial')
    with pytest.raises(FileExistsError): journal(lineage)


def test_default_coarse_behavior_unchanged(lineage):
    parent, sha, prior = lineage
    Path(prior['path']).unlink()
    item = PressureJournal(parent, sha)
    assert item.paths['claim'].name == parent.stem + '.ordinal-2.json'
    with pytest.raises(ValueError): PressureJournal(parent, sha, predecessor=prior)


@pytest.fixture
def trees(tmp_path, monkeypatch):
    original, output, prior = [tmp_path / n for n in ('original', 'output', 'coarse')]
    for p in (original, output, prior): p.mkdir()
    (original / 'raw').write_bytes(b'zero')
    (output / 'raw').write_bytes(b'n8')
    (prior / 'raw').write_bytes(b'coarse')
    claim, lock, ledger = [tmp_path / n for n in ('claim', 'lock', 'ledger')]
    for p in (claim, lock, ledger): p.write_bytes(b'x')
    monkeypatch.setattr(storage, 'ORIGINAL_BYTES', 4)
    return (original, claim, output, [ledger], lock), prior


def test_prior_capture_counts_and_reserve(trees):
    args, prior = trees
    result = storage.cumulative_storage(*args, free_bytes=4*1024**3, prior_capture_roots=[prior])
    assert result['total_bytes'] == 15
    assert any(r['path'] == 'prior-0/raw' and r['bytes'] == 6 for r in result['files'])
    required = result['required_free_bytes']
    assert storage.cumulative_storage(*args, free_bytes=required-1, prior_capture_roots=[prior])['status'] == 'FAIL'


@pytest.mark.parametrize('fault', ['overlap', 'duplicate', 'hardlink', 'missing', 'string'])
def test_prior_capture_refusals(trees, fault):
    args, prior = trees
    roots = [prior]
    if fault == 'overlap': roots = [args[0]]
    elif fault == 'duplicate': roots = [prior, prior]
    elif fault == 'hardlink': os.link(args[0] / 'raw', prior / 'alias')
    elif fault == 'missing': roots = [prior / 'absent']
    else: roots = str(prior)
    with pytest.raises(ValueError):
        storage.cumulative_storage(*args, free_bytes=4*1024**3, prior_capture_roots=roots)


def test_prior_exhaustion_retains_originals(trees, monkeypatch):
    args, prior = trees
    monkeypatch.setattr(storage, 'ALLOWANCE_BYTES', 14)
    result = storage.cumulative_storage(*args, free_bytes=4*1024**3, prior_capture_roots=[prior])
    assert result['status'] == 'FAIL' and result['remaining_allowance_bytes'] == -1
    assert (prior / 'raw').read_bytes() == b'coarse'


def test_failed_claim_write_consumes_and_refuses_restart(lineage):
    from digitalmodel.ansys.cylinder_pressure_journal import write_exclusive
    def interrupted(path, value):
        write_exclusive(path, value)
        if path.name.endswith('.ordinal-3.json'):
            raise OSError('synthetic after durable claim')
    item = PressureJournal(lineage[0], lineage[1], ordinal=3,
                           predecessor=lineage[2], writer=interrupted)
    item.start({})
    with pytest.raises(OSError): item.claim({})
    assert item.consumed
    with pytest.raises(FileExistsError): journal(lineage)


def test_predecessor_descriptor_is_copied(lineage):
    item = journal(lineage)
    lineage[2]['sha256'] = '0' * 64
    item.start({})
    assert item.started


def test_coarse_terminal_retains_existing_failure_reporting_semantics(lineage):
    parent, sha, prior = lineage
    Path(prior['path']).unlink()
    item = PressureJournal(parent, sha)
    item.start({}); item.claim({})
    parent.write_bytes(b'changed after consumption')
    item.terminal({'reason': 'parent changed'})
    assert item.paths['terminal'].exists()
