"""Offline contract tests for the opt-in native re-solve support."""
import json
import shutil
from pathlib import Path
from types import SimpleNamespace

import pytest

from digitalmodel.ansys.runner import ANSYSRunStatus
from tests.ansys import native_resolve_support as support


@pytest.fixture
def candidate(tmp_path):
    source = Path(__file__).resolve().parents[2] / 'examples/ansys/pressure-vessel'
    case = tmp_path / 'pressure-vessel'
    shutil.copytree(source / 'golden', case / 'golden')
    shutil.copyfile(source / 'pv.inp', case / 'pv.inp')
    (case / 'synthetic-mapdl').write_text('synthetic executable; never launched')
    return case, tmp_path / 'fresh'


def simulated_runner(case, failure=None):
    def run(script, **kwargs):
        output = kwargs['output_dir']
        assert kwargs['extra_args'] == ['-np', '1', '-smp']
        assert script.parent == output and script.read_bytes() == (case / 'pv.inp').read_bytes()
        log = output / 'pv.out'
        log.write_text('Ansys 2026 R1.01 Build 26.1 UP20260202 WINDOWS x64')
        digest = output / 'pv_result.csv'
        shutil.copyfile(case / 'golden/pv_result.csv', digest)
        if failure == 'version':
            log.write_text('Ansys 2025 R1')
        if failure == 'partial_version':
            log.write_text('Ansys 2026 R1.010 Build 126.1 UP202602020 WINDOWS x64-extra')
        if failure == 'drift':
            digest.write_text(digest.read_text().replace('134.7185', '234.7185'))
        if failure == 'nonfinite':
            digest.write_text(digest.read_text().replace('134.7185', 'nan'))
        return SimpleNamespace(status=ANSYSRunStatus.FAILED if failure == 'failed' else ANSYSRunStatus.COMPLETED,
                               return_code=1 if failure == 'failed' else 0,
                               log_file=log, result_files=[log] if failure == 'missing' else [log, digest],
                               error_message='synthetic failure', duration_seconds=.01)
    return run


def test_resolve_preserves_bytes_profile_and_receipt(candidate, monkeypatch):
    case, output = candidate
    monkeypatch.setattr(support, 'run_ansys', simulated_runner(case))
    receipt = support.resolve_golden(case, output, case / 'synthetic-mapdl')
    assert receipt['status'] == 'comparison_passed_unreviewed'
    assert not receipt['native_qualification_complete']
    assert receipt['execution']['executable_sha256'] == support._sha256(case / 'synthetic-mapdl')
    assert receipt['execution']['argv'][-3:] == ['-np', '1', '-smp']
    assert receipt['execution']['timeout_seconds'] == 120
    assert json.loads((output / 'resolve_receipt.json').read_text()) == receipt
    assert {item['path'] for item in receipt['artifacts']} >= {'pv.inp', 'pv.out', 'pv_result.csv'}


@pytest.mark.parametrize('failure', ['version', 'drift', 'nonfinite', 'failed', 'missing', 'partial_version'])
def test_invalid_native_evidence_never_passes(candidate, monkeypatch, failure):
    case, output = candidate
    monkeypatch.setattr(support, 'run_ansys', simulated_runner(case, failure))
    with pytest.raises(ValueError):
        support.resolve_golden(case, output, case / 'synthetic-mapdl')
    assert not (output / 'resolve_receipt.json').exists()


@pytest.mark.parametrize('invalid', ['hash', 'profile', 'reuse', 'duplicate_digest', 'deck_path'])
def test_preflight_rejects_invalid_inputs_before_launch(candidate, monkeypatch, invalid):
    case, output = candidate
    provenance_path = case / 'golden/PROVENANCE.json'
    provenance = json.loads(provenance_path.read_text())
    if invalid == 'hash':
        provenance['input']['sha256'] = '0' * 64
    elif invalid == 'profile':
        provenance['solver']['cores'] = 2
    elif invalid == 'reuse':
        output.mkdir()
    elif invalid == 'duplicate_digest':
        shutil.copyfile(case / 'golden/pv_result.csv', case / 'golden/other_result.csv')
    elif invalid == 'deck_path':
        provenance['input']['deck'] = '../pv.inp'
    provenance_path.write_text(json.dumps(provenance))
    monkeypatch.setattr(support, 'run_ansys', lambda *a, **k: pytest.fail('invalid candidate launched'))
    with pytest.raises(ValueError):
        support.resolve_golden(case, output, Path('synthetic-mapdl'))


def test_native_marker_invokes_real_support_when_explicitly_enabled(tmp_path, monkeypatch):
    from tests.ansys import test_example_goldens as goldens
    called = []
    monkeypatch.setenv('ANSYS_NATIVE_TESTS', '1')
    monkeypatch.delenv('PYTEST_XDIST_WORKER', raising=False)
    monkeypatch.setenv('ANSYS_NATIVE_EXECUTABLE', str(tmp_path / 'mapdl.exe'))
    monkeypatch.setenv('ANSYS_NATIVE_OUTPUT_ROOT', str(tmp_path / 'captures'))
    (tmp_path / 'mapdl.exe').write_text('synthetic stub')
    monkeypatch.setattr(support, 'resolve_golden', lambda *args: called.append(args))
    try:
        goldens.test_committed_golden_is_stable('pressure-vessel')
    except pytest.skip.Exception:
        pytest.fail('Native test is still an unconditional skip')
    assert len(called) == 1
    assert called[0][1] == tmp_path / 'captures/pressure-vessel'


@pytest.mark.parametrize('text', ['x,1\nx,2', 'x,1, x ,2', 'x,nan\nx,2'])
def test_duplicate_normalized_labels_are_rejected(tmp_path, text):
    digest = tmp_path / 'digest.csv'
    digest.write_text(text)
    with pytest.raises(ValueError, match='Duplicate'):
        support._digest(digest)
