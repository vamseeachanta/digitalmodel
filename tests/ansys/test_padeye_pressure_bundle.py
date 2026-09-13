"""Composition and fixed-input checks, including an observed native CDB fixture."""
import hashlib
import json
from pathlib import Path

import pytest

from digitalmodel.ansys.padeye_pressure import build_pressure_mesh, generate_pressure_preparation, pressure_geometry
from tests.ansys.padeye_pressure_bundle import verify_pressure_bundle
from tests.ansys.padeye_pressure_study import verify_pressure_mesh

FIXTURES = Path(__file__).parent/'fixtures'
INPUT_HASHES = {
    10: 'a54f2e7a3bee45b3943dd98b23e1a9b340c33745f083a6ccf48ffa8af2fef18a',
    5: 'f0c84c4284a9e4524f6eb2f4b0cc15105f984f7874a39f8e4bfa85fb66754e5f',
    2.5: '3e8f5675696dadac1f70450686bd4755f99c749ac856181fd1914e69a5929ae2',
}


def bundle():
    files = [generate_pressure_preparation(pressure_geometry(10)).encode(),
             (FIXTURES/'pressure-coarse-v261.cdb').read_bytes(),
             (FIXTURES/'pressure-coarse-shape-v261.txt').read_bytes()]
    provenance = json.loads((FIXTURES/'pressure-coarse-v261-provenance.json').read_text())
    expected = {'input_sha256': INPUT_HASHES[10], 'cdb_sha256': provenance['cdb_sha256'],
                'log_sha256': provenance['shape_excerpt_sha256'],
                'source_head': provenance['source_head'], 'capture_id': provenance['capture'],
                'log_artifact_class': 'format_reproduction'}
    return files, expected


def test_composed_native_format_fixture_passes_without_qualification():
    files, expected = bundle()
    result = verify_pressure_bundle(*files, expected)
    assert result['artifact_hashes_verified'] == 3
    assert result['domain']['frozen_domain_verified'] is True
    assert result['shape']['shape_gate_passed'] is True
    assert result['pressure']['force_n'][1] == pytest.approx(50000, abs=0.001)
    assert result['native_qualification_complete'] is False
    assert result['log_artifact_class'] == 'format_reproduction'
    assert result['status'] == 'format_fixture_checks_passed'
    assert result['verified_hashes']['cdb_sha256'] == expected['cdb_sha256']


@pytest.mark.parametrize('index', [0, 1, 2])
def test_altered_input_database_or_log_rejected(index):
    files, expected = bundle()
    files[index] += b'\n'
    with pytest.raises(ValueError):
        verify_pressure_bundle(*files, expected)


def test_self_rehashed_changed_generator_is_not_the_frozen_case():
    files, expected = bundle()
    files[0] += b'! changed generator\n'
    expected['input_sha256'] = hashlib.sha256(files[0]).hexdigest()
    with pytest.raises(ValueError):
        verify_pressure_bundle(*files, expected)


def test_rehashed_wrong_endpoint_pressure_reaches_semantic_rejection():
    files, expected = bundle()
    lines = files[1].splitlines(keepends=True)
    i = next(i for i, line in enumerate(lines) if line.startswith(b'SFEBLOCK')) + 2
    lines[i] = lines[i][:17] + f'{1.0:20.9g}'.encode() + lines[i][37:]
    files[1] = b''.join(lines)
    expected['cdb_sha256'] = hashlib.sha256(files[1]).hexdigest()
    with pytest.raises(ValueError, match='pressure shape'):
        verify_pressure_bundle(*files, expected)


def test_rehashed_wrong_log_count_reaches_quality_rejection():
    import re
    files, expected = bundle()
    files[2] = re.sub(rb'(Element count\s+)1032', rb'\g<1>1031', files[2])
    expected['log_sha256'] = hashlib.sha256(files[2]).hexdigest()
    with pytest.raises(ValueError, match='shape element count'):
        verify_pressure_bundle(*files, expected)


def test_unrecognized_receipt_assertions_are_not_silently_ignored():
    files, expected = bundle()
    expected['native_qualification_complete'] = True
    with pytest.raises(ValueError):
        verify_pressure_bundle(*files, expected)


@pytest.mark.parametrize('size', [10, 5, 2.5])
def test_all_generated_levels_have_fixed_lf_bytes_and_independent_pressure_verification(size):
    raw = generate_pressure_preparation(pressure_geometry(size)).encode('utf-8')
    assert b'\r' not in raw
    assert hashlib.sha256(raw).hexdigest() == INPUT_HASHES[size]
    mesh = build_pressure_mesh(pressure_geometry(size))
    assert verify_pressure_mesh(mesh, mesh['pressures'])['force_n'][1] == pytest.approx(50000)
