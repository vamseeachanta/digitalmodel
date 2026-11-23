"""Retained input descriptors never imply native output or operator approval."""
import shutil
from pathlib import Path

import pytest

from digitalmodel.ansys.analysis_pending_inputs import pending_inputs

ROOT = Path(__file__).resolve().parents[2]
PINS = {'manifest_sha256': 'ea69920aad1d84a17ce37aacabcfc2308ac33eb99c4155d6b7b6421e9192f8c1',
        'reference_sha256': '385ff3d42e4219faefd8cc1a72c20525f4b2731cf2fcb31aa51ba5b1de63c3b4'}


def test_frozen_input_descriptors_have_no_numeric_results():
    cases, resolver = pending_inputs(ROOT, source_revision='test-snapshot', observed_at='2026-09-14T00:00:00Z', **PINS)
    assert len(cases) == 4
    assert sum(len(case['responses']) for case in cases) == 256
    for case in cases:
        assert case['native_attempt_count'] == 0
        assert case['author_status'] == 'unverified'
        assert all(row['value'] is None for row in case['responses'])
        assert all('native-not-attempted' in row['limitations'] for row in case['responses'])
        assert all(resolver[ref['id']].is_file() for ref in case['evidence'])


def test_changed_deck_is_refused_before_case_construction(tmp_path):
    relative = Path('examples/ansys/cylinder-benchmark')
    shutil.copytree(ROOT/relative, tmp_path/relative)
    (tmp_path/relative/'prepared/ocv-t60-p10-n4.inp').write_text('changed')
    with pytest.raises(ValueError, match='evidence changed'):
        pending_inputs(tmp_path, source_revision='test-snapshot', observed_at='2026-09-14T00:00:00Z', **PINS)


def test_changed_manifest_cannot_authenticate_itself(tmp_path):
    relative = Path('examples/ansys/cylinder-benchmark')
    shutil.copytree(ROOT/relative, tmp_path/relative)
    path = tmp_path/relative/'manifest.json'
    path.write_bytes(path.read_bytes() + b'\n')
    with pytest.raises(ValueError, match='evidence changed'):
        pending_inputs(tmp_path, source_revision='test-snapshot', observed_at='2026-09-14T00:00:00Z', **PINS)
