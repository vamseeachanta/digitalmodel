# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Fail-closed loader tests using deliberately synthetic interpolation data."""

import json
import hashlib

import pytest

from digitalmodel.citations.schema import CitationResolutionError
from tests.subsea.on_bottom_stability.synthetic_e305_tables import make_synthetic_tables


def test_synthetic_tables_present_interpolation_and_citation_sidecar(tmp_path, monkeypatch):
    from digitalmodel.subsea.on_bottom_stability.e305_tables import load_tables, TableError

    path, _, wiki = make_synthetic_tables(tmp_path)
    monkeypatch.setenv('E305_TABLES_FILE', str(path))
    tables = load_tables(wiki_root=tmp_path)
    value = tables.lookup('Fw', K=1.0, M=1.0)
    assert value.value == pytest.approx(6.0)  # Mean of the four invented corners.
    assert value.units == '1'
    assert value.citation.code_id == 'dnv-rp-e305'
    assert tables.lookup('roughness', grain_size_m=1.0).value == pytest.approx(4.0)
    with pytest.raises(TableError):
        tables.lookup('Fw', K=3.0, M=1.0)
    with pytest.raises(TableError):
        tables.lookup('Fw', K=1.0)
    # Resolution is repeated at calculation time, after a valid initial load.
    original = wiki.read_bytes()
    before = hashlib.sha256(original).hexdigest()
    wiki.write_text(wiki.read_text().replace('revision: 1988', 'revision: synthetic-mismatch'))
    tampered = hashlib.sha256(wiki.read_bytes()).hexdigest()
    try:
        with pytest.raises(CitationResolutionError):
            tables.lookup('Fw', K=1.0, M=1.0)
    finally:
        wiki.write_bytes(original)
    restored = hashlib.sha256(wiki.read_bytes()).hexdigest()
    assert before == restored and before != tampered
    proof = {'before': before, 'tampered': tampered, 'restored': restored}
    (tmp_path / 'citation_probe_hashes.json').write_text(json.dumps(proof, sort_keys=True)+'\n')
    assert json.loads((tmp_path / 'citation_probe_hashes.json').read_text()) == proof


def test_tables_absent_fail_closed(tmp_path, monkeypatch):
    from digitalmodel.subsea.on_bottom_stability.e305_tables import load_tables, TableError

    monkeypatch.delenv('E305_TABLES_FILE', raising=False)
    with pytest.raises(TableError, match='configuration'):
        load_tables(wiki_root=tmp_path)
    with pytest.raises(TableError, match='file'):
        load_tables(tmp_path / 'absent.json', wiki_root=tmp_path)


@pytest.mark.parametrize('defect', ['missing_table', 'unsorted_axis', 'nonfinite',
                                  'missing_citation', 'wrong_units', 'ragged',
                                  'duplicate_axis', 'citation_mismatch', 'duplicate_key'])
def test_malformed_synthetic_tables_fail_closed(tmp_path, defect):
    from digitalmodel.subsea.on_bottom_stability.e305_tables import load_tables, TableError

    path, payload, _ = make_synthetic_tables(tmp_path)
    original = path.read_bytes()
    row = payload['tables']['Fw']
    if defect == 'missing_table':
        del payload['tables']['R']
    elif defect == 'unsorted_axis':
        row['axes'][0]['values'] = [2.0, 0.0]
    elif defect == 'nonfinite':
        row['values'][0][0] = float('nan')
    elif defect == 'missing_citation':
        del row['citation']
    elif defect == 'wrong_units':
        row['units'] = 'm'
    elif defect == 'ragged':
        row['values'][0] = [2.0]
    elif defect == 'duplicate_axis':
        row['axes'][1]['name'] = 'K'
    elif defect == 'citation_mismatch':
        row['citation']['revision'] = 'synthetic-mismatch'
    path.write_text(json.dumps(payload))
    if defect == 'duplicate_key':
        path.write_text('{"schema_version":1,"schema_version":1}')
    before = hashlib.sha256(original).hexdigest()
    tampered = hashlib.sha256(path.read_bytes()).hexdigest()
    try:
        with pytest.raises((TableError, CitationResolutionError)):
            load_tables(path, wiki_root=tmp_path)
    finally:
        path.write_bytes(original)
    restored = hashlib.sha256(path.read_bytes()).hexdigest()
    assert before == restored and before != tampered
    proof = {'before': before, 'tampered': tampered, 'restored': restored}
    (tmp_path / 'table_probe_hashes.json').write_text(json.dumps(proof, sort_keys=True)+'\n')
