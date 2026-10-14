"""Offline behaviour tests for the production public component library."""
import hashlib
import importlib.util
import json
import sys
import shutil
import socket
from pathlib import Path

import numpy as np
import pandas as pd
import pytest

# Load the actual production module in isolation from unrelated package
# initializers (which eagerly import the marine analysis plotting stack).
MODULE = Path(__file__).parents[3] / 'src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/component_database.py'
spec = importlib.util.spec_from_file_location('production_component_database', MODULE)
module = importlib.util.module_from_spec(spec)
sys.modules[spec.name] = module
spec.loader.exec_module(module)
ComponentDatabase = module.ComponentDatabase

DATA = Path(__file__).parents[3] / 'src/digitalmodel/data/marine_engineering/mooring_components'
EXCERPT = DATA / 'sources/moorpy_coefficients.json'


@pytest.fixture(autouse=True)
def forbid_network(monkeypatch):
    def reject(*args, **kwargs):
        raise AssertionError('These component tests must remain offline')
    monkeypatch.setattr(socket, 'socket', reject)
    monkeypatch.setattr(socket, 'create_connection', reject)


@pytest.fixture
def copied_data(tmp_path):
    shutil.copytree(DATA, tmp_path, dirs_exist_ok=True)
    return tmp_path


def test_default_load_and_cited_pins():
    db = ComponentDatabase()
    assert db.data_dir == DATA
    # No complete, eligible chain property source has been established.
    assert db.chain_db.empty
    excerpt = json.loads(EXCERPT.read_text())
    for table, family_column, families in [
        (db.wire_db, 'construction', excerpt['wire']),
        (db.line_db, 'material', excerpt['line']),
    ]:
        assert not table.empty
        for family, coefficients in families.items():
            rows = table[table[family_column] == family]
            assert not rows.empty
            d = rows.diameter / excerpt['mm_per_m']
            mbl_n = coefficients['MBL_d2'] * d**2 + coefficients.get('MBL_d3', 0) * d**3
            ea_n = coefficients.get('EA_d2', 0) * d**2 + coefficients.get('EA_MBL', 0) * mbl_n
            np.testing.assert_allclose(rows.mbl, mbl_n / excerpt['n_per_kn'])
            np.testing.assert_allclose(rows.stiffness, ea_n / excerpt['n_per_kn'])
            np.testing.assert_allclose(rows.weight_air, coefficients['mass_d2'] * d**2)
            if 'density' in coefficients:
                wet = rows.weight_air * (1 - excerpt['water_density'] / coefficients['density'])
            else:
                wet = rows.weight_air - np.pi / 4 * (coefficients['dvol_dnom'] * d)**2 * excerpt['water_density']
            np.testing.assert_allclose(rows.weight_water, wet)
    wire = db.wire_db.iloc[0]
    assert db.get_wire_rope(wire.diameter, wire.construction).mbl == wire.mbl


def test_complete_provenance_and_monotonic_families():
    db = ComponentDatabase()
    for kind, table, family in [('wire', db.wire_db, 'construction'), ('line', db.line_db, 'material')]:
        for _, row in table.iterrows():
            p = db.get_provenance(kind, row.diameter, **{family: row[family]})
            assert p['kind'] == 'computed'
            assert p['diameter_source']['commit']
            assert p['diameter_source']['license'] == 'MIT License'
            assert p['formula']
            assert p['source']['license'] == 'BSD 3-Clause License'
            for key in ['url', 'commit', 'file', 'locator', 'retrieved_at', 'terms', 'terms_url', 'sha256']:
                assert p['source'][key]
            for column in ['diameter', 'mbl', 'stiffness', 'weight_air', 'weight_water']:
                assert np.isfinite(row[column]) and row[column] > 0
        for _, rows in table.groupby(family):
            rows = rows.sort_values('diameter')
            assert len(rows) > 1
            assert (rows.mbl.diff().dropna() > 0).all()
            assert (rows.weight_air.diff().dropna() > 0).all()


def test_csv_round_trip(copied_data):
    original = ComponentDatabase()
    for kind in ['chain', 'wire', 'line']:
        getattr(original, f'{kind}_db').to_csv(copied_data / f'{kind}_properties.csv', index=False)
    path = copied_data / 'provenance.json'
    p = json.loads(path.read_text())
    for kind in ['chain', 'wire', 'line']:
        p['tables'][kind]['sha256'] = hashlib.sha256((copied_data / f'{kind}_properties.csv').read_bytes()).hexdigest()
    path.write_text(json.dumps(p))
    restored = ComponentDatabase(copied_data)
    for kind in ['chain', 'wire', 'line']:
        pd.testing.assert_frame_equal(getattr(original, f'{kind}_db'), getattr(restored, f'{kind}_db'))
    assert restored.provenance['rows'] == original.provenance['rows']


@pytest.mark.parametrize('bad_value', ['bad', '-1', 'NaN', 'inf'])
def test_malformed_numeric_row_rejected(copied_data, bad_value):
    path = copied_data / 'wire_properties.csv'
    table = pd.read_csv(path).astype({'mbl': str})
    table.loc[table.index[0], 'mbl'] = bad_value
    table.to_csv(path, index=False)
    with pytest.raises(ValueError, match='wire_properties.csv.*mbl'):
        ComponentDatabase(copied_data)


def test_missing_column_rejected(copied_data):
    path = copied_data / 'line_properties.csv'
    table = pd.read_csv(path).drop(columns=['stiffness'])
    table.to_csv(path, index=False)
    with pytest.raises(ValueError, match='line_properties.csv.*stiffness'):
        ComponentDatabase(copied_data)


@pytest.mark.parametrize('defect', ['missing_row', 'missing_terms', 'wrong_unit', 'stale_digest', 'missing_diameter_source'])
def test_invalid_provenance_rejected(copied_data, defect):
    path = copied_data / 'provenance.json'
    p = json.loads(path.read_text())
    if defect == 'missing_row':
        p['rows']['wire'].pop()
    elif defect == 'missing_terms':
        p['sources']['moorpy']['terms'] = ''
    elif defect == 'wrong_unit':
        p['units']['mbl'] = 'N'
    elif defect == 'stale_digest':
        p['tables']['wire']['sha256'] = 'invalid'
    else:
        p.pop('diameter_source')
    path.write_text(json.dumps(p))
    with pytest.raises(ValueError, match='provenance'):
        ComponentDatabase(copied_data)


def test_missing_sidecar_is_explicitly_unverified(copied_data):
    # An optional sidecar preserves existing custom-directory loading.
    (copied_data / 'provenance.json').rename(copied_data / 'provenance.saved')
    db = ComponentDatabase(copied_data)
    assert db.provenance is None
    with pytest.raises(ValueError, match='No provenance'):
        db.get_provenance('wire', db.wire_db.iloc[0].diameter)


def test_provenance_lookup_does_not_guess():
    db = ComponentDatabase()
    with pytest.raises(ValueError, match='Provenance not found'):
        db.get_provenance('chain', db.wire_db.iloc[0].diameter)


def test_generator_reproduces_all_outputs(tmp_path):
    path = Path(__file__).parents[3] / 'scripts/data/generate_mooring_components.py'
    generator_spec = importlib.util.spec_from_file_location('component_generator', path)
    generator = importlib.util.module_from_spec(generator_spec)
    generator_spec.loader.exec_module(generator)
    generator.generate(tmp_path)
    for name in ['chain_properties.csv', 'wire_properties.csv', 'line_properties.csv', 'provenance.json']:
        assert (tmp_path / name).read_bytes() == (DATA / name).read_bytes()


def test_generated_directory_is_loadable(tmp_path):
    path = Path(__file__).parents[3] / 'scripts/data/generate_mooring_components.py'
    generator_spec = importlib.util.spec_from_file_location('component_generator', path)
    generator = importlib.util.module_from_spec(generator_spec)
    generator_spec.loader.exec_module(generator)
    generator.generate(tmp_path)
    db = ComponentDatabase(tmp_path)
    assert not db.wire_db.empty and not db.line_db.empty


def test_provenance_return_is_independent():
    db = ComponentDatabase()
    row = db.wire_db.iloc[0]
    record = db.get_provenance('wire', row.diameter, construction=row.construction)
    record['source']['terms'] = ''
    assert db.get_provenance('wire', row.diameter, construction=row.construction)['source']['terms']


def test_wrong_manifest_row_count_rejected(copied_data):
    path = copied_data / 'provenance.json'
    p = json.loads(path.read_text())
    p['tables']['wire']['row_count'] = len(p['rows']['line'])
    path.write_text(json.dumps(p))
    with pytest.raises(ValueError, match='provenance.*row count'):
        ComponentDatabase(copied_data)


def test_malformed_sidecar_has_clear_error(copied_data):
    (copied_data / 'provenance.json').write_text('{')
    with pytest.raises(ValueError, match='provenance.*JSON'):
        ComponentDatabase(copied_data)


def test_monotonicity_is_enforced(copied_data):
    path = copied_data / 'wire_properties.csv'
    table = pd.read_csv(path)
    table.loc[table.index[0], 'mbl'] = table.mbl.max()
    table.to_csv(path, index=False)
    with pytest.raises(ValueError, match='wire_properties.csv.*mbl must increase'):
        ComponentDatabase(copied_data)


def test_legacy_chain_query_interface(tmp_path):
    # Existing test_component_database.py uses this diameter/grade/link fixture.
    from dataclasses import asdict
    chain = module.ChainProperties.from_excel_formula(
        76, module.ChainGrade.R4, module.LinkType.STUD_LINK)
    pd.DataFrame([asdict(chain)]).to_csv(tmp_path / 'chain_properties.csv', index=False)
    db = ComponentDatabase(tmp_path)
    assert db.get_chain(chain.diameter, chain.grade, chain.link_type) == chain
    assert db.find_chain_by_mbl(chain.mbl, chain.grade, chain.link_type) == chain
    assert not db.list_available_chains(chain.grade, chain.link_type).empty
    assert db.wire_db.empty and db.line_db.empty
