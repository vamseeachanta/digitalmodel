"""Sensitivity preserves verified heave and every unselected model field."""
import hashlib
import json

import pytest
import yaml

from digitalmodel.solvers.orcaflex.yaml_utils import OrcaFlexLoader
from digitalmodel.workflows.installation_hydrodynamic_sensitivity import prepare_sensitivity
from digitalmodel.workflows.structure_hydrodynamic_candidate import build_candidate
from tests.workflows.test_structure_hydrodynamic_candidate import case


@pytest.fixture
def prepared(case):
    source, sha, output, config, wiki, _ = case
    build_candidate(source, sha, output, config, wiki_root=wiki)
    receipt = output / 'properties.json'
    return receipt, hashlib.sha256(receipt.read_bytes()).hexdigest(), wiki


def test_scoped_sensitivity(prepared, tmp_path):
    receipt, sha, wiki = prepared
    output = tmp_path / 'sensitivity'
    result = prepare_sensitivity(receipt, sha, output, wiki_root=wiki)
    assert len(result['cases']) == 5
    assert result['analysis_executed'] is False
    assert result['acceptance_established'] is False
    baseline, flat, endpoint, low, high = result['cases']
    assert flat['added_mass_kg'][2] == baseline['added_mass_kg'][2]
    assert flat['added_mass_kg'][0] < baseline['added_mass_kg'][0]
    # Source fixture X dimensions: a=.5, b=1; plate coefficient .757.
    import math
    assert flat['added_mass_kg'][0] == pytest.approx(.757 * math.pi / 4 * 1025 * .5**2)
    assert flat['added_mass_kg'][1] == pytest.approx(.872 * math.pi / 4 * 1025 * .5**2 * 2)
    assert endpoint['added_mass_kg'][0::2] == baseline['added_mass_kg'][0::2]
    assert endpoint['added_mass_kg'][1] == pytest.approx(baseline['added_mass_kg'][1] / .872)
    assert low['added_mass_kg'] == high['added_mass_kg'] == baseline['added_mass_kg']
    assert low['cd'] == pytest.approx([v * .8 for v in baseline['cd']])
    assert high['cd'] == pytest.approx([v * 1.2 for v in baseline['cd']])
    for entry in result['cases']:
        change = yaml.load((output / entry['change_file']).read_text(), Loader=OrcaFlexLoader)
        assert change['BaseFile'] == '../master.yml'
        assert set(change) <= {'BaseFile', '6DBuoys'}
        if entry['overrides']:
            assert isinstance(change['6DBuoys'], dict)
            assert change['6DBuoys'] == {result['body_name']: entry['overrides']}
    assert (output / 'master.yml').read_bytes() == (receipt.parent / 'master.yml').read_bytes()
    assert json.loads((output / 'sensitivity.json').read_text()) == result
    with pytest.raises(FileExistsError):
        prepare_sensitivity(receipt, sha, output, wiki_root=wiki)


@pytest.mark.parametrize('bad', ['receipt_hash', 'master_hash', 'citation', 'property'])
def test_fail_closed(prepared, tmp_path, bad):
    receipt, sha, wiki = prepared
    data = json.loads(receipt.read_text())
    if bad == 'master_hash': data['master_sha256'] = '0' * 64
    if bad == 'citation': data['citations'] = []
    if bad == 'property': data['properties']['translational']['ca']['x'] *= 2
    receipt.write_text(json.dumps(data))
    sha = hashlib.sha256(receipt.read_bytes()).hexdigest()
    if bad == 'receipt_hash': sha = '0' * 64
    output = tmp_path / 'rejected'
    with pytest.raises(ValueError):
        prepare_sensitivity(receipt, sha, output, wiki_root=wiki)
    assert not output.exists()


@pytest.mark.parametrize('target', ['master', 'receipt'])
def test_source_drift_rejected(prepared, tmp_path, monkeypatch, target):
    import digitalmodel.workflows.installation_hydrodynamic_sensitivity as workflow
    receipt, sha, wiki = prepared
    original = workflow._factors

    def mutate(data):
        path = receipt if target == 'receipt' else receipt.parent / 'master.yml'
        path.write_bytes(path.read_bytes() + b'\n')
        return original(data)

    monkeypatch.setattr(workflow, '_factors', mutate)
    with pytest.raises(ValueError, match='drift'):
        prepare_sensitivity(receipt, sha, tmp_path / 'drift', wiki_root=wiki)
    assert not (tmp_path / 'drift').exists()


def test_late_source_drift_rejected(prepared, tmp_path, monkeypatch):
    import digitalmodel.workflows.installation_hydrodynamic_sensitivity as workflow
    receipt, sha, wiki = prepared
    original = workflow._case_files

    def mutate(*args):
        cases = original(*args)
        master = receipt.parent / 'master.yml'
        master.write_bytes(master.read_bytes() + b'\n')
        return cases

    monkeypatch.setattr(workflow, '_case_files', mutate)
    with pytest.raises(ValueError, match='preservation'):
        prepare_sensitivity(receipt, sha, tmp_path / 'late-drift', wiki_root=wiki)


@pytest.mark.parametrize('field', ['BaseFile', 'IncludeFile'])
def test_nested_master_rejected(prepared, tmp_path, field):
    receipt, sha, wiki = prepared
    master = receipt.parent / 'master.yml'
    master.write_text(master.read_text() + f'\n{field}: unpinned.yml\n')
    data = json.loads(receipt.read_text())
    data['master_sha256'] = hashlib.sha256(master.read_bytes()).hexdigest()
    receipt.write_text(json.dumps(data))
    sha = hashlib.sha256(receipt.read_bytes()).hexdigest()
    with pytest.raises(ValueError, match='self-contained'):
        prepare_sensitivity(receipt, sha, tmp_path / 'nested', wiki_root=wiki)
    assert not (tmp_path / 'nested').exists()


def test_native_named_override_preserves_connections(tmp_path):
    api = pytest.importorskip('OrcFxAPI')
    import digitalmodel.workflows.installation_hydrodynamic_sensitivity as workflow
    model = api.Model(threadCount=1)
    buoy = model.CreateObject(api.ObjectType.Buoy6D, 'Body')
    buoy.BuoyType = 'Lumped buoy'
    buoy.Mass = 2.0
    for axis in 'XYZ':
        setattr(buoy, 'HydrodynamicMass' + axis, 1.0)
        setattr(buoy, 'AddedMassCoefficient' + axis, 1.0)
    buoy.InitialX = 3.0
    buoy.InitialZ = -15.0
    line = model.CreateObject(api.ObjectType.Line, 'Sling')
    line.EndAConnection = 'Body'
    master = tmp_path / 'master.yml'
    model.SaveData(str(master))
    source = yaml.load(master.read_text(encoding='utf-8-sig'), Loader=OrcaFlexLoader)
    body = next(b for b in source['6DBuoys'] if b['Name'] == 'Body')
    fields = {key: body[key] for key in ('DragForceCoefficient', 'AddedMassCoefficient', 'HydrodynamicMass')}
    (tmp_path / 'changes').mkdir()
    scenarios = [('native', 'regression', {'DragForceCoefficient': [1.1, 1.2, 1.3]})]
    result = workflow._case_files(tmp_path, {'body_name': 'Body'}, source, fields, scenarios)
    resolved = api.Model(threadCount=1)
    resolved.LoadData(str(tmp_path / result[0]['change_file']))
    assert resolved['Sling'].EndAConnection == 'Body'
    assert resolved['Body'].InitialX == 3.0
    assert resolved['Body'].InitialZ == -15.0
    assert resolved['Body'].Mass == 2.0
    for axis in 'XYZ':
        setattr(resolved['Body'], 'DragForceCoefficient' + axis,
                getattr(buoy, 'DragForceCoefficient' + axis))
    resolved.general.ModelType = api.ModelType.Standard.value
    resolved.SaveData(str(tmp_path / 'roundtrip.yml'))
    after = yaml.load((tmp_path / 'roundtrip.yml').read_text(encoding='utf-8-sig'), Loader=OrcaFlexLoader)
    assert after == source
