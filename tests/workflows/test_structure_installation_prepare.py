"""Preparation preserves physics and fails closed on unresolved dependencies."""
import copy
import hashlib
import json
from pathlib import Path

import pytest
import yaml

from digitalmodel.solvers.orcaflex.yaml_utils import OrcaFlexLoader, orcaflex_dump
from digitalmodel.workflows.structure_installation_prepare import build_ahc_off_master


def test_parsed_source_is_same_byte_snapshot_as_digest(source, monkeypatch):
    path, digest, output, _ = prepare(source)
    original = Path.read_text
    def read_text(self, *args, **kwargs):
        if self.resolve() == path.resolve():
            pytest.fail('Source text reopened after digest read')
        return original(self, *args, **kwargs)
    monkeypatch.setattr(Path, 'read_text', read_text)
    build_ahc_off_master(path, digest, output, 'Compensator', 'Control')


@pytest.fixture
def source(tmp_path):
    model = {
        'General': {'UnitsSystem': 'SI'},
        'VariableData': {'Externalfunctions': [{'Name': 'Control',
            'Source': 'External file', 'FileName': 'control.dll'}]},
        'Winches': [{'Name': 'Compensator', 'WinchControlType': 'Whole simulation',
            'WholeSimulationControlMode': 'Specified payout rate',
            'StaticMode': 'Specified length', 'StaticValue': 10,
            'WholeSimulationPayoutRate': 'Control', 'Tags': {'gain': '0.9'},
            'Connection, ConnectionX': [['Wire', 0], ['Wire2', 0]]}],
        '6DBuoys': [{'Name': 'Body', 'Mass': 5, 'Volume': .7}],
    }
    return tmp_path, model


def prepare(source, mutate=None):
    root, model = source
    if mutate:
        mutate(model)
    path = root / 'source.yml'
    orcaflex_dump(model, path)
    digest = hashlib.sha256(path.read_bytes()).hexdigest()
    return path, digest, root / 'prepared', model


def test_only_controller_and_payout_change(source):
    path, digest, output, before = prepare(source)
    result = build_ahc_off_master(path, digest, output, 'Compensator', 'Control')
    actual = yaml.load((output / 'master.yml').read_text(), Loader=OrcaFlexLoader)
    expected = copy.deepcopy(before)
    expected['Winches'][0]['WholeSimulationPayoutRate'] = 0
    expected['VariableData'].pop('Externalfunctions')
    assert actual == expected
    assert hashlib.sha256(path.read_bytes()).hexdigest() == digest
    manifest = json.loads((output / 'preparation.json').read_text())
    assert manifest == result
    assert manifest['state'] == 'prepared_not_run'
    assert len(manifest['semantic_diff']) == 2
    assert manifest['analysis_executed'] is False


@pytest.mark.parametrize('mutation', [
    lambda m: m.update(BaseFile='other.yml'),
    lambda m: m['General'].update(IncludeFile='other.yml'),
    lambda m: m['Winches'][0].update(StaticValue=0),
    lambda m: m['Winches'][0].update(StaticValue=float('nan')),
    lambda m: m['Winches'][0].update(StaticMode='Specified tension'),
    lambda m: m['Winches'].append(copy.deepcopy(m['Winches'][0])),
    lambda m: m['VariableData']['Externalfunctions'].append(
        copy.deepcopy(m['VariableData']['Externalfunctions'][0])),
    lambda m: m['General'].update(AnotherControl='Control'),
    lambda m: m['General'].update(FileName='motion.csv'),
    lambda m: m['Winches'][0].update(WholeSimulationPayoutRate='Unknown'),
])
def test_rejects_invalid_or_dependent_source(source, mutation):
    path, digest, output, _ = prepare(source, mutation)
    with pytest.raises(ValueError):
        build_ahc_off_master(path, digest, output, 'Compensator', 'Control')
    assert not output.exists()


def test_digest_and_existing_destination_rejected(source):
    path, digest, output, _ = prepare(source)
    with pytest.raises(ValueError):
        build_ahc_off_master(path, '0' * 64, output, 'Compensator', 'Control')
    output.mkdir()
    with pytest.raises(FileExistsError):
        build_ahc_off_master(path, digest, output, 'Compensator', 'Control')
