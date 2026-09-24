"""RED preservation contracts; no native API, source fixture changes or solve."""
from pathlib import Path
import sys
import importlib.util
import json
import hashlib

import pytest
import yaml


@pytest.fixture
def builders(monkeypatch):
    monkeypatch.setitem(sys.modules, 'OrcFxAPI', None)
    from digitalmodel.solvers.orcaflex.modular_generator.schema import ProjectInputSpec
    from digitalmodel.solvers.orcaflex.modular_generator.builders.context import BuilderContext
    from digitalmodel.solvers.orcaflex.modular_generator.builders.general_builder import GeneralBuilder
    from digitalmodel.solvers.orcaflex.modular_generator.builders.generic_builder import GenericModelBuilder
    from digitalmodel.solvers.orcaflex.modular_generator.builders.environment_builder import EnvironmentBuilder

    root = Path(__file__).resolve().parents[4]
    source = root / 'docs/domains/orcaflex/library/templates/mooring_buoy/spec.yml'
    spec = ProjectInputSpec(**yaml.safe_load(source.read_bytes()))
    return spec, BuilderContext, GeneralBuilder, GenericModelBuilder, EnvironmentBuilder


def test_mooring_general_has_one_include_owner(builders):
    spec, context, general, generic, _ = builders
    sections = [general(spec, context()).build(), generic(spec, context()).build()]
    assert sum('General' in section for section in sections) == 1
    assert 'General' in sections[0]


def test_general_owner_preserves_explicit_source_controls(builders):
    spec, context, general, _, _ = builders
    output = general(spec, context()).build()['General']
    props = spec.generic.general_properties
    assert props['TargetLogSampleInterval'] == 0.1
    assert spec.simulation.time_step == 0.01
    assert output['TargetLogSampleInterval'] == props['TargetLogSampleInterval']
    assert output['BuoysIncludedInStatics'] == props['BuoysIncludedInStatics'] == 'All'
    assert output['ImplicitConstantTimeStep'] == spec.simulation.time_step


def test_explicit_reference_null_wind_setting_is_preserved_not_omitted(builders):
    spec, context, _, _, environment = builders
    # This is explicit caller intent, not a new default for missing source data.
    spec.environment.raw_properties = {'WindType': 'Constant', 'VerticalWindVariationFactor': None}
    output = environment(spec, context()).build()['Environment']
    assert 'VerticalWindVariationFactor' in output
    assert output['VerticalWindVariationFactor'] is None


def test_variable_mode_suppresses_constant_default_and_orders_maximum(builders):
    spec, context, general, _, _ = builders
    spec.generic.general_properties.update(ImplicitUseVariableTimeStep=True,
                                           ImplicitVariableMaxTimeStep=0.2)
    output = general(spec, context()).build()['General']
    assert output['ImplicitUseVariableTimeStep'] is True
    assert 'ImplicitConstantTimeStep' not in output
    keys = list(output)
    assert keys[keys.index('ImplicitUseVariableTimeStep') + 1] == 'ImplicitVariableMaxTimeStep'
    assert output['ImplicitVariableMaxTimeStep'] == 0.2


def test_constant_mode_suppresses_dormant_variable_maximum(builders):
    spec, context, general, _, _ = builders
    spec.generic.general_properties.update(ImplicitUseVariableTimeStep=False,
                                           ImplicitVariableMaxTimeStep=0.2)
    output = general(spec, context()).build()['General']
    assert 'ImplicitVariableMaxTimeStep' not in output
    assert output['ImplicitConstantTimeStep'] == spec.simulation.time_step
    keys = list(output)
    assert keys[keys.index('ImplicitUseVariableTimeStep') + 1] == 'ImplicitConstantTimeStep'


def test_explicit_general_values_win_over_typed_fallbacks(builders, caplog):
    spec, context, general, _, _ = builders
    spec.generic.general_properties.update(ImplicitConstantTimeStep=0.02,
                                           StageDuration=[7, 20], NorthDirection=27)
    output = general(spec, context()).build()['General']
    assert output['ImplicitConstantTimeStep'] == 0.02
    assert output['StageDuration'] == [7, 20]
    assert output['NorthDirection'] == 27
    for key in ('ImplicitConstantTimeStep', 'StageDuration', 'NorthDirection'):
        assert any(key in record.message for record in caplog.records)


def test_general_source_statics_order_and_early_custom_units(builders):
    spec, context, general, _, _ = builders
    spec.generic.general_properties.update(UnitsSystem='User', LengthUnits='m',
                                           MassUnits='kg', ForceUnits='N', g=9.81)
    output = general(spec, context()).build()['General']
    keys = list(output)
    assert keys[:5] == ['UnitsSystem', 'LengthUnits', 'MassUnits', 'ForceUnits', 'g']
    statics = ['WholeSystemStaticsEnabled', 'StaticsMaxIterations', 'StaticsMinDamping',
               'StaticsMaxDamping', 'DynamicsSolutionMethod']
    start = keys.index(statics[0])
    assert keys[start:start + len(statics)] == statics


@pytest.mark.parametrize('raw,expected_present,expected', [
    ({'WindType': 'Constant'}, False, None),
    ({'WindType': 'Constant', 'VerticalWindVariationFactor': None}, True, None),
    ({'WindType': 'Constant', 'VerticalWindVariationFactor': 0.8}, True, 0.8),
])
def test_wind_presence_and_value_are_source_specific(builders, raw, expected_present, expected):
    spec, context, _, _, environment = builders
    spec.environment.raw_properties = raw
    output = environment(spec, context()).build()['Environment']
    assert ('VerticalWindVariationFactor' in output) is expected_present
    if expected_present:
        assert output['VerticalWindVariationFactor'] == expected
        keys = list(output)
        assert keys.index('VerticalWindVariationFactor') > keys.index('WindDirection')


def packaging_module(monkeypatch):
    monkeypatch.setitem(sys.modules, 'OrcFxAPI', None)
    root = Path(__file__).resolve().parents[4]
    spec = importlib.util.spec_from_file_location('mooring_packaging_red',
                                                root / 'scripts/prepare_mooring_qualification.py')
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    monkeypatch.setenv('PYTHONHASHSEED', '0')
    # Packaging-only fixture. Generator settings and real verifier remain separate contracts.
    class StubGenerator:
        def __init__(self, source):
            self.source = source

        def generate(self, target):
            target.mkdir()
            (target / 'master.yml').write_bytes(b'[]\n')
    monkeypatch.setattr(module, 'ModularModelGenerator', StubGenerator)
    monkeypatch.setattr(module, 'verify_manifest', lambda path: None)
    return module


def test_derived_source_and_provenance_are_in_manifest(tmp_path, monkeypatch):
    module = packaging_module(monkeypatch)
    original = module.SOURCE.read_bytes()
    document = yaml.safe_load(original)
    document['environment']['raw_properties'] = {
        'WindType': 'Constant', 'VerticalWindVariationFactor': None}
    derived = tmp_path / 'derived.yml'
    derived.write_bytes(yaml.safe_dump(document, sort_keys=False).encode('utf-8'))
    manifest_path = module.prepare_bundle(tmp_path / 'output', source_path=derived)
    manifest = json.loads(manifest_path.read_bytes())
    output = manifest_path.parent
    assert (output / manifest['source']['path']).read_bytes() == derived.read_bytes()
    assert module.SOURCE.read_bytes() == original
    files = {entry['path']: entry['sha256'] for entry in manifest['files']}
    for relative in ('source/derivation.json', 'reference-differences.json'):
        assert files[relative] == hashlib.sha256((output / relative).read_bytes()).hexdigest()
    provenance = json.loads((output / 'source/derivation.json').read_bytes())
    assert provenance['template_sha256'] == hashlib.sha256(original).hexdigest()
    assert provenance['reference_sha256'] == module.digest(module.REFERENCE)
    assert provenance['source_sha256'] == module.digest(derived)
    assert provenance['applied_keys'] == [
        '/environment/raw_properties/VerticalWindVariationFactor',
        '/environment/raw_properties/WindType']
    retained = yaml.safe_load((output / manifest['source']['path']).read_bytes())
    assert retained['environment']['raw_properties'] == {
        'WindType': 'Constant', 'VerticalWindVariationFactor': None}


def test_derived_source_cannot_silently_change_physical_basis(tmp_path, monkeypatch):
    module = packaging_module(monkeypatch)
    document = yaml.safe_load(module.SOURCE.read_bytes())
    document['simulation']['time_step'] = 0.02
    derived = tmp_path / 'changed-physics.yml'
    derived.write_bytes(yaml.safe_dump(document, sort_keys=False).encode('utf-8'))
    with pytest.raises(ValueError, match='unreviewed source difference'):
        module.prepare_bundle(tmp_path / 'output', source_path=derived)
    assert not (tmp_path / 'output/manifest.json').exists()



def test_complete_generated_include_closure_has_single_general_owner(builders, tmp_path):
    from digitalmodel.solvers.orcaflex.modular_generator import ModularModelGenerator
    spec, _, _, _, _ = builders
    spec.environment.raw_properties = {'WindType': 'Constant', 'VerticalWindVariationFactor': None}
    ModularModelGenerator.from_spec(spec).generate(tmp_path)
    master = yaml.safe_load((tmp_path / 'master.yml').read_bytes())
    documents = [yaml.safe_load((tmp_path / row['includefile']).read_bytes()) for row in master]
    owners = [doc['General'] for doc in documents if 'General' in doc]
    assert len(owners) == 1
    assert owners[0]['TargetLogSampleInterval'] == 0.1
    assert owners[0]['ImplicitConstantTimeStep'] == 0.01
    assert owners[0]['BuoysIncludedInStatics'] == 'All'
    environment = next(doc['Environment'] for doc in documents if 'Environment' in doc)
    assert 'VerticalWindVariationFactor' in environment
    assert environment['VerticalWindVariationFactor'] is None
