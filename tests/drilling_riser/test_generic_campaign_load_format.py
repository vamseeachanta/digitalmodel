"""Solver-free format checks against the in-repository program example."""
import copy
from pathlib import Path

import pytest
import yaml

from digitalmodel.solvers.orcaflex.modular_generator.schema.generic import (
    FIELD_TO_SECTION, SECTION_REGISTRY,
)

ROOT = Path(__file__).resolve().parents[2]
CAMPAIGN = ROOT / 'docs/domains/orcaflex/library/campaigns/generic-drilling-riser'


def assert_load_structure(objects):
    assert 'P-yModels' in objects
    assert 'PyModels' not in objects
    for vessel in objects['VesselTypes']:
        draught_keys = list(vessel['Draughts'][0])
        ordered = ('AMDMethod', 'ReferenceOrigin', 'ReferenceOriginDatumPosition',
                   'HydrostaticStiffnessMethod',
                   'HydrostaticStiffnessz, HydrostaticStiffnessRx, HydrostaticStiffnessRy')
        assert [key for key in draught_keys if key in ordered] == list(ordered)
        for key in ('AMDMethod', 'ReferenceOrigin', 'ReferenceOriginDatumPosition',
                    'HydrostaticStiffnessMethod',
                    'HydrostaticStiffnessz, HydrostaticStiffnessRx, HydrostaticStiffnessRy'):
            assert key not in vessel
            assert key in vessel['Draughts'][0]


def test_generated_load_structure():
    paths = sorted((CAMPAIGN / 'models').glob('*/includes/20_generic_objects.yml'))
    assert len(paths) == 24
    for path in paths:
        assert_load_structure(yaml.safe_load(path.read_text(encoding='utf-8')))


def test_py_collection_registry():
    assert FIELD_TO_SECTION['py_models'] == 'P-yModels'
    assert 'P-yModels' in SECTION_REGISTRY
    assert 'PyModels' not in SECTION_REGISTRY


def test_load_structure_negative_controls():
    path = CAMPAIGN / 'models/o0-c0/includes/20_generic_objects.yml'
    objects = yaml.safe_load(path.read_text(encoding='utf-8'))
    # Repair a private copy so controls also run before the production correction.
    objects['P-yModels'] = objects.pop('PyModels', objects.get('P-yModels'))
    vessel = objects['VesselTypes'][0]
    keys = ('AMDMethod', 'ReferenceOrigin', 'ReferenceOriginDatumPosition',
            'HydrostaticStiffnessMethod',
            'HydrostaticStiffnessz, HydrostaticStiffnessRx, HydrostaticStiffnessRy')
    for key in keys:
        if key in vessel:
            vessel['Draughts'][0][key] = vessel.pop(key)
    assert_load_structure(objects)
    wrong_key = copy.deepcopy(objects)
    wrong_key['PyModels'] = wrong_key.pop('P-yModels')
    with pytest.raises(AssertionError):
        assert_load_structure(wrong_key)
    for key in keys:
        wrong_scope = copy.deepcopy(objects)
        vessel = wrong_scope['VesselTypes'][0]
        vessel[key] = vessel['Draughts'][0].pop(key)
        with pytest.raises(AssertionError):
            assert_load_structure(wrong_scope)


def test_template_and_generated_contacts_preserve_source(tmp_path):
    from digitalmodel.solvers.orcaflex.modular_generator import ModularModelGenerator

    library = ROOT / 'docs/domains/orcaflex/library'
    source = yaml.safe_load(
        (library / 'model_library/b01_drilling_riser/spec.yml').read_text(encoding='utf-8'))
    template_path = library / 'templates/drilling_riser/spec.yml'
    template = yaml.safe_load(template_path.read_text(encoding='utf-8'))
    expected = source['generic']['line_contact_data']
    assert template['generic'].get('line_contact_data') == expected
    relationships = expected['data']['Relationships']
    assert [(item['PenetratingLine'], item['PenetratingLineIs'],
             item['SplinedLine'], item['ContainmentEnabled'])
            for item in relationships] == [
        ('Riser', 'Around', 'Choke', False),
        ('Riser', 'Around', 'Kill', False),
        ('Drill String', 'Inside', 'Riser', True),
        ('Drill String Below BOP', 'Inside', 'Casing', True),
    ]
    names = {line['name'] for line in template['generic']['lines']}
    assert all(item['Enabled'] and item['PenetratingLine'] in names
               and item['SplinedLine'] in names for item in relationships)
    ModularModelGenerator(template_path).generate(tmp_path)
    objects = yaml.safe_load(
        (tmp_path / 'includes/20_generic_objects.yml').read_text(encoding='utf-8'))
    assert objects['LineContactData'] == expected['data']
