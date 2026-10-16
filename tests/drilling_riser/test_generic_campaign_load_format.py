"""Solver-free format checks against the recorded SaveData structure."""
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
