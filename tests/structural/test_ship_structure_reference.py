"""Synthetic metadata assertions; no manufacturer values or engineering limits.

Expected outcomes follow the record contract in data/ship_structure_reference/
README.md and JSON Schema 2020-12 Validation (2022), sections 6 and 7.
All dimensions below are arbitrary positive schema fixtures, not design inputs.
"""

import copy
import importlib.util
import json
import socket
from pathlib import Path

import pytest
from jsonschema import Draft202012Validator, FormatChecker, ValidationError

ROOT = Path(__file__).resolve().parents[2]
SCHEMA = json.loads((ROOT / 'data/ship_structure_reference/research.schema.json').read_text())


def record():
    """Return authored synthetic metadata with unresolved source rights."""
    unknown = {'state': 'unknown', 'reason': 'Synthetic fixture has no source edition'}
    return {
        'schema_version': 2, 'observed_on': '2026-10-19',
        'status': 'internal_research_pending_source_rights',
        'publication_allowed': False, 'raw_sources_copied': False,
        'sources': [{
            'key': 'synthetic-source', 'catalog_source_id': unknown,
            'issuer': 'Synthetic fixture', 'url': 'https://example.org/reference',
            'edition': unknown, 'source_date': unknown, 'locator': 'Synthetic fixture',
            'coverage': 'Metadata testing only', 'retrieved_on': '2026-10-19',
            'rights_status': 'unresolved',
        }],
        'products': [{
            'id': 'synthetic-product', 'family': 'bulb',
            'source_key': 'synthetic-source', 'dimension_kind': 'supply_envelope',
        }],
        'sections': [{
            'id': 'synthetic-section', 'family': 'bulb',
            'source_key': 'synthetic-source', 'dimension_kind': 'discrete_catalogue',
            **dict.fromkeys(('width_mm', 'thickness_mm', 'area_mm2', 'Ix_mm4',
                             'Iy_mm4', 'Zx_mm3', 'Zy_mm3', 'J_mm4'), 1.0),
            'units': {'length': 'mm', 'area': 'mm2', 'inertia': 'mm4',
                      'section_modulus': 'mm3', 'torsion': 'mm4'},
            'property_basis': 'Nominal bare section; synthetic fixture',
            'axes': 'x and y are explicitly labelled synthetic principal axes',
            'material_grade': unknown, 'delivery_condition': unknown,
            'solver_eligibility': 'inapplicable_existing_panel_solver_has_no_bulb_geometry',
        }], 'type_coverage': {},
    }


def validate_schema(data):
    Draft202012Validator(SCHEMA, format_checker=FormatChecker()).validate(data)


def load_validator():
    spec = importlib.util.spec_from_file_location(
        'reference_validator', ROOT / 'scripts/studies/ship_structure_reference.py')
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_complete_unresolved_record_is_valid():
    Draft202012Validator.check_schema(SCHEMA)
    validate_schema(record())


@pytest.mark.parametrize('field', ['retrieved_on', 'rights_status', 'edition', 'locator'])
def test_source_provenance_is_required(field):
    data = record()
    del data['sources'][0][field]
    with pytest.raises(ValidationError):
        validate_schema(data)


@pytest.mark.parametrize('field', ['units', 'axes', 'property_basis', 'delivery_condition'])
def test_section_basis_is_required(field):
    data = record()
    del data['sections'][0][field]
    with pytest.raises(ValidationError):
        validate_schema(data)


@pytest.mark.parametrize('field,value', [
    ('observed_on', '2026-02-30'), ('publication_allowed', True),
    ('raw_sources_copied', True), ('schema_version', 1),
])
def test_invalid_root_metadata_is_rejected(field, value):
    data = record()
    data[field] = value
    with pytest.raises(ValidationError):
        validate_schema(data)


@pytest.mark.parametrize('field,value', [
    ('units', {'length': 'in'}), ('axes', ''), ('property_basis', ' '),
    ('width_mm', True), ('Ix_mm4', -1), ('dimension_kind', 'stock_confirmed'),
    ('solver_eligibility', 'qualified'),
])
def test_incompatible_section_metadata_is_rejected(field, value):
    data = record()
    data['sections'][0][field] = value
    with pytest.raises(ValidationError):
        validate_schema(data)


def test_unknown_requires_a_reason():
    data = record()
    data['sources'][0]['edition'] = {'state': 'unknown', 'reason': ''}
    with pytest.raises(ValidationError):
        validate_schema(data)


def test_relationship_validation_preserves_input():
    data = record()
    before = copy.deepcopy(data)
    assert load_validator().validate_research(data) is None
    assert data == before


@pytest.mark.parametrize('collection,field,value', [
    ('sources', 'key', 'synthetic-source'),
    ('products', 'id', 'synthetic-product'),
    ('sections', 'id', 'synthetic-section'),
])
def test_duplicate_identifiers_are_rejected(collection, field, value):
    data = record()
    duplicate = copy.deepcopy(data[collection][0])
    duplicate[field] = value
    data[collection].append(duplicate)
    with pytest.raises(ValueError, match='Duplicate'):
        load_validator().validate_research(data)


@pytest.mark.parametrize('collection', ['products', 'sections'])
def test_dangling_sources_are_rejected(collection):
    data = record()
    data[collection][0]['source_key'] = 'missing-source'
    with pytest.raises(ValueError, match='Unknown source'):
        load_validator().validate_research(data)


@pytest.mark.parametrize('value', [float('nan'), float('inf'), float('-inf')])
def test_non_json_numbers_are_rejected(value):
    data = record()
    data['sections'][0]['Ix_mm4'] = value
    with pytest.raises(ValueError):
        load_validator().validate_research(data)


def test_validation_cli_does_not_execute_study(capsys, monkeypatch):
    module = load_validator()
    monkeypatch.setattr(module, 'read_research', lambda _: record())
    assert module.main(['unused.json']) == 0
    assert json.loads(capsys.readouterr().out) == {
        'status': 'metadata_valid', 'engineering_status': 'NOT_EVALUATED',
        'publication_allowed': False,
    }


@pytest.mark.parametrize('contents', ['{"sources": [], "sources": []}', '{broken'])
def test_reader_rejects_ambiguous_or_malformed_json(tmp_path, contents):
    path = tmp_path / 'research.json'
    path.write_text(contents)
    with pytest.raises(ValueError):
        load_validator().read_research(path)


def test_reader_preserves_record(tmp_path):
    path = tmp_path / 'research.json'
    path.write_text(json.dumps(record()))
    assert load_validator().read_research(path) == record()


@pytest.mark.parametrize('failure', [ValueError('invalid'), OSError('unavailable')])
def test_cli_failure_is_nonzero_and_does_not_echo_input(capsys, monkeypatch, failure):
    module = load_validator()

    def fail(_):
        raise failure

    monkeypatch.setattr(module, 'read_research', fail)
    assert module.main(['unused.json']) == 2
    assert json.loads(capsys.readouterr().out) == {
        'status': 'invalid_metadata', 'engineering_status': 'NOT_EVALUATED',
    }


def test_cli_rejects_invalid_record(capsys, monkeypatch):
    module = load_validator()
    data = record()
    del data['sources'][0]['retrieved_on']
    monkeypatch.setattr(module, 'read_research', lambda _: data)
    assert module.main(['unused.json']) == 2
    assert json.loads(capsys.readouterr().out)['status'] == 'invalid_metadata'


@pytest.mark.parametrize('field,value', [
    ('retrieved_on', 'not-a-date'), ('url', 'relative/path'),
    ('catalog_source_id', None), ('rights_status', 'assumed_reusable'),
    ('edition', ' '), ('source_date', ' '),
])
def test_invalid_source_metadata_is_rejected(field, value):
    data = record()
    data['sources'][0][field] = value
    with pytest.raises(ValidationError):
        validate_schema(data)


@pytest.fixture(params=['default_formats', 'date_only'])
def offline_url_validator(request, monkeypatch):
    """Exercise authority validation without network or optional URI checking."""
    module = load_validator()
    if request.param == 'date_only':
        monkeypatch.setattr(module, 'FormatChecker', lambda: FormatChecker(formats=['date']))

    def reject_network(*args, **kwargs):
        pytest.fail('Source metadata validation attempted network access')

    monkeypatch.setattr(socket, 'socket', reject_network)
    monkeypatch.setattr(socket, 'getaddrinfo', reject_network)
    return module


@pytest.mark.parametrize('url', [
    'https://#source', 'https://?source', 'https://:443',
    'https://example.org:invalid/reference', 'https://example.org:-1/reference',
    'https://example.org:65536/reference', 'https://example.org:443:444/reference',
    'https://[2001:db8::1]:invalid/reference',
])
def test_source_url_rejects_missing_hostname_or_invalid_port(offline_url_validator, url):
    data = record()
    data['sources'][0]['url'] = url
    with pytest.raises((ValueError, ValidationError)):
        offline_url_validator.validate_research(data)


@pytest.mark.parametrize('url', [
    'http://example.org/reference', 'https://example.org',
    'https://example.org?edition=2#table',
    'https://example.org/reference?edition=2#table',
    'https://example.org:0/reference', 'https://example.org:65535/reference',
    'https://[2001:db8::1]:443/reference',
])
def test_source_url_accepts_hostname_and_valid_port_without_network(offline_url_validator, url):
    data = record()
    data['sources'][0]['url'] = url
    assert offline_url_validator.validate_research(data) is None


@pytest.mark.parametrize('url', ['https://#source', 'https://?source', 'https://:443'])
def test_cli_rejects_hostless_source_file(offline_url_validator, tmp_path, capsys, url):
    data = record()
    data['sources'][0]['url'] = url
    path = tmp_path / 'research.json'
    path.write_text(json.dumps(data), encoding='utf-8')
    assert offline_url_validator.main([str(path)]) == 2
    assert json.loads(capsys.readouterr().out) == {
        'status': 'invalid_metadata', 'engineering_status': 'NOT_EVALUATED',
    }
