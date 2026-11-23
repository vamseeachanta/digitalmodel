"""Candidate generation fails closed and preserves the installation arrangement."""
import copy
import hashlib
import json
import math

import pytest
import yaml

from digitalmodel.citations.schema import CitationResolutionError
from digitalmodel.solvers.orcaflex.yaml_utils import OrcaFlexLoader, orcaflex_dump
from digitalmodel.workflows.structure_hydrodynamic_candidate import build_candidate, main


@pytest.fixture
def case(tmp_path):
    body = {'Name': 'Body', 'Mass': 2., 'Volume': .2, 'Height': .5,
            'CentreOfMass': [.1, 0, 0], 'CentreOfVolume': [0, 0, 0],
            'MomentsOfInertia': [1, 2, 3], 'DragArea': [1, 1, 1],
            'BuoyType': 'Lumped buoy', 'LumpedBuoyAddedMassMethod': 'Diagonal values',
            'DragAreaMoment': [10, 20, 30], 'AddedInertiaCoefficient': [.1, .2, .3]}
    model = {'General': {'UnitsSystem': 'SI'}, '6DBuoys': [body],
             'Environment': {'Density': 1.025},
             'Lines': [{'Name': 'Sling', 'Length': 3}]}
    path = tmp_path / 'source.yml'
    orcaflex_dump(model, path)
    citation = {'code_id': 'dnv-rp-h103', 'publisher': 'DNV', 'revision': '2011',
                'section': 'A-2; B-2; 4.6.3.3; 4.6.4.1', 'wiki_path': 'wikis/test/standard.md',
                'source_sibling': 'generic'}
    wiki = tmp_path / 'wiki'
    target = wiki / citation['wiki_path']
    target.parent.mkdir(parents=True)
    target.write_text('---\ncode_id: dnv-rp-h103\npublisher: DNV\nrevision: 2011\n---\n')
    cfg = {'body_name': 'Body', 'expected_source_properties': copy.deepcopy(body),
           'inputs': {'m_air': 2000, 'm_water': 1795, 'l': 2., 'w': 1., 'h': .5,
                      'cog': {'x': .1, 'y': 0, 'z': 0},
                      'cov': {'x': 0, 'y': 0, 'z': 0},
                      'perforation_ratio': {'x': 0, 'y': 0, 'z': 0}},
           'citations': [citation]}
    basis = {'schema_version': 1, 'status': 'project_assumption', 'units': 'm',
             'dimensions_m': {k: cfg['inputs'][k] for k in ('l', 'w', 'h')},
             'centres_m': {k: cfg['inputs'][k] for k in ('cog', 'cov')},
             'input_datum': 'body origin', 'model_datum': 'body origin',
             'translation_m': [0, 0, 0], 'rounding_tolerance_m': .001,
             'provenance': [{'source_id': 'fixture-design', 'sha256': 'a' * 64,
                             'locator': 'design dimensions and centres'}],
             'limitations': ['Illustrative geometry; no design qualification.']}
    basis_path = tmp_path / 'basis.json'
    basis_path.write_text(json.dumps(basis))
    cfg['geometry_basis'] = {'path': 'basis.json',
                             'sha256': hashlib.sha256(basis_path.read_bytes()).hexdigest()}
    return path, hashlib.sha256(path.read_bytes()).hexdigest(), tmp_path / 'out', cfg, wiki, model


def test_candidate_preserves_source_and_inertia(case):
    path, digest, output, cfg, wiki, before = case
    result = build_candidate(path, digest, output, cfg, wiki_root=wiki)
    actual = yaml.load((output / 'master.yml').read_text(), Loader=OrcaFlexLoader)
    assert actual['Lines'] == before['Lines']
    body = actual['6DBuoys'][0]
    assert body['MomentsOfInertia'] == [1, 2, 3]
    assert body['CentreOfMass'] == [.1, 0, 0]
    assert body['DragAreaMoment'] == [10, 20, 30]
    assert body['AddedInertiaCoefficient'] == [.1, .2, .3]
    assert body['HydrodynamicMass'] == [.205, .205, .205]
    assert body['DragArea'] == [.5, 1., 2.]
    # Independent hand calculation: H103 Table A-2 ratio 2; section 4.6.3.3.
    slenderness = math.sqrt(2) / (.5 + math.sqrt(2))
    added_mass_z = .757 * math.pi / 4 * 1025 * 2 * (
        1 + math.sqrt((1 - slenderness**2) / (2 * (1 + slenderness**2))))
    assert body['AddedMassCoefficient'][2] == pytest.approx(added_mass_z / 205)
    assert result['properties']['translational']['added_mass']['z'] == pytest.approx(added_mass_z)
    assert body['DragForceCoefficient'][2] == pytest.approx(1.17)
    assert 'splash' not in result['properties']['translational']['cd']
    assert hashlib.sha256(path.read_bytes()).hexdigest() == digest
    for entry in result['semantic_diff']:
        key = entry['field']
        if entry['present_before']:
            body[key] = entry['before']
        else:
            del body[key]
    assert actual == before
    assert result['analysis_executed'] is False
    assert result['acceptance_established'] is False
    assert json.loads((output / 'properties.json').read_text()) == result


@pytest.mark.parametrize('bad', ['digest', 'source', 'mass', 'dimension', 'perforation', 'scope'])
def test_bad_inputs_write_nothing(case, bad):
    path, digest, output, cfg, wiki, _ = case
    if bad == 'digest': digest = '0' * 64
    if bad == 'source': cfg['expected_source_properties']['Mass'] = 9
    if bad == 'mass': cfg['inputs']['m_water'] = 2100
    if bad == 'dimension': cfg['inputs']['h'] = 0
    if bad == 'perforation': cfg['inputs']['perforation_ratio']['z'] = 1.1
    if bad == 'scope': cfg['inputs']['perforation_ratio']['z'] = .51
    with pytest.raises(ValueError):
        build_candidate(path, digest, output, cfg, wiki_root=wiki)
    assert not output.exists()


@pytest.mark.parametrize('bad', ['missing', 'mismatch', 'empty', 'unrelated', 'section'])
def test_citations_fail_before_calculation(case, bad, monkeypatch):
    path, digest, output, cfg, wiki, _ = case
    if bad == 'missing': cfg['citations'][0]['wiki_path'] = 'wikis/missing.md'
    if bad == 'mismatch': cfg['citations'][0]['revision'] = '2'
    if bad == 'empty': cfg['citations'] = []
    if bad == 'unrelated': cfg['citations'][0]['code_id'] = 'another-code'
    if bad == 'section': cfg['citations'][0]['section'] = 'A-2'
    monkeypatch.setattr('digitalmodel.workflows.structure_hydrodynamic_candidate._calculate',
                        lambda cfg: pytest.fail('Calculation preceded citation validation'))
    with pytest.raises((CitationResolutionError, ValueError)):
        build_candidate(path, digest, output, cfg, wiki_root=wiki)
    assert not output.exists()


def test_density_mismatch_fails(case):
    path, _, output, cfg, wiki, model = case
    model['Environment']['Density'] = 1.0
    orcaflex_dump(model, path)
    digest = hashlib.sha256(path.read_bytes()).hexdigest()
    with pytest.raises(ValueError, match='density'):
        build_candidate(path, digest, output, cfg, wiki_root=wiki)
    assert not output.exists()


def test_existing_output_preserved(case):
    path, digest, output, cfg, wiki, _ = case
    output.mkdir()
    with pytest.raises(FileExistsError):
        build_candidate(path, digest, output, cfg, wiki_root=wiki)


def test_cli_uses_same_validation(case):
    path, digest, output, cfg, wiki, _ = case
    config_path = path.parent / 'config.yml'
    config_path.write_text(yaml.safe_dump(cfg))
    assert main(['--source', str(path), '--source-sha256', digest,
                 '--config', str(config_path), '--output', str(output),
                 '--wiki-root', str(wiki)]) == 0
    assert (output / 'master.yml').is_file()


def _replace_basis(case, update):
    path, _, _, cfg, _, _ = case
    target = path.parent / 'basis.json'
    record = json.loads(target.read_text())
    update(record)
    target.write_text(json.dumps(record))
    cfg['geometry_basis']['sha256'] = hashlib.sha256(target.read_bytes()).hexdigest()
    return record


def test_top_plate_datum_preserves_distinct_centres(case):
    path, digest, output, cfg, wiki, _ = case
    cfg['inputs']['cog']['z'] = -.25
    cfg['inputs']['cov']['z'] = -.25
    record = _replace_basis(case, lambda b: b.update(
        centres_m=copy.deepcopy({k: cfg['inputs'][k] for k in ('cog', 'cov')}),
        input_datum='top plate', translation_m=[0, 0, .25]))
    result = build_candidate(path, digest, output, cfg, wiki_root=wiki)
    assert result['geometry_basis']['record'] == record
    assert result['geometry_basis']['sha256'] == cfg['geometry_basis']['sha256']
    body = yaml.load((output / 'master.yml').read_bytes(), Loader=OrcaFlexLoader)['6DBuoys'][0]
    assert body['CentreOfMass'] == [.1, 0, 0]
    assert body['CentreOfVolume'] == [0, 0, 0]


@pytest.mark.parametrize('bad', ['missing', 'digest', 'length', 'width', 'centre',
                                'offset', 'datum', 'units', 'tolerance', 'provenance'])
def test_geometry_basis_fails_before_output(case, bad):
    path, digest, output, cfg, wiki, _ = case
    if bad == 'missing': cfg.pop('geometry_basis')
    elif bad == 'digest': cfg['geometry_basis']['sha256'] = 'f' * 64
    elif bad in ('length', 'width'): cfg['inputs'][{'length': 'l', 'width': 'w'}[bad]] += 1
    elif bad == 'centre': cfg['inputs']['cog']['x'] += .1
    else:
        changes = {'offset': {'translation_m': [0, 0, -.25]},
                   'datum': {'input_datum': ''}, 'units': {'units': 'ft'},
                   'tolerance': {'rounding_tolerance_m': .2},
                   'provenance': {'provenance': []}}
        _replace_basis(case, lambda b: b.update(changes[bad]))
    with pytest.raises(ValueError):
        build_candidate(path, digest, output, cfg, wiki_root=wiki)
    assert not output.exists()


def test_geometry_basis_change_during_calculation_fails(case, monkeypatch):
    from digitalmodel.workflows import structure_hydrodynamic_candidate as module
    path, digest, output, cfg, wiki, _ = case
    original = module._calculate
    def tamper(inputs):
        result = original(inputs)
        (path.parent / 'basis.json').write_text('{}')
        return result
    monkeypatch.setattr(module, '_calculate', tamper)
    with pytest.raises(ValueError, match='Geometry basis changed'):
        build_candidate(path, digest, output, cfg, wiki_root=wiki)
    assert not output.exists()


@pytest.mark.parametrize('section', ['A-25;B-2;4.6.3.3;4.6.4.1',
                                    'A-2;B-2;4.6.3.30;4.6.4.1'])
def test_citation_coverage_rejects_substring_sections(case, section):
    path, digest, output, cfg, wiki, _ = case
    cfg['citations'][0]['section'] = section
    with pytest.raises(ValueError, match='coverage'):
        build_candidate(path, digest, output, cfg, wiki_root=wiki)
    assert not output.exists()


def test_explicit_wiki_root_required(case):
    path, digest, output, cfg, _, _ = case
    with pytest.raises(ValueError, match='wiki_root'):
        build_candidate(path, digest, output, cfg, wiki_root=None)
    assert not output.exists()


def test_non_utf8_source_reports_named_encoding_precondition(case):
    path, _, output, cfg, wiki, _ = case
    path.write_bytes(path.read_bytes() + b'\n# degree: \xb0\n')
    with pytest.raises(ValueError, match='UTF-8'):
        build_candidate(path, hashlib.sha256(path.read_bytes()).hexdigest(), output, cfg, wiki_root=wiki)
    assert not output.exists()
