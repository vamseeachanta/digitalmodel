"""Eleven public acceptance tests; all catalogue values are synthetic assumptions."""
from copy import deepcopy
import hashlib
import shutil

import pytest
import yaml
from pydantic import ValidationError

from digitalmodel.bha_database import BhaDatabaseError, load_database
from digitalmodel.bha_database.loader import DEFAULT_ROOT
from digitalmodel.bha_database.schema import Connection


@pytest.fixture
def catalogue(tmp_path):
    root = tmp_path / 'database'
    shutil.copytree(DEFAULT_ROOT, root)
    return root


def read(root):
    return yaml.safe_load((root / 'examples.yml').read_text())


def write(root, doc):
    raw = yaml.safe_dump(doc, sort_keys=False).encode()
    (root / 'examples.yml').write_bytes(raw)
    manifest = {'schema_version': 1, 'files': {'examples.yml': {
        'sha256': hashlib.sha256(raw).hexdigest(),
        **{key: len(doc[key]) for key in ('sources', 'components', 'assemblies')},
    }}}
    (root / 'manifest.yaml').write_text(yaml.safe_dump(manifest))


def test_pilot():
    db = load_database()
    result = db.summary('SYNTHETIC-PENDULUM')
    assert result['total_length_m']['value'] == pytest.approx(91 * .3048)
    assert result['air_weight_N']['value'] > result['buoyed_weight_N']['value'] > 0
    assert len(db.sections('SYNTHETIC-PENDULUM')) == 4
    assert not result['warnings']


def test_three_synthetic_examples():
    db = load_database()
    assert len(db.assemblies) == 3
    for key, assembly in db.assemblies.items():
        assert assembly.data_class == 'SYNTHETIC-EXAMPLE'
        assert 'SYNTHETIC' in assembly.name
        bit = db.components[assembly.items[0].component_id]
        assert bit.od.to('m') == pytest.approx(assembly.hole_size.to('m'))
        assert bit.top_connection.gender == 'pin'
        assert not db.summary(key)['warnings']
    assert db.components['SYN-MOTOR'].bottom_connection.gender == 'box'
    with pytest.raises(BhaDatabaseError, match='unknown bha_id'):
        db.summary('missing')


def test_tamper_and_bounded_files(catalogue):
    path = catalogue / 'examples.yml'
    original = path.read_bytes()
    path.write_bytes(original + b'\n')
    with pytest.raises(BhaDatabaseError, match='examples.yml'):
        load_database(catalogue)
    path.write_bytes(b' ' * (1024 * 1024 + 1))
    with pytest.raises(BhaDatabaseError, match='1 MiB'):
        load_database(catalogue)
    path.write_bytes(original)
    (catalogue / 'extra.yml').write_text('sources: []')
    with pytest.raises(BhaDatabaseError, match='unexpected'):
        load_database(catalogue)


def test_provenance(catalogue):
    original = read(catalogue)
    for field, value in [('source_key', 'missing'), ('basis', 'computed'),
                         ('unit', 'second'), ('value', float('nan')),
                         ('as_published', ''), ('as_published_on', '   ')]:
        doc = deepcopy(original)
        doc['components'][0]['od'][field] = value
        write(catalogue, doc)
        with pytest.raises(BhaDatabaseError):
            load_database(catalogue)
    doc = deepcopy(original)
    doc['components'][1]['length']['value'] = 1e308
    doc['components'][1]['length']['unit'] = 'mile'
    write(catalogue, doc)
    with pytest.raises(BhaDatabaseError):
        load_database(catalogue)
    doc = deepcopy(original)
    del doc['components'][0]['od']['unit']
    write(catalogue, doc)
    with pytest.raises(BhaDatabaseError):
        load_database(catalogue)


def test_strict_keys_duplicates_and_yaml(catalogue):
    original = read(catalogue)
    for change in ('unknown', 'duplicate', 'boolean', 'source_duplicate'):
        doc = deepcopy(original)
        if change == 'unknown':
            doc['components'][0]['unexpected'] = 1
        elif change == 'duplicate':
            doc['components'].append(doc['components'][0])
        elif change == 'boolean':
            doc['assemblies'][0]['items'][0]['count'] = True
        else:
            doc['sources'].append(doc['sources'][0])
        write(catalogue, doc)
        with pytest.raises(BhaDatabaseError):
            load_database(catalogue)
    for label in ('NC24', 'NC27', 'NC50extra', 'NC50\n', ' NC50'):
        doc = deepcopy(original)
        doc['components'][0]['top_connection']['label'] = label
        write(catalogue, doc)
        with pytest.raises(ValidationError, match='label'):
            Connection(label=label, gender='pin')
        with pytest.raises(BhaDatabaseError):
            load_database(catalogue)
    for text in ('schema_version: 1\nschema_version: 1\n', 'a: &x []\nb: *x\n'):
        (catalogue / 'examples.yml').write_text(text)
        with pytest.raises(BhaDatabaseError):
            load_database(catalogue)


def test_geometry(catalogue):
    original = read(catalogue)
    for field, value in [('id', 20), ('length', -1), ('od_max', 1)]:
        doc = deepcopy(original)
        doc['components'][1][field]['value'] = value
        write(catalogue, doc)
        with pytest.raises(BhaDatabaseError):
            load_database(catalogue)
    doc = deepcopy(original)
    doc['assemblies'][0]['items'][1]['length_override'] = deepcopy(doc['components'][1]['length'])
    doc['assemblies'][0]['items'][1]['length_override']['value'] = -1
    write(catalogue, doc)
    with pytest.raises(BhaDatabaseError):
        load_database(catalogue)


    for override in (False, True):
        doc = deepcopy(original)
        if override:
            a = next(a for a in doc['assemblies'] if a['purpose'] == 'building')
            a['items'][1]['length_override'] = deepcopy(doc['components'][0]['length'])
        else:
            c = next(c for c in doc['components'] if c['component_id'] == 'SYN-NB')
            c['blade_bottom_to_component_bottom']['value'] = 1000
        write(catalogue, doc)
        with pytest.raises(BhaDatabaseError):
            load_database(catalogue)


    for missing in ('blade_length', 'blade_bottom_to_component_bottom'):
        doc = deepcopy(original)
        c = next(c for c in doc['components'] if c['component_id'] == 'SYN-NB')
        del c[missing]
        write(catalogue, doc)
        with pytest.raises(BhaDatabaseError):
            load_database(catalogue)


def test_units_roundtrip():
    db = load_database()
    for component in db.components.values():
        for field in ('od', 'id', 'length'):
            q = getattr(component, field)
            from digitalmodel.bha_database.schema import UNITS
            assert UNITS.Quantity(q.to('m'), 'm').to(q.unit).magnitude == pytest.approx(q.value, abs=1e-9)


def test_rules(catalogue):
    original = read(catalogue)
    for rule in range(1, 7):
        doc = deepcopy(original)
        a = next(a for a in doc['assemblies'] if a['purpose'] == 'building')
        by_id = {c['component_id']: c for c in doc['components']}
        if rule == 1:
            a['items'][0]['component_id'] = 'SYN-DC'
        elif rule == 2:
            # Recreate the drawing error: MWD below a near-bit stabilizer.
            a['items'][1], a['items'][2] = a['items'][2], a['items'][1]
            for i, item in enumerate(a['items'], 1):
                item['pos'] = i
        elif rule == 3:
            by_id['SYN-NB']['blade_od']['value'] = 13
        elif rule == 4:
            a['items'][-1], a['items'][-2] = a['items'][-2], a['items'][-1]
            for i, item in enumerate(a['items'], 1):
                item['pos'] = i
        elif rule == 5:
            by_id['SYN-DC']['top_connection']['label'] = 'NC46'
        else:
            a['items'][1]['component_id'] = 'missing'
        write(catalogue, doc)
        with pytest.raises(BhaDatabaseError, match=f'R{rule}') as error:
            load_database(catalogue)
        if rule == 2:
            assert 'directly' in str(error.value) and 'bit' in str(error.value)
    check_bit_hole_geometry(catalogue, original)
    # A crossover is a physical adapter, never a waiver of its interfaces.
    doc = deepcopy(original)
    doc['components'][1]['type'] = 'crossover_sub'
    doc['components'][1]['bottom_connection']['label'] = 'NC46'
    write(catalogue, doc)
    with pytest.raises(BhaDatabaseError, match='R5'):
        load_database(catalogue)


def check_bit_hole_geometry(catalogue, original):
    for factor in (1 + 2e-9, 1 - 2e-9, 13 / 12):
        doc = deepcopy(original)
        doc['assemblies'][0]['hole_size']['value'] = 12 * factor
        write(catalogue, doc)
        with pytest.raises(BhaDatabaseError, match='R3.*bit.*hole'):
            load_database(catalogue)
    for factor in (1.0, 1 + 0.5e-9, 1 - 0.5e-9):
        doc = deepcopy(original)
        doc['assemblies'][0]['hole_size'].update(value=0.3048 * factor, unit='m')
        write(catalogue, doc)
        load_database(catalogue)


def test_calcs_count_override_and_blade(catalogue):
    doc = read(catalogue)
    a = doc['assemblies'][0]
    a['items'][2]['count'] = 2
    a['items'][2]['length_override'] = deepcopy(doc['components'][1]['length'])
    a['items'][2]['length_override']['value'] = 20
    write(catalogue, doc)
    db = load_database(catalogue)
    summary = db.summary(a['bha_id'])
    assert summary['total_length_m']['value'] == pytest.approx(101 * .3048)
    assert summary['air_weight_N']['value'] == pytest.approx(101 * 80 * 4.4482216152605)
    assert summary['buoyed_weight_N']['value'] == pytest.approx(
        101 * 80 * 4.4482216152605 * (1 - 10 / 65.5), rel=.001)
    assert summary['max_od_m']['value'] == pytest.approx(12 * .0254)
    sections = db.sections('SYNTHETIC-BUILDING')
    assert sections[1]['blade_centre_from_bit']['value'] == pytest.approx(2 * .3048)
    assert sections[-1]['distance_from_bit']['value'] > sections[1]['distance_from_bit']['value']
    assert summary['air_weight_N']['source_keys']
    doc['sources'].append({**doc['sources'][0], 'key': 'OTHER-SYNTHETIC'})
    doc['components'][0]['od']['source_key'] = 'OTHER-SYNTHETIC'
    write(catalogue, doc)
    sections = load_database(catalogue).sections('SYNTHETIC-PENDULUM')
    assert sections[0]['od']['source_keys'] == ['OTHER-SYNTHETIC']
    assert 'OTHER-SYNTHETIC' not in sections[1]['od']['source_keys']



def test_r7_warning_and_missing_input(catalogue):
    doc = read(catalogue)
    doc['assemblies'][0]['planned_wob']['value'] = 1e8
    write(catalogue, doc)
    result = load_database(catalogue).summary('SYNTHETIC-PENDULUM')
    assert result['r7']['state'] == 'EXCEEDED'
    assert 'R7' in result['warnings'][0] and 'available' in result['warnings'][0]
    assert result['r7']['available_wob_N']['value'] == pytest.approx(
        31 * 80 * 4.4482216152605 * (1 - 10 / 65.5) * .85)
    del doc['assemblies'][0]['mud_weight_basis']
    write(catalogue, doc)
    result = load_database(catalogue).summary('SYNTHETIC-PENDULUM')
    assert result['r7']['state'] == 'NOT_EVALUATED'
    assert 'R7' in result['warnings'][0]
    assert result['buoyed_weight_N'] is None


def test_public_loader_refuses_restricted_rows(catalogue):
    original = read(catalogue)
    for table in ('components', 'assemblies'):
        for marker in ('CLIENT-PRIVATE', 'client-private', 'CLIENT-PRIVATE '):
            doc = deepcopy(original)
            doc[table][0]['data_class'] = marker
            write(catalogue, doc)  # Valid hash must never bypass the classification gate.
            with pytest.raises(
                BhaDatabaseError, match='CLIENT-PRIVATE' if marker == 'CLIENT-PRIVATE' else None,
            ):
                load_database(catalogue)
    # Outgoing rows contain no e-mail, local absolute path or copied source URL.
    for row in original['components'] + original['assemblies']:
        text = yaml.safe_dump(row)
        assert '@' not in text and '/home/' not in text and 'https://' not in text
