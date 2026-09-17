"""Retained N4 recovery grammar; fixture checks grant no engineering qualification."""
import hashlib
import json
from pathlib import Path

import pytest

from digitalmodel.ansys.cylinder_results import EvidenceError, parse_station_values, validate_recovery
from digitalmodel.ansys.cylinder_results_listings import parse_nodal_listing, parse_contribution_listing
from digitalmodel.ansys.cylinder_results_native_listings import normalize_nodal
from digitalmodel.ansys.cylinder_results_native_reactions import verify_reactions
from digitalmodel.ansys.cylinder_results_state import parse_support_reactions

FIXTURE = Path(__file__).parent / 'fixtures' / 'n4_recovery'


def data(name):
    return (FIXTURE / name).read_bytes()


def case():
    return json.loads(data('case.json'))


def test_fixture_hashes_bind_retained_numeric_bytes():
    raw = data('provenance.json')
    assert hashlib.sha256(raw).hexdigest() == '7b8c83810b5b51e9b6b48e7ab153a80efb776770315bf4ccb1759cbed44f1c02'
    for name, record in json.loads(raw)['files'].items():
        assert hashlib.sha256(data(name)).hexdigest() == record['sha256']


@pytest.mark.parametrize('name,kind', [('stress.txt', 'stress'), ('disp.txt', 'displacement')])
def test_original_fixed_width_nodal_summary_parses(name, kind):
    result = parse_nodal_listing(data(name), case()['stations'], kind)
    assert len(result) == (36 if kind == 'stress' else 18)


def test_retained_n4_recovery_matches_exports_and_element_contributions():
    metadata = case()
    values = parse_station_values(data('station_values.txt'), 'P10N4', metadata['stations'])
    listing = parse_nodal_listing(data('stress.txt'), metadata['stations'], 'stress')
    listing.update(parse_nodal_listing(data('disp.txt'), metadata['stations'], 'displacement'))
    contributions = parse_contribution_listing(data('presol.txt'), metadata['stations'],
        element_ids={e['element_id'] for e in metadata['elements']})
    validate_recovery(values, metadata['stations'], listing, contributions)
    assert len(values) == 63


def test_retained_n4_reactions_match_nine_exports_and_total():
    metadata = case()
    nodes = [n for n in metadata['nodes'] if n['node_id'] in metadata['bottom_node_ids']]
    reactions = parse_support_reactions(data('support_reactions.txt'), 'P10N4', nodes)
    verify_reactions(data('support.txt'), reactions['by_node'])
    assert str(reactions['sum_rfy_n']) == '-0.00100716186836650647'


@pytest.mark.parametrize('name,kind', [('stress.txt', 'stress'), ('disp.txt', 'displacement')])
@pytest.mark.parametrize('damage', ['short', 'long', 'prefix', 'precision', 'overflow', 'extra_row'])
def test_native_summary_damage_refuses(name, kind, damage):
    raw = data(name)
    row = next(v for v in raw.splitlines() if v.startswith(b' VALUE  '))
    changed = {'short': row[:-1], 'long': row + b' ', 'prefix': b' VALUE X' + row[8:],
        'precision': row[:10] + b'*' + row[11:], 'overflow': row[:8] + b'*' * 24 + row[32:],
        'extra_row': row + b'\n' + row}[damage]
    with pytest.raises(EvidenceError):
        normalize_nodal(raw.replace(row, changed, 1), kind)


@pytest.mark.parametrize('damage', ['short', 'extra', 'prefix', 'nonzero_fx', 'wrong_total'])
def test_native_reaction_total_damage_refuses(damage):
    metadata = case()
    nodes = [n for n in metadata['nodes'] if n['node_id'] in metadata['bottom_node_ids']]
    expected = parse_support_reactions(data('support_reactions.txt'), 'P10N4', nodes)['by_node']
    raw = data('support.txt')
    row = next(v for v in raw.splitlines() if v.startswith(b' VALUE  '))
    changed = {'short': row[:-1], 'extra': row + b' ', 'prefix': b' VALUE X' + row[8:],
        'nonzero_fx': row[:8] + b' 0.1000000000000000E+000' + row[32:],
        'wrong_total': row[:32] + b'-0.2000000000000000E-002'}[damage]
    with pytest.raises(EvidenceError):
        verify_reactions(raw.replace(row, changed, 1), expected)


def test_left_stripped_positive_summary_refuses():
    raw = data('stress.txt')
    rows = [row for row in raw.splitlines() if row.startswith(b' VALUE  ')]
    row = rows[1]
    assert b'E+003-' not in row
    from digitalmodel.ansys.cylinder_results_native_listings import _summary
    lines = [line for line in raw.splitlines() if line.strip()]
    index = lines.index(row)
    with pytest.raises(EvidenceError):
        _summary([lines[index - 2], lines[index - 1], row.lstrip()],
                 b'MAXIMUM VALUES', 6, {s['node_id'] for s in case()['stations']})


@pytest.mark.parametrize('field,message', [
    (b' 0.1000000000000000E+300', 'Exponent outside'),
    (b'0.1000000000E+000'.rjust(24), 'Missing E24.16 precision'),
])
def test_width_preserving_numeric_refusals(field, message):
    from digitalmodel.ansys.cylinder_results_native_values import summary_fields
    assert len(field) == 24
    with pytest.raises(EvidenceError, match=message):
        summary_fields(b' VALUE  ' + field * 2, 2)
