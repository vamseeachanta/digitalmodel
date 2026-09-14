"""Synthetic fixed-width MAPDL table grammar; native variants are not assumed."""
import pytest

from digitalmodel.ansys.cylinder_results import EvidenceError
from digitalmodel.ansys.cylinder_results_listings import parse_nodal_listing, parse_contribution_listing, extract_block
from tests.ansys.test_cylinder_results import station_metadata, e24


def table(nodes, displacement=False):
    header = 'NODE UX UY UZ' if displacement else 'NODE SX SY SZ SXY SYZ SXZ'
    vals = ['.5', '-.02', '0'] if displacement else ['-10','0','130','0','0','0']
    return (header+'\n'+''.join(f'{n:8d}'+''.join(e24(v) for v in vals)+'\n' for n in nodes)).encode()


def test_complete_nodal_listing_requires_labels_and_nine_nodes():
    stations=station_metadata()
    values=parse_nodal_listing(table(range(1,10)),stations,'stress')
    assert len(values)==36
    assert values['inner_y120','sigma_r']==e24('-10').encode()
    with pytest.raises(EvidenceError):
        parse_nodal_listing(table(range(1,9)),stations,'stress')


@pytest.mark.parametrize('fault',['header','wrap','lowprecision','duplicate','wrongunit'])
def test_listing_protocol_mutations_refuse(fault):
    raw=table(range(1,10))
    if fault=='header':raw=raw.replace(b'SX SY SZ',b'SX SZ SY')
    if fault=='wrap':raw=raw[:60]+b'\n'+raw[60:]
    if fault=='lowprecision':raw=raw.replace(e24('130').encode(),b'  1.30000000E+02'.rjust(24))
    if fault=='duplicate':raw+=raw.splitlines(keepends=True)[1]
    if fault=='wrongunit':raw=b'UNITS Pa\n'+raw
    with pytest.raises(EvidenceError):parse_nodal_listing(raw,station_metadata(),'stress')


def test_element_contributions_require_exact_adjacency_pairs():
    stations=station_metadata()
    raw=b''.join(f'ELEMENT = {e}\n'.encode()+table([s['node_id'] for s in stations
        if e in s['adjacent_element_ids']]) for e in range(1,5))
    result=parse_contribution_listing(raw,stations,element_ids={1,2,3,4})
    assert set(result['middle_y120'])=={1,2,3,4}
    with pytest.raises(EvidenceError):
        parse_contribution_listing(raw,stations,element_ids={1,2,3})


def test_marked_block_duplicates_and_missing_end_are_not_complete():
    raw=b'OCV_STRESS_BEGIN\n'+table(range(1,10))+b'OCV_STRESS_END\n'
    assert extract_block(raw,'STRESS')==table(range(1,10))
    with pytest.raises(EvidenceError):extract_block(raw+raw,'STRESS')
    with pytest.raises(EvidenceError):extract_block(raw[:-16],'STRESS')
