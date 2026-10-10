"""Synthetic Q8 CDB records based on the separately documented block grammar."""
import pytest

from digitalmodel.ansys.cylinder_results import EvidenceError
from digitalmodel.ansys.cylinder_results_cdb import parse_model_cdb, verify_model


def model_fixture():
    xy=[(750,0),(810,0),(810,240),(750,240),(780,0),(810,120),(780,240),(750,120)]
    nodes=''.join(''.join(f'{v:9d}' for v in [n,0,0])+''.join(f'{v:21.13E}' for v in [x,y,0])+'\n'
                  for n,(x,y) in enumerate(xy,1))
    text=('/COM,ANSYS RELEASE 2026 R1.01\n/PREP7\nETBLOCK,1,1\n(2i9,19a9)\n'+
          ''.join(f'{v:9d}' for v in [1,183,0,0,1]+[0]*16)+'\n-1\n'+
          'NBLOCK,6,SOLID,8,8\n(3i9,6e21.13e3)\n'+nodes+'N,UNBL,LOC,-1,\n'+
          'EBLOCK,19,SOLID,1,1\n(19i10)\n'+''.join(f'{v:10d}' for v in
              [1,1,0,0,0,0,0,0,8,0,1,1,2,3,4,5,6,7,8])+'\n-1\n'+
          'MPTEMP,UNBL,1,1,0,\nMPDATA,UNBL,1,EX,1,1,200000,\n'+
          'MPDATA,UNBL,1,PRXY,1,1,.3,\nMPDATA,UNBL,1,NUXY,1,1,.3,\nFINISH\n')
    expected={'nodes':[dict(node_id=n,x_mm=str(x),y_mm=str(y)) for n,(x,y) in enumerate(xy,1)],
              'elements':[dict(element_id=1,nodes=list(range(1,9)))]}
    return text.encode(),expected


def test_q8_native_records_are_parsed_and_compared_to_expected_mesh():
    raw,expected=model_fixture();native=parse_model_cdb(raw)
    verify_model(native,expected)
    assert len(native['nodes'])==8


@pytest.mark.parametrize('fault',['material','type','node','unknown','truncated','keyopt','midside'])
def test_model_mismatch_never_becomes_verified(fault):
    raw,expected=model_fixture()
    if fault=='material':raw=raw.replace(b'200000',b'205000')
    if fault=='type':raw=raw.replace(b'      183',b'      182')
    if fault=='node':expected['nodes'][0]['x_mm']='751'
    if fault=='unknown':raw=raw.replace(b'FINISH',b'UNKNOWN,1\nFINISH')
    if fault=='truncated':raw=raw[:-7]
    if fault=='keyopt':raw=raw.replace(b'      183        0        0        1',b'      183        0        0        2')
    if fault=='midside':expected['elements'][0]['nodes'][-1]=7
    with pytest.raises(EvidenceError):verify_model(parse_model_cdb(raw),expected)
