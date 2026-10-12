"""Synthetic neutral representations of observed paginated v261 listings."""
import pytest
from digitalmodel.ansys.cylinder_results import EvidenceError
from digitalmodel.ansys.cylinder_results_native_listings import normalize_nodal, normalize_presol


def page(kind='stress',element=False):
    title=('ELEMENT NODAL STRESS' if element else 'NODAL STRESS') if kind=='stress' else 'NODAL DEGREE OF FREEDOM'
    frame='THE FOLLOWING X,Y,Z VALUES ARE IN GLOBAL COORDINATES' if kind=='stress' else 'THE FOLLOWING DEGREE OF FREEDOM RESULTS ARE IN THE GLOBAL COORDINATE SYSTEM'
    return ('*** MAPDL - ENGINEERING ANALYSIS SYSTEM RELEASE 2026 R1.01 26.1 ***\n'
        'Ansys Mechanical Enterprise\n00000000 VERSION=WINDOWS x64 01:00:00 SEP 14, 2026 Elapsed Time= 1.0\n'
        'Open cylinder verification CTRL16\n'+f'***** POST1 {title} LISTING *****\n'
        'LOAD STEP= 1 SUBSTEP= 1\nTIME= 1.0000 LOAD CASE= 0\n'+frame+'\n').encode()


def row(node=1,count=6):
    return f'{node:10d}'.encode()+b' 0.0000000000000000E+000'*count+b'\n'


def summary(label,count=6):
    return label+b'\nNODE '+b'1 '*count+b'\n VALUE  '+b' 0.0000000000000000E+000'*count+b'\n'


def nodal(kind='stress'):
    stress=kind=='stress'; count=6 if stress else 4
    raw=(b'PRINT S NODAL SOLUTION PER NODE\n' if stress else b'PRINT U NODAL SOLUTION PER NODE\n')+page(kind)
    raw+=(b'NODE SX SY SZ SXY SYZ SXZ\n' if stress else b'NODE UX UY UZ USUM\n')+row(count=count)
    if not stress:return raw+summary(b'MAXIMUM ABSOLUTE VALUES',count)
    raw+=summary(b'MINIMUM VALUES')+summary(b'MAXIMUM VALUES')
    raw+=b'***** ESTIMATED BOUNDS CONSIDERING THE EFFECT OF DISCRETIZATION ERROR *****\n'
    return raw+summary(b'MINIMUM VALUES')+summary(b'MAXIMUM VALUES')+b'***************************************************************************\n'


def presol():
    raw=b'PRINT S ELEMENT SOLUTION PER ELEMENT\n'+page(element=True)
    for element in (1,2):
        if element==2:raw+=page(element=True)
        raw+=f'ELEMENT= {element} PLANE183\nNODE SX SY SZ SXY SYZ SXZ\n'.encode()
        raw+=b''.join(row(n) for n in (1,2,3,4))
    return raw


def test_native_nodal_and_paginated_element_layouts_preserve_e24_fields():
    assert normalize_nodal(nodal(),'stress').splitlines()[1]==row()[2:-1]
    assert normalize_nodal(nodal('displacement'),'displacement').splitlines()[0]==b'NODE UX UY UZ'
    raw=normalize_presol(presol())
    assert raw.count(b'ELEMENT=')==2 and b'PLANE183' not in raw


@pytest.mark.parametrize('damage',['frame','loadstep','precision','duplicate_summary','unknown','wrong_element'])
def test_native_layout_mutations_refuse(damage):
    raw=presol() if damage=='wrong_element' else nodal()
    if damage=='frame':raw=raw.replace(b'GLOBAL COORDINATES',b'LOCAL COORDINATES')
    elif damage=='loadstep':raw=raw.replace(b'SUBSTEP= 1',b'SUBSTEP= 2')
    elif damage=='precision':raw=raw.replace(b'0.0000000000000000E+000',b'0.00000000E+000')
    elif damage=='duplicate_summary':raw+=summary(b'MINIMUM VALUES')
    elif damage=='wrong_element':raw=raw.replace(b'PLANE183',b'PLANE182')
    else:raw+=b'UNKNOWN DATA\n'
    with pytest.raises(EvidenceError):
        normalize_presol(raw) if damage=='wrong_element' else normalize_nodal(raw,'stress')


def test_native_node_separator_spaces_preserve_numeric_payload():
    raw=nodal().replace(row(),b' '+f'{1:8d}'.encode()+b' '+row()[10:])
    assert normalize_nodal(raw,'stress').splitlines()[1]==row()[2:-1]
