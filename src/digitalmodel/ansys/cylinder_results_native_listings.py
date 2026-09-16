"""Observed v261 listing envelope; numeric E24 bytes are preserved unchanged."""
import re
from digitalmodel.ansys.cylinder_results import EvidenceError,parse_e24
from digitalmodel.ansys.cylinder_results_native_status import page_header
from digitalmodel.ansys.cylinder_results_native_values import summary_fields

STRESS = b'NODE SX SY SZ SXY SYZ SXZ'
DISPLACEMENT = b'NODE UX UY UZ USUM'


def _rows(raw):
    if not isinstance(raw,bytes) or not raw.endswith(b'\n'):
        raise EvidenceError('Missing/truncated native listing')
    return [line.rstrip(b'\r') for line in raw.split(b'\n')[:-1] if line.strip()]


def _words(row):
    return b' '.join(row.split())


def _page(rows,kind,element=False):
    page_header([_words(row) for row in rows[:4]])
    title=(b'ELEMENT NODAL STRESS' if element else b'NODAL STRESS') if kind=='stress' else b'NODAL DEGREE OF FREEDOM'
    frame=(b'THE FOLLOWING X,Y,Z VALUES ARE IN GLOBAL COORDINATES' if kind=='stress' else
           b'THE FOLLOWING DEGREE OF FREEDOM RESULTS ARE IN THE GLOBAL COORDINATE SYSTEM')
    expected=[b'***** POST1 '+title+b' LISTING *****',b'LOAD STEP= 1 SUBSTEP= 1',
              b'TIME= 1.0000 LOAD CASE= 0',frame]
    if [_words(r) for r in rows[4:8]]!=expected:
        raise EvidenceError('Native result set/frame/listing type differs')
    return rows[8:]


def _numeric(row,count,unit):
    if len(row)!=10+24*count or row[:2]!=b'  ' or not re.fullmatch(rb' *[1-9][0-9]* *',row[:10]):
        raise EvidenceError('Native node row width/identity differs')
    for i in range(count): parse_e24(row[10+i*24:34+i*24],unit)
    return f'{int(row[:10]):8d}'.encode()+row[10:]


def _summary(rows,label,count,nodes):
    if len(rows)<3 or _words(rows[0])!=label:
        raise EvidenceError('Missing native extrema summary')
    ids=rows[1].split()
    if len(ids)!=count+1 or ids[0]!=b'NODE':
        raise EvidenceError('Truncated native extrema summary')
    if any(not re.fullmatch(rb'[0-9]+',n) or int(n) not in nodes|{0} for n in ids[1:]):
        raise EvidenceError('Unknown summary node')
    summary_fields(rows[2], count)
    return rows[3:]


def normalize_nodal(raw,kind):
    rows=_rows(raw); stress=kind=='stress'; count=6 if stress else 4
    expected=b'PRINT S NODAL SOLUTION PER NODE' if stress else b'PRINT U NODAL SOLUTION PER NODE'
    if not rows or _words(rows[0])!=expected:raise EvidenceError('Unknown native nodal preamble')
    rows=_page(rows[1:],kind)
    header=STRESS if stress else DISPLACEMENT
    if not rows or _words(rows[0])!=header:raise EvidenceError('Unknown native component labels')
    rows=rows[1:];output=[];nodes=set()
    while rows and re.match(rb' *[0-9]',rows[0]):
        value=_numeric(rows[0],count,'MPa' if stress else 'mm')
        nodes.add(int(value[:8]));output.append(value if stress else value[:8+3*24]);rows=rows[1:]
    if not output:raise EvidenceError('Empty native nodal rows')
    if stress:
        rows=_summary(rows,b'MINIMUM VALUES',count,nodes)
        rows=_summary(rows,b'MAXIMUM VALUES',count,nodes)
        if not rows or _words(rows[0])!=b'***** ESTIMATED BOUNDS CONSIDERING THE EFFECT OF DISCRETIZATION ERROR *****':
            raise EvidenceError('Missing estimated-bounds display')
        rows=_summary(rows[1:],b'MINIMUM VALUES',count,nodes)
        rows=_summary(rows,b'MAXIMUM VALUES',count,nodes)
        if len(rows)!=1 or _words(rows[0])!=b'*'*75:raise EvidenceError('Unknown nodal trailing text')
    elif _summary(rows,b'MAXIMUM ABSOLUTE VALUES',count,nodes):
        raise EvidenceError('Unknown displacement trailing text')
    target=STRESS if stress else b'NODE UX UY UZ'
    return b'\n'.join([target,*output])+b'\n'


def normalize_presol(raw):
    rows=_rows(raw)
    if not rows or _words(rows[0])!=b'PRINT S ELEMENT SOLUTION PER ELEMENT':
        raise EvidenceError('Unknown native element preamble')
    rows=_page(rows[1:],'stress',True);output=[]
    while rows:
        if _words(rows[0]).startswith(b'*** MAPDL'):
            rows=_page(rows,'stress',True)
        if not rows:raise EvidenceError('Empty trailing element page')
        match=re.fullmatch(rb'ELEMENT= ([1-9][0-9]*) PLANE183',_words(rows[0]))
        if not match or len(rows)<6 or _words(rows[1])!=STRESS:
            raise EvidenceError('Unknown or truncated native element section')
        output.extend([b'ELEMENT= '+match[1],STRESS])
        output.extend(_numeric(row,6,'MPa') for row in rows[2:6])
        rows=rows[6:]
    return b'\n'.join(output)+b'\n'
