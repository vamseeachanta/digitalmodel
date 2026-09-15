"""Bounded PLANE183 CDB reader: database bytes are data, never executed commands.

NBLOCK/EBLOCK/ETBLOCK grammar follows the existing pressure-CDB reader and
Ansys Programmer's Reference Coded Database File Commands. Q8 fixtures are synthetic. Retained native CDB observations additionally inform
the explicit zero-default grammar; full result acceptance remains separate.
"""
from decimal import Decimal

from digitalmodel.ansys.cylinder_results import EvidenceError, decimal_value, decimal_context


def _require(condition, message):
    if not condition:
        raise EvidenceError(message)


def _parts(line):
    return [p.strip().upper() for p in line.split('!', 1)[0].split(',')]


def _fields(line, widths):
    _require(not line[sum(widths):].strip(), 'Unexpected CDB trailing fields')
    line = line.ljust(sum(widths))
    values, start = [], 0
    for width in widths:
        values.append(line[start:start+width].strip())
        start += width
    return values


def _block(lines, index, fmt, terminator):
    _require(index < len(lines) and lines[index].strip().lower() == fmt,
             'Unsupported CDB block format')
    index += 1
    rows = []
    while index < len(lines) and _parts(lines[index]) != terminator:
        _require(bool(lines[index].strip()), 'Blank CDB block row')
        rows.append(lines[index])
        index += 1
    _require(index < len(lines), 'Missing CDB block terminator')
    return rows, index+1


def _nodes(header, rows):
    _require(header[:3] == ['NBLOCK','6','SOLID'] and len(header) == 5,
             'Unsupported NBLOCK declaration')
    result = {}
    for row in rows:
        fields = _fields(row, [9]*3+[21]*6)
        node, solid, location = map(int, fields[:3])
        # NBLOCK omits trailing zero coordinates (v242 Programmer's Reference).
        # A missing nonzero Y still refuses in verify_model against frozen nodes.
        _require(fields[3] and (fields[4] or not any(fields[5:])),
                 'Native node requires explicit X and Y unless trailing zeros omitted')
        xyz = [decimal_value(v or '0') for v in fields[3:]]
        _require(node > 0 and node not in result and not solid and not location,
                 'Invalid/duplicate node or unsupported flags')
        _require(not any(xyz[2:]), 'Nonplanar or rotated native node')
        result[node] = tuple(xyz[:2])
    _require(result and [max(result),len(result)] == list(map(int,header[3:])),
             'NBLOCK count/max mismatch')
    return result


def _elements(header, rows):
    _require(header[:3] == ['EBLOCK','19','SOLID'] and len(header) == 5,
             'Unsupported EBLOCK declaration')
    result = {}
    for row in rows:
        values = list(map(int, _fields(row, [10]*19)))
        material, kind, real, section, esys = values[:5]
        _require(material == kind == 1 and real in (0,1) and section in (0,1)
                 and esys == 0 and values[5:10] == [0,0,0,8,0],
                 'Unsupported Q8 element attributes')
        element, nodes = values[10], tuple(values[11:])
        _require(element > 0 and element not in result and len(set(nodes)) == 8,
                 'Duplicate or degenerate Q8 element')
        result[element] = nodes
    _require(result and [max(result),len(result)] == list(map(int,header[3:])),
             'EBLOCK count/max mismatch')
    return result


def _inert(parts):
    fixed = {'','/PREP7','/NOPR','/GO','FINISH','DOF,DELETE','*ELSE','*ENDIF',
             '*IF,_CDRDOFF,EQ,1,THEN','_CDRDOFF=','BFUNIF,TEMP,_TINY',
             'ERESX,DEFA','EXTOPT,ACLEAR,0','EXTOPT,ATTR,0,0,0'}
    if ','.join(parts) in fixed or parts[0] in ('/COM','/TITLE'):
        return True
    # Current OMEGA grammar has three components; retain the prior all-zero
    # fourth-field fixture only. No nonzero legacy option is accepted.
    if parts[0] == 'OMEGA':
        return len(parts) in (4,5) and all(decimal_value(v)==0 for v in parts[1:])
    zero_counts = {'ACEL':3,'DOMEGA':3,'CGLOC':3,'CGOMEGA':3,
                   'DCGOMG':3,'TREF':1,'IRLF':1,'KUSE':1,'TIME':1,'ALPHAD':1,
                   'BETAD':1,'DMPRAT':1,'DMPSTR':1,'NEQIT':1}
    if parts[0] in zero_counts:
        return len(parts)==zero_counts[parts[0]]+1 and all(decimal_value(v)==0 for v in parts[1:])
    if parts[:2] == ['EXTOPT','ESIZE']:
        return len(parts)==4 and all(decimal_value(v)==0 for v in parts[2:])
    if parts[0] == 'CRPLIM':
        return len(parts)==3 and (decimal_value(parts[1]),int(parts[2])) in ((Decimal('.1'),0),(0,1))
    if parts[0] == 'NCNV':
        return [decimal_value(v) for v in parts[1:]] == [1,0,0,0,0]
    if parts[:2] in (['*SET','_RETURN'],['*SET','_STATUS']):
        return len(parts)==3 and decimal_value(parts[2])==0
    return False


def _material(parts, materials):
    _require(len(parts)==8 and parts[:3]==['MPDATA','UNBL','1']
             and parts[4:6]==['1','1'] and parts[7]=='', 'Unsupported MPDATA')
    key = parts[3]
    _require(key in ('EX','NUXY','PRXY') and key not in materials,
             'Unexpected/duplicate material property')
    materials[key] = decimal_value(parts[6])
    _require(materials[key] == (200000 if key=='EX' else Decimal('.3')),
             'Native material differs from fixed canary')


def _read(lines):
    formats = {'ETBLOCK':('(2i9,19a9)',['-1']),
               'NBLOCK':('(3i9,6e21.13e3)',['N','UNBL','LOC','-1','']),
               'EBLOCK':('(19i10)',['-1'])}
    blocks, materials, offsets, index = {}, {}, {}, 0
    while index < len(lines):
        parts = _parts(lines[index]); index += 1
        command = parts[0]
        if command in formats:
            _require(command not in blocks, 'Duplicate CDB block')
            rows, index = _block(lines,index,*formats[command])
            blocks[command] = (parts,rows)
        elif command=='MPDATA':
            _material(parts,materials)
        elif command=='MPTEMP':
            _require(len(parts)==6 and parts[:4]==['MPTEMP','UNBL','1','1']
                     and decimal_value(parts[4])==0 and parts[5]=='', 'Unsupported material temperature')
        elif command=='NUMOFF':
            _require(len(parts)==3 and parts[1] not in offsets, 'Duplicate/invalid NUMOFF')
            offsets[parts[1]] = int(parts[2])
        else:
            _require(_inert(parts), f'Unsupported CDB command {command}')
    _require(set(blocks)==set(formats), 'Missing required native blocks')
    _require('EX' in materials and ('PRXY' in materials or 'NUXY' in materials),
             'Missing native elastic properties')
    return blocks, materials, offsets


def parse_model_cdb(raw):
    """Parse an unloaded pre-solve database; load/support checks are separate."""
    try:
        _require(isinstance(raw,bytes) and raw.endswith(b'\n'), 'Missing/truncated CDB')
        text = raw.decode('ascii',errors='strict')
        _require('\x00' not in text and '$' not in text, 'Invalid CDB control text')
        lines = text.splitlines()
        _require(lines[0].startswith('/COM,ANSYS RELEASE ') and lines[-1].strip()=='FINISH',
                 'Missing native CDB release/completion envelope')
        blocks, materials, offsets = _read(lines)
        header, rows = blocks['ETBLOCK']
        _require(header==['ETBLOCK','1','1'] and len(rows)==1, 'Unsupported ETBLOCK')
        _require(list(map(int,_fields(rows[0],[9]*21)))==[1,183,0,0,1]+[0]*16,
                 'Native type/keyopts differ from fixed PLANE183')
        nodes, elements = _nodes(*blocks['NBLOCK']), _elements(*blocks['EBLOCK'])
        used = set().union(*(set(e) for e in elements.values()))
        _require(used==set(nodes), 'Unused or undefined native node')
        allowed = {'NODE':max(nodes),'ELEM':max(elements),'MAT':1,'TYPE':1,'REAL':1}
        _require(all(k in allowed and v==allowed[k] for k,v in offsets.items()), 'Unexpected offsets')
        return {'nodes':nodes,'elements':elements,'materials':materials,'release_header':lines[0]}
    except (UnicodeError,ValueError,IndexError,KeyError) as exc:
        raise EvidenceError(str(exc)) from exc


@decimal_context
def verify_model(native, case):
    """Compare native mesh to the independently validated approved case mesh."""
    expected_nodes = {n['node_id']: n for n in case['nodes']}
    expected_elements = {e['element_id']: tuple(e['nodes']) for e in case['elements']}
    _require(len(expected_nodes)==len(case['nodes']) and len(expected_elements)==len(case['elements']),
             'Duplicate expected model identity')
    _require(set(native['nodes'])==set(expected_nodes) and native['elements']==expected_elements,
             'Native node set or Q8 connectivity differs from frozen case')
    for node, (x,y) in native['nodes'].items():
        expected = expected_nodes[node]
        _require(abs(x-decimal_value(expected['x_mm']))<=Decimal('1e-9')
                 and abs(y-decimal_value(expected['y_mm']))<=Decimal('1e-9'),
                 'Native node coordinate differs from frozen mesh')
