"""Read coherent unaveraged PRESOL tensors; reject incomplete point coverage."""
from pathlib import Path
import math
import re

FLOAT = re.compile(r'[+-]?(?:\d+\.\d*|\.\d+|\d+)(?:[EeDd][+-]?\d+)?')
ELEMENT = re.compile(r'^\s*ELEMENT\s*=\s*(\d+)\b')
ROW = re.compile(r'^\s*(\d+)\s+(.+?)\s*$')
COMPONENTS = ('sx','sy','sz','sxy','syz','sxz')
SURFACE = re.compile(r'^\s*(?:(?:SHELL|SURFACE)\s*=?\s*)?(TOP|BOTTOM|BOT|MID)\s*$', re.I)
SHELL_FORMAT = 'SHELL RESULTS FOR TOP/BOTTOM ALSO MID WHERE APPROPRIATE'


def _numbers(text):
    matches = list(FLOAT.finditer(text))
    residue = FLOAT.sub('',text)
    if residue.strip() or not matches:
        raise ValueError('Non-numeric, overflow or nonfinite stress field')
    for first,second in zip(matches,matches[1:]):
        if first.end() == second.start() and not re.search(r'[EeDd]', first.group()):
            raise ValueError('Ambiguous adjacent fields or E-less Fortran exponent')
    values = [float(m.group().replace('D','E').replace('d','e')) for m in matches]
    if not all(math.isfinite(v) for v in values):
        raise ValueError('Nonfinite stress value')
    return values


def _block(lines,element_id):
    rows = []
    pending = None
    surface = None
    for line in lines:
        stripped = line.strip()
        if not stripped or 'VERSION=' in line:
            continue
        label = SURFACE.match(line)
        if label:
            if pending is not None:
                raise ValueError('Surface label interrupts tensor')
            surface = {'bot':'bottom'}.get(label[1].lower(),label[1].lower())
            continue
        match = ROW.match(line)
        if match:
            if pending is not None:
                raise ValueError('Missing wrapped stress continuation')
            pending = [int(match[1]),_numbers(match[2])]
        elif pending is not None:
            pending[1].extend(_numbers(line))
        elif stripped[0] in '+-.0123456789' or 'NaN' in stripped or 'Inf' in stripped:
            raise ValueError('Orphan stress continuation or nonfinite output')
        else:
            continue  # MAPDL titles, column labels and page headers.
        if len(pending[1]) > 6:
            raise ValueError('More than six tensor components')
        if len(pending[1]) == 6:
            rows.append(dict(element_id=element_id,node_id=pending[0],
                             _surface=surface,
                             **dict(zip(COMPONENTS,pending[1]))))
            pending = None
    if pending is not None:
        raise ValueError('Truncated stress continuation')
    return rows


def _blocks(text):
    blocks = {}
    current = None
    for line in text.splitlines():
        match = ELEMENT.match(line)
        if match:
            current = int(match[1])
            if current in blocks:
                raise ValueError('Duplicate element block')
            blocks[current] = []
        elif current is not None:
            blocks[current].append(line)
    return blocks


def _shell_surfaces(rows,expected,format_verified):
    labels = [row['_surface'] for row in rows]
    if any(label is not None for label in labels):
        if any(label is None for label in labels):
            raise ValueError('Mixed labeled and unlabeled shell surfaces')
        groups = {key:[r for r in rows if r['_surface']==key] for key in ('top','bottom','mid')}
    else:
        if not format_verified:
            raise ValueError('Unlabeled shell output lacks verified TOP/BOTTOM/MID format header')
        groups = {key:rows[i*4:(i+1)*4] for i,key in enumerate(('top','bottom','mid'))}
        if len(rows) != 12:
            raise ValueError('Shell surface coverage mismatch')
    if any([r['node_id'] for r in group] != expected for group in groups.values()):
        raise ValueError('Shell surface node order or coverage mismatch')
    return groups


def read_presol(path,model,kind='shell'):
    """Return shell top/bottom/mid lists or solid list, preserving element-node identity."""
    if kind not in ('shell','solid'):
        raise ValueError('PRESOL kind must be shell or solid')
    text = Path(path).read_text(encoding='utf-8',errors='strict')
    blocks = _blocks(text)
    elements = model['elements']
    ids = [e['element_id'] for e in elements]
    if len(set(ids)) != len(ids) or set(ids) != set(blocks):
        raise ValueError('PRESOL element coverage differs from model')
    count,repeats = (4,3) if kind == 'shell' else (8,1)
    result = {'top':[],'bottom':[],'mid':[]} if kind == 'shell' else []
    for element in elements:
        rows = _block(blocks[element['element_id']],element['element_id'])
        expected = list(element['nodes'])
        if len(expected) != count or len(set(expected)) != count:
            raise ValueError('Unsupported or degenerate element connectivity')
        if kind == 'solid':
            if [r['node_id'] for r in rows] != expected or any(r['_surface'] for r in rows):
                raise ValueError('Solid node coverage or unexpected surface labels')
            for row in rows:
                row.pop('_surface')
            result.extend(rows)
        else:
            groups = _shell_surfaces(rows,expected,SHELL_FORMAT in text)
            for surface,group in groups.items():
                for row in group:
                    row.pop('_surface')
                result[surface].extend(group)
    if not elements:
        raise ValueError('Empty model cannot qualify stress output')
    return result
