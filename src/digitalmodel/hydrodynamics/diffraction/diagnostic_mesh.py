"""Pure in-memory geometry operation; callers must establish source rights."""
import hashlib
from decimal import Decimal, InvalidOperation

ROLES = ('bottom', 'y_min', 'x_max', 'y_max', 'x_min')
NORMALS = ((0, 0, -1), (0, 1, 0), (-1, 0, 0), (0, -1, 0), (1, 0, 0))


def _numbers(line, count):
    try:
        values = tuple(Decimal(word) for word in line.split())
    except InvalidOperation as error:
        raise ValueError('Invalid mesh scalar') from error
    if len(values) != count or not all(value.is_finite() for value in values):
        raise ValueError('Invalid mesh record')
    return values


def _normal(points):
    u = tuple(points[1][i] - points[0][i] for i in range(3))
    v = tuple(points[2][i] - points[0][i] for i in range(3))
    return (u[1]*v[2]-u[2]*v[1], u[2]*v[0]-u[0]*v[2], u[0]*v[1]-u[1]*v[0])


def _role(points):
    if len(set(points)) != 4:
        raise ValueError('Repeated mesh vertex')
    candidates = []
    for role, axis, value in [('bottom', 2, -1), ('y_min', 1, 0), ('x_max', 0, 1),
                              ('y_max', 1, 1), ('x_min', 0, 0)]:
        bounds = [(0, 1), (0, 1), (-1, 0)]
        bounds[axis] = (value,)
        expected = {(Decimal(x), Decimal(y), Decimal(z))
                    for x in bounds[0] for y in bounds[1] for z in bounds[2]}
        if set(points) == expected:
            candidates.append(role)
    if len(candidates) != 1:
        raise ValueError('Unclassifiable panel')
    return candidates[0]


def transform_mesh(raw):
    """Validate the prescribed five-face box and return bytes plus full ledger."""
    if not isinstance(raw, bytes) or len(raw) > 100000:
        raise ValueError('Mesh must be bounded bytes')
    if b'\r' in raw or not raw.endswith(b'\n'):
        raise ValueError('Only LF-terminated mesh records with final LF supported')
    try:
        rows = raw.decode('utf-8').splitlines()
    except UnicodeError as error:
        raise ValueError('Mesh must be UTF-8') from error
    if len(rows) != 24 or not rows[0].strip():
        raise ValueError('Exactly five panels and four header records required')
    ulen, grav = _numbers(rows[1], 2)
    if ulen != 1 or grav <= 0 or _numbers(rows[2], 2) != (0, 0) or _numbers(rows[3], 1) != (5,):
        raise ValueError('Unsupported GDF header')
    output, ledger = list(rows[:4]), []
    for index in range(5):
        original = rows[4 + 4*index:8 + 4*index]
        points = [_numbers(row, 3) for row in original]
        role = _role(points)
        normals = [_normal(points[corner:] + points[:corner]) for corner in range(4)]
        if role != ROLES[index] or any(normal != NORMALS[index] for normal in normals):
            raise ValueError('Panel role/index or winding mismatch')
        operation = 'retain' if role == 'bottom' else 'reverse'
        derived = original if operation == 'retain' else list(reversed(original))
        output.extend(derived)
        ledger.append({'index': index + 1, 'role': role, 'operation': operation,
                       'original_vertices': original, 'derived_vertices': derived})
    derived_raw = ('\n'.join(output) + '\n').encode('utf-8')
    return {'source_sha256': hashlib.sha256(raw).hexdigest(),
            'derived_sha256': hashlib.sha256(derived_raw).hexdigest(), 'derived_gdf': derived_raw,
            'panels': ledger, 'header': {'title': rows[0], 'ulen': str(ulen),
            'grav': {'value': str(grav), 'effect': 'unknown_pending_native_readback'},
            'symmetry': rows[2], 'panel_count': rows[3]}}
