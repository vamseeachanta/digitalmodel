"""Strict canary keyed exports and Decimal recovery checks; no solver calls."""
from decimal import (Context, Decimal, InvalidOperation, DivisionByZero, Overflow,
                     ROUND_HALF_EVEN, localcontext)
from functools import wraps
import re

D = Decimal
COMPONENTS = {'SX': 'sigma_r', 'SZ': 'sigma_theta', 'SY': 'sigma_z',
              'SXY': 'tau_rz', 'UX': 'u_r', 'UY': 'u_z'}
QUANTITIES = tuple(COMPONENTS.values()) + ('sigma_vm',)
CASE_TOKENS = {'CTRL16', 'P10N4', 'P10N8', 'P10N16'}
E24 = re.compile(rb' *[+-]?\d\.\d{16}E[+-]\d{2,3}')


class EvidenceError(ValueError):
    """Incomplete or incompatible evidence cannot establish a numeric outcome."""


def fixed_decimal_context():
    return Context(prec=50, rounding=ROUND_HALF_EVEN, Emin=-999999, Emax=999999,
                   capitals=1, clamp=0, flags=[],
                   traps=[InvalidOperation, DivisionByZero, Overflow])


def decimal_context(function):
    """Isolate calculations from caller precision, rounding mode and traps."""
    @wraps(function)
    def isolated(*args, **kwargs):
        with localcontext(fixed_decimal_context()):
            return function(*args, **kwargs)
    return isolated


def decimal_value(value):
    """Accept exact decimal strings/Decimals only, never silently coerce floats."""
    if not isinstance(value, (str, Decimal)):
        raise EvidenceError('Expected an exact decimal string or Decimal')
    try:
        number = D(value)
    except InvalidOperation as exc:
        raise EvidenceError('Invalid decimal') from exc
    if not number.is_finite():
        raise EvidenceError('Nonfinite decimal')
    return number


def dimensional_floor(quantity):
    return D('1e-9') if quantity in ('u_r', 'u_z') else D('1e-8')


@decimal_context
def parse_e24(field, unit):
    """Return (value, half-quantum) for one fixed-width E24.16 field."""
    if unit not in ('MPa', 'mm', 'N', 'dimensionless'):
        raise EvidenceError('Unsupported unit')
    if not isinstance(field, bytes) or len(field) != 24 or not E24.fullmatch(field):
        raise EvidenceError('Missing E24.16 precision, overflow or invalid field')
    value = decimal_value(field.decode('ascii').strip())
    exponent = int(field.split(b'E')[1])
    if abs(exponent) > 100:
        raise EvidenceError('Exponent outside bounded canary representation')
    return value, D(5).scaleb(exponent - 17)


def keyed_records(raw, case_token):
    """Decode dedicated 96-column records; all byte and key ownership is explicit."""
    if not isinstance(raw, bytes) or case_token not in CASE_TOKENS:
        raise EvidenceError('Missing raw bytes or unknown case token')
    if not raw or not raw.endswith(b'\n'):
        raise EvidenceError('Missing or unterminated dedicated export')
    records = []
    for line in raw.split(b'\n')[:-1]:
        line = line.removesuffix(b'\r')
        if len(line) != 96:
            raise EvidenceError('Expected exactly 96 field columns')
        try:
            token, node, component = (line[:8].decode('ascii').strip(),
                                     line[8:16].decode('ascii').strip(),
                                     line[64:72].decode('ascii').strip())
        except UnicodeDecodeError as exc:
            raise EvidenceError('Non-ASCII export') from exc
        if token != case_token or not re.fullmatch(r'\d+\.?', node):
            raise EvidenceError('Wrong case token or noninteger node field')
        x, qx = parse_e24(line[16:40], 'mm')
        y, qy = parse_e24(line[40:64], 'mm')
        if max(qx, qy) > D('1e-11'):
            raise EvidenceError('Coordinate precision is insufficient')
        records.append((int(node.rstrip('.')), x, y, component, line[72:96]))
    return records


def validated_stations(stations):
    if not isinstance(stations, list) or len(stations) != 9:
        raise EvidenceError('Exactly nine station metadata records required')
    by_node, keys = {}, set()
    for station in stations:
        try:
            node, radial = station['node_id'], station['radial_id']
            x, y = decimal_value(station['x_mm']), decimal_value(station['y_mm'])
            expected_x = {'inner': D(750), 'middle': D(780), 'outer': D(810)}[radial]
            edges = station['adjacent_element_ids']
            valid = (type(node) is int and node > 0 and node not in by_node
                     and station['id'] == f'{radial}_y{y:f}' and y in (60,120,180)
                     and x == expected_x and station['node_type'] == 'corner'
                     and len(edges) == (4 if radial == 'middle' else 2)
                     and len(set(edges)) == len(edges)
                     and all(type(e) is int and e > 0 for e in edges))
        except (KeyError, TypeError) as exc:
            raise EvidenceError('Invalid station metadata') from exc
        if not valid or (radial, y) in keys:
            raise EvidenceError('Wrong station identity, coordinate or adjacency')
        by_node[node] = station
        keys.add((radial, y))
    return by_node


def von_mises(radial, hoop, axial, shear):
    with localcontext(fixed_decimal_context()):
        square = ((radial-hoop)**2+(hoop-axial)**2+(axial-radial)**2)/2+3*shear**2
        return square.sqrt()


@decimal_context
def parse_station_values(raw, case_token, stations):
    by_node = validated_stations(stations)
    values = {}
    for node, x, y, component, field in keyed_records(raw, case_token):
        if node not in by_node or component not in COMPONENTS:
            raise EvidenceError('Unexpected station node or component')
        station = by_node[node]
        if any(abs(got-decimal_value(station[key])) > D('1e-9')
               for got, key in ((x, 'x_mm'), (y, 'y_mm'))):
            raise EvidenceError('Native coordinates differ from station')
        quantity = COMPONENTS[component]
        key = (station['id'], quantity)
        value, quantum = parse_e24(field, 'mm' if quantity.startswith('u_') else 'MPa')
        if key in values or quantum > dimensional_floor(quantity)/100:
            raise EvidenceError('Duplicate key or inadequate export quantum')
        values[key] = value
    expected = {(s['id'], q) for s in stations for q in COMPONENTS.values()}
    if values.keys() != expected:
        raise EvidenceError('Incomplete station export')
    for s in stations:
        values[s['id'], 'sigma_vm'] = von_mises(*(values[s['id'], q] for q in
            ('sigma_r', 'sigma_theta', 'sigma_z', 'tau_rz')))
    return values


@decimal_context
def compare_readback(value, fields, quantity):
    if not fields:
        raise EvidenceError('Missing independent readback')
    unit = 'mm' if quantity.startswith('u_') else 'MPa'
    pairs = [parse_e24(field, unit) for field in fields]
    with localcontext(fixed_decimal_context()):
        limit = max(D('1e-12')*max(abs(value), *(abs(v) for v, _ in pairs)),
                    dimensional_floor(quantity)/100)
        if any(q > limit/100 for _, q in pairs):
            raise EvidenceError('Independent representation quantum too large')
        if abs(value-sum(v for v, _ in pairs)/len(pairs)) > limit:
            raise EvidenceError('Independent readback differs')


def validate_recovery(values, stations, listing, contributions):
    """Validate parsed native listings; this does not authenticate their source."""
    validated_stations(stations)
    expected = {(s['id'], q) for s in stations for q in COMPONENTS.values()}
    if set(listing) != expected or set(contributions) != {s['id'] for s in stations}:
        raise EvidenceError('Missing, duplicate or unexpected listing coverage')
    for station in stations:
        sid = station['id']
        if set(contributions[sid]) != set(station['adjacent_element_ids']):
            raise EvidenceError('Contribution adjacency mismatch')
        for q in COMPONENTS.values():
            compare_readback(values[sid, q], [listing[sid, q]], q)
            if q.startswith('u_'):
                continue
            try:
                fields = [contributions[sid][e][q] for e in station['adjacent_element_ids']]
            except KeyError as exc:
                raise EvidenceError('Missing component contribution') from exc
            compare_readback(values[sid, q], fields, q)
