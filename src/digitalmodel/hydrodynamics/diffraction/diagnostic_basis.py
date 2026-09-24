"""Prescribed offline diagnostic intent, never native qualification."""
import json
import re
from decimal import Decimal, InvalidOperation

DECIMAL_PATTERN = re.compile(r'^-?(0|[1-9][0-9]*)(\.[0-9]*[1-9])?$')


def canonical_decimal(value):
    """Require one finite non-exponent spelling, including a unique zero."""
    if not isinstance(value, str) or not DECIMAL_PATTERN.fullmatch(value) or value == '-0':
        raise ValueError('Noncanonical decimal')
    return value


def unique_pairs(pairs):
    result = {}
    for key, value in pairs:
        if key in result:
            raise ValueError('Duplicate input key')
        result[key] = value
    return result


def reject_constant(value):
    raise ValueError('Non-finite input')


def strict_json(raw):
    try:
        return json.loads(raw, object_pairs_hook=unique_pairs, parse_constant=reject_constant)
    except (TypeError, UnicodeError, json.JSONDecodeError) as error:
        raise ValueError('Invalid JSON input') from error


def _intent_types(value, numeric=False):
    if numeric and not isinstance(value, list):
        canonical_decimal(value)
        return
    if isinstance(value, dict):
        if not all(isinstance(key, str) for key in value):
            raise ValueError('Intent keys must be strings')
        for key, child in value.items():
            _intent_types(child, numeric=key in {'value', 'numerator', 'denominator'})
    elif isinstance(value, list):
        for child in value:
            _intent_types(child, numeric=numeric)
    elif not isinstance(value, (str, bool, type(None))):
        raise ValueError('Intent numeric leaves must be canonical strings')


def canonical_intent(value):
    if isinstance(value, (str, bytes)):
        value = strict_json(value)
    _intent_types(value)
    return (json.dumps(value, ensure_ascii=False, sort_keys=True,
                       separators=(',', ':'), allow_nan=False) + '\n').encode('utf-8')


def roundtrip_equal(observed, expected):
    canonical_decimal(expected)
    if not isinstance(observed, str) or not re.fullmatch(
            r'[+-]?(?:[0-9]+(?:\.[0-9]*)?|\.[0-9]+)(?:[eE][+-]?[0-9]+)?', observed):
        raise ValueError('Invalid observed decimal')
    try:
        actual = Decimal(observed)
    except InvalidOperation as error:
        raise ValueError('Invalid observed decimal') from error
    return actual.is_finite() and actual == Decimal(expected)


def prescribed_basis():
    """Return a new assumption, not a reconstruction of the supplied source."""
    return {
        'mass': {'value': '1.025', 'unit': 't'},
        'density': {'value': '1.025', 'unit': 't/m3'},
        'centre_of_gravity': {'value': ['0.5', '0.5', '-0.5'], 'unit': 'm'},
        'radii': {'value': ['0.4', '0.4', '0.4'], 'unit': 'm'},
        'inertia_diagonal': {'value': ['0.164'] * 3, 'unit': 't m2'},
        'inertia_origin': 'centre_of_gravity', 'body_origin': 'centre_of_gravity',
        'reference_point': {'value': ['0', '0', '0'], 'unit': 'm'},
        'water_depth': {'value': '100', 'unit': 'm'},
        'gravity': {'value': '9.80665', 'unit': 'm/s2', 'status': 'conditional_assumption'},
        'angular_frequency': {'value': '0.3', 'unit': 'rad/s'},
        'symmetry': 'none', 'interior_surface_panels': 'none',
        'comparison_to_source': 'not_established',
        'reference': {'usable': False, 'effective_native_gravity': 'unknown',
                      'requires': 'independent_effective_native_gravity_and_mapping_readback',
                      'c33': {'numerator': '8041453', 'denominator': '800000', 'unit': 'kN/m'},
                      'c44_c55': {'numerator': '8041453', 'denominator': '9600000', 'unit': 'kN m/rad'}},
    }
