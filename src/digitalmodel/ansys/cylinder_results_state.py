"""Keyed native *GET state and support-export checks for the fixed canary."""
from decimal import Decimal

from digitalmodel.ansys.cylinder_results import (
    EvidenceError, decimal_value, keyed_records, parse_e24, decimal_context,
)


def parse_state_values(raw, case_token, expected_nodes, expected_elements):
    expected = dict(NSET=1, LSTP=1, SBST=1, PRE_N=expected_nodes,
                    PRE_E=expected_elements, LIST_N=9, LIST_E=expected_elements,
                    POST_N=expected_nodes, POST_E=expected_elements)
    values = {}
    for node, x, y, component, field in keyed_records(raw, case_token):
        value, _ = parse_e24(field, 'dimensionless')
        if node or x or y or component not in expected or component in values:
            raise EvidenceError('Unexpected or duplicate native state key')
        if value != expected[component]:
            raise EvidenceError('Native state count/result set differs')
        values[component] = int(value)
    if values.keys() != expected.keys():
        raise EvidenceError('Incomplete native state export')
    return values


def validate_precision_witness(raw, case_token):
    records = keyed_records(raw, case_token)
    if len(records) != 1:
        raise EvidenceError('Exactly one precision witness required')
    node, x, y, component, field = records[0]
    value, quantum = parse_e24(field, 'dimensionless')
    # E24.16 may use 0.ddddE+01 for this exact 16-significant-digit literal.
    # parse_e24 returns half the last-place quantum: 0.5 * 10**(1-16).
    if (node or x or y or component != 'WITNESS'
            or value != Decimal('1.234567890123456') or quantum > Decimal('5e-16')):
        raise EvidenceError('Precision witness does not match approved digits')


@decimal_context
def parse_support_reactions(raw, case_token, bottom_nodes):
    if not bottom_nodes:
        raise EvidenceError('Missing expected support set')
    expected = {s['node_id']: s for s in bottom_nodes}
    if len(expected) != len(bottom_nodes):
        raise EvidenceError('Duplicate expected support nodes')
    values = {}
    for node, x, y, component, field in keyed_records(raw, case_token):
        if node not in expected or node in values or component != 'RFY':
            raise EvidenceError('Unexpected or duplicate support reaction')
        target = expected[node]
        if (y != 0 or decimal_value(target['y_mm']) != 0
                or abs(x-decimal_value(target['x_mm'])) > Decimal('1e-9')):
            raise EvidenceError('Support reaction coordinate mismatch')
        value, quantum = parse_e24(field, 'N')
        if quantum > Decimal('1e-8'):
            raise EvidenceError('Reaction export precision insufficient')
        values[node] = value
    if set(values) != set(expected):
        raise EvidenceError('Incomplete support reactions')
    return {'by_node': values, 'sum_rfy_n': sum(values.values(), Decimal(0))}
