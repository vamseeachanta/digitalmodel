"""Synthetic keyed state/support protocol; metadata is not native authentication."""
from decimal import Decimal

import pytest

from digitalmodel.ansys.cylinder_results import EvidenceError
from digitalmodel.ansys.cylinder_results_state import (
    parse_state_values, parse_support_reactions, validate_precision_witness,
)
from tests.ansys.test_cylinder_results import e24


def record(component,value,node=0,x='0',y='0'):
    return (f"{'P10N16':8}{node:8.0f}{e24(x)}{e24(y)}{component:8}{e24(value)}\n").encode()


def test_exact_state_counts_and_result_set():
    raw=b''.join(record(k,v) for k,v in dict(NSET=1,LSTP=1,SBST=1,PRE_N=4833,
        PRE_E=1536,LIST_N=9,LIST_E=1536,POST_N=4833,POST_E=1536).items())
    assert parse_state_values(raw,'P10N16',4833,1536)['NSET']==1
    with pytest.raises(EvidenceError):
        parse_state_values(raw.replace(e24(1).encode(),e24(0).encode()),'P10N16',4833,1536)


def test_support_set_and_coordinates_are_complete():
    nodes=[dict(node_id=10,x_mm='750',y_mm='0'),dict(node_id=11,x_mm='780',y_mm='0')]
    raw=record('RFY','-.1',10,'750')+record('RFY','.1',11,'780')
    assert parse_support_reactions(raw,'P10N16',nodes)['sum_rfy_n']==Decimal(0)
    with pytest.raises(EvidenceError):parse_support_reactions(raw,'P10N16',nodes[:1])


def test_witness_exact_identity_not_just_present():
    validate_precision_witness(record('WITNESS','1.234567890123456'),'P10N16')
    with pytest.raises(EvidenceError):
        validate_precision_witness(record('WITNESS','1.234567890123455'),'P10N16')
