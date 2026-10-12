"""Synthetic keyed fields reproducing native Fortran E24.16 notation."""
import pytest
from decimal import Decimal
from digitalmodel.ansys.cylinder_results import EvidenceError, parse_e24
from digitalmodel.ansys.cylinder_results_state import validate_precision_witness


def witness(value='0.1234567890123456E+01', token='CTRL16'):
    return (f'{token:<8}{0:8.0f}{0:24.16E}{0:24.16E}{"WITNESS":<8}{value:>24}\r\n').encode()


def test_native_fortran_witness_exact_literal_and_half_quantum():
    raw=witness()
    value,half_quantum=parse_e24(raw[72:96], 'dimensionless')
    assert value==Decimal('1.234567890123456')
    assert half_quantum==Decimal('5e-16')
    validate_precision_witness(raw,'CTRL16')


def test_python_normalized_fixture_remains_supported():
    validate_precision_witness(witness('1.2345678901234560E+00'),'CTRL16')


@pytest.mark.parametrize('value',['0.123456789012345E+01','0.1234567890123450E+01',
    '0.1234567890123457E+01','0.1234567890123456E+00','NaN','Infinity','*'*24])
def test_truncated_mutated_nonfinite_or_overflow_witness_refuses(value):
    with pytest.raises(EvidenceError): validate_precision_witness(witness(value),'CTRL16')


@pytest.mark.parametrize('damage',['case','duplicate','node','width','component'])
def test_witness_identity_count_and_field_width_remain_strict(damage):
    raw=witness()
    if damage=='case': raw=witness(token='P10N4')
    elif damage=='duplicate': raw+=raw
    elif damage=='node': raw=raw[:8]+f'{1:8.0f}'.encode()+raw[16:]
    elif damage=='width': raw=raw[:72]+raw[73:]
    else: raw=raw.replace(b'WITNESS ',b'OTHER   ')
    with pytest.raises(EvidenceError): validate_precision_witness(raw,'CTRL16')
