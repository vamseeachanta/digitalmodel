"""Synthetic protocol tests; no native or engineering qualification evidence."""
import pytest

from digitalmodel.hydrodynamics.diffraction.diagnostic_basis import (
    canonical_decimal, canonical_intent, prescribed_basis, roundtrip_equal,
)


@pytest.mark.parametrize('value', ['-0', '.5', '00.5', '0.50', 'NaN', 'Infinity', '+1', 0.5, True])
def test_noncanonical_decimals_refuse(value):
    with pytest.raises(ValueError):
        canonical_decimal(value)


def test_basis_semantics_and_conditional_reference():
    basis = prescribed_basis()
    assert canonical_decimal('0.5') == '0.5'
    assert basis['angular_frequency'] == {'value': '0.3', 'unit': 'rad/s'}
    assert basis['interior_surface_panels'] == 'none'
    assert basis['reference']['usable'] is False
    assert basis['reference']['effective_native_gravity'] == 'unknown'
    assert basis['reference']['c33'] == {'numerator': '8041453', 'denominator': '800000', 'unit': 'kN/m'}
    assert basis['comparison_to_source'] == 'not_established'


def test_serialization_is_stable_and_duplicate_input_refuses():
    assert canonical_intent({'b': '1', 'a': '0.5'}) == b'{"a":"0.5","b":"1"}\n'
    for raw in ['{"a":"1","a":"2"}', '{"a":NaN}']:
        with pytest.raises(ValueError):
            canonical_intent(raw)
    with pytest.raises(ValueError):
        canonical_intent({'a': 0.5})


def test_roundtrip_exact_decimal_without_tolerance():
    assert roundtrip_equal('1.025000', '1.025')
    assert roundtrip_equal('5e-1', '0.5')
    assert not roundtrip_equal('1.02500000001', '1.025')
    with pytest.raises(ValueError):
        roundtrip_equal('NaN', '1')


@pytest.mark.parametrize('value', ['0.50', '-0', 'NaN'])
def test_numeric_intent_strings_must_be_canonical(value):
    with pytest.raises(ValueError):
        canonical_intent({'mass': {'value': value}})
    with pytest.raises(ValueError):
        canonical_intent({'centre_of_gravity': {'value': ['0', value, '1']}})
