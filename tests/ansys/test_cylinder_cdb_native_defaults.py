"""Synthetic inert-command mutations informed by retained native CDB grammar."""
import pytest
from digitalmodel.ansys.cylinder_results import EvidenceError
from digitalmodel.ansys.cylinder_results_cdb import parse_model_cdb
from tests.ansys.test_cylinder_results_cdb import model_fixture


@pytest.mark.parametrize('command', ['OMEGA,0,0,0', 'OMEGA,0,0,0,0',
                                    'OMEGA,0.0000000000000E+000,-0,0'])
def test_explicit_three_or_legacy_four_zero_omega_fields(command):
    raw, _ = model_fixture()
    result = parse_model_cdb(raw.replace(b'FINISH', command.encode()+b'\nFINISH'))
    assert len(result['nodes']) == 8


@pytest.mark.parametrize('command', ['OMEGA,1,0,0', 'OMEGA,0,-1,0', 'OMEGA,0,0,1',
    'OMEGA,0,0,0,1', 'OMEGA,0,0', 'OMEGA,0,0,0,0,0', 'OMEGA,0,,0',
    'OMEGA,0,NaN,0', 'OMEGA,0,Infinity,0', 'OMEGA,0,unknown,0', 'UNKNOWN,0,0,0'])
def test_nonzero_malformed_or_unknown_defaults_refuse(command):
    raw, _ = model_fixture()
    with pytest.raises(EvidenceError):
        parse_model_cdb(raw.replace(b'FINISH', command.encode()+b'\nFINISH'))


@pytest.mark.parametrize('axis', [0, 1])
def test_missing_explicit_planar_coordinate_refuses(axis):
    raw, _ = model_fixture()
    rows = raw.splitlines(keepends=True)
    index = rows.index(b'(3i9,6e21.13e3)\n') + 1
    start = 27 + axis * 21
    rows[index] = rows[index][:start] + b' ' * 21 + rows[index][start+21:]
    with pytest.raises(EvidenceError, match='explicit X and Y'):
        parse_model_cdb(b''.join(rows))


def test_blank_out_of_plane_coordinates_remain_supported():
    raw, _ = model_fixture()
    rows = raw.splitlines(keepends=True)
    index = rows.index(b'(3i9,6e21.13e3)\n') + 1
    rows[index] = rows[index][:69] + b'\n'
    assert parse_model_cdb(b''.join(rows))['nodes'][1] == (750, 0)



def test_documented_trailing_zero_y_omission_requires_frozen_mesh_agreement():
    from digitalmodel.ansys.cylinder_results_cdb import verify_model
    raw, expected = model_fixture()
    rows = raw.splitlines(keepends=True)
    start = rows.index(b'(3i9,6e21.13e3)\n') + 1
    rows[start] = rows[start][:48] + b'\n'
    native = parse_model_cdb(b''.join(rows))
    verify_model(native, expected)
    rows[start+2] = rows[start+2][:48] + b'\n'
    with pytest.raises(EvidenceError, match='coordinate differs'):
        verify_model(parse_model_cdb(b''.join(rows)), expected)
