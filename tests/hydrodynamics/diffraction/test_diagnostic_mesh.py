"""Fresh synthetic coordinates, not copied vendor or project evidence."""
import pytest

from digitalmodel.hydrodynamics.diffraction.diagnostic_mesh import transform_mesh


def synthetic_mesh():
    panels = [
        [(0, 1, -1), (1, 1, -1), (1, 0, -1), (0, 0, -1)],
        [(0, 0, 0), (1, 0, 0), (1, 0, -1), (0, 0, -1)],
        [(1, 0, 0), (1, 1, 0), (1, 1, -1), (1, 0, -1)],
        [(1, 1, 0), (0, 1, 0), (0, 1, -1), (1, 1, -1)],
        [(0, 1, 0), (0, 0, 0), (0, 0, -1), (0, 1, -1)],
    ]
    rows = ['SYNTHETIC TEST ONLY', '1 9.81', '0 0', '5']
    rows += [' '.join(map(str, point)) for panel in panels for point in panel]
    return ('\n'.join(rows) + '\n').encode()


def test_verified_roles_drive_side_reversal():
    result = transform_mesh(synthetic_mesh())
    assert [row['role'] for row in result['panels']] == ['bottom', 'y_min', 'x_max', 'y_max', 'x_min']
    assert [row['operation'] for row in result['panels']] == ['retain'] + ['reverse'] * 4
    assert result['header']['grav']['effect'] == 'unknown_pending_native_readback'
    assert result['derived_gdf'].splitlines()[:4] == synthetic_mesh().splitlines()[:4]
    assert result['source_sha256'] != result['derived_sha256']


@pytest.mark.parametrize('fault', ['permuted', 'top', 'symmetry', 'nan', 'trailing', 'normal', 'crossed'])
def test_malformed_or_wrong_role_mesh_refuses(fault):
    rows = synthetic_mesh().decode().splitlines()
    if fault == 'permuted':
        rows[4:8], rows[8:12] = rows[8:12], rows[4:8]
    elif fault == 'top':
        rows[4:8] = [row.replace('-1', '0') for row in rows[4:8]]
    elif fault == 'symmetry':
        rows[2] = '1 0'
    elif fault == 'nan':
        rows[4] = 'NaN 1 -1'
    elif fault == 'normal':
        rows[8:12] = reversed(rows[8:12])
    elif fault == 'crossed':
        rows[6], rows[7] = rows[7], rows[6]
    else:
        rows.append('0 0 0')
    with pytest.raises(ValueError):
        transform_mesh(('\n'.join(rows) + '\n').encode())


@pytest.mark.parametrize('style', ['crlf', 'missing_final', 'mixed'])
def test_source_newline_changes_refuse(style):
    raw = synthetic_mesh()
    if style == 'crlf':
        raw = raw.replace(b'\n', b'\r\n')
    elif style == 'missing_final':
        raw = raw.rstrip(b'\n')
    else:
        raw = raw.replace(b'\n', b'\r\n', 1)
    with pytest.raises(ValueError):
        transform_mesh(raw)
