"""Preparation-only pressure case; synthetic checks establish no native stress."""
import hashlib
import math
from dataclasses import replace

import pytest

from digitalmodel.ansys.padeye import generate_padeye_apdl
from digitalmodel.ansys.padeye_pressure import (
    build_pressure_mesh, generate_pressure_preparation, pressure_geometry,
    prepare_pressure_study,
)


@pytest.mark.parametrize('size,edges,layers', [(10, 16, 26), (5, 32, 52), (2.5, 64, 104)])
def test_fixed_refinement_and_positive_quad_jacobians(size, edges, layers):
    mesh = build_pressure_mesh(pressure_geometry(size))
    assert mesh['upper_edges'] == edges
    assert mesh['radial_layers'] == layers
    assert len(mesh['nodes']) == 2 * edges * (layers + 1)
    assert len(mesh['elements']) == 2 * edges * layers
    nodes = {n['id']: (n['x_mm'], n['y_mm']) for n in mesh['nodes']}
    for element in mesh['elements']:
        xy = [nodes[n] for n in element['nodes']]
        for i in range(4):
            a, b, c = xy[i], xy[(i+1) % 4], xy[(i+2) % 4]
            assert (b[0]-a[0])*(c[1]-b[1])-(b[1]-a[1])*(c[0]-b[0]) > 0
    points = {(round(x, 7), round(y, 7)) for x, y in nodes.values()}
    assert all((round(400-x, 7), round(y, 7)) in points for x, y in nodes.values())
    assert {(0, 0), (400, 0), (0, 300), (400, 300)} <= points


@pytest.mark.parametrize('size', [10, 5, 2.5])
def test_pressure_shape_resultant_and_native_database_export(size):
    mesh = build_pressure_mesh(pressure_geometry(size))
    nodes = {n['id']: (n['x_mm'], n['y_mm']) for n in mesh['nodes']}
    fx = fy = 0
    for row in mesh['pressures']:
        element = mesh['elements'][row['element'] - 1]
        a, b = nodes[element['nodes'][0]], nodes[element['nodes'][3]]
        p1, p2 = row['p1_mpa'], row['p2_mpa']
        assert row['face'] == 4
        assert p1 == pytest.approx(mesh['pressure_slope_mpa_per_mm'] * (a[1]-220), abs=1e-10)
        assert p2 == pytest.approx(mesh['pressure_slope_mpa_per_mm'] * (b[1]-220), abs=1e-10)
        fx += 8 * (p1+p2)/2 * (b[1]-a[1])
        fy -= 8 * (p1+p2)/2 * (b[0]-a[0])
    assert (fx, fy) == pytest.approx((0, 50000), abs=1e-7)
    assert mesh['alpha'] > 1
    text = generate_pressure_preparation(pressure_geometry(size))
    commands = [line.split('!')[0].split(',')[0].strip().upper() for line in text.splitlines()]
    assert 'SOLVE' not in commands and '/SOLU' not in commands
    assert 'SFGRAD,PRES,0,Y,220' in text
    assert 'CDWRITE,DB,pressure_native,cdb' in text
    assert 'SFELIST,ALL,PRES' in text
    assert 'SHPP,ON' in text and 'SHPP,SUMMARY' in text
    assert 'SHPP,OFF' not in text and 'SHPP,MODIFY' not in text


@pytest.mark.parametrize('changes', [
    {'sling_angle_deg': 1}, {'sling_load_kn': 0}, {'sling_load_kn': 60},
    {'thickness_mm': -1}, {'thickness_mm': math.nan}, {'youngs_modulus_mpa': math.inf},
    {'element_size_mm': 3}, {'hole_center_x_mm': 201}, {'poisson': 0.31},
])
def test_reparameterization_fails_before_preparation(changes):
    with pytest.raises(ValueError):
        build_pressure_mesh(replace(pressure_geometry(10), **changes))


def test_case_files_are_separate_and_do_not_overwrite(tmp_path):
    paths = prepare_pressure_study(tmp_path)
    assert len(paths) == 3
    assert all(p.name == 'pressure_prepare.inp' for p in paths)
    assert all(p.with_name('pressure_intent.json').is_file() for p in paths)
    with pytest.raises(FileExistsError):
        prepare_pressure_study(tmp_path)
    assert not list(tmp_path.rglob('*.rst'))


def test_example_build_exposes_separate_pressure_preparation(tmp_path):
    from tests.ansys.test_padeye_preparation import load_build
    paths = load_build().prepare_pressure_study(tmp_path)
    assert len(paths) == 3
    assert all(p.name == 'pressure_prepare.inp' for p in paths)


@pytest.mark.parametrize('size,lf,crlf', [
    (10, '9d0c99dd48fa12117f8d971374563a1f10430f19176dab2c338bb8213416c2ff',
     '6900ea44695fed80031f007b5fe79e1926b626877b7d31582a0d1199b80ce084'),
    (5, 'f72ec7334aca81d16057c08590440e4b3a4a308cc2c60f9612cd07d03e087a92',
     '933750ebbc1c9d9771b55fa37a21eb0d5ad46b5873d1acc123b6f378cf8590be'),
])
def test_old_equal_force_deck_bytes_remain_frozen(size, lf, crlf):
    text = generate_padeye_apdl(pressure_geometry(size))
    assert hashlib.sha256(text.encode('utf-8')).hexdigest() == lf
    assert hashlib.sha256(text.replace('\n', '\r\n').encode('utf-8')).hexdigest() == crlf
