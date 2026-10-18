"""Issue 2745: independent values and failure boundaries for offline screening."""
import hashlib
import json
from pathlib import Path

import pytest
import yaml

from digitalmodel.orcaflex.pipelay_analysis import (
    JLayConfig, PipelayPipeProperties, calculate_stinger_radius,
)
from digitalmodel.pipelay.workflow import router, run_jlay, run_reel_lay

ROOT = Path(__file__).resolve().parents[2]


def template(method='jlay'):
    path = ROOT / 'examples/workflows' / f'{method}-pipelay/input.yml'
    cfg = yaml.safe_load(path.read_text(encoding='utf-8'))
    # Explicit design basis, also usable against the pre-fix templates.
    cfg['pipelay']['pipe']['steel_density'] = 7850.0
    if method == 'jlay':
        cfg['pipelay']['config']['top_tension_kN'] = 2624.9
    return cfg


def test_jlay_hand_values():
    # screening-basis.md, equations E1-E5: values independently evaluated.
    result = run_jlay(template()['pipelay'])
    assert result['pipe_submerged_weight_N_per_m'] == pytest.approx(1658.33, rel=1e-3)
    assert result['top_tension_kN'] == pytest.approx(2624.9, rel=1e-3)
    for key, expected in {
        'catenary_parameter_m': 82.839414, 'sagbend_bending_strain': 0.00195438,
        'horizontal_offset_m': 301.745330, 'suspended_length_m': 1580.670188,
    }.items():
        assert result['sagbend'][key] == pytest.approx(expected, rel=1e-3)


def test_analytical_class_derives_tension():
    result = JLayConfig(water_depth=1500.0, tower_angle=3.0).calculate_sagbend()
    assert result['derived_top_tension_kN'] == pytest.approx(2624.872, rel=1e-3)
    assert result['catenary_parameter_m'] == pytest.approx(82.8394, rel=1e-3)


def test_sagbend_status_uses_unrounded_strain():
    cfg = template()['pipelay']
    cfg['config']['allowable_sagbend_strain'] = 0.0019541
    assert run_jlay(cfg)['screening_status'] == 'fail'


@pytest.mark.parametrize('tension, message', [(3000, 'closure'), (800, 'suspended weight')])
def test_inconsistent_or_insufficient_tension(tension, message):
    cfg = template()['pipelay']
    cfg['config']['top_tension_kN'] = tension
    with pytest.raises(ValueError, match=message):
        run_jlay(cfg)


def test_buoyant_pipe_raises_before_output(tmp_path):
    cfg = template()
    cfg['pipelay']['pipe']['coating_density'] = 0.0
    cfg['pipelay']['outputs']['directory'] = str(tmp_path)
    with pytest.raises(ValueError, match='submerged weight'):
        router(cfg)
    assert list(tmp_path.iterdir()) == []


def test_missing_design_basis():
    with pytest.raises(ValueError, match='required|Missing'):
        run_jlay({})


@pytest.mark.parametrize('key', ['water_depth_m', 'tower_angle_deg', 'top_tension_kN'])
def test_missing_jlay_scalar(key):
    cfg = template()['pipelay']
    del cfg['config'][key]
    with pytest.raises(ValueError, match=key):
        run_jlay(cfg)


@pytest.mark.parametrize('key', ['outer_diameter', 'wall_thickness', 'steel_density',
                                 'coating_density', 'seawater_density'])
def test_missing_pipe_basis(key):
    cfg = template()['pipelay']
    del cfg['pipe'][key]
    with pytest.raises(ValueError, match=key):
        run_jlay(cfg)


@pytest.mark.parametrize('block, key', [('pipe', 'outer_diameter_m'),
    ('config', 'water_depth'), ('operability', 'max_wave_height'),
    ('outputs', 'summary_file'), ('case', 'case_name')])
def test_c02_unknown_input_key(block, key, tmp_path):
    cfg = template()
    cfg['pipelay']['outputs']['directory'] = str(tmp_path)
    cfg['pipelay'][block][key] = 1
    with pytest.raises(ValueError, match=key):
        router(cfg)
    assert list(tmp_path.iterdir()) == []


def test_unknown_settings_key():
    cfg = template()['pipelay']
    cfg['water_depth'] = 2500
    with pytest.raises(ValueError, match='water_depth'):
        run_jlay(cfg)


@pytest.mark.parametrize('angle', [0, float('nan'), float('inf')])
def test_degenerate_angle(angle):
    cfg = template()['pipelay']
    cfg['config']['tower_angle_deg'] = angle
    with pytest.raises(ValueError, match='tower_angle'):
        run_jlay(cfg)


@pytest.mark.parametrize('limit', [0, -0.002, float('inf'), float('nan'), 0.15, 0.01])
def test_sagbend_limit_range(limit):
    cfg = template()['pipelay']
    cfg['config']['allowable_sagbend_strain'] = limit
    with pytest.raises(ValueError, match='allowable_sagbend_strain'):
        run_jlay(cfg)


@pytest.mark.parametrize('limit', [0, -1, float('inf'), float('nan'), 0.05, 0.15])
def test_reeling_limit_range(limit):
    cfg = template('reel-lay')['pipelay']
    cfg['config']['allowable_reeling_strain'] = limit
    with pytest.raises(ValueError, match='allowable_reeling_strain'):
        run_reel_lay(cfg)


@pytest.mark.parametrize('radius', [0, -1, float('nan'), float('inf')])
def test_reel_radius_range(radius):
    cfg = template('reel-lay')['pipelay']
    cfg['config']['reel_core_radius_m'] = radius
    with pytest.raises(ValueError, match='reel_core_radius_m'):
        run_reel_lay(cfg)


@pytest.mark.parametrize('strain', [0, -0.002, 1, float('nan'), float('inf')])
def test_overbend_rejected_before_output(strain, tmp_path):
    cfg = template()
    cfg['pipelay']['config']['allowable_overbend_strain'] = strain
    cfg['pipelay']['outputs']['directory'] = str(tmp_path)
    with pytest.raises(ValueError, match='allowable_overbend_strain'):
        router(cfg)
    assert list(tmp_path.iterdir()) == []
    with pytest.raises(ValueError, match='strain'):
        calculate_stinger_radius(0.3238, strain)


@pytest.mark.parametrize('thickness', [0.1619, 0.2])
def test_impossible_pipe_geometry(thickness):
    with pytest.raises(ValueError, match='wall_thickness'):
        PipelayPipeProperties(outer_diameter=0.3238, wall_thickness=thickness)


def reject_constant(value):
    raise ValueError(f'Non-finite JSON constant: {value}')


def test_perfect_straightening_strict_json(tmp_path):
    cfg = template('reel-lay')
    cfg['pipelay']['config']['straightener_efficiency'] = 1.0
    cfg['pipelay']['outputs']['directory'] = str(tmp_path)
    result = router(cfg)['pipelay']
    payload = json.loads(Path(result['summary_json']).read_text(), parse_constant=reject_constant)
    assert payload['residual_radius_m'] is None
    assert payload['residual_radius_status'] == 'unbounded_zero_curvature'
    assert payload['residual_bending_strain'] == 0.0


@pytest.mark.parametrize('method', ['jlay', 'reel-lay'])
def test_c07_summary_digest_readback(method, tmp_path):
    source = ROOT / 'examples/workflows' / f'{method}-pipelay/input.yml'
    source_bytes = source.read_bytes()
    source_digest = hashlib.sha256(source_bytes).hexdigest()
    cfg = yaml.safe_load(source_bytes)
    cfg['pipelay']['outputs']['directory'] = str(tmp_path)
    result = router(cfg)['pipelay']
    output = Path(result['summary_json'])
    output_bytes = output.read_bytes()
    output_digest = hashlib.sha256(output_bytes).hexdigest()
    payload = json.loads(output_bytes, parse_constant=reject_constant)
    assert payload['method'] == result['method']
    assert payload['screening_status'] == result['screening_status'] == 'pass'
    assert hashlib.sha256(source.read_bytes()).hexdigest() == source_digest
    assert hashlib.sha256(output.read_bytes()).hexdigest() == output_digest
    expected = {k: result[k] for k in payload}
    assert payload == expected


def test_applied_defaults_are_reported():
    cfg = template()['pipelay']
    del cfg['pipe']['youngs_modulus']
    result = run_jlay(cfg)
    assert result['applied_defaults']['pipe.youngs_modulus'] == 207e9
    assert 'pipe.steel_density' not in result['applied_defaults']


def test_reel_hand_values():
    result = run_reel_lay(template('reel-lay')['pipelay'])
    for key, expected in {
        'reeling_bending_strain': 0.017069,
        'residual_curvature_1_per_m': 0.00625, 'residual_radius_m': 160,
        'residual_bending_strain': 0.000853, 'residual_out_of_straightness_m': 0.1125,
    }.items():
        assert result[key] == pytest.approx(expected, rel=1e-3)


def test_jlay_welding_wave_limit():
    result = run_jlay(template()['pipelay'])
    rates = {row['hs_m']: row['lay_rate_km_per_day']
             for row in result['operability']['lay_rate_table']}
    assert rates[3.0] == 0.0
    assert rates[2.5] == 1.75
