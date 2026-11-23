"""Executable pack gates assert package outputs, not documentation alone."""
import json
from pathlib import Path

import pytest
import yaml

from digitalmodel.pipelay.workflow import router, run_jlay, run_reel_lay

ROOT = Path(__file__).resolve().parents[2]
PACK = ROOT / 'src/digitalmodel/pipelay/docs/agent-pack.md'


def settings(method):
    source = ROOT / 'examples/workflows' / f'{method}-pipelay/input.yml'
    return yaml.safe_load(source.read_text(encoding='utf-8'))


def test_c01_router_rejects_unsupported_method():
    with pytest.raises(ValueError, match='Unknown pipelay method'):
        router({'pipelay': {'method': 'Tow-Lay'}})


def test_c02_router_rejects_unknown_key(tmp_path):
    cfg = settings('jlay')
    cfg['pipelay']['pipe']['outer_diameter_m'] = 0.508
    cfg['pipelay']['outputs']['directory'] = str(tmp_path)
    with pytest.raises(ValueError, match='outer_diameter_m'):
        router(cfg)
    assert not list(tmp_path.iterdir())


def test_c03_jlay_hand_check():
    result = run_jlay(settings('jlay')['pipelay'])
    assert result['sagbend']['catenary_parameter_m'] == pytest.approx(82.8394, rel=1e-3)
    assert result['sagbend']['sagbend_bending_strain'] == pytest.approx(0.00195438, rel=1e-3)
    assert result['top_tension_kN'] == pytest.approx(2624.872, rel=1e-3)


def test_c04_reel_residual_hand_check():
    cfg = settings('reel-lay')['pipelay']
    result = run_reel_lay(cfg)
    assert result['residual_curvature_1_per_m'] == pytest.approx(0.00625, rel=1e-3)
    assert result['residual_radius_m'] == pytest.approx(160.0, rel=1e-3)
    assert result['residual_bending_strain'] == pytest.approx(0.000853, rel=1e-3)
    assert result['residual_out_of_straightness_m'] == pytest.approx(0.1125, rel=1e-3)
    cfg['config']['reel_core_radius_m'] = 3.0
    assert run_reel_lay(cfg)['screening_status'] == 'fail'


def test_c05_feasible_tension_trend():
    cfg = settings('jlay')['pipelay']
    low = run_jlay(cfg)
    cfg['config'].update(top_tension_kN=3000.0, tower_angle_deg=9.8362)
    high = run_jlay(cfg)
    assert high['sagbend']['sagbend_bending_strain'] < low['sagbend']['sagbend_bending_strain']
    assert abs(high['sagbend']['tension_closure_relative_error']) <= 0.001


@pytest.mark.parametrize('method, expected', [('jlay', 'J-Lay'), ('reel-lay', 'Reel-Lay')])
def test_c06_summary_matches_router(method, expected, tmp_path):
    cfg = settings(method)
    cfg['pipelay']['outputs']['directory'] = str(tmp_path)
    result = router(cfg)['pipelay']
    summary = json.loads(Path(result['summary_json']).read_text(encoding='utf-8'))
    assert summary['method'] == result['method'] == expected
    assert summary['screening_status'] == result['screening_status'] == 'pass'
    assert 'native_model' not in summary
    assert 'time_history' not in summary


def test_pack_tracking_and_attributed_defaults():
    text = PACK.read_text(encoding='utf-8')
    assert 'issue pending' not in text
    for key in ('config.allowable_sagbend_strain', 'config.allowable_overbend_strain', 'pipe.smys'):
        row = next(line for line in text.splitlines() if line.startswith('|') and f'`{key}`' in line)
        assert 'code default at the cited line; no standard allowable asserted' in row
        assert not any(char.isdigit() for char in row.split('|')[4])
