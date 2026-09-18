"""Independent tensor and solver-output validation checks."""
import math
import pytest
from digitalmodel.asset_integrity.assessment import example_vessel_fea_results as results


@pytest.fixture(autouse=True)
def citation_fixture(tmp_path, monkeypatch):
    p = tmp_path / results.WIKI_PATH
    p.parent.mkdir(parents=True)
    p.write_text('---\ncode_id: api-579-1\npublisher: API\nrevision: "2007"\n---\n')
    monkeypatch.setenv('LLM_WIKI_PATH', str(tmp_path))


def test_tensor_invariants_and_conditional_pressure_limit():
    row = dict(sx=100., sy=50., sz=0., sxy=0., syz=0., sxz=0.)
    assert results.invariants(row)['von_mises_mpa'] == pytest.approx(math.sqrt(7500))
    assert results.invariants(row)['trace_mpa'] == 150
    hydro = dict(sx=200., sy=200., sz=200., sxy=0., syz=0., sxz=0.)
    assert results.invariants(hydro)['von_mises_mpa'] == 0
    r = results.screen_tensors([row], [hydro], [row], pressure=1.5, stress=120)
    assert not r['acceptance']  # Local-failure trace 600 > 4S=480 despite zero VM.
    assert r['pressure_limit_mpa'] == pytest.approx(1.2)


def test_output_reader_rejects_missing_duplicate_nonfinite(tmp_path):
    p = tmp_path/'stress.csv'
    p.write_text('element_id,sx,sy,sz,sxy,syz,sxz\n1,1,2,3,0,0,0\n')
    with pytest.raises(ValueError, match='coverage'):
        results.read_stresses(p, [1,2])
    p.write_text('element_id,sx,sy,sz,sxy,syz,sxz\n1,nan,2,3,0,0,0\n')
    with pytest.raises(ValueError, match='finite'):
        results.read_stresses(p, [1])
    p.write_text('element_id,sx,sy,sz,sxy,syz,sxz\n1,1,2,3,0,0,0\n1,1,2,3,0,0,0\n')
    with pytest.raises(ValueError, match='Duplicate'):
        results.read_stresses(p, [1])


def test_pressure_scale_is_derived_from_each_independent_criterion():
    row = dict(sx=240., sy=0., sz=0., sxy=0., syz=0., sxz=0.)
    r = results.screen_tensors([row], [row], [row], pressure=1.5, stress=120)
    assert r['pressure_limit_mpa'] == pytest.approx(.75)
    assert r['governing_criterion'] == 'membrane'
    with pytest.raises(ValueError):
        results.screen_tensors([], [row], [row], pressure=1.5, stress=120)


def test_piecewise_tensor_linearization_retains_membrane_and_bending():
    low = dict(sx=80., sy=0., sz=0., sxy=0., syz=0., sxz=0.)
    high = dict(low, sx=120.)
    mid, top, bottom = results.linearize_segments([(0., 10., low, high)])
    assert mid['sx'] == pytest.approx(100)
    assert top['sx'] == pytest.approx(120)
    assert bottom['sx'] == pytest.approx(80)
    split = results.linearize_segments([(0.,5.,low,dict(low,sx=100)),
                                       (5.,10.,dict(low,sx=100),high)])
    for actual, expected in zip(split, (mid, top, bottom)):
        assert actual == pytest.approx(expected)


def test_radial_components_retain_membrane_without_bending():
    low = dict(sx=80.,sy=0.,sz=-10.,sxy=2.,syz=4.,sxz=6.)
    high = dict(sx=120.,sy=0.,sz=0.,sxy=4.,syz=8.,sxz=10.)
    mid, top, bottom = results.linearize_segments([(1000.,1010.,low,high)])
    for key in ('sz','syz','sxz'):
        assert top[key] == bottom[key] == mid[key]
    assert top['sxy'] == 4.


def test_interior_raw_trace_cannot_be_hidden_by_linearization():
    reconstructed = dict(sx=100., sy=100., sz=100., sxy=0., syz=0., sxz=0.)
    peak = dict(reconstructed, sx=200., sy=200., sz=200.)
    r = results.screen_tensors([reconstructed], [reconstructed], [reconstructed],
        pressure=1.5, stress=120, raw_points=[peak])
    assert r['demands_mpa']['local_failure'] == 600
    assert not r['acceptance']


def test_incomplete_radial_column_is_rejected():
    model = dict(nodes=[], basis=dict(radial_layers=4),
                 elements=[dict(column_id=1,radial_layer=0,nodes=[])])
    with pytest.raises(ValueError, match='radial layer coverage'):
        results.linearize_solid(model, [])


def test_citation_validation_fails_closed(tmp_path, monkeypatch):
    monkeypatch.setenv('LLM_WIKI_PATH', str(tmp_path/'missing'))
    row = dict(sx=100.,sy=0.,sz=0.,sxy=0.,syz=0.,sxz=0.)
    with pytest.raises(Exception, match='missing|resolve|found'):
        results.screen_tensors([row],[row],[row],pressure=1.5,stress=120,wiki_root=tmp_path/'missing')
