"""Independent checks for the conditional 2007 example assessment."""
import math
import pytest
from digitalmodel.asset_integrity.assessment.example_vessel_data import AREAS, sample, _basis
from digitalmodel.asset_integrity.assessment.example_vessel_ffs import (
    assess, folias, profile_rsf, circumferential_required, convergence, WIKI_PATH,
)
from digitalmodel.citations.schema import CitationResolutionError


@pytest.fixture
def wiki(tmp_path):
    target = tmp_path / WIKI_PATH
    target.parent.mkdir(parents=True)
    target.write_text('---\ncode_id: api-579-1\npublisher: API\nrevision: 2007\n---\n')
    return tmp_path


def test_missing_citation_fails_closed(tmp_path):
    with pytest.raises(CitationResolutionError):
        assess(AREAS[0], sample(AREAS[0], 25), _basis(AREAS), tmp_path)


def test_rectangular_profile_oracle():
    points = [[0, 8], [50, 8], [100, 8]]
    mt = folias(1.285 * 100 / math.sqrt(2000 * 15.3))
    expected = (8 / 15.3) / (1 - (1 - 8 / 15.3) / mt)
    result = profile_rsf(points, 15.3, 2000)
    assert result['rsf'] == pytest.approx(expected)
    assert result['interval_mm'] == [0, 100]


def test_spatial_order_matters():
    separated = [[0, 15], [50, 5], [100, 15], [150, 5], [200, 15]]
    adjacent = [[0, 15], [50, 5], [100, 5], [150, 15], [200, 15]]
    assert profile_rsf(adjacent, 15, 2000)['rsf'] < profile_rsf(separated, 15, 2000)['rsf']


def test_sound_pressure_and_citation(wiki):
    result = assess(AREAS[0], sample(AREAS[0], 25), _basis(AREAS), wiki)
    assert result['sound_mawp_mpa'] == pytest.approx(120 * 15.3 / (1000 + .6 * 15.3))
    assert result['citations'][0]['revision'] == '2007'
    assert result['code_qualified_actual_asset'] is False


def test_circumferential_piecewise_and_interpolation():
    assert circumferential_required(.1, 1.0) == .2
    expected = .85947 - .40012 / 2 - 2.7979 / 4 + 5.0729 / 8 - 3.5217 / 16 + .91877 / 32
    assert circumferential_required(2, 1.0) == pytest.approx(expected)
    assert circumferential_required(2, 1.1) == pytest.approx(
        (circumferential_required(2, 1.0) + circumferential_required(2, 1.2)) / 2)
    with pytest.raises(ValueError):
        circumferential_required(10, 1)


@pytest.mark.parametrize('value', [float('nan'), float('inf'), -1, 0])
def test_invalid_profile_rejected(value):
    with pytest.raises(ValueError):
        profile_rsf([[0, value], [1, 10]], 15, 2000)


def test_unsorted_profile_rejected():
    with pytest.raises(ValueError):
        profile_rsf([[1, 8], [0, 8]], 15, 2000)


def test_folias_table_endpoints():
    assert folias(0) == pytest.approx(1.001)
    assert folias(20) == pytest.approx(24.027, abs=.003)
    assert folias(25) == folias(20)


def test_negative_allowance_rejected(wiki):
    basis = _basis(AREAS)
    basis['future_loss_mm'] = -1
    with pytest.raises(ValueError):
        assess(AREAS[0], sample(AREAS[0], 25), basis, wiki)


def test_incomplete_grid_rejected(wiki):
    grid = sample(AREAS[0], 25)
    grid['rows'].pop()
    with pytest.raises(ValueError):
        assess(AREAS[0], grid, _basis(AREAS), wiki)


def test_grid_basis_lineage_mismatch_rejected(wiki):
    basis = _basis(AREAS)
    basis['future_loss_mm'] = .1
    with pytest.raises(ValueError):
        assess(AREAS[0], sample(AREAS[0], 25), basis, wiki)


def test_four_area_routes(wiki):
    results = [assess(a, sample(a, 12.5), _basis(AREAS), wiki) for a in AREAS]
    assert [r['level1']['acceptance'] for r in results] == [True, False, False, False]
    assert [r['level2']['acceptance'] for r in results] == [True, True, False, False]
    assert results[-1]['level2']['pressure_rating_qualified'] is False
    assert 'circumferential applicability' in results[-1]['level2']['non_pass_reasons'][0]


def test_fine_profile_convergence(wiki):
    result = convergence(AREAS[1], _basis(AREAS), wiki)
    assert result['criterion_met']
    assert result['relative_rsf_change'] < .01
    assert result['status_stable']


def test_clipped_grid_rejected(wiki):
    grid = sample(AREAS[0], 25)
    grid['rows'] = [r for r in grid['rows'] if abs(r['local_x_mm']) <= 25]
    with pytest.raises(ValueError):
        assess(AREAS[0], grid, _basis(AREAS), wiki)


def test_unreported_loss_beyond_declared_footprint_rejected(wiki):
    grid = sample(AREAS[0], 25)
    row = grid['rows'][0]
    row['current_mm'] -= .5
    row['assessed_mm'] -= .5
    with pytest.raises(ValueError, match='footprint'):
        assess(AREAS[0], grid, _basis(AREAS), wiki)


def test_assumed_elliptical_head_pressure_check(wiki):
    from digitalmodel.asset_integrity.assessment.example_vessel_ffs import head_check
    result = head_check(_basis(AREAS), wiki)
    assert result['assessed_head_thickness_mm'] == pytest.approx(19.3)
    assert result['required_thickness_mm'] == pytest.approx(3000 / 239.7)
    assert result['mawp_mpa'] == pytest.approx(4632 / 2003.86)
    assert result['acceptance']
