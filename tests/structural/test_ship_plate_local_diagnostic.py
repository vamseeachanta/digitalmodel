"""Limit checks on the existing isolated-patch wrapper, not damage acceptance."""
import importlib.util
from pathlib import Path

import pytest

path = Path(__file__).parents[2] / 'scripts/studies/ship_plate_local_loss_diagnostic.py'
spec = importlib.util.spec_from_file_location('local_diagnostic', path)
study = importlib.util.module_from_spec(spec)
spec.loader.exec_module(study)


def test_full_patch_matches_uniform_at_every_saved_loss():
    row = study.compute(1200., 600., 100.)
    for point in row['curve']:
        assert point['isolated_patch_utilization'] == pytest.approx(point['whole_field_uniform_comparison_utilization'])
        assert point['isolated_patch_critical_MPa'] == pytest.approx(point['whole_field_uniform_comparison_critical_MPa'])


def test_zero_loss_still_omits_parent_mode():
    row = study.compute(600., 300., 50.)
    zero = row['curve'][0]
    assert zero['metal_loss_mm'] == 0
    assert zero['isolated_patch_utilization'] < zero['parent_nominal_utilization']
    assert row['acceptance_status'] == 'inapplicable_unvalidated_local_patch'
    assert row['minimum_remaining_thickness_mm'] is None
    assert not {'passes', 'level', 'max_acceptable_loss_mm', 'model_note'} & set(zero)


def test_elastic_zero_loss_narrow_patch_omits_parent_mode_by_factor_four():
    # Both fields are on the elastic branch at 2 mm. Merely naming a patch
    # must not be interpreted as giving the unchanged parent new supports.
    parent = study.PlateGeometry(1200.,600.,2.)
    full = study.assess_plate_uniform_loss(parent,study.STEEL_AH36,0.,sigma_x=50.)
    patch = study.assess_plate_local_loss(parent,study.STEEL_AH36,0.,600.,300.,sigma_x=50.)
    assert patch.utilization == pytest.approx(full.utilization/4)


@pytest.mark.parametrize('length,breadth,stress', [(0,300,50), (1300,300,50),
    (600,700,50), (600,float('nan'),50), (600,300,-1), (True,300,50)])
def test_invalid_diagnostic_geometry_and_load_rejected(length,breadth,stress):
    with pytest.raises(ValueError):
        study.compute(length,breadth,stress)
