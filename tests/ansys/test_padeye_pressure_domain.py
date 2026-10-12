"""Independent full-domain gate; geometry mutation tests are not native evidence."""
import copy

import pytest

from digitalmodel.ansys.padeye_pressure import build_pressure_mesh, pressure_geometry
from tests.ansys.padeye_pressure_domain import verify_frozen_pressure_domain


@pytest.mark.parametrize('size', [10, 5, 2.5])
def test_generated_domain_has_complete_plate_and_hole(size):
    result = verify_frozen_pressure_domain(build_pressure_mesh(pressure_geometry(size)))
    assert result['outer_perimeter_mm'] == pytest.approx(1400)
    assert result['frozen_domain_verified'] is True


@pytest.mark.parametrize('change', ['left_out', 'left_in', 'top_out', 'lower_hole', 'extra_hole'])
def test_changed_plate_or_hole_cannot_pass_frozen_domain(change):
    mesh = copy.deepcopy(build_pressure_mesh(pressure_geometry(10)))
    for node in mesh['nodes']:
        if change == 'left_out' and node['x_mm'] == 0:
            node['x_mm'] = -10
        elif change == 'left_in' and node['x_mm'] == 0:
            node['x_mm'] = 7
        elif change == 'top_out' and node['y_mm'] == 300:
            node['y_mm'] = 310
        elif change == 'lower_hole' and node['id'] == 25:
            node['y_mm'] -= 1
    if change == 'extra_hole':
        mesh['elements'].pop(200)
    with pytest.raises(ValueError):
        verify_frozen_pressure_domain(mesh)
