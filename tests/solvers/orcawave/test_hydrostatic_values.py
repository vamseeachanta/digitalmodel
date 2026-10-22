# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Solver-free hydrostatic checks through the actual public converter.

Source: WAMIT User Manual v7.5, section 3.1, hydrostatic and gravitational
restoring coefficients. No solver binding is needed for an offline mapping.
"""

import importlib.abc
import importlib
from pathlib import Path
import sys

import numpy as np
import pytest

from digitalmodel.solvers.orcawave.hydrostatics import HydrostaticProperties
from digitalmodel.solvers.orcawave.diffraction.scripts.convert_to_orcaflex import (
    OrcaFlexConverter,
    VesselData,
)


@pytest.fixture
def unit_box():
    """Use the tracked unit-box panels; missing geometry fails rather than skips."""
    path = Path(__file__).resolve().parents[3] / 'tests/hydrodynamics/diffraction/fixtures/acceptance_610/unit_box.gdf'
    assert path.is_file(), 'Required unit-box fixture is missing'
    vertices = np.loadtxt(path, skiprows=4)
    assert vertices.shape == (20, 3)
    np.testing.assert_array_equal(vertices.min(axis=0), [0, 0, -1])
    np.testing.assert_array_equal(vertices.max(axis=0), [1, 1, 0])
    return vertices


@pytest.fixture
def vessel_data():
    return VesselData(
        name='synthetic-box', frequencies=np.array([1.0]), directions=np.array([0.0]),
        added_mass=np.zeros((1, 6, 6)), damping=np.zeros((1, 6, 6)),
        excitation_force=np.zeros((1, 1, 6), complex),
        excitation_moment=np.zeros((1, 1, 6), complex), displacement=1,
        centre_of_gravity=[0, 0, -0.5], centre_of_buoyancy=[0, 0, -0.5],
        waterplane_area=1, metacentric_height=1/12,
    )


@pytest.mark.parametrize('rho,gravity', [(1000, 10), (1025, 9.81)])
def test_adapter_preserves_box_restoring(unit_box, vessel_data, tmp_path, rho, gravity):
    """Hand: centered waterplane [-1/2,1/2]^2, A=1 m², V=1 m³.

    Ixx=Iyy=integral(y² dxdy)=1/12 m⁴. With zB=zG=-1/2 m,
    C33=rho*g; C44=C55=rho*g*(1/12+1*(-1/2+1/2))=rho*g/12.
    rho and g are selected arithmetic inputs, not material/site defaults.
    Matrix indices follow surge,sway,heave,roll,pitch,yaw; rotations are radians.
    Fixture geometry is translated to its waterplane centroid for these axes.
    """
    properties = HydrostaticProperties(
        density=rho, gravity=gravity, volume=1, waterplane_area=1,
        waterplane_ixx=1/12, waterplane_iyy=1/12, buoyancy_z=-0.5, gravity_z=-0.5,
    )
    result = OrcaFlexConverter(str(tmp_path), str(tmp_path / 'output')).convert_to_orcaflex(
        vessel_data, hydrostatics=properties,
    )['VesselType']['Hydrostatics']
    expected = np.diag([0, 0, rho*gravity, rho*gravity/12, rho*gravity/12, 0])
    np.testing.assert_allclose(result['RestoringMatrix'], expected, rtol=1e-12, atol=0)
    assert result['Density'] == rho
    assert result['Gravity'] == gravity
    assert result['TranslationUnits'] == 'm'
    assert result['RotationUnits'] == 'rad'
    assert result['ForceUnits'] == 'N'
    assert result['MomentUnits'] == 'N m'
    assert result['Status'] == 'EVALUATED'


@pytest.mark.parametrize('zg,expected', [(-0.75, 10000/3), (-0.25, -10000/6)])
def test_buoyancy_and_gravity_terms(vessel_data, tmp_path, zg, expected):
    """Hand with rho*g=10000: C44=C55=10000*(1/12 + zB-zG).

    zB=-1/2. For zG=-3/4, coefficient=10000/3; for zG=-1/4,
    coefficient=-10000/6. Negative stiffness is preserved, not clamped.
    """
    properties = HydrostaticProperties(1000, 10, 1, 1, 1/12, 1/12, -0.5, zg)
    result = OrcaFlexConverter(str(tmp_path), str(tmp_path)).convert_to_orcaflex(
        vessel_data, hydrostatics=properties,
    )['VesselType']['Hydrostatics']['RestoringMatrix']
    assert result[3][3] == pytest.approx(expected)
    assert result[4][4] == pytest.approx(expected)


def test_missing_hydrostatics_are_not_evaluated(vessel_data, tmp_path):
    result = OrcaFlexConverter(str(tmp_path), str(tmp_path)).convert_to_orcaflex(vessel_data)
    assert result['VesselType']['Hydrostatics']['Status'] == 'NOT_EVALUATED'
    assert 'RestoringMatrix' not in result['VesselType']['Hydrostatics']


def test_unequal_roll_pitch_inertia(vessel_data, tmp_path):
    """Hand: rho*g=10000 and zB=zG -> C44=20000, C55=30000.

    Distinct input second moments expose an axis swap hidden by a square box.
    """
    properties = HydrostaticProperties(1000, 10, 1, 1, 2, 3, 0, 0)
    result = OrcaFlexConverter(str(tmp_path), str(tmp_path)).convert_to_orcaflex(
        vessel_data, hydrostatics=properties,
    )['VesselType']['Hydrostatics']['RestoringMatrix']
    assert result[3][3] == 20000
    assert result[4][4] == 30000


@pytest.mark.parametrize('field,value', [('density', 0), ('gravity', -1), ('volume', float('nan')), ('waterplane_area', -1), ('waterplane_ixx', -1), ('buoyancy_z', float('inf'))])
def test_invalid_hydrostatic_inputs(field, value):
    inputs = dict(density=1000, gravity=10, volume=1, waterplane_area=1,
                  waterplane_ixx=1/12, waterplane_iyy=1/12, buoyancy_z=-0.5, gravity_z=-0.5)
    inputs[field] = value
    with pytest.raises(ValueError):
        HydrostaticProperties(**inputs).to_mapping()


def test_no_licensed_import_during_adapter_reload():
    """Fail an attempted binding import; never skip a missing adapter module."""
    class RejectLicensed(importlib.abc.MetaPathFinder):
        def find_spec(self, fullname, path=None, target=None):
            if fullname.split('.')[0] in {'OrcFxAPI', 'OrcaWave', 'win32com'}:
                pytest.fail('Offline adapter attempted a licensed binding import')
            return None

    guard = RejectLicensed()
    sys.meta_path.insert(0, guard)
    try:
        module = importlib.import_module('digitalmodel.solvers.orcawave.diffraction.scripts.convert_to_orcaflex')
        importlib.reload(module)
        assert module.OrcaFlexConverter is not None
    finally:
        sys.meta_path.remove(guard)


def test_serialized_outputs_preserve_restoring_record(vessel_data, tmp_path):
    """Hand: rho*g=10000, A=1, I=1/12, zB=zG -> diag 10000,10000/12."""
    import yaml

    converter = OrcaFlexConverter(str(tmp_path), str(tmp_path))
    properties = HydrostaticProperties(1000, 10, 1, 1, 1/12, 1/12, 0, 0)
    converted = converter.convert_to_orcaflex(vessel_data, hydrostatics=properties)
    converter.save_orcaflex_file(converted, filename='synthetic.yml')
    full = yaml.safe_load((tmp_path/'synthetic.yml').read_text())
    compact = yaml.safe_load((tmp_path/'compact_synthetic.yml').read_text())
    expected = converted['VesselType']['Hydrostatics']
    assert full['VesselType']['Hydrostatics'] == expected
    assert compact['HydrodynamicData']['Hydrostatics'] == expected
