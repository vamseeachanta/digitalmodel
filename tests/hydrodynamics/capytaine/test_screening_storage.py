# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Read the held public export and retain SI result metadata.

The unit-response control is synthetic, assumed, based on previous experience.
"""

import numpy as np
import pytest


def test_reference_input_shape_and_units():
    """The held export contains 32 periods, nine headings and six DOFs."""
    from digitalmodel.hydrodynamics.capytaine.screening_io import read_reference
    held = read_reference()
    assert held['added_mass'].shape == (32, 6, 6)
    assert held['damping'].shape == (32, 6, 6)
    assert held['amplitude'].shape == (32, 9, 6)
    np.testing.assert_allclose(held['headings_deg'], np.arange(0, 181, 22.5))
    assert min(held['periods_s']) == pytest.approx(3.75)
    assert max(held['periods_s']) == pytest.approx(30)
    assert held['added_mass'][0, 2, 2] == pytest.approx(14772212.62270797)
    assert held['amplitude'][0, 0, 4] == pytest.approx(0.01079980121073181)
    assert held['mass_kg'] == pytest.approx(14072718)
    assert held['inertia'].shape == (6, 6)


def test_reference_parallel_axis_transfer():
    """The origin roll inertia adds mass times the squared vertical offset."""
    from digitalmodel.hydrodynamics.capytaine.screening_io import read_reference
    held = read_reference()
    expected = 11.3274304299744e9 + held['mass_kg'] * held['cog'][2]**2
    assert held['inertia'][3, 3] == pytest.approx(expected)
    assert held['external_damping'][2, 2] == 180000
    assert held['external_stiffness'][0, 0] == pytest.approx(69836.53348045482)


def test_si_schema_rotations_and_solver_source_round_trip():
    """Synthetic assumed unit response stores rotations in degrees per metre."""
    from digitalmodel.hydrodynamics.capytaine.screening_io import diffraction_results
    from digitalmodel.hydrodynamics.capytaine.models import BEMResult
    from digitalmodel.hydrodynamics.diffraction.output_schemas import DiffractionResults, validate_diffraction_results
    bem = BEMResult(omegas=np.array([1.0, 2.0]), periods=np.array([2*np.pi, np.pi]),
                    headings=np.array([0.0]), dof_names=['Surge','Sway','Heave','Roll','Pitch','Yaw'],
                    added_mass=np.tile(np.eye(6), (2, 1, 1)),
                    radiation_damping=np.tile(np.eye(6), (2, 1, 1)),
                    body_name='SEMI-G1', water_depth=1000)
    hydro = dict(displacement_volume=1.0, mass=1025.0, centre_of_gravity=[0,0,-1],
                 centre_of_buoyancy=[0,0,-0.5], waterplane_area=1.0, stiffness_matrix=np.eye(6))
    result = diffraction_results(bem, np.ones((2, 1, 6), dtype=complex), hydro, '2026-10-13')
    assert result.raos.pitch.magnitude[0, 0] == pytest.approx(180/np.pi)
    assert result.added_mass.matrices[0].source == 'solver'
    assert result.phase_convention == 'unknown'
    assert result.analysis_tool == 'Capytaine'
    assert result.unit_system == 'SI'
    reconstructed = DiffractionResults.from_dict(result.to_dict())
    assert reconstructed.to_dict() == result.to_dict()
    assert all(not findings for findings in validate_diffraction_results(result).values())
