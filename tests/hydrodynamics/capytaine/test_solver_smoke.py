# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Exercise one sphere period through the optional native solver.

The radius, frequency, analytic expression and tolerance follow
test_sphere_benchmark.py::TestSphereValidation.test_heave_added_mass_analytical.
"""

import numpy as np
import pytest

# Hosted checks do not install Capytaine, so this file is skipped there.
# Local: PYTHONPATH=src uv run --no-sources --with capytaine==3.0.0 python -m pytest \
#   tests/hydrodynamics/capytaine/test_solver_smoke.py -q
pytest.importorskip("capytaine")
pytestmark = pytest.mark.slow

from digitalmodel.hydrodynamics.capytaine import (
    BodyConfig, DOF, MeshConfig, WaveConditions, run_bem_analysis,
)


def test_one_period_sphere_has_finite_coefficients_and_analytical_added_mass():
    """One heave period satisfies the existing benchmark's fifteen-percent bound."""
    radius, rho = 5.0, 1025.0
    body = BodyConfig(
        mesh=MeshConfig(predefined_shape="sphere", shape_params={
            "radius": radius, "center": (0, 0, 0), "resolution": (40, 40),
        }),
        dofs=[DOF.HEAVE], center_of_mass=(0, 0, 0),
    )
    waves = WaveConditions(omegas=np.array([0.1]), headings=np.array([0.0]), rho=rho)
    result = run_bem_analysis(body, waves, compute_hydrostatics=False)
    assert result.added_mass.shape == (1, 1, 1)
    assert result.radiation_damping.shape == (1, 1, 1)
    assert np.all(np.isfinite(result.added_mass))
    assert np.all(np.isfinite(result.radiation_damping))
    analytical = (2.0 / 3.0) * rho * np.pi * radius**3
    assert abs(result.added_mass[0, 0, 0] - analytical) / analytical < 0.15
