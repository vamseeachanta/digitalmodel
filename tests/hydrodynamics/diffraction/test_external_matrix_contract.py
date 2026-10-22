# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""External matrix contract; all coefficients below are synthetic inputs.

Units follow OrcaWave help, Constraints (external matrix block equations),
accessed 2026-10-22. No coefficient is an engineering acceptance limit.
"""

import copy

import numpy as np
import pytest
from pydantic import ValidationError

from digitalmodel.hydrodynamics.diffraction.input_schemas import VesselSpec


def matrix_body(**updates):
    """A synthetic rigid body; geometry and inertia are arbitrary test inputs."""
    data = {
        "name": "synthetic",
        "geometry": {"mesh_file": "synthetic.gdf"},
        "inertia": {
            "mass": 1000.0,
            "centre_of_gravity": [0.0, 0.0, 2.0],
            "radii_of_gyration": [1.0, 1.0, 1.0],
        },
        "external_stiffness": np.diag([2., 3., 5., 7., 11., 13.]).tolist(),
        "external_stiffness_units": "SI",
        "external_stiffness_reference_point": "centre_of_gravity",
        "external_damping": np.diag([2., 3., 5., 7., 11., 13.]).tolist(),
        "external_damping_units": "SI",
        "external_damping_reference_point": "body_origin",
    }
    data.update(updates)
    return data


@pytest.mark.parametrize("point", ["body_origin", "centre_of_gravity"])
def test_accepts_explicit_contract_and_preserves_serialization(point):
    data = matrix_body(external_stiffness_reference_point=point)
    body = VesselSpec.model_validate(data)
    assert body.external_stiffness_reference_point == point
    assert VesselSpec.model_validate(body.model_dump()) == body


@pytest.mark.parametrize("kind", ["stiffness", "damping"])
@pytest.mark.parametrize("suffix", ["units", "reference_point"])
def test_missing_contract_is_rejected(kind, suffix):
    data = matrix_body()
    del data[f"external_{kind}_{suffix}"]
    with pytest.raises(ValidationError, match=f"external_{kind}_{suffix}"):
        VesselSpec.model_validate(data)


@pytest.mark.parametrize("kind", ["stiffness", "damping"])
@pytest.mark.parametrize("suffix,value", [("units", "tonnes"),
                                         ("reference_point", "unspecified")])
def test_unknown_contract_is_rejected(kind, suffix, value):
    with pytest.raises(ValidationError):
        VesselSpec.model_validate(matrix_body(**{f"external_{kind}_{suffix}": value}))


@pytest.mark.parametrize("kind", ["stiffness", "damping"])
@pytest.mark.parametrize("bad", [[], [[0.] * 6] * 5, [[0.] * 5] * 6,
                                 [[0.] * 6] * 7, [[0.], [0.] * 6]])
def test_wrong_shape_is_rejected(kind, bad):
    with pytest.raises(ValidationError, match="6x6"):
        VesselSpec.model_validate(matrix_body(**{f"external_{kind}": bad}))


@pytest.mark.parametrize("kind", ["stiffness", "damping"])
@pytest.mark.parametrize("value", [float("nan"), float("inf"), -float("inf")])
def test_nonfinite_is_rejected(kind, value):
    data = matrix_body()
    data[f"external_{kind}"][0][0] = value
    with pytest.raises(ValidationError, match="finite"):
        VesselSpec.model_validate(data)


@pytest.mark.parametrize("kind", ["stiffness", "damping"])
def test_exact_reciprocal_symmetry_is_required(kind):
    """Canonical reciprocal linear matrices require Kij=Kji, without tolerance."""
    data = copy.deepcopy(matrix_body())
    data[f"external_{kind}"][0][3] = 1.0
    with pytest.raises(ValidationError, match="symmetric"):
        VesselSpec.model_validate(data)


def test_no_matrices_needs_no_contract():
    data = matrix_body()
    for key in list(data):
        if key.startswith("external_"):
            del data[key]
    assert VesselSpec.model_validate(data).external_stiffness is None


@pytest.mark.parametrize("kind", ["stiffness", "damping"])
def test_nonfinite_cog_is_rejected_with_a_matrix(kind):
    data = matrix_body()
    data["inertia"]["centre_of_gravity"][0] = float("nan")
    with pytest.raises(ValidationError, match="finite centre_of_gravity"):
        VesselSpec.model_validate(data)
