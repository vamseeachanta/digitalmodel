"""Generated-form BRep qualification; archived-run and native-status comparators."""

import numpy as np
import pytest

pytest.importorskip("hullprod")
pytestmark = pytest.mark.slow

from digitalmodel.hydrodynamics.hull_library import (
    MonohullFormParameters,
    generate_profile,
)
from digitalmodel.hydrodynamics.hull_library.curvature_screen import screen_step
from digitalmodel.hydrodynamics.hull_library.hull_surface_brep import (
    bspline_face_from_grid,
    profile_to_step,
)
from digitalmodel.hydrodynamics.hull_library.hull_surface_brep_sampling import (
    section_arclength_grid,
)
from scripts.hull_library.diagnose_brep_geometry import sample_surface
from scripts.hull_library.diagnose_form_brep import native_acceptance


def generated_profile(case):
    extra = (
        {}
        if case == "rounded"
        else {"cb": 0.72, "transom_fraction": 0.9, "lcb_fraction": -0.1}
    )
    return generate_profile(
        MonohullFormParameters(length_bp=100, beam=20, draft=8, depth=12, **extra)
    )


@pytest.mark.parametrize("case", ["rounded", "transom"])
def test_default_generated_form_preserves_invalid_status(case, tmp_path):
    """Archived-run comparator: preserve the reproduced default failure contract."""
    profile = generated_profile(case)
    path = profile_to_step(profile, tmp_path / f"{case}.step", n_x=41, n_z=21)
    result = screen_step(path, lref=profile.length_bp)
    expected = (
        "geometric_singularity_nonintegrable"
        if case == "rounded"
        else "quadrature_unconverged"
    )
    assert result.signature.status == expected
    assert np.isnan(result.signature.I_D)


@pytest.mark.parametrize("case", ["rounded", "transom"])
def test_coarse_candidate_native_gates_are_not_geometry_acceptance(case, tmp_path):
    profile = generated_profile(case)
    path = profile_to_step(
        profile, tmp_path / f"{case}.step", n_x=41, n_z=21, sampling="section_arclength"
    )
    result = screen_step(path, lref=profile.length_bp)
    assert native_acceptance(result.signature, result.provenance) == []


@pytest.mark.parametrize("case", ["rounded", "transom"])
@pytest.mark.xfail(
    strict=True,
    raises=AssertionError,
    reason="#2241: coarse arclength fit overshoots keel by >1 mm; limit 1e-6 m",
)
def test_candidate_stays_inside_reference_depth(case):
    profile = generated_profile(case)
    face = bspline_face_from_grid(section_arclength_grid(profile, 41, 21))
    geometry = sample_surface(face, 161)
    assert geometry["bounds_min_m"][2] >= -profile.draft - 1e-6


class KnownPlanarityFailure(AssertionError):
    """Only the specifically reproduced candidate rejection is expected."""


@pytest.mark.parametrize("case", ["rounded", "transom"])
@pytest.mark.parametrize("nx,nz", [(81, 41), (161, 81)])
@pytest.mark.xfail(
    strict=True,
    raises=KnownPlanarityFailure,
    reason="#2241: fine arclength fit has nonplanar keel (46–56 mm)",
)
def test_candidate_refinement_preserves_planar_keel(case, nx, nz, tmp_path):
    try:
        profile_to_step(
            generated_profile(case),
            tmp_path / "candidate.step",
            n_x=nx,
            n_z=nz,
            sampling="section_arclength",
        )
    except ValueError as error:
        assert str(error) == "keel must be planar at the profile draft"
        raise KnownPlanarityFailure(str(error)) from error
