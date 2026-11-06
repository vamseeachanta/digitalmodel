"""Characterize generated geometry; see docs/domains/naval-architecture/generated-geometry-audit.md.

Area/flux equations: Polyhedral Mass Properties (Revisited), 2009, sections 2–3,
equations (2), (7)–(11). All dimensions below are manufactured binary fixtures,
not engineering acceptance values. Expected values are hand-derived per test.
The existing policy coefficients are cited in the audit's threshold inventory.
"""

import numpy as np
import pytest

from digitalmodel.naval_architecture import mesh_hydrostatics as hydro
from digitalmodel.naval_architecture.mesh_clipping import _check_physical_faces
from digitalmodel.naval_architecture.mesh_geometry import _DEGENERATE_AREA_REL
from digitalmodel.naval_architecture import appendage_submergence, kcs_geometry
from digitalmodel.naval_architecture import solid_occlusion, symmetry_cut
from digitalmodel.naval_architecture.mesh_sections import SectionIndex
from digitalmodel.naval_architecture.trim_hydrostatics import waterplane_extent


@pytest.mark.parametrize("scale", [2.0**-10, 1.0, 2.0**10])
@pytest.mark.parametrize("height_in_eps,refused", [(1, True), (2, True), (4, False)])
def test_generated_guard_boundary_uses_local_edge_scale(scale, height_in_eps, refused):
    """For legs s and s*h: A=s²*h/2 and Lmax² rounds to s².

    With h=k*eps, the existing A <= eps*Lmax² rule refuses k <= 2,
    including equality, and accepts k=4. Scaling cancels exactly. Coefficient
    source: mesh_clipping._check_physical_faces (repository policy, 2026).
    Binary powers keep the arithmetic exact for this boundary experiment.
    """
    height = height_in_eps * np.finfo(float).eps
    vertices = scale * np.array([[0, 0, 0], [1, 0, 0], [0, height, 0]])
    faces = np.array([[0, 1, 2]])
    if refused:
        with pytest.raises(hydro.MeshContractError, match="physical faces"):
            _check_physical_faces(vertices, faces, np.array([False]))
    else:
        _check_physical_faces(vertices, faces, np.array([False]))


def test_source_floor_and_generated_guard_have_distinct_contracts():
    """A thin tetrahedron has two faces of area h/2, h=8*eps.

    Its diagonal² is about 2, so h/2 < the source coefficient * diagonal².
    Each triangle nevertheless has A/Lmax² >= h/4 = 2*eps, above the
    generated-face guard. This tests both consumers on identical geometry,
    without using either consumer to compute the expected decision.
    Coefficients: mesh_geometry._DEGENERATE_AREA_REL and
    mesh_clipping._check_physical_faces (repository policy, 2026).
    """
    h = 8 * np.finfo(float).eps
    vertices = np.array([[0, 0, 0], [1, 0, 0], [0, h, 0], [0, 0, 1]])
    faces = np.array([[0, 2, 1], [0, 1, 3], [0, 3, 2], [1, 2, 3]])
    with pytest.raises(hydro.MeshContractError, match="degenerate"):
        hydro.TriMesh(vertices, faces, units="m", axes=hydro.CANONICAL_AXES)
    _check_physical_faces(vertices, faces, np.zeros(len(faces), dtype=bool))


@pytest.mark.parametrize("kind", ["collinear", "repeated", "nonfinite"])
def test_generated_guard_refuses_unusable_physical_faces(kind):
    """Zero cross product, repeated index, or NaN area must be refused.

    These are representation failures, independent of a source quality floor.
    """
    vertices = np.array([[0.0, 0, 0], [1, 0, 0], [2, 0, 0]])
    faces = np.array([[0, 1, 1 if kind == "repeated" else 2]])
    if kind == "nonfinite":
        vertices[2, 1] = np.nan
    with pytest.raises(hydro.MeshContractError, match="physical faces"):
        _check_physical_faces(vertices, faces, np.array([False]))


def test_signed_cap_can_have_zero_area():
    """A collinear artificial fan contributes exactly zero to area/flux sums."""
    vertices = np.array([[0, 0, 0], [1, 0, 0], [2, 0, 0]])
    _check_physical_faces(vertices, np.array([[0, 1, 2]]), np.array([True]))


@pytest.mark.parametrize("offset", [0.0, 2.0**8])
@pytest.mark.parametrize("trim_fraction", [-1.0, 0.0, 1.0])
def test_near_keel_trimmed_box_keeps_resolved_faces_below_source_floor(offset, trim_fraction):
    """Box L=8, B=4, H=2, mean draft d=2**-20, trim=±d or 0.

    Depth is d + trim*x/L, x in [-L/2,L/2]. Thus V=B*integral(depth dx)
    = L*B*d = 32*d; the odd trim term integrates to zero. Local depth stays
    positive. Horizontal translation cannot change this integral. The small
    corner triangles have area proportional to d² and fall below the source
    floor while remaining resolved. The comparison budget 2**-20 is a fixture
    regression precision (20 binary fractional bits), not a mesh quality limit.
    Source floor: mesh_geometry._DEGENERATE_AREA_REL (repository policy, 2026).
    """
    vertices, faces = hydro.box_mesh(8, 4, 2)
    vertices[:, 0] += offset
    mesh = hydro.TriMesh(vertices, faces, units="m", axes=hydro.CANONICAL_AXES)
    draft = 2.0**-20
    clipped = hydro.clip_at_waterline(mesh, draft, trim_fraction * draft)
    assert np.min(clipped.face_areas()[~clipped.artificial]) < (
        _DEGENERATE_AREA_REL * mesh.scale_diag**2
    )
    assert hydro.volume_tetra(clipped.vertices, clipped.faces) == pytest.approx(
        32 * draft, rel=2.0**-20, abs=0
    )


@pytest.mark.parametrize("consumer", ["surface_area", "volume_clip", "symmetry", "appendage"])
@pytest.mark.parametrize("offset", [0.0, 2.0**8])
@pytest.mark.parametrize("height", [0.0, 2.0**-20, 0.5, 1.0])
@pytest.mark.parametrize("levels", [0, 1, 3])
def test_clipping_area_survives_translation_and_subdivision(consumer, offset, height, levels):
    """Triangle (o,3,0),(o+1,3,1),(o-1,3,1) has area 1.

    Cutting at z=h gives a similar triangle of base 2h and height h, so
    A=h², including zero for tangency. Midpoint subdivision produces four
    triangles of quarter area, preserving the cut region at every level.
    Translation along x changes neither area nor the -y normal. The signed
    y flux is therefore -3*h². Binary inputs make these comparisons exact;
    no tolerance or source-area floor is appropriate for these fixtures.
    """
    triangles = [((offset, 3.0, 0.0), (offset + 1, 3.0, 1.0), (offset - 1, 3.0, 1.0))]
    for _ in range(levels):
        triangles = [child for tri in triangles for child in solid_occlusion._subdivide(tri)]
    if consumer == "surface_area":
        area = kcs_geometry.wetted_surface_area(triangles, height)
    elif consumer == "appendage":
        area = sum(appendage_submergence._area_submerged(t, lambda p: height - p[2])
                   for t in triangles)
    else:
        pieces = [piece for t in triangles for piece in (
            kcs_geometry._clip_polygon_below(t, height) if consumer == "volume_clip"
            else symmetry_cut._clip(t, 2, height, True)
        )]
        # Independent projection: in the x-z plane, -Ay is unsigned area.
        area = -sum(symmetry_cut._projected(piece, 1) for piece in pieces)
        assert sum(symmetry_cut._flux(piece, 1) for piece in pieces) == -3 * height**2
    assert area == height**2


@pytest.mark.parametrize("offset", [0.0, 2.0**8])
def test_tilted_depth_uses_original_triangle_area(offset):
    """Use the same unit-area triangle with depth=h-z-(x-o)/2, h=2**-20.

    Depth at the top corners is h-3/2 and h-1/2. The retained edge fractions
    are h/(3/2) and h/(1/2); their product times parent area is 4*h²/3.
    The 20-bit relative regression budget matches the box fixture above.
    No shearing or normal normalization is used in the independent result.
    """
    height = 2.0**-20
    tri = ((offset, 3.0, 0.0), (offset + 1, 3.0, 1.0), (offset - 1, 3.0, 1.0))
    result = appendage_submergence._area_submerged(
        tri, lambda p: height - p[2] - (p[0] - offset) / 2
    )
    assert result == pytest.approx(4 * height**2 / 3, rel=2.0**-20, abs=0)


@pytest.mark.parametrize("offset", [0.0, 2.0**8])
def test_station_near_end_retains_analytic_section(offset):
    """Box L=8, B=4, H=2 cut at d=1/2: every interior section is B*d=2.

    A station 2**-20 from the end must retain that section, despite creating
    small side triangles. The explicit snap epsilon comes from the existing
    mesh_geometry._SNAP_REL policy (2026), as in compute_hydrostatics.
    """
    from digitalmodel.naval_architecture.mesh_geometry import _SNAP_REL

    vertices, faces = hydro.box_mesh(8, 4, 2)
    vertices[:, 0] += offset
    mesh = hydro.TriMesh(vertices, faces, units="m", axes=hydro.CANONICAL_AXES)
    clipped = hydro.clip_at_waterline(mesh, 0.5)
    index = SectionIndex(clipped, offset, _SNAP_REL * mesh.scale_diag)
    area, _ = index.section(offset - 4 + 2.0**-20)
    assert area == 2


@pytest.mark.parametrize("offset", [0.0, 2.0**8])
def test_near_vertex_extent_uses_intersections(offset):
    """The unit-area triangle cut at h=2**-20 spans x=o±h and has zero beam.

    Thus length=2h and beam=0, rather than the uncut length 2. Binary
    interpolation is exact at both translations.
    """
    height = 2.0**-20
    tri = ((offset, 0.0, 0.0), (offset + 1, 0.0, 1.0), (offset - 1, 0.0, 1.0))
    assert waterplane_extent([tri], height) == (2 * height, 0)
