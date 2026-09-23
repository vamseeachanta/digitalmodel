"""An open wetted surface must be refused, not silently accepted.

The module's stated precondition is a wetted surface that closes at the free
surface. The divergence-theorem argument it rests on requires that: with a hole
below the waterline the three axis integrals are no longer the enclosed volume,
and they stop agreeing with each other only when the body happens to sit off
the origin.

`boundary_edges` was computed, stored and reported, and read by nothing. A unit
box with its x = 0 face removed reported `axis_volumes (1.0, 1.0, 1.0)`,
`consistent True`, `outward True` -- indistinguishable from the closed box --
because a face lying in a plane through the origin contributes nothing to any
of the three integrals. That is not a corner case here: symmetry-reduced meshes
are cut on exactly those planes.

Which is also why a boundary edge cannot simply be forbidden. The rule is that
every boundary edge must lie either at the waterline or in a declared symmetry
plane. Anything else is a hole in the wetted surface.
"""

from __future__ import annotations

import numpy as np
import pytest

from digitalmodel.hydrodynamics.diffraction import mesh_orientation as mo
from digitalmodel.hydrodynamics.diffraction.mesh_orientation import (
    UnreliableOrientation,
    repair_gdf_text,
)

V = {
    "a": (0.0, -0.5, -1.0), "b": (1.0, -0.5, -1.0),
    "c": (1.0, 0.5, -1.0), "d": (0.0, 0.5, -1.0),
    "e": (0.0, -0.5, 0.0), "f": (1.0, -0.5, 0.0),
    "g": (1.0, 0.5, 0.0), "h": (0.0, 0.5, 0.0),
}
FACES = {
    "bottom": ["a", "d", "c", "b"],
    "xmax": ["b", "c", "g", "f"],
    "xmin": ["a", "e", "h", "d"],
    "ymin": ["a", "b", "f", "e"],
    "ymax": ["d", "h", "g", "c"],
}


def coords(faces, dx=0.0, inward=False):
    out = []
    for quad in faces.values():
        pts = [(V[n][0] + dx, V[n][1], V[n][2]) for n in quad]
        out.append(list(reversed(pts)) if inward else pts)
    return np.asarray(out, dtype=float)


def gdf(faces, dx=0.0, isx=0, isy=0):
    lines = ["probe", "1.0 9.80665", f"{isx} {isy}", str(len(faces))]
    for quad in faces.values():
        for n in quad:
            x, y, z = V[n]
            lines.append(f"{x + dx:.6f} {y:.6f} {z:.6f}")
    return "\n".join(lines) + "\n"


def report(faces, **kw):
    return mo._report_from_coords(coords(faces, **kw))


class TestClosedMeshesAreUnaffected:
    def test_closed_box_at_the_origin(self):
        r = report(FACES)
        assert r.submerged_boundary_edges == 0
        assert r.ok is True

    def test_closed_box_offset_from_the_origin(self):
        r = report(FACES, dx=-10.0)
        assert r.submerged_boundary_edges == 0
        assert r.ok is True

    def test_the_waterline_edges_are_still_counted_as_boundary(self):
        """The mesh is legitimately open at z = 0; that is not a hole."""
        r = report(FACES)
        assert r.boundary_edges == 4
        assert r.submerged_boundary_edges == 0


class TestOpenMeshes:
    @pytest.mark.parametrize("dropped", ["xmin", "xmax", "bottom", "ymin"])
    def test_a_missing_face_is_detected_wherever_the_body_sits(self, dropped):
        """The origin-plane case is the one the axis volumes cannot see."""
        faces = {k: v for k, v in FACES.items() if k != dropped}
        for dx in (0.0, -10.0):
            r = report(faces, dx=dx)
            assert r.submerged_boundary_edges > 0, (
                f"dropping {dropped} at dx={dx} left no submerged boundary")
            assert r.ok is False

    def test_the_origin_plane_hole_is_invisible_to_the_axis_volumes(self):
        """Guards the test above against passing for the wrong reason.

        If the axis volumes caught this, the new check would be redundant. They
        do not: removing the face in the plane x = 0 leaves all three integrals
        at the closed value.
        """
        faces = {k: v for k, v in FACES.items() if k != "xmin"}
        r = report(faces)
        assert r.axis_volumes == pytest.approx((1.0, 1.0, 1.0))
        assert r.consistent is True
        assert r.outward is True
        # Only the boundary check separates it from a closed mesh.
        assert r.submerged_boundary_edges > 0
        assert r.ok is False

    def test_strict_repair_refuses_an_open_mesh(self):
        faces = {k: v for k, v in FACES.items() if k != "xmin"}
        with pytest.raises(UnreliableOrientation) as exc:
            repair_gdf_text(gdf(faces), strict=True)
        assert "boundary" in str(exc.value).lower()

    def test_an_inward_open_mesh_is_refused_rather_than_repaired(self):
        """The dangerous direction: bad input must not be silently accepted."""
        faces = {k: v for k, v in FACES.items() if k != "xmin"}
        text = gdf({k: list(reversed(v)) for k, v in faces.items()})
        with pytest.raises(UnreliableOrientation):
            repair_gdf_text(text, strict=True)


class TestSymmetryReducedMeshes:
    """A declared symmetry cut is a legitimate boundary, not a hole."""

    def test_a_half_model_cut_on_x_is_accepted_when_declared(self):
        # Keep the half at x >= 0 by dropping the face at x = 0 and declaring
        # symmetry about that plane.
        faces = {k: v for k, v in FACES.items() if k != "xmin"}
        out, flipped = repair_gdf_text(gdf(faces, isx=1), strict=True)
        assert out == gdf(faces, isx=1)
        assert flipped == ()

    def test_the_same_mesh_without_the_declaration_is_refused(self):
        faces = {k: v for k, v in FACES.items() if k != "xmin"}
        with pytest.raises(UnreliableOrientation):
            repair_gdf_text(gdf(faces, isx=0), strict=True)

    def test_a_hole_away_from_the_declared_plane_is_still_refused(self):
        """Declaring symmetry must not excuse an unrelated hole."""
        faces = {k: v for k, v in FACES.items() if k != "bottom"}
        with pytest.raises(UnreliableOrientation):
            repair_gdf_text(gdf(faces, isx=1), strict=True)


class TestCommittedFixturesStillPass:
    """The twelve meshes this branch repaired must not start failing."""

    def test_the_repaired_unit_box_is_closed(self):
        from pathlib import Path

        repo = Path(__file__).resolve().parents[3]
        box = (repo / "examples" / "hydrodynamics" / "diffraction"
               / "unit_box_rao" / "unit_box.gdf")
        if not box.exists():
            pytest.skip("committed unit box absent")
        out, flipped = repair_gdf_text(box.read_text(), strict=True)
        assert flipped == ()
