"""Multi-region hull ingestion: hull + interpenetrating appendages (#2023).

WHAT THESE TESTS DEFEND
-----------------------
A rudder and a propeller boss arrive as their own closed bodies that overlap
the hull. Four things can go wrong, and every one of them meshes and solves:

  * MERGING them. Two interpenetrating closed bodies concatenated into one
    triangle soup are non-manifold where they cross. The ingestion lane exists
    to refuse that, so the appendages must come out as SEPARATE STLs, each
    closed on its own.
  * PLACING them independently. An appendage inferred on its own gets its own
    unit guess, its own forward axis and its own keel datum; the rudder ends up
    amidships, at the wrong size, and the case still runs.
  * SUMMING their wetted areas. Part of the appendage is inside the hull and
    part of the hull is inside the appendage; neither is wetted. The sum is an
    upper bound. It is also the denominator of every reported coefficient.
  * ASSUMING the centreplane cuts them. The bodies STRADDLE y = 0, and a body
    that is not cleanly cut there leaves snappyHexMesh unable to tell inside
    from outside -- it keeps the interior as fluid and the solve converges.

Every fixture here is a synthetic closed box built in pure Python, so the
suite never needs a CAD kernel or a client file. Two interpenetrating boxes
exercise the same code path a rudder does; a box's areas and volumes are known
in closed form, so the assertions are arithmetic and not a previous run.
"""

from __future__ import annotations

import json
import os
from pathlib import Path
from typing import Dict, List, Tuple

import pytest

from digitalmodel.naval_architecture.appendage_submergence import (
    Attitude,
    check_submergence,
    submergence_report,
)
from digitalmodel.naval_architecture.hull_ingest import (
    HullIngestError,
    HullTransform,
    NotWatertightError,
    ingest_triangles,
)
from digitalmodel.naval_architecture.solid_occlusion import (
    SolidIndex,
    classify_wetted_area,
)
from digitalmodel.naval_architecture.symmetry_cut import (
    check_symmetry_cut,
    plane_section,
)

Vec3 = Tuple[float, float, float]
Tri = Tuple[Vec3, Vec3, Vec3]


# --------------------------------------------------------------------------- #
#  Synthetic closed bodies
# --------------------------------------------------------------------------- #

def closed_box(
    x: Tuple[float, float], y: Tuple[float, float], z: Tuple[float, float]
) -> List[Tri]:
    """A watertight axis-aligned box with outward-consistent normals."""
    (x0, x1), (y0, y1), (z0, z1) = x, y, z

    def quad(a: Vec3, b: Vec3, c: Vec3, d: Vec3) -> List[Tri]:
        return [(a, b, c), (a, c, d)]

    p000, p100 = (x0, y0, z0), (x1, y0, z0)
    p110, p010 = (x1, y1, z0), (x0, y1, z0)
    p001, p101 = (x0, y0, z1), (x1, y0, z1)
    p111, p011 = (x1, y1, z1), (x0, y1, z1)
    tris: List[Tri] = []
    tris += quad(p000, p010, p110, p100)   # -z
    tris += quad(p001, p101, p111, p011)   # +z
    tris += quad(p000, p100, p101, p001)   # -y
    tris += quad(p010, p011, p111, p110)   # +y
    tris += quad(p000, p001, p011, p010)   # -x
    tris += quad(p100, p110, p111, p101)   # +x
    return tris


#: A barge, and an appendage that pierces its aft face while straddling the
#: centreplane -- the two properties a rudder has that a hull does not.
HULL = closed_box((0.0, 20.0), (-3.0, 3.0), (0.0, 4.0))
FIN = closed_box((-1.0, 1.0), (-0.5, 0.5), (1.0, 3.0))
DRAFT = 3.0


def ingest(tmp_path: Path, **kwargs):
    return ingest_triangles(
        HULL,
        tmp_path,
        units_in="m",
        forward="+x",
        draft_m=DRAFT,
        **kwargs,
    )


# --------------------------------------------------------------------------- #
#  Separate STLs, each closed
# --------------------------------------------------------------------------- #

def test_each_appendage_is_emitted_as_its_own_closed_stl(tmp_path: Path) -> None:
    """One file per region, and the closure recorded per region.

    Not one merged surface: the bodies interpenetrate, so their concatenation
    is not a manifold, and snappyHexMesh does not need it to be one -- it
    forms the union from per-surface inside/outside tests.
    """
    manifest = ingest(tmp_path, appendages={"fin": FIN, "boss": FIN})
    block = manifest.to_dict()["regions"]

    assert (tmp_path / "hull.stl").is_file()
    assert (tmp_path / "fin.stl").is_file()
    assert (tmp_path / "boss.stl").is_file()

    names = [r["name"] for r in block["regions"]]
    assert names[0] == "hull", "the hull must lead; its patch name is frozen"
    assert set(names) == {"hull", "fin", "boss"}
    for region in block["regions"]:
        assert region["watertight"] is True
        assert region["open_edge_count"] == 0
        assert region["nonmanifold_edge_count"] == 0
        assert region["n_triangles"] > 0


def test_a_hull_only_ingest_carries_no_regions_block(tmp_path: Path) -> None:
    """The single-surface manifest is unchanged, key for key.

    Asserted because every downstream consumer reads this file, and a new
    top-level key that appears unconditionally is a contract change dressed as
    a feature.
    """
    data = ingest(tmp_path).to_dict()
    assert "regions" not in data


def test_an_open_appendage_is_refused_by_name(tmp_path: Path) -> None:
    """A leaky appendage fails the SAME gate the hull does.

    snappy decides what is solid from a per-surface inside/outside test. On an
    open surface that test keeps the interior, so the rudder becomes a
    fluid-filled shell and the solve converges to a resistance that includes
    the drag of a cavity.
    """
    torn = FIN[:-2]
    with pytest.raises(NotWatertightError) as excinfo:
        ingest(tmp_path, appendages={"fin": torn})
    assert "fin" in str(excinfo.value)
    assert not (tmp_path / "fin.stl").exists()


def test_the_hull_cap_is_not_applied_to_the_appendages(tmp_path: Path) -> None:
    """Capping is a HULL step.

    The hull surface stops at deck level and its openings are lidded. An
    appendage is already closed, so a cap there would either do nothing or
    lid an opening that is not an opening.
    """
    open_top = [t for t in HULL if not _is_top(t, 4.0)]
    manifest = ingest_triangles(
        open_top,
        tmp_path,
        units_in="m",
        forward="+x",
        draft_m=DRAFT,
        cap_open_boundaries=True,
        appendages={"fin": FIN},
    )
    data = manifest.to_dict()
    assert data["provenance"]["capped"] is True
    fin = _region(data, "fin")
    # The cap added triangles to the hull only: the fin is the box it was.
    assert fin["n_triangles"] == len(FIN)


def _is_top(tri: Tri, z: float) -> bool:
    return all(abs(p[2] - z) < 1e-12 for p in tri)


# --------------------------------------------------------------------------- #
#  Placement
# --------------------------------------------------------------------------- #

def test_appendages_are_placed_by_the_hulls_transform_not_their_own(
    tmp_path: Path,
) -> None:
    """The rudder moves with the hull, and by the hull's decision.

    The hull here is ingested from MILLIMETRES and translated onto the aft
    perpendicular and the keel. If the appendage inferred its own units it
    would be 1000x too large; if it found its own origin its keel would sit on
    z = 0 instead of 1 m above the hull's.
    """
    mm = lambda tris: [  # noqa: E731
        tuple(tuple(c * 1000.0 for c in p) for p in tri) for tri in tris
    ]
    manifest = ingest_triangles(
        mm(HULL),
        tmp_path,
        units_in="mm",
        forward="+x",
        draft_m=DRAFT,
        appendages={"fin": mm(FIN)},
    )
    fin = _region(manifest.to_dict(), "fin")
    assert fin["bbox_min_m"] == pytest.approx([-1.0, -0.5, 1.0])
    assert fin["bbox_max_m"] == pytest.approx([1.0, 0.5, 3.0])


def test_the_transform_is_recorded_and_reproduces_the_placement() -> None:
    """``HullTransform`` is the placement, not a description of it."""
    transform = HullTransform(
        scale_to_m=0.001, forward="+x", offset=(-2.0, 0.0, 1.0)
    )
    placed = transform.apply(
        [((1000.0, 0.0, 0.0), (2000.0, 0.0, 0.0), (1000.0, 1000.0, 0.0))]
    )
    assert placed[0][0] == pytest.approx((-1.0, 0.0, 1.0))
    assert placed[0][1] == pytest.approx((0.0, 0.0, 1.0))
    assert transform.to_dict()["offset_m"] == [-2.0, 0.0, 1.0]


def test_an_appendage_that_reuses_the_hull_name_is_refused(tmp_path: Path) -> None:
    """Two regions writing one STL means the second silently wins."""
    with pytest.raises(ValueError, match="collides"):
        ingest(tmp_path, appendages={"hull": FIN})


# --------------------------------------------------------------------------- #
#  The wetted-area double count
# --------------------------------------------------------------------------- #

def test_the_union_area_is_reported_below_the_naive_sum(tmp_path: Path) -> None:
    """The sum double-counts, and the manifest says so in numbers.

    The fin pierces the hull's aft face at x = 0. Below the waterline:

      * the half of the fin at x > 0 is INSIDE the hull -> not wetted, and
      * the patch of the hull's x = 0 face inside the fin -> not wetted.

    Both are computable in closed form for boxes, which is why boxes are the
    fixture. The union's external area must land below the sum by exactly
    those two patches.
    """
    data = ingest(tmp_path, appendages={"fin": FIN}).to_dict()
    union = data["regions"]["union"]

    # Fin: a 2 x 1 x 2 box whose top sits exactly at the waterline, so all six
    # faces are wet; the x > 0 half of it is inside the hull.
    fin = _region(data, "fin")
    assert fin["wetted_area_m2"] == pytest.approx(fin_wetted_area(), rel=1e-9)
    assert fin["wetted_area_occluded_m2"] == pytest.approx(
        fin_occluded_area(), abs=1e-6
    )
    # Hull: the 1 x 2 window of its aft face that the fin encloses. The hull's
    # aft face is TWO triangles spanning 6 x 4 m, so the window is resolved by
    # subdivision and the answer is an estimate -- which is why the module
    # publishes an error bar. The estimate must lie inside it.
    hull = _region(data, "hull")
    assert hull["wetted_area_occluded_m2"] == pytest.approx(
        2.0, abs=hull["wetted_area_undecided_m2"]
    )

    assert union["wetted_surface_naive_sum_m2"] > union[
        "wetted_surface_external_m2"
    ]
    assert union["double_counted_m2"] == pytest.approx(
        fin_occluded_area() + 2.0,
        abs=union["wetted_surface_external_uncertainty_m2"],
    )
    assert "UPPER BOUND" in union["accounting"]


def test_the_error_bar_shrinks_and_the_estimate_converges(tmp_path: Path) -> None:
    """The uncertainty is a real bound, not decoration.

    The hull's aft face is two big triangles hiding a 2 m2 window. Deeper
    subdivision has to walk the estimate toward the closed-form answer AND
    shrink the band it is quoted with; if it did not, the band would be
    telling the reader nothing.
    """
    errors = []
    bands = []
    for depth in (1, 3, 5):
        data = ingest_triangles(
            HULL,
            tmp_path / f"d{depth}",
            units_in="m",
            forward="+x",
            draft_m=DRAFT,
            appendages={"fin": FIN},
            subdivision_depth=depth,
        ).to_dict()
        hull = _region(data, "hull")
        errors.append(abs(hull["wetted_area_occluded_m2"] - 2.0))
        bands.append(hull["wetted_area_undecided_m2"])
        assert errors[-1] <= bands[-1] + 1e-9, f"depth {depth} escaped its band"

    assert bands[0] > bands[1] > bands[2]
    assert errors[-1] < errors[0]


def fin_wetted_area() -> float:
    """All six faces of a 2 (x) by 1 (y) by 2 (z) box, all at or below z = 3."""
    return 2 * (1.0 * 2.0) + 2 * (2.0 * 2.0) + 2 * (2.0 * 1.0)


def fin_occluded_area() -> float:
    """The half at x > 0, inside the hull: the +x end, and half of the four
    faces that span x."""
    return (1.0 * 2.0) + 2 * (1.0 * 2.0) + 2 * (1.0 * 1.0)


def test_the_reported_union_area_is_never_the_sum(tmp_path: Path) -> None:
    """A regression guard on the one shortcut that would be invisible.

    Aref is a denominator. Summing inflates it, every coefficient comes back
    low by exactly that inflation, and the case converges perfectly.
    """
    data = ingest(tmp_path, appendages={"fin": FIN}).to_dict()
    union = data["regions"]["union"]
    per_region = [r["wetted_area_m2"] for r in data["regions"]["regions"]]
    assert union["wetted_surface_naive_sum_m2"] == pytest.approx(sum(per_region))
    assert union["wetted_surface_external_m2"] < sum(per_region)


def test_disjoint_bodies_are_not_double_counted_either(tmp_path: Path) -> None:
    """Nothing is subtracted when nothing overlaps.

    The correction must be a measurement, not a fudge factor: an appendage
    that touches nothing has to come through at its full area.
    """
    far = closed_box((5.0, 6.0), (-0.5, 0.5), (0.5, 1.5))
    inside_hull = ingest(tmp_path, appendages={"pod": far}).to_dict()
    pod = _region(inside_hull, "pod")
    # Fully inside the hull, so entirely occluded -- the honest answer.
    assert pod["wetted_area_external_m2"] == pytest.approx(0.0, abs=1e-6)

    separate = closed_box((30.0, 31.0), (-0.5, 0.5), (0.5, 1.5))
    apart = ingest_triangles(
        HULL,
        tmp_path / "apart",
        units_in="m",
        forward="+x",
        draft_m=DRAFT,
        appendages={"pod": separate},
    ).to_dict()
    pod = _region(apart, "pod")
    assert pod["wetted_area_occluded_m2"] == pytest.approx(0.0, abs=1e-9)
    assert pod["wetted_area_external_m2"] == pytest.approx(
        pod["wetted_area_m2"], rel=1e-9
    )


def test_containment_is_a_parity_test_that_survives_a_grazing_ray() -> None:
    """The point-in-solid test is the primitive everything else rests on.

    A ray that grazes an edge is counted once, twice or not at all, and the
    parity answer flips. Points are placed ON the grid lines of the box's own
    vertices to hit that case deliberately.
    """
    index = SolidIndex(closed_box((0.0, 2.0), (0.0, 2.0), (0.0, 2.0)))
    assert index.contains((1.0, 1.0, 1.0))
    assert index.contains((0.5, 0.0 + 1e-9, 1.0)) in (True, False)  # boundary
    assert not index.contains((3.0, 1.0, 1.0))
    assert not index.contains((-1.0, 1.0, 1.0))
    assert not index.contains((1.0, 1.0, 5.0))
    # A ray fired exactly along a face plane: y = 2 is the box's own face.
    assert not index.contains((1.0, 3.0, 2.0))


def test_classify_falls_back_to_the_plain_area_with_no_other_regions() -> None:
    """The single-region path must cost nothing and change nothing."""
    box = closed_box((0.0, 1.0), (0.0, 1.0), (0.0, 1.0))
    split = classify_wetted_area(box, 1.0, [])
    assert split.total_m2 == pytest.approx(6.0)
    assert split.external_m2 == pytest.approx(6.0)
    assert split.occluded_m2 == 0.0
    assert split.undecided_m2 == 0.0


# --------------------------------------------------------------------------- #
#  The centreplane cut
# --------------------------------------------------------------------------- #

def test_a_straddling_appendage_is_cut_cleanly(tmp_path: Path) -> None:
    """The half-domain premise, measured rather than assumed.

    The fin spans y = -0.5 .. +0.5 and the domain stops at y = 0. The half the
    domain keeps has to be a well-defined solid lidded by the plane; if it were
    not, snappy could not tell its inside from its outside and would keep the
    interior as fluid.
    """
    data = ingest(tmp_path, appendages={"fin": FIN}).to_dict()
    for region in data["regions"]["regions"]:
        section = region["centreplane_section"]
        assert section["straddles"] is True, region["name"]
        assert section["cut_is_clean"] is True, region["name"]
        assert section["section_area_m2"] > 0.0
        assert section["volume_closure_error"] < 1e-9, region["name"]
        # Both fixtures are symmetric about y = 0, so the halves must match.
        assert section["volume_symmetry_error"] < 1e-9, region["name"]


def test_the_section_area_and_half_volumes_are_arithmetic() -> None:
    """Checked against closed form: a 2 x 2 x 4 box straddling y = 0."""
    section = plane_section(closed_box((0.0, 2.0), (-1.0, 1.0), (0.0, 4.0)), 0.0)
    assert section.straddles is True
    assert section.clean is True
    assert section.total_volume_m3 == pytest.approx(16.0)
    assert section.kept.volume_m3 == pytest.approx(8.0)
    assert section.discarded.volume_m3 == pytest.approx(8.0)
    # The lid is the 2 x 4 buttock section.
    assert section.kept.lid_area_m2 == pytest.approx(8.0)


def test_the_kept_half_is_the_side_the_domain_keeps() -> None:
    """Which side is kept is not decidable on a symmetric body.

    Every fixture in this suite is symmetric about y = 0, so an inverted clip
    would return the same two volumes and no assertion above would notice. An
    ASYMMETRIC box makes the two halves different numbers, and the domain runs
    from y_side to y = 0, so the kept half is the one at NEGATIVE y.
    """
    lopsided = closed_box((0.0, 1.0), (-3.0, 1.0), (0.0, 1.0))
    section = plane_section(lopsided, 0.0)
    assert section.total_volume_m3 == pytest.approx(4.0)
    assert section.kept.volume_m3 == pytest.approx(3.0)
    assert section.discarded.volume_m3 == pytest.approx(1.0)
    assert section.symmetry_error == pytest.approx(0.5)
    assert section.clean is True


def test_the_plane_being_TANGENT_to_the_surface_is_not_a_defect() -> None:
    """A symmetric foil's leading and trailing edges lie ON the centreplane.

    The plane touches the surface there without crossing it, so an
    intersection-curve test finds loose ends on geometry that is perfectly
    sound -- 6 of them on the real 32,842-triangle client hull, which has zero
    open and zero non-manifold edges. The volume test must be indifferent to
    tangency, because tangency encloses no volume.
    """
    prism = [
        # A wedge whose apex edge lies exactly on y = 0, tangent from below.
        ((0.0, 0.0, 1.0), (1.0, 0.0, 1.0), (1.0, -1.0, 0.0)),
        ((0.0, 0.0, 1.0), (1.0, -1.0, 0.0), (0.0, -1.0, 0.0)),
        ((0.0, 0.0, 1.0), (0.0, -1.0, 0.0), (0.0, -1.0, 1.0)),
        ((0.0, 0.0, 1.0), (0.0, -1.0, 1.0), (1.0, 0.0, 1.0)),
        ((1.0, 0.0, 1.0), (0.0, -1.0, 1.0), (1.0, -1.0, 1.0)),
        ((1.0, 0.0, 1.0), (1.0, -1.0, 1.0), (1.0, -1.0, 0.0)),
        ((0.0, -1.0, 0.0), (1.0, -1.0, 0.0), (1.0, -1.0, 1.0)),
        ((0.0, -1.0, 0.0), (1.0, -1.0, 1.0), (0.0, -1.0, 1.0)),
    ]
    section = plane_section(prism, 0.0)
    assert section.straddles is False, "it touches the plane, it does not cross"
    assert section.clean is True
    assert section.discarded.volume_m3 == pytest.approx(0.0, abs=1e-12)
    assert section.kept.volume_m3 == pytest.approx(section.total_volume_m3)


def test_a_hole_at_the_centreplane_breaks_the_volume_closure() -> None:
    """The check has to FAIL on the geometry it exists to catch.

    If this passed, the whole verification would be a tautology. The three
    divergence-theorem volumes weight a missing patch differently, so a hole
    makes them disagree -- which is the signal.
    """
    torn = [t for t in closed_box((0.0, 2.0), (-1.0, 1.0), (0.0, 4.0))
            if not _touches(t, 0.0)]
    section = plane_section(torn, 0.0)
    assert section.clean is False
    assert section.kept.consistency > 1e-6


def _touches(tri: Tri, y: float) -> bool:
    return min(p[1] for p in tri) <= y <= max(p[1] for p in tri)


def test_the_keep_point_must_be_outside_every_region() -> None:
    """The other half of the premise.

    snappyHexMesh keeps the region reachable from ``locationInMesh``. Inside
    a rudder, it keeps the rudder's interior and deletes the water.
    """
    regions = [("hull", HULL), ("fin", FIN)]
    good = check_symmetry_cut(regions, location_in_mesh=(-50.0, -10.0, 1.0))
    assert good.location_outside_all is True
    assert good.ok is True
    assert good.failures() == []

    bad = check_symmetry_cut(regions, location_in_mesh=(0.0, -0.25, 2.0))
    assert bad.location_outside_all is False
    assert set(bad.location_inside) == {"hull", "fin"}
    assert bad.ok is False
    assert any("locationInMesh" in line for line in bad.failures())


# --------------------------------------------------------------------------- #
#  Submergence across an attitude matrix
# --------------------------------------------------------------------------- #

def test_trim_decides_submergence_not_the_mean_draft() -> None:
    """The clearance is LOCAL, over the appendage's own station.

    Both attitudes below have the same mean draft. One trims bow-down and
    lifts the stern; that is the one that exposes a stern appendage, and a
    check written against the mean draft would report both as submerged.
    """
    stern_fin = closed_box((1.0, 3.0), (-0.5, 0.5), (0.0, 5.0))
    even = Attitude("even_keel", 6.0, 6.0, reference_length_m=20.0)
    bow_down = Attitude("bow_down", 9.0, 3.0, reference_length_m=20.0)
    assert even.draft_mean_m == bow_down.draft_mean_m

    results = {
        r.attitude: r
        for r in check_submergence([("fin", stern_fin)], [even, bow_down])
    }
    assert results["even_keel"].fully_submerged is True
    assert results["bow_down"].fully_submerged is False
    assert results["bow_down"].min_clearance_m < 0.0
    assert results["bow_down"].exposed_area_m2 > 0.0
    assert results["even_keel"].exposed_area_m2 == pytest.approx(0.0, abs=1e-9)


def test_the_report_names_every_exposure_rather_than_returning_a_flag() -> None:
    """A boolean that says "one of eleven failed" is not actionable."""
    stern_fin = closed_box((1.0, 3.0), (-0.5, 0.5), (0.0, 5.0))
    attitudes = [
        Attitude("deep", 8.0, 8.0, reference_length_m=20.0),
        Attitude("shallow", 4.0, 2.0, reference_length_m=20.0),
    ]
    report = submergence_report(check_submergence([("fin", stern_fin)], attitudes))
    assert report["all_fully_submerged"] is False
    assert len(report["exposures"]) == 1
    assert "shallow" in report["exposures"][0]
    assert "fin" in report["exposures"][0]


def test_the_exposed_area_is_measured_on_the_tilted_plane() -> None:
    """Clipping against a tilted plane, not a sheared z-threshold.

    A vertical fin whose top half is out of the water at even keel has a known
    exposed area; shearing the triangles to reuse a horizontal clipper would
    change it by the shear factor.
    """
    fin = closed_box((0.0, 2.0), (-0.5, 0.5), (0.0, 4.0))
    even = Attitude("half_out", 2.0, 2.0, reference_length_m=20.0)
    (result,) = check_submergence([("fin", fin)], [even])
    # Above z = 2: four side strips of height 2 plus the 2 x 1 top face.
    expected = 2 * (2.0 * 2.0) + 2 * (1.0 * 2.0) + (2.0 * 1.0)
    assert result.exposed_area_m2 == pytest.approx(expected, rel=1e-9)


def test_the_water_plane_interpolates_between_the_end_drafts() -> None:
    attitude = Attitude("trimmed", 10.0, 6.0, reference_length_m=100.0)
    assert attitude.waterline_z(0.0) == pytest.approx(6.0)
    assert attitude.waterline_z(100.0) == pytest.approx(10.0)
    assert attitude.waterline_z(50.0) == pytest.approx(8.0)
    assert attitude.trim_m == pytest.approx(4.0)
    assert attitude.draft_mean_m == pytest.approx(8.0)


# --------------------------------------------------------------------------- #
#  Manifest shape
# --------------------------------------------------------------------------- #

def test_the_regions_block_is_valid_json_on_disk(tmp_path: Path) -> None:
    ingest(tmp_path, appendages={"fin": FIN})
    data = json.loads((tmp_path / "hull_manifest.json").read_text())
    assert data["regions"]["n_regions"] == 2
    assert data["regions"]["waterline_z_m"] == pytest.approx(DRAFT)
    assert data["provenance"]["transform"]["forward_axis_in"] == "+x"
    for region in data["regions"]["regions"]:
        assert region["stl_file"].endswith(".stl")
        assert (tmp_path / region["stl_file"]).is_file()


def test_an_appendage_beyond_the_hull_box_is_flagged(tmp_path: Path) -> None:
    """The refinement boxes are derived from the HULL bounding box.

    Something reaching past it is meshed at background resolution where it
    protrudes. The mesher does not complain and the shortfall shows up only as
    a force that is a little wrong for no visible reason.
    """
    tall = closed_box((1.0, 3.0), (-0.5, 0.5), (1.0, 9.0))
    data = ingest(tmp_path, appendages={"mast": tall}).to_dict()
    assert _region(data, "mast")["outside_hull_bbox"] == ["z+"]
    assert any("outside the hull bounding box" in n.lower()
               for n in data["regions"]["union"]["notes"])


def test_an_empty_appendage_is_refused(tmp_path: Path) -> None:
    with pytest.raises(HullIngestError, match="no triangles"):
        ingest(tmp_path, appendages={"fin": []})


def _region(data: Dict[str, object], name: str) -> Dict[str, object]:
    for region in data["regions"]["regions"]:  # type: ignore[index]
        if region["name"] == name:
            return region
    raise AssertionError(f"no region named {name!r}")


# --------------------------------------------------------------------------- #
#  The real file -- opt-in, skipped unless a path is supplied out of band
# --------------------------------------------------------------------------- #
#
# Same gate as ``test_brep_tessellate.py``: client geometry never enters the
# repository, so the committed suite runs on synthetic boxes and this one is
# reached only when someone points it at a real model.

_HULL_3DM = os.environ.get("DIGITALMODEL_3DM_HULL")
_APPENDAGE_LAYERS = os.environ.get("DIGITALMODEL_3DM_APPENDAGE_LAYERS", "")


@pytest.mark.skipif(
    not (_HULL_3DM and _APPENDAGE_LAYERS),
    reason=(
        "set DIGITALMODEL_3DM_HULL and DIGITALMODEL_3DM_APPENDAGE_LAYERS "
        "(name=Layer,name=Layer) to run the multi-region acceptance gate"
    ),
)
def test_real_3dm_appendages_ingest_as_separate_closed_regions(
    tmp_path: Path,
) -> None:
    """The acceptance gate, on a real CAD file with real appendages.

    Asserts the properties that make the design valid rather than any
    particular number: each region closed on its own, the merged soup NOT
    closed (which is why they stay separate), the union area strictly below
    the naive sum, and every region cut cleanly by the centreplane.
    """
    from digitalmodel.naval_architecture.hull_ingest import ingest_3dm
    from digitalmodel.naval_architecture.kcs_geometry import check_surface

    layers = dict(
        pair.split("=", 1) for pair in _APPENDAGE_LAYERS.split(",") if pair
    )
    manifest = ingest_3dm(
        Path(_HULL_3DM),
        tmp_path,
        layers=os.environ.get("DIGITALMODEL_3DM_LAYERS", "Hull").split(","),
        appendage_layers={n: [layer] for n, layer in layers.items()},
        units_in=os.environ.get("DIGITALMODEL_3DM_UNITS") or None,
        draft_m=float(os.environ.get("DIGITALMODEL_3DM_DRAFT", "10.4")),
        cap_open_boundaries=True,
        tessellate_breps=True,
    )
    block = manifest.to_dict()["regions"]
    assert block["n_regions"] == 1 + len(layers)

    soup: List[Tri] = []
    for region in block["regions"]:
        assert region["watertight"] is True, region["name"]
        assert region["centreplane_section"]["cut_is_clean"] is True
        stl = tmp_path / region["stl_file"]
        assert stl.is_file()
        soup.extend(_read_stl(stl))

    # The whole reason they are separate files: merged, they are not a surface.
    assert check_surface(soup).nonmanifold_edge_count > 0
    assert block["union"]["merged_nonmanifold_edge_count"] > 0

    union = block["union"]
    assert union["wetted_surface_external_m2"] < union[
        "wetted_surface_naive_sum_m2"
    ]
    assert union["double_counted_m2"] > 0.0
    assert union["wetted_surface_external_uncertainty_m2"] >= 0.0


def _read_stl(path: Path) -> List[Tri]:
    verts: List[Vec3] = []
    tris: List[Tri] = []
    for line in path.read_text().splitlines():
        line = line.strip()
        if line.startswith("vertex "):
            _, x, y, z = line.split()
            verts.append((float(x), float(y), float(z)))
            if len(verts) == 3:
                tris.append((verts[0], verts[1], verts[2]))
                verts = []
    return tris
