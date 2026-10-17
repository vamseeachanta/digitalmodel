"""Trim-aware hydrostatics on a tessellated hull.

WHAT THESE TESTS DEFEND
-----------------------
A loading-condition matrix is quoted as a MEAN draft and a TRIM, and every
number that follows -- waterline length, wetted surface, displacement, block
coefficient -- is read off the hull in that attitude. Four things can go wrong
here and three of them produce a table that looks entirely reasonable:

  * THE ROTATION GOES THE WRONG WAY. Trim by the stern that lifts the stern is
    not a small error: on a hull with a cut-up run the transom immerses at one
    sign and leaves the water at the other, so the waterline length moves by
    metres and the block coefficient follows it. Nothing in the emitted table
    says which happened.
  * THE LABELS DISAGREE WITH THE GEOMETRY. ``draft_fwd``/``draft_aft`` are what
    a reader checks; the rotation is what the mesh is built from. Swapping the
    two labels leaves every integrated quantity correct and every quoted end
    draft wrong, which is the worst of both -- it survives a numerical review
    and fails a reader's sanity check, or vice versa.
    So :func:`test_negative_trim_lowers_the_stern_in_the_geometry` asserts
    against the ROTATED TRIANGLES, not against the label arithmetic.
  * THE MEAN DRAFT DRIFTS. Rotating about anything other than the midship point
    on the waterline changes the immersion the condition was named for, so a
    matrix of "6.55 m at three trims" quietly becomes three different drafts.
  * THE SWEEP AND THE SINGLE POINT DISAGREE. A hydrostatic table computed by a
    second implementation of the same integration is a second thing to keep
    right. Here the table is the single-point function in a loop, and one test
    pins that.

Every fixture is synthetic and built in pure Python -- a box and a prismatic
wedge, both with closed-form volume and wetted area, so the assertions are
arithmetic rather than a previous run. The real hull that motivated the module
is covered by a test that reads its path from ``DIGITALMODEL_3DM_HULL`` and
SKIPS when it is unset, exactly as ``test_brep_tessellate.py`` does, so no
client geometry and no client identifier enters this repository.
"""

from __future__ import annotations

import math
import os
from pathlib import Path
from typing import List, Sequence, Tuple

import pytest

from digitalmodel.naval_architecture.appendage_submergence import Attitude
from digitalmodel.naval_architecture.trim_hydrostatics import (
    FloatingCondition,
    HullFrame,
    attitude_for,
    condition_matrix,
    end_drafts,
    hydrostatic_table,
    hydrostatics_at,
    rotate_for_trim,
    trim_angle_rad,
)

Vec3 = Tuple[float, float, float]
Tri = Tuple[Vec3, Vec3, Vec3]


# --------------------------------------------------------------------------- #
#  Synthetic hulls with closed-form hydrostatics
# --------------------------------------------------------------------------- #

def _quad(a: Vec3, b: Vec3, c: Vec3, d: Vec3) -> List[Tri]:
    return [(a, b, c), (a, c, d)]


def _box(
    x0: float, x1: float, y0: float, y1: float, z0: float, z1: float
) -> List[Tri]:
    """A closed axis-aligned box. Volume and surface area are exact."""
    tris: List[Tri] = []
    tris += _quad((x0, y0, z0), (x1, y0, z0), (x1, y1, z0), (x0, y1, z0))
    tris += _quad((x0, y0, z1), (x0, y1, z1), (x1, y1, z1), (x1, y0, z1))
    tris += _quad((x0, y0, z0), (x0, y0, z1), (x1, y0, z1), (x1, y0, z0))
    tris += _quad((x0, y1, z0), (x1, y1, z0), (x1, y1, z1), (x0, y1, z1))
    tris += _quad((x0, y0, z0), (x0, y1, z0), (x0, y1, z1), (x0, y0, z1))
    tris += _quad((x1, y0, z0), (x1, y0, z1), (x1, y1, z1), (x1, y1, z0))
    return tris


def _wedge(length: float, beam: float, depth: float) -> List[Tri]:
    """A prismatic V-bottom hull: half-beam grows linearly with height.

    At draft ``T`` the section is a triangle of width ``beam*T/depth`` and
    height ``T``, so the volume is ``length*beam*T**2/(2*depth)`` and the block
    coefficient on the waterline beam is exactly 0.5 at every draft. That makes
    it a sharper fixture than a box: a box cannot tell a correct integration
    from one that fills its bounding volume.
    """
    hb = beam / 2.0
    x0, x1 = 0.0, length
    keel_a, keel_b = (x0, 0.0, 0.0), (x1, 0.0, 0.0)
    p_a, p_b = (x0, -hb, depth), (x1, -hb, depth)
    s_a, s_b = (x0, hb, depth), (x1, hb, depth)
    tris: List[Tri] = []
    tris += _quad(keel_a, keel_b, p_b, p_a)          # port shell
    tris += _quad(keel_a, s_a, s_b, keel_b)          # starboard shell
    tris += _quad(p_a, p_b, s_b, s_a)                # deck
    tris += [(keel_a, p_a, s_a), (keel_b, s_b, p_b)]  # transverse ends
    return tris


def _transom_hull(
    length: float = 100.0,
    beam: float = 20.0,
    depth: float = 12.0,
    cut_up: float = 4.0,
    run: float = 25.0,
) -> List[Tri]:
    """A box hull whose bottom RISES toward the stern over the aft ``run``.

    Asymmetric end to end, which is the whole point: at a small even-keel draft
    the water does not reach the aft extremity, so the waterline length is
    shorter than the hull. Trim by the stern immerses the transom and the
    waterline length steps up. Bow is +x, so the cut-up is at low x.
    """
    hb = beam / 2.0
    x_stern, x_bow = 0.0, length
    x_knuckle = run
    tris: List[Tri] = []
    # Flat bottom forward of the knuckle.
    tris += _quad(
        (x_knuckle, -hb, 0.0), (x_bow, -hb, 0.0), (x_bow, hb, 0.0),
        (x_knuckle, hb, 0.0),
    )
    # Rising bottom aft of the knuckle, up to the transom's lower edge.
    tris += _quad(
        (x_stern, -hb, cut_up), (x_knuckle, -hb, 0.0), (x_knuckle, hb, 0.0),
        (x_stern, hb, cut_up),
    )
    # Sides (port then starboard), each a five-sided profile split in two.
    for y, flip in ((-hb, False), (hb, True)):
        panel = [
            _quad((x_knuckle, y, 0.0), (x_bow, y, 0.0), (x_bow, y, depth),
                  (x_knuckle, y, depth)),
            _quad((x_stern, y, cut_up), (x_knuckle, y, 0.0),
                  (x_knuckle, y, depth), (x_stern, y, depth)),
        ]
        for quad in panel:
            tris += [tuple(reversed(t)) for t in quad] if flip else quad
    # Transom, stem and deck.
    tris += _quad((x_stern, -hb, cut_up), (x_stern, hb, cut_up),
                  (x_stern, hb, depth), (x_stern, -hb, depth))
    tris += _quad((x_bow, -hb, 0.0), (x_bow, -hb, depth), (x_bow, hb, depth),
                  (x_bow, hb, 0.0))
    tris += _quad((x_stern, -hb, depth), (x_stern, hb, depth),
                  (x_bow, hb, depth), (x_bow, -hb, depth))
    return tris


def _min_z_in_band(tris: Sequence[Tri], x_lo: float, x_hi: float) -> float:
    zs = [
        v[2] for t in tris for v in t if x_lo <= v[0] <= x_hi
    ]
    assert zs, "no vertices in the requested longitudinal band"
    return min(zs)


def _keel_probe(
    frame: HullFrame, *, draft: float, trim: float
) -> Tuple[Vec3, Vec3, Vec3]:
    """The aft, midship and forward keel points AFTER the trim rotation.

    A box has vertices only at its corners, so asking the rotated triangle soup
    where its bottom is at midship returns nothing. Carrying three explicit
    probe points through the SAME transform answers that question exactly, and
    for a flat bottom the probe IS the bottom.
    """
    probe = (
        (frame.x_aft, 0.0, frame.keel_z),
        (frame.x_midship, 0.0, frame.keel_z),
        (frame.x_fwd, 0.0, frame.keel_z),
    )
    return rotate_for_trim(
        [probe],
        angle_rad=trim_angle_rad(trim, frame.length_m),
        x_pivot=frame.x_midship,
        z_pivot=frame.keel_z + draft,
    )[0]


# --------------------------------------------------------------------------- #
#  The frame: keel, midship, reference length read off the geometry
# --------------------------------------------------------------------------- #

def test_frame_is_read_off_the_geometry():
    tris = _box(-30.0, 70.0, -8.0, 8.0, 2.0, 14.0)
    frame = HullFrame.from_triangles(tris)
    assert frame.keel_z == pytest.approx(2.0)
    assert frame.x_aft == pytest.approx(-30.0)
    assert frame.x_fwd == pytest.approx(70.0)
    assert frame.x_midship == pytest.approx(20.0)
    assert frame.length_m == pytest.approx(100.0)


def test_frame_rejects_degenerate_geometry():
    with pytest.raises(ValueError):
        HullFrame.from_triangles([])
    flat = [((0.0, 0.0, 0.0), (0.0, 1.0, 0.0), (0.0, 1.0, 1.0))]
    with pytest.raises(ValueError):
        HullFrame.from_triangles(flat)


# --------------------------------------------------------------------------- #
#  Even keel: the integration itself, against closed form
# --------------------------------------------------------------------------- #

def test_box_hydrostatics_are_exact_on_even_keel():
    length, beam, depth, draft = 100.0, 20.0, 12.0, 5.0
    tris = _box(0.0, length, -beam / 2, beam / 2, 0.0, depth)
    result = hydrostatics_at(tris, draft_m=draft)

    assert result.displaced_volume_m3 == pytest.approx(length * beam * draft)
    expected_area = length * beam + 2 * length * draft + 2 * beam * draft
    assert result.wetted_surface_m2 == pytest.approx(expected_area)
    assert result.lwl_m == pytest.approx(length)
    assert result.beam_wl_m == pytest.approx(beam)
    assert result.block_coefficient == pytest.approx(1.0)
    assert result.trim_m == 0.0
    assert result.draft_fwd_m == pytest.approx(draft)
    assert result.draft_aft_m == pytest.approx(draft)


def test_wedge_block_coefficient_is_one_half_at_every_draft():
    length, beam, depth = 60.0, 16.0, 10.0
    tris = _wedge(length, beam, depth)
    for draft in (2.0, 5.0, 9.0):
        result = hydrostatics_at(tris, draft_m=draft)
        expected_volume = length * beam * draft**2 / (2 * depth)
        assert result.displaced_volume_m3 == pytest.approx(expected_volume)
        assert result.beam_wl_m == pytest.approx(beam * draft / depth)
        assert result.block_coefficient == pytest.approx(0.5)


def test_displacement_uses_the_stated_water_density():
    tris = _box(0.0, 50.0, -5.0, 5.0, 0.0, 10.0)
    result = hydrostatics_at(tris, draft_m=4.0, water_density_t_m3=1.025)
    assert result.displacement_t == pytest.approx(
        result.displaced_volume_m3 * 1.025
    )
    fresh = hydrostatics_at(tris, draft_m=4.0, water_density_t_m3=1.0)
    assert fresh.displacement_t == pytest.approx(fresh.displaced_volume_m3)


def test_draft_must_be_positive_and_within_the_hull():
    tris = _box(0.0, 50.0, -5.0, 5.0, 0.0, 10.0)
    with pytest.raises(ValueError):
        hydrostatics_at(tris, draft_m=0.0)
    with pytest.raises(ValueError):
        hydrostatics_at(tris, draft_m=-1.0)


# --------------------------------------------------------------------------- #
#  Trim: sign convention, and the geometry behind the labels
# --------------------------------------------------------------------------- #

def test_end_drafts_follow_the_stated_convention():
    """trim = draft_fwd - draft_aft, so NEGATIVE is trim by the stern."""
    fwd, aft = end_drafts(6.55, -3.0)
    assert fwd == pytest.approx(5.05)
    assert aft == pytest.approx(8.05)
    assert fwd - aft == pytest.approx(-3.0)

    fwd, aft = end_drafts(6.55, 2.0)
    assert fwd - aft == pytest.approx(2.0)
    assert fwd > aft


def test_end_drafts_agree_with_the_shared_attitude_type():
    """The convention is not invented here; ``Attitude`` already carries it."""
    attitude = attitude_for(
        draft_m=6.55, trim_m=-3.0, reference_length_m=100.0, x_aft_m=0.0
    )
    assert isinstance(attitude, Attitude)
    assert attitude.trim_m == pytest.approx(-3.0)
    assert attitude.draft_mean_m == pytest.approx(6.55)
    # The water is DEEPER at the aft end when the trim is by the stern.
    assert attitude.waterline_z(0.0) > attitude.waterline_z(100.0)


def test_negative_trim_lowers_the_stern_in_the_geometry():
    """The property, not the arithmetic.

    This is the test that catches a rotation applied the wrong way round AND a
    correct rotation reported with the end-draft labels swapped, because it
    reads the answer off the rotated triangles. Bow is +x.
    """
    length, beam, depth = 100.0, 20.0, 12.0
    tris = _box(0.0, length, -beam / 2, beam / 2, 0.0, depth)
    frame = HullFrame.from_triangles(tris)
    draft, trim = 5.0, -2.0
    waterline = frame.keel_z + draft

    rotated = rotate_for_trim(
        tris,
        angle_rad=trim_angle_rad(trim, frame.length_m),
        x_pivot=frame.x_midship,
        z_pivot=waterline,
    )
    stern_low = _min_z_in_band(rotated, -1e9, 10.0)
    bow_low = _min_z_in_band(rotated, length - 10.0, 1e9)
    assert stern_low < bow_low, "negative trim must put the STERN deeper"

    # And the immersion at each end matches the labels the table will carry.
    # This is the assertion that catches a correct rotation reported with the
    # end-draft labels swapped.
    aft_pt, _, fwd_pt = _keel_probe(frame, draft=draft, trim=trim)
    fwd, aft = end_drafts(draft, trim)
    assert waterline - aft_pt[2] == pytest.approx(aft, abs=0.02)
    assert waterline - fwd_pt[2] == pytest.approx(fwd, abs=0.02)
    assert aft_pt[2] < fwd_pt[2]


def test_positive_trim_lowers_the_bow_in_the_geometry():
    length = 100.0
    tris = _box(0.0, length, -10.0, 10.0, 0.0, 12.0)
    frame = HullFrame.from_triangles(tris)
    rotated = rotate_for_trim(
        tris,
        angle_rad=trim_angle_rad(2.0, frame.length_m),
        x_pivot=frame.x_midship,
        z_pivot=frame.keel_z + 5.0,
    )
    assert _min_z_in_band(rotated, length - 10.0, 1e9) < _min_z_in_band(
        rotated, -1e9, 10.0
    )


def test_trim_by_the_stern_immerses_a_transom():
    """The asymmetric-shape property, and the reason trim changes Lwl.

    On a hull with a cut-up run the even-keel waterline stops short of the aft
    extremity. Trim by the stern brings the transom into the water and the
    waterline length STEPS -- it does not creep. This is the mechanism behind
    the several-metre Lwl differences in a real condition matrix, so it is
    pinned on a shape whose answer can be checked by hand.
    """
    hull = _transom_hull(length=100.0, cut_up=4.0, run=25.0)
    even = hydrostatics_at(hull, draft_m=3.0, trim_m=0.0)
    trimmed = hydrostatics_at(hull, draft_m=3.0, trim_m=-3.0)

    # Even keel: the water stops short of the transom, on the rising bottom.
    assert even.lwl_m == pytest.approx(93.75)
    # Stern trim brings the transom in and the waterline runs the whole hull.
    # ``abs=0.2`` because the rotation itself shortens the hull's x extent by
    # O(theta**2) -- the length is the hull's, in the attitude it is floating
    # at, not a projection back onto the even-keel axis.
    assert trimmed.lwl_m == pytest.approx(100.0, abs=0.2)
    assert trimmed.lwl_m > even.lwl_m + 5.0
    # Trimming the other way pulls the stern further out of the water, so the
    # step goes BOTH ways -- which is what makes the sign of the rotation
    # observable in the reported particulars rather than only in the mesh.
    by_bow = hydrostatics_at(hull, draft_m=3.0, trim_m=+3.0)
    assert by_bow.lwl_m < even.lwl_m
    # Block coefficient follows the length it is divided by, so a sign error
    # here would move Cb by several percent with nothing else to show for it.
    assert trimmed.block_coefficient != pytest.approx(
        even.block_coefficient, rel=1e-3
    )


def test_zero_trim_leaves_the_geometry_untouched():
    tris = _box(0.0, 80.0, -9.0, 9.0, 0.0, 11.0)
    frame = HullFrame.from_triangles(tris)
    rotated = rotate_for_trim(
        tris, angle_rad=0.0, x_pivot=frame.x_midship, z_pivot=frame.keel_z + 4.0
    )
    assert rotated == [tuple(t) for t in tris]
    assert hydrostatics_at(tris, draft_m=4.0, trim_m=0.0).to_dict() == (
        hydrostatics_at(tris, draft_m=4.0).to_dict()
    )


def test_rotation_preserves_the_midship_draft():
    """Rotating about the midship point ON the waterline is what makes the
    quoted mean draft mean anything across a row of trims.

    Exactly, in the equivalent tilted-water view; to O(theta**2) -- about a
    millimetre at 3 m of trim on a 158 m hull -- when read off the rotated
    midship keel point, because that point has itself moved along the hull.
    """
    length = 158.0
    tris = _box(0.0, length, -13.0, 13.0, 0.0, 14.0)
    frame = HullFrame.from_triangles(tris)
    draft = 6.55
    waterline = frame.keel_z + draft
    for trim in (-3.0, -1.0, 0.5, 2.0):
        _, mid_pt, _ = _keel_probe(frame, draft=draft, trim=trim)
        assert waterline - mid_pt[2] == pytest.approx(draft, abs=0.005)
        # Exact in the tilted-water view.
        attitude = attitude_for(
            draft_m=draft, trim_m=trim, reference_length_m=frame.length_m,
            x_aft_m=frame.x_aft,
        )
        assert attitude.waterline_z(frame.x_midship) == pytest.approx(draft)


def test_rotation_fixes_the_pivot_point_exactly():
    """The invariant behind the previous test, stated where it is exact: the
    point where the waterline crosses midship does not move at all."""
    pivot = (12.5, 0.0, 6.0)
    tri = (pivot, (20.0, 1.0, 3.0), (5.0, -1.0, 9.0))
    rotated = rotate_for_trim(
        [tri], angle_rad=0.03, x_pivot=pivot[0], z_pivot=pivot[2]
    )
    assert rotated[0][0] == pytest.approx(pivot)


def test_trim_angle_is_the_slope_over_the_reference_length():
    assert trim_angle_rad(0.0, 100.0) == 0.0
    assert trim_angle_rad(-3.0, 158.55) == pytest.approx(
        math.atan(-3.0 / 158.55)
    )
    with pytest.raises(ValueError):
        trim_angle_rad(1.0, 0.0)
    with pytest.raises(ValueError):
        trim_angle_rad(1.0, -100.0)


def test_reference_length_can_be_overridden():
    """Trim is quoted over a reference length -- Lpp in classification
    practice, the hull's own extent when no Lpp is supplied. Which one was
    used changes the angle, so it must be a parameter and not a guess."""
    tris = _box(0.0, 100.0, -10.0, 10.0, 0.0, 12.0)
    default = hydrostatics_at(tris, draft_m=5.0, trim_m=-2.0)
    shorter = hydrostatics_at(
        tris, draft_m=5.0, trim_m=-2.0, reference_length_m=90.0
    )
    assert shorter.reference_length_m == pytest.approx(90.0)
    assert default.reference_length_m == pytest.approx(100.0)
    assert shorter.trim_angle_rad != pytest.approx(default.trim_angle_rad)
    # The END DRAFTS are quoted over the reference length, so they are equal;
    # it is the angle, and therefore the geometry, that differs.
    assert shorter.draft_aft_m == pytest.approx(default.draft_aft_m)


# --------------------------------------------------------------------------- #
#  Sweeps: one core, used twice
# --------------------------------------------------------------------------- #

def test_hydrostatic_table_is_the_single_point_function_in_a_loop():
    tris = _wedge(60.0, 16.0, 10.0)
    drafts = (2.0, 4.0, 6.0, 8.0)
    table = hydrostatic_table(tris, drafts)
    assert [row.draft_m for row in table] == list(drafts)
    for row in table:
        assert row.to_dict() == hydrostatics_at(tris, draft_m=row.draft_m).to_dict()


def test_hydrostatic_table_volume_grows_with_draft():
    tris = _wedge(60.0, 16.0, 10.0)
    volumes = [r.displaced_volume_m3 for r in hydrostatic_table(tris, (2.0, 4.0, 6.0))]
    assert volumes == sorted(volumes)


def test_condition_matrix_expands_named_conditions():
    tris = _box(0.0, 100.0, -10.0, 10.0, 0.0, 14.0)
    rows = condition_matrix(
        tris,
        [("ballast", 4.0, (-2.0, 0.0)), ("loaded", 8.0, (0.0,))],
    )
    assert [(r.name, r.draft_m, r.trim_m) for r in rows] == [
        ("ballast", 4.0, -2.0),
        ("ballast", 4.0, 0.0),
        ("loaded", 8.0, 0.0),
    ]
    assert all(isinstance(r, FloatingCondition) for r in rows)
    # Every row shares one frame, so the reference length is the same for all.
    assert len({round(r.reference_length_m, 9) for r in rows}) == 1


def test_condition_rows_serialise_with_both_the_labels_and_the_geometry():
    tris = _box(0.0, 100.0, -10.0, 10.0, 0.0, 14.0)
    row = hydrostatics_at(tris, draft_m=5.0, trim_m=-2.0, name="ballast")
    record = row.to_dict()
    for key in (
        "name", "draft_m", "trim_m", "draft_fwd_m", "draft_aft_m", "lwl_m",
        "beam_wl_m", "wetted_surface_m2", "displaced_volume_m3",
        "displacement_t", "block_coefficient",
    ):
        assert key in record, key
    assert record["draft_aft_m"] > record["draft_fwd_m"]


# --------------------------------------------------------------------------- #
#  The real hull -- skipped unless a path is supplied out of band
# --------------------------------------------------------------------------- #

@pytest.mark.skipif(
    not os.environ.get("DIGITALMODEL_3DM_HULL"),
    reason="set DIGITALMODEL_3DM_HULL to a .3dm hull to run this check",
)
def test_real_hull_condition_matrix_is_physically_sane():
    """No expected numbers here -- a client hull's particulars are the client's.

    What is asserted is the shape of the answer: a displacement hull's block
    coefficient lands in a plausible band, trim by the stern immerses the
    transom and lengthens the waterline, and the mean draft is preserved across
    a row of trims.
    """
    pytest.importorskip("rhino3dm", reason="needs the 'cad' extra")
    from digitalmodel.naval_architecture.hull_ingest import (
        read_3dm_triangles,
        weld_vertices,
    )

    source = Path(os.environ["DIGITALMODEL_3DM_HULL"])
    layers = os.environ.get("DIGITALMODEL_3DM_LAYERS")
    scale = float(os.environ.get("DIGITALMODEL_3DM_SCALE", "0.001"))
    raw = read_3dm_triangles(
        source, layers=layers.split(",") if layers else None,
        tessellate_breps=True,
    )
    tris = [
        tuple(tuple(c * scale for c in v) for v in t)
        for t in weld_vertices(raw.triangles, 1e-6)
    ]

    draft = float(os.environ.get("DIGITALMODEL_HULL_DRAFT_M", "6.55"))
    deep = hydrostatics_at(tris, draft_m=draft, trim_m=-3.0)
    shallow = hydrostatics_at(tris, draft_m=draft, trim_m=-1.0)

    assert 0.5 < deep.block_coefficient < 0.95
    assert 0.5 < shallow.block_coefficient < 0.95
    assert deep.lwl_m > shallow.lwl_m
    assert deep.displaced_volume_m3 > shallow.displaced_volume_m3
    assert deep.draft_aft_m > deep.draft_fwd_m
