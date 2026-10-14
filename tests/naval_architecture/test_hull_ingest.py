"""Client hull geometry ingestion: .3dm -> normalised watertight STL (#2023).

WHAT THESE TESTS DEFEND
-----------------------
The stage converts a client CAD file into a solvable case. Three of its
failure modes are silent, which is why they are asserted here rather than
left to inspection:

  * a wrong unit guess produces a hull that is plausible but the wrong SIZE,
    and therefore the wrong Reynolds number. Inference is asserted to FAIL
    when the evidence is ambiguous rather than pick the likelier option.
  * a wrong forward direction tows the hull stern-first. It meshes, it
    solves, and it answers a different question.
  * a leaky surface meshes cleanly under snappyHexMesh and yields a
    confidently wrong answer. Watertightness is asserted as an edge COUNT and
    enforced as a gate, not recorded as a note.

Every geometry test here uses a SYNTHETIC hull built in pure Python, so the
suite never needs ``rhino3dm`` or a client file to run. Tests that exercise a
real ``.3dm`` are opt-in through ``DIGITALMODEL_HULL_3DM`` and skip cleanly
when it is unset, which keeps both CI green and client paths out of the repo.
"""

from __future__ import annotations

import hashlib
import json
import math
import os
from pathlib import Path
from typing import List, Tuple

import pytest

from digitalmodel.naval_architecture.hull_ingest import (
    CAD_EXTRA,
    MANIFEST_CONTRACT_KEYS,
    RHINO_UNIT_CODES,
    UNIT_SCALE_TO_M,
    AmbiguousOrientationError,
    AmbiguousUnitsError,
    ImplausibleScaleError,
    MissingCadDependencyError,
    NotWatertightError,
    infer_orientation,
    infer_units,
    ingest_3dm,
    ingest_triangles,
    weld_vertices,
)

Vec3 = Tuple[float, float, float]
Tri = Tuple[Vec3, Vec3, Vec3]


# --------------------------------------------------------------------------- #
#  Synthetic hulls - exact arithmetic, no CAD dependency
# --------------------------------------------------------------------------- #

def _quad(a: Vec3, b: Vec3, c: Vec3, d: Vec3) -> List[Tri]:
    return [(a, b, c), (a, c, d)]


def box_hull(length: float, beam: float, depth: float) -> List[Tri]:
    """A closed rectangular barge: keel at z=0, centreplane at y=0.

    Chosen because every quantity the manifest reports is known in closed
    form, so the hydrostatics are checked against arithmetic rather than
    against a previous run of the same code.
    """
    x0, x1 = 0.0, length
    y0, y1 = -beam / 2.0, beam / 2.0
    z0, z1 = 0.0, depth
    a = (x0, y0, z0)
    b = (x1, y0, z0)
    c = (x1, y1, z0)
    d = (x0, y1, z0)
    e = (x0, y0, z1)
    f = (x1, y0, z1)
    g = (x1, y1, z1)
    h = (x0, y1, z1)
    tris: List[Tri] = []
    tris += _quad(a, d, c, b)  # bottom, -z
    tris += _quad(e, f, g, h)  # top, +z
    tris += _quad(a, b, f, e)  # starboard-of-nothing side at y0
    tris += _quad(d, h, g, c)  # side at y1
    tris += _quad(a, e, h, d)  # end at x0
    tris += _quad(b, c, g, f)  # end at x1
    return tris


def wedge_hull(length: float, beam: float, depth: float) -> List[Tri]:
    """A barge whose +x end tapers to a vertical stem.

    The taper is the only asymmetry, so it is the signal orientation
    inference must find: the fine end is the bow.
    """
    s1 = (0.0, -beam / 2.0, 0.0)
    s2 = (0.0, beam / 2.0, 0.0)
    s3 = (0.0, beam / 2.0, depth)
    s4 = (0.0, -beam / 2.0, depth)
    b1 = (length, 0.0, 0.0)
    b2 = (length, 0.0, depth)
    tris: List[Tri] = []
    tris += _quad(s1, s4, s3, s2)      # transom at x=0
    tris.append((s1, s2, b1))          # bottom
    tris.append((s4, b2, s3))          # top
    tris += _quad(s1, b1, b2, s4)      # port-side face
    tris += _quad(s2, s3, b2, b1)      # starboard-side face
    return tris


def prismatic_hull(
    stations: List[Tuple[float, float]], depth: float = 10.0
) -> List[Tri]:
    """A closed hull lofted through ``(x, half_beam)`` stations.

    Deliberately box-sectioned: the point of this fixture is the PLAN shape,
    because that is what forward-direction inference reads.
    """
    tris: List[Tri] = []
    for (x0, b0), (x1, b1) in zip(stations, stations[1:]):
        for sign in (-1.0, 1.0):                       # the two sides
            tris += _quad(
                (x0, sign * b0, 0.0),
                (x1, sign * b1, 0.0),
                (x1, sign * b1, depth),
                (x0, sign * b0, depth),
            )
        for z in (0.0, depth):                         # bottom and top
            tris += _quad(
                (x0, -b0, z), (x1, -b1, z), (x1, b1, z), (x0, b0, z)
            )
    for x, b in (stations[0], stations[-1]):           # the end caps
        if b > 0.0:
            tris += _quad(
                (x, -b, 0.0), (x, b, 0.0), (x, b, depth), (x, -b, depth)
            )
    return [t for t in tris if _area(*t) > 0.0]


def _area(a: Vec3, b: Vec3, c: Vec3) -> float:
    ux, uy, uz = b[0] - a[0], b[1] - a[1], b[2] - a[2]
    vx, vy, vz = c[0] - a[0], c[1] - a[1], c[2] - a[2]
    nx, ny, nz = uy * vz - uz * vy, uz * vx - ux * vz, ux * vy - uy * vx
    return 0.5 * math.sqrt(nx * nx + ny * ny + nz * nz)


#: A full-form hull: fine ends, long parallel midbody, bow (the finer end) at
#: +x. Measured 20% in from each end it looks perfectly symmetric.
FULL_FORM_STATIONS: List[Tuple[float, float]] = [
    (0.0, 6.0), (2.0, 9.0), (5.0, 12.0), (10.0, 16.0),
    (20.0, 20.0), (30.0, 20.0), (70.0, 20.0), (80.0, 20.0),
    (90.0, 16.0), (95.0, 11.0), (98.0, 7.0), (100.0, 3.0),
]


def scaled(tris: List[Tri], factor: float) -> List[Tri]:
    out = [tuple(tuple(c * factor for c in p) for p in t) for t in tris]
    return out  # type: ignore[return-value]


@pytest.fixture
def out_dir(tmp_path: Path) -> Path:
    return tmp_path / "case"


# --------------------------------------------------------------------------- #
#  The manifest is a contract with another lane
# --------------------------------------------------------------------------- #

def test_manifest_carries_exactly_the_agreed_contract_keys(out_dir: Path) -> None:
    """The consuming lane sizes its domain from these names.

    Asserted as an explicit set so that renaming a field breaks here rather
    than silently producing a domain of the wrong size downstream.
    """
    assert MANIFEST_CONTRACT_KEYS == (
        "source_file",
        "source_sha256",
        "units_in",
        "scale_to_m",
        "orientation",
        "origin",
        "lpp_m",
        "beam_m",
        "draft_m",
        "wetted_surface_m2",
        "displacement_m3",
        "watertight",
        "n_triangles",
        "bbox_min_m",
        "bbox_max_m",
    )

    manifest = ingest_triangles(
        box_hull(100.0, 20.0, 10.0),
        out_dir,
        source_name="synthetic.3dm",
        units_in="m",
        forward="+x",
        draft_m=5.0,
    )
    data = manifest.to_dict()
    for key in MANIFEST_CONTRACT_KEYS:
        assert key in data, f"manifest lost contract key {key!r}"

    assert data["orientation"] == {"x": "forward", "y": "port", "z": "up"}
    assert data["origin"] == "aft_perpendicular_keel"
    assert isinstance(data["watertight"], bool)
    assert isinstance(data["n_triangles"], int)
    assert len(data["bbox_min_m"]) == 3
    assert len(data["bbox_max_m"]) == 3


def test_manifest_is_written_beside_the_stl_and_is_valid_json(out_dir: Path) -> None:
    manifest = ingest_triangles(
        box_hull(100.0, 20.0, 10.0),
        out_dir,
        source_name="synthetic.3dm",
        units_in="m",
        forward="+x",
        draft_m=5.0,
    )
    stl = out_dir / "hull.stl"
    side = out_dir / "hull_manifest.json"
    assert stl.exists() and side.exists()
    assert side.parent == stl.parent

    on_disk = json.loads(side.read_text())
    assert on_disk == manifest.to_dict()
    assert stl.read_text().startswith("solid ")


def test_source_sha256_is_the_digest_of_the_source_file(tmp_path: Path) -> None:
    """Provenance has to be checkable, not merely present."""
    src = tmp_path / "synthetic.3dm"
    src.write_bytes(b"not really a 3dm, but a stable byte string")
    expected = hashlib.sha256(src.read_bytes()).hexdigest()

    manifest = ingest_triangles(
        box_hull(100.0, 20.0, 10.0),
        tmp_path / "case",
        source_path=src,
        units_in="m",
        forward="+x",
        draft_m=5.0,
    )
    assert manifest.source_sha256 == expected
    assert manifest.source_file == "synthetic.3dm"


# --------------------------------------------------------------------------- #
#  Units: inferred, stated, overridable - never silently guessed
# --------------------------------------------------------------------------- #

def test_unit_scale_table_is_exact() -> None:
    assert UNIT_SCALE_TO_M["mm"] == 0.001
    assert UNIT_SCALE_TO_M["m"] == 1.0
    assert UNIT_SCALE_TO_M["ft"] == 0.3048
    assert UNIT_SCALE_TO_M["in"] == 0.0254
    assert RHINO_UNIT_CODES[4] == "m"
    assert RHINO_UNIT_CODES[8] == "in"


def test_units_declared_by_the_file_are_used_and_recorded() -> None:
    decision = infer_units(336.0, declared="m")
    assert decision.units == "m"
    assert decision.scale_to_m == 1.0
    assert decision.source == "file_header"


def test_caller_override_beats_the_file_header() -> None:
    """A file's declared unit is often wrong; the caller must be able to win."""
    decision = infer_units(336.0, declared="m", override="ft")
    assert decision.units == "ft"
    assert decision.source == "caller"


def test_ambiguous_extent_fails_rather_than_picking() -> None:
    """180 units is 180 m or 54.9 m depending on the file; both are ships.

    This is the exact case that produces a plausible hull of the wrong size,
    so it must raise rather than resolve to the more common option.
    """
    with pytest.raises(AmbiguousUnitsError) as excinfo:
        infer_units(180.0, declared=None)
    message = str(excinfo.value)
    assert "m" in message and "ft" in message
    assert "units_in" in message


def test_unambiguous_extent_is_inferred_when_the_file_is_silent() -> None:
    """180 000 units can only be millimetres: every other reading is absurd."""
    decision = infer_units(180_000.0, declared=None)
    assert decision.units == "mm"
    assert decision.scale_to_m == 0.001
    assert decision.source == "inferred_extent"


def test_declared_units_that_give_an_absurd_hull_are_rejected() -> None:
    """A file declaring mm while holding metres is a real and silent failure."""
    with pytest.raises(ImplausibleScaleError):
        infer_units(180.0, declared="mm")


def test_forced_units_bypass_the_plausibility_gate() -> None:
    """A model-scale towing-tank hull is genuinely small; the caller may say so."""
    decision = infer_units(180.0, override="mm", force=True)
    assert decision.units == "mm"
    assert decision.source == "caller"


def test_millimetre_input_is_scaled_to_metres_in_the_manifest(out_dir: Path) -> None:
    manifest = ingest_triangles(
        scaled(box_hull(100.0, 20.0, 10.0), 1000.0),
        out_dir,
        source_name="synthetic.3dm",
        units_in="mm",
        forward="+x",
        draft_m=5.0,
    )
    assert manifest.scale_to_m == 0.001
    assert manifest.lpp_m == pytest.approx(100.0)
    assert manifest.beam_m == pytest.approx(20.0)
    assert manifest.bbox_max_m[0] == pytest.approx(100.0)


# --------------------------------------------------------------------------- #
#  Orientation
# --------------------------------------------------------------------------- #

def test_orientation_inference_finds_the_bow_at_the_fine_end() -> None:
    decision = infer_orientation(wedge_hull(100.0, 20.0, 10.0))
    assert decision.forward == "+x"
    assert decision.source == "inferred_taper"


def test_orientation_inference_follows_a_mirrored_hull() -> None:
    """The signal is the taper, not a coordinate convention."""
    mirrored = [
        tuple((-p[0], p[1], p[2]) for p in tri) for tri in wedge_hull(100.0, 20.0, 10.0)
    ]
    decision = infer_orientation(mirrored)  # type: ignore[arg-type]
    assert decision.forward == "-x"


def test_full_form_hull_is_resolved_from_its_extremities() -> None:
    """A hull with parallel midbody must still be read correctly.

    Regression test for a real defect. Fineness was originally compared 20% in
    from each end, where a hull with any parallel midbody is at full beam on
    both sides; on a real tanker that reported the bow at the WRONG end, while
    every band at or below 10% agreed on the right one. This fixture has the
    same trap: measured 20% in it is symmetric, and only the extremities carry
    the signal.
    """
    wide_band = infer_orientation(
        prismatic_hull(FULL_FORM_STATIONS), bands=(0.01, 0.02, 0.05)
    )
    assert wide_band.forward == "+x"
    assert wide_band.taper_ratio < 0.9

    with pytest.raises(AmbiguousOrientationError):
        infer_orientation(prismatic_hull(FULL_FORM_STATIONS), bands=(0.2,))


def test_bands_that_disagree_refuse_rather_than_average() -> None:
    """Disagreement between bands is evidence of no signal, not a tie to break."""
    with pytest.raises(AmbiguousOrientationError) as excinfo:
        infer_orientation(
            prismatic_hull(FULL_FORM_STATIONS), bands=(0.02, 0.2)
        )
    assert "disagree" in str(excinfo.value)


def test_symmetric_hull_refuses_to_guess_a_bow() -> None:
    """A barge has no finer end. Picking one would be a coin flip."""
    with pytest.raises(AmbiguousOrientationError) as excinfo:
        infer_orientation(box_hull(100.0, 20.0, 10.0))
    assert "forward" in str(excinfo.value)


def test_caller_supplied_forward_direction_is_honoured(out_dir: Path) -> None:
    """The barge that cannot be inferred is still ingestible when told."""
    manifest = ingest_triangles(
        box_hull(100.0, 20.0, 10.0),
        out_dir,
        source_name="synthetic.3dm",
        units_in="m",
        forward="-x",
        draft_m=5.0,
    )
    assert manifest.lpp_m == pytest.approx(100.0)
    assert manifest.orientation == {"x": "forward", "y": "port", "z": "up"}


def test_a_hull_pointing_along_y_is_rotated_onto_x(out_dir: Path) -> None:
    """Rotation must be a real change of basis, not a relabelling."""
    along_y = [
        tuple((p[1], p[0], p[2]) for p in tri) for tri in wedge_hull(100.0, 20.0, 10.0)
    ]
    manifest = ingest_triangles(
        along_y,  # type: ignore[arg-type]
        out_dir,
        source_name="synthetic.3dm",
        units_in="m",
        draft_m=5.0,
    )
    assert manifest.lpp_m == pytest.approx(100.0)
    assert manifest.beam_m == pytest.approx(20.0)
    assert manifest.bbox_max_m[0] == pytest.approx(100.0)


# --------------------------------------------------------------------------- #
#  Origin normalisation
# --------------------------------------------------------------------------- #

def test_hull_is_placed_on_the_aft_perpendicular_at_the_keel(out_dir: Path) -> None:
    """``origin`` in the manifest is a promise about the STL's coordinates."""
    shifted = [
        tuple((p[0] + 500.0, p[1] + 30.0, p[2] + 7.0) for p in tri)
        for tri in box_hull(100.0, 20.0, 10.0)
    ]
    manifest = ingest_triangles(
        shifted,  # type: ignore[arg-type]
        out_dir,
        source_name="synthetic.3dm",
        units_in="m",
        forward="+x",
        draft_m=5.0,
    )
    assert manifest.bbox_min_m[0] == pytest.approx(0.0, abs=1e-9)  # AP at x=0
    assert manifest.bbox_min_m[2] == pytest.approx(0.0, abs=1e-9)  # keel at z=0
    assert manifest.bbox_min_m[1] == pytest.approx(-10.0)          # centreplane
    assert manifest.bbox_max_m[1] == pytest.approx(10.0)


# --------------------------------------------------------------------------- #
#  Hydrostatics, against closed-form arithmetic
# --------------------------------------------------------------------------- #

def test_displacement_and_wetted_area_match_the_closed_form(out_dir: Path) -> None:
    length, beam, depth, draft = 100.0, 20.0, 10.0, 6.0
    manifest = ingest_triangles(
        box_hull(length, beam, depth),
        out_dir,
        source_name="synthetic.3dm",
        units_in="m",
        forward="+x",
        draft_m=draft,
    )
    assert manifest.displacement_m3 == pytest.approx(length * beam * draft)
    expected_wetted = length * beam + 2 * length * draft + 2 * beam * draft
    assert manifest.wetted_surface_m2 == pytest.approx(expected_wetted)
    assert manifest.draft_m == pytest.approx(draft)


def test_wedge_displacement_matches_the_closed_form(out_dir: Path) -> None:
    length, beam, draft = 100.0, 20.0, 4.0
    manifest = ingest_triangles(
        wedge_hull(length, beam, 10.0),
        out_dir,
        source_name="synthetic.3dm",
        units_in="m",
        draft_m=draft,
    )
    assert manifest.displacement_m3 == pytest.approx(0.5 * beam * length * draft)


# --------------------------------------------------------------------------- #
#  Watertightness is a gate
# --------------------------------------------------------------------------- #

def test_closed_hull_is_reported_watertight(out_dir: Path) -> None:
    manifest = ingest_triangles(
        box_hull(100.0, 20.0, 10.0),
        out_dir,
        source_name="synthetic.3dm",
        units_in="m",
        forward="+x",
        draft_m=5.0,
    )
    assert manifest.watertight is True
    assert manifest.open_edge_count == 0
    assert manifest.n_triangles == 12


def test_leaky_hull_is_refused(out_dir: Path) -> None:
    """snappyHexMesh would mesh this and answer confidently. The stage must not."""
    leaky = box_hull(100.0, 20.0, 10.0)[:-1]
    with pytest.raises(NotWatertightError) as excinfo:
        ingest_triangles(
            leaky,
            out_dir,
            source_name="synthetic.3dm",
            units_in="m",
            forward="+x",
            draft_m=5.0,
        )
    assert "open" in str(excinfo.value).lower()
    assert not (out_dir / "hull_manifest.json").exists(), (
        "a refused hull must not leave a manifest another lane could consume"
    )


def test_leaky_hull_can_be_forced_but_is_marked_not_watertight(out_dir: Path) -> None:
    leaky = box_hull(100.0, 20.0, 10.0)[:-1]
    manifest = ingest_triangles(
        leaky,
        out_dir,
        source_name="synthetic.3dm",
        units_in="m",
        forward="+x",
        draft_m=5.0,
        force=True,
    )
    assert manifest.watertight is False
    assert manifest.open_edge_count > 0
    assert manifest.to_dict()["watertight"] is False


# --------------------------------------------------------------------------- #
#  Welding - float32 render meshes do not share bit-identical vertices
# --------------------------------------------------------------------------- #

def test_welding_closes_a_surface_split_by_float_noise() -> None:
    """Rhino render meshes store vertices as float32.

    Adjacent patches therefore meet at coordinates that differ in the last
    bits. Without a weld the surface reads as leaky and a real hull would be
    rejected for a defect it does not have.
    """
    tris = box_hull(100.0, 20.0, 10.0)
    nudged: List[Tri] = []
    for i, tri in enumerate(tris):
        if i % 2:
            shifted = tuple((p[0] + 3e-7, p[1], p[2]) for p in tri)
            nudged.append(shifted)  # type: ignore[arg-type]
        else:
            nudged.append(tri)

    welded = weld_vertices(nudged, tolerance=1e-5)
    coords = {p for tri in welded for p in tri}
    assert len(coords) == 8, "a welded box has exactly eight distinct corners"


def test_welding_does_not_collapse_genuinely_distinct_points() -> None:
    tris = box_hull(100.0, 20.0, 10.0)
    welded = weld_vertices(tris, tolerance=1e-6)
    assert len({p for tri in welded for p in tri}) == 8


# --------------------------------------------------------------------------- #
#  The optional CAD dependency
# --------------------------------------------------------------------------- #

def test_missing_cad_dependency_names_the_extra_to_install(monkeypatch) -> None:
    """The error has to tell the user the exact command that fixes it."""
    import digitalmodel.naval_architecture.hull_ingest as mod

    monkeypatch.setattr(mod, "_import_rhino3dm", mod._raise_missing_cad)
    with pytest.raises(MissingCadDependencyError) as excinfo:
        ingest_3dm(Path("nonexistent.3dm"), Path("out"))
    message = str(excinfo.value)
    assert CAD_EXTRA in message
    assert "rhino3dm" in message
    assert "install" in message.lower()


def test_core_package_does_not_import_rhino3dm_at_module_level() -> None:
    """The core install must work without the CAD extra.

    Asserted on the module source rather than on an import, because the
    dependency may happen to be installed in the developing environment.
    """
    import digitalmodel.naval_architecture.hull_ingest as mod

    source = Path(mod.__file__).read_text()
    for line in source.splitlines():
        stripped = line.strip()
        if stripped.startswith(("import rhino3dm", "from rhino3dm")):
            assert line.startswith((" ", "\t")), (
                "rhino3dm must only be imported inside a function"
            )


# --------------------------------------------------------------------------- #
#  Real .3dm files - opt-in, skipped when unavailable
# --------------------------------------------------------------------------- #

_REAL_3DM = os.environ.get("DIGITALMODEL_HULL_3DM", "")

requires_real_3dm = pytest.mark.skipif(
    not _REAL_3DM or not Path(_REAL_3DM).is_file(),
    reason="set DIGITALMODEL_HULL_3DM to a readable .3dm to run this",
)


@requires_real_3dm
def test_real_3dm_ingests_to_a_manifest(tmp_path: Path) -> None:
    pytest.importorskip("rhino3dm", reason="install the [cad] extra")
    manifest = ingest_3dm(
        Path(_REAL_3DM),
        tmp_path / "case",
        layers=os.environ.get("DIGITALMODEL_HULL_LAYERS", "").split(",") or None,
        force=True,
    )
    assert manifest.n_triangles > 0
    assert manifest.lpp_m > 0.0
    assert (tmp_path / "case" / "hull_manifest.json").exists()
