"""
ABOUTME: The ingestion-lane contract (#2023). ``hull_manifest.json`` is the ONLY
channel through which an arbitrary client hull reaches the case builder, so its
loader fails closed on every field the downstream derivations depend on.

A manifest that loads but is wrong produces a case that solves happily and
answers a different question. Every check below corresponds to a quantity that
is silently plausible when wrong: a hull in millimetres, a hull whose origin is
not the keel, a hull mirrored bow-for-stern.
"""

from __future__ import annotations

import json

import pytest

from digitalmodel.solvers.openfoam.hull_manifest import (
    HullManifest,
    HullManifestError,
    load_hull_manifest,
)

from .conftest import SYNTHETIC_MANIFEST, scaled_manifest_dict


def test_loads_every_contract_field(manifest_file) -> None:
    m = load_hull_manifest(manifest_file)
    assert m.source_file == "synthetic_hull.stl"
    assert m.source_sha256 == "0" * 64
    assert m.units_in == "mm"
    assert m.scale_to_m == pytest.approx(0.001)
    assert m.origin == "aft_perpendicular_keel"
    assert m.lpp_m == pytest.approx(6.0)
    assert m.beam_m == pytest.approx(0.9)
    assert m.draft_m == pytest.approx(0.30)
    assert m.wetted_surface_m2 == pytest.approx(7.2)
    assert m.displacement_m3 == pytest.approx(0.72)
    assert m.watertight is True
    assert m.n_triangles == 120000


def test_dimensions_are_metres_regardless_of_source_units(manifest_dict) -> None:
    """``units_in``/``scale_to_m`` describe the SOURCE file, not the manifest.

    Every ``*_m`` field is already metres by contract. A loader that helpfully
    re-applied ``scale_to_m`` would shrink a 6 m hull to 6 mm and every
    downstream extent with it, and the case would still mesh.
    """
    m = HullManifest.from_dict(manifest_dict)
    assert m.lpp_m == pytest.approx(manifest_dict["lpp_m"])
    assert m.bbox_max_m[0] == pytest.approx(manifest_dict["bbox_max_m"][0])


@pytest.mark.parametrize("field", ["lpp_m", "beam_m", "draft_m", "wetted_surface_m2"])
def test_non_positive_principal_dimension_is_refused(manifest_dict, field) -> None:
    manifest_dict[field] = 0.0
    with pytest.raises(HullManifestError, match=field):
        HullManifest.from_dict(manifest_dict)


@pytest.mark.parametrize(
    "field",
    ["lpp_m", "beam_m", "draft_m", "orientation", "origin", "bbox_min_m"],
)
def test_missing_required_field_is_refused(manifest_dict, field) -> None:
    del manifest_dict[field]
    with pytest.raises(HullManifestError, match=field):
        HullManifest.from_dict(manifest_dict)


def test_unexpected_orientation_is_refused(manifest_dict) -> None:
    """A hull whose +x is aft is a hull towed stern-first.

    It solves. It converges. It reports a resistance. The builder places the
    bow at the inlet on the strength of this field alone, so an unexpected
    value has to stop the build rather than be reinterpreted.
    """
    manifest_dict["orientation"] = {"x": "aft", "y": "port", "z": "up"}
    with pytest.raises(HullManifestError, match="orientation"):
        HullManifest.from_dict(manifest_dict)


def test_unexpected_origin_is_refused(manifest_dict) -> None:
    """The free surface is placed at z = draft, which is only true keel-up."""
    manifest_dict["origin"] = "centre_of_buoyancy"
    with pytest.raises(HullManifestError, match="origin"):
        HullManifest.from_dict(manifest_dict)


def test_non_watertight_hull_is_refused(manifest_dict) -> None:
    """snappyHexMesh will mesh a leaking surface and lose the interior."""
    manifest_dict["watertight"] = False
    with pytest.raises(HullManifestError, match="watertight"):
        HullManifest.from_dict(manifest_dict)


def test_bbox_inconsistent_with_lpp_is_refused(manifest_dict) -> None:
    """A bbox that cannot contain the stated Lpp means one of them is wrong."""
    manifest_dict["bbox_max_m"] = [1.0, 0.45, 0.55]
    with pytest.raises(HullManifestError, match="bbox"):
        HullManifest.from_dict(manifest_dict)


def test_bbox_inconsistent_with_draft_is_refused(manifest_dict) -> None:
    """The keel is at z = 0 and the draft must fit inside the hull's z-extent."""
    manifest_dict["draft_m"] = 1.5
    manifest_dict["bbox_max_m"] = [6.15, 0.45, 0.55]
    with pytest.raises(HullManifestError, match="draft"):
        HullManifest.from_dict(manifest_dict)


def test_keel_off_the_origin_is_refused(manifest_dict) -> None:
    manifest_dict["bbox_min_m"] = [-0.05, -0.45, 0.20]
    manifest_dict["bbox_max_m"] = [6.15, 0.45, 0.75]
    with pytest.raises(HullManifestError, match="keel"):
        HullManifest.from_dict(manifest_dict)


def test_derived_geometry_properties(manifest_dict) -> None:
    m = HullManifest.from_dict(manifest_dict)
    assert m.loa_m == pytest.approx(6.20)
    assert m.midship_x_m == pytest.approx(3.05)
    assert m.hull_height_m == pytest.approx(0.55)
    assert m.block_coefficient == pytest.approx(0.72 / (6.0 * 0.9 * 0.30))


def test_bad_json_names_the_file(tmp_path) -> None:
    path = tmp_path / "hull_manifest.json"
    path.write_text("{not json")
    with pytest.raises(HullManifestError, match="hull_manifest.json"):
        load_hull_manifest(path)


def test_missing_file_names_the_file(tmp_path) -> None:
    with pytest.raises(HullManifestError, match="nope.json"):
        load_hull_manifest(tmp_path / "nope.json")


def test_scaled_fixture_is_geometrically_similar() -> None:
    """Guards the fixture the scale-sanity tests depend on."""
    big = scaled_manifest_dict(10.0)
    assert big["lpp_m"] == pytest.approx(SYNTHETIC_MANIFEST["lpp_m"] * 10)
    assert big["wetted_surface_m2"] == pytest.approx(
        SYNTHETIC_MANIFEST["wetted_surface_m2"] * 100
    )
    assert HullManifest.from_dict(big).block_coefficient == pytest.approx(
        HullManifest.from_dict(json.loads(json.dumps(SYNTHETIC_MANIFEST)))
        .block_coefficient
    )


# --------------------------------------------------------------------------- #
#  Cross-lane contract
# --------------------------------------------------------------------------- #

def test_the_ingestion_lane_and_this_loader_agree_on_the_contract() -> None:
    """The two lanes meet at this file and nowhere else.

    Skipped when the ingestion lane is not present, so this suite never depends
    on that lane landing -- but when both are present, a rename on either side
    has to fail here rather than at the first client hull.
    """
    ingest = pytest.importorskip(
        "digitalmodel.naval_architecture.hull_ingest",
        reason="ingestion lane not present in this checkout",
    )
    from digitalmodel.solvers.openfoam.hull_manifest import (
        EXPECTED_ORIENTATION,
        EXPECTED_ORIGIN,
        _REQUIRED,
    )

    assert set(ingest.MANIFEST_CONTRACT_KEYS) == set(_REQUIRED)
    assert dict(ingest.OUTPUT_ORIENTATION) == dict(EXPECTED_ORIENTATION)
    assert ingest.OUTPUT_ORIGIN == EXPECTED_ORIGIN
