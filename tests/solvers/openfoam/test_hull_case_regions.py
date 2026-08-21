"""N-surface case construction: hull plus appendages (#2023).

WHAT THESE TESTS DEFEND
-----------------------
The single-surface builder placed one STL, named it in three dictionaries, and
integrated forces over one patch. Extending it to N surfaces has four failure
modes and every one of them produces a case that meshes and solves:

  * a surface DECLARED but not COPIED, or copied but not declared. snappy
    simply does not know about the rudder. Nothing in the log says so.
  * a surface missing from ``refinementSurfaces``. It is then geometry snappy
    can see but will not make a patch from, so no force is ever integrated over
    it and no error is raised.
  * the hull's patch name drifting. ``forces``, ``forceCoeffs`` and the report
    lane all resolve it by name; a rename leaves a case that reports nothing.
  * ``Aref`` built from the SUM of the per-region wetted areas. The sum
    double-counts the interpenetration, Aref is a denominator, and every
    coefficient comes back low by exactly that inflation.

The manifest fixtures are synthetic. Nothing here needs a client file.
"""

from __future__ import annotations

import json
import re
from pathlib import Path
from typing import Any, Dict

import pytest

from digitalmodel.solvers.openfoam.hull_case import (
    HullCaseConfig,
    build_hull_case,
    derive_hull_case,
)
from digitalmodel.solvers.openfoam.hull_case_dicts import hull_case_tokens
from digitalmodel.solvers.openfoam.hull_case_regions import (
    HULL_PATCH,
    PATCH_FORCES_PREFIX,
    SurfaceRegion,
    check_region_surfaces,
    hull_region,
    region_tokens,
)
from digitalmodel.solvers.openfoam.hull_manifest import (
    HullManifest,
    HullManifestError,
)

VELOCITY = 1.5
RANKS = 8


# --------------------------------------------------------------------------- #
#  Fixtures
# --------------------------------------------------------------------------- #

def write_stl(path: Path, name: str) -> Path:
    path.write_text(
        f"solid {name}\n"
        "  facet normal 0 0 1\n"
        "    outer loop\n"
        "      vertex 0 0 0\n"
        "      vertex 1 0 0\n"
        "      vertex 0 1 0\n"
        "    endloop\n"
        "  endfacet\n"
        f"endsolid {name}\n"
    )
    return path


def region_block(
    hull_area: float, rudder_area: float, external: float
) -> Dict[str, Any]:
    """A manifest ``regions`` block in the ingestion lane's published shape."""
    return {
        "n_regions": 2,
        "waterline_z_m": 0.30,
        "regions": [
            {
                "name": "hull",
                "role": "hull",
                "stl_file": "synthetic_hull.stl",
                "n_triangles": 120000,
                "watertight": True,
                "open_edge_count": 0,
                "nonmanifold_edge_count": 0,
                "wetted_area_m2": hull_area,
                "wetted_area_external_m2": hull_area - 0.1,
                "wetted_area_occluded_m2": 0.1,
                "wetted_area_undecided_m2": 0.02,
            },
            {
                "name": "rudder",
                "role": "appendage",
                "stl_file": "rudder.stl",
                "n_triangles": 3400,
                "watertight": True,
                "open_edge_count": 0,
                "nonmanifold_edge_count": 0,
                "wetted_area_m2": rudder_area,
                "wetted_area_external_m2": rudder_area - 0.05,
                "wetted_area_occluded_m2": 0.05,
                "wetted_area_undecided_m2": 0.01,
            },
        ],
        "union": {
            "wetted_surface_naive_sum_m2": hull_area + rudder_area,
            "wetted_surface_external_m2": external,
            "wetted_surface_external_uncertainty_m2": 0.03,
            "double_counted_m2": hull_area + rudder_area - external,
            "merged_nonmanifold_edge_count": 520,
        },
    }


@pytest.fixture
def two_region_case(tmp_path: Path, manifest_dict: Dict[str, Any]):
    manifest_dict["regions"] = region_block(7.2, 0.4, 7.45)
    hull_stl = write_stl(tmp_path / "synthetic_hull.stl", "hull")
    rudder_stl = write_stl(tmp_path / "rudder.stl", "rudder")
    config = HullCaseConfig(
        manifest=HullManifest.from_dict(manifest_dict),
        stl_path=hull_stl,
        velocity=VELOCITY,
        ranks=RANKS,
        appendages=(SurfaceRegion(name="rudder", stl_path=rudder_stl),),
    )
    return config, tmp_path


def read(case: Path, rel: str) -> str:
    return (case / rel).read_text()


# --------------------------------------------------------------------------- #
#  Every surface reaches the case, in every dictionary that needs it
# --------------------------------------------------------------------------- #

def test_every_region_is_copied_and_declared_everywhere(two_region_case) -> None:
    """Declared, copied, refined, layered and feature-extracted -- all four
    dictionaries, or the surface is invisible to the mesher."""
    config, tmp_path = two_region_case
    case = build_hull_case(config, tmp_path / "out")

    tri = case / "constant" / "triSurface"
    assert (tri / "synthetic_hull.stl").is_file()
    assert (tri / "rudder.stl").is_file()

    snappy = read(case, "system/snappyHexMeshDict")
    for stl, patch in (("synthetic_hull.stl", "hull"), ("rudder.stl", "rudder")):
        assert stl in snappy, f"{stl} missing from geometry{{}}"
        assert f'file "{Path(stl).stem}.eMesh"' in snappy
        assert re.search(rf"^\s*{patch}\s*$", snappy, re.M), patch

    extract = read(case, "system/surfaceFeatureExtractDict")
    assert "synthetic_hull.stl" in extract
    assert "rudder.stl" in extract


def test_each_region_appears_in_refinement_and_in_layers(two_region_case) -> None:
    """A surface snappy can SEE but will not make a patch from integrates no
    force, and says nothing about it."""
    config, tmp_path = two_region_case
    case = build_hull_case(config, tmp_path / "out")
    snappy = read(case, "system/snappyHexMeshDict")
    refinement = _brace_block(snappy, "refinementSurfaces")
    layers = _brace_block(snappy, "layers")
    for patch in ("hull", "rudder"):
        assert patch in refinement
        assert patch in layers


def test_a_missing_appendage_stl_stops_the_build_before_it_writes(
    two_region_case,
) -> None:
    """A case missing one of two surfaces meshes perfectly. Nothing downstream
    reports the absence, so the refusal has to happen here."""
    config, tmp_path = two_region_case
    Path(config.appendages[0].stl_path).unlink()
    with pytest.raises(FileNotFoundError, match="rudder.stl"):
        build_hull_case(config, tmp_path / "out")
    assert not (tmp_path / "out").exists()


def test_the_hull_patch_name_is_pinned(two_region_case) -> None:
    """``forces``, ``forceCoeffs``, the layer block and the report lane all
    resolve the hull by this exact name."""
    config, _ = two_region_case
    assert HULL_PATCH == "hull"
    assert config.surface_regions[0].name == "hull"
    assert config.surface_regions[0].role == "hull"
    with pytest.raises(ValueError, match="first region must be the hull"):
        region_tokens(
            (SurfaceRegion(name="rudder", stl_path=Path("r.stl")),),
            c_of_r="0 0 0",
        )


def test_two_regions_sharing_an_stl_name_are_refused() -> None:
    """Both would be copied to the same path and one would win silently."""
    regions = (
        hull_region(Path("a/hull.stl")),
        SurfaceRegion(name="rudder", stl_path=Path("b/hull.stl")),
    )
    with pytest.raises(ValueError, match="share an STL file name"):
        check_region_surfaces(regions)


def test_a_region_name_that_is_not_a_patch_name_is_refused() -> None:
    """The name becomes a patch, a geometry key and a file stem at once."""
    with pytest.raises(ValueError, match="patch name"):
        SurfaceRegion(name="port rudder", stl_path=Path("r.stl"))


# --------------------------------------------------------------------------- #
#  Forces: union coefficients, per-patch newtons
# --------------------------------------------------------------------------- #

def test_forces_are_integrated_over_the_union_of_patches(two_region_case) -> None:
    """The reported total resistance is the VESSEL's, not the bare hull's."""
    config, tmp_path = two_region_case
    control = read(build_hull_case(config, tmp_path / "out"), "system/controlDict")
    union_blocks = re.findall(r"patches\s+\(([^)]*)\)", control)
    # forces, forceCoeffs, then one per patch.
    assert union_blocks[0].split() == ["hull", "rudder"]
    assert union_blocks[1].split() == ["hull", "rudder"]


def test_the_per_patch_breakdown_is_newtons_and_not_coefficients(
    two_region_case,
) -> None:
    """Newtons SUM. Per-patch coefficients would not: each would carry its own
    reference area, so the rudder's Cd could not be added to the hull's."""
    config, tmp_path = two_region_case
    control = read(build_hull_case(config, tmp_path / "out"), "system/controlDict")
    for patch in ("hull", "rudder"):
        block = _brace_block(control, f"{PATCH_FORCES_PREFIX}{patch}")
        assert "type            forces;" in block
        assert "forceCoeffs" not in block
        assert "Aref" not in block
        assert "rho             rho;" in block, "the VOF field, never a constant"
    assert control.count("type            forceCoeffs;") == 1


def test_a_single_region_case_gets_no_duplicate_forces_object(
    tmp_path: Path, manifest_dict: Dict[str, Any]
) -> None:
    """The union block already IS the hull's. A second identical function
    object would write the same force under a second name."""
    config = HullCaseConfig(
        manifest=HullManifest.from_dict(manifest_dict),
        stl_path=write_stl(tmp_path / "synthetic_hull.stl", "hull"),
        velocity=VELOCITY,
        ranks=RANKS,
    )
    control = read(build_hull_case(config, tmp_path / "out"), "system/controlDict")
    # The PROSE mentions forces_<patch>; what must be absent is the function
    # object, which is an entry on its own line.
    assert not re.search(rf"^\s*{PATCH_FORCES_PREFIX}\w+\s*$", control, re.M)
    assert control.count("type            forces;") == 1


# --------------------------------------------------------------------------- #
#  Aref
# --------------------------------------------------------------------------- #

def test_aref_is_half_the_UNION_area_not_half_the_sum(two_region_case) -> None:
    """The one number that is wrong in a way nothing downstream can detect.

    Aref is a denominator. The naive sum inflates it, and every reported
    coefficient comes back low by exactly that inflation in a case that
    converged perfectly.
    """
    config, tmp_path = two_region_case
    derivation = derive_hull_case(config)
    union = config.manifest.regions["union"]

    assert derivation.force_reference.a_ref == pytest.approx(
        union["wetted_surface_external_m2"] / 2.0
    )
    assert derivation.force_reference.a_ref != pytest.approx(
        union["wetted_surface_naive_sum_m2"] / 2.0
    )
    provenance = json.loads(
        (build_hull_case(config, tmp_path / "out") / "case_provenance.json")
        .read_text()
    )
    surfaces = provenance["surfaces"]
    assert surfaces["aref_source"] == "regions.union.wetted_surface_external_m2"
    assert surfaces["wetted_surface_upper_bound_m2"] == pytest.approx(
        union["wetted_surface_naive_sum_m2"]
    )
    assert surfaces["n_regions"] == 2


def test_a_hull_only_manifest_keeps_the_area_it_always_had(
    manifest_dict: Dict[str, Any]
) -> None:
    """No regions block means nothing changed for the single-surface path."""
    manifest = HullManifest.from_dict(manifest_dict)
    assert manifest.regions is None
    assert manifest.reference_wetted_surface_m2 == manifest.wetted_surface_m2
    assert manifest.wetted_surface_upper_bound_m2 == manifest.wetted_surface_m2
    assert manifest.appendage_regions == []


def test_a_regions_block_without_a_union_area_is_refused(
    manifest_dict: Dict[str, Any]
) -> None:
    """Falling back to the sum here would be the defect, silently."""
    block = region_block(7.2, 0.4, 7.45)
    del block["union"]["wetted_surface_external_m2"]
    manifest_dict["regions"] = block
    manifest = HullManifest.from_dict(manifest_dict)
    with pytest.raises(HullManifestError, match="wetted_surface_external_m2"):
        _ = manifest.reference_wetted_surface_m2


def test_a_region_that_is_not_watertight_is_refused_at_load(
    manifest_dict: Dict[str, Any]
) -> None:
    """Per REGION, because that is the premise the mesher relies on."""
    block = region_block(7.2, 0.4, 7.45)
    block["regions"][1]["watertight"] = False
    manifest_dict["regions"] = block
    with pytest.raises(HullManifestError, match="rudder"):
        HullManifest.from_dict(manifest_dict)


# --------------------------------------------------------------------------- #
#  Token hygiene
# --------------------------------------------------------------------------- #

def test_no_token_survives_a_multi_region_substitution(two_region_case) -> None:
    """OpenFOAM parses ``@COFR@`` as a word. The region blocks are the only
    token values built from another token's value, so this is where an
    ordering mistake would land."""
    config, tmp_path = two_region_case
    case = build_hull_case(config, tmp_path / "out")
    for path in sorted(case.rglob("*")):
        if path.is_file() and path.suffix != ".stl":
            assert not re.findall(r"@[A-Z0-9_]+@", path.read_text()), path


def test_the_per_patch_cofr_matches_the_union_cofr(two_region_case) -> None:
    """One centre of rotation, or the moments do not add up."""
    config, tmp_path = two_region_case
    tokens = hull_case_tokens(derive_hull_case(config))
    assert tokens["COFR"] in tokens["PERPATCHFORCES"]
    control = read(build_hull_case(config, tmp_path / "out"), "system/controlDict")
    assert len(set(re.findall(r"CofR\s+\(([^)]*)\)", control))) == 1


def test_the_double_body_tree_carries_the_same_regions(
    tmp_path: Path, manifest_dict: Dict[str, Any]
) -> None:
    """The two case types are DIVIDED to extract a form factor.

    (1 + k) = C_v,double-body / C_f,ITTC-57, and the double-body coefficient is
    then compared against the free-surface one. If one case meshed the rudder
    and the other did not, or if the two normalised by different areas, the
    ratio would be a comparison of two different ships and would still be a
    plausible number.
    """
    from digitalmodel.solvers.openfoam.hull_double_body import (
        DoubleBodyCaseConfig,
        build_double_body_case,
    )

    manifest_dict["regions"] = region_block(7.2, 0.4, 7.45)
    config = DoubleBodyCaseConfig(
        manifest=HullManifest.from_dict(manifest_dict),
        stl_path=write_stl(tmp_path / "synthetic_hull.stl", "hull"),
        velocity=VELOCITY,
        ranks=RANKS,
        appendages=(
            SurfaceRegion(
                name="rudder",
                stl_path=write_stl(tmp_path / "rudder.stl", "rudder"),
            ),
        ),
    )
    case = build_double_body_case(config, tmp_path / "db")
    assert (case / "constant/triSurface/rudder.stl").is_file()
    assert "rudder" in read(case, "system/snappyHexMeshDict")
    control = read(case, "system/controlDict")
    assert re.search(r"patches\s+\(hull rudder\)", control)
    assert re.search(rf"^\s*{PATCH_FORCES_PREFIX}rudder\s*$", control, re.M)

    provenance = json.loads(read(case, "case_provenance.json"))
    assert provenance["surfaces"]["aref_source"] == (
        "regions.union.wetted_surface_external_m2"
    )
    assert provenance["force_reference"]["a_ref"] == pytest.approx(7.45 / 2.0)


def _brace_block(text: str, key: str) -> str:
    """The ``{ ... }`` that follows the dictionary entry ``key``.

    Matched on the entry, not on the substring: ``layers`` also occurs inside
    a comment about buffer layers and inside ``nSurfaceLayers``, and the first
    of those is 100 lines from the block that was wanted.
    """
    match = re.search(rf"^\s*{re.escape(key)}\s*$", text, re.M)
    assert match, f"no dictionary entry named {key!r}"
    start = text.index("{", match.end())
    depth = 0
    for i in range(start, len(text)):
        if text[i] == "{":
            depth += 1
        elif text[i] == "}":
            depth -= 1
            if depth == 0:
                return text[start : i + 1]
    raise AssertionError(f"unbalanced braces after {key!r}")


def _with_regions(md: Dict[str, Any]) -> Dict[str, Any]:
    """A manifest that DECLARES a rudder, so its reference area counts one."""
    out = dict(md)
    out["regions"] = {
        "regions": [
            {"name": "hull", "n_triangles": 12, "watertight": True},
            {"name": "rudder", "n_triangles": 12, "watertight": True},
        ],
        "union": {
            "external_wetted_surface_m2": 130.0,
            "wetted_surface_naive_sum_m2": 132.0,
            "undecided_m2": 0.0,
        },
    }
    return out


def test_appendage_inclusive_aref_on_a_hull_only_mesh_is_refused(
        tmp_path: Path, manifest_dict: Dict[str, Any]):
    """Aref reads the MANIFEST; the geometry reads the CONFIG.

    Nothing connects them, so supplying one without the other builds cleanly,
    meshes cleanly, and divides every coefficient by an area covering surfaces
    the mesher never met. On the real hull that is a silent 1.4% error. The
    combination is never correct, so it is refused at construction.
    """
    with pytest.raises(ValueError, match="mesh never sees"):
        HullCaseConfig(
            manifest=HullManifest.from_dict(_with_regions(manifest_dict)),
            stl_path=write_stl(tmp_path / "synthetic_hull.stl", "hull"),
            velocity=VELOCITY,
            ranks=RANKS,
        )


def test_supplying_the_declared_appendage_is_accepted(
        tmp_path: Path, manifest_dict: Dict[str, Any]):
    """The guard must not fire on the correct case -- otherwise it is not a
    guard, it is a ban on appendages."""
    HullCaseConfig(
        manifest=HullManifest.from_dict(_with_regions(manifest_dict)),
        stl_path=write_stl(tmp_path / "synthetic_hull.stl", "hull"),
        velocity=VELOCITY,
        ranks=RANKS,
        appendages=(SurfaceRegion(
            name="rudder", stl_path=write_stl(tmp_path / "rudder.stl", "rudder")),),
    )


def test_every_mesh_patch_has_a_patchfield_in_every_field(two_region_case):
    """snappyHexMesh creates a patch per region; a field lacking an entry for
    one aborts with `Cannot find patchField entry for <name>` -- at setFields,
    which is AFTER the mesh is built, so the failure costs a full meshing run.

    Asserted as an agreement between two sets rather than as the presence of a
    named string, so adding a third region cannot pass by accident.
    """
    config, out = two_region_case
    case = build_hull_case(config, out / "fields")
    expected = {r.name for r in config.surface_regions}
    for field in sorted((case / "0.orig").glob("*")):
        text = field.read_text()
        present = {n for n in expected
                   if re.search(rf"^\s*{re.escape(n)}\s*$", text, re.M)}
        assert present == expected, (
            f"{field.name}: mesh will have {sorted(expected)}, "
            f"field declares {sorted(present)}"
        )


def test_the_appendage_inherits_the_hull_condition_rather_than_restating_it(
        two_region_case):
    """Copied, not restated: a change to the hull's boundary condition must
    not leave the appendages silently on the old one."""
    config, out = two_region_case
    case = build_hull_case(config, out / "inherit")
    alpha = (case / "0.orig" / "alpha.water").read_text()
    hull_type = re.search(r"^\s*hull\s*$\s*\{[^}]*?type\s+(\w+)", alpha, re.M | re.S)
    rud_type = re.search(r"^\s*rudder\s*$\s*\{[^}]*?type\s+(\w+)", alpha, re.M | re.S)
    assert hull_type and rud_type
    assert hull_type.group(1) == rud_type.group(1)
