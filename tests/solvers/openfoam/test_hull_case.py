"""
ABOUTME: The emitted case for an arbitrary client hull (#2023). Every assertion
reads a dictionary the builder actually wrote -- no solver runs here, and none
is needed: the defects this lane exists to prevent are all visible in the text.

The four scale bombs in the frozen KCS templates, restated as the properties
the emitted case must have:

  setFieldsDict     box (-999 ...) clips any domain wider than 999 m
  decomposeParDict  numberOfSubdomains 8 is untokenised, so a ranks setting
                    never arrives and decomposePar is fatal AFTER meshing
  snappyHexMeshDict maxGlobalCells 2000000 truncates silently
  0.orig/{k,omega,nut}  DTC's inlet turbulence, at DTC's condition
"""

from __future__ import annotations

import json
import math
import re
from pathlib import Path

import pytest

from digitalmodel.solvers.openfoam.hull_case import (
    HullCaseConfig,
    build_hull_case,
    hull_case_templates_dir,
)
from digitalmodel.solvers.openfoam.hull_case_physics import (
    CellBudgetError,
    decomposition_vector,
    derive_cell_budget,
    derive_inlet_turbulence,
)
from digitalmodel.solvers.openfoam.hull_domain import (
    block_divisions,
    build_hull_domain,
    refinement_boxes,
)
from digitalmodel.solvers.openfoam.hull_manifest import HullManifest

from .conftest import scaled_manifest_dict

SPEED = 2.0
RANKS = 16


# --------------------------------------------------------------------------- #
#  Helpers -- read the emitted dictionaries, ignoring comments
# --------------------------------------------------------------------------- #

def _strip_comments(text: str) -> str:
    """Drop // comments.

    These templates DOCUMENT the defects they guard against, verbatim. A naive
    substring search matches the explanation and passes while the setting does
    the wrong thing -- the same trap ``test_forces_density_source`` calls out.
    """
    text = re.sub(r"/\*.*?\*/", "", text, flags=re.S)
    return "\n".join(
        ln for ln in text.splitlines() if not ln.strip().startswith("//")
    )


def _entry(text: str, key: str) -> str:
    m = re.search(rf"^\s*{re.escape(key)}\s+(.+?);", _strip_comments(text), re.M)
    assert m, f"no entry {key!r} in dictionary"
    return m.group(1).strip()


def _numbers(text: str) -> list[float]:
    return [float(t) for t in re.findall(r"-?\d+\.?\d*(?:[eE][-+]?\d+)?", text)]


def _block(text: str, name: str) -> str:
    """The named sub-dictionary body, comments stripped."""
    stripped = _strip_comments(text)
    start = stripped.index(name)
    depth, i = 0, stripped.index("{", start)
    for j in range(i, len(stripped)):
        if stripped[j] == "{":
            depth += 1
        elif stripped[j] == "}":
            depth -= 1
            if depth == 0:
                return stripped[i + 1 : j]
    raise AssertionError(f"unbalanced braces around {name!r}")


# --------------------------------------------------------------------------- #
#  Fixtures
# --------------------------------------------------------------------------- #

@pytest.fixture
def manifest(manifest_dict) -> HullManifest:
    return HullManifest.from_dict(manifest_dict)


@pytest.fixture
def config(manifest, stl_file) -> HullCaseConfig:
    return HullCaseConfig(
        manifest=manifest,
        stl_path=stl_file,
        velocity=SPEED,
        ranks=RANKS,
        name="client_hull_resistance",
    )


@pytest.fixture
def case(config, tmp_path) -> Path:
    return build_hull_case(config, tmp_path / "cases")


def read(case: Path, rel: str) -> str:
    path = case / rel
    assert path.is_file(), f"missing {rel} in the emitted case"
    return path.read_text()


# --------------------------------------------------------------------------- #
#  Structure
# --------------------------------------------------------------------------- #

def test_the_case_is_structurally_complete(case, config) -> None:
    for sub in ("system", "constant", "0", "0.orig", "constant/triSurface"):
        assert (case / sub).is_dir(), f"missing {sub}/"
    for rel in (
        "system/controlDict",
        "system/blockMeshDict",
        "system/snappyHexMeshDict",
        "system/setFieldsDict",
        "system/decomposeParDict",
        "system/surfaceFeatureExtractDict",
        "system/fvSchemes",
        "system/fvSolution",
        "constant/transportProperties",
        "constant/turbulenceProperties",
        "constant/g",
        "constant/hRef",
        "0.orig/U",
        "0.orig/k",
        "0.orig/omega",
        "0.orig/nut",
        "0.orig/alpha.water",
        "0.orig/p_rgh",
    ):
        assert (case / rel).is_file(), f"missing {rel}"
    for i in range(1, 7):
        assert (case / f"system/topoSetDict.{i}").is_file()


def test_the_surface_is_copied_in_and_named_consistently(case, stl_file) -> None:
    """surfaceFeatureExtract names its output after the surface it was given.

    snappyHexMesh then reads that name back. Two literals drift; one derivation
    cannot. Getting it wrong aborts snappy only AFTER blockMesh, the extraction
    and six topoSet/refineMesh pairs have run.
    """
    assert (case / "constant/triSurface" / stl_file.name).is_file()
    snappy = read(case, "system/snappyHexMeshDict")
    extract = read(case, "system/surfaceFeatureExtractDict")
    assert stl_file.name in snappy
    assert stl_file.name in extract
    stem = Path(stl_file.name).stem
    assert f'"{stem}.eMesh"' in snappy


def test_no_token_survives_substitution(case) -> None:
    for path in sorted(case.rglob("*")):
        if path.is_file() and path.suffix != ".stl":
            leftover = re.findall(r"@[A-Z0-9_]+@", path.read_text())
            assert not leftover, f"{path.name}: {sorted(set(leftover))}"


def test_no_kcs_or_dtc_identity_reaches_the_emitted_case(case) -> None:
    for path in sorted(case.rglob("*")):
        if not path.is_file() or path.suffix == ".stl":
            continue
        text = path.read_text()
        for needle in ("kcs", "KCS", "DTC", "DTCHull"):
            assert needle not in text, f"{needle!r} survived in {path.name}"


# --------------------------------------------------------------------------- #
#  Scale bomb 1 -- setFieldsDict
# --------------------------------------------------------------------------- #

def test_setfields_box_is_the_domain_not_a_999_literal(case, manifest) -> None:
    text = read(case, "system/setFieldsDict")
    # Comment-stripped: the template DOCUMENTS the literal it replaced, and a
    # naive substring check would match the explanation.
    settings = _strip_comments(text)
    assert "999" not in settings
    assert "999" in text, "the reasoning was removed with the literal"
    domain = build_hull_domain(manifest)

    boxes = re.findall(r"box\s+\(([^)]*)\)\s*\(([^)]*)\)", settings)
    assert len(boxes) == 2, "boxToCell and boxToFace"
    for lo_s, hi_s in boxes:
        lo, hi = _numbers(lo_s), _numbers(hi_s)
        assert lo[0] <= domain.x_outlet and hi[0] >= domain.x_inlet
        assert lo[1] <= domain.y_side and hi[1] >= 0.0
        assert lo[2] <= domain.z_levels[0]
        assert hi[2] == pytest.approx(domain.waterline)


def test_setfields_box_covers_a_full_scale_domain(manifest_dict, stl_file, tmp_path):
    """A 999 m literal clips any full-scale hull; a 300 m ship is 2 km of domain.

    The failure is not a crash. alpha.water is initialised to zero outside the
    box, so the far field starts as air, and the solve reports a resistance for
    a ship falling through the sky.
    """
    big = HullManifest.from_dict(scaled_manifest_dict(50.0))  # Lpp = 300 m
    case = build_hull_case(
        HullCaseConfig(
            manifest=big, stl_path=stl_file, velocity=12.0, ranks=RANKS,
            name="full_scale",
        ),
        tmp_path,
    )
    domain = build_hull_domain(big)
    assert domain.length > 999.0, "fixture no longer exercises the clip"
    lo_s, hi_s = re.findall(
        r"box\s+\(([^)]*)\)\s*\(([^)]*)\)",
        _strip_comments(read(case, "system/setFieldsDict")),
    )[0]
    assert _numbers(lo_s)[0] <= domain.x_outlet
    assert _numbers(hi_s)[0] >= domain.x_inlet


# --------------------------------------------------------------------------- #
#  Scale bomb 2 -- decomposeParDict
# --------------------------------------------------------------------------- #

def test_number_of_subdomains_is_the_requested_rank_count(case) -> None:
    text = read(case, "system/decomposeParDict")
    assert int(_entry(text, "numberOfSubdomains")) == RANKS


def test_hierarchical_n_multiplies_to_the_rank_count(case) -> None:
    """prod(n) != numberOfSubdomains is fatal AFTER the mesh exists."""
    text = read(case, "system/decomposeParDict")
    n = [int(v) for v in _numbers(_entry(_block(text, "coeffs"), "n"))]
    assert len(n) == 3
    assert math.prod(n) == RANKS
    assert tuple(n) == decomposition_vector(RANKS)


def test_the_frozen_eight_does_not_survive(case) -> None:
    text = read(case, "system/decomposeParDict")
    assert int(_entry(text, "numberOfSubdomains")) != 8


# --------------------------------------------------------------------------- #
#  Scale bomb 3 -- snappyHexMeshDict maxGlobalCells
# --------------------------------------------------------------------------- #

def test_max_global_cells_is_derived_from_the_domain(case, manifest) -> None:
    domain = build_hull_domain(manifest)
    budget = derive_cell_budget(
        domain, refinement_boxes(manifest, domain), block_divisions(domain),
        ranks=RANKS,
    )
    text = read(case, "system/snappyHexMeshDict")
    assert int(_entry(text, "maxGlobalCells")) == budget.max_global_cells
    assert int(_entry(text, "maxGlobalCells")) != 2000000
    assert int(_entry(text, "maxLocalCells")) == budget.max_local_cells


def test_a_cap_below_the_requirement_fails_the_BUILD(config, tmp_path, manifest):
    """Not the mesher, and not the solve: the build."""
    domain = build_hull_domain(manifest)
    budget = derive_cell_budget(
        domain, refinement_boxes(manifest, domain), block_divisions(domain),
        ranks=RANKS,
    )
    capped = config.replace(max_global_cells=budget.estimated_cells // 4)
    with pytest.raises(CellBudgetError, match="(?i)truncat"):
        build_hull_case(capped, tmp_path / "should_not_exist")


def test_nothing_is_written_when_the_budget_check_fails(config, tmp_path, manifest):
    domain = build_hull_domain(manifest)
    budget = derive_cell_budget(
        domain, refinement_boxes(manifest, domain), block_divisions(domain),
        ranks=RANKS,
    )
    out = tmp_path / "aborted"
    with pytest.raises(CellBudgetError):
        build_hull_case(config.replace(max_global_cells=budget.estimated_cells // 4), out)
    assert not (out / config.name).exists()


# --------------------------------------------------------------------------- #
#  Scale bomb 4 -- 0.orig/{k,omega,nut}
# --------------------------------------------------------------------------- #

def test_inlet_turbulence_is_derived_from_the_condition(case, config) -> None:
    t = derive_inlet_turbulence(
        config.velocity,
        config.manifest.lpp_m,
        config.nu,
        intensity=config.turbulence_intensity,
    )
    for field, value in (("k", t.k), ("omega", t.omega), ("nut", t.nut)):
        emitted = float(_entry(read(case, f"0.orig/{field}"), "internalField").split()[-1])
        assert emitted == pytest.approx(value, rel=1e-5), field


def test_the_dtc_turbulence_literals_do_not_survive(case) -> None:
    assert "0.00015" not in read(case, "0.orig/k")
    assert _entry(read(case, "0.orig/omega"), "internalField") != "uniform 2"
    assert "5e-07" not in read(case, "0.orig/nut")


def test_inlet_turbulence_moves_with_the_speed(config, tmp_path) -> None:
    slow = build_hull_case(config.replace(velocity=1.0), tmp_path / "slow")
    fast = build_hull_case(config.replace(velocity=4.0), tmp_path / "fast")
    k_slow = float(_entry(read(slow, "0.orig/k"), "internalField").split()[-1])
    k_fast = float(_entry(read(fast, "0.orig/k"), "internalField").split()[-1])
    assert k_fast == pytest.approx(16.0 * k_slow, rel=1e-4)


# --------------------------------------------------------------------------- #
#  The density-source defect -- learned the hard way on the KCS hull
# --------------------------------------------------------------------------- #

def test_forces_uses_the_vof_density_field(case) -> None:
    """`rho rho`, never `rho rhoInf`.

    With a constant rhoInf the DRY topsides are integrated at water density.
    Measured on KCS: 48.4% of the hull patch sits above the waterline and
    supplied 62.3% of the reported viscous force, putting Ct +113.79% out.
    """
    body = _block(read(case, "system/controlDict"), "forces")
    assert re.search(r"^\s*rho\s+rho\s*;", body, re.M)
    assert not re.search(r"^\s*rho\s+rhoInf\s*;", body, re.M)
    assert not re.search(r"^\s*rhoInf\s", body, re.M), "ambiguous density source"


def test_forcecoeffs_needs_BOTH_the_field_and_a_reference_density(case, config):
    """The force integral takes the field; the coefficient denominator takes
    the constant. One entry each, and they are not interchangeable."""
    body = _block(read(case, "system/controlDict"), "forceCoeffs")
    assert re.search(r"^\s*rho\s+rho\s*;", body, re.M), "force integral"
    assert re.search(r"^\s*rhoInf\s+[\d.eE+-]+\s*;", body, re.M), "denominator"
    assert float(_entry(body, "rhoInf")) == pytest.approx(config.density)


def test_forcecoeffs_reference_values_come_from_the_manifest(case, config) -> None:
    body = _block(read(case, "system/controlDict"), "forceCoeffs")
    assert float(_entry(body, "magUInf")) == pytest.approx(config.velocity, rel=1e-5)
    assert float(_entry(body, "lRef")) == pytest.approx(
        config.manifest.lpp_m, rel=1e-5
    )
    assert float(_entry(body, "Aref")) == pytest.approx(
        config.manifest.wetted_surface_m2 / 2.0, rel=1e-5
    )
    cofr = _numbers(_entry(body, "CofR"))
    assert cofr[2] == pytest.approx(config.manifest.draft_m, rel=1e-5)


def test_the_density_source_reasoning_survives_in_the_emitted_case(case) -> None:
    """A bare `rho rho` invites a future editor to 'restore' rhoInf."""
    text = read(case, "system/controlDict")
    assert re.search(r"above[- ]water", text, re.I)


# --------------------------------------------------------------------------- #
#  Geometry reached the dictionaries
# --------------------------------------------------------------------------- #

def test_blockmesh_vertices_are_the_derived_domain(case, manifest) -> None:
    domain = build_hull_domain(manifest)
    text = _strip_comments(read(case, "system/blockMeshDict"))
    verts = [_numbers(v) for v in re.findall(r"\(\s*([-\d.eE+ ]+)\)", text)][:28]
    xs = sorted({round(v[0], 9) for v in verts})
    ys = sorted({round(v[1], 9) for v in verts})
    zs = sorted({round(v[2], 9) for v in verts})
    assert xs == pytest.approx([domain.x_outlet, domain.x_inlet])
    assert ys == pytest.approx([domain.y_side, 0.0])
    assert zs == pytest.approx(list(domain.z_levels))


def test_blockmesh_divisions_are_the_derived_counts(case, manifest) -> None:
    div = block_divisions(build_hull_domain(manifest))
    text = _strip_comments(read(case, "system/blockMeshDict"))
    hexes = re.findall(r"hex\s*\([^)]*\)\s*\(([^)]*)\)", text)
    assert len(hexes) == 6
    counts = [[int(v) for v in _numbers(h)] for h in hexes]
    assert [c[0] for c in counts] == [div["nx"]] * 6
    assert [c[1] for c in counts] == [div["ny"]] * 6
    assert [c[2] for c in counts] == [
        div["nza"], div["nza"], div["nzb"], div["nzb"], div["nzc"], div["nzd"]
    ]


def test_toposet_boxes_are_the_derived_refinement_boxes(case, manifest) -> None:
    domain = build_hull_domain(manifest)
    boxes = refinement_boxes(manifest, domain)
    for i, (lo, hi) in enumerate(boxes, start=1):
        text = _strip_comments(read(case, f"system/topoSetDict.{i}"))
        lo_s, hi_s = re.findall(r"box\s+\(([^)]*)\)\s*\(([^)]*)\)", text)[0]
        assert _numbers(lo_s) == pytest.approx(list(lo), rel=1e-5)
        assert _numbers(hi_s) == pytest.approx(list(hi), rel=1e-5)


def test_href_and_transport_properties_carry_the_condition(case, config) -> None:
    assert float(_entry(read(case, "constant/hRef"), "value")) == pytest.approx(
        config.manifest.draft_m, rel=1e-5
    )
    tp = _strip_comments(read(case, "constant/transportProperties"))
    assert float(_entry(tp, "nu").split()[-1]) == pytest.approx(config.nu, rel=1e-5)
    assert float(_entry(tp, "rho").split()[-1]) == pytest.approx(
        config.density, rel=1e-5
    )


def test_inlet_velocity_is_the_requested_speed(case, config) -> None:
    text = read(case, "0.orig/U")
    assert float(_entry(text, "Umean")) == pytest.approx(config.velocity, rel=1e-5)


def test_location_in_mesh_is_the_derived_point(case, manifest) -> None:
    domain = build_hull_domain(manifest)
    text = read(case, "system/snappyHexMeshDict")
    assert _numbers(_entry(text, "locationInMesh")) == pytest.approx(
        list(domain.location_in_mesh), rel=1e-5
    )


# --------------------------------------------------------------------------- #
#  Provenance
# --------------------------------------------------------------------------- #

def test_provenance_records_every_derived_quantity(case, config) -> None:
    prov = json.loads(read(case, "case_provenance.json"))
    assert prov["hull"]["source_sha256"] == config.manifest.source_sha256
    assert prov["hull"]["lpp_m"] == pytest.approx(config.manifest.lpp_m)
    assert prov["condition"]["velocity_m_s"] == pytest.approx(config.velocity)
    assert prov["condition"]["froude"] > 0
    assert prov["condition"]["reynolds"] > 0
    assert prov["decomposition"]["ranks"] == RANKS
    assert prov["mesh"]["max_global_cells"] > 0
    assert prov["turbulence"]["k"] > 0
    assert prov["force_reference"]["a_ref"] > 0
    # The Reynolds-scaled requirement the relative-size mesher cannot see.
    assert prov["mesh"]["wall_normal_first_cell_height_m"] > 0


# --------------------------------------------------------------------------- #
#  Scale sanity
# --------------------------------------------------------------------------- #

def test_a_ten_times_larger_hull_emits_a_ten_times_larger_case(
    manifest_dict, stl_file, tmp_path
) -> None:
    """Ten times the hull, the same target cell size.

    Domain extents grow by 10 in every direction; the cell budget follows the
    volume. A scale-invariance bug is invisible on one hull.
    """
    small = HullManifest.from_dict(manifest_dict)
    big = HullManifest.from_dict(scaled_manifest_dict(10.0))
    h = small.lpp_m / 6.3

    def emit(m, name):
        return build_hull_case(
            HullCaseConfig(
                manifest=m, stl_path=stl_file, velocity=SPEED, ranks=RANKS,
                name=name, base_cell_size=h,
            ),
            tmp_path,
        )

    cs, cb = emit(small, "small"), emit(big, "big")

    def extents(case):
        text = _strip_comments(read(case, "system/blockMeshDict"))
        verts = [_numbers(v) for v in re.findall(r"\(\s*([-\d.eE+ ]+)\)", text)][:28]
        return (
            max(v[0] for v in verts) - min(v[0] for v in verts),
            max(v[1] for v in verts) - min(v[1] for v in verts),
            max(v[2] for v in verts) - min(v[2] for v in verts),
        )

    es, eb = extents(cs), extents(cb)
    assert eb == pytest.approx([e * 10.0 for e in es], rel=1e-6)

    def cells(case):
        return int(_entry(read(case, "system/snappyHexMeshDict"), "maxGlobalCells"))

    assert cells(cb) > cells(cs) * 100


def test_the_same_hull_at_two_rank_counts_differs_only_in_decomposition(
    config, tmp_path
) -> None:
    a = build_hull_case(config.replace(ranks=8, name="r8"), tmp_path)
    b = build_hull_case(config.replace(ranks=32, name="r32"), tmp_path)
    assert read(a, "system/blockMeshDict") == read(b, "system/blockMeshDict")
    assert int(_entry(read(a, "system/decomposeParDict"), "numberOfSubdomains")) == 8
    assert int(_entry(read(b, "system/decomposeParDict"), "numberOfSubdomains")) == 32


# --------------------------------------------------------------------------- #
#  The KCS path must be untouched
# --------------------------------------------------------------------------- #

def test_the_hull_templates_are_a_separate_tree_from_the_frozen_kcs_ones() -> None:
    """Additive: the KCS templates are diffed against the shipped DTC tutorial
    by ``test_emitted_case_matches_dtc_tutorial_modulo_declared_deviations``.
    Parameterising them in place would break that gate, so this lane owns its
    own tree and the KCS one is left exactly as it is."""
    hull = hull_case_templates_dir()
    assert hull.name == "hull_resistance"
    kcs = hull.parent / "ship_resistance"
    assert kcs.is_dir()
    assert "maxGlobalCells 2000000" in (kcs / "system/snappyHexMeshDict").read_text()
    assert "numberOfSubdomains 8" in (kcs / "system/decomposeParDict").read_text()
    assert "-999" in (kcs / "system/setFieldsDict").read_text()


def test_the_kcs_emitter_still_builds(tmp_path) -> None:
    """The KCS gates must keep passing; this lane is additive."""
    from digitalmodel.solvers.openfoam.validation.ship_resistance import (
        build_ship_resistance_case,
    )

    case = build_ship_resistance_case(parent_dir=tmp_path)
    assert (case / "system" / "controlDict").is_file()
