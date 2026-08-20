"""
ABOUTME: The emitted DOUBLE-BODY case (#2023). A double-body run replaces the
free surface with a symmetry plane at the waterline, so it generates no waves
and reports viscous resistance alone -- which is the only thing a form factor
can be extracted from. It is the cheap gate that can void a resistance
programme in hours instead of weeks, so what it emits has to be checkable
without a solver.

THE INVERSION THESE TESTS EXIST TO PIN. In the VOF sibling the forces function
object MUST name the density FIELD (``rho rho``); a constant there integrates
the dry topsides at water density and put a benchmark Ct +113.79% out. Here
the same setting is wrong in the other direction and fatally so: simpleFoam is
incompressible, there is no density field to resolve, and the mesh stops at
the waterline so there is no dry surface to over-count. ``rho rhoInf`` is
correct HERE and wrong THERE, and the fact that distinguishes the two cases is
the number of phases each transports -- never its directory name.

WHAT MUST NOT BE ENFORCED. ``hull_free_surface`` refuses a mesh that resolves
fewer than 80 cells per wavelength. There is no wavelength here. The criterion
is stood down, and the provenance says it was stood down deliberately rather
than leaving its absence to be discovered.
"""

from __future__ import annotations

import json
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
    derive_inlet_turbulence,
)
from digitalmodel.solvers.openfoam.hull_domain import (
    FREEBOARD_LPP,
    HullDomainError,
    build_hull_domain,
)
from digitalmodel.solvers.openfoam.hull_double_body import (
    DoubleBodyCaseConfig,
    DoubleBodyCaseError,
    build_double_body_case,
    build_double_body_case_from_files,
    derive_double_body_case,
    double_body_templates_dir,
)
from digitalmodel.solvers.openfoam.hull_double_body_domain import (
    build_double_body_domain,
)
from digitalmodel.solvers.openfoam.hull_manifest import HullManifest

from .conftest import scaled_manifest_dict

SPEED = 2.0
#: Slow enough that the free-surface ladder at this level misses the
#: cells-per-wavelength criterion. lambda = 2 pi V^2 / g, so halving the speed
#: quarters the requirement's headroom.
SLOW = 1.0
RANKS = 16

#: Dictionaries that carry NO free-surface content and must therefore stay
#: identical in the two trees. Listed rather than globbed: adding a file to
#: this set is a decision that the two case types share it.
SHARED_DICTS = (
    "system/decomposeParDict",
    "system/meshQualityDict",
    "system/refineMeshDict",
    "system/snappyHexMeshDict",
    "system/surfaceFeatureExtractDict",
    "system/topoSetDict.1",
    "system/topoSetDict.2",
    "system/topoSetDict.3",
    "system/topoSetDict.4",
    "system/topoSetDict.5",
    "system/topoSetDict.6",
    "constant/turbulenceProperties",
)


# --------------------------------------------------------------------------- #
#  Helpers -- read the emitted dictionaries, ignoring comments
# --------------------------------------------------------------------------- #

def _strip_comments(text: str) -> str:
    """Drop comments. These templates DOCUMENT the defect they prevent, so a
    substring search over raw text matches the explanation and passes while
    the configuration does the wrong thing."""
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


def _delimited(text: str, name: str, open_ch: str, close_ch: str) -> str:
    stripped = _strip_comments(text)
    start = stripped.index(name)
    depth, i = 0, stripped.index(open_ch, start)
    for j in range(i, len(stripped)):
        if stripped[j] == open_ch:
            depth += 1
        elif stripped[j] == close_ch:
            depth -= 1
            if depth == 0:
                return stripped[i + 1 : j]
    raise AssertionError(f"unbalanced {open_ch}{close_ch} around {name!r}")


def _block(text: str, name: str) -> str:
    """The named sub-dictionary body, comments stripped."""
    return _delimited(text, name, "{", "}")


def _list(text: str, name: str) -> str:
    """The named parenthesised list body. ``blocks``, ``vertices`` and
    ``boundary`` are lists, not dictionaries, and reading them with the
    dictionary helper silently returns the first sub-dictionary inside."""
    return _delimited(text, name, "(", ")")


def read(case: Path, rel: str) -> str:
    path = case / rel
    assert path.is_file(), f"missing {rel} in the emitted case"
    return path.read_text()


# --------------------------------------------------------------------------- #
#  Fixtures
# --------------------------------------------------------------------------- #

@pytest.fixture
def manifest(manifest_dict) -> HullManifest:
    return HullManifest.from_dict(manifest_dict)


@pytest.fixture
def config(manifest, stl_file) -> DoubleBodyCaseConfig:
    return DoubleBodyCaseConfig(
        manifest=manifest,
        stl_path=stl_file,
        velocity=SPEED,
        ranks=RANKS,
        name="client_double_body",
    )


@pytest.fixture
def case(config, tmp_path) -> Path:
    return build_double_body_case(config, tmp_path / "cases")


# --------------------------------------------------------------------------- #
#  There is no free surface, and the case says so structurally
# --------------------------------------------------------------------------- #

def test_the_case_carries_no_free_surface_machinery(case) -> None:
    """Each of these files exists ONLY to run a free surface. Their absence is
    the case's own statement that it has none; a leftover would mean the
    variant was configured by editing rather than by construction."""
    for absent in (
        "constant/g",
        "constant/hRef",
        "system/setFieldsDict",
        "0.orig/alpha.water",
        "0.orig/p_rgh",
        "0.orig/pointDisplacement",
    ):
        assert not (case / absent).exists(), f"{absent} survived into a double-body case"


def test_gravity_is_off_because_there_is_no_g_to_read(case) -> None:
    """simpleFoam never reads ``constant/g``. Absence is not an oversight
    here: with one phase of uniform density the buoyant term is a constant
    gradient the kinematic pressure absorbs, which is also why there is no
    p_rgh split to make."""
    assert not (case / "constant" / "g").exists()
    assert _entry(read(case, "system/controlDict"), "application") == "simpleFoam"
    prov = json.loads(read(case, "case_provenance.json"))
    assert prov["free_surface"]["gravity_enabled"] is False
    assert prov["free_surface"]["present"] is False
    assert prov["free_surface"]["phases"] == 1


def test_the_case_is_single_phase_by_the_same_evidence_the_guard_reads(case) -> None:
    """The forces-density guard classifies a case from two structural facts.
    Both must be readable off THIS case, or the guard silently stops covering
    it -- and an unclassifiable case is skipped, which looks like a pass."""
    transport = _strip_comments(read(case, "constant/transportProperties"))
    assert not re.search(r"^\s*phases\s*\(", transport, re.M)
    assert not re.search(r"^\s*rho\s", transport, re.M), (
        "simpleFoam is incompressible and never asks for a density; a rho "
        "entry here means the density has two homes and can disagree with "
        "itself"
    )
    assert list((case / "0.orig").glob("alpha.*")) == []


def test_the_pressure_field_is_kinematic(case) -> None:
    """``p``, not ``p_rgh``, and m^2/s^2. Every force this case reports is
    kinematic until rhoInf multiplies it -- which is why rhoInf is required
    rather than forbidden."""
    p = read(case, "0.orig/p")
    assert _numbers(_entry(p, "dimensions")) == [0, 2, -2, 0, 0, 0, 0]


# --------------------------------------------------------------------------- #
#  THE INVERSION
# --------------------------------------------------------------------------- #

def test_forces_integrate_at_the_constant_density(case) -> None:
    """The inverse of the VOF rule, and correct for the same reason the VOF
    rule is: the setting must name the density the case actually has."""
    body = _block(read(case, "system/controlDict"), "forces")
    assert re.search(r"^\s*rho\s+rhoInf\s*;", body, re.M)
    assert not re.search(r"^\s*rho\s+rho\s*;", body, re.M), (
        "`rho rho` names a VOF density field that simpleFoam does not create"
    )
    assert float(_entry(body, "rhoInf")) == pytest.approx(998.2)


def test_the_vof_sibling_still_integrates_on_the_density_field(
    manifest, stl_file, tmp_path
) -> None:
    """The inversion is only meaningful if the other side stayed put. Building
    both cases here means a change that 'harmonised' the two would fail."""
    vof = build_hull_case(
        HullCaseConfig(
            manifest=manifest, stl_path=stl_file, velocity=SPEED, ranks=RANKS
        ),
        tmp_path,
    )
    body = _block(read(vof, "system/controlDict"), "forces")
    assert re.search(r"^\s*rho\s+rho\s*;", body, re.M)
    assert not re.search(r"^\s*rho\s+rhoInf\s*;", body, re.M)


def test_the_reason_for_the_inversion_survives_in_the_template(case) -> None:
    """A bare `rho rhoInf` looks like the defect it is not, and invites a
    future editor to 'fix' it into the defect it would be."""
    text = read(case, "system/controlDict")
    assert "rho rho" in text, "the VOF counterpart is not named"
    assert re.search(r"single[- ]phase", text, re.I)
    assert re.search(r"incompressible", text, re.I)


# --------------------------------------------------------------------------- #
#  Domain: truncated at the waterline, symmetry plane there
# --------------------------------------------------------------------------- #

def test_the_waterline_is_a_symmetry_plane(case) -> None:
    boundary = _list(read(case, "system/blockMeshDict"), "boundary")
    assert _entry(_block(boundary, "waterline"), "type") == "symmetryPlane"


def test_every_far_field_boundary_keeps_its_type(case) -> None:
    """The waterline REPLACES the atmosphere patch; it does not join it. A
    `patch` at the top would admit inflow and stop being a symmetry
    condition."""
    boundary = _list(read(case, "system/blockMeshDict"), "boundary")
    expected = {
        "waterline": "symmetryPlane",
        "bottom": "symmetryPlane",
        "side": "symmetryPlane",
        "midPlane": "symmetryPlane",
        "inlet": "patch",
        "outlet": "patch",
    }
    for name, kind in expected.items():
        assert _entry(_block(boundary, name), "type") == kind, name
    assert "atmosphere" not in boundary


def test_the_domain_stops_at_the_waterline(case, manifest) -> None:
    prov = json.loads(read(case, "case_provenance.json"))["domain"]
    assert prov["z_top"] == pytest.approx(manifest.draft_m)
    assert prov["waterline_m"] == pytest.approx(manifest.draft_m)
    assert prov["truncated_at_waterline"] is True
    assert len(prov["z_levels"]) == 3


def test_the_extents_are_the_free_surface_case_extents_minus_the_freeboard(
    manifest,
) -> None:
    """Same ITTC factors, same delegation, same box -- only the air is gone."""
    domain, reference = build_double_body_domain(
        manifest,
        upstream_lpp=2.0,
        downstream_lpp=4.5,
        lateral_lpp=3.0,
        depth_lpp=2.5,
        keel_clearance_drafts=4.0,
    )
    expected = build_hull_domain(manifest)
    assert domain.x_inlet == pytest.approx(expected.x_inlet)
    assert domain.x_outlet == pytest.approx(expected.x_outlet)
    assert domain.y_side == pytest.approx(expected.y_side)
    assert domain.base_cell_size == pytest.approx(expected.base_cell_size)
    assert domain.z_levels[0] == pytest.approx(expected.z_levels[0])
    assert domain.z_levels[1] == pytest.approx(expected.z_levels[1])
    assert domain.z_levels[2] == pytest.approx(expected.waterline)
    assert reference.z_levels == expected.z_levels
    assert domain.volume < reference.volume


def test_the_domain_scales_with_the_hull(stl_file, tmp_path) -> None:
    """Nothing inherited from a benchmark: a hull ten times larger gets a
    domain ten times larger and nothing else changes."""
    small = HullManifest.from_dict(scaled_manifest_dict(1.0))
    large = HullManifest.from_dict(scaled_manifest_dict(10.0))
    kw = dict(
        upstream_lpp=2.0,
        downstream_lpp=4.5,
        lateral_lpp=3.0,
        depth_lpp=2.5,
        keel_clearance_drafts=4.0,
    )
    a, _ = build_double_body_domain(small, **kw)
    b, _ = build_double_body_domain(large, **kw)
    assert b.length / a.length == pytest.approx(10.0)
    assert b.width / a.width == pytest.approx(10.0)
    for za, zb in zip(a.z_levels, b.z_levels):
        assert zb / za == pytest.approx(10.0)


def test_the_point_in_the_free_stream_is_inside_the_truncated_domain(
    case, config
) -> None:
    """locationInMesh decides which side of the hull snappy keeps. A point
    left above a waterline that no longer exists is outside the mesh, and
    snappyHexMesh's failure mode there is to keep the WRONG region."""
    point = _numbers(_entry(read(case, "system/snappyHexMeshDict"), "locationInMesh"))
    derivation = derive_double_body_case(config)
    z = derivation.domain.z_levels
    assert z[0] < point[2] < z[-1]
    assert derivation.domain.x_outlet < point[0] < derivation.domain.x_inlet
    assert derivation.domain.y_side < point[1] < 0.0


# --------------------------------------------------------------------------- #
#  The refinement staging, clipped
# --------------------------------------------------------------------------- #

def test_no_refinement_box_reaches_above_the_waterline(config) -> None:
    """The part of each box above the waterline enclosed air. Left in place it
    is not merely wasteful -- the innermost box is the hull's own bounding
    box, which on any hull with freeboard escapes a domain that stops at the
    waterline, and the staging check refuses it."""
    derivation = derive_double_body_case(config)
    top = derivation.domain.z_levels[-1]
    for lo, hi in derivation.boxes:
        assert lo[2] < hi[2] <= top + 1e-12


def test_the_clipped_boxes_are_still_nested(config) -> None:
    """Clamping is monotone, so nesting survives it. Asserted rather than
    assumed: a stage that escaped its parent makes a refinement shell negative
    in the cell estimate."""
    derivation = derive_double_body_case(config)
    for outer, inner in zip(derivation.boxes, derivation.boxes[1:]):
        for axis in range(3):
            assert outer[0][axis] <= inner[0][axis] + 1e-9, axis
            assert inner[1][axis] <= outer[1][axis] + 1e-9, axis


def test_the_emitted_toposet_boxes_are_the_clipped_ones(case, config) -> None:
    derivation = derive_double_body_case(config)
    for index, (lo, hi) in enumerate(derivation.boxes, start=1):
        box = _entry(read(case, f"system/topoSetDict.{index}"), "box")
        emitted = _numbers(box)
        assert emitted[:3] == pytest.approx(list(lo), rel=1e-9)
        assert emitted[3:] == pytest.approx(list(hi), rel=1e-9)


# --------------------------------------------------------------------------- #
#  The vertical stack and the cell budget
# --------------------------------------------------------------------------- #

def test_two_blocks_stacked_between_the_floor_and_the_waterline(case) -> None:
    block_mesh = read(case, "system/blockMeshDict")
    assert _list(block_mesh, "blocks").count("hex") == 2
    assert _list(block_mesh, "vertices").count("(") == 12


def test_the_near_field_vertical_cell_is_the_free_surface_ladders(
    config,
) -> None:
    """The load-bearing choice in this module, and the reason it is not free
    to differ: a form factor is a ratio applied to a free-surface result. A
    boundary layer resolved differently by the two meshes lands in k and is
    indistinguishable from hull form."""
    derivation = derive_double_body_case(config)
    reference = derivation.reference_domain
    from digitalmodel.solvers.openfoam.hull_domain import block_divisions

    ladder = block_divisions(reference)
    ladder_cell = (reference.z_levels[2] - reference.z_levels[1]) / ladder["nza"]
    z = derivation.domain.z_levels
    emitted_cell = (z[2] - z[1]) / derivation.divisions["nznear"]
    assert emitted_cell <= ladder_cell
    assert emitted_cell == pytest.approx(ladder_cell, rel=0.02)
    assert derivation.divisions["nzdeep"] == ladder["nza"]
    assert derivation.divisions["nx"] == ladder["nx"]
    assert derivation.divisions["ny"] == ladder["ny"]


def test_the_background_count_is_the_exact_blockmesh_product(config) -> None:
    derivation = derive_double_body_case(config)
    div = derivation.divisions
    assert derivation.budget.background_cells == (
        div["nx"] * div["ny"] * (div["nzdeep"] + div["nznear"])
    )


def test_a_double_body_costs_fewer_cells_than_the_vof_case_at_the_same_level(
    manifest, stl_file, tmp_path
) -> None:
    """The reason this is a four-hour gate rather than a four-day one. The air
    column and the free-surface band are gone, and the in-plane refinement
    that resolves the wave is not applied."""
    vof = build_hull_case(
        HullCaseConfig(
            manifest=manifest, stl_path=stl_file, velocity=SPEED, ranks=RANKS
        ),
        tmp_path / "vof",
    )
    db = build_double_body_case(
        DoubleBodyCaseConfig(
            manifest=manifest, stl_path=stl_file, velocity=SPEED, ranks=RANKS
        ),
        tmp_path / "db",
    )
    vof_cells = json.loads(read(vof, "case_provenance.json"))["mesh"][
        "estimated_cells"
    ]
    db_cells = json.loads(read(db, "case_provenance.json"))["mesh"][
        "estimated_cells"
    ]
    assert db_cells < vof_cells


def test_a_cap_below_the_estimate_is_refused(config, tmp_path) -> None:
    """The same posture as the VOF path: snappyHexMesh does not fail on a
    cap, it stops refining and reports success."""
    with pytest.raises(CellBudgetError):
        build_double_body_case(config.replace(max_global_cells=1000), tmp_path)


# --------------------------------------------------------------------------- #
#  The wave criterion, deliberately not enforced
# --------------------------------------------------------------------------- #

def test_the_wave_criterion_is_not_enforced_and_the_case_says_so(case) -> None:
    fs = json.loads(read(case, "case_provenance.json"))["free_surface"]
    criterion = fs["cells_per_wavelength"]
    assert criterion["enforced"] is False
    assert "DELIBERATELY NOT ENFORCED" in criterion["reason"]
    assert "symmetry plane" in criterion["reason"]


def test_a_grid_the_vof_case_would_refuse_builds_here(
    manifest, stl_file, tmp_path
) -> None:
    """The criterion is the VOF case's, not this one's.

    The wavelength falls with the SQUARE of the speed, so the same stated
    background cell that resolves the wave at 2 m/s does not at 1 m/s -- and
    there the VOF builder refuses before writing anything. That refusal is
    correct and it does not transfer: enforcing it here would cost the square
    of a linear refinement factor, applied to every cell in the case, for a
    quantity a symmetry plane has removed.
    """
    coarse = manifest.lpp_m / 6.3
    from digitalmodel.solvers.openfoam.hull_free_surface import (
        FreeSurfaceResolutionError,
    )

    with pytest.raises(FreeSurfaceResolutionError):
        build_hull_case(
            HullCaseConfig(
                manifest=manifest,
                stl_path=stl_file,
                velocity=SLOW,
                ranks=RANKS,
                base_cell_size=coarse,
            ),
            tmp_path / "vof",
        )
    case = build_double_body_case(
        DoubleBodyCaseConfig(
            manifest=manifest,
            stl_path=stl_file,
            velocity=SLOW,
            ranks=RANKS,
            base_cell_size=coarse,
        ),
        tmp_path / "db",
    )
    assert (case / "system" / "blockMeshDict").is_file()


def test_stating_the_wave_criterion_on_a_double_body_is_refused(
    manifest, stl_file
) -> None:
    """A knob that is accepted and discarded is how a reviewer comes to
    believe a case was built to a specification it never saw."""
    with pytest.raises(DoubleBodyCaseError) as excinfo:
        DoubleBodyCaseConfig(
            manifest=manifest,
            stl_path=stl_file,
            velocity=SPEED,
            ranks=RANKS,
            free_surface_cells_per_wavelength=80.0,
        )
    assert "no wave" in str(excinfo.value)


def test_stating_a_freeboard_on_a_double_body_is_refused(
    manifest, stl_file
) -> None:
    with pytest.raises(DoubleBodyCaseError) as excinfo:
        DoubleBodyCaseConfig(
            manifest=manifest,
            stl_path=stl_file,
            velocity=SPEED,
            ranks=RANKS,
            freeboard_lpp=0.30,
        )
    assert "air column" in str(excinfo.value)
    assert FREEBOARD_LPP == 0.65, "the accepted default moved; the refusal follows it"


# --------------------------------------------------------------------------- #
#  Everything that must NOT be inherited from a benchmark
# --------------------------------------------------------------------------- #

def test_the_force_reference_is_this_hull(case, manifest) -> None:
    body = _block(read(case, "system/controlDict"), "forceCoeffs")
    assert float(_entry(body, "Aref")) == pytest.approx(
        manifest.wetted_surface_m2 / 2.0
    ), "Aref must be HALF the wetted surface on a centreplane-cut domain"
    assert float(_entry(body, "lRef")) == pytest.approx(manifest.lpp_m)
    assert float(_entry(body, "magUInf")) == pytest.approx(SPEED)
    assert _numbers(_entry(body, "CofR")) == pytest.approx(
        [0.0, 0.0, manifest.draft_m]
    )


def test_the_inlet_turbulence_is_derived_from_this_condition(
    case, config
) -> None:
    turb = derive_inlet_turbulence(config.velocity, config.manifest.lpp_m, config.nu)
    assert float(_entry(read(case, "0.orig/k"), "internalField").split()[-1]) == (
        pytest.approx(turb.k, rel=1e-6)
    )
    assert float(
        _entry(read(case, "0.orig/omega"), "internalField").split()[-1]
    ) == pytest.approx(turb.omega, rel=1e-6)
    assert float(
        _entry(read(case, "0.orig/nut"), "internalField").split()[-1]
    ) == pytest.approx(turb.nut, rel=1e-6)


def test_the_decomposition_reaches_the_case_and_multiplies_to_the_ranks(
    case,
) -> None:
    """decomposePar exits fatally on a mismatch, and it does so AFTER the mesh
    is built."""
    text = read(case, "system/decomposeParDict")
    assert int(_entry(text, "numberOfSubdomains")) == RANKS
    n = [int(v) for v in _numbers(_entry(_block(text, "coeffs"), "n"))]
    assert n == list(decomposition_vector(RANKS))
    assert n[0] * n[1] * n[2] == RANKS


def test_the_viscosity_reaches_transport_properties(case, config) -> None:
    nu = float(_entry(read(case, "constant/transportProperties"), "nu"))
    assert nu == pytest.approx(config.nu, rel=1e-9)


def test_the_surface_is_copied_and_named_consistently(case, config) -> None:
    assert (case / "constant" / "triSurface" / config.stl_name).is_file()
    for rel in ("system/snappyHexMeshDict", "system/surfaceFeatureExtractDict"):
        assert config.stl_name in read(case, rel)
    assert config.emesh_name in read(case, "system/snappyHexMeshDict")


# --------------------------------------------------------------------------- #
#  The two trees, held together
# --------------------------------------------------------------------------- #

@pytest.mark.parametrize("rel", SHARED_DICTS)
def test_the_shared_dictionaries_are_byte_identical(rel: str) -> None:
    """The cost of a sibling tree is duplication, and this is the interest
    payment. These files carry no free-surface content, so a change to one and
    not the other is drift -- the mesh the two cases are compared on would
    stop being the same mesh."""
    vof = (hull_case_templates_dir() / rel).read_bytes()
    double_body = (double_body_templates_dir() / rel).read_bytes()
    assert vof == double_body, f"{rel} has drifted between the two trees"


def test_no_template_in_the_tree_is_left_unsubstituted(case) -> None:
    for path in sorted(case.rglob("*")):
        if path.is_file() and path.suffix != ".stl":
            assert not re.search(r"@[A-Z0-9_]+@", path.read_text()), path


def test_the_case_can_be_built_from_files(manifest_file, stl_file, tmp_path) -> None:
    case = build_double_body_case_from_files(
        manifest_file, stl_file, velocity=SPEED, ranks=8, parent_dir=tmp_path
    )
    assert (case / "system" / "controlDict").is_file()
    assert (case / "0").is_dir()
    assert json.loads(read(case, "case_provenance.json"))["case_type"] == "double_body"


def test_the_provenance_states_what_the_form_factor_is(case) -> None:
    """A case emitted without the definition it feeds is a directory of
    dictionaries nobody can interpret six weeks later."""
    ff = json.loads(read(case, "case_provenance.json"))["form_factor"]
    assert ff["definition"] == "(1 + k) = C_v,double-body / C_f,ITTC-57"
    assert "double_body_form_factor" in ff["reduced_by"]
    assert "TOTAL" in ff["note"], "the pressure component must not be dropped"


def test_a_hull_whose_keel_clearance_swallows_the_waterline_is_refused(
    manifest, stl_file
) -> None:
    """An inverted block is a negative-volume cell, and blockMesh refuses it
    after the case has been written. Refuse first, and name the constraint."""
    with pytest.raises(HullDomainError):
        derive_double_body_case(
            DoubleBodyCaseConfig(
                manifest=manifest,
                stl_path=stl_file,
                velocity=SPEED,
                ranks=RANKS,
                keel_clearance_drafts=-1.0,
            )
        )
