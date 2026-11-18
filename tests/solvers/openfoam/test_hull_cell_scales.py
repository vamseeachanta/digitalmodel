"""Distinguish requested box resolution from near-wall mesh measurements."""

import json
from dataclasses import replace
from pathlib import Path

import pytest

from digitalmodel.solvers.openfoam.hull_case import HullCaseConfig, build_hull_case
from digitalmodel.solvers.openfoam.hull_case_physics import (
    CellBudgetError,
    derive_cell_budget,
)
from digitalmodel.solvers.openfoam.hull_case_regions import SurfaceRegion
from digitalmodel.solvers.openfoam.hull_double_body import (
    DoubleBodyCaseConfig,
    build_double_body_case,
)
from digitalmodel.solvers.openfoam.hull_domain import (
    block_divisions,
    build_hull_domain,
)
from digitalmodel.solvers.openfoam.hull_manifest import HullManifest


def test_legacy_size_is_explicitly_a_requested_box_scale(manifest_dict):
    """Hand calculation: a requested 0.8 m cell, halved twice, is 0.2 m.

    The synthetic boxes cover the domain; geometry is irrelevant to this
    scale calculation. Halving is specified by the repository's
    templates/hull_resistance/system/refineMeshDict (tan1 and tan2 only).
    OpenCFD, OpenFOAM v2312 (2023), snappyHexMesh Castellation introduction
    also describes halving per refinement level. These are test inputs,
    not a calibrated mesh or an engineering acceptance criterion.
    """
    domain = replace(
        build_hull_domain(HullManifest.from_dict(manifest_dict)), base_cell_size=0.8
    )
    box = (
        (domain.x_outlet, domain.y_side, domain.z_levels[0]),
        (domain.x_inlet, domain.y_centreplane, domain.z_levels[-1]),
    )
    budget = derive_cell_budget(domain, (box, box), block_divisions(domain), 1)
    mesh = budget.to_provenance()

    assert budget.finest_cell_size == pytest.approx(0.2)
    assert budget.finest_box_cell_size == pytest.approx(0.2)
    assert mesh["finest_box_cell_size_m"] == mesh["finest_cell_size_m"]
    assert "requested" in mesh["finest_cell_size_note"]
    assert "in-plane" in mesh["finest_cell_size_note"]


@pytest.mark.parametrize("nx", [10, 20])
def test_surface_targets_use_emitted_divisions_and_actual_region_levels(manifest_dict, nx):
    """Published example: background 0.2 m -> level 1: 0.1; level 2: 0.05.

    Source: OpenCFD, OpenFOAM v2312 (2023), snappyHexMesh Castellation,
    introduction, https://doc.openfoam.com/2312/tools/pre-processing/mesh/
    generation/snappyhexmesh/castellation/ . Independently, 2 m / 10 cells
    = 1 m / 5 cells = 0.2 m, then 0.2 / 2 = 0.1 and 0.2 / 4 = 0.05.
    With 20 x divisions, dx = 0.1 m; dy = 0.2 m remains the coarser scale.
    The deliberately different requested size (0.8 m) must not enter this
    target. These are nominal pre-snap in-plane targets, not wall measurements.
    """
    domain = replace(
        build_hull_domain(HullManifest.from_dict(manifest_dict)),
        base_cell_size=0.8,
        x_outlet=0.0,
        x_inlet=2.0,
        y_side=-1.0,
        y_centreplane=0.0,
    )
    divisions = dict(block_divisions(domain), nx=nx, ny=5)
    regions = (
        SurfaceRegion("hull", Path("hull.stl"), refinement_level=(1, 2)),
        SurfaceRegion("appendage", Path("appendage.stl"), refinement_level=(0, 1)),
    )
    mesh = derive_cell_budget(
        domain, (), divisions, 1, surface_regions=regions
    ).to_provenance()
    target = mesh["surface_cell_targets"]["hull"]
    assert target["refinement_level"] == [1, 2]
    assert target["coarsest_in_plane_cell_size_m"] == pytest.approx(0.1)
    assert target["finest_in_plane_cell_size_m"] == pytest.approx(0.05)
    assert mesh["surface_cell_targets"]["appendage"][
        "coarsest_in_plane_cell_size_m"
    ] == pytest.approx(0.2)
    assert mesh["near_wall_cell_size_m"] is None
    assert mesh["near_wall_cell_size_status"] == "NOT_EVALUATED"
    assert "innermost box" in mesh["surface_cell_targets_note"]


@pytest.mark.parametrize("levels", [(1.5, 2), (True, 1)])
def test_surface_levels_must_be_integers(manifest_dict, levels):
    """A fractional or boolean level does not define a valid refinement count."""
    domain = build_hull_domain(HullManifest.from_dict(manifest_dict))
    region = SurfaceRegion("hull", Path("hull.stl"), refinement_level=levels)
    with pytest.raises(CellBudgetError, match="integers"):
        derive_cell_budget(
            domain, (), block_divisions(domain), 1, surface_regions=(region,)
        )


def test_duplicate_names_cannot_silently_overwrite_a_surface_target(manifest_dict):
    """Two region records must not collapse into one provenance entry."""
    domain = build_hull_domain(HullManifest.from_dict(manifest_dict))
    region = SurfaceRegion("hull", Path("hull.stl"))
    with pytest.raises(CellBudgetError, match="duplicate surface region"):
        derive_cell_budget(
            domain, (), block_divisions(domain), 1, surface_regions=(region, region)
        )


def test_count_estimate_identifies_the_missing_surface_contribution(manifest_dict):
    """Hand calculation: two background cells * four x/y children = eight.

    The test has one full-domain box and one uniform vertical block.
    Source: templates/hull_resistance/system/refineMeshDict, directions
    (tan1 tan2). OpenCFD OpenFOAM v2312 (2023), Castellation introduction,
    specifies eight children for isotropic surface cuts; their total cannot
    be calculated without the number of intersected cells. In particular,
    multiplying the entire domain count by eight is not established.
    All dimensions, divisions, ranks and safety_factor are synthetic inputs.
    """
    domain = replace(
        build_hull_domain(HullManifest.from_dict(manifest_dict)),
        x_outlet=0.0,
        x_inlet=2.0,
        y_side=-1.0,
        y_centreplane=0.0,
    )
    divisions = dict(block_divisions(domain), nx=2, ny=1)
    box = ((0.0, -1.0, 0.0), (2.0, 0.0, 1.0))
    region = SurfaceRegion("hull", Path("hull.stl"), refinement_level=(1, 1))
    mesh = derive_cell_budget(
        domain,
        (box,),
        divisions,
        1,
        blocks=((0.0, 1.0, 1),),
        safety_factor=1.0,
        surface_regions=(region,),
    ).to_provenance()
    assert mesh["background_cells"] == 2
    assert mesh["estimated_cells"] == 8
    assert mesh["estimated_cells_basis"] == "box_refinement_only"
    assert mesh["surface_refinement_cell_count"] is None
    assert mesh["surface_refinement_cell_count_status"] == "NOT_EVALUATED"


def test_wall_cell_size_is_not_inferred_when_regions_are_unspecified(manifest_dict):
    """Absent regions cannot establish a surface or wall-cell size."""
    domain = build_hull_domain(HullManifest.from_dict(manifest_dict))
    mesh = derive_cell_budget(domain, (), block_divisions(domain), 1).to_provenance()
    assert mesh["surface_cell_targets"] == {}
    assert mesh["near_wall_cell_size_m"] is None
    assert mesh["near_wall_cell_size_status"] == "NOT_EVALUATED"


@pytest.mark.parametrize(
    "config_type,builder",
    [
        (HullCaseConfig, build_hull_case),
        (DoubleBodyCaseConfig, build_double_body_case),
    ],
)
def test_built_cases_record_hull_surface_targets(
    config_type, builder, manifest_dict, stl_file, tmp_path
):
    """Hand calculation: level (1,1) halves the emitted finest-box scale.

    Source: OpenCFD, OpenFOAM v2312 (2023), Castellation introduction;
    hull_case_regions.HULL_REFINEMENT_LEVEL supplies the current hull input.
    Both case builders must carry their emitted levels into saved provenance.
    The speed and rank count are synthetic test inputs, not acceptance limits.
    """
    config = config_type(
        manifest=HullManifest.from_dict(manifest_dict),
        stl_path=stl_file,
        velocity=2.0,
        ranks=1,
    )
    case = builder(config, tmp_path)
    mesh = json.loads((case / "case_provenance.json").read_text())["mesh"]
    target = mesh["surface_cell_targets"]["hull"]
    assert target["refinement_level"] == [1, 1]
    assert target["finest_in_plane_cell_size_m"] == pytest.approx(
        mesh["finest_in_plane_cell_m"] / 2
    )
    assert mesh["near_wall_cell_size_m"] is None
    assert "level (0 0)" not in mesh["wall_resolution_note"]
