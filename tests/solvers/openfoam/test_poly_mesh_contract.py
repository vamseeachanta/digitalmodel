"""Structural contract tests for converted OpenFOAM polyMesh trees."""

from __future__ import annotations

from pathlib import Path

import pytest

from digitalmodel.solvers.openfoam.poly_mesh_contract import (
    DEFAULT_BOUNDARY_CONTRACT,
    PolyMeshContractError,
    parse_boundary,
    parse_cell_zones,
    parse_check_mesh_output,
    validate_poly_mesh_contract,
)


VALID_CHECK_MESH = """Mesh stats
    points:           8
    faces:            9
    internal faces:   3
    cells:            4
Failed 0 mesh checks.
Mesh OK.
"""


def boundary_text(
    *,
    walls_type: str = "wall",
    wall_faces: int = 4,
    wall_start: int = 3,
    atmosphere_faces: int = 2,
    atmosphere_start: int = 7,
    extra: str = "",
) -> str:
    count = 2 + bool(extra)
    return f"""FoamFile
{{
    version 2.0;
    format ascii;
    class polyBoundaryMesh;
    object boundary;
}}
// defaultFaces in a comment is not a patch.
{count}
(
    walls
    {{
        type {walls_type};
        nFaces {wall_faces};
        startFace {wall_start};
        inGroups 1(wall);
    }}
    atmosphere
    {{
        type patch;
        nFaces {atmosphere_faces};
        startFace {atmosphere_start};
    }}
    {extra}
)
"""


def cell_zones_text(labels: tuple[int, ...] = (0, 1, 2, 3)) -> str:
    joined = " ".join(str(label) for label in labels)
    return f"""FoamFile
{{
    version 2.0;
    format ascii;
    class regIOobject;
    object cellZones;
}}
1
(
    fluid
    {{
        type cellZone;
        cellLabels List<label>
        {len(labels)}
        (
            {joined}
        );
    }}
)
"""


def write_poly_mesh(
    root: Path,
    *,
    boundary: str | None = None,
    cell_zones: str | None = None,
) -> Path:
    poly_mesh = root / "constant" / "polyMesh"
    poly_mesh.mkdir(parents=True)
    (poly_mesh / "boundary").write_text(
        boundary if boundary is not None else boundary_text(), encoding="utf-8"
    )
    (poly_mesh / "cellZones").write_text(
        cell_zones if cell_zones is not None else cell_zones_text(),
        encoding="utf-8",
    )
    return poly_mesh


def test_structural_parsers_ignore_comments_and_read_lists(tmp_path: Path) -> None:
    poly_mesh = write_poly_mesh(tmp_path)

    patches = parse_boundary(poly_mesh / "boundary")
    zones = parse_cell_zones(poly_mesh / "cellZones")

    assert [(patch.name, patch.patch_type) for patch in patches] == [
        ("walls", "wall"),
        ("atmosphere", "patch"),
    ]
    assert [(patch.start_face, patch.n_faces) for patch in patches] == [
        (3, 4),
        (7, 2),
    ]
    assert zones["fluid"].zone_type == "cellZone"
    assert zones["fluid"].cell_labels == (0, 1, 2, 3)


def test_valid_poly_mesh_covers_boundary_faces_and_all_cells(tmp_path: Path) -> None:
    poly_mesh = write_poly_mesh(tmp_path)

    result = validate_poly_mesh_contract(
        poly_mesh,
        check_mesh_output=VALID_CHECK_MESH,
        check_mesh_return_code=0,
        boundary_contract=DEFAULT_BOUNDARY_CONTRACT,
    )

    assert result.cell_count == 4
    assert result.face_count == 9
    assert result.internal_face_count == 3
    assert result.boundary_face_count == 6
    assert result.patch_names == ("walls", "atmosphere")
    assert result.fluid_cell_labels == (0, 1, 2, 3)


def test_return_code_zero_with_failed_mesh_check_is_rejected() -> None:
    output = VALID_CHECK_MESH.replace(
        "Failed 0 mesh checks.", "Failed 1 mesh checks."
    )

    with pytest.raises(PolyMeshContractError, match="Failed 1 mesh checks"):
        parse_check_mesh_output(output, return_code=0)


def test_openfoam_2312_success_may_omit_zero_failed_check_count() -> None:
    output = VALID_CHECK_MESH.replace("Failed 0 mesh checks.\n", "")

    result = parse_check_mesh_output(output, return_code=0)

    assert result.cell_count == 4


@pytest.mark.parametrize(
    ("output", "return_code", "message"),
    [
        (VALID_CHECK_MESH, 2, "return code 2"),
        (VALID_CHECK_MESH + "FOAM FATAL ERROR\n", 0, "FATAL"),
        (VALID_CHECK_MESH.replace("Mesh OK.\n", ""), 0, "Mesh OK"),
        (
            VALID_CHECK_MESH.replace("    cells:            4\n", ""),
            0,
            "cells",
        ),
    ],
)
def test_check_mesh_requires_semantic_success_and_structural_counts(
    output: str, return_code: int, message: str
) -> None:
    with pytest.raises(PolyMeshContractError, match=message):
        parse_check_mesh_output(output, return_code=return_code)


@pytest.mark.parametrize(
    ("boundary", "message"),
    [
        (boundary_text(walls_type="patch"), "walls.*wall"),
        (boundary_text(wall_faces=0), "positive"),
        (boundary_text(atmosphere_start=8), "gap"),
        (boundary_text(atmosphere_start=6), "overlap"),
        (
            boundary_text(
                extra="defaultFaces { type patch; nFaces 1; startFace 9; }"
            ),
            "defaultFaces",
        ),
    ],
)
def test_patch_type_size_name_and_ranges_fail_closed(
    tmp_path: Path, boundary: str, message: str
) -> None:
    poly_mesh = write_poly_mesh(tmp_path, boundary=boundary)

    with pytest.raises(PolyMeshContractError, match=message):
        validate_poly_mesh_contract(
            poly_mesh,
            check_mesh_output=VALID_CHECK_MESH,
            check_mesh_return_code=0,
        )


@pytest.mark.parametrize(
    ("labels", "message"),
    [
        ((0, 1, 2), "cover every cell"),
        ((0, 1, 2, 2), "duplicate"),
        ((0, 1, 2, 4), "cover every cell"),
    ],
)
def test_fluid_zone_must_cover_every_cell_once(
    tmp_path: Path, labels: tuple[int, ...], message: str
) -> None:
    poly_mesh = write_poly_mesh(tmp_path, cell_zones=cell_zones_text(labels))

    with pytest.raises(PolyMeshContractError, match=message):
        validate_poly_mesh_contract(
            poly_mesh,
            check_mesh_output=VALID_CHECK_MESH,
            check_mesh_return_code=0,
        )


def test_reported_face_counts_must_match_patch_coverage(tmp_path: Path) -> None:
    poly_mesh = write_poly_mesh(tmp_path)
    inconsistent = VALID_CHECK_MESH.replace("faces:            9", "faces:            10")

    with pytest.raises(PolyMeshContractError, match="boundary face count"):
        validate_poly_mesh_contract(
            poly_mesh,
            check_mesh_output=inconsistent,
            check_mesh_return_code=0,
        )
