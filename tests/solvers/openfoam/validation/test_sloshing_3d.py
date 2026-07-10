"""Contract tests for the reusable frame-locked 3D case writer."""

from __future__ import annotations

import hashlib
import json
from pathlib import Path

import pytest

from digitalmodel.solvers.openfoam.poly_mesh_contract import BoundaryContract
from digitalmodel.solvers.openfoam.validation.sloshing_3d import (
    Sloshing3DConfig,
    write_sloshing_case,
)


FIXTURE = Path(__file__).parent / "fixtures" / "sloshing_3d_case_sha256.json"


def _hashes(root: Path) -> dict[str, str]:
    return {
        path.relative_to(root).as_posix(): hashlib.sha256(path.read_bytes()).hexdigest()
        for path in sorted(root.rglob("*"))
        if path.is_file()
    }


def test_writer_preserves_independent_pre_refactor_golden(tmp_path: Path) -> None:
    golden = json.loads(FIXTURE.read_text(encoding="utf-8"))
    config = Sloshing3DConfig(cpb=6, end_time=0.30, delta_t=0.001)

    assert write_sloshing_case(tmp_path / "case", config) == 216
    assert _hashes(tmp_path / "case") == golden["files"]


def test_writer_accepts_exact_mesh_boundary_contract(tmp_path: Path) -> None:
    contract = BoundaryContract(
        wall_patches=("leftWall", "rightWall", "lowerWall", "frontWall", "backWall"),
        atmosphere_patch="atmosphere",
    )
    config = Sloshing3DConfig(cpb=2, end_time=0.1, delta_t=0.001)

    write_sloshing_case(tmp_path / "case", config, boundary_contract=contract)
    fields = [tmp_path / "case" / "0" / name for name in ("U", "p_rgh", "alpha.water")]
    for field in fields:
        text = field.read_text(encoding="utf-8")
        assert all(f"    {name} {{" in text for name in contract.patch_names)


def test_writer_rejects_non_si_frame(tmp_path: Path) -> None:
    config = Sloshing3DConfig(cpb=2, end_time=0.1, delta_t=0.001, length_unit="mm")

    with pytest.raises(ValueError, match="length_unit"):
        write_sloshing_case(tmp_path / "case", config)


def test_prebuilt_mode_does_not_write_blockmesh(tmp_path: Path) -> None:
    config = Sloshing3DConfig(cpb=2, end_time=0.1, delta_t=0.001, mesh_mode="prebuilt")

    write_sloshing_case(tmp_path / "case", config)

    assert not (tmp_path / "case" / "system" / "blockMeshDict").exists()
