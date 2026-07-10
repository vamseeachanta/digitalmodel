"""Contract tests for the reusable frame-locked 3D case writer."""

from __future__ import annotations

import hashlib
import importlib.util
import json
import sys
from pathlib import Path

import pytest

from digitalmodel.solvers.openfoam.poly_mesh_contract import BoundaryContract
from digitalmodel.solvers.openfoam.validation.sloshing_3d import (
    Sloshing3DConfig,
    write_sloshing_case,
)


FIXTURE = Path(__file__).parent / "fixtures" / "sloshing_3d_case_sha256.json"
BENCHMARK_SCRIPT = Path(__file__).resolve().parents[4] / "scripts/cfd/run_sloshing_3d_benchmark.py"


def _load_benchmark():
    spec = importlib.util.spec_from_file_location("run_sloshing_3d_benchmark", BENCHMARK_SCRIPT)
    assert spec is not None and spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


benchmark = _load_benchmark()


def _hashes(root: Path) -> dict[str, str]:
    return {
        path.relative_to(root).as_posix(): hashlib.sha256(path.read_bytes()).hexdigest()
        for path in sorted(root.rglob("*"))
        if path.is_file()
    }


def _occupy_destination(case: Path, kind: str) -> Path | None:
    if kind == "file":
        case.write_text("keep", encoding="utf-8")
        return case
    if kind == "directory":
        case.mkdir()
        protected = case / "sentinel"
        protected.write_text("keep", encoding="utf-8")
        return protected
    target = case.parent / ("target" if kind == "symlink" else "missing")
    if kind == "symlink":
        target.mkdir()
        protected = target / "sentinel"
        protected.write_text("keep", encoding="utf-8")
    else:
        protected = None
    case.symlink_to(target, target_is_directory=True)
    return protected


def test_writer_preserves_independent_pre_refactor_golden(tmp_path: Path) -> None:
    golden = json.loads(FIXTURE.read_text(encoding="utf-8"))
    config = Sloshing3DConfig(cpb=6, end_time=0.30, delta_t=0.001)

    assert write_sloshing_case(tmp_path / "case", config) == 216
    assert _hashes(tmp_path / "case") == golden["files"]


@pytest.mark.parametrize("kind", ["file", "directory", "symlink", "broken_symlink"])
def test_writer_rejects_every_existing_destination(tmp_path: Path, kind: str) -> None:
    case = tmp_path / "case"
    protected = _occupy_destination(case, kind)
    config = Sloshing3DConfig(cpb=2, end_time=0.1, delta_t=0.001)

    with pytest.raises(FileExistsError, match="already exists"):
        write_sloshing_case(case, config)

    assert case.exists() or case.is_symlink()
    if protected is not None:
        assert protected.read_text(encoding="utf-8") == "keep"


def test_benchmark_cleans_only_its_generated_case(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    work_dir = tmp_path / "work"
    case = work_dir / "s3d_bench_cpb2_np02"
    case.mkdir(parents=True)
    (case / "stale").write_text("old", encoding="utf-8")
    neighbor = work_dir / "keep"
    neighbor.write_text("untouched", encoding="utf-8")

    def fake_build(path: Path, *_args) -> int:
        assert path == case
        assert not path.exists()
        path.mkdir()
        return 8

    monkeypatch.setattr(benchmark, "build_case", fake_build)
    monkeypatch.setattr(benchmark, "_sh", lambda *_args, **_kwargs: 1)

    row = benchmark.run_at_ranks(work_dir, 2, 0.1, 0.001, 2)

    assert row["status"] == "blockMesh_failed"
    assert neighbor.read_text(encoding="utf-8") == "untouched"


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


@pytest.mark.parametrize("ranks", [0, True, 2.5])
def test_writer_rejects_invalid_decompose_ranks(tmp_path: Path, ranks: object) -> None:
    config = Sloshing3DConfig(
        cpb=2,
        end_time=0.1,
        delta_t=0.001,
        decompose_ranks=ranks,
    )

    with pytest.raises(ValueError, match="decompose_ranks"):
        write_sloshing_case(tmp_path / "case", config)


def test_prebuilt_mode_does_not_write_blockmesh(tmp_path: Path) -> None:
    config = Sloshing3DConfig(
        cpb=2,
        end_time=0.1,
        delta_t=0.001,
        mesh_mode="prebuilt",
        decompose_ranks=8,
    )

    write_sloshing_case(tmp_path / "case", config)

    assert not (tmp_path / "case" / "system" / "blockMeshDict").exists()
    decompose = (tmp_path / "case" / "system" / "decomposeParDict").read_text()
    assert "numberOfSubdomains 8;" in decompose
    assert "method scotch;" in decompose
