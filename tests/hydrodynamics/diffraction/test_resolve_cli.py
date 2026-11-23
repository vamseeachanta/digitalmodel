"""CLI tests for diffraction inverse resolver."""

from __future__ import annotations

from pathlib import Path

from click.testing import CliRunner

from digitalmodel.hydrodynamics.diffraction.cli import cli
from digitalmodel.hydrodynamics.diffraction.input_schemas import DiffractionSpec


def test_resolve_cli_writes_spec_and_prints_assumptions(tmp_path: Path) -> None:
    mesh = tmp_path / "box.gdf"
    mesh.write_text(
        "\n".join(
            [
                "0 0 -2",
                "10 0 -2",
                "10 4 -2",
                "0 4 -2",
                "0 0 0",
                "10 0 0",
            ]
        ),
        encoding="utf-8",
    )
    out = tmp_path / "resolved.yml"

    result = CliRunner().invoke(
        cli,
        [
            "resolve",
            "--outcome",
            "ship_raos",
            "--mesh",
            str(mesh),
            "--water-depth",
            "50",
            "--out",
            str(out),
        ],
    )

    assert result.exit_code == 0, result.output
    assert out.exists()
    spec = DiffractionSpec.from_yaml(out)
    assert spec.vessel.geometry.mesh_file == str(mesh)
    assert "Assumptions:" in result.output
    assert "vessel.inertia.mass" in result.output
