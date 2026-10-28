# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Execute the documented synthetic mesh workflow without external services."""

from pathlib import Path
import re
import shlex
import shutil
import socket
import subprocess
import sys

from click.testing import CliRunner
import pytest

from digitalmodel.hydrodynamics.diffraction.cli import cli
from digitalmodel.hydrodynamics.diffraction.input_schemas import DiffractionSpec
from digitalmodel.hydrodynamics.diffraction import resolver


EXAMPLE = (
    Path(__file__).resolve().parents[3]
    / "docs/domains/diffraction/tutorials/mesh-to-spec.md"
)


@pytest.fixture
def documented_example(tmp_path, monkeypatch):
    """Run the published mesh creation and CLI arguments in a temporary folder."""
    def forbidden(*args, **kwargs):
        raise AssertionError("The example must not use network or solver discovery")

    monkeypatch.setattr(socket, "create_connection", forbidden)
    monkeypatch.setattr(socket.socket, "connect", forbidden)
    monkeypatch.setattr(subprocess, "Popen", forbidden)
    monkeypatch.setattr(shutil, "which", forbidden)
    output_dir = tmp_path / "caller output"
    text = EXAMPLE.read_text(encoding="utf-8")
    snippet = re.search(r"<<'PY'\n(.*?)\nPY", text, re.DOTALL)
    assert snippet is not None, "The documented mesh creation must remain runnable"
    monkeypatch.setattr(sys, "argv", ["-", str(output_dir)])
    exec(compile(snippet.group(1), str(EXAMPLE), "exec"), {"__name__": "__main__"})
    command = re.search(r"^diffraction resolve .*?(?=\n```)", text, re.MULTILINE | re.DOTALL)
    assert command is not None, "The documented resolve command must remain runnable"
    arguments = shlex.split(command.group().replace("\\\n", " "))
    assert arguments.pop(0) == "diffraction"
    arguments = [arg.replace("$OUTPUT_DIR", str(output_dir)) for arg in arguments]
    result = CliRunner().invoke(cli, arguments)
    assert result.exit_code == 0, result.output
    return output_dir, result, arguments


def test_documented_mesh_to_spec_example_runs_offline(documented_example):
    """Check the physical inputs against the example's independent box basis."""
    output_dir, result, _ = documented_example
    spec = DiffractionSpec.from_yaml(output_dir / "spec.yml")
    mesh = output_dir / "box.gdf"
    assert Path(spec.vessel.geometry.mesh_file) == mesh
    assert mesh.is_file()
    # Source: mesh-to-spec.md, Synthetic input basis. Volume = 10 * 4 * 2
    # = 80 cubic metres; 82 tonnes * 1000 kg/tonne = 82000 kg.
    assert spec.vessel.inertia.mass == 82000.0
    assert spec.environment.water_depth == 50.0
    # Independent uniform-box centroid: midpoint of x=[0, 10], y=[-2, 2],
    # z=[-2, 0]. The example labels its use for inertia as an estimate.
    assert spec.vessel.inertia.centre_of_gravity == [5.0, 0.0, -1.0]
    assert spec.vessel.inertia.cog_z == -1.0
    assert spec.vessel.inertia.mode == "free_floating"
    assert "[OK] Resolved diffraction spec." in result.output


def test_documented_example_records_assumptions(documented_example, monkeypatch):
    """Retain all estimates and defaults in the CLI assumption output."""
    _, result, arguments = documented_example
    assert "Assumptions:" in result.output
    rows = {
        line.strip().removeprefix("- ").split(":", 1)[0]: line
        for line in result.output.splitlines()
        if line.startswith("  - ")
    }
    estimated = {
        "vessel.inertia.mass",
        "vessel.inertia.centre_of_gravity",
        "vessel.inertia.radii_of_gyration",
        "vessel.inertia.cog_z",
    }
    defaults = {
        "vessel.name",
        "vessel.geometry.mesh_format",
        "vessel.geometry.symmetry",
        "vessel.geometry.reference_point",
        "vessel.geometry.waterline_z",
        "vessel.geometry.length_units",
        "environment.water_density",
        "environment.gravity",
        "frequencies.range",
        "wave_headings.range",
    }
    assert set(rows) == estimated | defaults
    for field in estimated:
        assert "[estimated_from_data, medium]" in rows[field]
    for field in defaults:
        assert "[assumed_default, low]" in rows[field]

    def no_mesh_dimensions(*args, **kwargs):
        raise AssertionError("Declared dimensions must not be inferred from the mesh")

    monkeypatch.setattr(resolver, "_derive_dimensions_from_mesh", no_mesh_dimensions)
    repeated = CliRunner().invoke(cli, arguments)
    assert repeated.exit_code == 0, repeated.output
    assert repeated.output == result.output
