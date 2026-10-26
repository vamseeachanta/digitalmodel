# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Command execution must receive and release its explicit material inputs."""
from pathlib import Path

import pytest

from digitalmodel.materials import get, use_catalog
from digitalmodel.solvers.orcaflex.modular_generator.cli import (
    create_parser, run_with_material_catalog,
)


@pytest.mark.parametrize("allow", [False, True])
def test_command_scopes_explicit_catalogue(allow):
    fixture = Path(__file__).resolve().parents[3] / "materials" / "fixtures" / "catalog.json"
    argv = ["--material-catalog", str(fixture)]
    if allow:
        argv.append("--allow-synthetic-materials")
    argv += ["generate", "--input", "synthetic.yml", "--output", "generated"]
    args = create_parser().parse_args(argv)

    def command(_args):
        assert get("X65").smys_mpa == 317
        return 0

    args.func = command
    with use_catalog(None):
        if allow:
            assert run_with_material_catalog(args) == 0
        else:
            with pytest.raises(ValueError, match="synthetic"):
                run_with_material_catalog(args)
        with pytest.raises(ValueError, match="caller-supplied"):
            get("X65")
