# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Generate and validate jumper input YAML without running a licensed solver.

The existing converter supplies geometry, properties and environment data.
Only descriptive identifiers are replaced; numerical settings are preserved.
Validation covers the input schema, YAML format, include composition and
cross-object references. It does not establish solver loadability or results.
"""

from __future__ import annotations

import argparse
from pathlib import Path

import yaml

from digitalmodel.marine_ops.installation.jumper_to_modular_spec import (
    build_modular_spec,
)
from digitalmodel.solvers.orcaflex.modular_generator import ModularModelGenerator
from digitalmodel.solvers.orcaflex.modular_generator.post_validator import (
    PostGenerationValidator,
    check_collection_collisions,
)
from digitalmodel.solvers.orcaflex.modular_generator.schema import ProjectInputSpec
from digitalmodel.solvers.orcaflex.yaml_validator import validate_orcaflex_yaml


def validate_input(output_dir: str | Path) -> None:
    """Require a complete generated model passing repository validators.

    Args:
        output_dir: Directory containing master, parameters and includes.

    Raises:
        ValueError: An include is missing or repository validation fails.
    """
    output = Path(output_dir)
    master = output / "master.yml"
    entries = yaml.safe_load(master.read_text(encoding="utf-8"))
    if not isinstance(entries, list) or not entries:
        raise ValueError("Master must contain an include list")
    includes = []
    for entry in entries:
        if not isinstance(entry, dict) or "includefile" not in entry:
            raise ValueError("Master entry must identify an include")
        include = (output / entry["includefile"]).resolve()
        if not include.is_relative_to(output.resolve()):
            raise ValueError("Include must remain within the model directory")
        if not include.is_file():
            raise ValueError("Missing include")
        includes.append(include)
    for path in [master, *includes]:
        result = validate_orcaflex_yaml(path)
        if result.errors:
            raise ValueError(f"YAML validation failed: {path.name}")
    parameters = output / "inputs/parameters.yml"
    if not parameters.is_file():
        raise ValueError("Missing parameters")
    yaml.safe_load(parameters.read_text(encoding="utf-8"))
    check_collection_collisions(master, raise_on_collision=True)
    findings = PostGenerationValidator().validate_directory(output)
    if any(finding.level == "error" for finding in findings):
        raise ValueError("Cross-object validation failed")


def generate_input(
    spec_path: str | Path, output_dir: str | Path, *, name: str = "jumper-a"
) -> None:
    """Convert a domain specification, generate input and validate it.

    Args:
        spec_path: Existing jumper installation domain specification.
        output_dir: Destination for the generated model.
        name: Neutral model identifier used instead of source identifiers.
    """
    data = build_modular_spec(str(spec_path))
    data["metadata"] = {
        "name": name,
        "description": "Jumper installation input; assumed M-profile and hub clearance",
        "structure": "jumper",
        "operation": "generic",
    }
    data["generic"]["lines"][0]["name"] = name
    spec = ProjectInputSpec(**data)
    ModularModelGenerator.from_spec(spec).generate(Path(output_dir))
    validate_input(output_dir)


def main() -> None:
    """Run generation or solver-free validation from the command line."""
    parser = argparse.ArgumentParser(description=__doc__)
    commands = parser.add_subparsers(dest="command", required=True)
    generate = commands.add_parser("generate")
    generate.add_argument("spec", type=Path)
    generate.add_argument("output", type=Path)
    generate.add_argument("--name", default="jumper-a")
    validate = commands.add_parser("validate")
    validate.add_argument("output", type=Path)
    args = parser.parse_args()
    if args.command == "generate":
        generate_input(args.spec, args.output, name=args.name)
    else:
        validate_input(args.output)
    print("PASS: repository solver-free input validation; no solver run")


if __name__ == "__main__":
    main()
