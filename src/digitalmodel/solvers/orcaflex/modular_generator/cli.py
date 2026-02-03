"""
Command-line interface for the OrcaFlex Modular Model Generator.

This module provides CLI commands for generating OrcaFlex modular models from
YAML specification files and validating specifications without generating output.

Usage:
    # Generate model
    uv run python -m digitalmodel.solvers.orcaflex.modular_generator generate \
        --input spec.yml \
        --output generated/

    # Validate spec
    uv run python -m digitalmodel.solvers.orcaflex.modular_generator validate \
        --input spec.yml
"""

import argparse
import sys
from pathlib import Path
from typing import NoReturn

import yaml
from pydantic import ValidationError

from .schema import ProjectInputSpec
from . import ModularModelGenerator
from .builders.registry import BuilderRegistry


def print_error(message: str) -> None:
    """Print error message to stderr."""
    print(f"ERROR: {message}", file=sys.stderr)


def print_success(message: str) -> None:
    """Print success message."""
    print(f"[OK] {message}")


def print_progress(message: str) -> None:
    """Print progress message."""
    print(f"  -> {message}")


def validate_spec(input_path: Path) -> ProjectInputSpec | None:
    """
    Validate a YAML specification file.

    Args:
        input_path: Path to the YAML spec file

    Returns:
        Validated ProjectInputSpec if successful, None otherwise
    """
    if not input_path.exists():
        print_error(f"Input file not found: {input_path}")
        return None

    if not input_path.suffix.lower() in ('.yml', '.yaml'):
        print_error(f"Input file must be a YAML file (.yml or .yaml): {input_path}")
        return None

    try:
        with open(input_path) as f:
            data = yaml.safe_load(f)
    except yaml.YAMLError as e:
        print_error(f"Failed to parse YAML file: {e}")
        return None
    except Exception as e:
        print_error(f"Failed to read file: {e}")
        return None

    if data is None:
        print_error("Input file is empty")
        return None

    try:
        spec = ProjectInputSpec(**data)
        return spec
    except ValidationError as e:
        print_error("Specification validation failed:")
        for error in e.errors():
            loc = " -> ".join(str(x) for x in error['loc'])
            msg = error['msg']
            print(f"  - {loc}: {msg}", file=sys.stderr)
        return None


def cmd_validate(args: argparse.Namespace) -> int:
    """
    Execute the validate command.

    Args:
        args: Parsed command-line arguments

    Returns:
        Exit code (0 for success, 1 for failure)
    """
    input_path = Path(args.input)

    print(f"Validating specification: {input_path}")
    print("-" * 60)

    spec = validate_spec(input_path)

    if spec is None:
        return 1

    # Print validation summary
    print_success("Specification is valid")
    print()
    print("Summary:")
    print(f"  Model name:        {spec.metadata.name}")
    print(f"  Description:       {spec.metadata.description}")
    print(f"  Structure type:    {spec.metadata.structure}")
    print(f"  Operation:         {spec.metadata.operation}")
    print(f"  Project:           {spec.metadata.project}")
    print()
    print("Environment:")
    print(f"  Water depth:       {spec.environment.water.depth} m")
    print(f"  Water density:     {spec.environment.water.density} te/m3")
    print(f"  Wave type:         {spec.environment.waves.type}")
    print(f"  Wave height:       {spec.environment.waves.height} m")
    print(f"  Current speed:     {spec.environment.current.speed} m/s")
    print(f"  Wind speed:        {spec.environment.wind.speed} m/s")
    print()
    print("Pipeline:")
    print(f"  Name:              {spec.pipeline.name}")
    print(f"  Material:          {spec.pipeline.material}")
    print(f"  Outer diameter:    {spec.pipeline.dimensions.outer_diameter} m")
    print(f"  Wall thickness:    {spec.pipeline.dimensions.wall_thickness} m")
    print(f"  Total length:      {spec.get_total_pipeline_length()} m")
    print(f"  Segments:          {len(spec.pipeline.segments)}")
    print()
    print("Equipment:")
    if spec.equipment.tugs:
        print(f"  Tugs:              {spec.equipment.tugs.count}")
        print(f"  Tug spacing:       {spec.equipment.tugs.spacing} m")
    else:
        print("  Tugs:              None")
    if spec.equipment.buoyancy_modules:
        print(f"  BM spacing:        {spec.equipment.buoyancy_modules.spacing} m")
    else:
        print("  Buoyancy modules:  None")
    print(f"  Ramps/shapes:      {len(spec.equipment.ramps)}")
    print()
    print("Simulation:")
    print(f"  Time step:         {spec.simulation.time_step} s")
    print(f"  Stages:            {spec.simulation.stages}")
    print(f"  North direction:   {spec.simulation.north_direction} deg")

    return 0


def cmd_generate(args: argparse.Namespace) -> int:
    """
    Execute the generate command.

    Args:
        args: Parsed command-line arguments

    Returns:
        Exit code (0 for success, 1 for failure)
    """
    input_path = Path(args.input)
    output_dir = Path(args.output)

    print(f"OrcaFlex Modular Model Generator")
    print("=" * 60)
    print(f"Input:  {input_path}")
    print(f"Output: {output_dir}")
    print("-" * 60)

    # Validate first
    print("Step 1: Validating specification...")
    spec = validate_spec(input_path)

    if spec is None:
        return 1

    print_success(f"Validated: {spec.metadata.name}")
    print()

    # Create output directory
    print("Step 2: Creating output directory structure...")
    try:
        output_dir.mkdir(parents=True, exist_ok=True)
        includes_dir = output_dir / 'includes'
        inputs_dir = output_dir / 'inputs'
        includes_dir.mkdir(exist_ok=True)
        inputs_dir.mkdir(exist_ok=True)
        print_success(f"Created: {output_dir}/")
        print_progress(f"includes/")
        print_progress(f"inputs/")
    except Exception as e:
        print_error(f"Failed to create output directory: {e}")
        return 1

    print()

    # Generate model
    print("Step 3: Running builders...")
    print()

    try:
        generator = ModularModelGenerator(input_path)

        from .builders.context import BuilderContext
        from .builders.registry import BuilderRegistry

        context = BuilderContext()

        for file_name, builder_class in BuilderRegistry.get_ordered_builders():
            builder_name = builder_class.__name__
            print_progress(f"Running {builder_name}...")

            builder = builder_class(spec, context)

            if not builder.should_generate():
                print_progress(f"Skipped {file_name} (no data)")
                continue

            data = builder.build()
            context.update_from_dict(builder.get_generated_entities())

            file_path = includes_dir / file_name
            with open(file_path, 'w') as f:
                yaml.dump(data, f, default_flow_style=False, allow_unicode=True, sort_keys=False)

            print_success(f"Generated: includes/{file_name}")

        # Generate parameters.yml
        print_progress("Writing parameters...")
        params = {
            'water_depth': spec.environment.water.depth,
            'water_density': spec.environment.water.density,
            'current_speed': spec.environment.current.speed,
            'current_direction': spec.environment.current.direction,
            'wind_speed': spec.environment.wind.speed,
            'hs': spec.environment.waves.height,
            'tp': spec.environment.waves.period,
            'wave_direction': spec.environment.waves.direction,
            'stage_durations': spec.simulation.stages,
            'time_step': spec.simulation.time_step,
        }
        params_path = inputs_dir / 'parameters.yml'
        with open(params_path, 'w') as f:
            yaml.dump(params, f, default_flow_style=False)
        print_success("Generated: inputs/parameters.yml")

        # Generate master.yml
        print_progress("Writing master file...")
        master_lines = [
            '%YAML 1.1',
            '# Type: Model',
            f'# Generated from: {input_path.name}',
            f'# Model: {spec.metadata.name}',
            '',
        ]
        for file_name in BuilderRegistry.get_include_order():
            master_lines.append(f'- includefile: includes/{file_name}')

        master_path = output_dir / 'master.yml'
        with open(master_path, 'w') as f:
            f.write('\n'.join(master_lines))
        print_success("Generated: master.yml")

    except Exception as e:
        print_error(f"Generation failed: {e}")
        import traceback
        traceback.print_exc()
        return 1

    print()
    print("=" * 60)
    print_success(f"Model generated successfully in: {output_dir}")
    print()
    print("Generated files:")
    print(f"  {output_dir}/")
    print(f"    master.yml")
    print(f"    includes/")
    for file_name in BuilderRegistry.get_include_order():
        print(f"      {file_name}")
    print(f"    inputs/")
    print(f"      parameters.yml")

    return 0


def create_parser() -> argparse.ArgumentParser:
    """
    Create and configure the argument parser.

    Returns:
        Configured ArgumentParser instance
    """
    parser = argparse.ArgumentParser(
        prog='modular_generator',
        description='OrcaFlex Modular Model Generator - Generate OrcaFlex models from YAML specs',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Generate a model from a spec file
  uv run python -m digitalmodel.solvers.orcaflex.modular_generator generate \\
      --input spec.yml --output generated/

  # Validate a spec file without generating
  uv run python -m digitalmodel.solvers.orcaflex.modular_generator validate \\
      --input spec.yml

For more information, see the documentation at:
  docs/modules/orcaflex/pipeline/installation/floating/30in_pipeline/
"""
    )

    subparsers = parser.add_subparsers(
        dest='command',
        title='commands',
        description='Available commands',
        help='Command to execute'
    )

    # Generate command
    generate_parser = subparsers.add_parser(
        'generate',
        help='Generate OrcaFlex modular model from spec',
        description='Generate a complete OrcaFlex modular model structure from a YAML specification file.'
    )
    generate_parser.add_argument(
        '--input', '-i',
        required=True,
        help='Path to the input YAML specification file'
    )
    generate_parser.add_argument(
        '--output', '-o',
        required=True,
        help='Path to the output directory for generated files'
    )
    generate_parser.set_defaults(func=cmd_generate)

    # Validate command
    validate_parser = subparsers.add_parser(
        'validate',
        help='Validate spec without generating',
        description='Validate a YAML specification file without generating any output files.'
    )
    validate_parser.add_argument(
        '--input', '-i',
        required=True,
        help='Path to the input YAML specification file to validate'
    )
    validate_parser.set_defaults(func=cmd_validate)

    return parser


def main() -> NoReturn:
    """
    Main entry point for the CLI.

    Parses arguments and dispatches to the appropriate command handler.
    """
    parser = create_parser()
    args = parser.parse_args()

    if args.command is None:
        parser.print_help()
        sys.exit(0)

    exit_code = args.func(args)
    sys.exit(exit_code)


if __name__ == '__main__':
    main()
