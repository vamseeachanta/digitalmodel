"""Shared fixtures for modular generator tests."""

from __future__ import annotations

from pathlib import Path

import pytest
import yaml


# Path constants
SPEC_DIR = Path(__file__).parent.parent.parent.parent.parent / (
    "docs/modules/orcaflex/pipeline/installation/floating/30in_pipeline"
)
SPEC_FILE = SPEC_DIR / "spec.yml"
FIXTURES_DIR = Path(__file__).parent / "fixtures"

# Source package path
SRC_PACKAGE = "digitalmodel.orcaflex.modular_generator"


@pytest.fixture
def spec_data() -> dict:
    """Load raw YAML data from the 30in pipeline spec."""
    with open(SPEC_FILE) as f:
        return yaml.safe_load(f)


@pytest.fixture
def validated_spec(spec_data):
    """Load and validate the 30in pipeline spec."""
    from digitalmodel.orcaflex.modular_generator.schema import ProjectInputSpec

    return ProjectInputSpec(**spec_data)


@pytest.fixture
def golden_output(validated_spec, tmp_path):
    """Generate output from validated spec and return as dict of file contents.

    Returns:
        Dict mapping filename -> parsed YAML content for each include file.
    """
    from digitalmodel.orcaflex.modular_generator import ModularModelGenerator

    # Generate to temp directory
    output_dir = tmp_path / "golden"
    generator = ModularModelGenerator(SPEC_FILE)
    generator.generate(output_dir)

    # Load all generated include files
    includes_dir = output_dir / "includes"
    result = {}
    for yml_file in sorted(includes_dir.glob("*.yml")):
        with open(yml_file) as f:
            result[yml_file.name] = yaml.safe_load(f)

    # Also load parameters and master
    params_file = output_dir / "inputs" / "parameters.yml"
    if params_file.exists():
        with open(params_file) as f:
            result["parameters.yml"] = yaml.safe_load(f)

    master_file = output_dir / "master.yml"
    if master_file.exists():
        result["master.yml"] = master_file.read_text()

    return result


@pytest.fixture
def builder_context():
    """Create a fresh BuilderContext instance."""
    from digitalmodel.orcaflex.modular_generator.builders.context import (
        BuilderContext,
    )

    return BuilderContext()
