"""Tests for ModularModelGenerator.from_spec() classmethod."""
from __future__ import annotations

from pathlib import Path

import pytest
import yaml

SPEC_DIR = Path(__file__).parent.parent.parent.parent.parent / (
    "docs/domains/orcaflex/pipeline/installation/floating/30in_pipeline"
)
SPEC_FILE = SPEC_DIR / "spec.yml"


class TestFromSpec:
    """Tests for ModularModelGenerator.from_spec()."""

    def test_from_spec_accepts_project_input_spec(self, validated_spec):
        """from_spec() accepts a ProjectInputSpec object."""
        from digitalmodel.solvers.orcaflex.modular_generator import ModularModelGenerator

        generator = ModularModelGenerator.from_spec(validated_spec)
        assert generator.spec is validated_spec

    def test_from_spec_generates_identical_output(self, validated_spec, tmp_path):
        """from_spec() produces same output as file-based constructor."""
        from digitalmodel.solvers.orcaflex.modular_generator import ModularModelGenerator

        # Generate via file-based path
        file_dir = tmp_path / "file_based"
        file_generator = ModularModelGenerator(SPEC_FILE)
        file_generator.generate(file_dir)

        # Generate via from_spec
        spec_dir = tmp_path / "spec_based"
        spec_generator = ModularModelGenerator.from_spec(validated_spec)
        spec_generator.generate(spec_dir)

        # Compare include files
        file_includes = sorted((file_dir / "includes").glob("*.yml"))
        spec_includes = sorted((spec_dir / "includes").glob("*.yml"))

        assert len(file_includes) == len(spec_includes)

        for file_yml, spec_yml in zip(file_includes, spec_includes):
            assert file_yml.name == spec_yml.name
            with open(file_yml) as f:
                file_data = yaml.safe_load(f)
            with open(spec_yml) as f:
                spec_data = yaml.safe_load(f)
            assert file_data == spec_data, f"Mismatch in {file_yml.name}"

    def test_from_spec_spec_file_is_none(self, validated_spec):
        """from_spec() sets spec_file to None (no file source)."""
        from digitalmodel.solvers.orcaflex.modular_generator import ModularModelGenerator

        generator = ModularModelGenerator.from_spec(validated_spec)
        assert generator.spec_file is None

    def test_file_based_constructor_still_works(self, tmp_path):
        """Existing file-based constructor is unaffected."""
        from digitalmodel.solvers.orcaflex.modular_generator import ModularModelGenerator

        generator = ModularModelGenerator(SPEC_FILE)
        assert generator.spec is not None
        assert generator.spec_file == Path(SPEC_FILE)

        output_dir = tmp_path / "output"
        generator.generate(output_dir)
        assert (output_dir / "master.yml").exists()
