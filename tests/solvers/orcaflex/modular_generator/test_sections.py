"""Tests for VariableResolver and ModularModelGenerator.generate_with_overrides()."""
from __future__ import annotations

from pathlib import Path

import pytest
import yaml


class TestVariableResolver:
    """Tests for VariableResolver.resolve() static method."""

    def _resolver(self):
        from digitalmodel.solvers.orcaflex.modular_generator.sections import (
            VariableResolver,
        )

        return VariableResolver

    def test_resolve_simple_token(self):
        """${water_depth} replaced with its value."""
        resolver = self._resolver()
        template = "water_depth: ${water_depth}"
        result = resolver.resolve(template, {"water_depth": 30})
        assert result == "water_depth: 30"

    def test_resolve_format_spec(self):
        """${water_depth:.0f} formats the value as integer string."""
        resolver = self._resolver()
        template = "water_depth: ${water_depth:.0f}"
        result = resolver.resolve(template, {"water_depth": 30.567})
        assert result == "water_depth: 31"

    def test_resolve_whole_number_float_auto_format(self):
        """Whole-number float (30.0) auto-formats as '30' without explicit format spec."""
        resolver = self._resolver()
        template = "depth: ${water_depth}"
        result = resolver.resolve(template, {"water_depth": 30.0})
        assert result == "depth: 30"

    def test_resolve_unresolved_token_raises(self):
        """Leftover ${unknown} token raises ValueError."""
        resolver = self._resolver()
        template = "depth: ${water_depth}\nname: ${unknown_var}"
        with pytest.raises(ValueError, match="unresolved"):
            resolver.resolve(template, {"water_depth": 30})

    def test_resolve_post_substitution_yaml_validation(self):
        """Broken YAML structure after substitution raises ValueError."""
        resolver = self._resolver()
        # After substitution this produces invalid YAML (unbalanced braces/bad structure)
        template = "key: ${value}"
        with pytest.raises(ValueError, match="[Yy]AML"):
            resolver.resolve(template, {"value": "a: [b: c"})

    def test_resolve_no_tokens_passthrough(self):
        """String without tokens is returned unchanged."""
        resolver = self._resolver()
        template = "water_depth: 30\nname: test"
        result = resolver.resolve(template, {"water_depth": 999})
        assert result == template

    def test_resolve_multiple_tokens(self):
        """Multiple different tokens in same string all resolved."""
        resolver = self._resolver()
        template = "depth: ${water_depth}\nspeed: ${current_speed}"
        result = resolver.resolve(
            template, {"water_depth": 30, "current_speed": 1.5}
        )
        assert result == "depth: 30\nspeed: 1.5"

    def test_resolve_non_float_values(self):
        """String values substituted correctly."""
        resolver = self._resolver()
        template = "name: ${project_name}\ntype: ${install_type}"
        result = resolver.resolve(
            template, {"project_name": "Pipeline_A", "install_type": "floating"}
        )
        assert result == "name: Pipeline_A\ntype: floating"


class TestGenerateWithOverrides:
    """Tests for ModularModelGenerator.generate_with_overrides()."""

    def test_generate_with_overrides_no_overrides(self, validated_spec, tmp_path):
        """With no overrides, produces same master.yml as generate()."""
        from digitalmodel.solvers.orcaflex.modular_generator import (
            ModularModelGenerator,
        )

        # Standard generate
        std_dir = tmp_path / "standard"
        gen = ModularModelGenerator.from_spec(validated_spec)
        gen.generate(std_dir)

        # generate_with_overrides with no overrides
        ovr_dir = tmp_path / "overrides"
        gen2 = ModularModelGenerator.from_spec(validated_spec)
        result = gen2.generate_with_overrides(ovr_dir)

        # Compare master.yml content
        std_master = (std_dir / "master.yml").read_text()
        ovr_master = (ovr_dir / "master.yml").read_text()
        assert std_master == ovr_master

    def test_generate_with_overrides_disabled_section(self, validated_spec, tmp_path):
        """Disabled section's file is not generated."""
        from digitalmodel.solvers.orcaflex.modular_generator import (
            ModularModelGenerator,
        )
        from digitalmodel.solvers.orcaflex.modular_generator.schema.campaign import (
            InstallationSection,
        )

        sections = [
            InstallationSection(builder_file="08_buoys.yml", enabled=False),
        ]

        gen = ModularModelGenerator.from_spec(validated_spec)
        result = gen.generate_with_overrides(tmp_path / "out", sections=sections)

        # The disabled file should not exist in includes
        includes_dir = tmp_path / "out" / "includes"
        assert not (includes_dir / "08_buoys.yml").exists()

    def test_generate_with_overrides_custom_template(self, validated_spec, tmp_path):
        """Template loaded, vars resolved, written to correct file."""
        from digitalmodel.solvers.orcaflex.modular_generator import (
            ModularModelGenerator,
        )
        from digitalmodel.solvers.orcaflex.modular_generator.schema.campaign import (
            InstallationSection,
        )

        # Create a template file
        template_dir = tmp_path / "templates"
        template_dir.mkdir()
        template_file = template_dir / "custom_buoys.yml"
        template_file.write_text("buoy_depth: ${water_depth}\nbuoy_name: ${name}")

        sections = [
            InstallationSection(
                builder_file="08_buoys.yml",
                template="custom_buoys.yml",
                variables={"water_depth": 45.0, "name": "TestBuoy"},
            ),
        ]

        out_dir = tmp_path / "out"
        gen = ModularModelGenerator.from_spec(validated_spec)
        result = gen.generate_with_overrides(
            out_dir,
            sections=sections,
            variables={"water_depth": 45.0, "name": "TestBuoy"},
            template_base_dir=template_dir,
        )

        # Check the file was written with resolved content
        buoys_file = out_dir / "includes" / "08_buoys.yml"
        assert buoys_file.exists()
        content = buoys_file.read_text()
        assert "buoy_depth: 45" in content
        assert "buoy_name: TestBuoy" in content

    def test_generate_with_overrides_returns_generation_result(
        self, validated_spec, tmp_path
    ):
        """Returns GenerationResult with correct fields."""
        from digitalmodel.solvers.orcaflex.modular_generator import (
            GenerationResult,
            ModularModelGenerator,
        )

        out_dir = tmp_path / "out"
        gen = ModularModelGenerator.from_spec(validated_spec)
        result = gen.generate_with_overrides(out_dir)

        assert isinstance(result, GenerationResult)
        assert result.master_file == out_dir / "master.yml"
        assert isinstance(result.include_files, list)
        assert len(result.include_files) > 0
        assert isinstance(result.warnings, list)

    def test_generate_with_overrides_master_excludes_disabled(
        self, validated_spec, tmp_path
    ):
        """master.yml does not list disabled sections."""
        from digitalmodel.solvers.orcaflex.modular_generator import (
            ModularModelGenerator,
        )
        from digitalmodel.solvers.orcaflex.modular_generator.schema.campaign import (
            InstallationSection,
        )

        sections = [
            InstallationSection(builder_file="08_buoys.yml", enabled=False),
        ]

        out_dir = tmp_path / "out"
        gen = ModularModelGenerator.from_spec(validated_spec)
        gen.generate_with_overrides(out_dir, sections=sections)

        master_content = (out_dir / "master.yml").read_text()
        assert "08_buoys.yml" not in master_content
