"""Tests for BuilderRegistry decorator-based registration."""

from __future__ import annotations

import pytest


class TestBuilderRegistry:
    """Test the builder registry system."""

    def test_all_builders_registered(self):
        """Verify all 10 builders are registered."""
        from digitalmodel.modules.orcaflex.modular_generator.builders.registry import (
            BuilderRegistry,
        )
        # Force import of all builder modules to trigger registration
        import digitalmodel.modules.orcaflex.modular_generator.builders  # noqa: F401

        ordered = BuilderRegistry.get_ordered_builders()
        assert len(ordered) == 10
        file_names = [name for name, _ in ordered]
        assert "01_general.yml" in file_names
        assert "10_groups.yml" in file_names

    def test_execution_order(self):
        """Verify builders execute in correct dependency order."""
        from digitalmodel.modules.orcaflex.modular_generator.builders.registry import (
            BuilderRegistry,
        )
        import digitalmodel.modules.orcaflex.modular_generator.builders  # noqa: F401

        ordered = BuilderRegistry.get_ordered_builders()
        file_names = [name for name, _ in ordered]

        # Key ordering constraints:
        # var_data before line_types (coatings needed)
        assert file_names.index("02_var_data.yml") < file_names.index("05_line_types.yml")
        # supports before buoys (support types needed)
        assert file_names.index("13_supports.yml") < file_names.index("08_buoys.yml")
        # buoys before lines (end buoy name needed)
        assert file_names.index("08_buoys.yml") < file_names.index("07_lines.yml")
        # lines before groups (line names needed)
        assert file_names.index("07_lines.yml") < file_names.index("10_groups.yml")

    def test_get_include_order(self):
        """Verify include order matches expected list."""
        from digitalmodel.modules.orcaflex.modular_generator.builders.registry import (
            BuilderRegistry,
        )
        import digitalmodel.modules.orcaflex.modular_generator.builders  # noqa: F401

        include_order = BuilderRegistry.get_include_order()
        assert include_order == [
            "01_general.yml",
            "02_var_data.yml",
            "03_environment.yml",
            "05_line_types.yml",
            "13_supports.yml",
            "14_morison.yml",
            "09_shapes.yml",
            "08_buoys.yml",
            "07_lines.yml",
            "10_groups.yml",
        ]

    def test_get_specific_builder(self):
        """Verify we can look up a specific builder."""
        from digitalmodel.modules.orcaflex.modular_generator.builders.registry import (
            BuilderRegistry,
        )
        import digitalmodel.modules.orcaflex.modular_generator.builders  # noqa: F401

        builder_class = BuilderRegistry.get_builder("03_environment.yml")
        assert builder_class is not None
        assert builder_class.__name__ == "EnvironmentBuilder"

    def test_get_nonexistent_builder_returns_none(self):
        from digitalmodel.modules.orcaflex.modular_generator.builders.registry import (
            BuilderRegistry,
        )

        assert BuilderRegistry.get_builder("99_nonexistent.yml") is None
