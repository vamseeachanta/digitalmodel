"""Tests for hull schematic generator -- SVG views of hull profiles."""

import pytest
from pathlib import Path
import tempfile


class TestSchematicGenerator:
    """Tests for SchematicGenerator."""

    def test_profile_view_returns_svg(self, box_profile):
        from digitalmodel.hydrodynamics.hull_library.schematic_generator import (
            SchematicGenerator,
        )

        gen = SchematicGenerator()
        svg = gen.profile_view(box_profile)
        assert isinstance(svg, str)
        assert svg.startswith("<svg")
        assert "</svg>" in svg

    def test_plan_view_returns_svg(self, box_profile):
        from digitalmodel.hydrodynamics.hull_library.schematic_generator import (
            SchematicGenerator,
        )

        gen = SchematicGenerator()
        svg = gen.plan_view(box_profile)
        assert isinstance(svg, str)
        assert "<svg" in svg

    def test_body_plan_returns_svg(self, box_profile):
        from digitalmodel.hydrodynamics.hull_library.schematic_generator import (
            SchematicGenerator,
        )

        gen = SchematicGenerator()
        svg = gen.body_plan(box_profile)
        assert isinstance(svg, str)
        assert "<svg" in svg

    def test_profile_view_contains_waterline(self, box_profile):
        from digitalmodel.hydrodynamics.hull_library.schematic_generator import (
            SchematicGenerator,
        )

        gen = SchematicGenerator()
        svg = gen.profile_view(box_profile)
        # Should contain waterline indicator
        assert "waterline" in svg.lower() or "WL" in svg

    def test_profile_view_contains_title(self, box_profile):
        from digitalmodel.hydrodynamics.hull_library.schematic_generator import (
            SchematicGenerator,
        )

        gen = SchematicGenerator()
        svg = gen.profile_view(box_profile)
        assert "unit_box" in svg

    def test_body_plan_contains_station_lines(self, box_profile):
        from digitalmodel.hydrodynamics.hull_library.schematic_generator import (
            SchematicGenerator,
        )

        gen = SchematicGenerator()
        svg = gen.body_plan(box_profile)
        # Should contain path or line elements for stations
        assert "<path" in svg or "<line" in svg or "<polyline" in svg

    def test_generate_all_views(self, box_profile):
        """generate_all returns dict with all three views."""
        from digitalmodel.hydrodynamics.hull_library.schematic_generator import (
            SchematicGenerator,
        )

        gen = SchematicGenerator()
        views = gen.generate_all(box_profile)
        assert "profile_view" in views
        assert "plan_view" in views
        assert "body_plan" in views
        for key, svg in views.items():
            assert "<svg" in svg

    def test_save_svg(self, box_profile):
        """SVG can be saved to file."""
        from digitalmodel.hydrodynamics.hull_library.schematic_generator import (
            SchematicGenerator,
        )

        gen = SchematicGenerator()
        svg = gen.profile_view(box_profile)
        with tempfile.TemporaryDirectory() as tmpdir:
            path = Path(tmpdir) / "profile.svg"
            path.write_text(svg)
            assert path.exists()
            content = path.read_text()
            assert "<svg" in content

    def test_save_all(self, box_profile):
        """save_all writes all SVGs to directory."""
        from digitalmodel.hydrodynamics.hull_library.schematic_generator import (
            SchematicGenerator,
        )

        gen = SchematicGenerator()
        with tempfile.TemporaryDirectory() as tmpdir:
            paths = gen.save_all(box_profile, Path(tmpdir))
            assert len(paths) == 3
            for p in paths.values():
                assert Path(p).exists()
