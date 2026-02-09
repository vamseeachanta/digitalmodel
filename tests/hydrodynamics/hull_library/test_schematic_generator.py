"""Tests for hull schematic generator -- SVG views of hull profiles."""

import pytest
from pathlib import Path
import tempfile


class TestSchematicGenerator:
    """Tests for SchematicGenerator."""

    def _make_box_profile(self):
        from digitalmodel.hydrodynamics.hull_library.profile_schema import (
            HullProfile,
            HullStation,
            HullType,
        )

        stations = [
            HullStation(
                x_position=0.0,
                waterline_offsets=[(0.0, 5.0), (4.0, 5.0)],
            ),
            HullStation(
                x_position=25.0,
                waterline_offsets=[(0.0, 5.0), (4.0, 5.0)],
            ),
            HullStation(
                x_position=50.0,
                waterline_offsets=[(0.0, 5.0), (4.0, 5.0)],
            ),
        ]
        return HullProfile(
            name="test_box",
            hull_type=HullType.BARGE,
            stations=stations,
            length_bp=50.0,
            beam=10.0,
            draft=4.0,
            depth=6.0,
            source="test",
        )

    def test_profile_view_returns_svg(self):
        from digitalmodel.hydrodynamics.hull_library.schematic_generator import (
            SchematicGenerator,
        )

        gen = SchematicGenerator()
        svg = gen.profile_view(self._make_box_profile())
        assert isinstance(svg, str)
        assert svg.startswith("<svg")
        assert "</svg>" in svg

    def test_plan_view_returns_svg(self):
        from digitalmodel.hydrodynamics.hull_library.schematic_generator import (
            SchematicGenerator,
        )

        gen = SchematicGenerator()
        svg = gen.plan_view(self._make_box_profile())
        assert isinstance(svg, str)
        assert "<svg" in svg

    def test_body_plan_returns_svg(self):
        from digitalmodel.hydrodynamics.hull_library.schematic_generator import (
            SchematicGenerator,
        )

        gen = SchematicGenerator()
        svg = gen.body_plan(self._make_box_profile())
        assert isinstance(svg, str)
        assert "<svg" in svg

    def test_profile_view_contains_waterline(self):
        from digitalmodel.hydrodynamics.hull_library.schematic_generator import (
            SchematicGenerator,
        )

        gen = SchematicGenerator()
        svg = gen.profile_view(self._make_box_profile())
        # Should contain waterline indicator
        assert "waterline" in svg.lower() or "WL" in svg

    def test_profile_view_contains_title(self):
        from digitalmodel.hydrodynamics.hull_library.schematic_generator import (
            SchematicGenerator,
        )

        gen = SchematicGenerator()
        svg = gen.profile_view(self._make_box_profile())
        assert "test_box" in svg

    def test_body_plan_contains_station_lines(self):
        from digitalmodel.hydrodynamics.hull_library.schematic_generator import (
            SchematicGenerator,
        )

        gen = SchematicGenerator()
        svg = gen.body_plan(self._make_box_profile())
        # Should contain path or line elements for stations
        assert "<path" in svg or "<line" in svg or "<polyline" in svg

    def test_generate_all_views(self):
        """generate_all returns dict with all three views."""
        from digitalmodel.hydrodynamics.hull_library.schematic_generator import (
            SchematicGenerator,
        )

        gen = SchematicGenerator()
        views = gen.generate_all(self._make_box_profile())
        assert "profile_view" in views
        assert "plan_view" in views
        assert "body_plan" in views
        for key, svg in views.items():
            assert "<svg" in svg

    def test_save_svg(self):
        """SVG can be saved to file."""
        from digitalmodel.hydrodynamics.hull_library.schematic_generator import (
            SchematicGenerator,
        )

        gen = SchematicGenerator()
        svg = gen.profile_view(self._make_box_profile())
        with tempfile.TemporaryDirectory() as tmpdir:
            path = Path(tmpdir) / "profile.svg"
            path.write_text(svg)
            assert path.exists()
            content = path.read_text()
            assert "<svg" in content

    def test_save_all(self):
        """save_all writes all SVGs to directory."""
        from digitalmodel.hydrodynamics.hull_library.schematic_generator import (
            SchematicGenerator,
        )

        gen = SchematicGenerator()
        profile = self._make_box_profile()
        with tempfile.TemporaryDirectory() as tmpdir:
            paths = gen.save_all(profile, Path(tmpdir))
            assert len(paths) == 3
            for p in paths.values():
                assert Path(p).exists()
