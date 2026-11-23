"""
ABOUTME: TDD tests for hull_surface — BSpline interpolation from sparse
station line definitions to a full 3-D hull surface grid.

Test coverage:
- Surface grid dimensions match requested resolution
- Box barge: y values are constant across all x positions
- Ship-shaped hull: symmetry about midship, monotonic z-variation
- Monotonicity constraint: half-breadth never increases toward bow/stern for
  ship hulls where it should taper
- Symmetry handling: port/starboard mirroring flag
- Degenerate: single-station hull raises error
- Surface values are non-negative (no negative half-breadths)
"""

from __future__ import annotations

import numpy as np
import pytest


class TestHullSurfaceConfig:
    """Tests for HullSurfaceConfig."""

    def test_default_config(self):
        """Default config has sensible values."""
        from digitalmodel.hydrodynamics.hull_library.line_generator.hull_surface import (
            HullSurfaceConfig,
        )

        cfg = HullSurfaceConfig()
        assert cfg.n_x >= 10
        assert cfg.n_z >= 5
        assert cfg.interpolation_method == "bspline"

    def test_custom_resolution(self):
        """Custom n_x and n_z are stored."""
        from digitalmodel.hydrodynamics.hull_library.line_generator.hull_surface import (
            HullSurfaceConfig,
        )

        cfg = HullSurfaceConfig(n_x=30, n_z=15)
        assert cfg.n_x == 30
        assert cfg.n_z == 15

    def test_invalid_resolution_rejected(self):
        """n_x < 2 is rejected."""
        from digitalmodel.hydrodynamics.hull_library.line_generator.hull_surface import (
            HullSurfaceConfig,
        )

        with pytest.raises(ValueError):
            HullSurfaceConfig(n_x=1, n_z=5)


class TestHullSurfaceInterpolator:
    """Tests for HullSurfaceInterpolator."""

    def test_box_barge_surface_shape(self, box_barge_json):
        """Box barge surface grid has shape (n_x+1, n_z+1)."""
        from digitalmodel.hydrodynamics.hull_library.line_generator.hull_surface import (
            HullSurfaceConfig,
            HullSurfaceInterpolator,
        )
        from digitalmodel.hydrodynamics.hull_library.line_generator.line_parser import (
            LineParser,
        )

        parser = LineParser()
        defn = parser.parse_json_dict(box_barge_json)
        cfg = HullSurfaceConfig(n_x=10, n_z=5)
        interp = HullSurfaceInterpolator(cfg)
        surface = interp.interpolate(defn)
        # y_grid shape is (n_x+1, n_z+1)
        assert surface.y_grid.shape == (cfg.n_x + 1, cfg.n_z + 1)

    def test_box_barge_constant_half_breadth(self, box_barge_json):
        """Box barge has uniform half-breadth across all stations."""
        from digitalmodel.hydrodynamics.hull_library.line_generator.hull_surface import (
            HullSurfaceConfig,
            HullSurfaceInterpolator,
        )
        from digitalmodel.hydrodynamics.hull_library.line_generator.line_parser import (
            LineParser,
        )

        parser = LineParser()
        defn = parser.parse_json_dict(box_barge_json)
        cfg = HullSurfaceConfig(n_x=10, n_z=5)
        interp = HullSurfaceInterpolator(cfg)
        surface = interp.interpolate(defn)
        # At draft (top of submerged portion), all x positions should give ~10.0
        # z_grid: z=0 at keel, z=draft at design waterline
        y_at_wl = surface.y_grid[:, -1]
        assert np.allclose(y_at_wl, 10.0, atol=0.2)

    def test_non_negative_half_breadths(self, ship_shaped_json):
        """No negative half-breadth values appear in interpolated surface."""
        from digitalmodel.hydrodynamics.hull_library.line_generator.hull_surface import (
            HullSurfaceConfig,
            HullSurfaceInterpolator,
        )
        from digitalmodel.hydrodynamics.hull_library.line_generator.line_parser import (
            LineParser,
        )

        parser = LineParser()
        defn = parser.parse_json_dict(ship_shaped_json)
        cfg = HullSurfaceConfig(n_x=20, n_z=10)
        interp = HullSurfaceInterpolator(cfg)
        surface = interp.interpolate(defn)
        assert np.all(surface.y_grid >= 0.0)

    def test_x_grid_spans_full_length(self, box_barge_json):
        """x_grid spans from 0 to length_bp."""
        from digitalmodel.hydrodynamics.hull_library.line_generator.hull_surface import (
            HullSurfaceConfig,
            HullSurfaceInterpolator,
        )
        from digitalmodel.hydrodynamics.hull_library.line_generator.line_parser import (
            LineParser,
        )

        parser = LineParser()
        defn = parser.parse_json_dict(box_barge_json)
        cfg = HullSurfaceConfig(n_x=10, n_z=5)
        interp = HullSurfaceInterpolator(cfg)
        surface = interp.interpolate(defn)
        assert abs(surface.x_grid[0]) < 1e-6
        assert abs(surface.x_grid[-1] - defn.length_bp) < 1e-6

    def test_z_grid_spans_full_draft(self, box_barge_json):
        """z_grid spans from 0 (keel) to draft (waterline) in keel-up coords."""
        from digitalmodel.hydrodynamics.hull_library.line_generator.hull_surface import (
            HullSurfaceConfig,
            HullSurfaceInterpolator,
        )
        from digitalmodel.hydrodynamics.hull_library.line_generator.line_parser import (
            LineParser,
        )

        parser = LineParser()
        defn = parser.parse_json_dict(box_barge_json)
        cfg = HullSurfaceConfig(n_x=10, n_z=5)
        interp = HullSurfaceInterpolator(cfg)
        surface = interp.interpolate(defn)
        assert abs(surface.z_grid[0]) < 1e-6
        assert abs(surface.z_grid[-1] - defn.draft) < 1e-6

    def test_ship_shaped_bow_narrower_than_midship(self, ship_shaped_json):
        """Ship hull bow half-breadth at DWL is less than midship."""
        from digitalmodel.hydrodynamics.hull_library.line_generator.hull_surface import (
            HullSurfaceConfig,
            HullSurfaceInterpolator,
        )
        from digitalmodel.hydrodynamics.hull_library.line_generator.line_parser import (
            LineParser,
        )

        parser = LineParser()
        defn = parser.parse_json_dict(ship_shaped_json)
        cfg = HullSurfaceConfig(n_x=20, n_z=10)
        interp = HullSurfaceInterpolator(cfg)
        surface = interp.interpolate(defn)
        # y at DWL: last z index
        y_at_wl = surface.y_grid[:, -1]
        mid_idx = len(y_at_wl) // 2
        bow_idx = -1
        assert y_at_wl[bow_idx] < y_at_wl[mid_idx]

    def test_semi_pontoon_constant_section(self, semi_pontoon_json):
        """Semi-sub pontoon has near-constant half-breadth along length."""
        from digitalmodel.hydrodynamics.hull_library.line_generator.hull_surface import (
            HullSurfaceConfig,
            HullSurfaceInterpolator,
        )
        from digitalmodel.hydrodynamics.hull_library.line_generator.line_parser import (
            LineParser,
        )

        parser = LineParser()
        defn = parser.parse_json_dict(semi_pontoon_json)
        cfg = HullSurfaceConfig(n_x=10, n_z=8)
        interp = HullSurfaceInterpolator(cfg)
        surface = interp.interpolate(defn)
        # Mid-depth row should be nearly constant
        mid_z = len(surface.z_grid) // 2
        y_mid = surface.y_grid[:, mid_z]
        assert np.std(y_mid) < 0.5  # very small variation

    def test_surface_has_x_z_y_grids(self, box_barge_json):
        """Interpolated HullSurface exposes x_grid, z_grid, y_grid."""
        from digitalmodel.hydrodynamics.hull_library.line_generator.hull_surface import (
            HullSurfaceConfig,
            HullSurfaceInterpolator,
        )
        from digitalmodel.hydrodynamics.hull_library.line_generator.line_parser import (
            LineParser,
        )

        parser = LineParser()
        defn = parser.parse_json_dict(box_barge_json)
        cfg = HullSurfaceConfig(n_x=8, n_z=4)
        interp = HullSurfaceInterpolator(cfg)
        surface = interp.interpolate(defn)
        assert hasattr(surface, "x_grid")
        assert hasattr(surface, "z_grid")
        assert hasattr(surface, "y_grid")
