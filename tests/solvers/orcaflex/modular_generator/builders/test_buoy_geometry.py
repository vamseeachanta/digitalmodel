"""Tests for shared _buoy_geometry constants extracted from buoys_builder."""
from __future__ import annotations


class TestBuoyGeometryConstants:
    def test_default_wireframe_vertices_extracted_verbatim(self):
        from digitalmodel.solvers.orcaflex.modular_generator.builders._buoy_geometry import (
            DEFAULT_WIREFRAME_VERTICES,
        )
        from digitalmodel.solvers.orcaflex.modular_generator.builders.buoys_builder import (
            DEFAULT_WIREFRAME_VERTICES as ORIGINAL,
        )
        assert DEFAULT_WIREFRAME_VERTICES == ORIGINAL
        assert len(DEFAULT_WIREFRAME_VERTICES) == 8

    def test_default_wireframe_edges_extracted_verbatim(self):
        from digitalmodel.solvers.orcaflex.modular_generator.builders._buoy_geometry import (
            DEFAULT_WIREFRAME_EDGES,
        )
        from digitalmodel.solvers.orcaflex.modular_generator.builders.buoys_builder import (
            DEFAULT_WIREFRAME_EDGES as ORIGINAL,
        )
        assert DEFAULT_WIREFRAME_EDGES == ORIGINAL
        assert len(DEFAULT_WIREFRAME_EDGES) == 12
