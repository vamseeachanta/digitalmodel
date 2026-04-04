"""Tests for benchmark_input_reports backward-compat shim.

ABOUTME: Verifies that the benchmark_input_reports module correctly
re-exports functions from the split sub-modules.
"""
from __future__ import annotations


class TestBackwardCompatImports:
    """Verify all backward-compat re-exports are importable."""

    def test_import_build_input_comparison_html(self):
        from digitalmodel.hydrodynamics.diffraction.benchmark_input_reports import (
            build_input_comparison_html,
        )
        assert callable(build_input_comparison_html)

    def test_import_build_semantic_equivalence_html(self):
        from digitalmodel.hydrodynamics.diffraction.benchmark_input_reports import (
            build_semantic_equivalence_html,
        )
        assert callable(build_semantic_equivalence_html)

    def test_import_build_input_files_html(self):
        from digitalmodel.hydrodynamics.diffraction.benchmark_input_reports import (
            build_input_files_html,
        )
        assert callable(build_input_files_html)

    def test_import_build_mesh_schematic_html(self):
        from digitalmodel.hydrodynamics.diffraction.benchmark_input_reports import (
            build_mesh_schematic_html,
        )
        assert callable(build_mesh_schematic_html)

    def test_all_exports_listed(self):
        from digitalmodel.hydrodynamics.diffraction import benchmark_input_reports
        expected = {
            "build_input_comparison_html",
            "build_semantic_equivalence_html",
            "build_input_files_html",
            "build_mesh_schematic_html",
        }
        assert expected.issubset(set(benchmark_input_reports.__all__))

    def test_functions_are_same_objects(self):
        """Re-exports should be the exact same function objects."""
        from digitalmodel.hydrodynamics.diffraction.benchmark_input_reports import (
            build_input_comparison_html as from_shim,
        )
        from digitalmodel.hydrodynamics.diffraction.benchmark_input_comparison import (
            build_input_comparison_html as from_source,
        )
        assert from_shim is from_source
