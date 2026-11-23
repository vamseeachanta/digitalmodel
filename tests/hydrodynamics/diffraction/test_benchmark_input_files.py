"""Tests for benchmark_input_files module.

ABOUTME: Tests for build_input_files_html function which renders scrollable
input file previews for each solver.
"""
from __future__ import annotations

from pathlib import Path
from typing import Any, Dict

import pytest

from digitalmodel.hydrodynamics.diffraction.benchmark_input_files import (
    build_input_files_html,
)


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


def _make_metadata_with_file(
    tmp_path: Path,
    solver_names: list[str],
    file_content: str = "LINE1\nLINE2\nLINE3\n",
    solver_type_key: str | None = None,
) -> Dict[str, Dict[str, Any]]:
    """Create solver_metadata with temporary input files."""
    meta: Dict[str, Dict[str, Any]] = {}
    for i, solver in enumerate(solver_names):
        fpath = tmp_path / f"input_{solver.replace(' ', '_')}_{i}.txt"
        fpath.write_text(file_content, encoding="utf-8")
        entry: Dict[str, Any] = {"input_file": str(fpath)}
        if solver_type_key:
            entry["solver_type"] = solver_type_key
        meta[solver] = entry
    return meta


# ---------------------------------------------------------------------------
# Tests: build_input_files_html
# ---------------------------------------------------------------------------


class TestBuildInputFilesHtml:
    """Tests for build_input_files_html."""

    def test_returns_html_string(self, tmp_path):
        names = ["SolverA"]
        meta = _make_metadata_with_file(tmp_path, names)
        html = build_input_files_html(names, meta)
        assert isinstance(html, str)
        assert "Input Files" in html

    def test_contains_file_content(self, tmp_path):
        content = "SOLVER_PARAM=42\nMESH_FILE=hull.gdf\n"
        names = ["SolverA"]
        meta = _make_metadata_with_file(tmp_path, names, file_content=content)
        html = build_input_files_html(names, meta)
        assert "SOLVER_PARAM=42" in html
        assert "MESH_FILE=hull.gdf" in html

    def test_html_escapes_content(self, tmp_path):
        content = "<script>alert('xss')</script>\n"
        names = ["SolverA"]
        meta = _make_metadata_with_file(tmp_path, names, file_content=content)
        html = build_input_files_html(names, meta)
        # Raw <script> should be escaped
        assert "<script>alert" not in html
        assert "&lt;script&gt;" in html

    def test_multiple_solvers(self, tmp_path):
        names = ["AQWA", "OrcaWave"]
        meta = _make_metadata_with_file(tmp_path, names)
        html = build_input_files_html(names, meta)
        assert "AQWA" in html
        assert "OrcaWave" in html

    def test_no_files_returns_empty(self):
        names = ["SolverA"]
        meta: Dict[str, Dict[str, Any]] = {"SolverA": {}}
        html = build_input_files_html(names, meta)
        assert html == ""

    def test_missing_file_skipped(self, tmp_path):
        names = ["SolverA"]
        meta: Dict[str, Dict[str, Any]] = {
            "SolverA": {"input_file": str(tmp_path / "nonexistent.txt")}
        }
        html = build_input_files_html(names, meta)
        assert html == ""

    def test_contains_line_numbers(self, tmp_path):
        content = "line1\nline2\nline3\n"
        names = ["SolverA"]
        meta = _make_metadata_with_file(tmp_path, names, file_content=content)
        html = build_input_files_html(names, meta)
        # Content is wrapped in <span class="line"> tags
        assert 'class="line"' in html

    def test_file_path_shown(self, tmp_path):
        names = ["SolverA"]
        meta = _make_metadata_with_file(tmp_path, names)
        html = build_input_files_html(names, meta)
        # The file path should appear in the header
        assert "input_SolverA_0.txt" in html

    def test_truncation_for_large_files(self, tmp_path):
        # Create a file with >2000 lines
        content = "\n".join(f"line {i}" for i in range(2500))
        names = ["SolverA"]
        meta = _make_metadata_with_file(tmp_path, names, file_content=content)
        html = build_input_files_html(names, meta)
        assert "truncated" in html.lower()

    def test_open_in_new_window_button(self, tmp_path):
        names = ["SolverA"]
        meta = _make_metadata_with_file(tmp_path, names)
        html = build_input_files_html(names, meta)
        assert "Open in New Window" in html

    def test_file_viewer_css_class(self, tmp_path):
        names = ["SolverA"]
        meta = _make_metadata_with_file(tmp_path, names)
        html = build_input_files_html(names, meta)
        assert "file-viewer" in html

    def test_semantic_equivalence_embedded(self, tmp_path):
        """When metadata has _semantic_equivalence, it should be rendered."""
        names = ["SolverA", "SolverB"]
        meta = _make_metadata_with_file(tmp_path, names)
        meta["SolverA"]["_semantic_equivalence"] = {
            "match_count": 10,
            "cosmetic_count": 1,
            "convention_count": 0,
            "significant_count": 0,
            "diffs": [],
        }
        html = build_input_files_html(names, meta)
        assert "Semantic Equivalence" in html
