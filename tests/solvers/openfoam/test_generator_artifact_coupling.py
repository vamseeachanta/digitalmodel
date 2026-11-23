#!/usr/bin/env python3
"""
ABOUTME: Anti-drift coupling tests binding two tracked generated artifacts to the
generator literals they come from (dm#1574, the defect class of dm#1888/#1903).
Each test extracts a static literal from a generator's source and asserts the
committed artifact carries exactly that text, so a page or manifest can never
silently diverge from the generator that produces it. No test names a protected
value; each reads the current literal from the generator itself.
"""

import ast
import json
from pathlib import Path

import pytest

_REPO = Path(__file__).resolve().parents[3]

_BENCHMARK_GENERATOR = _REPO / "scripts" / "cfd" / "run_sloshing_3d_benchmark.py"
_BENCHMARK_MANIFEST = _REPO / "docs" / "api" / "cfd" / "sloshing-3d-benchmark.json"

_EXPLORER_GENERATOR = (
    _REPO / "scripts" / "capabilities" / "build_sloshing_explorer.py"
)
_EXPLORER_PAGE = _REPO / "docs" / "api" / "structural" / "sloshing-explorer.html"

# Stable anchor for the prose region that carried an organization name. The
# anchor is deliberately generic wording, not an identifier.
_PROSE_ANCHOR = "This is the tiering behind"


def _manifest_geometry_literal() -> str:
    """The ``geometry`` string literal in the benchmark generator's manifest.

    Parsed rather than imported: the generator pulls in the solver stack and
    must not be executed to check a static string.
    """
    tree = ast.parse(_BENCHMARK_GENERATOR.read_text(encoding="utf-8"))
    found = [
        value.value
        for node in ast.walk(tree)
        if isinstance(node, ast.Dict)
        for key, value in zip(node.keys, node.values)
        if isinstance(key, ast.Constant)
        and key.value == "geometry"
        and isinstance(value, ast.Constant)
        and isinstance(value.value, str)
    ]
    assert len(found) == 1, f"expected exactly one geometry literal, got {len(found)}"
    return found[0]


def _explorer_prose_sentence() -> str:
    """The sentence in the explorer generator's template starting at the anchor."""
    source = _EXPLORER_GENERATOR.read_text(encoding="utf-8")
    start = source.find(_PROSE_ANCHOR)
    assert start != -1, f"anchor {_PROSE_ANCHOR!r} not found in the explorer generator"
    assert (
        source.find(_PROSE_ANCHOR, start + 1) == -1
    ), "anchor is not unique in the explorer generator"
    end = source.find(".", start + len(_PROSE_ANCHOR))
    assert end != -1, "anchor sentence is unterminated in the explorer generator"
    return source[start : end + 1]


class TestBenchmarkManifestCoupling:
    """The measurement manifest tracks its generator's static literal.

    The harness embeds live solver timings and cannot be re-run without the
    CFD node, so this coupling test is the anti-drift mechanism for the one
    field this issue changes.
    """

    def test_manifest_exists(self):
        assert _BENCHMARK_MANIFEST.is_file()

    def test_benchmark_manifest_field_tracks_generator_literal(self):
        manifest = json.loads(_BENCHMARK_MANIFEST.read_text(encoding="utf-8"))
        assert manifest["meta"]["geometry"] == _manifest_geometry_literal()


class TestExplorerPageCoupling:
    """The capability page tracks its generator's template prose.

    This page is additionally covered by the repository-wide generated-HTML
    drift gate; this test localises a failure to the prose region this issue
    changes rather than reporting a whole-page diff.
    """

    def test_page_exists(self):
        assert _EXPLORER_PAGE.is_file()

    def test_explorer_page_prose_tracks_generator_literal(self):
        page = _EXPLORER_PAGE.read_text(encoding="utf-8")
        assert page.count(_explorer_prose_sentence()) == 1


class TestGeneratorsCarryNoRemovedSymbol:
    """Neither generator references the removed project-coded factory shape."""

    @pytest.mark.parametrize(
        "generator", [_BENCHMARK_GENERATOR, _EXPLORER_GENERATOR], ids=lambda p: p.name
    )
    def test_generator_has_no_removed_factory_reference(self, generator):
        assert "_default_taps" not in generator.read_text(encoding="utf-8")
