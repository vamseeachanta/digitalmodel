#!/usr/bin/env python3
"""Unit tests for the reusable digitalmodel.reporting block library (#1018)."""

from __future__ import annotations

from pathlib import Path

import pytest

from digitalmodel.reporting import (
    ReportBackbone,
    ReportBlock,
    ReportBlockFn,
    ReportDataModel,
    ReportRenderer,
    ReportSection,
    SectionMode,
)


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


def _section(key, mode, fragment=None):
    """A simple section whose builder emits a keyed marker (or a fragment)."""

    def _builder(data, **kwargs):
        if fragment is not None:
            return fragment
        return f"<div>{key}</div>"

    return ReportSection(key=key, label=key.title(), mode=mode, builder=_builder)


# ---------------------------------------------------------------------------
# ReportBackbone: ordering
# ---------------------------------------------------------------------------


class TestBackboneOrdering:
    def test_sections_render_in_declared_order(self):
        backbone = ReportBackbone(
            title="T",
            sections=[
                _section("a", SectionMode.ALWAYS),
                _section("b", SectionMode.ALWAYS),
                _section("c", SectionMode.ALWAYS),
            ],
        )
        out = backbone.render(object())
        assert out == ["<div>a</div>", "<div>b</div>", "<div>c</div>"]

    def test_empty_builder_output_is_omitted(self):
        backbone = ReportBackbone(
            title="T",
            sections=[
                _section("a", SectionMode.ALWAYS),
                _section("blank", SectionMode.ALWAYS, fragment=""),
                _section("c", SectionMode.ALWAYS),
            ],
        )
        out = backbone.render(object())
        assert out == ["<div>a</div>", "<div>c</div>"]

    def test_builder_receives_mode_and_kwargs(self):
        seen = {}

        def _builder(data, *, mode=None, benchmark_sections=None, flavor=None, **_):
            seen["mode"] = mode
            seen["flavor"] = flavor
            return "<x/>"

        backbone = ReportBackbone(
            title="T",
            sections=[ReportSection("x", "X", SectionMode.ALWAYS, _builder)],
        )
        backbone.render(object(), mode="compact", flavor="spicy")
        assert seen == {"mode": "compact", "flavor": "spicy"}


# ---------------------------------------------------------------------------
# ReportBackbone: COMPACT_SKIP
# ---------------------------------------------------------------------------


class TestCompactSkip:
    def _backbone(self):
        return ReportBackbone(
            title="T",
            sections=[
                _section("always", SectionMode.ALWAYS),
                _section("detail", SectionMode.COMPACT_SKIP),
            ],
        )

    def test_full_mode_includes_compact_skip(self):
        out = self._backbone().render(object(), mode="full")
        assert out == ["<div>always</div>", "<div>detail</div>"]

    def test_compact_mode_drops_compact_skip(self):
        out = self._backbone().render(object(), mode="compact")
        assert out == ["<div>always</div>"]

    def test_benchmark_only_renders_in_compact_mode(self):
        backbone = ReportBackbone(
            title="T",
            sections=[
                _section("always", SectionMode.ALWAYS),
                _section("detail", SectionMode.COMPACT_SKIP),
                _section("bench", SectionMode.BENCHMARK_ONLY),
            ],
        )
        out = backbone.render(
            object(), mode="compact", benchmark_sections={"bench": "frag"}
        )
        # COMPACT_SKIP dropped, BENCHMARK_ONLY still renders.
        assert out == ["<div>always</div>", "<div>bench</div>"]


# ---------------------------------------------------------------------------
# ReportBackbone: BENCHMARK_ONLY + benchmark injection
# ---------------------------------------------------------------------------


class TestBenchmark:
    def test_benchmark_only_skipped_without_fragment(self):
        backbone = ReportBackbone(
            title="T",
            sections=[_section("bench", SectionMode.BENCHMARK_ONLY)],
        )
        assert backbone.render(object()) == []
        assert backbone.render(object(), benchmark_sections={}) == []

    def test_benchmark_only_rendered_with_fragment(self):
        # A typical benchmark builder wraps the fragment for its key.
        def _wrap(data, *, benchmark_sections=None, **_):
            frag = (benchmark_sections or {}).get("bench")
            return f'<div class="section">{frag}</div>' if frag else ""

        backbone = ReportBackbone(
            title="T",
            sections=[ReportSection("bench", "B", SectionMode.BENCHMARK_ONLY, _wrap)],
        )
        out = backbone.render(object(), benchmark_sections={"bench": "<p>X</p>"})
        assert out == ['<div class="section"><p>X</p></div>']

    def test_empty_string_fragment_omitted(self):
        def _wrap(data, *, benchmark_sections=None, **_):
            frag = (benchmark_sections or {}).get("bench")
            return f'<div class="section">{frag}</div>' if frag else ""

        backbone = ReportBackbone(
            title="T",
            sections=[ReportSection("bench", "B", SectionMode.BENCHMARK_ONLY, _wrap)],
        )
        out = backbone.render(object(), benchmark_sections={"bench": ""})
        assert out == []

    def test_injection_after_matching_regular_section(self):
        """A fragment keyed to a rendered regular section is appended after it."""
        backbone = ReportBackbone(
            title="T",
            sections=[
                _section("intro", SectionMode.ALWAYS),
                _section("outro", SectionMode.ALWAYS),
            ],
        )
        out = backbone.render(
            object(), benchmark_sections={"intro": "<p>injected</p>"}
        )
        assert out == [
            "<div>intro</div>",
            '<div class="section"><p>injected</p></div>',
            "<div>outro</div>",
        ]

    def test_no_injection_when_key_absent(self):
        backbone = ReportBackbone(
            title="T", sections=[_section("intro", SectionMode.ALWAYS)]
        )
        out = backbone.render(
            object(), benchmark_sections={"other": "<p>nope</p>"}
        )
        assert out == ["<div>intro</div>"]


# ---------------------------------------------------------------------------
# ReportRenderer
# ---------------------------------------------------------------------------


class TestReportRenderer:
    def test_render_html_structure(self):
        renderer = ReportRenderer()
        html = renderer.render_html(
            ["<div>one</div>", "<div>two</div>"],
            title="My Report",
            css="  body { color: red; }\n",
            plotlyjs_src="<script></script>",
        )
        assert html.startswith("<!DOCTYPE html>\n")
        assert html.endswith("</html>")
        assert "<title>My Report</title>" in html
        assert "<script></script>\n<style>" in html
        assert "  body { color: red; }\n</style>" in html
        assert '<div class="container">\n<div>one</div><div>two</div>\n</div>' in html

    def test_render_html_defaults_no_plotly_line_blank(self):
        renderer = ReportRenderer()
        html = renderer.render_html([], title="T")
        # plotlyjs_src defaults to "" -> the line is blank, then <style>.
        assert "<title>T</title>\n\n<style>\n</style>" in html
        assert '<div class="container">' in html

    def test_container_max_width_inline_style(self):
        renderer = ReportRenderer()
        html = renderer.render_html(
            [], title="T", container_class="wrap", container_max_width="900px"
        )
        assert '<div class="wrap" style="max-width: 900px;">' in html

    def test_head_extra_inserted_before_style(self):
        renderer = ReportRenderer()
        html = renderer.render_html(
            [], title="T", head_extra='<meta name="x" content="y">\n'
        )
        assert '<meta name="x" content="y">\n<style>' in html

    def test_write_html_round_trip(self, tmp_path):
        renderer = ReportRenderer()
        html = renderer.render_html(["<p>hi</p>"], title="T")
        out = tmp_path / "nested" / "dir" / "report.html"
        result = renderer.write_html(html, out)
        assert result == out
        assert out.exists()
        assert out.read_text(encoding="utf-8") == html

    def test_write_html_accepts_str_path(self, tmp_path):
        renderer = ReportRenderer()
        html = renderer.render_html([], title="T")
        target = tmp_path / "r.html"
        result = renderer.write_html(html, str(target))
        assert isinstance(result, Path)
        assert result.read_text(encoding="utf-8") == html


# ---------------------------------------------------------------------------
# ReportBlock protocol + ReportBlockFn
# ---------------------------------------------------------------------------


class TestReportBlockProtocol:
    def test_object_with_render_satisfies_protocol(self):
        class Block:
            def render(self, data, **kwargs) -> str:
                return "<x/>"

        assert isinstance(Block(), ReportBlock)

    def test_object_without_render_does_not_satisfy(self):
        class NotABlock:
            def other(self):  # pragma: no cover - shape only
                return "x"

        assert not isinstance(NotABlock(), ReportBlock)

    def test_runtime_checkable_marker(self):
        # Protocol is runtime_checkable -> isinstance must not raise.
        assert isinstance(object(), ReportBlock) is False

    def test_report_block_fn_is_callable_alias(self):
        def builder(data, **kwargs) -> str:
            return ""

        fn: ReportBlockFn = builder
        assert callable(fn)


# ---------------------------------------------------------------------------
# ReportDataModel
# ---------------------------------------------------------------------------


class TestReportDataModel:
    def test_subclassing_and_validation(self):
        class MyData(ReportDataModel):
            name: str
            count: int = 0

        m = MyData(name="abc")
        assert m.name == "abc"
        assert m.count == 0
        assert m.model_dump() == {"name": "abc", "count": 0}

    def test_arbitrary_types_allowed(self):
        class Holder:
            pass

        class MyData(ReportDataModel):
            payload: Holder

        h = Holder()
        m = MyData(payload=h)
        assert m.payload is h

    def test_is_pydantic_model(self):
        from pydantic import BaseModel

        class MyData(ReportDataModel):
            x: int

        assert issubclass(MyData, BaseModel)
        with pytest.raises(Exception):
            MyData()  # missing required field
