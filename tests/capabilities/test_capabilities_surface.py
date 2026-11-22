# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Synthetic offline fixtures; no correspondence to a published site is asserted."""

import hashlib
from html import escape
import importlib.util
import json
import os
import subprocess
import sys
from pathlib import Path

import pytest
import yaml

REPO = Path(__file__).resolve().parents[2]
SCRIPT = REPO / "scripts/capabilities/compare_capabilities_surface.py"


@pytest.fixture
def surface():
    spec = importlib.util.spec_from_file_location("surface", SCRIPT)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


@pytest.fixture
def declared(tmp_path):
    path = tmp_path / "docs/capability-map/capabilities-sections.yml"
    path.parent.mkdir(parents=True)
    path.write_text(yaml.safe_dump({
        "canonical_surface": "https://example.invalid/capabilities/",
        "sections": [{"id": "alpha", "title": "Alpha & work — analysis"},
                     {"id": "beta", "title": "Beta"}],
    }, allow_unicode=True), encoding="utf-8")
    return tmp_path


def page(beta="<section id='beta'><h2>Beta</h2></section>"):
    return ("<nav><a href='#alpha'>A</a><a href='#beta'>B</a>"
            "<a href='/elsewhere#other'>Other</a></nav>"
            "<section id='alpha'><header><h2> Alpha &amp; <em>work</em> — analysis </h2>"
            "</header><h3>Detail</h3></section>" + beta +
            "<footer id='unrelated'><a href='#footer'>Footer</a></footer>")


def test_match_normalization_and_provenance(surface, declared):
    report = surface.compare_surface(page(), declared)
    assert report == surface.compare_surface(page(), declared)
    assert report["status"] == "match"
    assert report["schema_version"] == "1.0"
    assert report["source_sha256"] == hashlib.sha256(page().encode()).hexdigest()
    assert report["canonical_surface"] == "https://example.invalid/capabilities/"
    assert report["expected_count"] == report["observed_count"] == 2
    assert report["diagnostics"]["nav_fragment_count"] == 2
    assert report["diagnostics"]["section_ids"] == ["alpha", "beta"]
    assert not any(report["mismatches"].values())


@pytest.mark.parametrize("real_census", [False, True])
def test_census_uses_utf8_under_legacy_locale(surface, declared, monkeypatch, real_census):
    repo = REPO if real_census else declared
    source = repo / surface._inventory.SECTIONS_YML
    config = yaml.safe_load(source.read_bytes().decode("utf-8"))
    html = "".join(f"<section id='{s['id']}'><h2>{escape(s['title'])}</h2></section>"
                   for s in config["sections"])
    read_text = Path.read_text
    reads = []

    def legacy_read_text(path, encoding=None, errors=None):
        reads.append((path, encoding))
        return read_text(path, encoding=encoding or "cp1252", errors=errors)

    monkeypatch.setattr(Path, "read_text", legacy_read_text)
    report = surface.compare_surface(html, repo)
    assert report["status"] == "match"
    assert report["expected_count"] == report["observed_count"] == len(config["sections"])
    assert not report["mismatches"]["changed_title"]
    assert reads == [(source, "utf-8")]


@pytest.mark.parametrize("replacement,key,value", [
    ("", "missing", ["beta"]),
    ("<section id='gamma'><h2>Gamma</h2></section>", "extra", ["gamma"]),
    ("<section id='beta'><h2>Beta</h2></section>" * 2, "duplicate", ["beta"]),
    ("<section id='beta'><h2>Changed</h2></section>", "changed_title",
     [{"id": "beta", "expected": "Beta", "observed": ["Changed"]}]),
])
def test_drift(surface, declared, replacement, key, value):
    report = surface.compare_surface(page(replacement), declared)
    assert report["status"] == "drift"
    assert report["mismatches"][key] == value


@pytest.mark.parametrize("html,reason", [
    ("", "no_titled_sections"),
    ("<meta http-equiv='refresh' content='0;url=/new'><h1>Moved</h1>", "redirect"),
    ("<form><input type='password'><h1>Sign in</h1></form>", "no_titled_sections"),
    ("<nav><a href='#alpha'>A</a></nav><div id='alpha'><h2>A</h2></div>",
     "no_titled_sections"),
    (page().replace("</nav>", ""), "malformed_structure"),
    (page().replace("</section>", "", 1), "malformed_structure"),
    (page("<section id='beta'><p>No heading</p></section>"), "untitled_section"),
])
def test_unrecognized_shape(surface, declared, html, reason):
    report = surface.compare_surface(html, declared)
    assert report["status"] == "unrecognized-shape"
    assert reason in report["diagnostics"]["reasons"]
    assert isinstance(report["diagnostics"]["tag_counts"], dict)


@pytest.mark.parametrize("nav,state", [("", "absent"), ("<nav></nav>", "empty")])
def test_nav_absence_is_separate(surface, declared, nav, state):
    html = nav + page().split("</nav>", 1)[1]
    report = surface.compare_surface(html, declared)
    assert report["status"] == "match"
    assert report["diagnostics"]["navigation"] == state


@pytest.mark.parametrize("html,state", [
    (page().replace("</section>", "", 1), "present"),
    (page().replace("</h2>", "", 1), "present"),
    (page().replace("<nav><a href='#alpha'>A</a><a href='#beta'>B</a>"
                    "<a href='/elsewhere#other'>Other</a></nav>", "<nav></nav>")
     .replace("</h2>", "", 1), "empty"),
    ("<nav><section><h2>Heading</section><a href='#alpha'>A</a></nav>" + page(), "present"),
])
def test_section_errors_leave_navigation_independent(surface, declared, html, state):
    report = surface.compare_surface(html, declared)
    assert report["status"] == "unrecognized-shape"
    assert "malformed_structure" in report["diagnostics"]["reasons"]
    assert report["diagnostics"]["navigation"] == state


@pytest.mark.parametrize("nav", [
    "<nav><a href='#alpha'>A</a>",
    "</nav>",
    "<section><nav><a href='#alpha'>A</a></section>",
    "<nav><section><h2>Heading</h2></nav></section>",
])
def test_malformed_navigation_boundaries(surface, declared, nav):
    report = surface.compare_surface(nav + page().split("</nav>", 1)[1], declared)
    assert report["status"] == "unrecognized-shape"
    assert report["diagnostics"]["navigation"] == "malformed"


def test_nested_sections_and_inert_content(surface, declared):
    html = ("<section id='alpha'><section id='beta'><h2>Beta</h2></section>"
            "<h2>Alpha &amp; work — analysis</h2></section>"
            "<template><section id='fake'><h2>Fake</h2></section></template>"
            "<script>throw Error('must not execute')</script>")
    assert surface.compare_surface(html, declared)["status"] == "match"


def test_valueless_attributes_and_inert_navigation(surface, declared):
    html = "<meta http-equiv><nav><a href>Other</a></nav>" + page().split("</nav>", 1)[1]
    assert surface.compare_surface(html, declared)["diagnostics"]["navigation"] == "empty"
    html = "<template><nav><a href='#fake'>Fake</a></nav></template>" + page().split("</nav>", 1)[1]
    assert surface.compare_surface(html, declared)["diagnostics"]["navigation"] == "absent"
    report = surface.compare_surface(page() + "<section id><h2>Unknown</h2></section>", declared)
    assert report["status"] == "unrecognized-shape"


def test_bounded_diagnostics_and_fail_closed_schema(surface, declared):
    report = surface.compare_surface("<div id='private'>private text</div>" * 100, declared)
    assert "private" not in json.dumps(report)
    html = "".join(f"<section id='{i}{'x' * 500}'><h2>X</h2></section>" for i in range(100))
    report = surface.compare_surface(html, declared)
    assert report["observed_count"] == 100
    assert len(report["diagnostics"]["section_ids"]) <= 40
    assert max(map(len, report["diagnostics"]["section_ids"])) <= 120
    path = declared / "docs/capability-map/capabilities-sections.yml"
    path.write_text("sections: []", encoding="utf-8")
    with pytest.raises(RuntimeError, match="no sections"):
        surface.compare_surface(page(), declared)


def test_cli_explicit_utf8_input_and_no_inventory_writes(surface, declared, tmp_path):
    html = tmp_path / "supplied.html"
    html.write_text(page(), encoding="utf-8")
    source = declared / "docs/capability-map/capabilities-sections.yml"
    before = source.read_bytes()
    for content, status in [(page(), "match"), (page(""), "drift"), ("", "unrecognized-shape")]:
        html.write_text(content, encoding="utf-8")
        result = subprocess.run([sys.executable, str(SCRIPT), str(html), "--repo", str(declared)],
                                text=True, capture_output=True, check=True)
        assert json.loads(result.stdout)["status"] == status
        assert not result.stderr
    assert source.read_bytes() == before


def test_cli_json_under_legacy_console(declared, tmp_path):
    html = tmp_path / "supplied.html"
    html.write_text(page("<section id='beta'><h2>Tools 雪</h2></section>"), encoding="utf-8")
    result = subprocess.run([sys.executable, str(SCRIPT), str(html), "--repo", str(declared)],
                            env={**os.environ, "PYTHONIOENCODING": "cp1252:strict"},
                            capture_output=True, check=True)
    report = json.loads(result.stdout.decode("ascii"))
    assert report["status"] == "drift"
    assert report["mismatches"]["changed_title"] == [
        {"id": "beta", "expected": "Beta", "observed": ["Tools 雪"]}]
    assert not result.stderr
