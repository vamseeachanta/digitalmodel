# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Rebuild the public report and independently check its numerical evidence."""

import hashlib
import importlib.util
import json
import os
import re
import subprocess
import sys
from dataclasses import asdict, replace
from pathlib import Path

import pytest
import yaml
from bs4 import BeautifulSoup

from digitalmodel.riser_database import RiserDatabase

ROOT = Path(__file__).resolve().parents[2]
BUILDER = ROOT / "scripts/reports/build_riser_database_report.py"
REPORT_DIR = ROOT / "docs/reports/modules/riserdatabase"
REPORT_NAME = "riser-database-analysis.html"
REBUILD_COMMAND = "PYTHONPATH=src python scripts/reports/build_riser_database_report.py"


def builder_module():
    """Load the script without importing an unrelated reporting engine."""
    spec = importlib.util.spec_from_file_location("riser_report_builder", BUILDER)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


@pytest.fixture(scope="module")
def rebuilt(tmp_path_factory):
    """Exercise the shipped command with isolated output and repository bindings."""
    output = tmp_path_factory.mktemp("riser-report")
    env = {k: v for k, v in os.environ.items() if not k.startswith("GIT_")}
    env["PYTHONPATH"] = str(ROOT / "src")
    preserved = [ROOT / ".git", *sorted((ROOT / "data/riser_database").iterdir())]
    before = {path: path.read_bytes() for path in preserved if path.is_file()}
    result = subprocess.run(
        [sys.executable, str(BUILDER), "--output-dir", str(output)],
        cwd=output, env=env, text=True, capture_output=True, check=True,
    )
    assert result.stderr == ""
    assert all(path.read_bytes() == original for path, original in before.items())
    text = (output / REPORT_NAME).read_text(encoding="utf-8")
    soup = BeautifulSoup(text, "html.parser")
    model = json.loads(soup.find(id="report-data").string)
    return output, text, soup, model


def test_report_rebuild_is_byte_for_byte_current(rebuilt):
    output, _, _, _ = rebuilt
    assert {p.name for p in output.iterdir()} == {REPORT_NAME, "changes.json"}
    for name in (REPORT_NAME, "changes.json"):
        generated = (output / name).read_bytes()
        assert b"\r\n" not in generated
        assert generated == (REPORT_DIR / name).read_text(encoding="utf-8").encode("utf-8"), (
            f"Report evidence changed; rebuild with: {REBUILD_COMMAND}"
        )


@pytest.mark.parametrize("default", ["encoding", "newline"])
def test_windows_text_defaults_preserve_report_bytes(tmp_path, monkeypatch, default):
    builder = builder_module()
    checkout = tmp_path / "checkout"
    for relative in builder.collect_evidence(ROOT)["source_digests"]:
        target = checkout / relative
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_bytes((ROOT / relative).read_bytes())
    style = checkout / builder.EXTRA_STYLE
    style.write_text(style.read_text(encoding="utf-8") + "\n/* Unit symbol: μm */\n", encoding="utf-8")
    expected = builder.build_report(root=checkout, output_dir=tmp_path / "expected")
    original_open = Path.open

    def windows_open(path, mode="r", buffering=-1, encoding=None, errors=None, newline=None):
        if "b" not in mode:
            if default == "encoding" and encoding in (None, "locale"):
                encoding = "cp1252"
            if default == "newline" and "w" in mode and newline is None:
                newline = "\r\n"
        return original_open(path, mode, buffering, encoding, errors, newline)

    monkeypatch.setattr(Path, "open", windows_open)
    actual = builder.build_report(root=checkout, output_dir=tmp_path / "actual")
    for name in (REPORT_NAME, "changes.json"):
        assert (actual.parent / name).read_bytes() == (expected.parent / name).read_bytes()


def test_crlf_source_checkout_preserves_report_bytes(tmp_path):
    builder = builder_module()
    expected = builder.build_report(output_dir=tmp_path / "expected")
    checkout = tmp_path / "checkout"
    for relative in builder.collect_evidence(ROOT)["source_digests"]:
        target = checkout / relative
        target.parent.mkdir(parents=True, exist_ok=True)
        content = (ROOT / relative).read_bytes().replace(b"\r\n", b"\n")
        # Database bytes stay LF under the repository's existing attributes.
        target.write_bytes(content if relative.startswith("data/riser_database/")
                           else content.replace(b"\n", b"\r\n"))
    actual = builder.build_report(root=checkout, output_dir=tmp_path / "actual")
    for name in (REPORT_NAME, "changes.json"):
        assert (actual.parent / name).read_bytes() == (expected.parent / name).read_bytes()


def test_registered_report_regenerates_and_detects_drift(tmp_path, monkeypatch):
    from scripts import check_generated_html as checker

    builder = builder_module()
    entry = next((item for item in checker.GENERATORS
                  if item.script == BUILDER.relative_to(ROOT).as_posix()), None)
    assert entry is not None, "The report must participate in the documentation drift check"
    assert entry.script in checker.discover_candidate_generators(ROOT)
    monkeypatch.setattr(checker, "GENERATORS", (entry,))
    shadow = tmp_path / "shadow"
    checker.prepare_shadow(ROOT, shadow)
    assert all((shadow / relative).is_file()
               for relative in builder.collect_evidence(ROOT)["source_digests"])
    checker.clear_generated_html(shadow, entry)
    assert checker.run_generator(shadow, entry) is None
    assert checker.compare_outputs(ROOT, shadow) == [], REBUILD_COMMAND
    for name in (REPORT_NAME, "changes.json"):
        page = shadow / REPORT_DIR.relative_to(ROOT) / name
        original = page.read_bytes()
        page.write_bytes(original + b"\n")
        assert [path for path, _ in checker.compare_outputs(ROOT, shadow)] == [
            page.relative_to(shadow).as_posix()
        ]
        page.write_bytes(original)


def test_ignore_rules_preserve_later_module_exceptions(tmp_path):
    env = {k: v for k, v in os.environ.items() if not k.startswith("GIT_")}
    subprocess.run(["git", "init", "-q", str(tmp_path)], env=env, check=True)
    marker = "!docs/reports/pipeline-ffs-assessment.manifest.json"
    rules = (ROOT / ".gitignore").read_text(encoding="utf-8")
    assert marker in rules
    rules = rules.replace(marker, marker + (
        "\n!docs/reports/modules/\n!docs/reports/modules/example/"
        "\n!docs/reports/modules/example/example.html"
    ))
    (tmp_path / ".gitignore").write_text(rules, encoding="utf-8")
    public_paths = [
        "docs/reports/modules/example/example.html",
        "docs/reports/modules/riserdatabase/" + REPORT_NAME,
        "docs/reports/modules/riserdatabase/changes.json",
        "scripts/reports/build_riser_database_report.py",
        "scripts/reports/templates/riser_database_report.html",
        "scripts/reports/assets/riser_database_review.js",
    ]
    for relative in public_paths:
        result = subprocess.run(
            ["git", "check-ignore", "-q", "--", relative],
            cwd=tmp_path, env=env, capture_output=True, check=False,
        )
        assert result.returncode == 1, f"Required source is ignored: {relative}"
    result = subprocess.run(
        ["git", "check-ignore", "-q", "--", "scripts/reports/unrelated.html"],
        cwd=tmp_path, env=env, capture_output=True, check=False,
    )
    assert result.returncode == 0


def test_example_is_the_complete_module_return(rebuilt):
    model = rebuilt[3]
    row = RiserDatabase.load().config("riser_drilling")
    assert model["example"] == asdict(row)
    source = yaml.safe_load((ROOT / row.source_path).read_text(encoding="utf-8"))
    conv = yaml.safe_load((ROOT / "scripts/riser_database/sources.yml").read_text(encoding="utf-8"))
    conv = conv["unit_conversions"]
    expected = {
        "water_depth_m": source["geometry"]["water_depth"],
        "od_mm": source["material"]["outer_diameter"] * conv["m_to_mm"],
        "wt_mm": source["material"]["wall_thickness"] * conv["m_to_mm"],
    }
    assert model["source_values"] == expected
    assert all(getattr(row, key) == value for key, value in expected.items())
    numbers = model["numbers"]
    # Independent hand values: NIST SP 811 (2008), Appendix B.8, foot and inch.
    assert numbers["depth_ft"] == pytest.approx(8202.1, abs=0.05)
    assert numbers["od_in"] == pytest.approx(21.000, abs=0.0005)
    assert numbers["wall_in"] == pytest.approx(0.874, abs=0.0005)
    for key, value in expected.items():
        assert numbers[f"difference_{key}"] == 100 * (getattr(row, key) - value) / value


def test_every_visible_example_number_is_bound_to_the_payload(rebuilt):
    soup, model = rebuilt[2:]
    for node in soup.select("[data-number]"):
        value = model["numbers"][node["data-number"]]
        assert node.get_text() == format(value, node["data-format"])
    example = BeautifulSoup(str(soup.find(id="worked-example")), "html.parser")
    for node in example.select("[data-number], h3, figcaption, .caption"):
        node.decompose()
    assert not re.search(r"\b\d+(?:\.\d+)?\b", example.get_text())


def test_inventory_and_digests_are_read_from_current_sources(rebuilt):
    model = rebuilt[3]
    db = RiserDatabase.load()
    assert model["counts"] == {
        "config_catalog": len(db.configs),
        "standards_crosswalk": len(db.crosswalk_rows),
        "material_sn_scf_dff": len(db.materials),
        "riser_stackup": len(db.stackups),
        "rig_riser_interface": len(db.rigs),
    }
    for relative, digest in model["source_digests"].items():
        content = (ROOT / relative).read_bytes().replace(b"\r\n", b"\n")
        assert hashlib.sha256(content).hexdigest() == digest
    assert all(model["qa"].values())


def test_source_disagreement_stops_report_generation(monkeypatch):
    builder = builder_module()
    db = RiserDatabase.load()
    db._configs["riser_drilling"] = replace(
        db.config("riser_drilling"), wt_mm=db.config("riser_drilling").wt_mm + 1,
    )
    monkeypatch.setattr(builder.RiserDatabase, "load", lambda **kwargs: db)
    with pytest.raises(ValueError, match="Public example differs"):
        builder.collect_evidence(ROOT)


def test_layout_sources_and_cross_references(rebuilt):
    soup = rebuilt[2]
    assert "hs-doc" in soup.body["class"]
    assert len(soup.select("main > section.numbered")) == 10
    assert not soup.select("h4, h5, h6, b, strong")
    assert soup.find(id="summary").find_next_sibling().get("id") == "revision-history"
    ids = [node["id"] for node in soup.select("[id]")]
    assert len(ids) == len(set(ids))
    for link in soup.select('a[href^="#"]'):
        assert link["href"][1:] in ids
    for link in soup.select('a[data-repo-source]'):
        target = (REPORT_DIR / link["href"]).resolve()
        assert target.is_relative_to(ROOT) and target.is_file()
        assert link.find_parent(id="source-records") is not None
    assert not soup.select("script[src], link[rel=stylesheet], img[src]")
    assert "TOKENS" in soup.style.string


def test_built_public_text_and_embedded_data_are_disclosure_limited(rebuilt):
    text, soup, model = rebuilt[1:]
    visible = soup.get_text(" ", strip=True)
    prohibited = r"\b(?:owner|assistant|automation|agent|session|TODO)\b"
    assert not re.search(prohibited, visible, re.I)
    assert not re.search(r"\b(?:could|should|would)\b", visible, re.I)
    assert not re.search(r"/(?:home|mnt)/|[\w.+-]+@[\w-]+\.[\w.-]+", text)
    assert not re.search(r"https?://(?:github|raw\.githubusercontent)\.com/", text)
    assert "NOT_EVALUATED" in text
    assert set(model["example"]) == set(asdict(RiserDatabase.load().config("riser_drilling")))
    assert "get_riser_dff().value" not in text
    assert not any("wiki" in path for path in model["source_digests"])


def test_interactive_chart_has_case_component_demand_units_and_target(rebuilt):
    soup, model = rebuilt[2:]
    points = soup.select("#agreement-chart [data-measure]")
    assert len(points) == len(model["source_values"])
    assert soup.select_one("#agreement-chart [data-target]") is not None
    for point in points:
        title = point.find("title").get_text()
        assert all(word in title for word in ("Case:", "Component:", "Demand:", "Target:", "%"))
        assert point["tabindex"] == "0"
    assert soup.find(id="chart-readout")["aria-live"] == "polite"


def test_changes_panel_embeds_the_sidecar(rebuilt):
    output, _, soup, _ = rebuilt
    sidecar = json.loads((output / "changes.json").read_text())
    assert json.loads(soup.find(id="report-changes").string) == sidecar
    assert soup.find(id="changes-panel") is not None
    for change in sidecar["changes"]:
        assert soup.find(id=change["section"]) is not None


@pytest.fixture
def report_page(rebuilt):
    """Open the shipped single file and reject browser script errors."""
    sync_playwright = pytest.importorskip("playwright.sync_api").sync_playwright

    errors = []
    with sync_playwright() as playwright:
        browser = playwright.chromium.launch(
            headless=True, channel=os.environ.get("REPORT_BROWSER_CHANNEL") or None,
        )
        page = browser.new_page(accept_downloads=True)
        page.on("pageerror", lambda error: errors.append(str(error)))
        page.goto((rebuilt[0] / REPORT_NAME).as_uri())
        yield page
        assert errors == []
        browser.close()


def add_browser_comment(page):
    """Select actual report text before using the visible comment controls."""
    page.evaluate("""() => {
          const range = document.createRange();
          range.selectNodeContents(document.querySelector('#summary p'));
          const selection = window.getSelection();
          selection.removeAllRanges(); selection.addRange(range);
          document.dispatchEvent(new Event('selectionchange'));
        }""")
    page.locator("#comment-note").fill("Check the geometry basis.")
    page.locator("#comment-replacement").fill("Proposed wording.")
    page.locator("#add-comment").click()
    assert page.locator("#comment-list li").count() == 1


def test_chart_and_responsive_layouts_in_browser(report_page, tmp_path):
    page = report_page
    assert page.evaluate("document.documentElement.scrollWidth <= innerWidth")
    page.screenshot(path=str(tmp_path / "report-desktop.png"))
    page.locator("#stackup-figure").screenshot(path=str(tmp_path / "stackup.png"))
    page.locator("#agreement-chart [data-measure]").first.focus()
    assert "Demand:" in page.locator("#chart-readout").inner_text()
    target = page.locator("#agreement-chart [data-target]")
    assert target.evaluate("node => getComputedStyle(node).stroke") != "none"
    status_colour = page.evaluate("""() => {
      const probe = document.createElement('span'); probe.style.color = 'var(--good)';
      document.body.append(probe); const colour = getComputedStyle(probe).color;
      probe.remove(); return colour;
    }""")
    marker = page.locator("#agreement-chart circle").first
    assert marker.evaluate("node => getComputedStyle(node).fill") == status_colour
    page.locator("#changes-panel summary").click()
    assert page.locator("#changes-panel a").count() > 0
    page.set_viewport_size({"width": 390, "height": 844})
    page.evaluate("scrollTo(0, 0)")
    assert page.evaluate("document.documentElement.scrollWidth <= innerWidth")
    page.screenshot(path=str(tmp_path / "report-mobile.png"))


def test_comment_export_restore_and_import_in_browser(report_page, tmp_path):
    page = report_page
    add_browser_comment(page)
    with page.expect_download() as download:
        page.locator("#export-comments").click()
    saved = tmp_path / download.value.suggested_filename
    download.value.save_as(saved)
    payload = json.loads(saved.read_text())
    assert payload["comments"][0]["section"] == "summary"
    assert payload["comments"][0]["replacement"] == "Proposed wording."
    assert payload["comments"][0]["source_digests"] == payload["source_digests"]
    page.reload()
    assert page.locator("#comment-list li").count() == 1
    page.locator("#import-comments").set_input_files(saved)
    assert page.locator("#comment-list li").count() == 1
    invalid = tmp_path / "invalid-comments.json"
    invalid.write_text('{"document":"different document","comments":[]}', encoding="utf-8")
    page.locator("#import-comments").set_input_files(invalid)
    page.wait_for_function("document.getElementById('comment-status').textContent.includes('not imported')")
    assert page.locator("#comment-list li").count() == 1


def test_folder_save_branch_in_browser(report_page):
    page = report_page
    add_browser_comment(page)
    # Exercise folder writes through deterministic browser API handles.
    page.evaluate("""() => {
          window.savedReview = '';
          window.showDirectoryPicker = async () => ({
            getFileHandle: async (name, options) => {
              if (!options) throw new DOMException('Missing', 'NotFoundError');
              return {createWritable: async () => ({
                write: async (text) => {window.savedReview = text;}, close: async () => {}
              })};
            }
          });
        }""")
    page.locator("#choose-folder").click()
    page.wait_for_function("document.getElementById('comment-status').textContent.includes('folder selected')")
    page.locator("#save-comments").click()
    page.wait_for_function("document.getElementById('comment-status').textContent.includes('Comments saved beside')")
    payload = json.loads(page.evaluate("window.savedReview"))
    assert payload["comments"][0]["section"] == "summary"
    assert payload["comments"][0]["replacement"] == "Proposed wording."


def test_folder_save_preserves_existing_and_later_comments(report_page):
    page = report_page
    add_browser_comment(page)
    page.evaluate("""() => {
      const documentName = JSON.parse(document.getElementById('report-changes').textContent).document;
      const comment = {text:'Existing passage', section:'summary', note:'Existing note', replacement:''};
      window.diskReview = JSON.stringify({document:documentName, revision:'earlier',
        source_digests:{public_example:'earlier evidence'}, comments:[comment]});
      window.reviewWrites = 0;
      window.showDirectoryPicker = async () => ({getFileHandle: async () => ({
        getFile: async () => new File([window.diskReview], 'comments.json'),
        createWritable: async () => ({write:async (text) => {
          window.reviewWrites++; window.diskReview = text;
        }, close:async () => {}})
      })});
    }""")
    page.locator("#choose-folder").click()
    page.wait_for_function("document.getElementById('comment-status').textContent.includes('folder selected')")
    assert page.locator("#comment-list li").count() == 2
    page.evaluate("""() => {
      const data = JSON.parse(window.diskReview);
      data.comments.push({text:'Later passage',section:'summary',note:'Later note',replacement:''});
      window.diskReview = JSON.stringify(data);
    }""")
    page.locator("#save-comments").click()
    page.wait_for_function("document.getElementById('comment-status').textContent.includes('Comments saved beside')")
    saved = json.loads(page.evaluate("window.diskReview"))
    assert len(saved["comments"]) == 3
    previous = next(item for item in saved["comments"] if item["note"] == "Existing note")
    assert previous["revision"] == "earlier"
    assert previous["source_digests"] == {"public_example": "earlier evidence"}
    page.evaluate("window.diskReview = 'invalid json'; window.reviewWrites = 0")
    page.locator("#save-comments").click()
    page.wait_for_function("document.getElementById('comment-status').textContent.includes('not saved')")
    assert page.evaluate("window.reviewWrites") == 0
    assert page.evaluate("window.diskReview") == "invalid json"
