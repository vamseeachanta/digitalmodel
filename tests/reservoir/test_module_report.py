# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Rebuild and check the numerical and offline reservoir report contract."""

import hashlib
import importlib.util
import json
import os
from pathlib import Path
import re
import subprocess
import sys

from bs4 import BeautifulSoup
import pytest

from digitalmodel.reservoir import volumetrics

ROOT = Path(__file__).resolve().parents[2]
REPORT = ROOT / "docs/reports/modules/reservoir"
GENERATOR = ROOT / "scripts/reporting/generate_reservoir_report.py"


@pytest.fixture
def generator():
    """Load the builder so disposable source trees can exercise its contract."""
    spec = importlib.util.spec_from_file_location("reservoir_report", GENERATOR)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


@pytest.fixture(scope="module")
def rebuilt(tmp_path_factory):
    """Run the shipped generator without relying on an installed package."""
    target = tmp_path_factory.mktemp("reservoir-report")
    env = {k: v for k, v in os.environ.items()
           if k not in {"GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR"}}
    env["PYTHONPATH"] = str(ROOT / "src")
    subprocess.run([sys.executable, str(GENERATOR), "--output-dir", str(target)],
                   cwd=ROOT, env=env, check=True, capture_output=True, text=True)
    page = (target / "index.html").read_text(encoding="utf-8")
    soup = BeautifulSoup(page, "html.parser")
    payload = json.loads(soup.find("script", id="calculation-data").string)
    return target, page, soup, payload


def test_rebuild_matches_committed_artifacts(rebuilt):
    target, _, _, _ = rebuilt
    for name in ("index.html", "changes.json"):
        generated = (target / name).read_bytes()
        assert b"\r" not in generated
        assert generated == (REPORT / name).read_bytes().replace(b"\r\n", b"\n")


def test_freshness_registry_includes_report_and_all_inputs():
    from scripts import check_generated_html as checker

    entries = [entry for entry in checker.GENERATORS
               if entry.script == GENERATOR.relative_to(ROOT).as_posix()]
    assert len(entries) == 1
    entry = entries[0]
    assert set(entry.outputs) == {
        "docs/reports/modules/reservoir/index.html",
        "docs/reports/modules/reservoir/changes.json",
    }
    record = json.loads((REPORT / "validation.json").read_text(encoding="utf-8"))
    assert set(record["source_digests"]) <= set(entry.inputs)
    assert {"docs/collide_pe/briefs/pe-2026-05-21.html", "pyproject.toml",
            "docs/reports/modules/reservoir/validation.json",
            "docs/reports/pipeline-ffs-assessment.html"} <= set(entry.inputs)


def test_text_digest_is_independent_of_checkout_line_endings(tmp_path, monkeypatch, generator):
    monkeypatch.setattr(generator, "ROOT", tmp_path)
    source = tmp_path / "source.txt"
    text = "first line\nsecond line\n"
    source.write_bytes(text.encode("utf-8"))
    expected = hashlib.sha256(text.encode("utf-8")).hexdigest()
    assert generator.digest(source.name) == expected
    source.write_bytes(text.replace("\n", "\r\n").encode("utf-8"))
    assert generator.digest(source.name) == expected


def test_crlf_sources_preserve_evidence_and_rebuild(tmp_path, monkeypatch, generator, rebuilt):
    record = json.loads((REPORT / "validation.json").read_text(encoding="utf-8"))
    paths = set(record["source_digests"]) | {
        generator.BRIEF.as_posix(), generator.STYLE_SOURCE.as_posix(),
        generator.EVIDENCE.as_posix(),
    }
    for relative in paths:
        target = tmp_path / relative
        target.parent.mkdir(parents=True, exist_ok=True)
        text = (ROOT / relative).read_text(encoding="utf-8")
        target.write_bytes(text.replace("\n", "\r\n").encode("utf-8"))
    # Global configuration is deliberately unrelated to the reservoir evidence.
    for relative in ("pytest.ini", "tests/conftest.py", "conftest.py"):
        target = tmp_path / relative
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_bytes(b"unrelated configuration change\r\n")
    monkeypatch.setattr(generator, "ROOT", tmp_path)
    generator.build(tmp_path / "built")
    for name in ("index.html", "changes.json"):
        assert (tmp_path / "built" / name).read_bytes() == (rebuilt[0] / name).read_bytes()


def test_generated_outputs_remain_lf_on_windows(tmp_path, monkeypatch, generator, rebuilt):
    write_text = Path.write_text

    def windows_write_text(path, text, *args, **kwargs):
        if kwargs.get("newline") is None:
            text = text.replace("\n", "\r\n")
        return write_text(path, text, *args, **kwargs)

    monkeypatch.setattr(Path, "write_text", windows_write_text)
    generator.build(tmp_path)
    for name in ("index.html", "changes.json"):
        assert (tmp_path / name).read_bytes() == (rebuilt[0] / name).read_bytes()


def test_validation_binds_only_reservoir_sources_and_tests():
    record = json.loads((REPORT / "validation.json").read_text(encoding="utf-8"))
    paths = set(record["source_digests"])
    assert len(paths) == 7
    assert all(path.startswith(("src/digitalmodel/reservoir/", "tests/reservoir/"))
               or path == "tests/test_reservoir_framework.py" for path in paths)


def test_documented_inputs_are_read_from_the_retained_public_example(rebuilt):
    _, _, _, data = rebuilt
    brief = BeautifulSoup((ROOT / "docs/collide_pe/briefs/pe-2026-05-21.html")
                          .read_text(encoding="utf-8"), "html.parser")
    cells = brief.select("table.given td")
    given = {key.get_text(): float(value.get_text())
             for key, value in zip(cells[::2], cells[1::2], strict=True)}
    assert data["inputs"] == {
        "area_acres": given["area acres"], "net_pay_ft": given["net pay ft"],
        "porosity": given["porosity"], "oil_saturation": given["oil saturation"],
        "formation_volume_factor": given["formation volume factor boi"],
    }
    assert data["factors"] == {"primary_factor": given["primary recovery factor"],
                               "raised_factor": given["secondary recovery factor"]}


@pytest.mark.parametrize("key, expected", [
    ("oil_in_place", 42_898_636.8),
    ("primary", 9_437_700.096),
    ("raised", 12_869_591.04),
    ("increment", 3_431_890.944),
])
def test_report_results_match_independent_hand_calculation(rebuilt, key, expected):
    """Pin STB results independently of the volumetrics implementation."""
    # Inputs: docs/collide_pe/briefs/pe-2026-05-21.html, Given table.
    # Hand calculation: N = 7758 * 1200 * 40 * 0.18 * 0.80 / 1.25 STB;
    # primary = N * 0.22; raised = N * 0.30; increment = N * (0.30 - 0.22).
    _, _, _, data = rebuilt
    for section in ("results", "values"):
        assert data[section][key] == pytest.approx(expected, rel=0, abs=1e-6)


def test_every_example_number_and_chart_point_comes_from_module(rebuilt):
    _, _, soup, data = rebuilt
    inputs = volumetrics.VolumetricInputs(**data["inputs"])
    oil = volumetrics.ooip_stb(inputs)
    expected = {**data["inputs"], **data["factors"],
                "coefficient": volumetrics.BBL_PER_ACRE_FT, "oil_in_place": oil,
                "primary": volumetrics.recoverable_stb(oil, data["factors"]["primary_factor"]),
                "raised": volumetrics.recoverable_stb(oil, data["factors"]["raised_factor"]),
                "increment": volumetrics.incremental_recovery_stb(
                    oil, data["factors"]["primary_factor"], data["factors"]["raised_factor"])}
    assert data["values"] == expected
    assert data["results"] == {k: expected[k] for k in ("oil_in_place", "primary", "raised", "increment")}
    for node in soup.select("[data-value]"):
        value = expected[node["data-value"]]
        assert float(node["data-raw"]) == value
        assert node.get_text() == format(value, node["data-format"])
    assert set(expected) <= {node["data-value"] for node in soup.select("[data-value]")}
    for point in data["curve"]:
        assert point["volume"] == volumetrics.recoverable_stb(oil, point["factor"])
        assert point["limit"] == oil
        assert point["volume"] <= point["limit"]
    assert data["curve"][0]["volume"] == 0
    assert data["curve"][-1]["volume"] == oil
    # No manually entered numeric text in the worked-example section.
    example = soup.select_one("#section-5")
    for node in example.select("[data-value], svg, script"):
        node.decompose()
    assert not re.search(r"\d", example.get_text().replace("5. Detailed Results", ""))


def test_sources_evidence_and_document_contract(rebuilt):
    _, page, soup, data = rebuilt
    assert soup.select_one("header > p").get_text() == "Draft for review"
    assert "hs-doc" in soup.body["class"]
    headings = soup.select("main > section > h2")
    assert len(headings) == 10
    assert soup.find(id="revision-history")
    assert not soup.select("script[src], link[rel=stylesheet], img[src^=http]")
    assert soup.find(id="physical-limit")
    assert soup.find(id="recovery-selector")
    assert soup.find(id="comment-export")
    assert soup.find(id="comment-import")
    assert soup.find(id="changes-data")
    for link in soup.select('a[href^="#"]'):
        assert soup.find(id=link["href"][1:]), link["href"]
    style_source = (ROOT / data["style_source"]).read_text(encoding="utf-8")
    original = re.search(r"<style>(.*?)</style>", style_source, re.S).group(1)
    assert soup.find("style", id="house-style").string == original
    assert data["evidence"]["counts"] == {"passed": 23, "skipped": 30, "failed": 0, "errors": 0}
    assert "NOT_EVALUATED" in page
    assert "Not covered" in page
    assert not soup.select("strong, b")
    assert not re.search(r"\b(owner|could|should|would|certified|compliant)\b", soup.get_text(), re.I)
    assert not re.search(r"/(?:home|mnt)/|[\w.+-]+@[\w.-]+\.[a-z]{2,}", page, re.I)


def test_changed_source_invalidates_recorded_validation(tmp_path, monkeypatch):
    """Tamper only a disposable copy and verify that the live record is intact."""
    spec = importlib.util.spec_from_file_location("reservoir_report", GENERATOR)
    generator = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(generator)
    original = REPORT / "validation.json"
    before = hashlib.sha256(original.read_bytes()).hexdigest()
    record = json.loads(original.read_text(encoding="utf-8"))
    target = tmp_path / generator.EVIDENCE
    target.parent.mkdir(parents=True)
    target.write_text(json.dumps(record), encoding="utf-8")
    expected = dict(record["source_digests"])
    expected[next(iter(expected))] = "altered-source"
    monkeypatch.setattr(generator, "ROOT", tmp_path)
    monkeypatch.setattr(generator, "digest", lambda path: expected[path])
    with pytest.raises(ValueError, match="Validation evidence is stale"):
        generator.evidence_record()
    assert hashlib.sha256(original.read_bytes()).hexdigest() == before


@pytest.mark.parametrize("revision", ["0", "1"])
def test_actual_browser_interaction_and_comment_roundtrip(
    rebuilt, tmp_path, monkeypatch, generator, revision,
):
    """Exercise the shipped UI offline, including real export and import."""
    sync_playwright = pytest.importorskip("playwright.sync_api").sync_playwright

    target, _, _, data = rebuilt
    monkeypatch.setattr(generator, "REVISION", revision)
    target = tmp_path / "report"
    generator.build(target)
    with sync_playwright() as p:
        browser = p.chromium.launch(
            headless=True,
            executable_path=os.environ.get("PLAYWRIGHT_CHROMIUM_EXECUTABLE"),
        )
        context = browser.new_context(accept_downloads=True, offline=True)
        page = context.new_page()
        errors = []
        page.on("pageerror", lambda error: errors.append(str(error)))
        page.goto((target / "index.html").as_uri())
        page.locator("#recovery-selector").fill("30")
        page.locator("#recovery-selector").dispatch_event("input")
        assert format(data["results"]["raised"], ",.1f") in page.locator("#chart-readout").inner_text()
        page.locator("#comments-panel").evaluate("node => node.open = true")
        page.locator("#comment-section").select_option("section-5")
        page.locator("#comment-text").fill("Check the recovery basis <literal text>.")
        page.locator("#comment-add").click()
        assert "<literal text>" in page.locator("#comment-list").inner_text()
        page.locator("#comment-text").fill("Keep this unsaved comment.")
        page.locator("#comment-add").click()
        incoming = {"report": "reservoir-volumetric-analysis", "revision": revision,
                    "comments": [
                        {"section": "section-5", "text": "Check the recovery basis <literal text>."},
                        {"section": "section-7", "text": "Retain the arithmetic evidence."},
                    ]}
        upload = {"name": "comments.json", "mimeType": "application/json",
                  "buffer": json.dumps(incoming).encode("utf-8")}
        page.locator("#comment-import").set_input_files(upload)
        assert page.locator("#comment-list li").count() == 3
        assert "Keep this unsaved comment." in page.locator("#comment-list").inner_text()
        assert "<literal text>" in page.locator("#comment-list").inner_text()
        # Re-importing the same file must preserve both comments without duplicates.
        page.locator("#comment-import").set_input_files(upload)
        assert page.locator("#comment-list li").count() == 3
        incoming["revision"] = "different-revision"
        incoming["comments"] = [{"section": "section-5", "text": "Stale note."}]
        upload["buffer"] = json.dumps(incoming).encode("utf-8")
        page.locator("#comment-import").set_input_files(upload)
        assert page.locator("#comment-status").inner_text() == "Comments do not match this report and revision."
        assert page.locator("#comment-list li").count() == 3
        assert "Stale note." not in page.locator("#comment-list").inner_text()
        page.evaluate("window.showSaveFilePicker = undefined")
        with page.expect_download() as download_info:
            page.locator("#comment-export").click()
        download = download_info.value
        file = tmp_path / download.suggested_filename
        download.save_as(file)
        comments = json.loads(file.read_text(encoding="utf-8"))
        assert comments["report"] == "reservoir-volumetric-analysis"
        assert comments["revision"] == revision
        assert comments["comments"][0]["section"] == "section-5"
        page.reload()
        page.locator("#comments-panel").evaluate("node => node.open = true")
        page.locator("#comment-import").set_input_files(file)
        assert "<literal text>" in page.locator("#comment-list").inner_text()
        # Exercise the native save branch without granting directory access.
        page.evaluate("""window.showSaveFilePicker = async () => ({
            createWritable: async () => ({
                write: async value => {window.savedComments = value;},
                close: async () => {}
            })
        })""")
        page.locator("#comment-export").click()
        assert json.loads(page.evaluate("window.savedComments")) == comments
        assert not errors
        page.set_viewport_size({"width": 390, "height": 844})
        assert page.evaluate("document.documentElement.scrollWidth <= window.innerWidth")
        browser.close()
