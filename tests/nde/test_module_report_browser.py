# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Exercise the standalone report offline in a browser, when available."""

import importlib.util
import json
import os
import shutil
from pathlib import Path

import pytest


@pytest.fixture
def report_page(tmp_path):
    playwright = pytest.importorskip("playwright.sync_api")
    executable = next((shutil.which(name) for name in (
        "chromium", "chromium-browser", "google-chrome") if shutil.which(name)), None)
    if executable is None:
        pytest.skip("An installed Chromium browser is required for this check")
    root = Path(__file__).resolve().parents[2]
    spec = importlib.util.spec_from_file_location(
        "browser_nde_builder", root / "scripts/reporting/build_nde_report.py")
    builder = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(builder)
    builder.build_report(tmp_path)
    with playwright.sync_playwright() as runtime:
        browser = runtime.chromium.launch(
            headless=True, executable_path=executable,
            env={**os.environ, "XDG_CACHE_HOME": str(tmp_path / "browser-cache"),
                 "XDG_CONFIG_HOME": str(tmp_path / "browser-config")},
            args=["--no-sandbox", "--disable-dev-shm-usage"],
        )
        context = browser.new_context(viewport={"width": 1280, "height": 900})
        page = context.new_page()
        errors = []
        page.on("pageerror", lambda error: errors.append(str(error)))
        page.goto((tmp_path / "nde-module-analysis.html").as_uri())
        yield page
        assert not errors
        context.close()
        browser.close()


def test_chart_polarity_hover_and_comment_download(report_page, tmp_path):
    page = report_page
    assert page.locator("#trace-positive").is_visible()
    assert not page.locator("#trace-negative").is_visible()
    page.select_option("#polarity", "negative")
    assert page.locator("#trace-negative").is_visible()
    assert not page.locator("#trace-positive").is_visible()
    point = page.locator('#trace-negative circle[data-index="3"]')
    point.focus()
    detail = page.locator("#pulse-detail").inner_text()
    assert all(word in detail for word in ("negative", "receiver", "demand", "limit", "units"))
    page.locator("details").last.locator("summary").click()
    page.fill("#comment-text", "Check the emission reference <script>invalid()</script>")
    page.locator('#comment-form button[type="submit"]').click()
    assert page.locator("#comment-list li").count() == 1
    with page.expect_download() as pending:
        page.click("#save-comments")
    output = tmp_path / "saved-comments.json"
    pending.value.save_as(output)
    saved = json.loads(output.read_text())
    assert saved["comments"] == [{
        "section": "s1", "text": "Check the emission reference <script>invalid()</script>"}]
    assert not page.locator("#comment-list script").count()
    page.set_input_files("#load-comments", output)
    page.wait_for_function("document.querySelectorAll('#comment-list li').length === 2")
    page.locator("#s3").screenshot(path=str(tmp_path / "pulse-chart.png"))


def test_saving_preserves_existing_comments_and_rejects_wrong_revision(report_page):
    page = report_page
    original = {"document": "nde-module-analysis", "revision": "A", "comments": [
        {"section": "s4", "text": "Retain this existing basis comment."}]}
    page.evaluate("""original => {
      window.sidecar = JSON.stringify(original);
      window.showDirectoryPicker = async () => ({getFileHandle: async () => ({
        getFile: async () => new File([window.sidecar], 'comments.json'),
        createWritable: async () => ({
          write: async value => { window.sidecar = value; }, close: async () => {}
        })
      })});
    }""", original)
    page.locator("details").last.locator("summary").click()
    page.click("#choose-folder")
    page.fill("#comment-text", "Add a sampling-resolution comment.")
    page.locator('#comment-form button[type="submit"]').click()
    page.click("#save-comments")
    page.wait_for_function("document.getElementById('comment-status').textContent.includes('saved')")
    saved = json.loads(page.evaluate("window.sidecar"))
    assert original["comments"][0] in saved["comments"]
    assert len(saved["comments"]) == 2
    saved["revision"] = "earlier"
    page.evaluate("value => {window.sidecar = JSON.stringify(value);}", saved)
    before = page.evaluate("window.sidecar")
    page.click("#save-comments")
    page.wait_for_function("document.getElementById('comment-status').textContent.includes('not saved')")
    assert page.evaluate("window.sidecar") == before
