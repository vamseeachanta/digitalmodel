# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Rebuild the public module report and compare its example with the actual API."""

import importlib.util
import hashlib
import json
import os
import re
from pathlib import Path

import pytest
from bs4 import BeautifulSoup

from digitalmodel.naval_architecture.stability import free_surface_correction


ROOT = Path(__file__).resolve().parents[2]
REPORT = ROOT / "docs/reports/modules/navalarchitecture"


def builder():
    path = ROOT / "scripts/reporting/build_navalarchitecture_module_report.py"
    spec = importlib.util.spec_from_file_location("naval_module_report", path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_rebuild_matches_module_and_committed_report(tmp_path):
    module = builder()
    output = tmp_path / "report"
    module.build_report(output)
    html = (output / "report.html").read_text(encoding="utf-8")
    assert html == (REPORT / "report.html").read_text(encoding="utf-8")
    assert (output / "changes.json").read_bytes() == (REPORT / "changes.json").read_bytes()
    soup = BeautifulSoup(html, "html.parser")
    data = json.loads(soup.select_one("#example-data").string)
    source = json.loads((REPORT / "example.json").read_text(encoding="utf-8"))
    assert data["inputs"] == source["inputs"]
    expected = free_surface_correction(**source["inputs"])
    assert data["results"]["correction_ft"] == expected[0]
    assert data["results"]["effective_gm_ft"] == expected[1]
    for point in data["sweep"]:
        inputs = dict(source["inputs"], tank_breadth_ft=point["breadth_ft"])
        correction, gm = free_surface_correction(**inputs)
        assert point["correction_ft"] == correction
        assert point["effective_gm_ft"] == gm
    for point, marker in zip(data["sweep"], soup.select(".sample"), strict=True):
        assert f'{point["effective_gm_ft"]:.3f} ft' in marker["aria-label"]
        assert f'{point["correction_ft"]:.3f} ft' in marker["aria-label"]
    values = {**data["inputs"], **data["results"], **data["derived"],
              **data["geometry"], **data["constants"]}
    for element in soup.select("[data-number]"):
        key = element["data-number"]
        value = values[key]
        assert float(element["data-value"]) == value
        assert element.get_text() == module.format_number(value, key)
    # An independent identity guards against a report that merely repeats a wrong result.
    inputs = source["inputs"]
    import math
    from digitalmodel.naval_architecture.stability import G, LT_TO_LB
    moment = inputs["tank_length_ft"] * inputs["tank_breadth_ft"] ** 3 / 12
    correction = inputs["rho_fluid"] * G * moment / (inputs["displacement_lt"] * LT_TO_LB)
    assert expected[0] == pytest.approx(correction, rel=1e-12)
    assert math.isfinite(expected[1])
    # Engineering numbers in the worked narrative must come from generated spans.
    for paragraph in soup.select("#example > p"):
        for generated in paragraph.select("[data-number], a"):
            generated.decompose()
        assert not re.search(r"\d", paragraph.get_text())


def test_report_structure_sources_and_offline_delivery():
    module = builder()
    html = module.report_files()["report.html"]
    soup = BeautifulSoup(html, "html.parser")
    assert "hs-doc" in soup.body["class"]
    assert len(soup.select("main > section > h2")) == 10
    assert not soup.select("h4, h5, h6, b, strong")
    assert not soup.select("script[src], link[rel=stylesheet], iframe, img[src]")
    assert not soup.select("[onclick]")
    assert not re.search(r"\b(could|should|would|owner)\b", soup.get_text(" "), re.I)
    assert soup.select_one(".boundary") is not None
    assert all(point.has_attr("tabindex") for point in soup.select(".sample"))
    ids = [element["id"] for element in soup.select("[id]")]
    assert len(ids) == len(set(ids))
    for link in soup.select('a[href^="#"]'):
        assert link["href"][1:] in ids
    source = json.loads((REPORT / "example.json").read_text(encoding="utf-8"))
    assert source["source"]["edition"] == "Summer 2020"
    assert len(source["source"]["sha256"]) == 64
    assert all(value["source"] for value in source["constants"].values())
    style = re.search(r"<style>(.*?)</style>", module.STYLE_EXEMPLAR.read_text(), re.S).group(1)
    assert style in html
    digest = soup.select_one('meta[name="report-content-digest"]')["content"]
    canonical = html.replace(digest, "CONTENT_DIGEST")
    assert hashlib.sha256(canonical.encode()).hexdigest() == digest
    assert json.loads(soup.select_one("#changes-data").string) == json.loads(
        (REPORT / "changes.json").read_text(encoding="utf-8"))


def test_check_detects_stale_output_without_writing(tmp_path):
    module = builder()
    module.build_report(tmp_path)
    target = tmp_path / "report.html"
    stale = target.read_text().replace("Draft for client review", "Superseded text")
    target.write_text(stale)
    with pytest.raises(ValueError, match="requires rebuilding"):
        module.build_report(tmp_path, check=True)
    assert target.read_text() == stale


def test_offline_chart_comments_and_mobile_layout(tmp_path):
    playwright = pytest.importorskip("playwright.sync_api")
    module = builder()
    module.build_report(tmp_path)
    expected = module.worked_example()
    with playwright.sync_playwright() as runtime:
        browser = runtime.chromium.launch(
            executable_path=os.environ.get("PLAYWRIGHT_CHROMIUM_EXECUTABLE"), headless=True)
        page = browser.new_page(viewport={"width": 1365, "height": 900}, accept_downloads=True)
        errors, requests = [], []
        page.on("pageerror", lambda error: errors.append(str(error)))
        page.on("request", lambda request: requests.append(request.url))
        page.goto((tmp_path / "report.html").as_uri())
        dimensions = page.locator("svg tspan[data-number]")
        assert dimensions.count() == 3
        assert all(dimensions.nth(index).bounding_box()["width"] > 0 for index in range(3))
        page.locator('.sample[data-index="0"]').focus()
        first = expected["sweep"][0]
        assert f'{first["breadth_ft"]:.3f} ft' in page.locator("#chart-readout").inner_text()
        assert f'{first["effective_gm_ft"]:.3f} ft' in page.locator("#chart-readout").inner_text()
        page.locator('.sample[data-index="30"]').hover()
        assert f'{expected["results"]["effective_gm_ft"]:.3f} ft' in page.locator("#chart-readout").inner_text()
        page.locator("#sample-index").focus()
        page.keyboard.press("Home")
        assert page.locator("#sample-index").input_value() == "0"
        assert page.locator("#changes-list > li").count() == 3
        check_comment_roundtrip(page)
        for width in (1365, 390):
            page.set_viewport_size({"width": width, "height": 900})
            assert page.evaluate("document.documentElement.scrollWidth <= window.innerWidth")
        assert requests == [(tmp_path / "report.html").as_uri()]
        assert not errors
        browser.close()


def check_comment_roundtrip(page):
    """Check exported comments, revision binding and safe text rendering."""
    comment = "Check the connected surface. <img src=x onerror=alert(1)>"
    page.select_option("#comment-section", "example")
    page.fill("#comment-text", comment)
    page.click("#comment-add")
    assert comment in page.locator("#comment-list").inner_text()
    assert page.locator("#comment-list img").count() == 0
    page.evaluate("window.showSaveFilePicker = undefined")
    with page.expect_download() as download:
        page.click("#comment-save")
    downloaded = json.loads(Path(download.value.path()).read_text())
    assert downloaded["comments"] == [{"section": "example", "text": comment}]
    payload = {"name": "comments.json", "mimeType": "application/json",
               "buffer": json.dumps(downloaded).encode()}
    page.locator("#comment-load").set_input_files(payload)
    page.wait_for_function("document.querySelector('#comment-message').textContent.includes('reloaded')")
    assert page.locator("#comment-list > li").count() == 1
    stale = dict(downloaded, revision="superseded")
    page.locator("#comment-load").set_input_files(
        dict(payload, buffer=json.dumps(stale).encode()))
    page.wait_for_function("document.querySelector('#comment-message').textContent.includes('do not match')")
    assert page.locator("#comment-list > li").count() == 1
    page.evaluate("""window.showSaveFilePicker = async () => ({
      createWritable: async () => ({write: async text => {window.savedComments = text;}, close: async () => {}}),
      getFile: async () => ({text: async () => window.savedComments})
    })""")
    page.click("#comment-save")
    page.wait_for_function("document.querySelector('#comment-message').textContent.includes('read back')")
    assert json.loads(page.evaluate("window.savedComments")) == downloaded
