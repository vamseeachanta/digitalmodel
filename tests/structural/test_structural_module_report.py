# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Rebuild the structural report and verify its complete numerical example."""

import importlib.util
import json
import math
import re
import shutil
from pathlib import Path

import pytest
from bs4 import BeautifulSoup

from digitalmodel.structural.structural_analysis import (
    STEEL_S355,
    StressCalculator,
    StressState,
)

ROOT = Path(__file__).resolve().parents[2]
REPORT_DIR = ROOT / "docs/reports/modules/structural"


@pytest.fixture(scope="module")
def builder():
    spec = importlib.util.spec_from_file_location(
        "structural_report", ROOT / "scripts/reporting/build_structural_module_report.py"
    )
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


@pytest.fixture(scope="module")
def built(builder, tmp_path_factory):
    folder = tmp_path_factory.mktemp("structural-report")
    builder.build(folder)
    html = (folder / "structural-module-report.html").read_text(encoding="utf-8")
    soup = BeautifulSoup(html, "html.parser")
    return folder, html, soup, json.loads(soup.select_one("#calculation-data").string)


def lookup(data, key):
    for part in key.split("."):
        data = data[int(part)] if isinstance(data, list) else data[part]
    return data


def test_every_result_and_chart_point_calls_the_public_module(built):
    _, _, _, data = built
    raw = json.loads((REPORT_DIR / "public-cylinder.json").read_text())
    assert data["source_inputs"] == raw["inputs"]
    inputs = data["inputs"]
    assert inputs["pressure_mpa"] == pytest.approx(
        raw["inputs"]["pressure_psi"] * raw["conversions"]["psi_to_pa"] / 1e6
    )
    assert inputs["radius_m"] == pytest.approx(
        raw["inputs"]["diameter_in"] * raw["conversions"]["inch_to_m"] / 2
    )
    assert inputs["thickness_m"] == pytest.approx(
        raw["inputs"]["thickness_in"] * raw["conversions"]["inch_to_m"]
    )
    calculator = StressCalculator(STEEL_S355)
    for row in [data["results"], *data["series"]]:
        pressure = row["pressure_mpa"]
        hoop = calculator.hoop_stress(pressure, inputs["radius_m"], inputs["thickness_m"])
        axial = calculator.longitudinal_stress(pressure, inputs["radius_m"], inputs["thickness_m"])
        state = StressState(sigma_x=axial, sigma_y=hoop)
        assert row["hoop_mpa"] == hoop
        assert row["axial_mpa"] == axial
        assert row["equivalent_mpa"] == state.von_mises()
        assert row["principals_mpa"] == list(state.principal_stresses())
        assert row["max_shear_mpa"] == state.max_shear()
        # Independent equilibrium and principal-stress oracle, not report helpers.
        ratio = inputs["radius_m"] / inputs["thickness_m"]
        assert hoop == pytest.approx(pressure * ratio, rel=1e-12)
        assert axial == pytest.approx(pressure * ratio / 2, rel=1e-12)
        assert row["principals_mpa"] == pytest.approx(
            [pressure * ratio, pressure * ratio / 2, 0.0], rel=1e-12
        ), "Principal stresses differ from membrane equilibrium"
        assert row["equivalent_mpa"] == pytest.approx(math.sqrt(3) * axial, rel=1e-12)
        assert row["max_shear_mpa"] == pytest.approx(hoop / 2, rel=1e-12)
    assert data["strength_limit_mpa"] is None


@pytest.mark.parametrize("sample_index", range(21))
def test_independent_oracle_rejects_corrupted_intermediate_principal(
    builder, monkeypatch, sample_index
):
    reference = builder.calculate()["series"][sample_index]
    original = StressState.principal_stresses

    def corrupted_principals(self):
        largest, intermediate, smallest = original(self)
        if self.sigma_x == reference["axial_mpa"]:
            intermediate = largest if largest else 1.0
        return largest, intermediate, smallest

    monkeypatch.setattr(StressState, "principal_stresses", corrupted_principals)
    data = builder.calculate()
    assert data["series"][sample_index]["principals_mpa"][1] != reference["principals_mpa"][1]
    with pytest.raises(AssertionError, match="Principal stresses differ from membrane equilibrium"):
        test_every_result_and_chart_point_calls_the_public_module((None, None, None, data))


def test_all_displayed_example_numbers_match_embedded_calculation(built):
    _, _, soup, data = built
    displayed = soup.select("[data-number]")
    assert len(displayed) >= 27
    for node in displayed:
        value = lookup(data, node["data-number"])
        decimals = int(node["data-decimals"])
        assert node.get_text() == f"{value:,.{decimals}f}".replace("-", "−")
    # Every numeric token in the worked-example prose/tables is a bound value.
    worked = BeautifulSoup(str(soup.select_one("#section-5")), "html.parser")
    worked.h2.decompose()
    # Plot hover strings and axes are checked against each series point below.
    worked.svg.decompose()
    for node in worked.select("[data-number]"):
        node.decompose()
    assert not re.search(r"\d", worked.get_text())


def test_every_chart_hover_matches_a_module_sample(built):
    _, _, soup, data = built
    points = soup.select("[data-point]")
    assert len(points) == len(data["series"]) * 3
    for point in points:
        row = data["series"][int(point["data-point"])]
        demand = row[point["data-component"]]
        text = point["aria-label"]
        assert f"pressure {row['pressure_mpa']:.1f} MPa" in text
        assert f"demand {demand:.1f} MPa" in text
        assert "strength limit NOT_EVALUATED" in text
        assert point.title.get_text() == text
        assert float(point["cx"]) == pytest.approx(
            85 + 620 * row["pressure_mpa"] / data["inputs"]["pressure_mpa"], abs=0.005
        )
        assert float(point["cy"]) == pytest.approx(
            360 - 290 * demand / data["results"]["hoop_mpa"], abs=0.005
        )


def test_report_rebuild_is_deterministic_and_current(built):
    folder, _, _, _ = built
    for name in ("structural-module-report.html", "changes.json"):
        assert (folder / name).read_bytes() == (REPORT_DIR / name).read_bytes()


def test_house_style_single_file_sections_and_citations(built):
    _, html, soup, _ = built
    assert "hs-doc" in soup.body.get("class", [])
    exemplar = (ROOT / "docs/reports/pipeline-ffs-assessment.html").read_text()
    assert soup.style.string == BeautifulSoup(exemplar, "html.parser").style.string
    assert len(soup.select("main > section > h2")) == 10
    assert not soup.select("h4, h5, h6, strong, b, script[src], link[rel=stylesheet], iframe")
    assert "Draft for client review" in html
    assert soup.select_one("#revision-history") in soup.select_one("#section-1").descendants
    ids = [node["id"] for node in soup.select("[id]")]
    assert len(ids) == len(set(ids))
    for link in soup.select('a[href^="#"]'):
        assert link["href"][1:] in ids
    for link in soup.select("a[href]"):
        href = link["href"]
        if not href.startswith(("#", "https://")):
            assert (REPORT_DIR / href).exists(), href
    assert "ASSUMED - to be confirmed" in soup.get_text()
    assert "NOT_EVALUATED" in soup.get_text()
    assert not soup.select("img[src]")
    assert soup.select_one("svg[aria-label]")


def test_visible_text_has_no_internal_or_unqualified_language(built):
    _, html, _, _ = built
    soup = BeautifulSoup(html, "html.parser")
    for node in soup.select("script, style"):
        node.decompose()
    text = soup.get_text(" ", strip=True)
    # Source URLs are evidence links; account components are never visible text.
    assert not re.search(
        r"\b(owner|agent|lane|session|sha|peer.review|could|should|would)\b", text, re.I
    )
    assert not re.search(r"/(?:home|mnt|tmp)/|[\w.+-]+@[\w.-]+\.[a-z]{2,}", html, re.I)
    assert not re.search(r"\b(?:safe|certified|compliant|production.ready)\b", text, re.I)


def test_change_panel_and_comment_persistence_are_offline(built):
    folder, html, soup, _ = built
    changes = json.loads((folder / "changes.json").read_text())
    assert json.loads(soup.select_one("#change-data").string) == changes
    assert "showDirectoryPicker" in html
    assert "createWritable" in html
    assert "structural-module-report.comments.json" in html
    assert "localStorage" in html
    assert "textContent" in html
    assert not re.search(r"\bfetch\s*\(|\bXMLHttpRequest\b", html)


def test_module_output_change_reaches_report(builder, monkeypatch, tmp_path):
    original = StressCalculator.hoop_stress

    def changed(self, pressure, radius, thickness):
        return original(self, pressure, radius, thickness) * 1.01

    monkeypatch.setattr(StressCalculator, "hoop_stress", changed)
    builder.build(tmp_path)
    soup = BeautifulSoup((tmp_path / "structural-module-report.html").read_text(), "html.parser")
    data = json.loads(soup.select_one("#calculation-data").string)
    inputs = data["inputs"]
    expected = original(None, inputs["pressure_mpa"], inputs["radius_m"], inputs["thickness_m"])
    assert data["results"]["hoop_mpa"] == pytest.approx(expected * 1.01)
    assert soup.select_one('[data-number="results.hoop_mpa"]').text == f"{expected * 1.01:,.1f}"


@pytest.mark.integration
def test_offline_chart_and_comment_roundtrip(built):
    """Exercise the delivered HTML, including file-save readback, in a browser."""
    api = pytest.importorskip("playwright.sync_api")
    executable = shutil.which("google-chrome") or shutil.which("chromium")
    if executable is None:
        pytest.skip("An installed Chromium browser is required for this optional integration check")
    folder, _, _, data = built
    with api.sync_playwright() as runtime:
        browser = runtime.chromium.launch(executable_path=executable, headless=True, args=["--no-sandbox"])
        context = browser.new_context(offline=True, viewport={"width": 1100, "height": 850})
        context.add_init_script("""window.savedComments = null;
          window.showDirectoryPicker = async () => ({
            getFileHandle: async (name) => name.endsWith('.html') ? {
              getFile: async () => ({text: async () => document.documentElement.outerHTML})
            } : {
              createWritable: async () => ({
                write: async (text) => { window.savedComments = text; }, close: async () => {}
              })
            }
          });""")
        page = context.new_page()
        errors = []
        page.on("pageerror", lambda error: errors.append(str(error)))
        page.goto((folder / "structural-module-report.html").as_uri())
        assert page.locator("#section-3 svg [data-number]").count() == 3
        assert page.locator("#section-3 svg span").count() == 0
        assert f"{data['results']['hoop_mpa']:.1f} MPa" in page.locator("#sample-readout").inner_text()
        page.locator("#pressure-sample").fill("0")
        page.locator("#pressure-sample").dispatch_event("input")
        assert "hoop demand 0.0 MPa" in page.locator("#sample-readout").inner_text()
        page.locator("#comment-text").fill("<img src=x onerror=alert(1)> Confirm the diameter basis.")
        page.locator('#comment-form button').click()
        assert page.locator("#comment-list li").count() == 1
        assert page.locator("#comment-list img").count() == 0
        page.locator("#save-comments").click()
        page.wait_for_function("window.savedComments !== null")
        saved = json.loads(page.evaluate("window.savedComments"))
        assert saved["comments"][0]["text"].endswith("Confirm the diameter basis.")
        assert "Comments saved beside" in page.locator("#comment-status").inner_text()
        page.reload()
        assert page.locator("#comment-list li").count() == 1
        invalid = {**saved, "revision": "wrong"}
        page.locator("#load-comments").set_input_files({
            "name": "comments.json", "mimeType": "application/json", "buffer": json.dumps(invalid).encode()
        })
        page.wait_for_function("document.getElementById('comment-status').textContent.includes('not loaded')")
        assert page.locator("#comment-list li").count() == 1
        saved["comments"][0]["text"] = "Loaded comment."
        page.on("dialog", lambda dialog: dialog.accept())
        page.locator("#load-comments").set_input_files({
            "name": "comments.json", "mimeType": "application/json", "buffer": json.dumps(saved).encode()
        })
        page.wait_for_function("document.getElementById('comment-list').textContent.includes('Loaded comment.')")
        with page.expect_download() as result:
            page.locator("#download-comments").click()
        assert result.value.suggested_filename == "structural-module-report.comments.json"
        for width in (390, 1100):
            page.set_viewport_size({"width": width, "height": 850})
            assert page.locator("#section-5 svg").bounding_box()["width"] > 0
        assert not errors
        context.close()
        browser.close()
