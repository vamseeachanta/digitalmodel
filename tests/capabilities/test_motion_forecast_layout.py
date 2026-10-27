# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Browser layout regressions; Chromium must be installed for Playwright.

PLAYWRIGHT_CHROMIUM_EXECUTABLE_PATH may select an existing Chromium binary.
"""

import os
from pathlib import Path

import pytest
from playwright.sync_api import sync_playwright


PAGE = (Path(__file__).resolve().parents[2]
        / "docs/api/hydro/short-horizon-motion-forecast.html")
TRACES = "#cvWave, #cvMotion, #cvGov"


@pytest.fixture
def page():
    with sync_playwright() as playwright:
        options = {}
        if executable := os.environ.get("PLAYWRIGHT_CHROMIUM_EXECUTABLE_PATH"):
            options["executable_path"] = executable
        browser = playwright.chromium.launch(**options)
        page = browser.new_page(viewport={"width": 1440, "height": 900},
                                device_scale_factor=1, reduced_motion="reduce")
        page.route("https://**/*", lambda route: route.abort())
        page.route("http://**/*", lambda route: route.abort())
        page.goto(PAGE.as_uri())
        yield page
        browser.close()


def canvas_sizes(page):
    return page.locator(TRACES).evaluate_all(
        "canvases => canvases.map(c => { const r = c.getBoundingClientRect(); "
        "return [r.width, r.height]; })"
    )


def resize(page, width, height):
    page.set_viewport_size({"width": width, "height": height})
    # Wait for the resize handler and its render to finish.
    page.evaluate("() => new Promise(resolve => "
                  "requestAnimationFrame(() => requestAnimationFrame(resolve)))")


def test_phone_traces_and_status_do_not_overflow(page):
    resize(page, 390, 844)
    sizes = canvas_sizes(page)
    assert [height for _, height in sizes] == [126, 168, 126]
    assert all(height >= 100 for _, height in sizes)
    assert page.evaluate("document.documentElement.scrollWidth <= innerWidth")
    assert page.locator(".status").evaluate(
        "s => getComputedStyle(s).gridTemplateColumns.split(' ').length"
    ) == 1
    assert page.locator(".countdown").evaluate(
        "c => getComputedStyle(c).textAlign"
    ) == "left"
    assert page.locator(".lead-explain").evaluate(
        "c => getComputedStyle(c).maxWidth"
    ) == "none"


def test_resize_switches_between_mobile_and_desktop_sizing(page):
    desktop = canvas_sizes(page)
    # Desktop retains the original measured viewport budget and 30/40/30 split.
    total = page.evaluate("""() => {
        const traceHeight = ['cvWave', 'cvMotion', 'cvGov'].reduce(
            (sum, id) => sum + document.getElementById(id).getBoundingClientRect().height, 0);
        const nonTrace = document.querySelector('.transport').getBoundingClientRect().bottom - traceHeight;
        return Math.max(150, Math.min(innerHeight - nonTrace - 10, 560));
    }""")
    wave, motion = int(total * .3 + .5), int(total * .4 + .5)
    assert [height for _, height in desktop] == [wave, motion, int(total - wave - motion)]
    for width, height in [(390, 844), (844, 390), (860, 900)]:
        resize(page, width, height)
        # Fractional CSS widths can scale the canvas by a fraction of a pixel.
        heights = [h for _, h in canvas_sizes(page)]
        assert heights == pytest.approx([126, 168, 126], abs=1)
        assert all(h >= 100 for h in heights)
        assert page.evaluate("document.documentElement.scrollWidth <= innerWidth")
    resize(page, 861, 900)
    assert [h for _, h in canvas_sizes(page)] != [126, 168, 126]
    resize(page, 1440, 900)
    assert [width for width, _ in canvas_sizes(page)] == [width for width, _ in desktop]
    assert 150 <= sum(height for _, height in canvas_sizes(page)) <= 560
    assert [h for _, h in canvas_sizes(page)] != [126, 168, 126]
    # Compare fresh loads: the existing viewport fit measures other canvases
    # before rendering, so intermediate resize widths can affect its budget.
    page.reload()
    assert canvas_sizes(page) == desktop
