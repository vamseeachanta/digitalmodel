# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Browser layout regressions; Chromium must be installed for Playwright.

PLAYWRIGHT_CHROMIUM_EXECUTABLE_PATH may select an existing Chromium binary.
"""

import os
import shlex
import subprocess
from pathlib import Path

import pytest
import yaml
from playwright.sync_api import sync_playwright


PAGE = (Path(__file__).resolve().parents[2]
        / "docs/api/hydro/short-horizon-motion-forecast.html")
TRACES = "#cvWave, #cvMotion, #cvGov"


def test_capabilities_ci_installs_matching_chromium(monkeypatch, request):
    root = PAGE.parents[3]
    workflow = yaml.safe_load(
        (root / ".github/workflows/quality-gates-by-domain.yml").read_text(
            encoding="utf-8"
        )
    )
    steps = next(job["steps"] for job in workflow["jobs"].values()
                 if any(step.get("id") == "domain-gate"
                        for step in job.get("steps", [])))
    installs = [step for step in steps
                if "playwright" in step.get("run", "")]
    assert len(installs) == 1, "Capabilities CI must install Chromium"
    install = installs[0]
    assert install["if"] == "matrix.domain == 'capabilities'"
    gate_index = next(i for i, step in enumerate(steps)
                      if step.get("id") == "gate")
    run_index = next(i for i, step in enumerate(steps)
                     if step.get("id") == "domain-gate")
    assert gate_index < steps.index(install) < run_index
    assert install["env"]["DOMAIN_GATE_COMMAND"] == "${{ steps.gate.outputs.command }}"
    gates = yaml.safe_load(
        (root / ".claude/quality-gates.yaml").read_text(encoding="utf-8")
    )
    command = gates["gates"]["tests-capabilities"]["command"]
    monkeypatch.setenv("DOMAIN_GATE_COMMAND", command)
    calls = []
    monkeypatch.setattr(subprocess, "run",
                        lambda args, **kwargs: calls.append((args, kwargs)))
    script = install["run"].split("<<'PY'\n", 1)[1].rsplit("\nPY", 1)[0]
    exec(compile(script, "<browser-install>", "exec"), {})
    tokens = shlex.split(command)
    prefix = tokens[:tokens.index("pytest")]
    assert calls == [(prefix + ["playwright", "install", "--with-deps", "chromium"],
                      {"check": True})]

    def fail_install(args, **kwargs):
        raise subprocess.CalledProcessError(1, args)

    monkeypatch.setattr(subprocess, "run", fail_install)
    with pytest.raises(subprocess.CalledProcessError):
        exec(compile(script, "<browser-install>", "exec"), {})

    assert "-m" not in tokens[tokens.index("pytest") + 1:], (
        "The provisioned capabilities shard must run layout tests"
    )
    assert any(marker.startswith("browser_layout:")
               for marker in request.config.getini("markers"))
    for test in (test_phone_traces_and_status_do_not_overflow,
                 test_resize_switches_between_mobile_and_desktop_sizing):
        assert "browser_layout" in [marker.name for marker in test.pytestmark]

    # Every other workflow that selects these tests must exclude browser layout.
    for filename, step_id, expression in (
        ("full-matrix-sweep.yml", "domain-gate", "not browser_layout"),
        ("publish.yml", None, "not solver and not slow and not browser_layout"),
    ):
        caller = yaml.safe_load(
            (root / ".github/workflows" / filename).read_text(encoding="utf-8")
        )
        run = next(step["run"] for job in caller["jobs"].values()
                   for step in job.get("steps", [])
                   if (step.get("id") == step_id if step_id
                       else step.get("name") == "Run tests"))
        if step_id:
            # Select the capabilities branch of the shared domain invocation.
            run = next(line.strip() for line in run.splitlines()
                       if line.strip().startswith("${{ steps.gate.outputs.command }}"))
        args = shlex.split(run)
        assert args[args.index("-m") + 1] == expression, filename
    domain_run = steps[run_index]["run"]
    assert "-m " not in domain_run, "Provisioned layout tests must remain enabled"


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


@pytest.mark.browser_layout
def test_phone_traces_and_status_do_not_overflow(page):
    resize(page, 390, 844)
    sizes = canvas_sizes(page)
    # Application requirement: see sizePanels() in the HTML. Hand calculation:
    # wave = 420 x 0.30 = 126; motion = 420 x 0.40 = 168;
    # governing = 420 - 126 - 168 = 126 CSS px, all above the chosen 100 px floor.
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


@pytest.mark.browser_layout
def test_resize_switches_between_mobile_and_desktop_sizing(page):
    desktop = canvas_sizes(page)
    # Independent fresh-load measurement at baseline 05067ed1, 1440 x 900,
    # Chromium, light theme, scale 1, external fonts blocked (see HTML basis).
    assert [width for width, _ in desktop] == [864, 864, 864]
    assert [height for _, height in desktop] == [100, 133, 99]
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
