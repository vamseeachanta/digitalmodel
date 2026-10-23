# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Rebuild the public module report and compare its example with the actual API."""

import importlib.util
import hashlib
import json
import os
import re
import shutil
import subprocess
import tomllib
from types import SimpleNamespace
from pathlib import Path

import pytest
from packaging.requirements import Requirement
from bs4 import BeautifulSoup

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
    inputs, constants = source["inputs"], source["constants"]
    solid_gm = inputs["km_ft"] - inputs["kg_ft"]
    assert data["results"]["solid_gm_ft"] == pytest.approx(solid_gm, rel=1e-12)
    expected_correction = (inputs["rho_fluid"] * constants["g_ft_s2"]["value"]
                           * inputs["tank_length_ft"] * inputs["tank_breadth_ft"] ** 3
                           / constants["rectangular_inertia_denominator"]["value"]
                           / (inputs["displacement_lt"] * constants["lb_per_long_ton"]["value"]))
    assert data["derived"]["reduction_pct"] == pytest.approx(
        100 * expected_correction / solid_gm, rel=1e-12)
    conditions = [(inputs["tank_breadth_ft"], data["results"])]
    sensitivity = source["sensitivity"]
    count = sensitivity["sample_count"]
    assert len(data["sweep"]) == count
    for index, point in enumerate(data["sweep"]):
        fraction = sensitivity["breadth_fraction_min"] + index / (count - 1) * (
            sensitivity["breadth_fraction_max"] - sensitivity["breadth_fraction_min"])
        breadth = inputs["tank_breadth_ft"] * fraction
        assert point["breadth_ft"] == pytest.approx(breadth, rel=1e-12)
        conditions.append((breadth, point))
    for breadth, result in conditions:
        moment = inputs["tank_length_ft"] * breadth ** 3 / constants["rectangular_inertia_denominator"]["value"]
        correction = (inputs["rho_fluid"] * constants["g_ft_s2"]["value"] * moment
                      / (inputs["displacement_lt"] * constants["lb_per_long_ton"]["value"]))
        assert result["correction_ft"] == pytest.approx(correction, rel=1e-12)
        assert result["effective_gm_ft"] == pytest.approx(solid_gm - correction, rel=1e-12)
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
    # Engineering numbers in the worked narrative must come from generated spans.
    for paragraph in soup.select("#example > p"):
        for generated in paragraph.select("[data-number], a"):
            generated.decompose()
        assert not re.search(r"\d", paragraph.get_text())


def test_report_registered_for_shadow_rebuild():
    from scripts import check_generated_html as checker

    entries = [entry for entry in checker.GENERATORS
               if entry.script == "scripts/reporting/build_navalarchitecture_module_report.py"]
    assert len(entries) == 1
    assert set(entries[0].outputs) == {
        "docs/reports/modules/navalarchitecture/report.html",
        "docs/reports/modules/navalarchitecture/changes.json",
    }


@pytest.mark.parametrize("relative", [
    "src/digitalmodel/naval_architecture/stability.py",
    "docs/reports/modules/navalarchitecture/report.template.html",
    "scripts/reporting/build_navalarchitecture_module_report.py",
])
def test_changed_report_input_rejects_stale_evidence(digest_tree, relative):
    module = digest_tree
    original_outputs = {name: (REPORT / name).read_bytes()
                        for name in ("report.html", "changes.json", "checks.json")}
    checks = json.loads((REPORT / "checks.json").read_text(encoding="utf-8"))
    checks["source_digest"] = module.evidence_digest()
    (module.REPORT / "checks.json").write_text(json.dumps(checks), encoding="utf-8")
    module.build_report(output_dir=module.REPORT)
    original = (ROOT / relative).read_bytes()
    digest = module.evidence_digest()
    (module.ROOT / relative).write_bytes(original + b"\n# Changed report input\n")
    with pytest.raises(ValueError, match="retained check record"):
        module.build_report(output_dir=module.REPORT, check=True)
    assert module.evidence_digest() != digest
    assert (ROOT / relative).read_bytes() == original
    assert {name: (REPORT / name).read_bytes() for name in original_outputs} == original_outputs


def test_report_structure_sources_and_offline_delivery():
    module = builder()
    html = module.report_files()["report.html"]
    soup = BeautifulSoup(html, "html.parser")
    assert "hs-doc" in soup.body["class"]
    assert soup.select_one("#controls h2").get_text() == "3 Input Controls"
    assert soup.select_one(".sub").get_text() == "Draft for review"
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
    canonical = soup.select('style[data-source="docs/api/_assets/brand.css"]')
    assert len(canonical) == 1
    assert canonical[0].string == (ROOT / "docs/api/_assets/brand.css").read_text(encoding="utf-8")
    digest = soup.select_one('meta[name="report-content-digest"]')["content"]
    canonical = html.replace(digest, "CONTENT_DIGEST")
    assert hashlib.sha256(canonical.encode()).hexdigest() == digest
    assert json.loads(soup.select_one("#changes-data").string) == json.loads(
        (REPORT / "changes.json").read_text(encoding="utf-8"))


def test_check_detects_stale_output_without_writing(tmp_path):
    module = builder()
    module.build_report(tmp_path)
    target = tmp_path / "report.html"
    stale = target.read_text().replace("Draft for review", "Superseded text")
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
            executable_path=chromium_executable(runtime), headless=True)
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


@pytest.fixture
def digest_tree(tmp_path, monkeypatch):
    module = builder()
    relatives = (
        "scripts/reporting/build_navalarchitecture_module_report.py",
        "src/digitalmodel/naval_architecture/stability.py",
        "tests/naval_architecture/test_module_report.py",
        "docs/reports/modules/navalarchitecture/example.json",
        "docs/reports/modules/navalarchitecture/report.template.html",
        "docs/api/cfd/turbulent-flat-plate-verification.html",
        "docs/api/_assets/brand.css",
    )
    for relative in relatives:
        target = tmp_path / relative
        target.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy2(ROOT / relative, target)
    monkeypatch.setattr(module, "ROOT", tmp_path)
    monkeypatch.setattr(module, "REPORT", tmp_path / REPORT.relative_to(ROOT))
    monkeypatch.setattr(module, "STYLE_EXEMPLAR", tmp_path / module.STYLE_EXEMPLAR.relative_to(ROOT))
    monkeypatch.setattr(module, "__file__", str(tmp_path / Path(module.__file__).relative_to(ROOT)))
    return module


@pytest.mark.parametrize("relative", ["src/unrelated.py", "scripts/unrelated.py", "data/unrelated.json"])
def test_unrelated_changes_do_not_invalidate_report(digest_tree, relative):
    module = digest_tree
    before = module.evidence_digest()
    target = module.ROOT / relative
    target.parent.mkdir(parents=True, exist_ok=True)
    target.write_text("unrelated\n", encoding="utf-8")
    assert module.evidence_digest() == before


def test_digest_normalizes_checkout_line_endings(digest_tree):
    module = digest_tree
    before = module.evidence_digest()
    for path in module.ROOT.rglob("*"):
        if path.is_file():
            path.write_bytes(path.read_bytes().replace(b"\r\n", b"\n").replace(b"\n", b"\r\n"))
    assert module.evidence_digest() == before


def test_digest_uses_explicit_manifest_and_posix_order(digest_tree, monkeypatch):
    module = digest_tree
    names = ("src/a.py", "src/B.py")
    expected = hashlib.sha256()
    for name in sorted(names):
        path = module.ROOT / name
        path.write_bytes(b"example\n")
        expected.update(name.encode() + b"\0example\n\0")
    monkeypatch.setattr(module, "REPORT_INPUTS", names, raising=False)
    assert module.evidence_digest() == expected.hexdigest()


def test_check_receipt_records_command_and_environment(tmp_path):
    module = builder()
    module.REPORT = tmp_path
    receipt = tmp_path / "naval-architecture.xml"
    receipt.write_text(
        '<testsuite><testcase classname="tests.naval_architecture.test_example" name="test_pass"/>'
        '<testcase classname="tests.naval_architecture.test_example" name="test_fail"><failure/></testcase>'
        '<testcase classname="tests.naval_architecture.test_example" name="test_skip"><skipped/></testcase>'
        '<testcase classname="tests.naval_architecture.test_example" name="test_xfail">'
        '<skipped type="pytest.xfail"/></testcase></testsuite>', encoding="utf-8")
    command = "PYTHONPATH=src python -m pytest tests/naval_architecture --junitxml=naval-architecture.xml"
    module.record_checks(receipt, command=command)
    record = json.loads((tmp_path / "checks.json").read_text())
    assert record["command"] == command
    assert record["source_files"] == sorted(module.REPORT_INPUTS)
    assert record["environment"]["python"]
    assert record["environment"]["packages"]["pytest"]
    expected_extras = (["cad"] if {"rhino3dm", "cadquery-ocp"}
                       <= record["environment"]["packages"].keys() else [])
    assert record["environment"]["extras"] == expected_extras
    assert [record[key] for key in ("tests", "passed", "failed", "skipped", "expected_failures")] == [4, 1, 1, 1, 1]


def test_browser_availability_skips_missing_binary(tmp_path, monkeypatch):
    monkeypatch.delenv("PLAYWRIGHT_CHROMIUM_EXECUTABLE", raising=False)
    runtime = SimpleNamespace(chromium=SimpleNamespace(executable_path=str(tmp_path / "missing")))
    with pytest.raises(pytest.skip.Exception, match="Chromium executable unavailable"):
        chromium_executable(runtime)


def test_browser_availability_respects_explicit_binary(tmp_path, monkeypatch):
    executable = tmp_path / "chromium"
    executable.write_text("browser fixture", encoding="utf-8")
    monkeypatch.setenv("PLAYWRIGHT_CHROMIUM_EXECUTABLE", str(executable))
    assert chromium_executable(SimpleNamespace()) == str(executable)


def chromium_executable(runtime):
    """Skip absent browser installations; launch errors remain test failures."""
    executable = os.environ.get("PLAYWRIGHT_CHROMIUM_EXECUTABLE") or runtime.chromium.executable_path
    if not Path(executable).is_file():
        pytest.skip("Chromium executable unavailable; install Chromium or set PLAYWRIGHT_CHROMIUM_EXECUTABLE")
    return executable


@pytest.mark.parametrize("package", ["pandas", "pillow"])
def test_retained_receipt_uses_declared_dependency_bounds(package):
    record = json.loads((REPORT / "checks.json").read_text(encoding="utf-8"))
    project = tomllib.loads((ROOT / "pyproject.toml").read_text(encoding="utf-8"))
    requirements = [Requirement(item) for item in project["project"]["dependencies"]]
    requirement = next(item for item in requirements if item.name == package)
    assert record["environment"]["packages"][package] in requirement.specifier


def test_module_ignore_rules_preserve_earlier_sibling_allowlist(tmp_path):
    rules = (ROOT / ".gitignore").read_text(encoding="utf-8")
    marker = "# Retain the reproducible naval architecture module report and its source basis."
    sibling = "docs/reports/modules/sibling/"
    rules = rules.replace(marker, f"!{sibling}\n{sibling}*\n!{sibling}report.html\n\n{marker}")
    (tmp_path / ".gitignore").write_text(rules, encoding="utf-8")
    git_dir = subprocess.run(
        ["git", "rev-parse", "--absolute-git-dir"], cwd=ROOT,
        check=True, capture_output=True, text=True).stdout.strip()
    retained = [f"docs/reports/modules/navalarchitecture/{name}" for name in
                ("report.html", "report.template.html", "example.json", "checks.json", "changes.json")]
    retained.append(f"{sibling}report.html")
    ignored = ["docs/reports/modules/navalarchitecture/report.comments.json",
               f"{sibling}report.comments.json", "docs/reports/modules/unlisted/report.html"]
    result = subprocess.run(
        ["git", f"--git-dir={git_dir}", f"--work-tree={tmp_path}",
         "check-ignore", "--no-index", "--stdin"], cwd=tmp_path,
        input="\n".join(retained + ignored) + "\n", check=True,
        capture_output=True, text=True)
    assert set(result.stdout.splitlines()) == set(ignored)
    assert rules.splitlines().count("docs/reports/modules/*") == 1
