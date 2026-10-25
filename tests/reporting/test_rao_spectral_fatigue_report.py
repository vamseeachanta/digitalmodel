# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Rebuilt public-example values must match the numerical module, including charts."""

import copy
import csv
import hashlib
import importlib.util
import json
import math
import os
import re
import subprocess
import sys
from html.parser import HTMLParser
from pathlib import Path

import numpy as np
import pytest
import yaml

from digitalmodel.rao_spectral_fatigue.workflow import router, _stress_psd_from_rao

ROOT = Path(__file__).resolve().parents[2]
FOLDER = Path("docs/reports/modules/raospectralfatigue")
GENERATOR = ROOT / "scripts/reporting/build_rao_spectral_fatigue_report.py"
EXAMPLE = ROOT / "examples/workflows/rao-spectral-fatigue/input.yml"


class ReportParser(HTMLParser):
    """Collect displayed example values independently of the generator."""

    def __init__(self):
        super().__init__()
        self.values = []
        self.active = None
        self.visible = []
        self.hidden = 0
        self.remote_resources = []

    def handle_starttag(self, tag, attrs):
        attrs = dict(attrs)
        if (tag == "script" and "src" in attrs) or (tag == "link" and attrs.get("rel") == "stylesheet"):
            self.remote_resources.append(attrs)
        if tag in {"script", "style"}:
            self.hidden += 1
        if "data-value" in attrs:
            self.active = [attrs["data-value"], ""]

    def handle_data(self, data):
        if not self.hidden:
            self.visible.append(data)
        if self.active is not None:
            self.active[1] += data

    def handle_endtag(self, tag):
        if tag == "span" and self.active is not None:
            self.values.append(tuple(self.active))
            self.active = None
        if tag in {"script", "style"}:
            self.hidden -= 1


def load_generator():
    assert GENERATOR.is_file(), "The report generator has not been implemented"
    spec = importlib.util.spec_from_file_location("rao_report", GENERATOR)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def payload(page):
    return json.loads(re.search(
        r'<script id="report-data" type="application/json">(.*?)</script>',
        page, re.S,
    ).group(1))


def lookup(data, key):
    for part in key.split("."):
        data = data[int(part)] if isinstance(data, list) else data[part]
    return data


@pytest.fixture(scope="module")
def rebuilt(tmp_path_factory):
    folder = tmp_path_factory.mktemp("report")
    load_generator().build(folder, folder / "scratch")
    return folder, (folder / "index.html").read_text(encoding="utf-8")


def test_rebuild_is_deterministic_and_preserves_sources(rebuilt, tmp_path):
    folder, page = rebuilt
    second = tmp_path / "second"
    before = {p: hashlib.sha256((ROOT / p).read_bytes()).hexdigest()
              for p in payload(page)["sources"]}
    load_generator().build(second, tmp_path / "scratch")
    for name in ("index.html", "changes.json"):
        assert (folder / name).read_bytes() == (second / name).read_bytes()
        assert (folder / name).read_bytes() == (ROOT / FOLDER / name).read_bytes()
    assert before == {p: hashlib.sha256((ROOT / p).read_bytes()).hexdigest()
                      for p in before}
    assert payload(page)["sources"] == {
        p: hashlib.sha256((ROOT / p).read_text(encoding="utf-8").encode("utf-8")).hexdigest()
        for p in before
    }


def test_rebuild_normalizes_checkout_line_endings(rebuilt, tmp_path, monkeypatch):
    module = load_generator()
    checkout = tmp_path / "checkout"
    for relative in module.SOURCES:
        target = checkout / relative
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_bytes((ROOT / relative).read_text(encoding="utf-8").replace("\n", "\r\n").encode("utf-8"))
    monkeypatch.setattr(module, "ROOT", checkout)
    output = tmp_path / "output"
    module.build(output, tmp_path / "scratch")
    for name in ("index.html", "changes.json"):
        assert (output / name).read_bytes() == (rebuilt[0] / name).read_bytes()
        assert b"\r" not in (output / name).read_bytes()


def test_rebuild_tolerates_last_bit_numerical_variation(rebuilt, tmp_path, monkeypatch):
    module = load_generator()
    original = module.evaluate(tmp_path / "scratch")

    def perturb(item):
        if isinstance(item, float):
            return math.nextafter(item, math.inf) if item else item
        if isinstance(item, list):
            return [perturb(value) for value in item]
        if isinstance(item, dict):
            return {key: perturb(value) for key, value in item.items()}
        return item

    monkeypatch.setattr(module, "evaluate", lambda scratch: perturb(original))
    output = tmp_path / "perturbed"
    module.build(output, tmp_path / "scratch")
    assert (output / "index.html").read_bytes() == (rebuilt[0] / "index.html").read_bytes()


def test_builder_is_registered_with_its_reproduction_inputs():
    from scripts import check_generated_html as checker

    entries = [entry for entry in checker.GENERATORS
               if entry.script == GENERATOR.relative_to(ROOT).as_posix()]
    assert len(entries) == 1
    assert set(entries[0].outputs) == {str(FOLDER / name) for name in ("index.html", "changes.json")}
    # Regeneration needs evidence sources outside scripts/, docs/api/ and src/.
    assert {EXAMPLE.relative_to(ROOT).as_posix(), "tests/workflows/test_durable_workflows.py",
            "tests/fatigue/test_spectral_fatigue.py"} <= set(entries[0].inputs)


def test_every_example_value_and_chart_equals_module(rebuilt, tmp_path):
    _, page = rebuilt
    data = payload(page)["example"]
    original = yaml.safe_load(EXAMPLE.read_text(encoding="utf-8"))
    cfg = copy.deepcopy(original)
    cfg["rao_spectral_fatigue"]["output_dir"] = str(tmp_path)
    result = router(cfg)["rao_spectral_fatigue"]
    with Path(result["results_csv"]).open(encoding="utf-8") as stream:
        rows = list(csv.DictReader(stream))
    assert data["design_life_years"] == result["design_life_years"]
    assert data["dff"] == result["dff"]
    assert len(data["locations"]) == len(result["locations"])
    for actual, expected, source in zip(
        data["locations"], result["locations"],
        original["rao_spectral_fatigue"]["locations"], strict=True,
    ):
        for field in ("annual_damage", "fatigue_life_years", "required_life_years",
                      "margin", "passes"):
            assert actual[field] == pytest.approx(expected[field], rel=1e-11)
        for field in ("rao_frequency_Hz", "rao_stress_MPa_per_m"):
            assert actual[field] == source[field]
        location_rows = [r for r in rows if r["location_id"] == expected["location_id"]]
        for state, source_state, row in zip(
            actual["sea_states"], source["sea_states"], location_rows, strict=True,
        ):
            for field, value in source_state["wave_spectrum"].items():
                assert state[field] == value
            assert state["occurrence_fraction"] == source_state["occurrence_fraction"]
            for field in ("rms_stress_MPa", "bandwidth_parameter", "peak_rate_Hz",
                          "damage_per_year_full", "damage_per_year_weighted"):
                assert state[field] == pytest.approx(float(row[field]), rel=1e-11)
            frequency, psd = _stress_psd_from_rao(
                source["rao_frequency_Hz"], source["rao_stress_MPa_per_m"],
                source_state["wave_spectrum"], source["id"],
            )
            np.testing.assert_allclose(state["frequency_Hz"], frequency, rtol=1e-11, atol=0)
            np.testing.assert_allclose(state["stress_psd_MPa2_Hz"], psd, rtol=1e-11, atol=0)
    parser = ReportParser()
    parser.feed(page)
    assert len(parser.values) >= 40, "Example inputs and results must be visible"
    for key, text in parser.values:
        value = lookup(payload(page), key)
        expected = (f"{value:.2f}" if key.endswith("occurrence_fraction") else
                    f"{value:.3g}" if 0 < abs(value) < 0.1 else f"{value:.1f}")
        assert text == expected, (key, text, expected)


def test_structure_offline_style_and_engineering_wording(rebuilt):
    _, page = rebuilt
    assert '<body class="hs-doc">' in page
    assert len(re.findall(r'<section id="s\d+">', page)) == 10
    assert '<h4' not in page and '<b>' not in page and '<strong>' not in page
    assert page.index('id="revision-history"') < page.index('<section id="s2">')
    exemplar = (ROOT / "docs/api/cfd/turbulent-flat-plate-verification.html").read_text(encoding="utf-8")
    assert re.search(r'<style>(.*?)</style>', exemplar, re.S).group(1) in page
    parser = ReportParser()
    parser.feed(page)
    assert parser.remote_resources == []
    visible = " ".join(parser.visible)
    assert not re.search(r'\b(owner|could|should|would|lane|agent|session)\b', visible, re.I)
    private_roots = "|".join(f"/{name}/" for name in ("home", "mnt"))
    assert not re.search(private_roots + r'|@[\w.-]+\.[a-z]{2,}', page)
    assert 'screening arithmetic' in visible
    assert 'Not covered' in visible
    assert 'NOT_EVALUATED' in visible
    assert "Draft for maintainer review" in visible
    assert "#2583" in visible
    assert "single-slope" in visible
    changes = json.loads((rebuilt[0] / "changes.json").read_text(encoding="utf-8"))
    assert payload(page)["changes"] == changes


def test_reference_narrow_band_anchor_and_supplied_sn_basis(rebuilt):
    data = payload(rebuilt[1])["example"]
    source = yaml.safe_load(EXAMPLE.read_text(encoding="utf-8"))["rao_spectral_fatigue"]
    assert data["sn_curve"] == source["sn_curve"]
    # Independently evaluated public-grid Rayleigh range moment:
    # E[range**b] = (2*sqrt(2*m0))**b * Gamma(1+b/2).
    # These anchors use the supplied illustrative curve, not a qualified standard.
    keel = data["locations"][1]
    state = keel["sea_states"][0]
    frequency = np.asarray(state["frequency_Hz"])
    psd = np.asarray(state["stress_psd_MPa2_Hz"])
    m0 = np.trapz(psd, frequency)
    m2 = np.trapz(frequency ** 2 * psd, frequency)
    slope = source["sn_curve"]["slope"]
    closed_form = (math.sqrt(m2 / m0) * (2 * math.sqrt(2 * m0)) ** slope *
                   math.gamma(1 + slope / 2) / 10 ** source["sn_curve"]["log_intercept"] *
                   365.25 * 24 * 3600)
    assert keel["reference_narrow_band_annual_damage"] == pytest.approx(closed_form, rel=1e-11)
    assert keel["reference_narrow_band_annual_damage"] == pytest.approx(0.001791243679674115, rel=1e-11)
    assert keel["reference_life_years"] == pytest.approx(567.6537610246007, rel=1e-11)
    splash = data["locations"][0]
    assert splash["reference_life_years"] == pytest.approx(66.18722006460763, rel=1e-11)
    for location in data["locations"]:
        assert location["reference_life_years"] > location["fatigue_life_years"]


def test_comment_binding_covers_complete_html(rebuilt):
    _, page = rebuilt
    data = payload(page)
    digest = data.pop("content_id")
    skeleton = re.sub(
        r'(<script id="report-data" type="application/json">).*?(</script>)',
        r'\1__REPORT_DATA__\2', page, count=1, flags=re.S,
    )
    serialized = json.dumps(data, ensure_ascii=False, sort_keys=True,
                            separators=(",", ":"), allow_nan=False).replace("<", "\\u003c")
    assert hashlib.sha256((skeleton + serialized).encode()).hexdigest() == digest


def test_header_verifier_rejects_non_header_changes(tmp_path):
    module = load_generator()
    before, after = tmp_path / "before.py", tmp_path / "after.py"
    before.write_bytes(b'"""Example source."""\n')
    header = b"\n".join(b"# " + line for line in
                         (ROOT / ".license-header.txt").read_bytes().rstrip().splitlines()) + b"\n\n"
    after.write_bytes(header + before.read_bytes())
    module.verify_header_only(before, after)
    after.write_bytes(header + b'"""Changed source."""\n')
    with pytest.raises(ValueError, match="non-header"):
        module.verify_header_only(before, after)


def test_report_browser_behaviour_without_network():
    test = ROOT / "tests/reporting/rao_spectral_fatigue_ui_test.cjs"
    env = dict(os.environ)
    for key in ("GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR"):
        env.pop(key, None)
    result = subprocess.run(["node", str(test)], cwd=ROOT, env=env,
                            text=True, capture_output=True, check=False)
    assert result.returncode == 0, result.stdout + result.stderr


def test_cli_builds_without_importing_solver_adapters(tmp_path):
    code = '''
import importlib.abc
import runpy
import sys
class NoSolvers(importlib.abc.MetaPathFinder):
    def find_spec(self, fullname, path=None, target=None):
        if fullname.startswith(("digitalmodel.solvers", "OrcFxAPI", "orcfxapi", "ansys")):
            raise RuntimeError("Solver import is prohibited")
sys.meta_path.insert(0, NoSolvers())
script, output, scratch = sys.argv[1:]
sys.argv = [script, "--output-dir", output, "--scratch-dir", scratch]
runpy.run_path(script, run_name="__main__")
'''
    env = dict(os.environ, PYTHONDONTWRITEBYTECODE="1")
    for key in ("GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR"):
        env.pop(key, None)
    output = tmp_path / "cli"
    result = subprocess.run(
        [sys.executable, "-c", code, str(GENERATOR), str(output), str(tmp_path / "scratch")],
        cwd=ROOT, env=env, capture_output=True, text=True, check=False,
    )
    assert result.returncode == 0, result.stdout + result.stderr
    assert (output / "index.html").read_bytes() == (ROOT / FOLDER / "index.html").read_bytes()
