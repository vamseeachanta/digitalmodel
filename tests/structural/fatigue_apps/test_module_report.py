# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Check the synthetic converter report against independent equilibrium arithmetic.

Basis: uniform axial stress F = sigma*A; synthetic A = 2500 mm^2, also derived
in test_tension_stress_conversion_hand_calcs.py. NIST, Metric (SI) Prefixes,
https://www.nist.gov/pml/owm/metric-si-prefixes, kilo/mega/milli rows, gives
1000 N/kN, 1e6 Pa/MPa and (1e-3)^2 m^2/mm^2. Therefore sigma = 0.4*T MPa.
The hand table is (25,10), (100,40), (250,100) in kN/MPa. Midpoint probes
62.5 and 175 kN give 25 and 70 MPa; extrapolation at 0 and 500 kN gives
0 and 200 MPa. Decimal arithmetic below supplies the oracle independently.

Roundoff criterion: binary64 u = ulp(1)/2. The linear segment uses six rounded
operations; force/area with unit conversion uses four; comparison uses one;
the non-dyadic prefix factor needs one representation allowance. Four reserve
operations give a budget of 16. Use
gamma_16 = 16*u/(1-16*u). Magnification 4 exceeds the largest interpolation
coordinate (500-100)/(250-100) = 8/3. At the maximum synthetic stress 200 MPa,
the absolute budget is gamma_16*4*200 MPa; relative tolerance is zero. This
roundoff-only allowance has no physical calibration or measurement meaning.
"""
from __future__ import annotations

import csv
import hashlib
import importlib.util
import json
import math
import os
import re
import subprocess
import sys
from decimal import Decimal
from html.parser import HTMLParser
from pathlib import Path

import numpy as np
import pytest

from digitalmodel.structural.fatigue_apps.tension_stress_conversion import (
    TensionStressConverter,
)

ROOT = Path(__file__).resolve().parents[3]
REPORT = ROOT / "docs/reports/modules/fatigue_apps"
ARTIFACTS = ("report.html", "changes.json", "sources.json")
TABLE = ((25.0, 10.0), (100.0, 40.0), (250.0, 100.0))
PROBES = (25.0, 100.0, 250.0, 62.5, 175.0, 0.0, 500.0)
REGIMES = ("node", "node", "node", "interpolation", "interpolation",
           "extrapolation", "extrapolation")
UNIT_ROUNDOFF = math.ulp(1.0) / 2
BUDGET = (16 * UNIT_ROUNDOFF / (1 - 16 * UNIT_ROUNDOFF)) * 4 * 200


def hand_stress(tension_kn):
    """Evaluate F/A with decimal SI factors, never the converter or builder."""
    return float(Decimal(str(tension_kn)) * Decimal("1000") /
                 (Decimal("2500") * Decimal("0.000001")) / Decimal("1000000"))


class Page(HTMLParser):
    """Read visible numeric spans, table cells, links and embedded JSON."""

    def __init__(self, text):
        super().__init__(convert_charrefs=True)
        self.ids, self.links, self.resources, self.numbers = [], [], [], []
        self.visible, self.rows, self.headers = [], [], []
        self.scripts = {}
        self.hidden = self.binding = self.cell = self.row = None
        self.script_id = None
        self.feed(text)

    def handle_starttag(self, tag, attrs):
        attrs = dict(attrs)
        if "id" in attrs:
            self.ids.append(attrs["id"])
        if tag in {"style", "script"}:
            self.hidden = tag
        if tag == "script":
            self.script_id = attrs.get("id")
            self.scripts[self.script_id] = ""
        if tag == "a":
            self.links.append(attrs.get("href", ""))
        if tag in {"link", "script", "img", "iframe"}:
            self.resources.extend(attrs[k] for k in ("src", "href") if k in attrs)
        if tag == "tr":
            self.row = []
        if tag in {"td", "th"}:
            self.cell = []
        if "data-value" in attrs:
            self.binding = [attrs, ""]

    def handle_data(self, data):
        if self.hidden == "script":
            self.scripts[self.script_id] += data
        if not self.hidden:
            self.visible.append(data)
        if self.cell is not None:
            self.cell.append(data)
        if self.binding is not None:
            self.binding[1] += data

    def handle_endtag(self, tag):
        if tag == self.hidden:
            self.hidden = None
        if tag == "span" and self.binding is not None:
            self.numbers.append(tuple(self.binding))
            self.binding = None
        if tag in {"td", "th"}:
            value = "".join(self.cell).strip()
            self.row.append(value)
            if tag == "th":
                self.headers.append(value)
            self.cell = None
        if tag == "tr":
            self.rows.append(self.row)
            self.row = None


@pytest.fixture
def builder():
    path = REPORT / "build_report.py"
    assert path.is_file(), "The bounded converter report builder is missing"
    spec = importlib.util.spec_from_file_location("fatigue_apps_report", path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def lookup(record, key):
    for part in key.split("."):
        record = record[int(part)] if isinstance(record, list) else record[part]
    return record


def test_worked_values_match_independent_calculation(builder, tmp_path):
    record = builder.calculate(tmp_path)
    # Every dimensional input and output is independently specified above.
    assert record["basis"] == {
        "area_mm2": 2500.0, "area_m2": 0.0025, "newtons_per_kn": 1000.0,
        "m2_per_mm2": 1e-6, "pascals_per_mpa": 1e6, "slope_mpa_per_kn": 0.4,
    }
    assert record["units"] == {
        "area_mm2": "mm^2", "area_m2": "m^2", "newtons_per_kn": "N/kN",
        "m2_per_mm2": "m^2/mm^2", "pascals_per_mpa": "Pa/MPa",
        "slope_mpa_per_kn": "MPa/kN", "tension_kn": "kN",
        "stress_mpa": "MPa", "expected_mpa": "MPa", "absolute_error_mpa": "MPa",
    }
    assert record["lookup_rows"] == [list(row) for row in TABLE]
    assert len(record["cases"]) == len(PROBES)
    for case, tension, regime in zip(record["cases"], PROBES, REGIMES):
        assert case["tension_kn"] == tension
        assert case["regime"] == regime
        assert case["expected_mpa"] == hand_stress(tension)
        assert case["stress_mpa"] == pytest.approx(hand_stress(tension), rel=0, abs=BUDGET)
        assert case["absolute_error_mpa"] == abs(case["stress_mpa"] - hand_stress(tension))
    assert record["tolerance"]["absolute_mpa"] == BUDGET
    assert record["tolerance"]["relative"] == 0
    assert record["comparison"]["max_error_mpa"] == max(
        c["absolute_error_mpa"] for c in record["cases"])


def test_visible_values_follow_computed_results(builder, tmp_path, monkeypatch):
    original = builder.calculate(tmp_path / "original")
    converter_call = TensionStressConverter.convert_tension_to_stress

    def changed_calculation(self, tension_range, location=None):
        # A synthetic +1 MPa fault exceeds the roundoff budget and display step.
        return converter_call(self, tension_range, location) + 1.0

    monkeypatch.setattr(TensionStressConverter, "convert_tension_to_stress", changed_calculation)
    changed = builder.calculate(tmp_path / "changed")
    old_page = Page(builder.build(original))
    new_page = Page(builder.build(changed))
    assert old_page.numbers != new_page.numbers
    for index, case in enumerate(changed["cases"]):
        key = f"cases.{index}.stress_mpa"
        shown = [(attrs, text) for attrs, text in new_page.numbers if attrs["data-value"] == key]
        assert shown and all(float(text) == case["stress_mpa"] for _, text in shown)
        assert case["stress_mpa"] == original["cases"][index]["stress_mpa"] + 1.0
    assert "exceeds the roundoff criterion" in " ".join(new_page.visible)


def test_every_visible_quantity_matches_oracle_and_fresh_module(builder, tmp_path):
    lookup_path = tmp_path / "independent.csv"
    with lookup_path.open("w", newline="", encoding="utf-8") as stream:
        writer = csv.writer(stream)
        writer.writerow(("tension_kn", "stress_mpa"))
        writer.writerows(TABLE)
    fresh = TensionStressConverter(lookup_table_path=str(lookup_path),
                                  output_dir=str(tmp_path / "fresh-output"))
    stresses = fresh.convert_tension_to_stress(np.asarray(PROBES))
    record = builder.calculate(tmp_path / "report")
    for index, stress in enumerate(stresses):
        assert stress == pytest.approx(hand_stress(PROBES[index]), rel=0, abs=BUDGET)
        assert record["cases"][index]["stress_mpa"] == stress
    page = Page((REPORT / "report.html").read_text(encoding="utf-8"))
    expected_keys = {f"basis.{key}" for key in record["basis"]}
    expected_keys |= {f"cases.{i}.{key}" for i in range(len(PROBES))
                      for key in ("tension_kn", "stress_mpa", "expected_mpa", "absolute_error_mpa")}
    expected_keys |= {"tolerance.absolute_mpa", "comparison.max_error_mpa"}
    assert {attrs["data-value"] for attrs, _ in page.numbers} == expected_keys
    for attrs, text in page.numbers:
        value = lookup(record, attrs["data-value"])
        fmt = ".1e" if attrs["data-format"] == "scientific" else ".1f"
        assert text == format(value, fmt)
        assert attrs["data-equation"] and attrs["data-source"].startswith("#source-")
        key = attrs["data-value"].split(".")[-1]
        unit = record["units"].get(key, "MPa")
        assert attrs["data-unit"] == unit
        assert any(f"{text} {unit}" in cell for row in page.rows for cell in row)
    assert json.loads(page.scripts["calculation-record"]) == record
    worked = re.search(r'<section id="section-5">(.*?)</section>',
                       (REPORT / "report.html").read_text(encoding="utf-8"), re.S)[1]
    unbound = re.sub(r"<h2>.*?</h2>|<span\b[^>]*>.*?</span>|<a\b[^>]*>.*?</a>", "", worked, flags=re.S)
    assert not re.search(r"\d", " ".join(Page(unbound).visible))


def test_report_rebuild_is_deterministic(builder, tmp_path):
    for name in ("first", "second"):
        output = tmp_path / name
        environment = dict(os.environ, PYTHONPATH=str(ROOT / "src"), PYTHONDONTWRITEBYTECODE="1")
        for key in ("GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR"):
            environment.pop(key, None)
        subprocess.run([sys.executable, str(REPORT / "build_report.py"),
                        "--output-dir", str(output), "--scratch-dir", str(output / "scratch")],
                       cwd=tmp_path, env=environment, check=True, capture_output=True, text=True)
    for artifact in ARTIFACTS:
        expected = (REPORT / artifact).read_bytes()
        assert (tmp_path / "first" / artifact).read_bytes() == expected
        assert (tmp_path / "second" / artifact).read_bytes() == expected
        assert str(tmp_path).encode() not in expected
        assert b"file://" not in expected


def test_scope_limits_and_report_pointers(builder, tmp_path):
    text = builder.build(builder.calculate(tmp_path))
    page = Page(text)
    assert re.findall(r'<section id="section-(\d+)"', text) == [str(n) for n in range(1, 11)]
    assert text.index("</section>") < text.index('id="revision-history"') < text.index('id="section-2"')
    assert len(page.ids) == len(set(page.ids))
    assert 'id="engineering-changes"' in text and 'class="hs-doc"' in text
    assert not page.resources
    for method in ("Real stress-transfer tables", "Stress-concentration factors",
                   "Fatigue damage", "Counting qualification", "Service life"):
        assert any(method in row and "NOT_EVALUATED" in row for row in page.rows)
    assert "Reproducibility does not establish physical validation" in " ".join(page.visible)
    assert "refuse_damage_calculation" in " ".join(page.visible)
    for link in page.links:
        if link.startswith("#"):
            assert link[1:] in page.ids
        elif not link.startswith("https:"):
            assert not Path(link).is_absolute()
            assert (REPORT / link.split("#")[0]).is_file(), link
    sources = json.loads((REPORT / "sources.json").read_text(encoding="utf-8"))["sources"]
    for source in sources:
        if "path" in source:
            path = ROOT / source["path"]
            assert path.is_file() and not Path(source["path"]).is_absolute()
            assert hashlib.sha256(path.read_bytes()).hexdigest() == source["sha256"]
    for path, target in ((REPORT / "README.md", "report.html"),
                         (ROOT / "src/digitalmodel/structural/fatigue_apps/README.md",
                          "../../../../docs/reports/modules/fatigue_apps/README.md")):
        assert f"]({target})" in path.read_text(encoding="utf-8")


def test_explicit_lookup_and_output_stay_in_scratch(builder, tmp_path, monkeypatch):
    constructor = TensionStressConverter.__init__
    observed = []

    def inspect_paths(self, *args, **kwargs):
        assert not args
        lookup_path = Path(kwargs["lookup_table_path"])
        assert lookup_path.is_relative_to(tmp_path)
        assert Path(kwargs["output_dir"]).is_relative_to(tmp_path)
        with lookup_path.open(encoding="utf-8", newline="") as stream:
            rows = list(csv.DictReader(stream))
        assert [(float(row["tension_kn"]), float(row["stress_mpa"])) for row in rows] == list(TABLE)
        observed.append(kwargs)
        constructor(self, **kwargs)

    def no_default(self):
        pytest.fail("The report attempted default lookup creation")

    monkeypatch.setattr(TensionStressConverter, "__init__", inspect_paths)
    monkeypatch.setattr(TensionStressConverter, "_create_default_lookup_table", no_default)
    builder.calculate(tmp_path)
    assert len(observed) == 1


def test_html_and_embedded_json_are_escaped(builder, tmp_path, monkeypatch):
    payload = '</script><img src=x onerror="alert(1)"> & <unsafe>'
    record = builder.calculate(tmp_path)
    record["cases"][0]["regime"] = payload
    monkeypatch.setitem(builder.CHANGES, "changes", [payload])
    text = builder.build(record)
    page = Page(text)
    assert payload not in text
    assert not page.resources
    assert payload in " ".join(page.visible)
    assert json.loads(page.scripts["calculation-record"])["cases"][0]["regime"] == payload
