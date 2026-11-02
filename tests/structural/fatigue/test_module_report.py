# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent arithmetic and publication checks for the bounded fatigue report."""
from __future__ import annotations

import copy
import hashlib
import importlib.util
import json
import os
import re
import subprocess
import sys
from fractions import Fraction
from html.parser import HTMLParser
from pathlib import Path

import numpy as np
import pandas as pd
import pytest

from digitalmodel.structural.fatigue.damage_accumulation import LinearDamageAccumulation
from digitalmodel.structural.fatigue.sn_curves import PowerLawSNCurve

ROOT = Path(__file__).resolve().parents[3]
REPORT = ROOT / "docs/reports/modules/fatigue"
OUTPUTS = ("report.html", "changes.json", "sources.json")

# Independent hand calculation, not a material or standard curve:
# N*S^2 = 1,000,000 cycle*MPa^2; S=(10,20) MPa, n=(1000,500) cycles.
# N=(1000000/100,1000000/400)=(10000,2500) cycles;
# d=(1000/10000,500/2500)=(1/10,1/5), D=3/10 dimensionless;
# processed cycles=1000+500=1500. Mean=0 MPa; no material/mean correction.
# A synthetic cutoff of 20000 cycles exceeds both N values; limit=0 MPa.
# All input integers are exactly representable. Fewer than eight rounding
# stages cover reciprocal-square evaluation, coefficient multiplication,
# damage division and summation, including oracle conversion. Allow four
# epsilon per stage for the power evaluation: rtol=32*epsilon, atol=0.
# This is a numerical regression budget, not a physical uncertainty bound.
RTOL = 32 * sys.float_info.epsilon
ORACLE = {
    "basis.coefficient": (Fraction(1_000_000), "cycle*MPa^2"),
    "basis.exponent": (Fraction(2), "dimensionless"),
    "basis.fatigue_limit": (Fraction(0), "MPa"),
    "basis.cutoff": (Fraction(20_000), "cycles"),
    "levels.0.range": (Fraction(10), "MPa"),
    "levels.0.mean": (Fraction(0), "MPa"),
    "levels.0.count": (Fraction(1000), "cycles"),
    "levels.0.allowable": (Fraction(10_000), "cycles"),
    "levels.0.damage": (Fraction(1, 10), "dimensionless"),
    "levels.1.range": (Fraction(20), "MPa"),
    "levels.1.mean": (Fraction(0), "MPa"),
    "levels.1.count": (Fraction(500), "cycles"),
    "levels.1.allowable": (Fraction(2500), "cycles"),
    "levels.1.damage": (Fraction(1, 5), "dimensionless"),
    "total_damage": (Fraction(3, 10), "dimensionless"),
    "cycles_processed": (Fraction(1500), "cycles"),
}


def lookup(record, path):
    for part in path.split("."):
        record = record[int(part)] if isinstance(record, list) else record[part]
    return record


class Page(HTMLParser):
    """Read actual visible quantities as well as their units and source bindings."""

    def __init__(self, text):
        super().__init__()
        self.ids, self.links, self.resources, self.visible = [], [], [], []
        self.numbers, self.references, self.differences, self.scripts = {}, {}, {}, {}
        self.hidden = self.binding = self.script_id = None
        self.feed(text)

    def handle_starttag(self, tag, attrs):
        attrs = dict(attrs)
        if "id" in attrs:
            self.ids.append(attrs["id"])
        if tag in {"script", "style"}:
            self.hidden = tag
            self.script_id = attrs.get("id") if tag == "script" else None
        if tag == "a":
            self.links.append(attrs["href"])
        if tag in {"link", "script", "img", "iframe"}:
            self.resources.extend(attrs[k] for k in ("href", "src") if k in attrs)
        if "data-value" in attrs:
            self.binding = (self.numbers, attrs["data-value"])
            self.numbers.setdefault(attrs["data-value"], []).append([attrs, ""])
        if "data-oracle" in attrs:
            self.binding = (self.references, attrs["data-oracle"])
            self.references.setdefault(attrs["data-oracle"], []).append([attrs, ""])
        if "data-difference" in attrs:
            self.binding = (self.differences, attrs["data-difference"])
            self.differences.setdefault(attrs["data-difference"], []).append([attrs, ""])

    def handle_endtag(self, tag):
        if tag == self.hidden:
            self.hidden = self.script_id = None
        if tag == "span":
            self.binding = None

    def handle_data(self, text):
        if self.script_id:
            self.scripts[self.script_id] = self.scripts.get(self.script_id, "") + text
        if not self.hidden:
            self.visible.append(text)
        if self.binding:
            store, key = self.binding
            store[key][-1][1] += text


def load_builder():
    path = REPORT / "build_report.py"
    assert path.is_file(), "The fatigue report builder has not been implemented"
    spec = importlib.util.spec_from_file_location("fatigue_module_report", path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def fresh_module_values():
    # Same explicitly specified synthetic inputs; no builder inputs or counting.
    curve = PowerLawSNCurve("Synthetic arithmetic", A=1_000_000, m=2,
                            fatigue_limit=0, cutoff_cycles=20_000)
    cycles = pd.DataFrame({"range": [10, 20], "mean": [0, 0], "count": [1000, 500]})
    raw = LinearDamageAccumulation(mean_stress_correction=None).calculate_damage(cycles, curve)
    values = {key: float(expected) for key, (expected, _) in ORACLE.items()}
    for index, row in enumerate(raw["damage_contributions"]):
        values[f"levels.{index}.allowable"] = row["cycles_allowable"]
        values[f"levels.{index}.damage"] = row["damage_increment"]
    values.update(total_damage=raw["total_damage"], cycles_processed=raw["cycles_processed"])
    return curve, raw, values


def test_worked_values_match_independent_calculation():
    builder = load_builder()
    record = builder.calculate()
    assert set(record["units"]) == set(ORACLE)
    for key, (expected, unit) in ORACLE.items():
        assert lookup(record, key) == pytest.approx(float(expected), rel=RTOL, abs=0), key
        assert record["units"][key] == unit
    curve, raw, _ = fresh_module_values()
    np.testing.assert_allclose(curve.get_allowable_cycles([10, 20]), [10_000, 2500],
                               rtol=RTOL, atol=0)
    assert [r["damage_increment"] for r in raw["damage_contributions"]] == pytest.approx(
        [float(Fraction(1, 10)), float(Fraction(1, 5))], rel=RTOL, abs=0)
    assert raw["total_damage"] == pytest.approx(float(Fraction(3, 10)), rel=RTOL, abs=0)
    assert raw["cycles_processed"] == 1500
    # The base constructor attaches generic properties for material=None.
    # Those properties do not assign a physical identity to this artificial curve.
    assert curve.m2 is None and curve.fatigue_limit == 0


def test_displayed_values_and_units_match_oracle_and_fresh_call():
    builder = load_builder()
    page = Page((REPORT / "report.html").read_text(encoding="utf-8"))
    _, _, fresh = fresh_module_values()
    assert set(page.numbers) == set(ORACLE)
    for key, (expected, unit) in ORACLE.items():
        for attrs, visible in page.numbers[key]:
            assert attrs["data-unit"] == unit
            assert visible == f"{float(expected):.1f} {unit}", key
            assert visible == f"{fresh[key]:.1f} {unit}", key
            assert attrs["data-source"] in page.ids
    result_keys = {"levels.0.allowable", "levels.1.allowable", "levels.0.damage",
                   "levels.1.damage", "total_damage", "cycles_processed"}
    assert set(page.references) == set(page.differences) == result_keys
    for key in result_keys:
        expected, unit = ORACLE[key]
        assert [v for _, v in page.references[key]] == [f"{float(expected):.1f} {unit}"]
        difference = abs(fresh[key] - float(expected)) / abs(float(expected)) * 100
        assert [v for _, v in page.differences[key]] == [f"{difference:.1f} %"]
    assert not page.resources
    assert "url(" not in (REPORT / "report.html").read_text(encoding="utf-8")


@pytest.mark.parametrize("key,raw_key,index", [
    ("total_damage", "total_damage", None),
    ("cycles_processed", "cycles_processed", None),
    ("levels.0.allowable", "cycles_allowable", 0),
    ("levels.1.allowable", "cycles_allowable", 1),
    ("levels.0.damage", "damage_increment", 0),
    ("levels.1.damage", "damage_increment", 1),
])
def test_visible_values_follow_computed_results(monkeypatch, key, raw_key, index):
    builder = load_builder()
    before = Page(builder.build(builder.calculate()))
    original = builder.LinearDamageAccumulation.calculate_damage

    def changed_calculation(self, *args, **kwargs):
        raw = copy.deepcopy(original(self, *args, **kwargs))
        target = raw if index is None else raw["damage_contributions"][index]
        # A synthetic +1 perturbation is large enough to survive one-decimal display.
        target[raw_key] += 1
        return raw

    monkeypatch.setattr(builder.LinearDamageAccumulation, "calculate_damage", changed_calculation)
    after = Page(builder.build(builder.calculate()))
    expected, unit = ORACLE[key]
    assert [v for _, v in after.numbers[key]] == [
        f"{float(expected + 1):.1f} {unit}" for _ in before.numbers[key]]
    assert before.numbers[key] != after.numbers[key]


def test_report_rebuild_is_deterministic(tmp_path):
    builder = load_builder()
    environment = dict(os.environ, PYTHONPATH=str(ROOT / "src"), PYTHONDONTWRITEBYTECODE="1")
    # No inherited Git binding may redirect a child launched in the repository.
    for key in ("GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR"):
        environment.pop(key, None)
    outputs = [tmp_path / "first", tmp_path / "different" / "second"]
    for output in outputs:
        subprocess.run([sys.executable, str(REPORT / "build_report.py"),
                        "--output-dir", str(output)], cwd=tmp_path,
                       env=environment, check=True, capture_output=True, text=True)
        assert sorted(p.name for p in output.iterdir()) == sorted(OUTPUTS)
        for name in OUTPUTS:
            assert (output / name).read_bytes() == (REPORT / name).read_bytes(), name
            text = (output / name).read_text(encoding="utf-8")
            assert str(ROOT) not in text and str(tmp_path) not in text
            assert not re.search(r"/(?:home|mnt)/|file://|\d{2}:\d{2}:\d{2}", text)
    assert outputs[0].resolve() != outputs[1].resolve()


def test_scope_limits_and_report_pointers():
    builder = load_builder()
    text = (REPORT / "report.html").read_text(encoding="utf-8")
    page = Page(text)
    assert re.findall(r'<section id="section-(\d+)"', text) == [str(n) for n in range(1, 11)]
    assert len(page.ids) == len(set(page.ids))
    assert re.search(r'</section>\s*<div id="revision-history"', text)
    assert text.index('id="revision-history"') < text.index('id="section-2"')
    assert 'id="engineering-changes"' in text
    visible = " ".join(page.visible)
    for phrase in ("standard S-N coefficients", "mean-stress corrections",
                   "rainflow-to-damage qualification", "frequency-domain methods",
                   "physical remaining life"):
        assert re.search(re.escape(phrase) + r".*?NOT_EVALUATED", visible, re.S)
    for phrase in ("no material identity", "refuses", "diagnostic", "retained residual",
                   "not physical validation", "Way forward", "32", "epsilon"):
        assert phrase in visible
    for link in page.links:
        assert link.startswith("#"), "The standalone report uses internal source locators"
        assert link[1:] in page.ids
    sources = json.loads((REPORT / "sources.json").read_text(encoding="utf-8"))
    assert sources["basis"]["classification"] == "synthetic"
    for source in sources["files"]:
        path = ROOT / source["path"]
        assert not Path(source["path"]).is_absolute() and path.is_file()
        assert hashlib.sha256(path.read_bytes()).hexdigest() == source["sha256"]
        assert source["id"] in page.ids
        for symbol in source["symbols"]:
            assert symbol in path.read_text(encoding="utf-8")
    changes = json.loads((REPORT / "changes.json").read_text(encoding="utf-8"))
    assert changes["revision"] == "00" and changes["changes"]
    assert json.loads(page.scripts["change-record"]) == changes
    assert json.loads(page.scripts["source-record"]) == sources
    assert json.loads(page.scripts["calculation-record"]) == builder.rounded(builder.calculate())
    assert "[Module report](report.html)" in (REPORT / "README.md").read_text(encoding="utf-8")
    package_readme = ROOT / "src/digitalmodel/structural/fatigue/README.md"
    assert "../../../../docs/reports/modules/fatigue/README.md" in package_readme.read_text(encoding="utf-8")
    assert '"$TMPDIR/fatigue-report"' in (REPORT / "README.md").read_text(encoding="utf-8")


def test_html_and_embedded_json_are_escaped(monkeypatch):
    builder = load_builder()
    hostile = '</script><img src="unexpected" onerror="alert(1)">&'
    changes = copy.deepcopy(builder.CHANGES)
    changes["changes"].append(hostile)
    monkeypatch.setattr(builder, "CHANGES", changes)
    record = builder.calculate()
    record["note"] = hostile
    text = builder.build(record)
    page = Page(text)
    assert hostile not in text and not page.resources
    assert hostile in " ".join(page.visible)
    assert json.loads(page.scripts["change-record"])["changes"][-1] == hostile
    assert json.loads(page.scripts["calculation-record"])["note"] == hostile
