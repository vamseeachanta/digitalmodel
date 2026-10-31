# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent Case A oracle and visible, deterministic report contracts.

Basis: test_catenary_geometry.py, Case A; issue 3319 bounded report scope.
Theory: Classical Mechanics, online revision 8 August 2022, section 18.3,
equations 18.3.2-18.3.9 (Physics LibreTexts page 7046), with z=y-a.
All inputs are synthetic arithmetic choices, never material recommendations.
Integrating dz/dx=sinh(x/a), ds/dx=cosh(x/a) from a horizontal tangent gives
z=a(cosh(x/a)-1), s=a*sinh(x/a). At z=a=100 m, cosh(x/a)=2, hence
s=100*sqrt(3), x=100*ln(2+sqrt(3)), T=H+w*z, theta=60 degrees.
"""

from __future__ import annotations

import hashlib
import importlib.util
import json
import math
import os
import re
import subprocess
import sys
from dataclasses import replace
from html.parser import HTMLParser
from pathlib import Path

import pytest
from click.testing import CliRunner

from digitalmodel.subsea.mooring_analysis.catenary import CatenaryAnalyzer
from digitalmodel.subsea.mooring_analysis.cli import cli
from digitalmodel.subsea.mooring_analysis.models import LineType, MooringLineProperties

ROOT = Path(__file__).resolve().parents[3]
REPORT = ROOT / "docs/reports/modules/mooring_analysis"
OUTPUTS = ("report.html", "changes.json", "sources.json")
# Case A is well conditioned (z/a=1, cosh(x/a)=2). Allow 256 binary64
# spacings at unity: <32 elementary steps, a factor-two library-rounding
# margin, and a factor-four conditioning allowance. The largest subtraction
# condition is (500+100*sqrt(3))/(500-100*sqrt(3)) < 4.
# This arithmetic budget is separate from the carried-forward 0.1% criterion.
ROUND_OFF = 256 * math.ulp(1.0)
NUMERICAL_REL = 0.001  # test_catenary_geometry.py, issue 2561: 0.1%.
INPUTS = {
    "mass_equivalent": (1000.0, "kg/m"),
    "gravity": (9.81, "m/s²"),
    "total_length": (500.0, "m"),
    "vertical_height": (100.0, "m"),
    "horizontal_tension": (981.0, "kN"),
    # Unused positive dataclass placeholders, as in the existing Case A fixture.
    "diameter": (1.0, "mm"),
    "mbl": (1.0, "kN"),
    "ea": (1.0, "kN"),
    "drag_coeff": (1.0, "dimensionless"),
}
ORACLE = {
    "weight_per_length": (9.81, "kN/m"),  # 1000*9.81/1000; SI N -> kN.
    "horizontal_tension": (981.0, "kN"),
    "vertical_height": (100.0, "m"),
    "catenary_parameter": (100.0, "m"),  # a=981/9.81.
    "arc_length": (100 * math.sqrt(3), "m"),
    "horizontal_distance": (100 * math.log(2 + math.sqrt(3)), "m"),
    "fairlead_tension": (1962.0, "kN"),  # H+w*z=981+981.
    "touchdown_tension": (981.0, "kN"),  # Vertical component is zero.
    "fairlead_angle": (60.0, "degrees"),  # atan(sqrt(3))=pi/3.
    "grounded_length": (500 - 100 * math.sqrt(3), "m"),
}


def load_builder():
    """Fail explicitly on missing implementation before attempting imports."""
    path = REPORT / "build_report.py"
    assert path.is_file(), "The bounded module report builder is not implemented"
    spec = importlib.util.spec_from_file_location("mooring_module_report", path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def fresh_values():
    """Compare report results with a fresh call; never define the oracle here."""
    line = MooringLineProperties(
        LineType.CHAIN, 500.0, 1.0, 1.0, 1000.0, 1.0, drag_coeff=1.0,
    )
    analyzer = CatenaryAnalyzer(water_depth=100.0)
    result = analyzer.solve_catenary(line, 981.0, vertical_height=100.0)
    values = {key: getattr(result, key) for key in ORACLE if key != "weight_per_length"}
    values["weight_per_length"] = line.weight_water * analyzer.g / 1000
    return values


class Page(HTMLParser):
    """Read visible numeric spans, structure, links and embedded records."""

    def __init__(self, text):
        super().__init__()
        self.ids, self.links, self.resources, self.visible = [], [], [], []
        self.numbers, self.records = {}, {}
        self.binding = self.hidden = self.record_id = None
        self.feed(text)

    def handle_starttag(self, tag, attrs):
        attrs = dict(attrs)
        if "id" in attrs:
            self.ids.append(attrs["id"])
        if tag == "a":
            self.links.append(attrs["href"])
        if tag in {"script", "style"}:
            self.hidden = tag
            self.record_id = attrs.get("id")
        if tag in {"script", "img", "link", "iframe"}:
            self.resources.extend(attrs[k] for k in ("src", "href") if k in attrs)
        if "data-value" in attrs:
            self.binding = attrs

    def handle_endtag(self, tag):
        if tag == "span":
            self.binding = None
        if tag == self.hidden:
            self.hidden = self.record_id = None

    def handle_data(self, data):
        if not self.hidden:
            self.visible.append(data)
            if self.binding:
                key = self.binding["data-value"]
                assert key not in self.numbers, f"Duplicate numeric binding: {key}"
                self.numbers[key] = (self.binding.copy(), data)
        elif self.record_id:
            self.records[self.record_id] = data


def test_worked_values_match_independent_calculation():
    """Check all unrounded values, units and bindings against hand arithmetic."""
    record = load_builder().calculate()
    assert set(record["inputs"]) == set(INPUTS)
    assert set(record["computed"]) == set(ORACLE)
    fresh = fresh_values()
    for key, (expected, unit) in INPUTS.items():
        item = record["inputs"][key]
        assert item["value"] == expected
        assert item["unit"] == unit
        assert item["basis"] == "synthetic"
        assert item["source"] == "case-a"
    for key, (expected, unit) in ORACLE.items():
        for group in ("computed", "independent"):
            item = record[group][key]
            assert item["value"] == pytest.approx(expected, rel=ROUND_OFF, abs=0)
            assert item["unit"] == unit
            assert item["equation"]
            assert item["source"] == "case-a"
        assert record["computed"][key]["value"] == pytest.approx(fresh[key], rel=ROUND_OFF)
        assert record["computed"][key]["value"] == pytest.approx(expected, rel=NUMERICAL_REL)
    assert record["criterion_percent"] == NUMERICAL_REL * 100


def assert_visible(page, record, fresh):
    """Check every displayed input/result/oracle/difference and visible unit."""
    expected_bindings = set()
    for group, oracle in (("inputs", INPUTS), ("computed", ORACLE), ("independent", ORACLE)):
        for key, (expected, unit) in oracle.items():
            binding = f"{group}.{key}"
            expected_bindings.add(binding)
            attrs, visible = page.numbers[binding]
            digits = 2 if key == "gravity" else 1  # Preserve declared input g.
            assert attrs["data-unit"] == unit
            assert attrs["data-source"] == "case-a"
            assert visible == f"{record[group][key]['value']:.{digits}f} {unit}"
            assert visible == f"{expected:.{digits}f} {unit}"
            if group == "computed":
                assert visible == f"{fresh[key]:.{digits}f} {unit}"
    for key, (expected, _) in ORACLE.items():
        binding = f"difference.{key}"
        expected_bindings.add(binding)
        attrs, visible = page.numbers[binding]
        difference = abs(fresh[key] - expected) / abs(expected) * 100
        assert attrs["data-unit"] == "%"
        assert visible == f"{difference:.1f} %"
    assert set(page.numbers) == expected_bindings


@pytest.mark.parametrize("field", [key for key in ORACLE if key != "weight_per_length"])
def test_visible_values_follow_computed_results(monkeypatch, field):
    """Every result field must change the visible page under controlled mutation."""
    builder = load_builder()
    record = builder.calculate()
    page = Page(builder.build(record))
    assert_visible(page, record, fresh_values())
    delivered = Page((REPORT / "report.html").read_text(encoding="utf-8"))
    assert_visible(delivered, record, fresh_values())
    original = CatenaryAnalyzer.solve_catenary

    def mutated(self, *args, **kwargs):
        result = original(self, *args, **kwargs)
        # A synthetic 10% perturbation exceeds the 0.1% comparison criterion.
        return replace(result, **{field: getattr(result, field) * 1.1})

    monkeypatch.setattr(CatenaryAnalyzer, "solve_catenary", mutated)
    changed = builder.calculate()
    changed_page = Page(builder.build(changed))
    key = f"computed.{field}"
    assert changed_page.numbers[key][1] != page.numbers[key][1]
    assert changed_page.numbers[key][1] == f"{changed['computed'][field]['value']:.1f} {ORACLE[field][1]}"
    assert changed_page.numbers[f"independent.{field}"] == page.numbers[f"independent.{field}"]
    assert "Numerical agreement criterion exceeded" in " ".join(changed_page.visible)


def test_report_rebuild_is_deterministic(tmp_path):
    """Two isolated explicit destinations must reproduce all delivered bytes."""
    load_builder()
    environment = {k: v for k, v in os.environ.items() if not k.startswith("GIT_")}
    environment.update(PYTHONPATH=str(ROOT / "src"), PYTHONDONTWRITEBYTECODE="1")
    for name in ("first", "different-directory"):
        output = tmp_path / name
        subprocess.run(
            [sys.executable, str(REPORT / "build_report.py"), "--output-dir", str(output)],
            cwd=tmp_path, env=environment, check=True, capture_output=True,
        )
        assert set(p.name for p in output.iterdir()) == set(OUTPUTS)
        for filename in OUTPUTS:
            assert (output / filename).read_bytes() == (REPORT / filename).read_bytes(), (
                f"{filename} differs; rebuild with: {load_builder().REBUILD}"
            )


@pytest.mark.parametrize("version", ["rebuilt", "committed"])
def test_documented_cli_routes_are_registered(version):
    """Every documented command must resolve without running an analysis."""
    builder = load_builder()
    text = (builder.build(builder.calculate()) if version == "rebuilt"
            else (REPORT / "report.html").read_text(encoding="utf-8"))
    routes = {route for fragment in Page(text).visible
              for route in re.findall(r"\bmooring-analysis ([a-z][a-z-]*)\b", fragment)}
    assert routes, "The report must document the command routes"
    runner = CliRunner()
    for route in sorted(routes):
        assert route in cli.commands, f"Documented command is not registered: {route}"
        result = runner.invoke(cli, [route, "--help"])
        assert result.exit_code == 0, result.output
        assert f"{route} [OPTIONS]" in result.output
    assert routes == set(cli.commands)


def test_scope_limits_and_report_pointers():
    """Check report structure, evidence resolution, limitations and local pointers."""
    builder = load_builder()
    page = Page((REPORT / "report.html").read_text(encoding="utf-8"))
    sections = [f"section-{i}" for i in range(1, 11)]  # Ten-section GIS pattern.
    assert [key for key in page.ids if key.startswith("section-")] == sections
    assert page.ids.index("section-1") < page.ids.index("revision-history") < page.ids.index("section-2")
    assert "change-panel" in page.ids
    visible = " ".join(page.visible)
    for limit in ("Design-code compliance", "Actual line strength", "Stiffness qualification",
                  "Environmental loading", "Dynamic mooring behavior"):
        assert f"{limit}: NOT_EVALUATED" in visible
    assert "horizontal touchdown tangent" in visible
    assert "mass-equivalent" in visible
    assert "not recommended line properties" in visible
    assert "Reproducibility does not establish physical validation" in visible
    assert "CatenarySolver" in visible and "DYNMOOR" in visible
    assert "0.1%" in visible and "numerical" in visible
    assert not page.resources
    for link in page.links:
        assert not link.startswith(("/", "file:", "http:", "https:"))
        if link.startswith("#"):
            assert link[1:] in page.ids
        else:
            assert (REPORT / link).resolve().is_file()
    sources = json.loads((REPORT / "sources.json").read_text(encoding="utf-8"))
    for source in sources["sources"]:
        path = ROOT / source["path"]
        assert path.is_file()
        assert hashlib.sha256(path.read_bytes()).hexdigest() == source["sha256"], (
            f"Source digest changed; rebuild with: {builder.REBUILD}"
        )
    record = builder.calculate()
    assert json.loads(page.records["calculation-record"]) == record
    assert json.loads(page.records["change-record"]) == json.loads((REPORT / "changes.json").read_text(encoding="utf-8"))
    assert json.loads(page.records["source-record"]) == sources
    bound = record["agreement_bound_percent"]
    assert bound >= max(record["difference"].values())
    assert bound - max(record["difference"].values()) < 0.1
    index = (REPORT / "README.md").read_text(encoding="utf-8")
    assert "[report](report.html)" in index
    pointer = ROOT / "src/digitalmodel/subsea/mooring_analysis/README.md"
    pointer_text = pointer.read_text(encoding="utf-8")
    assert "../../../../docs/reports/modules/mooring_analysis/README.md" in pointer_text
    # Keep the original package metadata and standalone examples ahead of the
    # new pointer; the existing README has version metadata at both ends.
    assert pointer_text.index("## Bounded module report") > pointer_text.rindex("**Version**:")
    for filename in OUTPUTS:
        text = (REPORT / filename).read_text(encoding="utf-8")
        assert str(ROOT) not in text and str(Path.home()) not in text


def test_report_escapes_html_and_embedded_json():
    """Text supplied through records cannot terminate a JSON script or add HTML."""
    builder = load_builder()
    record = builder.calculate()
    payload = '</script><img src="injected" onerror="injected">&'
    record["inputs"]["mass_equivalent"]["basis"] = payload
    page_text = builder.build(record)
    assert payload not in page_text
    page = Page(page_text)
    assert not page.resources
    assert payload in " ".join(page.visible)
    assert json.loads(page.records["calculation-record"])["inputs"]["mass_equivalent"]["basis"] == payload
