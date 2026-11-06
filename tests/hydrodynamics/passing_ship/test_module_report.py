# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Read the bounded report against an independent synthetic polynomial oracle."""

import ast
import copy
import importlib.util
import json
import os
from fractions import Fraction
from html.parser import HTMLParser
from pathlib import Path
import re
import subprocess
import sys

import pytest


ROOT = Path(__file__).resolve().parents[3]
REPORT = ROOT / "docs/reports/modules/passing_ship"
PACKAGE = ROOT / "src/digitalmodel/hydrodynamics/passing_ship"
ARTIFACTS = {"report.html", "changes.json", "sources.json"}


def load(path, name):
    """Load an offline file without importing the package initializer."""
    spec = importlib.util.spec_from_file_location(name, path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


@pytest.fixture
def builder():
    return load(REPORT / "build_report.py", "passing_section_report")


class Page(HTMLParser):
    """Extract actual visible bindings separately from embedded JSON."""

    def __init__(self, content):
        super().__init__()
        self.ids = []
        self.sections = []
        self.links = []
        self.resources = []
        self.visible = []
        self.values = {}
        self.tables = []
        self.row = None
        self.cell = None
        self.binding = None
        self.hidden = None
        self.feed(content)

    def handle_starttag(self, tag, attrs):
        attrs = dict(attrs)
        if tag == "table":
            self.tables.append([])
        elif tag == "tr":
            self.row = []
        elif tag in {"td", "th"}:
            self.cell = []
        if "id" in attrs:
            self.ids.append(attrs["id"])
        if tag == "section":
            self.sections.append(attrs.get("id"))
        if tag in {"style", "script"}:
            self.hidden = tag
        if tag == "a":
            self.links.append(attrs["href"])
        if tag in {"link", "script", "img", "iframe"}:
            self.resources.extend(attrs[k] for k in ("src", "href") if k in attrs)
        if "data-value" in attrs:
            assert self.hidden is None
            self.binding = attrs

    def handle_endtag(self, tag):
        if tag in {"td", "th"}:
            self.row.append("".join(self.cell).strip())
            self.cell = None
        elif tag == "tr":
            self.tables[-1].append(self.row)
        if tag == self.hidden:
            self.hidden = None
        if tag == "span":
            self.binding = None

    def handle_data(self, data):
        if self.cell is not None:
            self.cell.append(data)
        if not self.hidden:
            self.visible.append(data)
        if self.binding is not None:
            key = self.binding["data-value"]
            assert key not in self.values, f"Duplicate visible quantity: {key}"
            self.values[key] = (data, self.binding)


def oracle():
    """Return rational values from H1, without any formulation or builder call."""
    stations = (-2, -1, 0, 1, 2)  # H1 synthetic metre coordinates.
    return {
        "length_m": (Fraction(4), "m"),
        **{
            f"rows.{i}.{key}": (value, unit)
            for i, x in enumerate(stations)
            for key, value, unit in (
                ("x_m", Fraction(x), "m"),
                ("shape", 1 - Fraction(x * x, 4), "dimensionless"),
                ("slope_per_m", -Fraction(x, 2), "m^-1"),
                ("expected_shape", 1 - Fraction(x * x, 4), "dimensionless"),
                ("expected_slope_per_m", -Fraction(x, 2), "m^-1"),
                ("shape_error", Fraction(0), "dimensionless"),
                ("slope_error_per_m", Fraction(0), "m^-1"),
            )
        },
    }


def lookup(record, key):
    for part in key.split("."):
        record = record[int(part)] if isinstance(record, list) else record[part]
    return record


def test_worked_values_match_independent_calculation(builder):
    """H1 independently solves S=a*x^2+b*x+c, S(-2)=S(2)=0, S(0)=1.

    The synthetic coordinates are metres; S is dimensionless. The midpoint
    gives c=1. Subtracting endpoint equations gives 4*b=0, hence b=0 m^-1;
    substitution gives a=-1/4 m^-2. Thus S=1-x^2/4 and S'=-x/2 per metre.
    At x=(-2,-1,0,1,2) m: S=(0,3/4,1,3/4,0); S'=(1,1/2,0,-1/2,-1).
    Endpoint slopes are one-sided interior limits; the zero extension has no
    two-sided derivative at either join. Interior slopes are (1/2,0,-1/2).

    Roundoff budget: these integer/half/quarter values and L=4 are dyadic.
    The shipped multiplies, divisions by powers of two, squares and subtraction
    all have exactly representable intermediates at these stations. Therefore
    absolute tolerance is zero in each stated unit; no force tolerance applies.
    The rational oracle above uses only this derivation, never the tested code.
    """
    record = builder.calculate()
    assert record["units"] == {
        "length_m": "m", "x_m": "m", "shape": "dimensionless",
        "slope_per_m": "m^-1", "expected_shape": "dimensionless",
        "expected_slope_per_m": "m^-1", "shape_error": "dimensionless",
        "slope_error_per_m": "m^-1",
    }
    assert len(record["rows"]) == 5  # H1 defines five stations.
    for key, (expected, unit) in oracle().items():
        # H1 equations above; exact dyadic budget: abs=0, rel=0 in this unit.
        assert lookup(record, key) == pytest.approx(float(expected), abs=0, rel=0)
        assert record["units"][key.split(".")[-1]] == unit
    for i, row in enumerate(record["rows"]):
        assert row["meaning"] == (
            "one-sided from interior" if i in {0, 4} else "interior derivative"
        )


def assert_visible(page, record, expected_values):
    assert set(page.values) == set(expected_values)
    for key, (expected, unit) in expected_values.items():
        text, attrs = page.values[key]
        assert text == f"{float(expected):.1f}".replace("-0.0", "0.0")
        assert attrs["data-unit"] == unit
        assert attrs["data-equation"] == "H1"
        assert float(text) == float(f"{lookup(record, key):.1f}")


def test_visible_values_follow_computed_results(builder, monkeypatch):
    record = builder.calculate()
    page = Page(builder.render(record))
    assert_visible(page, record, oracle())
    # Read the visible unit labels too; data attributes alone cannot establish
    # that readers see the same units as the calculation record.
    assert page.tables[1][0] == ["Input", "Value", "Unit", "Basis"]
    assert page.tables[1][1] == ["Section length L", "4.0", "m", "H1 synthetic definition"]
    assert page.tables[2][0] == [
        "x [m]", "S computed [dimensionless]", "S H1 [dimensionless]",
        "Absolute difference [dimensionless]", "Slope computed [m^-1]",
        "Slope H1 [m^-1]", "Absolute difference [m^-1]", "Slope meaning",
    ]
    assert len(page.tables[2][1:]) == 5  # H1's five synthetic stations.
    fresh = load(PACKAGE / "formulations.py", "fresh_section_helpers")
    for row in record["rows"]:
        assert row["shape"] == fresh.s1_function(row["x_m"], record["length_m"])
        assert row["slope_per_m"] == fresh.ds1_dx(row["x_m"], record["length_m"])

    helpers = builder.load_formulations()
    original = helpers.s1_function
    # Synthetic perturbation 1/8 is exact and exceeds the 0.1 display step.
    monkeypatch.setattr(helpers, "s1_function", lambda x, length: original(x, length) + 1 / 8)
    monkeypatch.setattr(builder, "load_formulations", lambda: helpers)
    changed = builder.calculate()
    mutated = Page(builder.render(changed))
    assert changed["rows"][0]["shape"] != record["rows"][0]["shape"]
    assert mutated.values["rows.0.shape"][0] == "0.1"
    assert mutated.values["rows.0.shape_error"][0] == "0.1"
    with pytest.raises(AssertionError):
        assert_visible(mutated, changed, oracle())
    # Also prove render() consumes the supplied calculated slope.
    altered = copy.deepcopy(record)
    altered["rows"][1]["slope_per_m"] += 1 / 2  # H1 synthetic perturbation.
    assert Page(builder.render(altered)).values["rows.1.slope_per_m"][0] == "1.0"


def rebuild(output, cwd):
    subprocess.run(
        [sys.executable, str(REPORT / "build_report.py"), "--output-dir", str(output)],
        cwd=cwd, env=dict(os.environ, PYTHONDONTWRITEBYTECODE="1"),
        capture_output=True, text=True, check=True,
    )
    assert {path.name for path in output.iterdir()} == ARTIFACTS
    return {name: (output / name).read_bytes() for name in ARTIFACTS}


def test_report_rebuild_is_deterministic(tmp_path):
    first = rebuild(tmp_path / "first", tmp_path)
    second = rebuild(tmp_path / "other" / "second", tmp_path)
    assert first == second
    assert first == {name: (REPORT / name).read_bytes() for name in ARTIFACTS}
    assert {path.name for path in tmp_path.iterdir()} == {"first", "other"}
    for content in first.values():
        assert str(tmp_path).encode() not in content
        assert str(ROOT).encode() not in content


def test_scope_limits_and_report_pointers(builder):
    content = (REPORT / "report.html").read_text(encoding="utf-8")
    page = Page(content)
    assert page.sections == [f"section-{i}" for i in range(1, 11)]
    assert len(page.ids) == len(set(page.ids))
    assert re.search(r'</section>\s*<div id="revision-history">', content)
    assert content.index('id="section-1"') < content.index('id="revision-history"')
    assert content.index('id="revision-history"') < content.index('id="section-2"')
    assert not page.resources
    assert 'class="hs-doc"' in content
    assert 'id="engineering-changes"' in content
    assert 'id="H1"' in content
    for category in ("Physical forces", "Force coefficients", "Separation/depth validity",
                     "Full force-calculator qualification"):
        assert f"<td>{category}</td><td>NOT_EVALUATED</td>" in content
    visible = " ".join(page.visible)
    for statement in ("not differentiable at the join", "Reproducibility is not physical validation",
                      "parent issue remains open", "source inspection only"):
        assert statement in visible
    for link in page.links:
        if link.startswith("#"):
            assert link[1:] in page.ids
        else:
            assert not re.match(r"(?:[a-z]+:|/)", link)
            assert (REPORT / link).is_file(), link
    sources = json.loads((REPORT / "sources.json").read_text())["sources"]
    assert {s["id"] for s in sources} <= set(page.ids)
    for source in sources:
        path = ROOT / source["path"]
        assert path.is_file()
        assert f"../../../../{source['path']}" in page.links
        if path.suffix == ".py":
            names = {node.name for node in ast.walk(ast.parse(path.read_text()))
                     if isinstance(node, (ast.FunctionDef, ast.ClassDef))}
            assert set(source["symbols"]) <= names
    changes = json.loads((REPORT / "changes.json").read_text())
    assert changes == builder.CHANGES
    assert all(change in visible for change in changes["changes"])
    assert "[Module report](report.html)" in (REPORT / "README.md").read_text()
    assert "../../../../docs/reports/modules/passing_ship/report.html" in (PACKAGE / "README.md").read_text()
    for name in ARTIFACTS | {"build_report.py", ".gitignore"}:
        result = subprocess.run(["git", "check-ignore", "-q", "--", str((REPORT / name).relative_to(ROOT))], cwd=ROOT)
        assert result.returncode == 1, f"Required deliverable is ignored: {name}"


def test_html_and_json_escaping(builder):
    record = builder.calculate()
    hostile = '</script><img src="unexpected" onerror="alert(1)">&'
    record["rows"][0]["meaning"] = hostile
    content = builder.render(record)
    page = Page(content)
    assert not page.resources
    assert hostile in " ".join(page.visible)
    embedded = re.search(r'<script type="application/json" id="calculation-record">(.*?)</script>',
                         content, re.S)[1]
    assert "<" not in embedded and ">" not in embedded and "&" not in embedded
    assert json.loads(embedded) == record
