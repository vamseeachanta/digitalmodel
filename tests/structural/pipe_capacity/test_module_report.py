# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Check the module report, independently of the standalone example contract."""

import ast
import copy
import hashlib
import importlib.util
import json
import math
import os
from pathlib import Path
import subprocess
import sys
import xml.etree.ElementTree as ET

import pytest

from digitalmodel.structural.pipe_capacity.common.PipeSizing import PipeSizing


ROOT = Path(__file__).resolve().parents[3]
REPORT = ROOT / "docs/reports/modules/pipe_capacity"
ARTIFACTS = ("report.html", "sources.json", "changes.json")


@pytest.fixture
def builder():
    """Load the report builder only when a test requests it."""
    spec = importlib.util.spec_from_file_location("pipe_module_report", REPORT / "build_report.py")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def oracle():
    """Independent polar integrals, in metres and corresponding powers.

    Synthetic Do=4 m, Di=2 m give Ro=2 m, Ri=1 m. With dA=r dr dtheta,
    A=2*pi*[r^2/2]_1^2=3*pi m^2;
    Ix=Iy=pi*[r^4/4]_1^2=15*pi/4 m^4;
    J=2*pi*[r^4/4]_1^2=15*pi/2 m^4=Ix+Iy.
    Disk contributions use the same integrals with lower radius zero.
    These are geometric probes, with no material or code allowable.
    """
    return {
        "Nominal_OD": (4.0, "m"), "Nominal_ID": (2.0, "m"),
        "Ro": (2.0, "m"), "Ri": (1.0, "m"),
        "Ao": (4 * math.pi, "m^2"), "Ai": (math.pi, "m^2"),
        "Io": (4 * math.pi, "m^4"), "Ii": (math.pi / 4, "m^4"),
        "Jo": (8 * math.pi, "m^4"), "Ji": (math.pi / 2, "m^4"),
        "A": (3 * math.pi, "m^2"), "I": (15 * math.pi / 4, "m^4"),
        "Ix": (15 * math.pi / 4, "m^4"), "Iy": (15 * math.pi / 4, "m^4"),
        "J": (15 * math.pi / 2, "m^4"),
    }


# Roundoff budget specific to this binary64 probe, not an engineering tolerance:
# integer powers and binary scale factors are exact; permit eight rounding steps
# per evaluation path (including pi), sixteen across the two paths. Using epsilon
# rather than half epsilon overbounds each step. Subtraction amplifies error by
# at most (Ro^2+Ri^2)/(Ro^2-Ri^2)=5/3; fourth powers give only 17/15.
# gamma(16)*5/3 bounds the relative comparison. No inherited 1e-12 criterion.
ROUNDING_STEPS = 16
EPSILON = sys.float_info.epsilon
REL_TOL = ROUNDING_STEPS * EPSILON / (1 - ROUNDING_STEPS * EPSILON) * 5 / 3


def fresh_values():
    cfg = {"Outer_Pipe": {"Geometry": {"Nominal_OD": 4.0, "Nominal_ID": 2.0}}}
    PipeSizing(cfg).pipe_section_properties("Outer_Pipe")
    props = cfg["Outer_Pipe"]["section_properties"]["pipe"]
    return {**cfg["Outer_Pipe"]["Geometry"], "Ro": 2.0, "Ri": 1.0,
            **{key: value for key, value in props.items() if key != "geometry"},
            "Ix": props["I"], "Iy": props["I"]}


def test_worked_values_match_independent_calculation(builder):
    """Every unrounded quantity and unit follows the integrals beside oracle()."""
    assert sys.float_info.radix == 2 and sys.float_info.mant_dig == 53
    record = builder.calculate()
    assert set(record) == set(oracle())
    module_values = fresh_values()
    for key, (expected, unit) in oracle().items():
        assert record[key]["value"] == pytest.approx(expected, rel=REL_TOL, abs=0)
        assert record[key]["reference"] == pytest.approx(expected, rel=REL_TOL, abs=0)
        assert module_values[key] == pytest.approx(expected, rel=REL_TOL, abs=0)
        assert record[key]["unit"] == unit
        assert record[key]["basis"] in {"synthetic", "polar-integrals"}
        assert record[key]["equation"]
    assert record["J"]["value"] == pytest.approx(
        record["Ix"]["value"] + record["Iy"]["value"], rel=REL_TOL, abs=0
    )


def assert_visible(page, record):
    """Inspect actual table text and units, not hidden binding attributes alone."""
    rows = page.findall(".//tbody[@id='example-values']/tr")
    assert {row.attrib["data-quantity"] for row in rows} == set(oracle())
    for row in rows:
        key = row.attrib["data-quantity"]
        item = record[key]
        assert row.find("td[@class='value']").text == f'{item["value"]:.1f}'
        assert row.find("td[@class='reference']").text == f'{item["reference"]:.1f}'
        assert row.find("td[@class='unit']").text == item["unit"]
        difference = abs(item["value"] - item["reference"])
        assert row.find("td[@class='difference']").text == f"{difference:.1f}"
        assert row.find("td[@class='relative']").text == f'{difference / abs(item["reference"]) * 100:.1f}%'
        assert row.find("td[@class='binding']/a").attrib["href"] in {"#input-basis", "#polar-integrals"}


def test_visible_values_follow_computed_results(builder, monkeypatch):
    """A controlled change at the calculation boundary must change visible output."""
    expected = {key: {"value": value, "reference": value, "unit": unit}
                for key, (value, unit) in oracle().items()}
    normal = builder.calculate()
    text = builder.build(normal)
    page = ET.fromstring(text)
    assert_visible(page, expected)
    fresh = {key: {**expected[key], "value": value} for key, value in fresh_values().items()}
    assert_visible(page, fresh)
    original = PipeSizing.pipe_section_properties

    def changed(self, pipe_flag):
        original(self, pipe_flag)
        # A synthetic perturbation of 1 m^2 exceeds a single display increment.
        self.cfg[pipe_flag]["section_properties"]["pipe"]["A"] += 1.0

    monkeypatch.setattr(PipeSizing, "pipe_section_properties", changed)
    mutated = builder.calculate()
    mutated_expected = copy.deepcopy(expected)
    mutated_expected["A"]["value"] += 1.0
    assert mutated["A"]["value"] == pytest.approx(3 * math.pi + 1, rel=REL_TOL, abs=0)
    changed_text = builder.build(mutated)
    assert changed_text != text
    assert_visible(ET.fromstring(changed_text), mutated_expected)
    old = page.find(".//tr[@data-quantity='A']/td[@class='value']").text
    new = ET.fromstring(changed_text).find(".//tr[@data-quantity='A']/td[@class='value']").text
    assert old != new
    with pytest.raises(AssertionError):
        assert_visible(page, mutated_expected)


def run_builder(directory, cwd):
    environment = dict(os.environ, PYTHONPATH=str(ROOT / "src"), PYTHONDONTWRITEBYTECODE="1")
    subprocess.run([sys.executable, str(REPORT / "build_report.py"), "--output-dir", str(directory)],
                   cwd=cwd, env=environment, check=True, capture_output=True)


def test_report_rebuild_is_deterministic(tmp_path):
    """Two isolated commands reproduce every delivered artifact byte for byte."""
    first, second = tmp_path / "first", tmp_path / "other" / "second"
    run_builder(first, tmp_path)
    run_builder(second, first)
    assert sorted(path.name for path in first.iterdir()) == sorted(ARTIFACTS)
    assert sorted(path.name for path in second.iterdir()) == sorted(ARTIFACTS)
    for name in ARTIFACTS:
        assert (first / name).read_bytes() == (second / name).read_bytes() == (REPORT / name).read_bytes()
        assert str(ROOT).encode() not in (first / name).read_bytes()
        assert str(tmp_path).encode() not in (first / name).read_bytes()
    assert {path for path in tmp_path.rglob("*") if path.is_file()} == {
        directory / name for directory in (first, second) for name in ARTIFACTS
    }


def test_scope_limits_and_report_pointers(builder):
    page = ET.parse(REPORT / "report.html").getroot()
    main = page.find(".//main")
    assert [s.attrib["id"] for s in main.findall("section")] == [f"section-{n}" for n in range(1, 11)]
    children = list(main)
    assert children[0].attrib["id"] == "section-1"
    assert children[1].attrib["id"] == "revision-history"
    assert "Summary" in children[0].find("h2").text
    ids = [node.attrib["id"] for node in page.iter() if "id" in node.attrib]
    assert len(ids) == len(set(ids))
    for a in page.findall(".//a"):
        href = a.attrib["href"]
        if href.startswith("#"):
            assert href[1:] in ids
        else:
            path = href.split("#")[0]
            assert not Path(path).is_absolute()
            assert (REPORT / path).is_file()
    assert not page.findall(".//*[@src]")
    assert not page.findall(".//link")
    limits = {row.find("th").text: row.find("td").text
              for row in page.findall(".//tbody[@id='scope-limits']/tr")}
    assert limits == {key: "NOT_EVALUATED" for key in (
        "pressure containment", "collapse", "combined loading", "material strength", "code compliance")}
    visible = " ".join(page.itertext())
    for phrase in ("geometric probes", "unit agnostic", "not physical validation",
                   "parent issue remains open", "gamma(16)"):
        assert phrase in visible
    assert page.find(".//*[@id='engineering-changes']") is not None
    for path, link in (
        (REPORT / "README.md", "[Module report](report.html)"),
        (ROOT / "src/digitalmodel/structural/pipe_capacity/README.md",
         "[Module report](../../../../docs/reports/modules/pipe_capacity/report.html)"),
    ):
        assert link in path.read_text(encoding="utf-8")
    sources = json.loads((REPORT / "sources.json").read_text(encoding="utf-8"))
    assert sources == builder.source_record()
    for source in sources["methods"]:
        path = ROOT / source["path"]
        node = ast.parse(path.read_text(encoding="utf-8"))
        for part in source["symbol"].split("."):
            node = next(child for child in node.body if getattr(child, "name", None) == part)
        assert (node.lineno, node.end_lineno) == (source["start_line"], source["end_line"])
        lines = path.read_text(encoding="utf-8").splitlines(keepends=True)
        snippet = "".join(lines[source["start_line"] - 1:source["end_line"]])
        assert hashlib.sha256(snippet.encode()).hexdigest() == source["sha256"]
        node = ast.parse(snippet if not snippet.startswith(" ") else "class Context:\n" + snippet)
        assert node.body
        assert source["implementation"] == "IMPLEMENTED"
        assert source["qualification"] in {"INDEPENDENTLY_CHECKED", "NOT_EVALUATED"}
    assert all(method["qualification"] == "NOT_EVALUATED" for method in sources["methods"]
               if method["id"] != "common-section")
    changes = json.loads((REPORT / "changes.json").read_text(encoding="utf-8"))
    assert changes["revisions"][0]["date"] == "2026-10-28"
    assert changes["revisions"][0]["changes"]


def test_safe_html_and_json(builder):
    record = copy.deepcopy(builder.calculate())
    probe = '</script><img src="invalid" /> & <unsafe>'
    record["A"]["equation"] = probe
    text = builder.build(record)
    page = ET.fromstring(text)
    assert not page.findall(".//img")
    assert probe in " ".join(page.itertext())
    embedded = json.loads(page.find(".//script[@id='calculation-record']").text)
    assert set(embedded) == set(oracle())
    for key, (expected, unit) in oracle().items():
        assert embedded[key]["value"] == round(expected, 1)
        assert embedded[key]["reference"] == round(expected, 1)
        assert embedded[key]["unit"] == unit
    assert embedded["A"]["equation"] == probe
    assert "</script><img" not in text


def test_import_is_read_only(tmp_path):
    environment = dict(os.environ, PYTHONPATH=str(ROOT / "src"), PYTHONDONTWRITEBYTECODE="1")
    subprocess.run([sys.executable, "-c", "import runpy, sys; runpy.run_path(sys.argv[1])",
                    str(REPORT / "build_report.py")], cwd=tmp_path, env=environment, check=True)
    assert list(tmp_path.iterdir()) == []


def test_deliverables_are_not_ignored():
    paths = [REPORT / name for name in (*ARTIFACTS, "build_report.py", ".gitignore", "README.md")]
    paths += [Path(__file__), ROOT / "src/digitalmodel/structural/pipe_capacity/README.md"]
    for path in paths:
        result = subprocess.run(["git", "check-ignore", "--no-index", "--", str(path.relative_to(ROOT))],
                                cwd=ROOT, capture_output=True, text=True)
        assert result.returncode == 1, result.stdout
