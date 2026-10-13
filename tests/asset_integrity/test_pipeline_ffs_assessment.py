"""Synthetic assessment: numerical traceability and presentation contract."""

import hashlib
import importlib.util
import json
import re
from html.parser import HTMLParser
from pathlib import Path

import pandas as pd
import pytest

from digitalmodel.asset_integrity.pipeline_defect_screen import assess
from digitalmodel.asset_integrity.assessment.ffs_coordinator import (
    FFSComponent, assess_component,
)
from digitalmodel.asset_integrity.assessment.ffs_decision import remaining_life, reduced_mawp
from digitalmodel.units import Q_

ROOT = Path(__file__).resolve().parents[2]
SPEC = importlib.util.spec_from_file_location(
    "pipeline_ffs_builder", ROOT / "scripts/reporting/build_pipeline_ffs_report.py"
)
builder = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(builder)


class VisibleText(HTMLParser):
    def __init__(self):
        super().__init__()
        self.hidden = 0
        self.text = []

    def handle_starttag(self, tag, attrs):
        if tag in ("style", "script"):
            self.hidden += 1

    def handle_endtag(self, tag):
        if tag in ("style", "script"):
            self.hidden -= 1

    def handle_data(self, data):
        if not self.hidden:
            self.text.append(data)


class TableRows(HTMLParser):
    def __init__(self):
        super().__init__()
        self.rows = []
        self.row = []
        self.cell = None

    def handle_starttag(self, tag, attrs):
        if tag == "tr":
            self.row = []
        if tag == "td":
            self.cell = ""

    def handle_data(self, data):
        if self.cell is not None:
            self.cell += data

    def handle_endtag(self, tag):
        if tag == "td":
            self.row.append(self.cell)
            self.cell = None
        if tag == "tr" and self.row:
            self.rows.append(self.row)


@pytest.fixture(scope="module")
def result():
    return builder.evaluate(ROOT)


def test_source_pins(result):
    for source in result["sources"]:
        payload = (ROOT / source["path"]).read_bytes().replace(b"\r\n", b"\n")
        assert hashlib.sha256(payload).hexdigest() == source["sha256_lf"]


def test_every_result_matches_held_methods(result):
    for section in result["sections"]:
        assert section["strength"] == assess(section["inputs"])
        assert section["strength_decision"]["verdict"] == section["strength"]["decision"]["verdict"]
        inputs = section["inputs"]
        frame = pd.DataFrame(inputs["grid"], index=inputs["axial_positions_in"])
        component = FFSComponent(**section["component"])
        held = assess_component(component, frame)
        assert section["coordinator"]["rsf"] == held.rsf
        assert section["sufficiency"]["status"] == held.sufficiency_status
        assert section["sufficiency"]["metrics"] == builder.serialise(held.sufficiency.metrics)
        for rate, life in section["life_sensitivity"]:
            assert life == remaining_life(
                Q_(held.t_measured_min_in, "inch"), Q_(held.t_min_in, "inch"),
                Q_(rate, "inch/year"),
            )
        assert section["decision"] == builder.map_decision(
            section["strength_decision"]["verdict"], held.sufficiency_status,
            section["remaining_life_yr"], section["interval_yr"],
            flags=not section["applicability"]["ok"],
            supplemental=(held.verdict,),
        )


@pytest.mark.parametrize("verdict,status,life,interval,flags,expected", [
    ("ACCEPT", "SUFFICIENT", 10, 5, False, "accept"),
    ("MONITOR", "SUFFICIENT", 10, 5, False, "accept"),
    ("MONITOR", "SUFFICIENT", 2, 5, False, "escalate"),
    ("DERATE", "SUFFICIENT", 10, 5, False, "re-rate"),
    ("ACCEPT", "TAKE_MORE", 10, 5, False, "take more measurements"),
    ("REPAIR", "TAKE_MORE", 0, 0, False, "take more measurements"),
    ("ESCALATE", "SUFFICIENT", 10, 5, False, "escalate"),
    ("REPAIR", "SUFFICIENT", 0, 0, False, "escalate"),
    ("REPLACE", "SUFFICIENT", 0, 0, False, "escalate"),
    ("ACCEPT", "TAKE_MORE", 10, 5, True, "escalate"),
])
def test_mapping(verdict, status, life, interval, flags, expected):
    assert builder.map_decision(verdict, status, life, interval, flags=flags) == expected


def test_supplemental_escalation():
    assert builder.map_decision("ACCEPT", "SUFFICIENT", 10, 5,
                                supplemental=("REPAIR",)) == "escalate"
    assert builder.map_decision("ACCEPT", "ESCALATE", 10, 5) == "escalate"


def test_rate_sensitivity(result):
    for section in result["sections"]:
        half, base, double = [row[1] for row in section["life_sensitivity"]]
        assert half == base * 2
        assert double == base / 2


def test_pressure_rating():
    expected = reduced_mawp(Q_(800, "psi"))(0.8, 1.0).value.to("psi").magnitude
    assert builder.pressure_rating(800, 0.8) == expected
    assert expected <= 800
    assert builder.pressure_rating(800, 1.4) == 800


def test_report_contract_and_determinism(tmp_path, result):
    first = builder.build(ROOT, tmp_path)
    before = [path.read_bytes() for path in first]
    second = builder.build(ROOT, tmp_path)
    assert before == [path.read_bytes() for path in second]
    page = first[0].read_text(encoding="utf-8")
    parser = VisibleText()
    parser.feed(page)
    visible = " ".join(parser.text)
    assert builder.TITLE in visible
    headings = re.findall(r"<h2>(.*?)</h2>", page)
    assert headings[:10] == [f"{i}. {t}" for i, t in enumerate(builder.SECTION_TITLES, 1)]
    references = page.split('<section id="section-9">')[1].split("</section>")[0]
    assert references.count("<ol>") == 1
    assert references.count("<li>") == 4
    for word in ("screen", "pass", "fail", "verdict", "review", "script", "repository",
                 "Checked", "Approved", "PIPE-SYNTHETIC-RIVER-BOTTOM", "LINE-001",
                 "digitalmodel", "Prepared"):
        assert not re.search(r"\b" + re.escape(word) + r"\b", visible, re.I), word
    assert not re.search(r"(?:src|docs|examples)/", visible)
    for match in re.findall(r"(?<![A-Za-z])\d+\.\d+(?:e[+-]?\d+)?", visible):
        mantissa = match.split("e")[0].replace(".", "").lstrip("0")
        assert len(mantissa) <= 4, match
    assert not re.search(r"(?:API|ASME|DNV).*?</td>\s*<td>\s*\d", page[page.index('id="assumptions"'):])
    manifest = json.loads(first[1].read_text(encoding="utf-8"))
    assert manifest["assessment"] == json.loads(json.dumps(result))
    assert manifest["report_sha256"] == hashlib.sha256(before[0]).hexdigest()


def test_actual_sufficiency_is_not_tuned(result):
    assert [s["decision"] for s in result["sections"]] == [
        "take more measurements", "take more measurements",
    ]
    assert result["sections"][0]["remaining_life_yr"] == 0
    assert result["sections"][1]["sufficiency"]["metrics"]["profile_cells"] == 2


def test_rendered_numbers_against_independent_calculations(tmp_path, result):
    report, _ = builder.build(ROOT, tmp_path)
    html = report.read_text(encoding="utf-8")
    details = html.split('<section id="section-5">')[1].split("</section>")[0]
    for section in result["sections"]:
        fragment = details.split(f"<h3>{section['name']}</h3>")[1].split("<h3>")[0]
        parsed = TableRows()
        parsed.feed(fragment)
        strength = assess(section["inputs"])
        for row in strength["methods"]:
            assert [
                builder.method_name(row["method"]), builder.number(row["capacity_pressure_psi"]),
                builder.number(row["allowable_pressure_psi"]), builder.number(row["allowable_axial_stress_psi"]),
                builder.number(row["demand_psi"]), builder.ratio(row["margin"]),
                "within stated limits" if row["applicability"]["ok"] else "outside stated limits",
            ] in parsed.rows
        inputs = section["inputs"]
        held = assess_component(FFSComponent(**section["component"]),
                               pd.DataFrame(inputs["grid"], index=inputs["axial_positions_in"]))
        for rate, _ in section["life_sensitivity"]:
            life = remaining_life(Q_(held.t_measured_min_in, "inch"), Q_(held.t_min_in, "inch"),
                                  Q_(rate, "inch/year"))
            assert [builder.number(rate), builder.number(life)] in parsed.rows
        assert ["Minimum remaining wall (in)", builder.number(held.t_measured_min_in, thickness=True),
                "Synthetic grid"] in parsed.rows
        assert ["Coefficient of variation", builder.ratio(held.sufficiency.metrics["cov"]),
                "General-loss limit 10.0%"] in parsed.rows
        assert held.sufficiency_status == "TAKE_MORE"
        assert ["Measurement sufficiency", "additional readings required",
                "Retained independently of strength"] in parsed.rows
        assert "Decision: <strong>take more measurements</strong>" in fragment
