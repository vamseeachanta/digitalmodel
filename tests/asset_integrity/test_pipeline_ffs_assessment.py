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


def test_every_result_matches_existing_methods(result):
    from digitalmodel.asset_integrity.assessment.measurement_sufficiency import MeasurementSufficiency

    for section in result["sections"]:
        assert section["strength"] == assess(section["inputs"])
        inputs = section["inputs"]
        frame = pd.DataFrame(inputs["grid"], index=inputs["axial_positions_in"])
        component = FFSComponent(**section["component"])
        existing = assess_component(component, frame, force_type="LML")
        sufficiency = MeasurementSufficiency().evaluate(
            frame, nominal_wt_in=component.nominal_wt_in, assessment_type="LML",
            level1_result=existing.level1, level2_result=section["level2"],
        )
        assert section["sufficiency"]["status"] == sufficiency.status
        assert section["sufficiency"]["metrics"] == builder.serialise(sufficiency.metrics)
        for rate, life in section["life_sensitivity"]:
            assert life == remaining_life(
                Q_(existing.t_measured_min_in, "inch"), Q_(existing.t_min_in, "inch"),
                Q_(rate, "inch/year"),
            )
        assert section["decision"] == builder.map_decision(
            section["strength_decision"]["verdict"], sufficiency.status,
            section["remaining_life_yr"], section["interval_yr"],
            flags=not section["applicability"]["ok"],
        )


@pytest.mark.parametrize("verdict,status,life,interval,flags,expected", [
    ("ACCEPT", "SUFFICIENT", 10, 5, False, "accept"),
    ("MONITOR", "SUFFICIENT", 10, 5, False, "accept"),
    ("MONITOR", "SUFFICIENT", 2, 5, False, "escalate"),
    ("DERATE", "SUFFICIENT", 10, 5, False, "re-rate"),
    ("ACCEPT", "TAKE_MORE", 10, 5, False, "take more measurements"),
    ("REPAIR", "TAKE_MORE", 0, 0, False, "escalate"),
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
    assert builder.map_decision("ACCEPT", "TAKE_MORE", 0, 0,
                                supplemental=("REPAIR",)) == "escalate"


def test_local_loss_and_reference_wall(result):
    from digitalmodel.asset_integrity.assessment.level2_engine import Level2Engine

    for section in result["sections"]:
        data = section["inputs"]
        component = FFSComponent(**section["component"])
        reference_wall = component.nominal_wt_in - component.fca_in
        expected = Level2Engine(
            "LML", component.nominal_od_in, component.nominal_wt_in,
            reference_wall, component.rsf_a,
        ).evaluate(pd.DataFrame(data["grid"], index=data["axial_positions_in"]))
        assert section["coordinator"]["assessment_type"] == "LML"
        assert section["level2"] == builder.serialise(expected)
        assert section["reference_wall_in"] == reference_wall
        assert section["level2"]["rsf"] < 1
        rating = reduced_mawp(Q_(section["uncorroded_rating_psi"], "psi"))(
            expected["rsf"], component.rsf_a,
        ).value.to("psi").magnitude
        assert section["local_loss_rating_psi"] == rating
        demand = component.design_pressure_psi
        assert section["strength_decision"]["margin"] == min(
            rating / demand, min(row["margin"] for row in section["strength"]["methods"]),
        )
        assert section["pressure_rating_psi"] <= demand


def test_tampered_source_digest_is_rejected(tmp_path):
    workflow = tmp_path / builder.WORKFLOW
    workflow.mkdir(parents=True)
    config = builder.load_yaml(ROOT / builder.WORKFLOW / "input.yml")
    (workflow / "input.yml").write_bytes((ROOT / builder.WORKFLOW / "input.yml").read_bytes())
    for source in config["sections"]:
        target = tmp_path / source["path"]
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_bytes((ROOT / source["path"]).read_bytes())
    target = tmp_path / config["sections"][0]["path"]
    target.write_bytes(target.read_bytes() + b"\n# tampered input\n")
    with pytest.raises(ValueError, match="Synthetic input digest mismatch"):
        builder.evaluate(tmp_path)


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
                 "PIPE-SYNTHETIC-RIVER-BOTTOM", "LINE-001", "digitalmodel"):
        assert not re.search(r"\b" + re.escape(word) + r"\b", visible, re.I), word
    assert not re.search(r"(?:src|docs|examples)/", visible)
    for match in re.findall(r"(?<![A-Za-z])\d+\.\d+(?:e[+-]?\d+)?", visible):
        mantissa = match.split("e")[0].replace(".", "").lstrip("0")
        assert len(mantissa) <= 4, match
    assert not re.search(r"(?:API|ASME|DNV).*?</td>\s*<td>\s*\d", page[page.index('id="assumptions"'):])
    manifest = json.loads(first[1].read_text(encoding="utf-8"))
    assert manifest["assessment"] == json.loads(json.dumps(result))
    assert manifest["report_sha256"] == hashlib.sha256(before[0]).hexdigest()
    assert "B0000-RPT-001-00" in visible
    assert "Revision 00" in visible
    assert "2026-10-13" in visible
    control = page.split('<div id="document-control">')[1].split('</div>')[0]
    assert re.sub(r"<[^>]+>", "", control).strip() == (
        "Synthetic example\nPrepared: — | Checked: — | Approved: —"
    )
    assert not re.search(r"\b\d+(?:\.\d+)?e[+-]\d+\b", visible)


def test_actual_sufficiency_is_not_tuned(result):
    assert [s["decision"] for s in result["sections"]] == [
        "escalate", "take more measurements",
    ]
    assert result["sections"][0]["remaining_life_yr"] == 0
    assert result["sections"][1]["sufficiency"]["metrics"]["profile_cells"] == 2


def test_rendered_numbers_match_existing_engine_outputs(tmp_path, result):
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
        existing = assess_component(FFSComponent(**section["component"]),
                               pd.DataFrame(inputs["grid"], index=inputs["axial_positions_in"]),
                               force_type="LML")
        for rate, _ in section["life_sensitivity"]:
            life = remaining_life(Q_(existing.t_measured_min_in, "inch"), Q_(existing.t_min_in, "inch"),
                                  Q_(rate, "inch/year"))
            assert [builder.number(rate), builder.number(life)] in parsed.rows
        assert ["Minimum remaining wall (in)", builder.number(existing.t_measured_min_in, thickness=True),
                "Synthetic grid"] in parsed.rows
        assert ["Coefficient of variation", builder.ratio(section["sufficiency"]["metrics"]["cov"]),
                "Descriptive only; no general-loss averaging criterion applied"] in parsed.rows
        status = section["sufficiency"]["status"]
        assert ["Measurement sufficiency", "additional readings required" if status == "TAKE_MORE" else status.lower(),
                "Retained independently of strength"] in parsed.rows
        assert f"Decision: <strong>{section['decision']}</strong>" in fragment


def test_public_assumptions_exclude_method_coefficients(tmp_path):
    report, _ = builder.build(ROOT, tmp_path)
    page = report.read_text(encoding="utf-8")
    assert re.findall(r"<h3>(.*?)</h3>", page) == [
        "3.1 Site data", "3.2 Equipment data", "3.3 Environmental data",
        "3.4 Vessel data", "3.5 Criteria", "3.6 Method", "3.7 Load cases",
        "Section A, 24 in line", "Section B, 12.75 in line",
    ]
    # Numbered headings are document structure, not coefficient values. Their
    # exact content is checked above; scan all remaining visible text below.
    parser = VisibleText()
    parser.feed(re.sub(r"<h3>.*?</h3>", "", page))
    visible = " ".join(parser.text)
    assumptions = builder.load_yaml(ROOT / builder.WORKFLOW / "assumptions.yml")
    assert {row["parameter"] for row in assumptions["assumptions"]} == {
        "Corrosion rate", "Re-inspection interval",
        "Section B supplementary strength, loads and geometry", "Decision precedence",
        "Point-minimum intervention rule", "Location class",
    }
    # Reject coefficient disclosures, allowing unrelated grid readings and
    # synthetic input quantities that happen to share digits with constants.
    for token in ("1.39", "0.72", "0.648", "1.285", "0.6275", "0.003375",
                  "0.354", "0.032", "0.9", "0.90", "1.1", "2/3", "0.8", "0.80",
                  "0.85", "3.3", "0.999", "0.5", "0.50", "0.31", "0.48", "0.25",
                  "0.10", "0.02", "0.020", "0.95", "0.05", "80.0%", "85.0%", "10.0%"):
        pattern = r"(?<![\d.])" + re.escape(token) + r"(?![\d.])"
        assert not re.search(pattern, visible), token
        assert not re.search(pattern, str(assumptions)), token
    assert "Minimum 15 readings" not in visible
    assert "minimum of 5" not in visible
    assert "assumed modelling" not in visible


def test_single_decision_in_all_decision_sections(tmp_path, result):
    page = builder.build(ROOT, tmp_path)[0].read_text(encoding="utf-8")
    for number in (1, 5, 6, 8):
        fragment = page.split(f'<section id="section-{number}">')[1].split('</section>')[0]
        assert "escalate" in fragment
        assert "take more measurements" in fragment
    assert "while the local profile remains under-sampled" in page
    assert "around axial position 4 in, column 2" in page
    assert "around axial position 2 in, column 4" in page


def test_design_rate_is_read_from_configuration(result):
    from copy import deepcopy

    changed = deepcopy(result)
    for section in changed["sections"]:
        section["component"]["corrosion_rate_in_per_yr"] *= 2
    tables = [block for block in builder.design_blocks(changed) if isinstance(block, builder.TableBlock)]
    rows = [row for block in tables for row in block.rows if row[1] == "Corrosion rate (in/yr)"]
    assert all(row[2] == builder.number(changed["sections"][0]["component"]["corrosion_rate_in_per_yr"]) for row in rows)
