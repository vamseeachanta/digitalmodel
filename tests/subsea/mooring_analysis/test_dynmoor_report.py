"""Rebuild and independently reconcile every worked-example output."""

import copy
import csv
import importlib.util
import json
import math
import re
from html.parser import HTMLParser
from pathlib import Path

from digitalmodel.dynmoor_mooring.workflow import router
from digitalmodel.subsea.mooring_analysis import dynmoor

ROOT = Path(__file__).resolve().parents[3]
REPORT = ROOT / "docs/reports/modules/dynmoormooring"


def load_builder():
    spec = importlib.util.spec_from_file_location("dynmoor_report", REPORT / "generate.py")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_report_rebuild_and_all_values(tmp_path):
    builder = load_builder()
    document, changes = builder.build(tmp_path / "build")
    assert document == (REPORT / "report.html").read_text(encoding="utf-8")
    assert changes == json.loads((REPORT / "changes.json").read_text())
    payload = json.loads(re.search(
        r'<script id="results" type="application/json">(.*?)</script>',
        document, re.S,
    ).group(1))
    cfg = json.loads((REPORT / "example.json").read_text())
    cfg["dynmoor_mooring"]["output_dir"] = str(tmp_path / "independent")
    expected = router(copy.deepcopy(cfg))["dynmoor_mooring"]
    assert payload["lines"] == expected["lines"]
    assert payload["offsets"] == expected["offsets_evaluated"]
    with Path(expected["line_tables_csv"]).open() as stream:
        table = [{key: float(value) if key != "model" else value
                  for key, value in row.items()} for row in csv.DictReader(stream)]
    assert payload["table"] == table
    assert payload["inputs"] == json.loads((REPORT / "example.json").read_text())["dynmoor_mooring"]
    assert payload["table_rows"] == dynmoor.N_TABLE
    assert payload["water_weight_factor"] == dynmoor.WATER_WEIGHT_FACTORS["chain"]
    for row in payload["benchmark"]:
        assert row["calculated_tension_kips"] == table[row["row"] - 1]["total_tension_kips"]
        assert row["difference_percent"] == 100 * (
            row["calculated_tension_kips"] / row["reference_tension_kips"] - 1)
    for key, value in builder.numeric_values(payload).items():
        assert f'data-value="{key}">{builder.number(value)}</span>' in document


def test_structure_and_offline_controls(tmp_path):
    builder = load_builder()
    document, _ = builder.build(tmp_path)
    assert '<body class="hs-doc">' in document
    assert len(re.findall(r'<h2>\d+\.', document)) == 10
    assert 'src="http' not in document
    assert 'href="http' not in document
    assert "showSaveFilePicker" in document
    assert "comments.json" in document
    assert 'id="change-panel"' in document
    assert '<title>Case ' in document
    assert "Breaking strength reference" in document
    assert '<strong' not in document and '<b>' not in document


def test_public_example_matches_existing_fixture(tmp_path):
    spec = importlib.util.spec_from_file_location(
        "existing_workflow_tests", Path(__file__).with_name("test_dynmoor_workflow.py")
    )
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    expected = module._base_config(tmp_path)
    expected.pop("_config_dir_path")
    assert json.loads((REPORT / "example.json").read_text()) == expected


def test_saved_curve_against_public_static_function():
    document = (REPORT / "report.html").read_text(encoding="utf-8")
    data = json.loads(re.search(
        r'<script id="results" type="application/json">(.*?)</script>',
        document, re.S,
    ).group(1))
    cfg = data["inputs"]["line_types"][0]
    line_type = dynmoor.DynmoorLineType(
        segment1=dynmoor.DynmoorSegment(**cfg["segments"][0]),
        water_depth_ft=cfg["vertical_distance_anchor_to_fairlead_ft"],
        bottom_friction_percent=cfg["bottom_friction_percent"],
    )
    native = dynmoor.build_line_table(line_type)
    for index, row in enumerate(data["table"]):
        assert row["distance_to_anchor_ft"] == native.u[index]
        assert row["total_tension_kips"] == native.t[index] / 1000
        assert row["horizontal_tension_kips"] == native.h[index] / 1000
        assert row["suspended_length_ft"] == native.total_length_ft - native.z[index]
        assert row["vertical_anchor_pull_kips"] == native.p[index] / 1000


def test_saved_html_has_no_unresolved_or_external_resources():
    document = (REPORT / "report.html").read_text(encoding="utf-8")
    assert not re.search(r"\{\{[A-Z_]+\}\}", document)
    assert 'src="http' not in document
    assert '<body class="hs-doc">' in document
    assert len(re.findall(r'<h2>\d+\.', document)) == 10
    class Links(HTMLParser):
        def __init__(self):
            super().__init__()
            self.links = []

        def handle_starttag(self, tag, attrs):
            self.links.extend(value for key, value in attrs if key == "href")

    parser = Links()
    parser.feed(document)
    for href in parser.links:
        assert (REPORT / href).resolve().is_file()


def test_saved_offsets_against_public_statics():
    document = (REPORT / "report.html").read_text(encoding="utf-8")
    data = json.loads(re.search(
        r'<script id="results" type="application/json">(.*?)</script>', document, re.S
    ).group(1))
    cfg = data["inputs"]["line_types"][0]
    line_type = dynmoor.DynmoorLineType(
        segment1=dynmoor.DynmoorSegment(**cfg["segments"][0]),
        water_depth_ft=cfg["vertical_distance_anchor_to_fairlead_ft"],
        bottom_friction_percent=cfg["bottom_friction_percent"],
    )
    table = dynmoor.build_line_table(line_type)
    lines = [dynmoor.DynmoorLine(
        item["type"], item["fairlead_x_ft"], item["fairlead_y_ft"],
        item["angle_deg"], item["pretension_percent"]
    ) for item in data["inputs"]["lines"]]
    solutions = [dynmoor.solve_pretension(line, line_type, table) for line in lines]
    anchors = [(item.anchor_x_ft, item.anchor_y_ft) for item in solutions]
    for actual, expected in zip(data["lines"], solutions):
        assert actual["pretension_kips"] == expected.pretension_lb / 1000
        assert actual["horizontal_distance_to_anchor_ft"] == expected.horizontal_distance_ft
        assert actual["anchor_x_ft"] == expected.anchor_x_ft
        assert actual["anchor_y_ft"] == expected.anchor_y_ft
        assert actual["fairlead_angle_deg"] == expected.fairlead_angle_deg
    for row in data["offsets"]:
        native = dynmoor.restoring_force(
            lines, [table], anchors, row["surge_ft"], row["sway_ft"],
            row["yaw_deg"] * math.pi / 180,
        )
        assert row["restoring_surge_kips"] == native.surge_lb / 1000
        assert row["restoring_sway_kips"] == native.sway_lb / 1000
        assert row["restoring_yaw_kip_ft"] == native.yaw_lb_ft / 1000
        assert row["max_tension_line"] == native.max_tension_line
        for index, tension in enumerate(native.total_tensions_lb, start=1):
            assert row[f"line_{index}_tension_kips"] == tension / 1000
            assert row[f"line_{index}_horizontal_kips"] == native.horizontal_tensions_lb[index - 1] / 1000
