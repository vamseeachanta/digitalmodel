# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Rebuild the delivered analysis and compare its numbers with live module calls."""

import copy
import importlib.util
import json
from pathlib import Path
import re

from bs4 import BeautifulSoup
import yaml

from digitalmodel.code_checks.workflow import router
from digitalmodel.orcaflex.code_check_engine import APIRP2RDInput, check_api_rp_2rd
import numpy as np

ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts/reporting/build_codechecks_module_report.py"
REPORT = ROOT / "docs/reports/modules/codechecks"


def builder():
    assert SCRIPT.is_file(), "The module report builder is missing"
    spec = importlib.util.spec_from_file_location("codechecks_report", SCRIPT)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def direct_result(tmp_path):
    cfg = yaml.safe_load((ROOT / "examples/workflows/api-rp-2rd-riser/input.yml").read_text())
    cfg["code_check"]["output_dir"] = str(tmp_path / "direct")
    inputs = copy.deepcopy(cfg["code_check"]["inputs"])
    result = router(cfg)["code_check"]
    result.pop("results_csv")
    return {"inputs": inputs, "result": result}


def lookup(data, key):
    for part in key.split("."):
        data = data[int(part)] if isinstance(data, list) else data[part]
    return data


def test_rebuild_and_every_quoted_number_matches_module(tmp_path):
    outputs = builder().generate(tmp_path / "rebuilt")
    assert {p.name for p in outputs} == {"index.html", "changes.json"}
    for path in outputs:
        assert path.read_text(encoding="utf-8") == (REPORT / path.name).read_text(encoding="utf-8")
    soup = BeautifulSoup(outputs[0].read_text(encoding="utf-8"), "html.parser")
    evidence = json.loads(soup.find("script", id="calculation-data").string)
    expected = direct_result(tmp_path)
    assert evidence["inputs"] == expected["inputs"]
    assert evidence["result"] == expected["result"]
    cells = soup.select("[data-field]")
    assert cells
    for cell in cells:
        value = lookup(expected, cell["data-field"])
        assert float(cell["data-value"]) == value
        assert cell.text == format(value * float(cell["data-scale"]), cell["data-format"])
    # Chart labels and hover values are checked separately below.
    example = BeautifulSoup(str(soup.find(id="worked-example")), "html.parser")
    example.svg.decompose()
    for cell in example.select("[data-field]"):
        cell.decompose()
    assert not re.search(r"\d", example.get_text())
    rows = soup.select("#station-results tbody tr")
    assert len(rows) == len(expected["result"]["results"])
    assert evidence["chart"]["utilisation"] == [r["utilisation"] for r in expected["result"]["results"]]
    assert evidence["chart"]["limit"] == 1.0
    assert evidence["chart"]["arc_length"] == expected["inputs"]["arc_lengths"]
    xmax = max(expected["inputs"]["arc_lengths"])
    ymax = max(1.0, expected["result"]["governing_utilisation"]) * 1.15
    for point, row in zip(soup.select(".chart-point"), expected["result"]["results"]):
        tooltip = (f'Public example; riser section; position {row["arc_length"]:.1f} m; '
                   f'tension {row["tension_kN"]:.1f} kN; bending {row["bending_moment_kNm"]:.1f} kN m; '
                   f'pressure echo {row["pressure_diff_MPa"]:.1f} MPa; demand {row["utilisation"]:.1%}; '
                   'screen limit 100.0%; Screen passes.')
        assert point["aria-label"] == tooltip
        assert point.title.text == tooltip
        assert float(point["cx"]) == round(90 + row["arc_length"] / xmax * 650, 3)
        assert float(point["cy"]) == round(300 - row["utilisation"] / ymax * 245, 3)
    labels = [node.text for node in soup.select("#worked-example svg text")]
    assert labels == ["Screen limit: 100.0%", "0.0%", "100.0%", "0.0", "750.0", "1500.0", "Arc coordinate [m]", "Utilisation [%]"]
    assert soup.find(id="utilisation-limit")["d"] == f"M90 {300 - 245 / ymax:.3f} H760"


def test_report_contract_sources_and_comments(tmp_path):
    outputs = builder().generate(tmp_path)
    soup = BeautifulSoup(outputs[0].read_text(encoding="utf-8"), "html.parser")
    assert "hs-doc" in soup.body["class"]
    assert len(soup.select("main > section")) == 10
    assert not soup.select("h4, h5, h6, b, strong, script[src], link[rel=stylesheet]")
    ids = [node["id"] for node in soup.select("[id]")]
    assert len(ids) == len(set(ids))
    assert all(link["href"][1:] in ids for link in soup.select('a[href^="#"]'))
    source = (ROOT / "src/digitalmodel/reporting/assets/calc_report_format.html").read_text()
    style = re.search(r"<style>.*?</style>", source, re.S).group()
    assert style in outputs[0].read_text()
    assert soup.select("#revision-history table")
    assert soup.select("#change-panel table")
    assert soup.select("#utilisation-limit")
    assert len(soup.select(".chart-point[tabindex='0']")) == 3
    assert soup.select("#comments-form textarea")
    script = soup.find("script", id="interaction").string
    assert "showDirectoryPicker" in script and "createWritable" in script
    assert "getFile" in script and "comments.json" in script
    evidence = json.loads(soup.find("script", id="calculation-data").string)
    import hashlib
    for item in evidence["sources"]:
        assert hashlib.sha256((ROOT / item["path"]).read_text(encoding="utf-8").encode("utf-8")).hexdigest() == item["sha256"]
    changes = json.loads(outputs[1].read_text())
    assert changes == evidence["changes"]
    text = soup.get_text(" ", strip=True)
    assert "ASSUMED - to be confirmed" in text
    assert "Not covered" in text and "Recommendations" in text
    assert "NOT_EVALUATED" in text
    assert not re.search(r"\b(certified|compliant)\b", text, re.I)


def test_report_formula_and_pressure_limit_are_real_module_behaviour(tmp_path):
    data = direct_result(tmp_path)
    inp = data["inputs"]
    pipe = APIRP2RDInput(**{key: value for key, value in inp.items() if key in APIRP2RDInput.model_fields})
    axial = pipe.yield_tension / 1000
    moment = pipe.smys * pipe.temperature_derating * (pipe.outer_diameter**3 - pipe.inner_diameter**3) / 6000
    for row in data["result"]["results"]:
        equation = ((row["tension_kN"] / axial)**2 + (row["bending_moment_kNm"] / moment)**2)**0.5 / pipe.design_factor
        assert row["utilisation"] == round(equation, 4)
        assert row["pass"] == (row["utilisation"] <= 1)
    args = (pipe, np.array(inp["arc_lengths"]), np.array(inp["effective_tension_kN"]), np.array(inp["bending_moment_kNm"]))
    no_pressure = check_api_rp_2rd(*args)
    pressure = check_api_rp_2rd(*args, np.array(inp["pressure_diff_MPa"]))
    assert [r.utilisation for r in pressure] == [r.utilisation for r in no_pressure]
