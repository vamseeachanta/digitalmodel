# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Rebuild the client report and independently read back its numeric evidence."""

import importlib.util
import json
import math
import re
from dataclasses import asdict
from pathlib import Path

import pytest
from bs4 import BeautifulSoup

from digitalmodel.asset_integrity import corroded_pipe

ROOT = Path(__file__).resolve().parents[2]
REPORT = ROOT / "docs/reports/modules/assetintegrity"


@pytest.fixture(scope="module")
def builder():
    spec = importlib.util.spec_from_file_location(
        "asset_report", REPORT / "build_report.py"
    )
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


@pytest.fixture(scope="module")
def rebuilt(builder, tmp_path_factory):
    output = tmp_path_factory.mktemp("asset-report") / "report.html"
    builder.build(output)
    return BeautifulSoup(output.read_text(encoding="utf-8"), "html.parser")


def evidence(soup):
    return json.loads(soup.find("script", id="calculation-data").string)


def test_checked_in_report_rebuilds_byte_for_byte(builder, tmp_path):
    output = tmp_path / "report.html"
    builder.build(output)
    assert output.read_bytes() == (REPORT / "report.html").read_bytes()


def test_every_worked_result_is_a_live_module_return(rebuilt):
    data = evidence(rebuilt)
    inputs = data["inputs"]
    for case in data["cases"]:
        result = corroded_pipe.b31g_original(
            inputs["D"],
            inputs["t"],
            case["depth_in"],
            inputs["L"],
            inputs["smys_psi"],
            maop_psi=inputs["maop_psi"],
            safety_factor=1 / inputs["design_factor"],
        )
        assert case["result"] == asdict(result)
        # The reference cap is a disclosed comparison adapter, not a module field.
        intact_yield = corroded_pipe.b31g_original(
            inputs["D"],
            inputs["t"],
            0,
            inputs["L"],
            inputs["smys_psi"],
            safety_factor=1 / inputs["design_factor"],
        )
        flow_multiplier = intact_yield.flow_stress_psi / inputs["smys_psi"]
        cap = intact_yield.safe_pressure_psi / flow_multiplier
        assert case["reference_cap_psi"] == cap
        assert case["reference_pressure_psi"] == min(result.safe_pressure_psi, cap)


def test_every_chart_sample_is_calculated_by_the_module(rebuilt):
    data = evidence(rebuilt)
    inputs = data["inputs"]
    for row in data["sensitivity"]:
        result = corroded_pipe.b31g_original(
            inputs["D"],
            inputs["t"],
            row["depth_in"],
            inputs["L"],
            inputs["smys_psi"],
            maop_psi=inputs["maop_psi"],
            safety_factor=1 / inputs["design_factor"],
        )
        assert row["pressure_psi"] == result.safe_pressure_psi
        assert row["within_applicability"] == result.applicability.ok
        assert row["reference_pressure_psi"] == min(
            result.safe_pressure_psi, data["cases"][0]["reference_cap_psi"]
        )
    assert data["limits"]["depth_fraction"] == corroded_pipe._B31G_MAX_DT
    assert data["limits"]["depth_in"] == inputs["t"] * corroded_pipe._B31G_MAX_DT
    assert rebuilt.select_one("#pressure-demand-limit")
    assert rebuilt.select_one("#depth-applicability-limit")
    assert rebuilt.select("#pressure-chart [data-sample]")


def test_published_reference_criteria_are_met(rebuilt):
    data = evidence(rebuilt)
    fixture = json.loads((REPORT / "public-example.json").read_text())
    for actual, reference in zip(data["cases"], fixture["cases"], strict=True):
        assert (
            abs(actual["reference_pressure_psi"] - reference["published_pressure_psi"])
            <= reference["tolerance_psi"]
        )
        difference = abs(
            actual["reference_pressure_psi"] - reference["published_pressure_psi"]
        )
        upper_bound = (
            math.ceil(1000 * difference / reference["published_pressure_psi"]) / 10
        )
        assert actual["difference_bound_percent"] == upper_bound
        assert actual["result"]["applicability"]["ok"]


def test_all_visible_numeric_values_have_data_bindings(rebuilt):
    data = evidence(rebuilt)
    values = data["display_values"]
    metrics = rebuilt.select("[data-value]")
    assert len(metrics) > 35
    for node in metrics:
        spec = values[node["data-value"]]
        assert math.isfinite(spec["value"])
        assert node.get_text() == format(spec["value"], spec["format"])
        group, field = node["data-value"].split(".")
        if group == "input":
            assert spec["value"] == data["inputs"][field]
        elif group.startswith("case"):
            case = data["cases"][int(group[4:])]
            expected = (
                case["reference_pressure_psi"] - data["inputs"]["maop_psi"]
                if field == "margin_psi"
                else case.get(field, case["result"].get(field))
            )
            assert spec["value"] == expected
    worked = BeautifulSoup(str(rebuilt.select_one("#section-6")), "html.parser")
    for node in worked.select("[data-value], h2, h3, sup"):
        node.decompose()
    assert not re.search(r"\d", worked.get_text())


def test_layout_sources_and_single_file_delivery(builder, rebuilt):
    assert "hs-doc" in rebuilt.body["class"]
    sections = rebuilt.select("main > section")
    assert len(sections) == 10
    assert not rebuilt.select("h4, h5, h6, b, strong")
    assert rebuilt.select_one("#revision-history")
    assert "Draft for client review" in rebuilt.get_text()
    assert rebuilt.select_one("#section-9-2")
    assert rebuilt.select_one("#comments-panel")
    assert rebuilt.select_one("#changes-panel")
    assert not rebuilt.select("script[src], link[rel='stylesheet'], img[src]")
    exemplar = (ROOT / builder.STYLE_SOURCE).read_text(encoding="utf-8")
    style = re.search(r"<style>(.*?)</style>", exemplar, re.S).group(1)
    assert style in str(rebuilt)
    ids = [node["id"] for node in rebuilt.select("[id]")]
    assert len(ids) == len(set(ids))
    for link in rebuilt.select("a[href^='#']"):
        assert link["href"][1:] in ids
    for link in rebuilt.select("#section-9-2 a[href]"):
        href = link["href"].split("#")[0]
        if "://" not in href:
            assert (REPORT / href).resolve().is_file()


def test_visible_text_and_embedded_values_are_public_wording(rebuilt):
    page = BeautifulSoup(str(rebuilt), "html.parser")
    for node in page.select("script, style"):
        node.decompose()
    visible = page.get_text(" ", strip=True)
    assert not re.search(
        r"\b(owner|agent|lane|session|sha|peer.review|job|PR)\b", visible, re.I
    )
    assert not re.search(r"\b(could|should|would|TBD|safe|certified)\b", visible, re.I)
    assert not re.search(r"\d+\.\d{4,}", visible)


def test_change_panel_matches_engineering_change_record(rebuilt):
    changes = json.loads((REPORT / "changes.json").read_text())
    embedded = json.loads(rebuilt.find("script", id="changes-data").string)
    assert embedded == changes
    assert changes["revisions"][0]["revision"] == "00"
    assert changes["revisions"][0]["changes"]
