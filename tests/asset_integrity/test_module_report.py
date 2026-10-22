# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Rebuild the public report and independently read back its numeric evidence."""

import importlib.util
import json
import math
import re
import shutil
from dataclasses import asdict, replace
from pathlib import Path
from threading import Event

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
    assert output.read_bytes() == (REPORT / "report.html").read_bytes().replace(
        b"\r\n", b"\n"
    )


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
    fixture = json.loads((REPORT / "public-example.json").read_text(encoding="utf-8"))
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
    assert "Draft for review" in rebuilt.get_text()
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
    changes = json.loads((REPORT / "changes.json").read_text(encoding="utf-8"))
    embedded = json.loads(rebuilt.find("script", id="changes-data").string)
    assert embedded == changes
    assert changes["revisions"][0]["revision"] == "01"
    assert changes["revisions"][-1]["revision"] == "00"
    assert changes["revisions"][0]["changes"]


def test_descriptor_uses_maintainer_and_located_coefficients():
    fixture = json.loads((REPORT / "public-example.json").read_text(encoding="utf-8"))
    assert fixture["maintainer"] == "digitalmodel"
    expected = {
        "flow_multiplier": 1.1,
        "area_multiplier": 2 / 3,
        "bulging_coefficient": 0.8,
        "branch_limit": 20.0,
    }
    for name, value in expected.items():
        coefficient = fixture["formula_coefficients"][name]
        assert coefficient["value"] == value
        assert coefficient["source"] == "theory"
        assert coefficient["locator"]


def test_worked_pressures_match_independent_hand_calculation(rebuilt):
    data = evidence(rebuilt)
    shallow, boundary = data["cases"]
    assert shallow["result"]["failure_pressure_psi"] == pytest.approx(1526.47, abs=0.01)
    assert shallow["result"]["safe_pressure_psi"] == pytest.approx(1099.06, abs=0.01)
    assert shallow["reference_cap_psi"] == pytest.approx(1093.25, abs=0.01)
    assert boundary["result"]["failure_pressure_psi"] == pytest.approx(
        1265.18, abs=0.01
    )
    assert boundary["result"]["safe_pressure_psi"] == pytest.approx(910.93, abs=0.01)
    assert boundary["reference_cap_psi"] == pytest.approx(1093.25, abs=0.01)


@pytest.mark.parametrize("field", ["flow_stress_psi", "area_ratio", "folias_factor"])
def test_coefficient_drift_stops_report_build(builder, monkeypatch, field):
    original = corroded_pipe.b31g_original

    def altered(*args, **kwargs):
        result = original(*args, **kwargs)
        return replace(result, **{field: getattr(result, field) * 1.01})

    monkeypatch.setattr(corroded_pipe, "b31g_original", altered)
    with pytest.raises(ValueError, match="coefficient"):
        builder.calculate()


def guarded_folias(z):
    if z < 0:
        raise ValueError("Negative geometry parameter")
    if z <= 20:
        return math.sqrt(1 + 0.8 * z)
    return None


def test_added_guard_does_not_change_cited_branch_limit(builder, monkeypatch):
    monkeypatch.setattr(corroded_pipe, "folias_original", guarded_folias)
    assert builder.calculate()["formula"]["branch_limit"] == 20


def test_changed_branch_boundary_stops_report_build(builder, monkeypatch):
    monkeypatch.setattr(
        corroded_pipe,
        "folias_original",
        lambda z: math.sqrt(1 + 0.8 * z) if z <= 21 else None,
    )
    with pytest.raises(ValueError, match="branch"):
        builder.calculate()


def test_crlf_inputs_preserve_canonical_hashes_and_output(
    builder, tmp_path, monkeypatch
):
    relative = REPORT.relative_to(ROOT)
    sources = (
        *builder.DEPENDENCIES,
        builder.STYLE_SOURCE,
        *(
            str(relative / name)
            for name in (
                "public-example.json",
                "changes.json",
                "report.template.html",
            )
        ),
    )
    for source in sources:
        destination = tmp_path / source
        destination.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy2(ROOT / source, destination)
    monkeypatch.setattr(builder, "ROOT", tmp_path)
    monkeypatch.setattr(builder, "HERE", tmp_path / relative)
    lf = builder.build(tmp_path / "lf.html").read_bytes()
    for source in sources:
        path = tmp_path / source
        path.write_bytes(
            path.read_bytes().replace(b"\r\n", b"\n").replace(b"\n", b"\r\n")
        )
    assert builder.build(tmp_path / "crlf.html").read_bytes() == lf


def test_boundary_prose_discloses_recomputed_depth_flag(rebuilt):
    criteria = rebuilt.select_one("#section-3").get_text()
    assert "equality remains inside" not in criteria
    assert "binary rounding" in criteria
    data = evidence(rebuilt)
    assert not data["sensitivity"][-1]["within_applicability"]


def test_module_report_is_registered_with_isolated_inputs(
    builder, tmp_path, monkeypatch
):
    from scripts import check_generated_html as checker

    script = "docs/reports/modules/assetintegrity/build_report.py"
    entries = [entry for entry in checker.GENERATORS if entry.script == script]
    assert len(entries) == 1
    entry = entries[0]
    assert script in checker.discover_candidate_generators(ROOT)
    required = {
        script,
        "pyproject.toml",
        builder.STYLE_SOURCE,
        *builder.DEPENDENCIES,
        *(
            f"docs/reports/modules/assetintegrity/{name}"
            for name in (
                "public-example.json",
                "changes.json",
                "report.template.html",
            )
        ),
    }
    assert required <= set(entry.inputs)
    monkeypatch.setattr(checker, "GENERATORS", (entry,))
    shadow = tmp_path / "shadow"
    checker.prepare_shadow(ROOT, shadow)
    checker.clear_generated_html(shadow, entry)
    assert checker.run_generator(shadow, entry) is None
    assert checker.compare_outputs(ROOT, shadow) == []


def test_lf_comparison_preserves_content_drift_detection(tmp_path, monkeypatch):
    from scripts import check_generated_html as checker

    entry = checker.Generator("build.py", ("report.html",), canonical_lf=True)
    monkeypatch.setattr(checker, "GENERATORS", (entry,))
    committed, fresh = tmp_path / "committed", tmp_path / "fresh"
    committed.mkdir()
    fresh.mkdir()
    (committed / "report.html").write_bytes(b"<p>Original</p>\r\n")
    (fresh / "report.html").write_bytes(b"<p>Original</p>\n")
    assert checker.compare_outputs(committed, fresh) == []
    (fresh / "report.html").write_bytes(b"<p>Changed</p>\n")
    assert len(checker.compare_outputs(committed, fresh)) == 1
    monkeypatch.setattr(checker, "GENERATORS", (replace(entry, canonical_lf=False),))
    (fresh / "report.html").write_bytes(b"<p>Original</p>\n")
    assert len(checker.compare_outputs(committed, fresh)) == 1


def test_registered_prerequisite_finishes_before_dependent_generator(
    tmp_path, monkeypatch
):
    from scripts import check_generated_html as checker

    ready = Event()
    producer = checker.Generator("producer.py")
    consumer = checker.Generator("consumer.py", prerequisites=("producer.py",))
    monkeypatch.setattr(checker, "GENERATORS", (producer, consumer))
    monkeypatch.setattr(checker, "clear_generated_html", lambda *args: None)

    def run(shadow, entry):
        if entry is producer:
            ready.wait(0.1)
            ready.set()
        elif not ready.is_set():
            return "Required input has not been regenerated"
        return None

    monkeypatch.setattr(checker, "run_generator", run)
    assert checker.regenerate(tmp_path) == []
