# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Absolute-pressure sweep coverage and plot/export traceability."""

import csv
import hashlib
import json
from collections import Counter

import plotly.graph_objects as go
import pytest

from digitalmodel.structural.analysis import wall_thickness_mt_report as report
from digitalmodel.structural.analysis.wall_thickness import (
    DesignCode,
    PipeGeometry,
    PipeMaterial,
)
from digitalmodel.structural.analysis.wall_thickness_mt_report import (
    CONDITIONS_BY_CODE,
    DEFAULT_WORKING_PRESSURES,
    PRESSURE_SWEEP_LIMITATION,
    WorkingPressure,
    _compute_pressure_sensitivity,
    generate_mt_pressure_reports,
    generate_mt_report,
)


@pytest.fixture
def pipe():
    """Synthetic geometry and material, unrelated to a worked example."""
    return (
        PipeGeometry(outer_diameter=0.3, wall_thickness=0.02),
        PipeMaterial(grade="X65", smys=450e6, smts=535e6),
    )


@pytest.fixture
def pressures():
    return [WorkingPressure(0.0), WorkingPressure(7.123456789, "ASSUMED test input")]


@pytest.fixture
def figures(monkeypatch):
    """Capture the actual chart data without a browser or external request."""
    captured = []

    def capture(figure, **kwargs):
        captured.append(figure.to_plotly_json())
        return "<div>captured plotly chart</div>"

    monkeypatch.setattr(go.Figure, "to_html", capture)
    return captured


@pytest.mark.parametrize("code", list(CONDITIONS_BY_CODE))
def test_every_condition_pressure_curve_matches_exports(
    pipe, pressures, figures, tmp_path, code,
):
    geometry, material = pipe
    output = tmp_path / f"{code.value}.html"
    html = generate_mt_report(
        geometry, material, 3e6, 1e6, code=code,
        working_pressures=pressures, output_path=str(output),
    )
    manifest = json.loads(output.with_suffix(".pressure-sweep.json").read_text())
    with output.with_suffix(".pressure-sweep.csv").open(newline="") as stream:
        csv_rows = list(csv.DictReader(stream))
    start = html.index('<script type="application/json" id="pressure-sweep-manifest">')
    embedded = json.loads(html[start:].split(">", 1)[1].split("</script>", 1)[0])
    assert embedded == manifest
    assert output.read_text() == html
    assert PRESSURE_SWEEP_LIMITATION in html
    assert manifest["working_pressures"] == [
        {"pressure_mpa": 0.0, "basis": "ASSUMED"},
        {"pressure_mpa": 7.123456789, "basis": "ASSUMED test input"},
    ]
    sweep = next(f for f in figures if "Working pressure sweep" in f["layout"]["title"]["text"])
    traces = sweep["data"]
    expected = Counter(
        (code.value, cond["name"], pressure.pressure_mpa)
        for cond in CONDITIONS_BY_CODE[code] for pressure in pressures
    )
    assert Counter(
        (t["meta"]["code"], t["meta"]["condition"], t["meta"]["pressure_mpa"])
        for t in traces
    ) == expected
    assert len(csv_rows) == len(manifest["rows"]) == sum(len(t["x"]) for t in traces)
    for trace in traces:
        rows = [r for r in manifest["rows"] if r["curve_id"] == trace["meta"]["curve_id"]]
        assert [r["tension_kn"] for r in rows] == trace["x"]
        assert [r["moment_knm"] for r in rows] == trace["y"]
        assert trace["x"][0] == trace["x"][-1]
        assert trace["y"][0] == trace["y"][-1]
        assert min(trace["y"]) < 0 < max(trace["y"])
        assert "nominal wall" in trace["name"]
        assert str(trace["meta"]["pressure_mpa"]) in trace["name"]
        for row in rows:
            for key, value in trace["meta"].items():
                assert row[key] == value
            assert row["external_pressure_mpa"] == 1.0
            assert "source" not in row
    for actual, exported in zip(manifest["rows"], csv_rows):
        assert set(actual) == set(exported)
        for key, value in actual.items():
            if isinstance(value, (float, int)):
                assert float(exported[key]) == value
            else:
                assert exported[key] == value
    assert "+50%" not in html
    assert "+100%" not in html
    assert manifest["limitations"] == [PRESSURE_SWEEP_LIMITATION]
    assert "limitation" not in manifest
    assert PRESSURE_SWEEP_LIMITATION != report.F101_SCREENING_LABEL
    assert all(label in html for label in manifest["limitations"])
    if code != DesignCode.DNV_ST_F101:
        assert report.F101_SCREENING_LABEL not in html
        assert report.F101_SIMPLIFIED_FORM_LABEL not in html
    assert "4th Edition" not in html


def test_absolute_pressure_retains_net_pressure_on_bore_approximation():
    """Distinguish the retained approximation from cylinder force balance.

    With d_i=0.2 m, t=0.01 m, pi=7 MPa and pe=2 MPa:
    the bore approximation gives (7-2)*0.2/(2*0.01) = 50 MPa.
    Cylinder balance gives (7*0.2-2*0.22)/(2*0.01) = 48 MPa.
    The approximation exceeds force balance by pe = 2 MPa.
    """
    cases = _compute_pressure_sensitivity(
        A=0.01, Z=0.001, d_i=0.2, t=0.01, pe=2e6,
        smys=450e6, T_y=4.5e6, M_p=450e3,
        conditions=[{"name": "Synthetic", "f_d": 0.72}],
        working_pressures=[WorkingPressure(7.0, "ASSUMED hand calculation")],
    )
    assert cases[0]["pi_MPa"] == 7.0
    assert cases[0]["sigma_h_MPa"] == pytest.approx(50.0)
    force_balance_mpa = (7.0 * 0.2 - 2.0 * 0.22) / (2 * 0.01)
    assert force_balance_mpa == pytest.approx(48.0)
    assert cases[0]["sigma_h_MPa"] - force_balance_mpa == pytest.approx(2.0)


def test_default_pressures_are_stated_and_assumed(pipe, figures, tmp_path):
    geometry, material = pipe
    output = tmp_path / "default.html"
    html = generate_mt_report(geometry, material, 30e6, 0.0, output_path=str(output))
    manifest = json.loads(output.with_suffix(".pressure-sweep.json").read_text())
    assert [p["pressure_mpa"] for p in manifest["working_pressures"]] == [0.0, 10.0, 20.0]
    assert all(p.basis == "ASSUMED" for p in DEFAULT_WORKING_PRESSURES)
    assert "may not bracket the design pressure" in html
    assert "net-pressure-on-bore approximation" in html
    assert "exceeds cylinder force balance by the external pressure" in html


def test_batch_writes_one_report_and_exports_per_supported_code(
    pipe, pressures, figures, tmp_path,
):
    geometry, material = pipe
    reports = generate_mt_pressure_reports(
        geometry, material, 3e6, 1e6, str(tmp_path), working_pressures=pressures,
    )
    assert set(reports) == set(CONDITIONS_BY_CODE)
    assert len(list(tmp_path.glob("*.html"))) == len(CONDITIONS_BY_CODE)
    assert len(list(tmp_path.glob("*.csv"))) == len(CONDITIONS_BY_CODE)
    assert len(list(tmp_path.glob("*.json"))) == len(CONDITIONS_BY_CODE)
    for code, path in reports.items():
        manifest = json.loads(path.with_suffix(".pressure-sweep.json").read_text())
        assert {r["code"] for r in manifest["rows"]} == {code.value}


def test_design_pressure_does_not_rescale_sweep(pipe, pressures, figures, tmp_path):
    geometry, material = pipe
    manifests = []
    for index, design_pressure in enumerate([0.0, 23e6]):
        path = tmp_path / f"comparison-{index}.html"
        generate_mt_report(
            geometry, material, design_pressure, 1e6,
            working_pressures=pressures, output_path=str(path),
        )
        manifests.append(json.loads(path.with_suffix(".pressure-sweep.json").read_text()))
    assert manifests[0] == manifests[1]


def test_pressure_basis_is_escaped_in_html(pipe, figures):
    geometry, material = pipe
    basis = "ASSUMED </script><b>input</b>"
    html = generate_mt_report(
        geometry, material, 3e6, 1e6,
        working_pressures=[WorkingPressure(5.0, basis)],
    )
    assert basis not in html
    assert "ASSUMED &lt;/script&gt;&lt;b&gt;input&lt;/b&gt;" in html
    start = html.index('<script type="application/json" id="pressure-sweep-manifest">')
    embedded = json.loads(html[start:].split(">", 1)[1].split("</script>", 1)[0])
    assert embedded["working_pressures"][0]["basis"] == basis


@pytest.mark.parametrize("value", [-1.0, float("nan"), float("inf"), float("-inf")])
def test_invalid_pressure_is_rejected(value):
    with pytest.raises(ValueError, match="finite and non-negative"):
        WorkingPressure(value)


@pytest.mark.parametrize("basis", ["", "   "])
def test_missing_basis_is_rejected(basis):
    with pytest.raises(ValueError, match="basis"):
        WorkingPressure(5.0, basis)


@pytest.mark.parametrize("pressures", [[], [WorkingPressure(5.0), WorkingPressure(5.0, "other basis")]])
def test_empty_or_duplicate_pressure_list_is_rejected(pipe, pressures, tmp_path):
    geometry, material = pipe
    with pytest.raises(ValueError, match="non-empty.*unique"):
        generate_mt_report(
            geometry, material, 3e6, 1e6,
            working_pressures=pressures, output_path=str(tmp_path / "invalid.html"),
        )
    assert not list(tmp_path.iterdir())


def test_unsupported_envelope_code_is_rejected(pipe, tmp_path):
    geometry, material = pipe
    with pytest.raises(ValueError, match="No pressure-sweep envelope"):
        generate_mt_report(
            geometry, material, 3e6, 1e6, code=DesignCode.ASME_B31_8,
            output_path=str(tmp_path / "unsupported.html"),
        )
    assert not list(tmp_path.iterdir())


@pytest.mark.parametrize("entries", [[0.0, 10.0], [WorkingPressure(5.0), 10.0]])
def test_non_working_pressure_entries_are_rejected(pipe, tmp_path, entries):
    geometry, material = pipe
    with pytest.raises(ValueError, match="WorkingPressure"):
        generate_mt_report(
            geometry, material, 3e6, 1e6, working_pressures=entries,
            output_path=str(tmp_path / "invalid.html"),
        )
    assert not list(tmp_path.iterdir())


@pytest.fixture
def synthetic_f101_review(tmp_path, monkeypatch):
    """Complete software-only evidence, without engineering approval."""
    implementation = tmp_path / "strategy.py"
    implementation.write_text("# Synthetic implementation for gate tests.\n")
    path = tmp_path / "review.json"
    monkeypatch.setattr(report, "F101_COMBINED_FORM_REVIEW", path)
    monkeypatch.setattr(report, "F101_COMBINED_FORM_IMPLEMENTATION", implementation)
    record = {
        "schema_version": 1, "code": "DNV-ST-F101", "status": "passed",
        "agreement": True, "review_date": "2026-10-15",
        "reason": "Synthetic evidence for label rendering only.",
        "form": {
            "id": "f101-root-sum-square",
            "implementation": (
                "src/digitalmodel/structural/analysis/wall_thickness_codes/dnv_st_f101.py"
            ),
            "symbol": "DnvStF101Strategy._check_combined_loading",
            "reviewed_revision": "0" * 40,
            "sha256": hashlib.sha256(implementation.read_bytes()).hexdigest(),
            "expression": "synthetic", "wall_basis": "synthetic",
            "normalisation": "synthetic", "domain": "synthetic",
            "terms": {key: "synthetic" for key in ("m", "n", "q")},
        },
        "source": {
            "title": "Synthetic", "series": "Synthetic", "publisher": "Synthetic",
            "locator": "Synthetic", "url": "https://example.org/synthetic",
            "scope": "Software tests only", "year": 2026, "pages": [1],
            "access": {
                "retrieved_utc": "2026-10-15T00:00:00Z", "http_status": 200,
                "sha256": "0" * 64,
            },
        },
        "comparison_points": [
            {"id": key, "reason": "Synthetic", "agreement": True}
            for key in (
                "single-load-limits", "mixed-moment-tension",
                "pressure-coupling", "report-envelope",
            )
        ],
    }
    return path, record


@pytest.mark.parametrize("state", ["passed", "failed", "missing", "malformed", "stale"])
def test_f101_gate_controls_all_charts_and_sweep_manifest(
    pipe, figures, tmp_path, synthetic_f101_review, state,
):
    path, record = synthetic_f101_review
    if state == "failed":
        record["status"] = "failed"
        record["agreement"] = False
    elif state == "stale":
        record["form"]["sha256"] = "0" * 64
    if state == "malformed":
        path.write_text("{")
    elif state != "missing":
        path.write_text(json.dumps(record))
    geometry, material = pipe
    output = tmp_path / "f101.html"
    html = generate_mt_report(
        geometry, material, 3e6, 1e6, code=DesignCode.DNV_ST_F101,
        working_pressures=[WorkingPressure(5.0)], output_path=str(output),
    )
    manifest = json.loads(output.with_suffix(".pressure-sweep.json").read_text())
    assert manifest["limitations"] == [PRESSURE_SWEEP_LIMITATION]
    assert report.F101_SCREENING_LABEL not in json.dumps(manifest)
    label = report.F101_SIMPLIFIED_FORM_LABEL
    assert label not in json.dumps(manifest)
    notices = html.split('<p class="f101-envelope-limitation">')[1:]
    assert len(notices) == 3
    for notice in notices:
        paragraph = notice.split("</p>", 1)[0]
        assert paragraph.count(report.F101_SCREENING_LABEL) == 1
        assert paragraph.count(label) == (0 if state == "passed" else 1)
    assert html.count(report.F101_SCREENING_LABEL) == 3
    assert html.count(label) == (0 if state == "passed" else 3)
