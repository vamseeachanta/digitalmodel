# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Offline CALM workflow tests; no native solver or binding is used.

Expected arithmetic is independent: T/allowable=60/100=0.6,
120/100=1.2; curvature/allowable=0.2/(1/4)=0.8; R=1/0.2=5 m.
These are constructed arithmetic examples, not measured or solver responses.
The definitions are in OrcaFlex help, Line results: Forces (Normalised tension)
and Moments (Normalised curvature; Bend radius), public edition accessed 2026-10-20.
"""

from __future__ import annotations

import copy
import json
import re
from pathlib import Path

import pytest
import yaml

from digitalmodel.solvers.orcaflex.calm_workflow import (
    assess_results,
    build_report,
    prepare_campaign,
)

ROOT = Path(__file__).resolve().parents[3]
EXAMPLE = ROOT / "docs/domains/orcaflex/worked_examples/calm_spm/input.yml"
ARITHMETIC_INPUT = Path(__file__).parent / "fixtures/calm_spm_arithmetic.yml"
HOSE_PROPERTIES = (
    "OD",
    "ID",
    "MassPerUnitLength",
    "EA",
    "EI",
    "GJ",
    "PoissonRatio",
    "Cd",
    "Ca",
)


@pytest.fixture
def cfg():
    return yaml.safe_load(ARITHMETIC_INPUT.read_text(encoding="utf-8"))


@pytest.fixture
def prepared(cfg, tmp_path):
    return prepare_campaign(cfg, ROOT, tmp_path / "models")


def response(prepared, *, kind="hand_calculation"):
    """One complete case: hand extrema 60 kN for lines and 0.2/m for hose."""
    case = prepared["cases"][0]
    return {
        "case_id": case["case_id"],
        "model_sha256": case["model_sha256"],
        "evidence_kind": kind,
        "solver_version": "test-version" if kind == "solver_export" else None,
        "source": "Independent arithmetic fixture, see module docstring",
        "period_s": [0, 100],
        "extrema": {
            name: {
                "max": 0.2 if name == "TransferHose" else 60,
                "unit": "1/m" if name == "TransferHose" else "kN",
                "quantity": quantity,
                "coverage": "whole_line",
            }
            for name, quantity in prepared["required_outputs"].items()
        },
    }


def limits(prepared):
    """Constructed allowables: 100 kN and MBR=4 m, not design criteria."""
    return {
        name: {
            "value": 4 if name == "TransferHose" else 100,
            "unit": "m" if name == "TransferHose" else "kN",
            "source": "Constructed arithmetic fixture; no design authority",
        }
        for name in prepared["required_outputs"]
    }


def test_preparation_has_combined_cases_topology_and_validation(prepared, tmp_path):
    """Three named conditions times eight explicitly supplied headings = 24."""
    assert len(prepared["cases"]) == 24
    assert len(prepared["required_outputs"]) == 8  # Six legs, hawser, hose.
    assert all(case["yaml_valid"] for case in prepared["cases"])
    model = yaml.safe_load((tmp_path / "models/case-001.yml").read_text())
    assert len(model["Lines"]) == 8
    assert len(model["6DBuoys"]) == len(model["Vessels"]) == 1
    assert model["Lines"][-2]["EndBConnection"] == "Vessel"
    assert model["Lines"][-1]["EndAConnection"] == "CALM_Buoy"
    assert "IncludeFile" not in json.dumps(model)
    assert len(prepared["sources"]) >= 5
    crossed = prepared["cases"][-1]
    assert crossed["wind_direction_deg"] == 345  # 315 + 30, modulo 360.
    assert crossed["current_direction_deg"] == 285  # 315 - 30.


def test_missing_results_never_pass(prepared):
    result = assess_results(prepared, [], {})
    assert result["status"] == "NOT_EVALUATED"
    assert len(result["checks"]) == 24 * 8
    assert {c["status"] for c in result["checks"]} == {"NOT_EVALUATED"}


def test_hand_example_is_illustrative_even_with_limits(prepared):
    result = assess_results(prepared, [response(prepared)], limits(prepared))
    checks = result["checks"][:8]
    assert checks[0]["utilisation"] == pytest.approx(0.6)
    assert checks[-1]["utilisation"] == pytest.approx(0.8)
    assert checks[-1]["bend_radius_m"] == pytest.approx(5)
    assert {c["status"] for c in checks} == {"ILLUSTRATIVE"}
    assert result["status"] == "NOT_EVALUATED"


def test_solver_comparison_and_missing_limit(prepared):
    row = response(prepared, kind="solver_export")
    criteria = limits(prepared)
    row["extrema"]["Hawser"]["max"] = 120
    row["extrema"]["Mooring_1"]["max"] = 100  # Equality is the specified bound.
    del criteria["Mooring_2"]
    result = assess_results(prepared, [row], criteria)
    checks = {c["object"]: c for c in result["checks"][:8]}
    assert checks["Hawser"]["status"] == "FAIL"
    assert checks["Hawser"]["utilisation"] == pytest.approx(1.2)
    assert checks["Mooring_1"]["status"] == "PASS"
    assert checks["Mooring_2"]["status"] == "NOT_EVALUATED"
    assert result["status"] == "FAIL"


def test_zero_curvature_is_straight_and_json_finite(prepared):
    row = response(prepared)
    row["extrema"]["TransferHose"]["max"] = 0
    result = assess_results(prepared, [row], limits(prepared))
    hose = result["checks"][7]
    assert hose["utilisation"] == 0
    assert hose["bend_radius_m"] is None
    json.dumps(result, allow_nan=False)


@pytest.mark.parametrize(
    "mutation",
    [
        lambda r: r.update(model_sha256="invalid"),
        lambda r: r.update(case_id="unknown"),
        lambda r: r.update(solver_version=None),
        lambda r: r.update(period_s=[-10, 100]),
        lambda r: r.update(evidence_kind="unverified"),
        lambda r: r.update(source=""),
        lambda r: r["extrema"]["Hawser"].update(unit="N"),
        lambda r: r["extrema"]["Hawser"].update(quantity="wall_tension"),
        lambda r: r["extrema"]["Hawser"].update(coverage="end_A"),
        lambda r: r["extrema"]["Hawser"].update(max=float("nan")),
        lambda r: r["extrema"]["Hawser"].update(max=-1),
        lambda r: r["extrema"].update(unknown={}),
    ],
)
def test_rejects_mismatched_response(prepared, mutation):
    row = response(prepared, kind="solver_export")
    mutation(row)
    with pytest.raises(ValueError):
        assess_results(prepared, [row], limits(prepared))


def test_partial_response_marks_missing_component(prepared):
    row = response(prepared, kind="solver_export")
    del row["extrema"]["TransferHose"]
    result = assess_results(prepared, [row], limits(prepared))
    assert result["checks"][7]["status"] == "NOT_EVALUATED"


def test_duplicate_results_and_mixed_solver_versions(prepared):
    row = response(prepared, kind="solver_export")
    with pytest.raises(ValueError, match="duplicate"):
        assess_results(prepared, [row, row], {})
    second = copy.deepcopy(row)
    second.update(
        case_id=prepared["cases"][1]["case_id"],
        model_sha256=prepared["cases"][1]["model_sha256"],
        solver_version="different-version",
    )
    with pytest.raises(ValueError, match="version"):
        assess_results(prepared, [row, second], {})


@pytest.mark.parametrize(
    "mutation",
    [
        lambda c: c.update(headings_deg=[0, 360]),
        lambda c: c.update(headings_deg=[]),
        lambda c: c.update(headings_source=""),
        lambda c: c["conditions"][0].update(wave_height_m=-1),
        lambda c: c["conditions"][0].update(wave_period_s=0),
        lambda c: c["conditions"][0].update(wind_speed_m_s=float("inf")),
        lambda c: c["conditions"][0].update(source=""),
        lambda c: c["coupling"].update(source=""),
        lambda c: c["coupling"]["hawser"].update(EndBConnection="missing"),
        lambda c: c["coupling"]["hose"].update(Name="Hawser"),
    ],
)
def test_rejects_invalid_input(cfg, tmp_path, mutation):
    mutation(cfg)
    with pytest.raises(ValueError):
        prepare_campaign(cfg, ROOT, tmp_path / "bad")


@pytest.mark.parametrize("value", [0, -1, float("nan"), float("inf"), True])
def test_rejects_invalid_limit(prepared, value):
    criteria = limits(prepared)
    criteria["Hawser"]["value"] = value
    with pytest.raises(ValueError):
        assess_results(prepared, [response(prepared)], criteria)


def test_relocation_is_deterministic_and_inputs_unchanged(cfg, prepared, tmp_path):
    original = copy.deepcopy(cfg)
    other = prepare_campaign(cfg, ROOT, tmp_path / "elsewhere")
    assert other == prepared
    assert cfg == original


def test_report_standard_form_and_revision(prepared):
    from digitalmodel.reporting import render_html

    report = build_report(prepared, assess_results(prepared, [], {}))
    keys = [s.key for s in report.sections]
    assert keys[:2] == ["summary", "revision-history"]
    rendered = render_html(report)
    assert "NOT_EVALUATED" in rendered
    assert "Way forward" in rendered
    assert "Normalised" in rendered
    assert "<script src=" not in rendered


@pytest.mark.parametrize(
    "curvature, displayed", [(0.04, "0.04"), (0.00004, "4e-05"), (0, "0")]
)
def test_report_preserves_quantity_precision(prepared, curvature, displayed):
    """96/100 and 104/100 straddle unity; nonzero curvature stays visible."""
    from digitalmodel.reporting import render_html

    row = response(prepared, kind="solver_export")
    row["extrema"]["Mooring_1"]["max"] = 96
    row["extrema"]["Hawser"]["max"] = 104
    row["extrema"]["TransferHose"]["max"] = curvature
    report = build_report(prepared, assess_results(prepared, [row], limits(prepared)))
    results = next(s for s in report.sections if s.key == "results").blocks[0]
    checks = {r[1]: r for r in results.rows[:8]}
    assert checks["Mooring_1"][3:5] == ["96.0", "100.0"]
    assert checks["Mooring_1"][7:9] == ["0.960", "PASS"]
    assert checks["Hawser"][7:9] == ["1.040", "FAIL"]
    assert checks["TransferHose"][3] == displayed
    rendered = render_html(report)
    assert f"<td>{displayed}</td>" in rendered
    assert "<td>0.960</td>" in rendered
    assert "<td>1.040</td>" in rendered


def test_full_solver_export_matrix_scalar_comparison(prepared):
    """All 24 complete arithmetic rows meet 100 kN and 4 m scalar limits."""
    rows = []
    for case in prepared["cases"]:
        row = response(prepared, kind="solver_export")
        row.update(case_id=case["case_id"], model_sha256=case["model_sha256"])
        rows.append(row)
    result = assess_results(prepared, rows, limits(prepared))
    assert result["status"] == "PASS"
    assert result["native_solver_status"] == "NOT_EVALUATED"
    assert len(result["checks"]) == 192  # 24 cases times eight components.


def test_cli_builds_report_and_manifest(cfg, prepared, tmp_path, monkeypatch):
    from digitalmodel.solvers.orcaflex.calm_workflow import main

    config = tmp_path / "input.yml"
    cfg["criteria"] = limits(prepared)
    config.write_text(yaml.safe_dump(cfg, sort_keys=False))
    rows = tmp_path / "responses.json"
    record = response(prepared)
    record["source"] = "Constructed arithmetic — illustrative"
    rows.write_text(json.dumps([record], ensure_ascii=False), encoding="utf-8")
    original_read_text = Path.read_text

    def read_text(path, *args, **kwargs):
        # Emulate a non-UTF-8 platform default for response files only.
        if path == rows and "encoding" not in kwargs:
            kwargs["encoding"] = "ascii"
        return original_read_text(path, *args, **kwargs)

    monkeypatch.setattr(Path, "read_text", read_text)
    out = tmp_path / "cli"
    assert (
        main(
            [
                str(config),
                "--repo-root",
                str(ROOT),
                "--output",
                str(out),
                "--responses",
                str(rows),
            ]
        )
        == 0
    )
    manifest = json.loads((out / "manifest.json").read_text())
    assert manifest["assessment"]["checks"][0]["utilisation"] == pytest.approx(0.6)
    assert "ILLUSTRATIVE" in (out / "report.html").read_text()
    assert len(list((out / "models").glob("*.yml"))) == 24


def test_resuming_changed_model_is_refused(cfg, prepared, tmp_path):
    """An output model is never silently replaced when inputs change."""
    cfg["conditions"][0]["wave_height_m"] = 1
    with pytest.raises(ValueError, match="existing model differs"):
        prepare_campaign(cfg, ROOT, tmp_path / "models")


def test_committed_example_reproduces_report_and_manifest(cfg, prepared, tmp_path):
    """Read constructed fixture values; expected ratios remain independent arithmetic."""
    from digitalmodel.solvers.orcaflex.calm_workflow import main

    example = EXAMPLE.parent
    output = tmp_path / "committed-example"
    assert (
        main(
            [
                str(ARITHMETIC_INPUT),
                "--repo-root",
                str(ROOT),
                "--output",
                str(output),
                "--responses",
                str(example / "hand_calculation.json"),
                "--criteria",
                str(example / "arithmetic_criteria.yml"),
            ]
        )
        == 0
    )
    current = json.loads((output / "manifest.json").read_text())
    committed = json.loads((example / "manifest.json").read_text())
    assert current == committed
    assert current["assessment"]["checks"][0]["utilisation"] == pytest.approx(0.6)
    assert current["assessment"]["checks"][7]["utilisation"] == pytest.approx(0.8)

    # Tool version metadata can differ for an installed package versus a source tree.
    def normalise_tool_version(html):
        return re.sub(
            r'(<span class="k">Tool version</span> )[^<]*(</li>)',
            r"\g<1>test\2",
            html,
        )

    assert normalise_tool_version(
        (output / "report.html").read_text(encoding="utf-8")
    ) == normalise_tool_version((example / "report.html").read_text(encoding="utf-8"))


@pytest.mark.parametrize(
    "mutation",
    [
        lambda c: c["coupling"]["hose_line_type"].update(Name="Hawser"),
        lambda c: c["coupling"]["hawser"].update(EndAX=float("nan")),
        lambda c: c["coupling"]["hawser"].update(
            {"LineType, Length, TargetSegmentLength": []}
        ),
    ],
)
def test_rejects_ambiguous_native_coupling(cfg, tmp_path, mutation):
    mutation(cfg)
    with pytest.raises(ValueError):
        prepare_campaign(cfg, ROOT, tmp_path / "invalid-coupling")


@pytest.mark.parametrize("field", HOSE_PROPERTIES)
@pytest.mark.parametrize("missing", [True, False])
def test_rejects_unresolved_hose_property(cfg, tmp_path, field, missing):
    if missing:
        del cfg["coupling"]["hose_line_type"][field]
    else:
        cfg["coupling"]["hose_line_type"][field] = None
    output = tmp_path / "unresolved"
    with pytest.raises(ValueError, match=field):
        prepare_campaign(cfg, ROOT, output)
    assert not output.exists()


@pytest.mark.parametrize("field", HOSE_PROPERTIES)
@pytest.mark.parametrize("missing", [True, False])
def test_requires_field_level_hose_sources(cfg, tmp_path, field, missing):
    sources = cfg["coupling"]["hose_property_sources"]
    if missing:
        del sources[field]
    else:
        sources[field] = " "
    output = tmp_path / "unsourced"
    with pytest.raises(ValueError, match=field):
        prepare_campaign(cfg, ROOT, output)
    assert not output.exists()


@pytest.mark.parametrize(
    "field, value",
    [
        ("EA", float("nan")),
        ("MassPerUnitLength", True),
        ("OD", 0),
        ("ID", 0.3),
        ("EI", [1]),
        ("EI", [1, None]),
        ("Cd", [1, 0]),
        ("Ca", [1, 0, float("inf")]),
        ("PoissonRatio", "unresolved"),
    ],
)
def test_rejects_invalid_hose_properties(cfg, tmp_path, field, value):
    cfg["coupling"]["hose_line_type"][field] = value
    output = tmp_path / "invalid-hose"
    with pytest.raises(ValueError, match=field):
        prepare_campaign(cfg, ROOT, output)
    assert not output.exists()


def test_engineering_input_is_unresolved_and_separate(cfg, tmp_path):
    engineering = yaml.safe_load(EXAMPLE.read_text(encoding="utf-8"))
    for field in HOSE_PROPERTIES:
        assert engineering["coupling"]["hose_line_type"][field] is None
        assert engineering["coupling"]["hose_property_sources"][field] is None
    # Only the hose data and its provenance differ in the arithmetic fixture.
    fixture = copy.deepcopy(cfg)
    for field in ("source", "hose_line_type", "hose_property_sources"):
        fixture["coupling"][field] = engineering["coupling"][field]
    assert fixture == engineering
    output = tmp_path / "engineering"
    with pytest.raises(ValueError, match="hose property"):
        prepare_campaign(engineering, ROOT, output)
    assert not output.exists()


def test_cli_refuses_unresolved_engineering_input(tmp_path):
    from digitalmodel.solvers.orcaflex.calm_workflow import main

    output = tmp_path / "cli-unresolved"
    with pytest.raises(ValueError, match="hose property"):
        main([str(EXAMPLE), "--repo-root", str(ROOT), "--output", str(output)])
    assert not output.exists()


def test_hose_sources_are_recorded_without_native_metadata(cfg, prepared, tmp_path):
    assert (
        prepared["input"]["coupling"]["hose_property_sources"]
        == (cfg["coupling"]["hose_property_sources"])
    )
    model = yaml.safe_load((tmp_path / "models/case-001.yml").read_text())
    assert model["LineTypes"][-1] == cfg["coupling"]["hose_line_type"]


@pytest.mark.parametrize("field", ["hose_line_type", "hose_property_sources"])
@pytest.mark.parametrize("value", [None, []])
def test_requires_hose_value_and_source_mappings(cfg, tmp_path, field, value):
    cfg["coupling"][field] = value
    output = tmp_path / "missing-hose-mapping"
    with pytest.raises(ValueError, match="hose property"):
        prepare_campaign(cfg, ROOT, output)
    assert not output.exists()
