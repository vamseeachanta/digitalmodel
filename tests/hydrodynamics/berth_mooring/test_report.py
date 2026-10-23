"""End-to-end worked-report checks; the report uses the standard engine."""

from pathlib import Path
from copy import deepcopy
import sys

import pytest

from digitalmodel.hydrodynamics.berth_mooring import (
    build_campaign,
    evaluate_results,
    input_digest,
    load_spec,
    write_campaign,
)
from digitalmodel.hydrodynamics.berth_mooring_report import build_report, write_example
from digitalmodel.reporting import TableBlock

INPUT = (
    Path(__file__).resolve().parents[3]
    / "examples/domains/mooring/berth_mooring/inputs.yml"
)


@pytest.fixture(scope="module")
def prepared_report(tmp_path_factory):
    spec = load_spec(INPUT)
    spec["sampling"]["time_step_s"] = 10
    return spec, write_campaign(spec, tmp_path_factory.mktemp("report"))


@pytest.fixture
def report_data(prepared_report):
    return deepcopy(prepared_report)


def _stored_manifest(spec, manifest):
    objects = spec["lines"] + spec["fenders"]
    stored = {
        "solver_version": "synthetic-test",
        "input_sha256": input_digest(spec),
        "cases": {
            case["name"]: {item["name"]: [10, 20] for item in objects}
            for case in build_campaign(spec)
        },
    }
    manifest["response"] = evaluate_results(spec, stored)
    manifest["solver_version"] = stored["solver_version"]
    return stored


def _summary(report):
    return next(s for s in report.sections if s.key == "summary").blocks[0].markdown


@pytest.mark.parametrize("partial", [False, True])
def test_report_summary_exposes_missing_stored_criteria(report_data, partial):
    """A solver version cannot turn absent criteria into evaluated acceptance."""
    spec, manifest = report_data
    if partial:
        spec["criteria"] = {
            "line_a": {"allowable_kn": 100, "source": "synthetic test criterion"}
        }
    _stored_manifest(spec, manifest)
    summary = _summary(build_report(spec, manifest))
    assert "Line/fender response: **NOT_EVALUATED**" in summary
    assert "**PASS**" not in summary


def test_report_summary_governing_failure_overrides_incomplete_criteria(report_data):
    """104/100 governs over 102/100 and a separate missing criterion."""
    spec, manifest = report_data
    spec["criteria"] = {
        item["name"]: {"allowable_kn": 100, "source": "synthetic test criterion"}
        for item in spec["lines"]
    }
    stored = _stored_manifest(spec, manifest)
    first, second = list(stored["cases"])[:2]
    stored["cases"][first]["line_a"] = [104]
    stored["cases"][second]["line_b"] = [102]
    manifest["response"] = evaluate_results(spec, stored)
    report = build_report(spec, manifest)
    summary = _summary(report)
    assert "Line/fender response: **FAIL**" in summary
    assert first in summary and "line_a" in summary and "1.04" in summary
    assert second not in summary
    response = next(s for s in report.sections if s.key == "response").blocks[1]
    assert response.rows[0][4] == "1.04"


def test_report_summary_empty_response_is_not_evaluated(report_data):
    spec, manifest = report_data
    manifest["response"] = []
    manifest["solver_version"] = "synthetic-test"
    assert "Line/fender response: **NOT_EVALUATED**" in _summary(
        build_report(spec, manifest)
    )


def test_report_summary_missing_response_row_is_not_evaluated(report_data):
    """A partial export cannot establish acceptance for every case and object."""
    spec, manifest = report_data
    spec["criteria"] = {
        item["name"]: {"allowable_kn": 100, "source": "synthetic test criterion"}
        for item in spec["lines"] + spec["fenders"]
    }
    _stored_manifest(spec, manifest)
    manifest["response"].pop()
    assert "Line/fender response: **NOT_EVALUATED**" in _summary(
        build_report(spec, manifest)
    )


def test_report_tables_expose_captions_and_unconverged_loads(report_data):
    spec, manifest = report_data
    manifest["cases"][0]["peak_force_n"] = {
        "surge_n": 1234567.89,
        "sway_n": 0.00123456789,
    }
    manifest["cases"][0]["peak_yaw_nm"] = 12345.6789
    report = build_report(spec, manifest)
    tables = [
        block
        for section in report.sections
        for block in section.blocks
        if isinstance(block, TableBlock)
    ]
    assert all(table.title.strip() for table in tables)
    assert "unconverged" in tables[3].title.lower()
    assert "1.23e+03" == tables[3].rows[0][5]
    assert "1.23e-06" == tables[3].rows[0][6]
    assert "12.3" == tables[3].rows[0][7]


def test_report_revision_preserves_initial_issue_and_records_correction(report_data):
    spec, manifest = report_data
    report = build_report(spec, manifest)
    assert report.document.revision == "01"
    assert report.document.number == "B0000-RPT-013-01"
    revision = next(s for s in report.sections if s.key == "revision").blocks[0]
    assert revision.rows[0] == [
        "00",
        "2026-10-20",
        "Initial generic preparation to expose inputs, methods and pending response.",
    ]
    assert revision.rows[1][:2] == ["01", "2026-10-20"]
    assert revision.rows[1][2].strip()


def test_report_describes_spatial_refinement_and_time_step_upper_bound(report_data):
    spec, manifest = report_data
    report = build_report(spec, manifest)
    method = next(s for s in report.sections if s.key == "method").blocks[0].markdown
    assert "time_step_s" in method and "upper bound" in method
    assert "stagger increment" in method and "slowest passage" in method


def test_report_flags_missing_rotational_inertia_before_solver_loading(report_data):
    spec, manifest = report_data
    report = build_report(spec, manifest)
    forward = next(s for s in report.sections if s.key == "forward").blocks[0].markdown
    assert "Roll/pitch inertia is NOT_EVALUATED" in forward
    assert "null placeholders" in forward and "before solver loading" in forward


def test_report_has_inputs_equations_answer_and_limits(tmp_path):
    """Hand answer: 60+100=160 kN; at x=10 m the moment is 1600 kN m.

    These are steady applied loads, not mooring or fender response loads.
    """
    spec = load_spec(INPUT)
    spec["sampling"]["time_step_s"] = 10
    manifest = write_campaign(spec, tmp_path)
    report = build_report(spec, manifest)
    assert [s.key for s in report.sections][:2] == ["summary", "revision"]
    from digitalmodel.reporting import render_html

    html = render_html(report)
    for expected in (
        "160",
        "1.60e+03",
        "NOT_EVALUATED",
        "rho",
        "Orcina",
        "assumed, based on previous experience",
        "Way forward",
    ):
        assert expected in html
    assert "OrcFxAPI" not in sys.modules
    assert "<script src=" not in html


def test_reproducible_example_build(tmp_path):
    """Two identical inputs produce identical reports and campaign digests."""
    first, second = tmp_path / "first", tmp_path / "second"
    write_example(INPUT, first)
    write_example(INPUT, second)
    for name in ("report.html", "case-table.csv", "manifest.json"):
        assert (first / name).read_bytes() == (second / name).read_bytes()


def test_report_with_synthetic_stored_results(tmp_path):
    """Independent ratio: 20 kN peak / 100 kN allowable = 0.2.

    These synthetic exports exercise reporting, not solver response validation.
    """
    spec = load_spec(INPUT)
    spec["sampling"]["time_step_s"] = 10
    objects = spec["lines"] + spec["fenders"]
    spec["criteria"] = {
        item["name"]: {"allowable_kn": 100, "source": "synthetic test criterion"}
        for item in objects
    }
    stored = {
        "solver_version": "synthetic-test",
        "input_sha256": input_digest(spec),
        "cases": {
            case["name"]: {item["name"]: [10, 20] for item in objects}
            for case in build_campaign(spec)
        },
    }
    manifest = write_campaign(spec, tmp_path, stored=stored)
    assert all(row["status"] == "PASS" for row in manifest["response"])
    from digitalmodel.reporting import render_html

    html = render_html(build_report(spec, manifest))
    assert "synthetic-test" in html
    assert "0.2" in html
    assert "synthetic test criterion" in html
    assert "Line/fender response: <strong>PASS</strong>" in html


def test_report_uses_changed_inputs(tmp_path):
    """At twice wind speed, 60 kN becomes 240 kN; with 100 kN current,
    combined sway is 340 kN and yaw at x=10 m is 3400 kN m.
    This ensures the worked arithmetic updates when the input changes.
    """
    spec = load_spec(INPUT)
    spec["sampling"]["time_step_s"] = 10
    spec["wind"]["velocity_m_s"] = [0, 20]
    manifest = write_campaign(spec, tmp_path)
    from digitalmodel.reporting import render_html

    html = render_html(build_report(spec, manifest))
    assert "340" in html and "3.40e+03" in html
    assert "squared = 240 kN" in html
    assert "squared = 60.0 kN" not in html
