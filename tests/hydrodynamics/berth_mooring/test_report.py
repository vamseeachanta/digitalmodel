"""End-to-end worked-report checks; the report uses the standard engine."""

from pathlib import Path
import sys

from digitalmodel.hydrodynamics.berth_mooring import load_spec, write_campaign
from digitalmodel.hydrodynamics.berth_mooring_report import build_report, write_example

INPUT = (
    Path(__file__).resolve().parents[3]
    / "examples/domains/mooring/berth_mooring/inputs.yml"
)


def test_report_has_inputs_equations_answer_and_limits(tmp_path):
    """Hand answer: 60+100=160 kN; at x=10 m the moment is 1600 kN m.

    These are steady applied loads, not mooring or fender response loads.
    """
    spec = load_spec(INPUT)
    manifest = write_campaign(spec, tmp_path)
    report = build_report(spec, manifest)
    assert [s.key for s in report.sections][:2] == ["summary", "revision"]
    from digitalmodel.reporting import render_html

    html = render_html(report)
    for expected in (
        "160.0",
        "1600.0",
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
    from digitalmodel.hydrodynamics.berth_mooring import build_campaign, input_digest

    spec = load_spec(INPUT)
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
    assert "exported samples evaluated against supplied criteria" in html


def test_report_uses_changed_inputs(tmp_path):
    """At twice wind speed, 60 kN becomes 240 kN; with 100 kN current,
    combined sway is 340 kN and yaw at x=10 m is 3400 kN m.
    This ensures the worked arithmetic updates when the input changes.
    """
    spec = load_spec(INPUT)
    spec["wind"]["velocity_m_s"] = [0, 20]
    manifest = write_campaign(spec, tmp_path)
    from digitalmodel.reporting import render_html

    html = render_html(build_report(spec, manifest))
    assert "340.0" in html and "3400.0" in html
    assert "squared = 240.0 kN" in html
    assert "squared = 60.0 kN" not in html
