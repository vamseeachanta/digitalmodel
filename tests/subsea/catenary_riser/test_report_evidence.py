# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Report evidence is a data contract; rendering never certifies a design."""

import builtins
from dataclasses import replace
import sys

import pytest

from digitalmodel.subsea.catenary_riser import report


def complete_data():
    data = report.sample_riser_report_data()
    data.provenance.generated_utc = "2026-10-22T00:00:00Z"
    return data


def test_builder_import_failure_and_fallback_leave_import_path_unchanged(monkeypatch):
    original = builtins.__import__

    def unavailable(name, *args, **kwargs):
        if name == "report_template":
            raise ImportError("synthetic unavailable builder")
        return original(name, *args, **kwargs)

    monkeypatch.setattr(builtins, "__import__", unavailable)
    before = list(sys.path)
    output = report.render_html(complete_data())
    assert "riser-renderer: fallback; reason: builder-unavailable" in output
    assert "Strength / Utilisation Summary" in output
    assert sys.path == before


@pytest.mark.parametrize("kind", ["constructor", "sections", "build", "non_text"])
def test_incompatible_builder_fallback_is_visible(monkeypatch, kind):
    class Builder:
        def __init__(self, **kwargs):
            if kind == "constructor":
                raise TypeError("synthetic incompatible constructor")

        def add_section(self, *args):
            if kind == "sections":
                raise TypeError("synthetic incompatible sections")

        def build(self, path):
            if kind == "build":
                raise RuntimeError("synthetic build failure")
            return None

    monkeypatch.setattr(report, "_try_import_builder", lambda: Builder)
    output = report.render_html(complete_data())
    assert "riser-renderer: fallback; reason: builder-incompatible" in output
    assert "Provenance" in output
    assert "0.82" in output


def test_compatible_builder_receives_evidence_sections(monkeypatch):
    class Builder:
        def __init__(self, **kwargs):
            self.sections = []

        def add_section(self, title, content):
            self.sections.append(content)

        def build(self, path):
            return "<html>" + "".join(self.sections) + "</html>"

    monkeypatch.setattr(report, "_try_import_builder", lambda: Builder)
    data = complete_data()
    data.strength_results = []
    output = report.render_html(data)
    assert "riser-renderer: builder" in output
    assert "PARTIAL EVIDENCE — NOT CERTIFIED" in output
    assert "strength_results" in output


@pytest.mark.parametrize("field", ["standard_revision", "run_id", "generated_utc"])
def test_required_provenance_is_not_inferred(field):
    data = complete_data()
    data.provenance = replace(data.provenance, **{field: ""})
    for render in (report.render_html, report.render_markdown):
        output = render(data)
        assert "PARTIAL EVIDENCE — NOT CERTIFIED" in output
        assert field in output
    assert getattr(data.provenance, field) == ""


@pytest.mark.parametrize("timestamp", ["yesterday", "2026-10-22", "2026-10-22T00:00:00"])
def test_timestamp_requires_explicit_timezone(timestamp):
    data = complete_data()
    data.provenance.generated_utc = timestamp
    assert "generated_utc" in data.evidence_issues


@pytest.mark.parametrize("group", ["strength_results", "fatigue_results", "code_checks"])
def test_missing_group_is_partial_evidence(group):
    data = complete_data()
    setattr(data, group, [])
    for render in (report.render_html, report.render_markdown):
        output = render(data)
        assert "PARTIAL EVIDENCE — NOT CERTIFIED" in output
        assert group in output
    # The sample's failure is fatigue: retain it whenever that group is present.
    assert data.overall_status == ("N/A" if group == "fatigue_results" else "FAIL")


def test_custom_and_unknown_modes_never_certify():
    data = complete_data()
    for mode in ("custom", "other"):
        data.provenance.mode = mode
        output = report.render_html(data)
        assert "not certified" in output.lower()
        assert "CERTIFIED (custom)" not in output
    assert "mode" in data.evidence_issues


def test_passing_subset_does_not_become_overall_pass():
    data = report.RiserReportData("synthetic", "synthetic", "SCR", 2,
                                 strength_results=[report.StrengthResult("top", 0, 1, 0, 1, .5)])
    assert data.overall_status == "N/A"
    assert "fatigue_results" in data.evidence_issues


def test_no_groups_plain_dictionary_and_unknown_style():
    payload = dict(project="synthetic", riser_name="synthetic", riser_type="SCR", water_depth_m=2)
    data = report._coerce(payload)
    assert data.strength_results == []
    assert data.fatigue_results == []
    assert data.code_checks == []
    output = report.render_html(payload)
    assert "PARTIAL EVIDENCE — NOT CERTIFIED" in output
    assert all(name in output for name in ("strength_results", "fatigue_results", "code_checks"))
    assert report._status_span("UNKNOWN") == "UNKNOWN"


def test_report_defaults_preserve_missing_evidence():
    provenance = report.Provenance()
    assert provenance.standard_revision == ""
    assert provenance.run_id == ""
    assert provenance.generated_utc == ""
    assert set(provenance.missing_fields) == {"standard_revision", "run_id", "generated_utc"}
    data = report.RiserReportData("synthetic", "synthetic", "SCR", 2)
    assert data.max_strength_utilisation is None
    assert data.min_fatigue_life_years is None
    assert data.overall_status == "N/A"
    assert "revision_history" in data.evidence_issues


def test_marginal_band_remains_a_report_convention():
    # Synthetic ratios either side of the EXISTING 0.9 display boundary;
    # engineering-basis.md E5 distinguishes this from a code acceptance factor.
    assert report._status_of(.9) == "PASS"
    assert report._status_of(.91) == "MARGINAL"
    assert report._status_of(1.01) == "FAIL"
