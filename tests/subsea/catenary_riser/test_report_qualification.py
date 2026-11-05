# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Synthetic report contracts; supplied evidence never establishes certification."""

import builtins
from dataclasses import asdict, replace
from datetime import datetime, timezone
from html import unescape
from pathlib import Path
import re
import sys

import pytest

from digitalmodel.subsea.catenary_riser import report


def supplied_data():
    """All values are synthetic display inputs, not a qualified calculation."""
    return report.RiserReportData(
        project="Synthetic example", riser_name="Synthetic riser",
        riser_type="SCR", water_depth_m=2,
        strength_results=[report.StrengthResult("Top", 0, 1, 0, 1, 0.5)],
        fatigue_results=[report.FatigueResult(
            "Top", 0, 0.001, 10, design_fatigue_factor=2,
            design_fatigue_factor_basis="Synthetic test input",
        )],
        code_checks=[report.CodeCheck("Synthetic check", "Synthetic standard", "Top", 0.5)],
        provenance=report.Provenance(
            workflow="synthetic-calculation", workflow_version="1.0",
            standard_revision="Synthetic standard:2026", mode="custom",
            run_id="synthetic-run", generated_utc="2026-01-01T00:00:00Z",
        ),
        revision_history=[{
            "revision": "DRAFT", "date": "2026-01-01",
            "change": "Synthetic inputs for report contract testing.",
        }],
    )


class CompatibleBuilder:
    def __init__(self, **kwargs):
        self.sections = []

    def add_section(self, title, content):
        self.sections.append(f"<h2>{title}</h2>{content}")

    def build(self, path):
        return "<!DOCTYPE html><html>" + "".join(self.sections) + "</html>"


def read_outputs(data, tmp_path):
    written = report.generate_riser_report(data, tmp_path / "analysis.html")
    assert set(written) == {"html", "markdown"}
    return {kind: path.read_text(encoding="utf-8") for kind, path in written.items()}


def assert_evidence_gaps(output, expected):
    match = re.search(r"<strong>Evidence gaps</strong></td><td>(.*?)</td>", output["html"])
    assert match is not None
    assert unescape(match[1]) == expected
    assert f"- **Evidence gaps:** {expected}" in output["markdown"].splitlines()


def test_import_failure_selects_fallback_in_written_package_output(monkeypatch, tmp_path):
    original_import = builtins.__import__

    def unavailable(name, *args, **kwargs):
        if name == "report_template":
            raise ImportError("Synthetic unavailable builder")
        return original_import(name, *args, **kwargs)

    monkeypatch.setattr(builtins, "__import__", unavailable)
    before_path = list(sys.path)
    data = supplied_data()
    before_data = asdict(data)
    output = read_outputs(data, tmp_path)
    assert "riser-renderer: fallback; reason: builder-unavailable" in output["html"]
    assert sys.path == before_path
    assert asdict(data) == before_data
    for text in output.values():
        assert "SUPPLIED EVIDENCE — NOT CERTIFIED" in text
        assert "synthetic-calculation v1.0" in text
        assert "2026-01-01T00:00:00Z" in text
        for title in report.SECTION_ORDER:
            assert title in unescape(text)


@pytest.mark.parametrize("failure", [
    "constructor", "sections", "build", "none", "empty", "whitespace", "bytes",
])
def test_incompatible_builder_selects_fallback_in_written_output(monkeypatch, tmp_path, failure):
    class IncompatibleBuilder(CompatibleBuilder):
        def __init__(self, **kwargs):
            if failure == "constructor":
                raise TypeError("Synthetic constructor failure")
            super().__init__(**kwargs)

        def add_section(self, title, content):
            if failure == "sections":
                raise TypeError("Synthetic section failure")
            super().add_section(title, content)

        def build(self, path):
            if failure == "build":
                raise RuntimeError("Synthetic build failure")
            return {"none": None, "empty": "", "whitespace": "  ", "bytes": b"html"}[failure]

    monkeypatch.setattr(report, "_try_import_builder", lambda: IncompatibleBuilder)
    output = read_outputs(supplied_data(), tmp_path)
    assert "riser-renderer: fallback; reason: builder-incompatible" in output["html"]
    for text in output.values():
        assert "SUPPLIED EVIDENCE — NOT CERTIFIED" in text
        assert "synthetic-run" in text


@pytest.fixture(params=[None, CompatibleBuilder], ids=["fallback", "builder"])
def renderer(request, monkeypatch):
    monkeypatch.setattr(report, "_try_import_builder", lambda: request.param)
    return request.param


def test_supplied_evidence_is_not_certified_in_written_output(tmp_path, renderer):
    data = supplied_data()
    output = read_outputs(asdict(data), tmp_path)
    marker = "riser-renderer: builder" if renderer else "riser-renderer: fallback"
    assert marker in output["html"]
    assert data.evidence_issues == []
    assert data.overall_status == "PASS"
    assert_evidence_gaps(output, "None identified by the data contract")
    for text in output.values():
        assert "SUPPLIED EVIDENCE — NOT CERTIFIED" in text
        assert "CUSTOM (not certified)" in text
        assert "None identified by the data contract" in text


@pytest.mark.parametrize("field", [
    "workflow", "workflow_version", "standard_revision", "run_id", "generated_utc",
])
@pytest.mark.parametrize("value", ["", "  ", None])
def test_missing_provenance_is_partial_in_both_written_outputs(tmp_path, renderer, field, value):
    data = supplied_data()
    data.provenance = replace(data.provenance, **{field: value})
    output = read_outputs(asdict(data), tmp_path)
    assert data.evidence_issues == [field]
    assert_evidence_gaps(output, field)
    assert data.overall_status == "N/A"
    for text in output.values():
        assert "PARTIAL EVIDENCE — NOT CERTIFIED" in text
        assert field in text
    assert getattr(data.provenance, field) == value


def test_unsupplied_workflow_identity_is_not_filled_in(tmp_path, renderer):
    data = supplied_data()
    data.provenance = report.Provenance(
        standard_revision="Synthetic standard:2026", run_id="synthetic-run",
        generated_utc="2026-01-01T00:00:00Z",
    )
    output = read_outputs(data, tmp_path)
    assert data.evidence_issues == ["workflow", "workflow_version"]
    assert_evidence_gaps(output, "workflow, workflow_version")
    for text in output.values():
        assert "PARTIAL EVIDENCE — NOT CERTIFIED" in text
        assert "— v—" in text


@pytest.mark.parametrize("field,value,gap", [
    ("standard_revision", "Synthetic standard", "standard_revision (edition required)"),
    ("generated_utc", "2026-01-01", "generated_utc"),
    ("generated_utc", "2026-01-01T00:00:00", "generated_utc"),
    ("generated_utc", "2026-01-01T01:00:00+01:00", "generated_utc"),
])
def test_unqualified_edition_or_time_is_not_replaced_by_render_time(tmp_path, renderer, field, value, gap):
    data = supplied_data()
    data.provenance = replace(data.provenance, **{field: value})
    output = read_outputs(data, tmp_path)
    assert data.evidence_issues == [gap]
    assert_evidence_gaps(output, gap)
    for text in output.values():
        assert "PARTIAL EVIDENCE — NOT CERTIFIED" in text
        assert gap in text
        assert value in text


@pytest.mark.parametrize("group", [
    "strength_results", "fatigue_results", "code_checks", "revision_history",
])
def test_missing_group_keeps_written_evidence_partial(tmp_path, renderer, group):
    data = supplied_data()
    payload = asdict(data)
    payload.pop(group)
    output = read_outputs(payload, tmp_path)
    assert_evidence_gaps(output, group)
    for text in output.values():
        assert "PARTIAL EVIDENCE — NOT CERTIFIED" in text
        assert group in text


@pytest.mark.parametrize("group,field,value", [
    ("strength_results", "utilisation", float("nan")),
    ("fatigue_results", "design_fatigue_factor", None),
    ("code_checks", "allowable", 0),
])
@pytest.mark.parametrize("failed", [False, True], ids=["unavailable", "failed-and-unavailable"])
def test_unavailable_row_is_partial_without_hiding_failure(tmp_path, renderer, group, field, value, failed):
    data = supplied_data()
    getattr(data, group)[0] = replace(getattr(data, group)[0], **{field: value})
    if failed:
        data.strength_results.append(report.StrengthResult("Bottom", 1, 1, 0, 1, 1.1))
    output = read_outputs(data, tmp_path)
    assert data.evidence_issues == [f"{group}[0] (Top: N/A)"]
    assert data.overall_status == ("FAIL" if failed else "N/A")
    assert_evidence_gaps(output, f"{group}[0] (Top: N/A)")
    status = "FAIL" if failed else "N/A"
    assert f"<strong>Overall status</strong></td><td>{status}</td>" in output["html"]
    assert f"- **Overall status:** {status}" in output["markdown"].splitlines()
    for text in output.values():
        assert "PARTIAL EVIDENCE — NOT CERTIFIED" in text
        assert f"{group}[0] (Top: N/A)" in text


def test_calculation_time_stays_missing_when_report_is_rendered(tmp_path, monkeypatch):
    monkeypatch.setattr(report, "_try_import_builder", lambda: None)
    data = supplied_data()
    data.provenance.generated_utc = ""
    output = read_outputs(data, tmp_path)
    assert "Rendered " in output["html"]
    assert "Rendered " in output["markdown"]
    assert "<strong>Calculation generated (UTC)</strong></td><td>—</td>" in output["html"]
    assert "- **Calculation generated (UTC):** —" in output["markdown"].splitlines()
    assert data.provenance.generated_utc == ""


def test_evidence_gap_order_is_stable_across_provenance_rows_and_groups(tmp_path, renderer):
    data = supplied_data()
    data.provenance.run_id = ""
    data.strength_results[0].utilisation = 0
    data.fatigue_results = []
    data.code_checks[0].allowable = 0
    data.revision_history = []
    expected = [
        "run_id", "strength_results[0] (Top: N/A)", "fatigue_results",
        "code_checks[0] (Top: N/A)", "revision_history",
    ]
    output = read_outputs(data, tmp_path)
    assert data.evidence_issues == expected
    assert_evidence_gaps(output, ", ".join(expected))


@pytest.mark.parametrize("run_id", ["", None])
def test_builder_cannot_invent_missing_calculation_identity(tmp_path, monkeypatch, run_id):
    class IdentityBuilder(CompatibleBuilder):
        def __init__(self, **kwargs):
            super().__init__(**kwargs)
            self.identity = kwargs["demo_id"]

        def build(self, path):
            return super().build(path) + f"<p>Calculation identity: {self.identity}</p>"

    monkeypatch.setattr(report, "_try_import_builder", lambda: IdentityBuilder)
    data = supplied_data()
    data.provenance.run_id = run_id
    output = read_outputs(data, tmp_path)
    assert "riser-renderer: builder" in output["html"]
    assert "Calculation identity: —" in output["html"]
    assert "riser-report" not in output["html"]
    assert_evidence_gaps(output, "run_id")
    assert data.provenance.run_id == run_id


def test_repository_builder_preserves_supplied_evidence_in_written_output(tmp_path):
    assert report._try_import_builder() is not None, "Repository builder requires pandas"
    output = read_outputs(supplied_data(), tmp_path)
    assert "riser-renderer: builder" in output["html"]
    assert_evidence_gaps(output, "None identified by the data contract")
    for text in output.values():
        assert "SUPPLIED EVIDENCE — NOT CERTIFIED" in text
        assert "synthetic-run" in text


@pytest.fixture(params=["fallback", "builder"])
def saved_renderer(request, monkeypatch):
    builder = report._try_import_builder()
    assert builder is not None, "Repository builder requires pandas"

    class RenderClock(datetime):
        @classmethod
        def now(cls, tz=None):
            return cls(2026, 2, 2, 3, 4, tzinfo=timezone.utc)

    monkeypatch.setattr(report, "datetime", RenderClock)
    monkeypatch.setattr(sys.modules[builder.__module__], "datetime", RenderClock)
    monkeypatch.setattr(
        report, "_try_import_builder", lambda: builder if request.param == "builder" else None,
    )
    return request.param


@pytest.mark.parametrize("groups,count", [
    ((), 0),  # No supplied groups: 0 + 0 + 0 = 0 rows.
    (("strength_results",), 1),  # One strength row: 1 + 0 + 0 = 1.
    (("fatigue_results",), 1),  # One fatigue row: 0 + 1 + 0 = 1.
    (("code_checks",), 1),  # One code-check row: 0 + 0 + 1 = 1.
    (("strength_results", "fatigue_results", "code_checks"), 3),  # 1 + 1 + 1 = 3.
])
def test_saved_reports_describe_supplied_rows(tmp_path, saved_renderer, groups, count):
    data = supplied_data()
    for group in ("strength_results", "fatigue_results", "code_checks"):
        if group not in groups:
            setattr(data, group, [])
    before = asdict(data)
    output = read_outputs(data, tmp_path)
    assert f"riser-renderer: {saved_renderer}" in output["html"]
    for text in output.values():
        assert f"Supplied result rows: {count}" in text
        assert "no solver analysis is run by this renderer" in text
        assert "Calculation time is supplied separately in Provenance" in text
        assert "parametric cases" not in text
        assert "analysed overnight" not in text
        assert "generated by automated" not in text
    assert asdict(data) == before


@pytest.mark.parametrize("calculation_time", ["2026-01-01T00:00:00Z", ""])
def test_saved_render_time_is_distinct_from_calculation_time(
    tmp_path, saved_renderer, calculation_time,
):
    data = supplied_data()
    data.provenance.generated_utc = calculation_time
    output = read_outputs(data, tmp_path)
    for text in output.values():
        assert "Rendered 2026-02-02 03:04 UTC" in text
        assert "Generated 2026-02-02" not in text
    shown_time = calculation_time or "—"
    assert (
        f"<strong>Calculation generated (UTC)</strong></td><td>{shown_time}</td>"
        in output["html"]
    )
    assert f"- **Calculation generated (UTC):** {shown_time}" in output["markdown"].splitlines()
    assert data.provenance.generated_utc == calculation_time
    if not calculation_time:
        assert_evidence_gaps(output, "generated_utc")


def test_repository_builder_defaults_remain_in_saved_output(tmp_path):
    builder = report._try_import_builder()
    assert builder is not None, "Repository builder requires pandas"
    path = tmp_path / "default.html"
    rendered = builder("Synthetic report", "Supplied inputs", "synthetic-run", 2).build(path)
    assert path.read_bytes() == rendered.encode("utf-8")
    assert "2 parametric cases analysed overnight" in rendered
    assert '<div class="meta">Generated ' in rendered
    assert (
        "These results are preliminary engineering estimates generated by automated\n"
        "        parametric analysis. All outputs require review by a qualified engineer\n"
        "        before use in design or operational decisions."
    ) in rendered


def test_repository_builder_escapes_report_metadata_in_saved_output(tmp_path):
    builder = report._try_import_builder()
    assert builder is not None, "Repository builder requires pandas"
    path = tmp_path / "metadata.html"
    rendered = builder(
        "Synthetic report", "Supplied inputs", "synthetic-run", 0,
        case_description="<badge>&", timestamp_label="<time>&", disclaimer="<notice>&",
    ).build(path)
    assert path.read_bytes() == rendered.encode("utf-8")
    for tag in ("badge", "time", "notice"):
        assert f"&lt;{tag}&gt;&amp;" in rendered
        assert f"<{tag}>" not in rendered


def test_saved_reports_use_utf8_and_lf(tmp_path, saved_renderer, monkeypatch):
    original_open = Path.open
    text_writes = []

    def windows_text_open(path, mode="r", *args, **kwargs):
        # Reproduce Windows' default newline translation on every platform.
        if mode == "w":
            text_writes.append(path)
            if kwargs.get("newline") is None:
                kwargs["newline"] = "\r\n"
            if kwargs.get("encoding") in (None, "locale"):
                kwargs["encoding"] = "cp1252"
        return original_open(path, mode, *args, **kwargs)

    monkeypatch.setattr(Path, "open", windows_text_open)
    data = supplied_data()
    data.executive_summary = "Synthetic café.\nSecond supplied line."
    written = report.generate_riser_report(data, tmp_path / "text.html")
    assert set(written) == {"html", "markdown"}
    assert set(written.values()) <= set(text_writes)
    for path in written.values():
        content = path.read_bytes()
        assert "Synthetic café.".encode("utf-8") in content
        assert b"\r" not in content
