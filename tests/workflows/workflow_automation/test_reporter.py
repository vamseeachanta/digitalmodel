"""HTML report content for workflow results."""

import re
from datetime import datetime, timedelta
from pathlib import Path

from digitalmodel.workflows.workflow_automation.models import (
    TaskStatus,
    WorkflowResult,
)
from digitalmodel.workflows.workflow_automation.reporter import (
    WorkflowHTMLReporter,
    generate_workflow_report,
)

START = datetime(2026, 1, 5, 9, 0, 0)


def make_result(status="partial", statuses=None, outputs=None):
    return WorkflowResult(
        workflow_id="wf123",
        workflow_name="Riser Check",
        status=status,
        start_time=START,
        end_time=START + timedelta(seconds=12.34),
        task_statuses=statuses if statuses is not None else {
            "catenary": TaskStatus.COMPLETED,
            "viv": TaskStatus.FAILED,
            "fatigue": TaskStatus.CACHED,
            "extra": TaskStatus.SKIPPED,
        },
        outputs=outputs if outputs is not None else {
            "top_tension": 251234.56789,
            "cycles": 1234567,
            "viv_status": "HIGH",
        },
    )


def test_report_summarises_status_timing_and_failures(tmp_path):
    path = WorkflowHTMLReporter(output_dir=str(tmp_path)).generate_report(
        make_result(), output_file="report.html"
    )

    assert Path(path) == tmp_path / "report.html"
    html = Path(path).read_text(encoding="utf-8")
    assert "<title>Workflow Report: Riser Check</title>" in html
    assert "Workflow ID: wf123" in html
    assert '<span class="metric-value status-partial">PARTIAL</span>' in html
    assert '<span class="metric-value">12.3s</span>' in html
    assert '<span class="metric-value">50.0%</span>' in html  # 2 of 4 tasks
    assert '<span class="metric-value">4</span>' in html
    assert (
        '<span class="metric-label">Failed Tasks</span>'
        '<span class="metric-value status-failed">viv</span>'
    ) in html


def test_report_formats_outputs_by_type(tmp_path):
    html = Path(
        WorkflowHTMLReporter(output_dir=str(tmp_path)).generate_report(make_result())
    ).read_text(encoding="utf-8")

    assert "<h2>Key Outputs</h2>" in html
    assert re.search(r"top_tension:</span>\s*<span class=\"output-value\">251,234\.5679<", html)
    assert re.search(r"cycles:</span>\s*<span class=\"output-value\">1,234,567<", html)
    assert re.search(r"viv_status:</span>\s*<span class=\"output-value\">HIGH<", html)


def test_status_chart_counts_each_status_with_its_colour(tmp_path):
    reporter = WorkflowHTMLReporter(output_dir=str(tmp_path))
    statuses = {
        "a": TaskStatus.COMPLETED,
        "b": TaskStatus.COMPLETED,
        "c": TaskStatus.FAILED,
        "d": "custom",  # not a TaskStatus: shown as text with the fallback colour
    }

    chart = reporter._create_task_status_chart(make_result(statuses=statuses))

    assert "values: [2, 1, 1]" in chart
    assert "labels: ['completed', 'failed', 'custom']" in chart
    assert "colors: ['#10b981', '#ef4444', '#94a3b8']" in chart
    assert "Plotly.newPlot('task-status-chart'" in chart
    assert reporter._create_timeline_chart(make_result()) == ""


def test_clean_result_has_no_failure_or_output_sections(tmp_path):
    result = make_result(
        status="completed",
        statuses={"a": TaskStatus.COMPLETED},
        outputs={},
    )

    html = Path(
        WorkflowHTMLReporter(output_dir=str(tmp_path)).generate_report(result)
    ).read_text(encoding="utf-8")

    assert "Failed Tasks" not in html
    assert "Key Outputs" not in html
    assert "status-completed" in html


def test_default_location_and_file_name(tmp_path, monkeypatch):
    monkeypatch.chdir(tmp_path)

    path = Path(generate_workflow_report(make_result()))

    assert path.parent == Path("reports")
    assert re.fullmatch(r"workflow_wf123_\d{8}_\d{6}\.html", path.name)
    assert (tmp_path / path).is_file()


def test_convenience_function_honours_directory_and_name(tmp_path):
    path = generate_workflow_report(
        make_result(), output_file="named.html", output_dir=str(tmp_path / "out")
    )

    assert Path(path) == tmp_path / "out" / "named.html"
    assert Path(path).is_file()
