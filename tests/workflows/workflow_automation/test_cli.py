"""Exercise the workflow-automation CLI through the real orchestrator and reporter.

Task modules are stand-ins registered by the ``*_modules`` fixtures
(see conftest.py); everything between the command line and those modules runs
unchanged.
"""

import json
import re
import sys
from pathlib import Path

import pytest
from click.testing import CliRunner

from digitalmodel.workflows.workflow_automation import cli as cli_module
from digitalmodel.workflows.workflow_automation.cli import cli, main

RISER_ARGS = [
    "riser-analysis",
    "--diameter", "0.5",
    "--thickness", "0.025",
    "--length", "1500",
    "--water-depth", "1000",
    "--offset", "100",
    "--current-speed", "1.0",
]
MOORING_ARGS = [
    "mooring-design",
    "--vessel-type", "fpso",
    "--water-depth", "1500",
    "--line-length", "2000",
]
STRUCTURAL_ARGS = [
    "structural-check",
    "--plate-length", "2.0",
    "--plate-width", "1.0",
    "--plate-thickness", "0.02",
]


def invoke(args):
    return CliRunner().invoke(cli, args)


def report_path(output):
    """Return the HTML report path the command printed."""
    match = re.search(r"^HTML report: (.+)$", output, re.MULTILINE)
    assert match, output
    return Path(match.group(1).strip())


def test_list_names_every_workflow_command():
    result = invoke(["list"])

    assert result.exit_code == 0
    for workflow_id in (
        "riser-analysis",
        "mooring-design",
        "structural-check",
        "vessel-response",
    ):
        assert f"ID: {workflow_id}" in result.output
    assert "Modules:     catenary_riser, viv_analysis, fatigue_analysis" in result.output
    assert "Usage: workflow-automation run <workflow-id> [options]" in result.output


def test_version_option_reports_package_version():
    result = invoke(["--version"])

    assert result.exit_code == 0
    assert "workflow-automation, version 1.1.0" in result.output


def test_main_runs_the_command_group(monkeypatch, capsys):
    monkeypatch.setattr(sys, "argv", ["workflow-automation", "list"])

    with pytest.raises(SystemExit) as exit_info:
        main()

    assert exit_info.value.code == 0
    assert "=== Available Workflows ===" in capsys.readouterr().out


def test_riser_analysis_reports_chained_results_and_saves_them(run_dir, riser_modules):
    result = invoke(RISER_ARGS + ["--output", "out/riser.json"])

    assert result.exit_code == 0, result.output
    assert "Riser:  OD=0.5m, WT=0.025m, L=1500.0m" in result.output
    assert "Status:         COMPLETED" in result.output
    assert "Success Rate:   100.0%" in result.output
    assert "Tasks:          4" in result.output
    assert "Top Tension:    250.0 kN" in result.output
    assert "VIV Status:     LOW" in result.output
    assert "Fatigue Life:   45.6 years" in result.output
    assert "Failed Tasks" not in result.output

    # Tasks run in dependency order; the screening task receives the
    # natural frequency produced by the previous task.
    assert [key.split(":")[1] for key, _ in riser_modules] == [
        "analyze_simple_catenary",
        "calculate_natural_frequency",
        "screen_viv_susceptibility",
        "assess_fatigue_life",
    ]
    catenary_inputs = riser_modules[0][1]
    assert catenary_inputs["material"] == "x65"
    assert catenary_inputs["internal_fluid"] == "oil"
    assert riser_modules[2][1] == {
        "current_speed": 1.0,
        "natural_frequency": 0.25,
        "diameter": 0.5,
    }

    saved = json.loads((run_dir / "out/riser.json").read_text())
    assert saved["status"] == "completed"
    assert saved["task_count"] == 4
    assert saved["outputs"]["riser_top_tension"] == 2.5e5
    assert saved["outputs"]["viv_reduced_velocity"] == pytest.approx(8.0)
    assert "Results saved to: out/riser.json" in result.output

    html = report_path(result.output).read_text(encoding="utf-8")
    assert "<h1>Complete Riser Analysis</h1>" in html
    assert "status-completed" in html


def test_riser_analysis_lists_failed_tasks_when_the_workflow_continues(
    run_dir, riser_modules, monkeypatch
):
    def no_modal_solution(**kwargs):
        raise RuntimeError("no modal solution")

    monkeypatch.setattr(
        sys.modules["digitalmodel.modules.viv_analysis.frequency_calculator"],
        "calculate_natural_frequency",
        no_modal_solution,
    )
    create = cli_module.CompleteRiserAnalysisWorkflow.create

    def continue_on_error(**kwargs):
        workflow = create(**kwargs)
        workflow.stop_on_error = False
        return workflow

    monkeypatch.setattr(
        cli_module.CompleteRiserAnalysisWorkflow,
        "create",
        staticmethod(continue_on_error),
    )

    result = invoke(RISER_ARGS)

    assert result.exit_code == 0, result.output
    assert "Status:         PARTIAL" in result.output
    # The screening task depends on the missing frequency and fails too.
    assert "Failed Tasks:   natural_freq, viv_screen" in result.output
    assert "Success Rate:   50.0%" in result.output
    assert "Top Tension:    250.0 kN" in result.output
    assert "VIV Status" not in result.output
    assert "Fatigue Life:   45.6 years" in result.output


def test_riser_analysis_without_analysis_modules_reports_failure(run_dir):
    # No stand-ins are registered: the first task's module cannot be imported.
    result = invoke(RISER_ARGS)

    assert "Status:         FAILED" in result.output
    assert (
        "Error: Task 'Catenary Static Analysis' failed: "
        "Module not found: digitalmodel.modules.catenary_riser.simple_catenary"
    ) in result.output
    assert "Top Tension" not in result.output
    assert "status-failed" in report_path(result.output).read_text(encoding="utf-8")


@pytest.mark.parametrize(
    "args", [RISER_ARGS, MOORING_ARGS, STRUCTURAL_ARGS], ids=lambda a: a[0]
)
def test_unwritable_output_path_exits_with_error(
    run_dir, riser_modules, mooring_modules, structural_modules, args
):
    (run_dir / "blocker").write_text("a file, not a directory")

    result = invoke(args + ["--output", "blocker/result.json"])

    assert result.exit_code == 1
    assert "Error: " in result.output
    assert "HTML report" not in result.output


def test_mooring_design_reports_loads_and_safety_factor(run_dir, mooring_modules):
    result = invoke(MOORING_ARGS + ["-o", "mooring.json"])

    assert result.exit_code == 0, result.output
    assert "Vessel:   FPSO" in result.output
    assert "Line:     L=2000.0m, D=0.12m" in result.output
    assert "Loading:  Wind=25.0m/s, Current=1.5m/s" in result.output
    assert "Total Load:     1800.0 kN" in result.output
    assert "Line Tension:   1650.0 kN" in result.output
    assert "Safety Factor:  1.85" in result.output
    # The catenary task receives the horizontal load from the loads task.
    assert mooring_modules[1][1]["horizontal_load"] == 1.5e6
    saved = json.loads((run_dir / "mooring.json").read_text())
    assert saved["outputs"]["mooring_design_status"] == "PASS"
    assert report_path(result.output).is_file()


def test_mooring_design_rejects_unknown_vessel_type(run_dir):
    result = invoke(
        ["mooring-design", "--vessel-type", "barge", "--water-depth", "100",
         "--line-length", "300"]
    )

    assert result.exit_code == 2
    assert "Invalid value for '--vessel-type'" in result.output


def test_structural_check_reports_stress_utilization_and_status(
    run_dir, structural_modules
):
    result = invoke(STRUCTURAL_ARGS + ["--material", "s420", "-o", "plate.json"])

    assert result.exit_code == 0, result.output
    assert "Material: S420" in result.output
    assert "Applied Stress: 200.0 MPa" in result.output
    assert "Von Mises:      210.5 MPa" in result.output
    assert "Utilization:    59.3%" in result.output
    assert "Status:         ADEQUATE" in result.output
    stress_inputs = structural_modules[0][1]
    assert stress_inputs == {
        "sigma_x": 200.0,
        "sigma_y": 100.0,
        "tau_xy": pytest.approx(60.0),
        "material": "s420",
    }
    assert structural_modules[1][1]["applied_stress"] == 210.5
    saved = json.loads((run_dir / "plate.json").read_text())
    assert saved["outputs"]["structural_safety_margin"] == 0.4
    assert report_path(result.output).is_file()
