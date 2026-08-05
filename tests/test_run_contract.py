"""Unit coverage for the run contract: verdict -> exit status, and the sidecar.

Issue #1631. The licensed lane reported ``returncode: 0`` for twelve runs, two
of which carried a validator ``FAIL``. This module pins the mapping that makes
a refusal expressible, the closed verdict vocabulary, and the structured
sidecar that replaces regexing ``stdout_tail``.

Every test here runs on an unlicensed host and none of them depends on whether
``OrcFxAPI`` imports on the runner.
"""

from __future__ import annotations

import json
from pathlib import Path

import pytest


def test_run_verdict_rejects_unknown_verdict_string() -> None:
    """The vocabulary is closed at five values; ``OK`` is not one of them."""
    from digitalmodel.run_contract import RunVerdict

    with pytest.raises(ValueError, match="OK"):
        RunVerdict(verdict="OK", workflow="diffraction", artifacts=["a.json"])


def test_fail_verdict_exit_status_is_one() -> None:
    """D3: ``FAIL`` refuses. This is the behaviour the twelve runs lacked."""
    from digitalmodel.run_contract import RunVerdict, exit_status_for

    verdict = RunVerdict(
        verdict="FAIL", workflow="diffraction", artifacts=["results.json"]
    )

    assert exit_status_for(verdict) == 1


def test_error_verdict_exit_status_is_one() -> None:
    """D3: ``ERROR`` refuses alongside ``FAIL``."""
    from digitalmodel.run_contract import RunVerdict, exit_status_for

    verdict = RunVerdict(
        verdict="ERROR", workflow="diffraction", artifacts=["results.json"]
    )

    assert exit_status_for(verdict) == 1


def test_warning_verdict_exit_status_is_zero() -> None:
    """D3 inverse-defect guard: five of the seven validated runs are WARNING.

    A gate that also refused WARNING would read as rigorous and make the lane
    unusable. It must not fire here.
    """
    from digitalmodel.run_contract import RunVerdict, exit_status_for

    verdict = RunVerdict(
        verdict="WARNING", workflow="diffraction", artifacts=["results.json"]
    )

    assert exit_status_for(verdict) == 0


def test_pass_verdict_exit_status_is_zero() -> None:
    from digitalmodel.run_contract import RunVerdict, exit_status_for

    verdict = RunVerdict(
        verdict="PASS", workflow="diffraction", artifacts=["results.json"]
    )

    assert exit_status_for(verdict) == 0


def test_completed_run_with_no_artifacts_is_refusal() -> None:
    """D10: reproduces ``lr_acma_ff132001b7ad``.

    That run reported ``returncode: 0`` with no ``returned_files`` key at all.
    A run that produced nothing must not pass.
    """
    from digitalmodel.run_contract import RunVerdict

    verdict = RunVerdict(verdict="PASS", workflow="orcaflex_post_process", artifacts=[])

    assert verdict.is_refusal() is True


def test_skipped_verdict_with_no_artifacts_is_not_refusal() -> None:
    """D4: an explicitly requested dry run declares SKIPPED and produces no
    solve artifacts. Unlicensed development must stay possible."""
    from digitalmodel.run_contract import RunVerdict

    verdict = RunVerdict(verdict="SKIPPED", workflow="diffraction", artifacts=[])

    assert verdict.is_refusal() is False


def test_run_verdict_sidecar_is_written_and_parseable(tmp_path: Path) -> None:
    """The sidecar must exist, be non-empty, and parse. An absent or empty file
    satisfies nothing."""
    from digitalmodel.run_contract import RunVerdict, write_run_verdict

    verdict = RunVerdict(
        verdict="WARNING", workflow="diffraction", artifacts=["results.json"]
    )

    written = write_run_verdict(verdict, tmp_path)

    assert json.loads(written.read_text())["verdict"] == "WARNING"


def test_run_verdict_sidecar_is_named_run_verdict_json(tmp_path: Path) -> None:
    from digitalmodel.run_contract import RunVerdict, write_run_verdict

    verdict = RunVerdict(verdict="PASS", workflow="diffraction", artifacts=["r.json"])

    written = write_run_verdict(verdict, tmp_path)

    assert written == tmp_path / "run_verdict.json"


def test_sidecar_records_solver_unavailable_on_this_host(tmp_path: Path) -> None:
    """D9: provenance is recorded positively, in one field, with no log parsing.

    ``OrcFxAPI`` is a Windows-only wheel behind an optional extra, so on this
    Linux host the honest answer is False.
    """
    from digitalmodel.run_contract import RunVerdict, write_run_verdict

    verdict = RunVerdict(
        verdict="SKIPPED",
        workflow="diffraction",
        solver_available=False,
        artifacts=[],
    )

    written = write_run_verdict(verdict, tmp_path)

    assert json.loads(written.read_text())["solver_available"] is False


def test_host_kind_is_unlicensed_when_solver_unavailable() -> None:
    from digitalmodel.run_contract import RunVerdict

    verdict = RunVerdict(
        verdict="SKIPPED", workflow="diffraction", solver_available=False
    )

    assert verdict.host_kind == "unlicensed"


def test_host_kind_is_licensed_when_solver_available() -> None:
    """The provenance field must be capable of reporting a licensed host, or it
    is not evidence of anything."""
    from digitalmodel.run_contract import RunVerdict

    verdict = RunVerdict(
        verdict="PASS",
        workflow="diffraction",
        solver_available=True,
        artifacts=["r.json"],
    )

    assert verdict.host_kind == "licensed"


def test_from_cfg_returns_none_when_workflow_declares_no_verdict() -> None:
    """Back-compat: an un-migrated workflow declares no verdict and must keep
    exiting 0."""
    from digitalmodel.run_contract import from_cfg

    assert from_cfg({"basename": "vertical_riser", "vertical_riser": {}}) is None


def test_from_cfg_reads_verdict_from_the_basename_section() -> None:
    from digitalmodel.run_contract import from_cfg

    cfg = {
        "basename": "diffraction",
        "diffraction": {
            "validation_verdict": "FAIL",
            "outputs": {"diffraction_results_json": "r.json"},
        },
    }

    assert from_cfg(cfg).verdict == "FAIL"


def test_from_cfg_carries_solver_availability_through() -> None:
    from digitalmodel.run_contract import from_cfg

    cfg = {
        "basename": "diffraction",
        "diffraction": {
            "validation_verdict": "PASS",
            "solver_available": False,
            "outputs": {"diffraction_results_json": "r.json"},
        },
    }

    assert from_cfg(cfg).solver_available is False
