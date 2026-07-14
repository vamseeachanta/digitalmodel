"""Regression tests for the Step 5 adversarial review findings."""

import json
from dataclasses import replace
from datetime import datetime, timezone
from pathlib import Path
from types import MappingProxyType
from unittest.mock import Mock
from urllib.parse import quote

import pytest

from digitalmodel.workflows import openfoam_batch_execution as execution
from digitalmodel.workflows import openfoam_batch_results as results
from digitalmodel.workflows import openfoam_run_batch as facade


class RecordingOutput:
    def __init__(self):
        self.files = {}

    def write(self, name, data):
        self.files[name] = data


def _row(**changes):
    row = {"index": 0, "name": "case-a", "status": "completed"}
    row.update(changes)
    return row


def _times():
    stamp = datetime(2026, 1, 2, tzinfo=timezone.utc)
    return stamp, stamp


def test_external_redaction_is_recursive_root_aware_and_alias_complete():
    root = Path("/private/operator/openfoam")
    percent_root = quote(str(root), safe="")
    unicode_root = str(root).replace("/", r"\u002f")
    json_root = str(root).replace("/", r"\/")
    row = _row(
        stdout_tail="private stdout diagnostic",
        std_out="second stdout diagnostic",
        stderrTail="private stderr diagnostic",
        tail="private tail diagnostic",
        argv=(str(root / "bin/simpleFoam"), "--token=secret"),
        nested={
            str(root / "mapping-key"): (
                {f"percent {percent_root}": [f"at {percent_root}%2Frun"]},
                {f"unicode {unicode_root}", f"json {json_root}"},
            ),
        },
    )

    redacted = results.redact_external_row(row, root)
    encoded = json.dumps(redacted, sort_keys=True)

    for secret in (
        str(root), percent_root, unicode_root, json_root, "private stdout",
        "second stdout", "private stderr", "private tail", "--token=secret",
    ):
        assert secret not in encoded
    for alias in ("stdout_tail", "std_out", "stderrTail", "tail", "argv"):
        assert redacted[alias] == "<redacted>"


@pytest.mark.parametrize(
    "basename",
    [
        "rho", "p_rgh", "pointDisplacement", "case.vtk", "case.VTU",
        "solver.log", "Processor0", "checkpoint.json", "vtk", "polyMesh.gz",
        "LOG.simpleFoam.gz", "processor0.tar", "0.orig", "uniform/time",
    ],
)
def test_closed_result_allowlist_rejects_every_non_policy_basename(basename):
    with pytest.raises(ValueError, match="result basename"):
        results.validate_result_basename(basename)


def test_closed_result_allowlist_contains_only_mandatory_and_active_records():
    assert results.allowed_result_basenames() == frozenset(
        {"cases.csv", "batch_summary.json"}
    )
    assert results.validate_result_basename("cases.csv") == "cases.csv"
    assert results.validate_result_basename("batch_summary.json") == "batch_summary.json"
    with pytest.raises(ValueError, match="result basename"):
        results.validate_result_basename("artifact_index.json")


def test_active_registry_record_must_have_a_safe_unique_nonmandatory_basename(
    monkeypatch,
):
    record = results.RESULT_EXTENSION_REGISTRY["openfoam-artifact-index-v1"]
    unsafe = replace(record, basename="solver.log", active=True)
    monkeypatch.setattr(
        results,
        "RESULT_EXTENSION_REGISTRY",
        MappingProxyType({unsafe.extension_id: unsafe}),
    )
    with pytest.raises(ValueError, match="registered result basename"):
        results.active_result_extensions()


def test_external_writer_enforces_allowlist_at_publication(monkeypatch):
    validate = Mock(wraps=results.validate_result_basename)
    monkeypatch.setattr(results, "validate_result_basename", validate)
    output = RecordingOutput()
    started, finished = _times()

    results.write_external_results(
        output, [_row()], "pool", 1, True, 3, started, finished
    )

    assert [call.args[0] for call in validate.call_args_list] == [
        "cases.csv", "batch_summary.json"
    ]


def test_result_policy_config_is_ignored_for_legacy_context(monkeypatch):
    cfg = {
        "openfoam_run_batch": {"run_batch": {"result_extensions": ["future-v1"]}}
    }
    monkeypatch.setattr(facade, "_prepare_batch", lambda _cfg: {"layout": None})
    monkeypatch.setattr(facade, "_execute_batch", lambda _batch: ([], *_times()))
    monkeypatch.setattr(facade, "_finalize_batch", lambda cfg, *_args: cfg)
    assert facade.router(cfg) is cfg


def test_command_failure_logging_is_external_only_and_root_aware(
    tmp_path, monkeypatch
):
    root = Path("/private/operator")
    executable = root / "bin/simpleFoam"
    encoded = quote(str(root), safe="")
    external_error = OSError(f"failed at {encoded}%2Frun with private diagnostic")
    error_log = Mock()
    monkeypatch.setattr(execution.logger, "error", error_log)
    monkeypatch.setattr(execution.subprocess, "run", Mock(side_effect=external_error))

    assert execution.run_command(
        [str(executable), "--token=secret"], tmp_path, tmp_path / "log", 3,
        external_root=root,
    ) == 1
    rendered = repr(error_log.call_args)
    assert str(root) not in rendered
    assert encoded not in rendered
    assert "private diagnostic" not in rendered
    assert "--token=secret" not in rendered

    legacy_error = OSError(f"failed at {root}/run with legacy diagnostic")
    error_log.reset_mock()
    execution.subprocess.run.side_effect = legacy_error
    assert execution.run_command(
        [str(executable)], tmp_path, tmp_path / "legacy.log", 3
    ) == 1
    error_log.assert_called_once_with(
        "openfoam_run_batch: {} invocation failed: {}",
        str(executable),
        legacy_error,
    )
