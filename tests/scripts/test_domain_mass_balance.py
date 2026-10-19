# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Synthetic arithmetic and log fixtures; no solver is invoked.

Hand calculation: on a fixed domain, water volume is f*V_domain, so
relative drift is abs(f-f_initial)/f_initial. For [0.5, 0.625, 0.5],
the maximum is 0.125/0.5 = 0.25 even though the endpoint drift is zero.
All tolerances below are chosen test inputs, not engineering acceptance limits.
"""

import importlib.util
import json
import os
from pathlib import Path
import subprocess
import sys

import pytest

ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts/cfd/domain_mass_balance.py"
SPEC = importlib.util.spec_from_file_location("domain_mass_balance", SCRIPT)
balance = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(balance)


def log_text(values):
    """Use the native print format in OpenFOAM v2312 alphaEqn.H, lines 124–128."""
    return "\n".join(
        f"Time = {i}\nPhase-1 volume fraction = {v}  "
        "Min(alpha.water) = 0 Max(alpha.water) = 1"
        for i, v in enumerate(values)
    )


@pytest.mark.parametrize(
    ("values", "limit", "status", "maximum"),
    [
        ([0.5, 0.5], 0, "PASS", 0),
        ([0.5, 0.625], 0.25, "PASS", 0.25),
        ([0.5, 0.625, 0.5], 0.125, "FAIL", 0.25),
        ([0.5, 0.375], 0.125, "FAIL", 0.25),
        ([0.5, 0], 0.5, "FAIL", 1),
    ],
)
def test_maximum_drift(values, limit, status, maximum):
    """Loss is 0.125/0.5=0.25; complete drainage is 0.5/0.5=1."""
    result = balance.evaluate_drift(values, limit)
    assert result["status"] == status
    assert result["max_relative_drift"] == maximum
    assert result["sample_count"] == len(values)


@pytest.mark.parametrize(
    "values",
    [[], [0.5], [0, 0.1], [-0.1, 0.5], [0.5, 1.1],
     [0.5, float("nan")], [0.5, float("inf")]],
)
def test_invalid_evidence_cannot_pass(values):
    assert balance.evaluate_drift(values, 0.125)["status"] == "NOT_EVALUATED"


@pytest.mark.parametrize("limit", [None, -1, float("nan"), float("inf")])
def test_tolerance_is_explicit_and_finite(limit):
    assert balance.evaluate_drift([0.5, 0.5], limit)["status"] == "NOT_EVALUATED"


def test_parser_scientific_notation_and_recovery():
    """5e-1 to 6.25e-1 is (0.625-0.5)/0.5=0.25, then recovery."""
    result = balance.assess_log(
        log_text(["5e-1", "6.25e-1", "5e-1"]).splitlines(), 0.125,
        fixed_domain=True,
    )
    assert result["status"] == "FAIL"
    assert result["max_relative_drift"] == 0.25
    assert result["initial_fraction"] == 0.5
    assert result["final_fraction"] == 0.5


@pytest.mark.parametrize("value", ["bad", "nan", "inf", "0.5junk", ""])
def test_malformed_record_is_not_silently_skipped(value):
    text = log_text([0.5, value, 0.5])
    result = balance.assess_log(text.splitlines(), 0.125, fixed_domain=True)
    assert result["status"] == "NOT_EVALUATED"
    assert "record" in result["reason"]


def test_fixed_domain_requires_confirmation():
    result = balance.assess_log(log_text([0.5, 0.5]).splitlines(), 0.125)
    assert result["status"] == "NOT_EVALUATED"
    assert "fixed domain" in result["reason"]


def test_restart_time_reset_is_not_combined():
    text = log_text([0.5, 0.5]) + "\n" + log_text([0.5, 0.5])
    result = balance.assess_log(text.splitlines(), 0.125, fixed_domain=True)
    assert result["status"] == "NOT_EVALUATED"
    assert "time" in result["reason"]


@pytest.mark.parametrize("record", ["Time =", "Time = nan", "Time = bad"])
def test_malformed_time_is_not_ignored(record):
    text = log_text([0.5, 0.5]) + "\n" + record
    assert balance.assess_log(
        text.splitlines(), 0.125, fixed_domain=True,
    )["status"] == "NOT_EVALUATED"


def test_first_phase_must_be_identified_as_water():
    text = log_text([0.5, 0.5]).replace("alpha.water", "alpha.air")
    result = balance.assess_log(text.splitlines(), 0.125, fixed_domain=True)
    assert result["status"] == "NOT_EVALUATED"
    assert "water" in result["reason"]


def test_all_corrector_records_at_one_time_are_retained():
    """A corrector excursion to 0.625 still has relative drift 0.25."""
    text = log_text([0.5, 0.625, 0.5]).replace("Time = 1", "Time = 0")
    result = balance.assess_log(text.splitlines(), 0.125, fixed_domain=True)
    assert result["status"] == "FAIL"
    assert result["max_relative_drift_sample"] == 1


def test_invalid_utf8_log_cannot_pass(tmp_path, capsys):
    path = tmp_path / "log.interFoam"
    path.write_bytes(log_text([0.5, 0.5]).encode() + bytes([255]))
    assert balance.main([str(path), "--fixed-domain", "--json"]) == 1
    assert json.loads(capsys.readouterr().out)["status"] == "NOT_EVALUATED"


@pytest.mark.parametrize(
    ("values", "extra", "status", "returncode"),
    [
        ([0.5, 0.5], ["--max-relative-drift", "0.125"], "PASS", 0),
        ([0.5, 0.625, 0.5], ["--max-relative-drift", "0.125"], "FAIL", 1),
        ([0.5, 0.5], [], "NOT_EVALUATED", 1),
    ],
)
def test_cli_json(tmp_path, values, extra, status, returncode):
    path = tmp_path / "log.interFoam"
    path.write_text(log_text(values))
    result = subprocess.run(
        [sys.executable, str(SCRIPT), str(path), "--fixed-domain", "--json", *extra],
        capture_output=True, text=True, check=False,
    )
    assert result.returncode == returncode
    payload = json.loads(result.stdout)
    assert payload["status"] == status
    assert payload["tolerance_source"] == "caller input"
    assert "alphaEqn" in payload["quantity_source"]


def test_missing_log_cannot_pass(tmp_path, capsys):
    assert balance.main([str(tmp_path / "missing"), "--json"]) == 1
    assert json.loads(capsys.readouterr().out)["status"] == "NOT_EVALUATED"


@pytest.mark.parametrize(
    ("values", "fixed", "limit", "status"),
    [
        ([0.5, 0.625, 0.5], "true", "0.125", "FAIL"),
        ([0.5, 0.5], "true", "0.125", "PASS"),
        ([0.5, 0.5], "false", "0.125", "NOT_EVALUATED"),
        ([0.5, 0.5], "true", "", "NOT_EVALUATED"),
    ],
)
def test_shell_gate_uses_same_assessment(tmp_path, values, fixed, limit, status):
    """No force file is supplied, so the shell never queries a solver tool."""
    case = tmp_path / "cases" / "example"
    case.mkdir(parents=True)
    (case / "log.interFoam").write_text(log_text(values))
    env = dict(os.environ, DM_CFD_ROOT=str(tmp_path), DM_CFD_CAMPAIGN="example",
               DM_CFD_FIXED_DOMAIN=fixed, DM_CFD_MASS_DRIFT_RTOL=limit)
    result = subprocess.run(
        ["bash", str(ROOT / "scripts/cfd/fs_gate.sh"), "example"],
        env=env, capture_output=True, text=True, check=False,
    )
    assert f"MASS BALANCE        : {status}" in result.stdout
    assert "SETTLING GATE" in result.stdout
    assert "NOT USABLE" in result.stdout
    assert result.returncode == 1  # missing force evidence prevents overall acceptance
