# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Standalone persistence contract; all inspections and outputs are synthetic."""

from copy import deepcopy
import importlib
import json
import os
from pathlib import Path
import re
import shlex
import subprocess
import sys

import pytest
import yaml

ROOT = Path(__file__).resolve().parents[2]
MODULE = "digitalmodel.asset_integrity.mooring_chain_cli"
EXAMPLE = Path("examples/workflows/mooring-chain-degradation-screen/input.yml")


def documented_input():
    text = (
        ROOT / "docs/domains/asset-integrity/mooring-chain-degradation-screen.md"
    ).read_text(encoding="utf-8")
    match = re.search(r"^```yaml\s*\n(.*?)^```", text, re.MULTILINE | re.DOTALL)
    assert match is not None
    return yaml.safe_load(match.group(1))


def run_cli(tmp_path, *args):
    env = {
        **os.environ,
        "PYTHONPATH": str(ROOT / "src"),
        "PYTHONDONTWRITEBYTECODE": "1",
    }
    return subprocess.run(
        [sys.executable, "-m", MODULE, *map(str, args)],
        cwd=tmp_path,
        env=env,
        capture_output=True,
        text=True,
        encoding="utf-8",
        timeout=30,
        creationflags=subprocess.CREATE_NO_WINDOW if os.name == "nt" else 0,
    )


def write_input(tmp_path, cfg=None, name="input.yml"):
    path = tmp_path / name
    path.write_text(
        yaml.safe_dump(cfg or documented_input(), allow_unicode=True), encoding="utf-8"
    )
    return path


def test_example_matches_document():
    assert (
        yaml.safe_load((ROOT / EXAMPLE).read_text(encoding="utf-8"))
        == documented_input()
    )


def assert_result(output):
    payload = json.loads(
        output.read_text(encoding="utf-8"),
        parse_constant=lambda token: pytest.fail(f"Nonstandard JSON constant: {token}"),
    )
    result = payload["result"]
    assert result["dimensions"]["samples"][0]["status"] == "WITHIN_LIMITS"
    assert result["decision"]["verdict"] == "ESCALATE"
    assert result["remaining_mbl_kn"] is None
    assert result["remaining_fatigue_life_years"] is None
    for key in ("margin", "margin_allowable", "rsf", "rsf_a", "remaining_life_yr"):
        assert result["decision"][key] is None
    assert set(result["mechanisms"]) == {
        "remaining_mbl",
        "pitting",
        "interlink_wear",
        "fatigue",
        "ovalization",
    }
    assert all(
        m["status"] == "NOT_EVALUATED" and m["required_evidence"]
        for m in result["mechanisms"].values()
    )
    assert "report_path" not in payload
    assert output.with_suffix(".html").read_bytes() == payload["report_html"].encode(
        "utf-8"
    )
    return payload


def test_utf8_input_and_report_readback(tmp_path):
    cfg = documented_input()
    cfg["mooring_chain_degradation"]["component_id"] = "synthetic-chain-α"
    source = write_input(tmp_path, cfg)
    original = source.read_bytes()
    output = tmp_path / "new-results" / "screen.json"
    completed = run_cli(tmp_path, source, "--output", output)
    assert completed.returncode == 0, completed.stderr
    payload = assert_result(output)
    assert payload["inspection"] == cfg["mooring_chain_degradation"]["inspection"]
    assert "synthetic-chain-α" in payload["report_html"]
    assert source.read_bytes() == original
    assert sorted(p.name for p in output.parent.iterdir()) == [
        "screen.html",
        "screen.json",
    ]


@pytest.mark.parametrize("suffix", ["json", "html"])
def test_report_bytes_with_windows_newline_translation(tmp_path, monkeypatch, suffix):
    """Exercise each output with the Windows default text newline translation."""
    cli = importlib.import_module(MODULE)
    cfg = documented_input()
    cfg["mooring_chain_degradation"]["component_id"] = "synthetic-chain-α"
    source = write_input(tmp_path, cfg)
    output = tmp_path / "results" / "screen.json"
    translated = output.with_suffix(f".{suffix}")
    real_open = Path.open

    def windows_open(
        path, mode="r", buffering=-1, encoding=None, errors=None, newline=None
    ):
        if path == translated and "b" not in mode and any(c in mode for c in "xwa"):
            if newline is None:
                newline = "\r\n"
        return real_open(path, mode, buffering, encoding, errors, newline)

    monkeypatch.setattr(Path, "open", windows_open)
    assert cli.main([str(source), "--output", str(output)]) == 0
    payload = assert_result(output)
    assert output.read_bytes() == (
        json.dumps(payload, ensure_ascii=False, allow_nan=False, indent=2) + "\n"
    ).encode("utf-8")


def test_workflow_cli_and_registry(tmp_path):
    registry = yaml.safe_load(
        (ROOT / "docs/registry/workflows.yaml").read_text(encoding="utf-8")
    )
    rows = [
        r
        for r in registry["workflows"]
        if r["id"] == "mooring-chain-degradation-screen"
    ]
    assert len(rows) == 1
    row = rows[0]
    assert (row["version"], row["status"], row["latest"], row["runtime"]) == (
        1,
        "experimental",
        False,
        "standalone",
    )
    assert row["basename"] == "mooring_chain_degradation" and row["title"]
    assert (
        row["test"]
        == "tests/asset_integrity/test_mooring_chain_cli.py::test_workflow_cli_and_registry"
    )
    assert row["input"] == EXAMPLE.as_posix()
    source = tmp_path / row["input"]
    source.parent.mkdir(parents=True)
    source.write_bytes((ROOT / row["input"]).read_bytes())
    command = shlex.split(row["invocation"].format(input=row["input"]))
    assert command[:6] == ["uv", "run", "python", "-m", MODULE, row["input"]]
    assert command[6:] == ["--output", row["outputs"][0]]
    assert row["outputs"] == [
        (EXAMPLE.parent / "results" / f"screen.{suffix}").as_posix()
        for suffix in ("json", "html")
    ]
    completed = run_cli(tmp_path, *command[5:])
    assert completed.returncode == 0, completed.stderr
    assert_result(tmp_path / row["outputs"][0])


@pytest.mark.parametrize("args", [[], ["input.yml"], ["--output", "screen.json"]])
def test_missing_arguments(tmp_path, args):
    completed = run_cli(tmp_path, *args)
    assert completed.returncode != 0
    assert "required" in completed.stderr
    assert list(tmp_path.iterdir()) == []


@pytest.mark.parametrize(
    "text", ["[broken", "[]", "null", "mooring_chain_degradation: {}"]
)
def test_malformed_input(tmp_path, text):
    source = tmp_path / "input.yml"
    source.write_text(text, encoding="utf-8")
    output = tmp_path / "results" / "screen.json"
    completed = run_cli(tmp_path, source, "--output", output)
    assert completed.returncode != 0 and "error:" in completed.stderr
    assert source.read_text(encoding="utf-8") == text
    assert not output.parent.exists()


@pytest.mark.parametrize(
    "field",
    [
        "min_area_ratio",
        "max_diameter_loss_fraction",
        "operator_limit_reference",
        "uncertainty_mm",
    ],
)
def test_missing_explicit_criteria(tmp_path, field):
    cfg = documented_input()
    inspection = cfg["mooring_chain_degradation"]["inspection"]
    del (inspection["readings"][0] if field == "uncertainty_mm" else inspection)[field]
    source = write_input(tmp_path, cfg)
    completed = run_cli(
        tmp_path, source, "--output", tmp_path / "results" / "screen.json"
    )
    assert completed.returncode != 0 and "error:" in completed.stderr
    assert not (tmp_path / "results").exists()


@pytest.mark.parametrize("suffix", ["json", "html"])
def test_input_output_collision(tmp_path, suffix):
    source = write_input(tmp_path, name=f"screen.{suffix}")
    original = source.read_bytes()
    completed = run_cli(tmp_path, source, "--output", tmp_path / "screen.json")
    assert completed.returncode != 0 and "error:" in completed.stderr
    assert source.read_bytes() == original
    assert len(list(tmp_path.iterdir())) == 1


@pytest.mark.parametrize("suffix", ["json", "html"])
def test_existing_output_refused(tmp_path, suffix):
    source = write_input(tmp_path)
    existing = tmp_path / f"screen.{suffix}"
    existing.write_bytes(b"preserve existing output")
    completed = run_cli(tmp_path, source, "--output", tmp_path / "screen.json")
    assert completed.returncode != 0 and "error:" in completed.stderr
    assert existing.read_bytes() == b"preserve existing output"
    assert len(list(tmp_path.iterdir())) == 2


def test_invalid_output_extension(tmp_path):
    source = write_input(tmp_path)
    completed = run_cli(
        tmp_path, source, "--output", tmp_path / "results" / "screen.yml"
    )
    assert completed.returncode != 0 and "error:" in completed.stderr
    assert not (tmp_path / "results").exists()


def test_router_once_and_strict_serialization(tmp_path, monkeypatch):
    cli = importlib.import_module(MODULE)
    source = write_input(tmp_path)
    output = tmp_path / "results" / "screen.json"
    calls = []
    real_router = cli.router

    def nonfinite_router(cfg):
        calls.append(deepcopy(cfg))
        response = real_router(cfg)
        response["mooring_chain_degradation"]["result"]["remaining_mbl_kn"] = float(
            "nan"
        )
        return response

    monkeypatch.setattr(cli, "router", nonfinite_router)
    with pytest.raises(SystemExit) as error:
        cli.main([str(source), "--output", str(output)])
    assert error.value.code != 0
    assert calls == [documented_input()]
    assert not output.parent.exists()
