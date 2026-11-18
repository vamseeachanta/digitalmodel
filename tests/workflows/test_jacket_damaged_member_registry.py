# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Discover and execute the experimental jacket screen in an isolated tree."""

import json
import os
from pathlib import Path
import shlex
import subprocess
import sys

import pytest
import yaml

ROOT = Path(__file__).resolve().parents[2]
WORKFLOW = "jacket-damaged-member-screen"
EXAMPLE = f"examples/workflows/{WORKFLOW}"
MODULE = "digitalmodel.structural.jacket_topside.damaged_member_screen"
TEST = "tests/workflows/test_jacket_damaged_member_registry.py"


def _registered_screen():
    registry = yaml.safe_load(
        (ROOT / "docs/registry/workflows.yaml").read_text(encoding="utf-8")
    )
    rows = [row for row in registry["workflows"] if row["id"] == WORKFLOW]
    assert len(rows) == 1, "expected exactly one experimental jacket screen"
    return rows[0]


def test_registry_contract():
    row = _registered_screen()
    assert row["basename"] == "damaged_member_screen"
    assert isinstance(row["title"], str) and row["title"].strip()
    assert type(row["version"]) is int and row["version"] == 1
    assert row["status"] == "experimental"
    assert row["latest"] is False
    assert row["runtime"] == "standalone"
    assert row["input"] == f"{EXAMPLE}/input.yml"
    assert (ROOT / row["input"]).is_file()
    assert row["invocation"] == (
        f"uv run python -m {MODULE} {{input}} "
        f"--output {EXAMPLE}/results/screen.json"
    )
    assert row["outputs"] == [
        f"{EXAMPLE}/results/screen.json",
        f"{EXAMPLE}/results/screen.html",
    ]
    assert row["test"] == f"{TEST}::test_workflow_cli_and_registry"


def test_workflow_cli_and_registry(tmp_path):
    row = _registered_screen()
    source = ROOT / row["input"]
    original = source.read_bytes()
    isolated_input = tmp_path / row["input"]
    isolated_input.parent.mkdir(parents=True)
    isolated_input.write_bytes(original)

    invocation = shlex.split(row["invocation"].format(input=row["input"]))
    assert invocation[:3] == ["uv", "run", "python"]
    # Reuse the active test environment; run all registered paths under tmp_path.
    environment = os.environ.copy()
    environment["PYTHONPATH"] = os.pathsep.join(
        filter(None, [str(ROOT / "src"), environment.get("PYTHONPATH")])
    )
    completed = subprocess.run(
        [sys.executable, *invocation[3:]],
        cwd=tmp_path,
        env=environment,
        capture_output=True,
        text=True,
        timeout=60,
        check=False,
        creationflags=subprocess.CREATE_NO_WINDOW if sys.platform == "win32" else 0,
    )
    assert completed.returncode == 0, completed.stderr
    assert source.read_bytes() == original
    assert isolated_input.read_bytes() == original

    output, html_output = [tmp_path / path for path in row["outputs"]]
    result = json.loads(
        output.read_text(encoding="utf-8"),
        parse_constant=lambda value: pytest.fail(f"non-finite JSON constant: {value}"),
    )
    assert html_output == output.with_suffix(".html")
    assert html_output.read_text(encoding="utf-8") == result["report_html"]
    assert result["workflow"] == WORKFLOW
    assert len(result["members"]) == 3
    assert result["verdict"] == "ESCALATE"
    assert result["engineering_acceptance_established"] is False
    for member in result["members"]:
        assert member["decision"]["verdict"] == "ESCALATE"
        assert member["engineering_acceptance_established"] is False
    handoffs = [member["handoff"] for member in result["members"] if member["handoff"]]
    assert len(handoffs) == 1
    assert handoffs[0]["solver_executed"] is False
    assert {path for path in tmp_path.rglob("*") if path.is_file()} == {
        isolated_input, output, html_output,
    }
