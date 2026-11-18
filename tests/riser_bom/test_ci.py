# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Execute the hosted selectors and check BOM test-domain ownership."""

import shlex
from pathlib import Path

import pytest
import yaml

from scripts.ci.detect_touched_domains import parse_domains, touched_domains


ROOT = Path(__file__).resolve().parents[2]


@pytest.mark.parametrize(
    "filename", ["quality-gates-by-domain.yml", "full-matrix-sweep.yml"]
)
def test_subsea_command_includes_bom_without_removing_tests(
    filename, monkeypatch, capsys
):
    workflow = yaml.safe_load(
        (ROOT / ".github/workflows" / filename).read_text(encoding="utf-8")
    )
    gates = yaml.safe_load(
        (ROOT / ".claude/quality-gates.yaml").read_text(encoding="utf-8")
    )["gates"]
    step = next(
        step
        for job in workflow["jobs"].values()
        for step in job.get("steps", [])
        if step.get("name") == "Read domain gate command"
    )
    code = step["run"].split("<<'PY'\n", 1)[1].split("\nPY", 1)[0]
    monkeypatch.chdir(ROOT)
    for name, gate in gates.items():
        if not name.startswith("tests-") or "command" not in gate:
            continue
        monkeypatch.setenv("GATE_NAME", name)
        exec(compile(code, "<workflow command selection>", "exec"), {})
        command = shlex.split(capsys.readouterr().out.strip())
        original = shlex.split(gate["command"])
        assert command[: len(original)] == original
        if name == "tests-subsea":
            assert command.count("tests/riser_bom/") == 1
            if filename == "quality-gates-by-domain.yml":
                assert "tests/riser_fatigue/" in command
        else:
            assert command == original


@pytest.mark.parametrize(
    "path,expected",
    [
        ("src/digitalmodel/riser_bom/costing.py", "all"),
        ("tests/riser_bom/test_costing.py", "subsea"),
        ("tests/workflows/test_ci_triggers.py", "workflows"),
    ],
)
def test_bom_ci_domain_selection(path, expected):
    domains = parse_domains(ROOT / "tests/DOMAINS.md")
    selected = {domain.name for domain in touched_domains([path], domains)}
    # Source edits intentionally retain the detector's full-matrix policy.
    wanted = {domain.name for domain in domains} if expected == "all" else {expected}
    assert selected == wanted
