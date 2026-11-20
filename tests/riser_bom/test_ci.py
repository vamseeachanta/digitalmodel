# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Check effective workflow commands and BOM test-domain ownership."""

import shlex
from pathlib import Path

import pytest

from scripts.ci.detect_touched_domains import parse_domains, touched_domains
from scripts.ci.domain_gate_command import effective_command, load_gates


ROOT = Path(__file__).resolve().parents[2]


@pytest.mark.parametrize("workflow", ["quality-gates-by-domain", "full-matrix-sweep"])
def test_subsea_command_includes_bom_without_removing_tests(workflow):
    document = load_gates(ROOT / ".claude/quality-gates.yaml")
    gates = document["gates"]
    original = shlex.split(gates["tests-subsea"]["command"])
    command = shlex.split(effective_command(document, "tests-subsea", workflow))
    assert command[: len(original)] == original
    assert command.count("tests/riser_bom/") == 1
    if workflow == "quality-gates-by-domain":
        assert "tests/riser_fatigue/" in command

    for name, gate in gates.items():
        if name != "tests-subsea" and "command" in gate:
            assert effective_command(document, name, workflow) == gate["command"]


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
