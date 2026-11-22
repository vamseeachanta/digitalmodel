"""The daily sweep must run each PR shard's gates and their prerequisites."""

import re
from pathlib import Path

import pytest

from scripts.ci.detect_touched_domains import parse_domains
from tests.workflows.test_ci_triggers import load_workflow


ROOT = Path(__file__).resolve().parents[2]
PR = load_workflow("quality-gates-by-domain.yml")["jobs"]["domain-tests"]
SWEEP = load_workflow("full-matrix-sweep.yml")["jobs"]["sweep-tests"]


def applies_to_shard(condition, domain):
    if condition in (None, "always()"):
        return True
    # Recognize only the shard predicates used here. A new expression needs
    # explicit handling so that an unknown condition cannot hide a test.
    alternatives = condition.split(" || ")
    matches = [re.fullmatch(r"matrix\.domain == '([\w-]+)'", term) for term in alternatives]
    assert all(matches), f"unhandled shard step condition: {condition}"
    return any(match[1] == domain for match in matches)


def shard_steps(job, domain, monkeypatch, capsys):
    result = {}
    for step in job["steps"]:
        name = step["name"]
        if name == "Build and guard local pull request merge":
            continue
        if not applies_to_shard(step.get("if"), domain):
            continue
        step = step.copy()
        if name == "Checkout code" and job is PR:
            # The PR prepares a local merge. Both paths need the same history.
            step["with"] = {k: v for k, v in step["with"].items() if k not in ("repository", "ref")}
        if step.get("if", "").startswith("matrix.domain"):
            del step["if"]  # Applicability was evaluated for this domain.
        if step.get("id") == "gate":
            # Exercise the actual resolver, including any workflow-only paths.
            before, rest = step["run"].split("<<'PY'\n")
            code, after = rest.split("\nPY\n")
            monkeypatch.chdir(ROOT)
            monkeypatch.setenv("GATE_NAME", f"tests-{domain}")
            exec(compile(code, "<domain gate reader>", "exec"), {})
            command = capsys.readouterr().out.strip()
            assert command, f"empty gate command for {domain}"
            step["run"] = (before, command, after)
        if name == "Upload domain artifacts":
            step["with"] = {k: v for k, v in step["with"].items() if k != "name"}
        assert name not in result, f"duplicate shard step: {name}"
        result[name] = step
    return result


@pytest.mark.parametrize("domain", [d.name for d in parse_domains(ROOT / "tests/DOMAINS.md")])
def test_sweep_includes_pr_shard_steps(domain, monkeypatch, capsys):
    for field in ("runs-on", "env", "defaults"):
        assert PR.get(field) == SWEEP.get(field)
    pr = shard_steps(PR, domain, monkeypatch, capsys)
    sweep = shard_steps(SWEEP, domain, monkeypatch, capsys)
    assert [name for name in sweep if name in pr] == list(pr)
    for name in pr:
        assert sweep[name] == pr[name], f"{domain}: {name}"
