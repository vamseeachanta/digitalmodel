# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Retained-ancestry guard; source archives cannot qualify Git provenance."""
import importlib.util
import os
from pathlib import Path
import shlex
import subprocess
import sys

import pytest
import yaml

ROOT = Path(__file__).resolve().parents[2]
spec = importlib.util.spec_from_file_location(
    "atlas_revision_inventory", ROOT / "scripts/provenance/inventory_atlas_manifests.py",
)
inventory = importlib.util.module_from_spec(spec)
spec.loader.exec_module(inventory)
EXCEPTIONS = Path(__file__).with_name("atlas_manifest_revision_exceptions.yaml")


def test_atlas_manifest_revisions():
    report = inventory.inventory_manifests(ROOT)
    if report["history_state"] == "SHALLOW":
        pytest.skip("shallow clone: complete Git history required for manifest ancestry")
    exceptions = yaml.safe_load(EXCEPTIONS.read_text(encoding="utf-8"))
    failures = inventory.manifest_revision_failures(report, exceptions)
    assert not failures, "\n".join(failures)
    print(f"history={report['history_state']}; counts={report['counts']}; "
          f"explicit historical exceptions={len(exceptions)}")


def test_guard_rejects_new_unknown_revision_and_exactly_scopes_exception():
    """One synthetic unknown object fails unless that exact path/hash is listed."""
    row = {"manifest": "atlases/toy/id/manifest.yaml", "code_version": "abcdef",
           "status": "UNAVAILABLE", "reason": "revision_unknown"}
    report = {"history_state": "COMPLETE", "manifests": [row]}
    assert inventory.manifest_revision_failures(report, {})
    exception = {"code_version": "abcdef", "status": "UNAVAILABLE",
                 "inventory_reason": "revision_unknown", "reason": "Historical object absent."}
    assert not inventory.manifest_revision_failures(report, {row["manifest"]: exception})
    row["code_version"] = "fedcba"
    assert inventory.manifest_revision_failures(report, {row["manifest"]: exception})


@pytest.mark.parametrize("state", ("SOURCE_ARCHIVE", "RETAINED_REF_UNAVAILABLE", "SHALLOW"))
def test_missing_history_cannot_be_allow_listed(state):
    report = {"history_state": state, "manifests": []}
    assert inventory.manifest_revision_failures(report, {})


def test_guard_rejects_unused_exceptions_and_empty_inventory():
    report = {"history_state": "COMPLETE", "manifests": [{
        "manifest": "atlases/toy/id/manifest.yaml", "status": "REACHABLE",
    }]}
    assert inventory.manifest_revision_failures(report, {"old/path": {}})
    report["manifests"] = []
    report["search_pattern"] = inventory.SEARCH_PATTERN
    assert inventory.manifest_revision_failures(report, {})


@pytest.mark.parametrize("path", sorted(Path(__file__).parent.glob("test_*.py")),
                         ids=lambda path: path.name)
def test_provenance_modules_run_in_contracts_shard(path):
    """Every provenance test must select a shard whose command runs its root."""
    spec = importlib.util.spec_from_file_location(
        "provenance_domain_detector", ROOT / "scripts/ci/detect_touched_domains.py",
    )
    detector = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = detector
    spec.loader.exec_module(detector)
    domains = detector.parse_domains(ROOT / "tests/DOMAINS.md")
    selected = detector.touched_domains([path.relative_to(ROOT).as_posix()], domains)
    assert {domain.name for domain in selected} == {"contracts"}
    gate = yaml.safe_load((ROOT / ".claude/quality-gates.yaml").read_text(encoding="utf-8"))[
        "gates"]["tests-contracts"]
    assert gate["enabled"] and gate["failure_action"] == "block"
    assert "tests/provenance/" in gate["test_roots"]
    assert "tests/provenance/" in shlex.split(gate["command"])


@pytest.mark.parametrize("status, reason", (
    ("UNAVAILABLE", "ancestry_check_unavailable"),
    ("UNAVAILABLE", "manifest_invalid"),
    ("UNAVAILABLE", "revision_invalid"),
    ("UNREACHABLE", "revision_unknown"),
))
def test_debt_never_allows_operational_or_invalid_evidence(status, reason):
    path = "atlases/toy/id/manifest.yaml"
    row = {"manifest": path, "code_version": "abcdef", "status": status, "reason": reason}
    exception = {"code_version": "abcdef", "status": "UNREACHABLE",
                 "inventory_reason": "not_ancestor_of_retained_ref", "reason": "Historical debt."}
    report = {"history_state": "COMPLETE", "manifests": [row]}
    assert inventory.manifest_revision_failures(report, {path: exception})


def test_contracts_command_expands_dependency_namespace():
    """Construct shell arguments without executing the package installer."""
    command = yaml.safe_load((ROOT / ".claude/quality-gates.yaml").read_text(encoding="utf-8"))[
        "gates"]["tests-contracts"]["command"]
    words = subprocess.check_output(
        ["bash", "-c", "set -- " + command + "\nprintf '%s\\n' \"$@\""],
        env={**os.environ, "GITHUB_REPOSITORY_OWNER": "source-namespace"},
        text=True, encoding="utf-8",
    ).splitlines()
    assert words[words.index("--with") + 1] == (
        "assetutilities @ git+https://github.com/source-namespace/assetutilities.git@main"
    )
