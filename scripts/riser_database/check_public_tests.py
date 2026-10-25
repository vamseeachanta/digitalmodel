# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Run the public module checks and emit outcomes with an audited skip list.

Usage: python scripts/riser_database/check_public_tests.py --output <evidence>
Additional arguments are passed to pytest, including --basetemp=<scratch>.
No private source is opened by this helper; existing tests govern discovery.
"""
from __future__ import annotations

import argparse
import json
from collections import Counter
from pathlib import Path

import pytest

# Exact reasons from the five availability guards on the public baseline.
SKIP_REASONS = {
    "test_citations.py": (
        "amjig-2000 wiki criteria page not available",
    ),
    "test_crosswalk.py": (
        "llm-wiki standards-registry.json not available (standalone mode) — "
        "this suite is part of the in-context merge gate and must run green "
        "in a workspace-hub checkout before the PR",
    ),
    "test_leak_gate.py": (
        "workspace-hub .legal-deny-list.yaml not available (standalone "
        "mode) — this test is the in-context HARD MERGE GATE and must run "
        "green in a workspace-hub checkout before the branch is pushed",
    ),
    "test_provenance_tripwire.py": (
        "llm-wiki not available (standalone mode) — the provenance-token "
        "tripwire is part of the in-context merge gate",
        "no name tokens in the private registry to plant",
    ),
    "test_stackup_rig.py": (
        "llm-wiki stack-up registry not available (standalone mode) — this "
        "reconciliation test is part of the in-context merge gate",
    ),
}


SKIP_TESTS = {
    "test_citations.py": {"test_get_amjig_envelope_criteria_emits_private_wiki_citation"},
    "test_crosswalk.py": {
        "test_registry_stats_sane", "test_every_crosswalk_code_id_resolves_in_registry",
        "test_2rd_uses_the_std_identifier", "test_registry_revision_mirrors_registry",
    },
    "test_leak_gate.py": {"test_layer_b_private_deny_list_scan"},
    "test_provenance_tripwire.py": {
        "test_no_provenance_tokens_in_public_artifacts", "test_scan_would_catch_a_planted_token",
        "test_no_provenance_tokens_in_operability_atlas", "test_no_provenance_tokens_in_drift_off_atlas",
    },
    "test_stackup_rig.py": {
        "test_registry_generic_names_not_republished", "test_registry_reconciliation_bidirectional",
    },
}


def documented_skip(nodeid: str, reason: str) -> bool:
    """Match both the emitting module and the complete reason, without prefixes."""
    path, _, name = nodeid.partition("::")
    module = Path(path).name
    return (name in SKIP_TESTS.get(module, set())
            and reason in SKIP_REASONS.get(module, ()))


class Evidence:
    """Retain failures at any phase and each skipped node's reason."""

    def __init__(self):
        self.passed = set()
        self.failed = set()
        self.skipped = {}
        self.collection_errors = set()

    def pytest_collectreport(self, report):
        if report.failed:
            self.collection_errors.add(report.nodeid)
        elif report.skipped:
            self.skipped[report.nodeid] = report.longrepr[2].removeprefix("Skipped: ")

    def pytest_runtest_logreport(self, report):
        if report.failed:
            self.failed.add(report.nodeid)
        elif report.skipped:
            self.skipped[report.nodeid] = report.longrepr[2].removeprefix("Skipped: ")
        elif report.when == "call":
            self.passed.add(report.nodeid)

    def result(self):
        unexpected = {
            node: reason for node, reason in self.skipped.items()
            if not documented_skip(node, reason)
        }
        return {
            "passed": len(self.passed - self.failed),
            "failed": len(self.failed),
            "collection_errors": len(self.collection_errors),
            "skipped": len(self.skipped),
            "skip_modules": dict(Counter(
                Path(node.split("::", 1)[0]).name for node in self.skipped
            )),
            "skips": self.skipped,
            "unexpected_skips": unexpected,
        }


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path, required=True)
    args, extra = parser.parse_known_args()
    evidence = Evidence()
    code = pytest.main([
        "tests/riser_database", "-q", "-rA", "-p", "no:cacheprovider", *extra,
    ], plugins=[evidence])
    result = evidence.result()
    args.output.write_text(
        json.dumps(result, indent=2, sort_keys=True) + "\n", encoding="utf-8", newline="\n"
    )
    print(f"PUBLIC: {result['passed']} passed, {result['failed']} failed, "
          f"{result['skipped']} skipped; {result['collection_errors']} collection errors; "
          f"{len(result['unexpected_skips'])} undocumented skips")
    return int(code) or int(bool(result["unexpected_skips"]))


if __name__ == "__main__":
    raise SystemExit(main())
