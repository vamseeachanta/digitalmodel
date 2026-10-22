"""Retained-ancestry guard; source archives cannot qualify Git provenance."""
import importlib.util
from pathlib import Path

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
