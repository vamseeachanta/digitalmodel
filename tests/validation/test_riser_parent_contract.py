# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Behavior tests for the #1602 parent-contract validator."""

from __future__ import annotations

import hashlib
import shutil
from pathlib import Path

import pytest
import yaml

from digitalmodel.contract_validation.riser_parent_contract import (
    ContractValidationError,
    validate_parent_contract,
)


REPO_ROOT = Path(__file__).resolve().parents[2]
PLAN_DIR = REPO_ROOT / "docs" / "plans"
FILES = (
    "issue-1602-riser-analysis-contract-v3.yaml",
    "issue-1602-riser-analysis-contract-v1.yaml",
    "issue-1602-riser-host-motion-contract-v3.yaml",
    "issue-1602-riser-assurance-contract-v3.yaml",
    "2026-07-18-issue-1602-riser-host-diffraction-plan.html",
)


def _sha(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def _bundle(tmp_path: Path) -> tuple[Path, Path]:
    plans = tmp_path / "docs" / "plans"
    plans.mkdir(parents=True)
    for name in FILES:
        shutil.copy2(PLAN_DIR / name, plans / name)
    specs = tmp_path / "docs" / "superpowers" / "specs"
    specs.mkdir(parents=True)
    design = "2026-07-18-riser-host-diffraction-monitoring-design.html"
    shutil.copy2(REPO_ROOT / "docs" / "superpowers" / "specs" / design, specs / design)
    evidence = plans / "evidence"
    evidence.mkdir()
    evidence_name = "issue-1602-drive-index-resource-intel-2026-07-18.json"
    shutil.copy2(PLAN_DIR / "evidence" / evidence_name, evidence / evidence_name)
    return plans / FILES[0], tmp_path / "build" / "composed.yaml"


def _load(path: Path) -> dict[str, object]:
    return yaml.safe_load(path.read_text(encoding="utf-8"))


def _save(path: Path, value: object) -> None:
    path.write_text(yaml.safe_dump(value, sort_keys=False), encoding="utf-8")


def _refresh_component_hashes(manifest_path: Path) -> None:
    root = _load(manifest_path)
    base = root["composition"]["base"]
    base["sha256"] = _sha(manifest_path.parent / base["path"])
    for item in root["composition"]["components"]:
        item["sha256"] = _sha(manifest_path.parent / item["path"])
    _save(manifest_path, root)


def _mutate_component(manifest: Path, name: str, change: object) -> None:
    path = manifest.parent / name
    value = _load(path)
    change(value)
    _save(path, value)
    _refresh_component_hashes(manifest)


def test_hash_mismatch_fails_without_emitting_output(tmp_path: Path) -> None:
    manifest, output = _bundle(tmp_path)
    host = manifest.parent / "issue-1602-riser-host-motion-contract-v3.yaml"
    host.write_text(host.read_text(encoding="utf-8") + "\n", encoding="utf-8")

    with pytest.raises(ContractValidationError, match="hash mismatch"):
        validate_parent_contract(manifest, output)

    assert not output.exists()


def test_valid_contract_emits_deterministic_composition(tmp_path: Path) -> None:
    manifest, output = _bundle(tmp_path)

    composed = validate_parent_contract(manifest, output)

    assert output.exists()
    assert yaml.safe_load(output.read_text(encoding="utf-8")) == composed
    first = output.read_bytes()
    validate_parent_contract(manifest, output)
    assert output.read_bytes() == first


def test_undeclared_yaml_conflict_fails_closed(tmp_path: Path) -> None:
    manifest, output = _bundle(tmp_path)

    def conflict(host: dict[str, object]) -> None:
        host["interface_envelopes"]["normalized_case"] = {
            "producer": "floating_host_identity"
        }

    _mutate_component(
        manifest, "issue-1602-riser-host-motion-contract-v3.yaml", conflict
    )

    with pytest.raises(ContractValidationError, match="undeclared conflict"):
        validate_parent_contract(manifest, output)

    assert not output.exists()


def test_missing_required_owner_or_envelope_fails_closed(tmp_path: Path) -> None:
    manifest, output = _bundle(tmp_path)

    def remove_owner(host: dict[str, object]) -> None:
        del host["owners"]["hydrodynamic_response"]

    _mutate_component(
        manifest, "issue-1602-riser-host-motion-contract-v3.yaml", remove_owner
    )

    with pytest.raises(ContractValidationError, match="missing required owner"):
        validate_parent_contract(manifest, output)


def test_missing_required_envelope_fails_closed(tmp_path: Path) -> None:
    manifest, output = _bundle(tmp_path)

    def remove_envelope(host: dict[str, object]) -> None:
        del host["interface_envelopes"]["hydrodynamic_response"]

    _mutate_component(
        manifest, "issue-1602-riser-host-motion-contract-v3.yaml", remove_envelope
    )

    with pytest.raises(ContractValidationError, match="missing required envelope"):
        validate_parent_contract(manifest, output)


def test_cyclic_blocking_dag_fails_closed(tmp_path: Path) -> None:
    manifest, output = _bundle(tmp_path)

    def add_cycle(host: dict[str, object]) -> None:
        host["milestone_DAG_extensions"]["edges"].append(
            "floating_host_identity_ready -> floating_host_source_ready"
        )

    _mutate_component(
        manifest, "issue-1602-riser-host-motion-contract-v3.yaml", add_cycle
    )

    with pytest.raises(ContractValidationError, match="cycle"):
        validate_parent_contract(manifest, output)


def test_dropped_host_motion_chain_field_fails_closed(tmp_path: Path) -> None:
    manifest, output = _bundle(tmp_path)

    def drop_field(host: dict[str, object]) -> None:
        host["downstream_chain_fields"].remove("vessel_motion_boundary_id")

    _mutate_component(
        manifest, "issue-1602-riser-host-motion-contract-v3.yaml", drop_field
    )

    with pytest.raises(ContractValidationError, match="chain fields"):
        validate_parent_contract(manifest, output)


def test_invalid_family_route_fails_closed(tmp_path: Path) -> None:
    manifest, output = _bundle(tmp_path)

    def reroute(base: dict[str, object]) -> None:
        base["required_family_routing"]["allowed_routes"][
            "drilling"
        ] = "completion_workover_analysis_request"

    _mutate_component(manifest, "issue-1602-riser-analysis-contract-v1.yaml", reroute)

    with pytest.raises(ContractValidationError, match="family route"):
        validate_parent_contract(manifest, output)


def test_wrong_public_config_set_fails_closed(tmp_path: Path) -> None:
    manifest, output = _bundle(tmp_path)
    root = _load(manifest)
    root["effective_public_dataset_surfaces"]["exact_required_configs"].pop()
    _save(manifest, root)

    with pytest.raises(ContractValidationError, match="17 public configs"):
        validate_parent_contract(manifest, output)


def test_monitoring_node_in_blocking_dag_fails_closed(tmp_path: Path) -> None:
    manifest, output = _bundle(tmp_path)

    def add_monitoring(host: dict[str, object]) -> None:
        host["milestone_DAG_extensions"]["edges"].append(
            "HF_publication_verified -> telemetry_identity_mapping"
        )
        host["milestone_DAG_extensions"]["milestone_owners"][
            "telemetry_identity_mapping"
        ] = "analysis_semantics"

    _mutate_component(
        manifest, "issue-1602-riser-host-motion-contract-v3.yaml", add_monitoring
    )

    with pytest.raises(ContractValidationError, match="monitoring"):
        validate_parent_contract(manifest, output)


def test_unsafe_component_path_fails_closed(tmp_path: Path) -> None:
    manifest, output = _bundle(tmp_path)
    root = _load(manifest)
    root["composition"]["base"]["path"] = "../../../outside.yaml"
    _save(manifest, root)

    with pytest.raises(ContractValidationError, match="escapes contract tree"):
        validate_parent_contract(manifest, output)


def test_undeclared_removal_fails_closed(tmp_path: Path) -> None:
    manifest, output = _bundle(tmp_path)
    root = _load(manifest)
    root["composition"]["explicit_removals"].append("owners.website")
    _save(manifest, root)

    with pytest.raises(ContractValidationError, match="undeclared removal"):
        validate_parent_contract(manifest, output)
