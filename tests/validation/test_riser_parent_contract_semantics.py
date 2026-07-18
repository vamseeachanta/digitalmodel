# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Mutation tests for exact approved #1602 contract semantics."""

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
ROOT = "issue-1602-riser-analysis-contract-v3.yaml"
BASE = "issue-1602-riser-analysis-contract-v1.yaml"
HOST = "issue-1602-riser-host-motion-contract-v3.yaml"
ASSURANCE = "issue-1602-riser-assurance-contract-v3.yaml"


def _load(path: Path) -> dict[str, object]:
    return yaml.safe_load(path.read_text(encoding="utf-8"))


def _save(path: Path, value: object) -> None:
    path.write_text(yaml.safe_dump(value, sort_keys=False), encoding="utf-8")


def _bundle(tmp_path: Path) -> tuple[Path, Path]:
    plans = tmp_path / "docs" / "plans"
    plans.mkdir(parents=True)
    names = [
        ROOT,
        BASE,
        HOST,
        ASSURANCE,
        "2026-07-18-issue-1602-riser-host-diffraction-plan.html",
    ]
    for name in names:
        shutil.copy2(PLAN_DIR / name, plans / name)
    specs = tmp_path / "docs" / "superpowers" / "specs"
    specs.mkdir(parents=True)
    design = "2026-07-18-riser-host-diffraction-monitoring-design.html"
    shutil.copy2(REPO_ROOT / "docs" / "superpowers" / "specs" / design, specs / design)
    evidence = plans / "evidence"
    evidence.mkdir()
    item = "issue-1602-drive-index-resource-intel-2026-07-18.json"
    shutil.copy2(PLAN_DIR / "evidence" / item, evidence / item)
    return plans / ROOT, tmp_path / "build" / "composed.yaml"


def _mutate(manifest: Path, name: str, change: object) -> None:
    path = manifest.parent / name
    value = _load(path)
    change(value)
    _save(path, value)
    root = _load(manifest)
    declarations = [root["composition"]["base"], *root["composition"]["components"]]
    for declaration in declarations:
        target = manifest.parent / declaration["path"]
        declaration["sha256"] = hashlib.sha256(target.read_bytes()).hexdigest()
    _save(manifest, root)


def test_derivation_record_mutation_is_rejected(tmp_path: Path) -> None:
    manifest, output = _bundle(tmp_path)

    def change(assurance: dict[str, object]) -> None:
        assurance["required_handoff_derivations"][0]["mode"] = "plausible_but_wrong"

    _mutate(manifest, ASSURANCE, change)
    with pytest.raises(ContractValidationError, match="derivation record"):
        validate_parent_contract(manifest, output)


def test_release_membership_rule_mutation_is_rejected(tmp_path: Path) -> None:
    manifest, output = _bundle(tmp_path)

    def change(assurance: dict[str, object]) -> None:
        assurance["release_membership_contract"]["rules"].pop()

    _mutate(manifest, ASSURANCE, change)
    with pytest.raises(ContractValidationError, match="release membership"):
        validate_parent_contract(manifest, output)


def test_nested_reference_to_wrong_existing_branch_is_rejected(tmp_path: Path) -> None:
    manifest, output = _bundle(tmp_path)

    def change(assurance: dict[str, object]) -> None:
        selected = assurance["normalized_route_handoff_expansion"][
            "selected_branch_by_discriminator"
        ]
        selected["required_ref"] = selected["not_applicable_ref"]

    _mutate(manifest, ASSURANCE, change)
    with pytest.raises(ContractValidationError, match="normalized.*reference"):
        validate_parent_contract(manifest, output)


@pytest.mark.parametrize("target", ["family", "drilling"])
def test_route_type_mutation_is_rejected(tmp_path: Path, target: str) -> None:
    manifest, output = _bundle(tmp_path)

    def change(host: dict[str, object]) -> None:
        if target == "family":
            host["host_motion_route_union"]["family_constraints"][
                "completion"
            ] = "required"
        else:
            host["existing_envelope_extensions"]["drilling_analysis_request"][
                "fixed_applicability"
            ] = "not_applicable"

    _mutate(manifest, HOST, change)
    with pytest.raises(ContractValidationError, match="route.*semantics"):
        validate_parent_contract(manifest, output)


def test_guard_predicate_mutation_is_rejected(tmp_path: Path) -> None:
    manifest, output = _bundle(tmp_path)

    def change(host: dict[str, object]) -> None:
        host["milestone_DAG_extensions"]["guard_predicates"][
            "normalized_case_not_applicable"
        ] = "plausible_but_wrong_predicate"

    _mutate(manifest, HOST, change)
    with pytest.raises(ContractValidationError, match="guard predicates"):
        validate_parent_contract(manifest, output)


def test_monitoring_baseline_equality_mutation_is_rejected(tmp_path: Path) -> None:
    manifest, output = _bundle(tmp_path)

    def change(assurance: dict[str, object]) -> None:
        equalities = assurance["non_blocking_consumer_graph"][
            "required_baseline_equalities"
        ]
        equalities[-1] = "operating_envelope_plausible_but_wrong"

    _mutate(manifest, ASSURANCE, change)
    with pytest.raises(ContractValidationError, match="monitoring baseline equalities"):
        validate_parent_contract(manifest, output)


def test_child_owner_mapping_mutation_is_rejected(tmp_path: Path) -> None:
    manifest, output = _bundle(tmp_path)

    def change(assurance: dict[str, object]) -> None:
        assurance["child_issue_owner_crosswalk"][
            "https://github.com/vamseeachanta/digitalmodel/issues/1609"
        ] = "analysis_semantics"

    _mutate(manifest, ASSURANCE, change)
    with pytest.raises(ContractValidationError, match="crosswalk mapping"):
        validate_parent_contract(manifest, output)


def test_bogus_every_covers_expression_is_rejected(tmp_path: Path) -> None:
    manifest, output = _bundle(tmp_path)

    def change(host: dict[str, object]) -> None:
        host["envelope_commitment_rules"]["logical_release"][
            "covers"
        ] = "every_bogus_field"

    _mutate(manifest, HOST, change)
    with pytest.raises(ContractValidationError, match="covers expression"):
        validate_parent_contract(manifest, output)


def test_unapproved_nested_reference_key_is_rejected(tmp_path: Path) -> None:
    manifest, output = _bundle(tmp_path)

    def change(assurance: dict[str, object]) -> None:
        assurance["normalized_route_handoff_expansion"][
            "attacker_ref"
        ] = "does.not.exist"

    _mutate(manifest, ASSURANCE, change)
    with pytest.raises(ContractValidationError, match="reference.*exact"):
        validate_parent_contract(manifest, output)


def test_extra_release_membership_key_is_rejected(tmp_path: Path) -> None:
    manifest, output = _bundle(tmp_path)

    def change(assurance: dict[str, object]) -> None:
        assurance["release_membership_contract"]["allow_orphan_members"] = True

    _mutate(manifest, ASSURANCE, change)
    with pytest.raises(ContractValidationError, match="release membership"):
        validate_parent_contract(manifest, output)


def test_extra_reference_handoff_record_is_rejected(tmp_path: Path) -> None:
    manifest, output = _bundle(tmp_path)

    def change(host: dict[str, object]) -> None:
        record = next(
            dict(item)
            for item in host["required_handoff_equalities"]
            if "fields_ref" in item
        )
        record["when"] = "attacker_override"
        host["required_handoff_equalities"].append(record)

    _mutate(manifest, HOST, change)
    with pytest.raises(ContractValidationError, match="fields_ref routes"):
        validate_parent_contract(manifest, output)
