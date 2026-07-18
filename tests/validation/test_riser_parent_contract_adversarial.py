# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Adversarial regression tests from the Task 1 artifact review."""

from __future__ import annotations

import hashlib
import os
import shutil
import subprocess
import sys
from concurrent.futures import ThreadPoolExecutor
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
    name = "issue-1602-drive-index-resource-intel-2026-07-18.json"
    shutil.copy2(PLAN_DIR / "evidence" / name, evidence / name)
    return plans / FILES[0], tmp_path / "build" / "composed.yaml"


def _load(path: Path) -> dict[str, object]:
    return yaml.safe_load(path.read_text(encoding="utf-8"))


def _save(path: Path, value: object) -> None:
    path.write_text(yaml.safe_dump(value, sort_keys=False), encoding="utf-8")


def _refresh(manifest: Path) -> None:
    root = _load(manifest)
    declarations = [root["composition"]["base"]]
    declarations += root["composition"]["components"]
    for declaration in declarations:
        declaration["sha256"] = _sha(manifest.parent / declaration["path"])
    _save(manifest, root)


def _mutate(manifest: Path, name: str, change: object) -> None:
    path = manifest.parent / name
    value = _load(path)
    change(value)
    _save(path, value)
    _refresh(manifest)


def test_composition_retains_complete_base_and_symbolic_semantics(
    tmp_path: Path,
) -> None:
    manifest, output = _bundle(tmp_path)
    base = _load(manifest.parent / FILES[1])
    host = _load(manifest.parent / FILES[2])

    composed = validate_parent_contract(manifest, output)

    for section in base:
        assert section in composed
    assert composed["source_of_truth"] == base["source_of_truth"]
    assert (
        composed["interface_envelopes"]["normalized_case"]["discriminated_union"]
        == host["existing_envelope_extensions"]["normalized_case"][
            "discriminated_union"
        ]
    )
    assert (
        composed["milestone_DAG"]["guard_predicates"]
        == host["milestone_DAG_extensions"]["guard_predicates"]
    )


def test_declared_load_order_and_merge_directives_are_exact(tmp_path: Path) -> None:
    manifest, output = _bundle(tmp_path)
    root = _load(manifest)
    root["composition"]["load_order"].reverse()
    _save(manifest, root)

    with pytest.raises(ContractValidationError, match="load order"):
        validate_parent_contract(manifest, output)


def test_configs_are_derived_from_base_and_extension(tmp_path: Path) -> None:
    manifest, output = _bundle(tmp_path)

    def remove_config(assurance: dict[str, object]) -> None:
        assurance["public_dataset_surface_extensions"]["add_configs"].pop()

    _mutate(manifest, FILES[3], remove_config)

    with pytest.raises(ContractValidationError, match="config"):
        validate_parent_contract(manifest, output)


def test_commitment_rule_must_cover_effective_preimage(tmp_path: Path) -> None:
    manifest, output = _bundle(tmp_path)

    def weaken(host: dict[str, object]) -> None:
        host["envelope_commitment_rules"]["logical_release"]["covers"] = "some_fields"

    _mutate(manifest, FILES[2], weaken)

    with pytest.raises(ContractValidationError, match="commitment preimage"):
        validate_parent_contract(manifest, output)


def test_route_union_preserves_forbidden_fields_and_exactly_one(tmp_path: Path) -> None:
    manifest, output = _bundle(tmp_path)

    def weaken(host: dict[str, object]) -> None:
        union = host["existing_envelope_extensions"]["deterministic_bundle"][
            "discriminated_union"
        ]
        union["exactly_one_branch"] = False
        union["not_applicable_branch"]["forbidden_fields"].pop()

    _mutate(manifest, FILES[2], weaken)

    with pytest.raises(ContractValidationError, match="route union"):
        validate_parent_contract(manifest, output)


def test_nonblocking_monitoring_graph_must_be_acyclic(tmp_path: Path) -> None:
    manifest, output = _bundle(tmp_path)

    def cycle(assurance: dict[str, object]) -> None:
        assurance["non_blocking_consumer_graph"]["edges"].append(
            "risk_reduction_evidence -> release_bound_monitoring_baseline"
        )

    _mutate(manifest, FILES[3], cycle)

    with pytest.raises(ContractValidationError, match="monitoring.*cycle"):
        validate_parent_contract(manifest, output)


def test_duplicate_nested_yaml_key_is_rejected(tmp_path: Path) -> None:
    manifest, output = _bundle(tmp_path)
    host = manifest.parent / FILES[2]
    host.write_text(
        host.read_text(encoding="utf-8")
        + "\nduplicate_probe:\n  key: one\n  key: two\n",
        encoding="utf-8",
    )
    _refresh(manifest)

    with pytest.raises(ContractValidationError, match="duplicate YAML key"):
        validate_parent_contract(manifest, output)


def test_hash_and_parse_use_same_bound_bytes(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    manifest, output = _bundle(tmp_path)
    host = manifest.parent / FILES[2]
    original = host.read_bytes()
    real_read_bytes = Path.read_bytes
    changed = False

    def swap_after_read(path: Path) -> bytes:
        nonlocal changed
        data = real_read_bytes(path)
        if path == host and not changed:
            changed = True
            tampered = original.replace(
                b"worldenergydata/issues/1050", b"worldenergydata/issues/9999"
            )
            with host.open("wb") as stream:
                stream.write(tampered)
        return data

    monkeypatch.setattr(Path, "read_bytes", swap_after_read)
    composed = validate_parent_contract(manifest, output)

    assert composed["owners"]["floating_host_source"]["issue"].endswith("/1050")


def test_symlink_input_and_output_aliases_are_rejected(tmp_path: Path) -> None:
    manifest, output = _bundle(tmp_path)
    host = manifest.parent / FILES[2]
    target = manifest.parent / "host-real.yaml"
    host.rename(target)
    host.symlink_to(target.name)
    _refresh(manifest)

    with pytest.raises(ContractValidationError, match="regular non-symlink"):
        validate_parent_contract(manifest, output)

    manifest, _ = _bundle(tmp_path / "alias")
    original = manifest.read_bytes()
    with pytest.raises(ContractValidationError, match="aliases input"):
        validate_parent_contract(manifest, manifest)
    assert manifest.read_bytes() == original


def test_output_symlink_is_not_clobbered(tmp_path: Path) -> None:
    manifest, output = _bundle(tmp_path)
    output.parent.mkdir()
    victim = tmp_path / "victim.txt"
    victim.write_text("safe", encoding="utf-8")
    output.symlink_to(victim)

    with pytest.raises(ContractValidationError, match="output.*symlink"):
        validate_parent_contract(manifest, output)
    assert victim.read_text(encoding="utf-8") == "safe"
    assert output.is_symlink()


def test_concurrent_atomic_emissions_are_identical_and_clean(tmp_path: Path) -> None:
    manifest, output = _bundle(tmp_path)

    with ThreadPoolExecutor(max_workers=8) as executor:
        results = list(
            executor.map(lambda _: validate_parent_contract(manifest, output), range(8))
        )

    observed = yaml.safe_load(output.read_text(encoding="utf-8"))
    assert all(result == observed for result in results)
    assert not list(output.parent.glob(f".{output.name}.*.tmp"))


def test_legacy_predictable_temp_symlink_is_ignored(tmp_path: Path) -> None:
    manifest, output = _bundle(tmp_path)
    output.parent.mkdir()
    victim = tmp_path / "victim.txt"
    victim.write_text("safe", encoding="utf-8")
    output.with_suffix(output.suffix + ".tmp").symlink_to(victim)

    validate_parent_contract(manifest, output)

    assert victim.read_text(encoding="utf-8") == "safe"


def test_failed_replace_preserves_output_and_cleans_temp(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    manifest, output = _bundle(tmp_path)
    output.parent.mkdir()
    output.write_text("previous", encoding="utf-8")

    def fail_replace(source: object, target: object) -> None:
        raise OSError("injected replace failure")

    monkeypatch.setattr(os, "replace", fail_replace)
    with pytest.raises(ContractValidationError, match="atomically emit"):
        validate_parent_contract(manifest, output)

    assert output.read_text(encoding="utf-8") == "previous"
    assert not list(output.parent.glob(f".{output.name}.*.tmp"))


def test_cli_output_is_stable_across_hash_seeds(tmp_path: Path) -> None:
    manifest, first = _bundle(tmp_path)
    second = first.with_name("second.yaml")
    script = REPO_ROOT / "scripts" / "validation" / "validate_riser_parent_contract.py"
    environment = os.environ.copy()
    environment["PYTHONPATH"] = str(REPO_ROOT / "src")

    for seed, output in (("1", first), ("2", second)):
        environment["PYTHONHASHSEED"] = seed
        subprocess.run(
            [sys.executable, str(script), str(manifest), str(output)],
            check=True,
            capture_output=True,
            text=True,
            env=environment,
        )

    assert first.read_bytes() == second.read_bytes()


def test_intermediate_symlink_escape_is_rejected(tmp_path: Path) -> None:
    manifest, output = _bundle(tmp_path)
    outside = tmp_path / "outside"
    outside.mkdir()
    target = outside / "component.yaml"
    shutil.copy2(manifest.parent / FILES[2], target)
    escape = manifest.parent.parent / "escape"
    escape.symlink_to(outside, target_is_directory=True)
    root = _load(manifest)
    root["composition"]["components"][0]["path"] = "../escape/component.yaml"
    root["composition"]["components"][0]["sha256"] = _sha(target)
    _save(manifest, root)

    with pytest.raises(ContractValidationError, match="escapes contract tree"):
        validate_parent_contract(manifest, output)


@pytest.mark.parametrize("endpoint", ["from", "to"])
def test_unknown_handoff_endpoint_is_rejected(tmp_path: Path, endpoint: str) -> None:
    manifest, output = _bundle(tmp_path)

    def change(host: dict[str, object]) -> None:
        record = dict(host["required_handoff_equalities"][0])
        record[endpoint] = "bogus_endpoint"
        host["required_handoff_equalities"].append(record)

    _mutate(manifest, FILES[2], change)
    with pytest.raises(ContractValidationError, match="unresolved handoff endpoint"):
        validate_parent_contract(manifest, output)


def test_undeclared_extra_component_is_rejected(tmp_path: Path) -> None:
    manifest, output = _bundle(tmp_path)
    extra = manifest.parent / "extra.yaml"
    _save(extra, {"component_id": "extra_component"})
    root = _load(manifest)
    root["composition"]["components"].append(
        {"path": extra.name, "component_id": "extra_component", "sha256": _sha(extra)}
    )
    _save(manifest, root)

    with pytest.raises(ContractValidationError, match="component declaration"):
        validate_parent_contract(manifest, output)


def test_duplicate_bound_path_is_rejected(tmp_path: Path) -> None:
    manifest, output = _bundle(tmp_path)
    root = _load(manifest)
    base = root["composition"]["base"]
    root["review_candidate_plan"]["path"] = base["path"]
    root["review_candidate_plan"]["sha256"] = base["sha256"]
    _save(manifest, root)

    with pytest.raises(ContractValidationError, match="duplicate bound path"):
        validate_parent_contract(manifest, output)
