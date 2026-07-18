# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Compose and validate the hash-bound #1602 riser parent contract."""

from __future__ import annotations

import copy
import hashlib
import os
import stat
import tempfile
from pathlib import Path
from typing import Any

import yaml  # type: ignore[import-untyped]

from .errors import ContractValidationError
from .riser_manifest_rules import resolve_bound_path, validate_manifest_declarations
from .riser_contract_rules import validate_contract

MAX_INPUT_BYTES = 16 * 1024 * 1024


class _UniqueKeySafeLoader(yaml.SafeLoader):
    """Safe YAML loader that rejects duplicate mapping keys."""


def _construct_unique_mapping(loader: Any, node: Any, deep: bool = False) -> Any:
    mapping: dict[Any, Any] = {}
    for key_node, value_node in node.value:
        key = loader.construct_object(key_node, deep=deep)
        if key in mapping:
            raise yaml.constructor.ConstructorError(
                "while constructing mapping",
                node.start_mark,
                f"duplicate YAML key: {key}",
                key_node.start_mark,
            )
        mapping[key] = loader.construct_object(value_node, deep=deep)
    return mapping


_UniqueKeySafeLoader.add_constructor(
    yaml.resolver.BaseResolver.DEFAULT_MAPPING_TAG, _construct_unique_mapping
)


def _parse_yaml(data: bytes, label: str) -> dict[str, Any]:
    try:
        value = yaml.load(data, Loader=_UniqueKeySafeLoader)  # nosec B506
    except (UnicodeError, yaml.YAMLError) as exc:
        raise ContractValidationError(f"cannot load YAML {label}: {exc}") from exc
    if not isinstance(value, dict):
        raise ContractValidationError(f"YAML root must be a mapping: {label}")
    return value


def _read_regular(path: Path) -> tuple[bytes, tuple[int, int]]:
    flags = os.O_RDONLY | getattr(os, "O_CLOEXEC", 0) | getattr(os, "O_NOFOLLOW", 0)
    try:
        descriptor = os.open(path, flags)
        with os.fdopen(descriptor, "rb") as stream:
            info = os.fstat(stream.fileno())
            if not stat.S_ISREG(info.st_mode):
                raise ContractValidationError(
                    f"input must be regular non-symlink: {path.name}"
                )
            path_info = os.stat(path, follow_symlinks=False)
            if (info.st_dev, info.st_ino) != (path_info.st_dev, path_info.st_ino):
                raise ContractValidationError(
                    f"input changed while opening: {path.name}"
                )
            if info.st_size > MAX_INPUT_BYTES:
                raise ContractValidationError(f"input exceeds size limit: {path.name}")
            data = stream.read(MAX_INPUT_BYTES + 1)
    except OSError as exc:
        raise ContractValidationError(
            f"input must be regular non-symlink: {path.name}: {exc}"
        ) from exc
    if len(data) > MAX_INPUT_BYTES:
        raise ContractValidationError(f"input exceeds size limit: {path.name}")
    return data, (info.st_dev, info.st_ino)


def _read_manifest(path: Path) -> tuple[dict[str, Any], tuple[int, int]]:
    data, identity = _read_regular(path)
    return _parse_yaml(data, path.name), identity


def _bound_declarations(root: dict[str, Any]) -> list[dict[str, Any]]:
    composition = root.get("composition", {})
    declarations = [composition.get("base", {})]
    declarations.extend(composition.get("components", []))
    declarations.extend(
        root.get(key, {})
        for key in (
            "review_candidate_plan",
            "approved_architecture_design",
            "resource_intelligence_evidence",
        )
    )
    if not all(isinstance(item, dict) for item in declarations):
        raise ContractValidationError("invalid bound-file declaration")
    return declarations


def _read_bound_files(
    root: dict[str, Any], base_dir: Path
) -> tuple[dict[str, bytes], set[tuple[int, int]], set[Path]]:
    payloads: dict[str, bytes] = {}
    identities: set[tuple[int, int]] = set()
    paths: set[Path] = set()
    for declaration in _bound_declarations(root):
        path = resolve_bound_path(base_dir, declaration.get("path"))
        if path in paths:
            raise ContractValidationError(f"duplicate bound path: {path.name}")
        data, identity = _read_regular(path)
        if identity in identities:
            raise ContractValidationError(f"duplicate bound path identity: {path.name}")
        expected = declaration.get("sha256")
        if (
            not isinstance(expected, str)
            or hashlib.sha256(data).hexdigest() != expected
        ):
            raise ContractValidationError(f"hash mismatch for {path.name}")
        payloads[declaration["path"]] = data
        identities.add(identity)
        paths.add(path)
    return payloads, identities, paths


def _load_components(
    root: dict[str, Any], payloads: dict[str, bytes]
) -> tuple[dict[str, Any], dict[str, dict[str, Any]]]:
    composition = root["composition"]
    base_decl = composition["base"]
    base = _parse_yaml(payloads[base_decl["path"]], base_decl["path"])
    components: dict[str, dict[str, Any]] = {}
    for declaration in composition["components"]:
        component = _parse_yaml(payloads[declaration["path"]], declaration["path"])
        component_id = declaration["component_id"]
        if component.get("component_id") != component_id:
            raise ContractValidationError(
                f"component identity mismatch: {component_id}"
            )
        components[component_id] = component
    return base, components


def _union(left: list[Any], right: list[Any]) -> list[Any]:
    result = copy.deepcopy(left)
    for item in right:
        if item not in result:
            result.append(copy.deepcopy(item))
    return result


def _extension_fields(extension: dict[str, Any]) -> list[str]:
    fields: list[str] = []
    for key in ("always_required_fields", "lineage_fields", "required_fields"):
        fields.extend(extension.get(key, []))
    union = extension.get("discriminated_union", {})
    fields.append(union.get("discriminator"))
    for branch in ("required_branch", "not_applicable_branch"):
        fields.extend(union.get(branch, {}).get("required_fields", []))
    return [field for field in fields if isinstance(field, str)]


def _merge_envelopes(base: dict[str, Any], host: dict[str, Any]) -> dict[str, Any]:
    envelopes: dict[str, Any] = copy.deepcopy(base["interface_envelopes"])
    overlap = set(envelopes) & set(host["interface_envelopes"])
    if overlap:
        raise ContractValidationError(
            f"undeclared conflict in envelopes: {sorted(overlap)}"
        )
    envelopes.update(copy.deepcopy(host["interface_envelopes"]))
    for name, extension in host["existing_envelope_extensions"].items():
        if name not in envelopes:
            raise ContractValidationError(f"missing envelope for extension: {name}")
        additions = (
            extension if isinstance(extension, list) else _extension_fields(extension)
        )
        envelopes[name]["minimum_bindings"] = _union(
            envelopes[name]["minimum_bindings"], additions
        )
        if isinstance(extension, dict):
            envelopes[name].update(copy.deepcopy(extension))
    return envelopes


def _merge_maps_of_lists(
    base: dict[str, Any], extension: dict[str, Any]
) -> dict[str, Any]:
    result = copy.deepcopy(base)
    for key, value in extension.items():
        result[key] = _union(result.get(key, []), value)
    return result


def _compose(
    root: dict[str, Any], base: dict[str, Any], components: dict[str, dict[str, Any]]
) -> dict[str, Any]:
    host = components["host_motion_component"]
    assurance = components["assurance_component"]
    result = copy.deepcopy(base)
    result["contract_version"] = root["contract_version"]
    result["authority"] = copy.deepcopy(root["effective_authority"])
    result["owners"] = _merge_owners(base["owners"], host["owners"])
    result["interface_envelopes"] = _merge_envelopes(base, host)
    result["identity_chain"] = copy.deepcopy(host["identity_chain"])
    result["envelope_commitment_rules"] = {
        **copy.deepcopy(base["envelope_commitment_rules"]),
        **copy.deepcopy(host["envelope_commitment_rules"]),
    }
    _compose_collections(result, base, host, assurance)
    _compose_host_helpers(result, host)
    _compose_assurance(result, assurance)
    return result


def _merge_owners(base: dict[str, Any], extension: dict[str, Any]) -> dict[str, Any]:
    owners = copy.deepcopy(base)
    for key, value in extension.items():
        owners[key] = {**owners.get(key, {}), **copy.deepcopy(value)}
    return owners


def _compose_collections(
    result: dict[str, Any],
    base: dict[str, Any],
    host: dict[str, Any],
    assurance: dict[str, Any],
) -> None:
    dag = copy.deepcopy(base["milestone_DAG"])
    extension = host["milestone_DAG_extensions"]
    dag["edges"] = _union(dag["edges"], extension["edges"])
    dag["milestone_owners"].update(copy.deepcopy(extension["milestone_owners"]))
    dag.update(
        copy.deepcopy(
            {
                k: v
                for k, v in extension.items()
                if k not in {"edges", "milestone_owners"}
            }
        )
    )
    result["milestone_DAG"] = dag
    result["required_handoff_equalities"] = _union(
        base["required_handoff_equalities"], host["required_handoff_equalities"]
    )
    result["required_handoff_derivations"] = _union(
        base["required_handoff_derivations"], assurance["required_handoff_derivations"]
    )
    result["promotion_gates"] = _merge_maps_of_lists(
        base["promotion_gates"], assurance["promotion_gate_extensions"]
    )


def _compose_host_helpers(result: dict[str, Any], host: dict[str, Any]) -> None:
    keys = (
        "downstream_chain_fields",
        "host_motion_route_union",
        "normalized_host_response_union",
        "analysis_lineage_fields",
        "host_motion_binding",
        "route_union_handoff_expansion",
    )
    for key in keys:
        result[key] = copy.deepcopy(host[key])


def _deep_merge(base: Any, extension: Any, path: str = "") -> Any:
    if isinstance(base, dict) and isinstance(extension, dict):
        result = copy.deepcopy(base)
        for key, value in extension.items():
            result[key] = (
                _deep_merge(result[key], value, f"{path}.{key}")
                if key in result
                else copy.deepcopy(value)
            )
        return result
    if isinstance(base, list) and isinstance(extension, list):
        return _union(base, extension)
    if base != extension:
        raise ContractValidationError(f"planning validation scalar conflict: {path}")
    return copy.deepcopy(base)


def _compose_assurance(result: dict[str, Any], assurance: dict[str, Any]) -> None:
    skip = {
        "schema_version",
        "contract_version",
        "component_id",
        "authority_issue",
        "required_handoff_derivations",
        "promotion_gate_extensions",
        "public_dataset_surface_extensions",
        "child_acceptance_extensions",
    }
    for key, value in assurance.items():
        if key not in skip and key != "planning_validation":
            result[key] = copy.deepcopy(value)
    result["planning_validation"] = _deep_merge(
        result["planning_validation"], assurance["planning_validation"]
    )
    surface_extension = assurance["public_dataset_surface_extensions"]
    result["public_dataset_surfaces"].update(
        copy.deepcopy(
            {
                key: value
                for key, value in surface_extension.items()
                if key != "add_configs"
            }
        )
    )
    configs = _union(
        result["public_dataset_surfaces"]["required_configs"],
        assurance["public_dataset_surface_extensions"]["add_configs"],
    )
    result["public_dataset_surfaces"]["required_configs"] = configs
    obligations = result["child_acceptance_obligations"]
    obligations["public_release"].remove("exact_13_config_schemas")
    for issue, additions in assurance["child_acceptance_extensions"].items():
        owner = assurance["child_issue_owner_crosswalk"][issue]
        obligations[owner] = _union(obligations.get(owner, []), additions)


def _reject_output(
    output: Path, input_paths: set[Path], identities: set[tuple[int, int]]
) -> None:
    lexical = Path(os.path.abspath(output))
    if lexical in input_paths:
        raise ContractValidationError("output aliases input")
    try:
        info = os.lstat(lexical)
    except FileNotFoundError:
        return
    except OSError as exc:
        raise ContractValidationError(f"cannot inspect output: {exc}") from exc
    if stat.S_ISLNK(info.st_mode):
        raise ContractValidationError("output must not be a symlink")
    if (info.st_dev, info.st_ino) in identities:
        raise ContractValidationError("output aliases input")


def _atomic_emit(output: Path, composed: dict[str, Any]) -> None:
    output.parent.mkdir(parents=True, exist_ok=True)
    descriptor = -1
    temporary: str | None = None
    try:
        descriptor, temporary = tempfile.mkstemp(
            prefix=f".{output.name}.", suffix=".tmp", dir=output.parent
        )
        data = yaml.safe_dump(composed, sort_keys=False).encode()
        with os.fdopen(descriptor, "wb") as stream:
            descriptor = -1
            stream.write(data)
            stream.flush()
            os.fsync(stream.fileno())
        os.replace(temporary, output)
        temporary = None
        directory = os.open(output.parent, os.O_RDONLY | getattr(os, "O_DIRECTORY", 0))
        try:
            os.fsync(directory)
        finally:
            os.close(directory)
    except OSError as exc:
        raise ContractValidationError(f"cannot atomically emit output: {exc}") from exc
    finally:
        if descriptor >= 0:
            os.close(descriptor)
        if temporary is not None:
            Path(temporary).unlink(missing_ok=True)


def validate_parent_contract(manifest_path: Path, output_path: Path) -> dict[str, Any]:
    """Validate and emit the composed contract, atomically on success only."""

    manifest = Path(os.path.abspath(manifest_path))
    root, manifest_identity = _read_manifest(manifest)
    payloads, identities, paths = _read_bound_files(root, manifest.parent)
    validate_manifest_declarations(root)
    identities.add(manifest_identity)
    paths.add(manifest)
    _reject_output(output_path, paths, identities)
    base, components = _load_components(root, payloads)
    composed = _compose(root, base, components)
    validate_contract(root, base, components, composed)
    _reject_output(output_path, paths, identities)
    _atomic_emit(output_path, composed)
    return composed
