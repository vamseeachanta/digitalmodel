# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Generated retirement schema ported from the reviewed retirement rule.

Cold-store references are relative to the register storage root; validation checks their form,
not store availability. Archived bytes require separate digest readback.
"""
from __future__ import annotations

import fnmatch
import hashlib
import os
import re
import subprocess
from datetime import date
from pathlib import Path

import yaml

_PLACEHOLDER_VALUES = {"", "todo", "tbd", "none", "n/a", "placeholder"}
_DATE_RE = re.compile(r"^\d{4}-\d{2}-\d{2}$")
_URL_RE = re.compile(r"^https://github\.com/vamseeachanta/digitalmodel/issues/\d+")


def _is_placeholder(value: str) -> bool:
    return value.strip().lower() in _PLACEHOLDER_VALUES


def _generated(path: str, config: dict) -> bool:
    patterns = (config.get("generated_patterns", []) + config.get("ci_artifact_patterns", [])
                + config.get("generated_retirement_patterns", []))
    return any(fnmatch.fnmatchcase(path, pattern) for pattern in patterns)


class RetirementSafeLoader(yaml.SafeLoader):
    """Read safe YAML while rejecting duplicate keys in every mapping."""


def _unique_retirement_mapping(loader: yaml.SafeLoader, node: yaml.MappingNode) -> dict:
    """Reject overwritten schema values before constructing a mapping."""
    loader.flatten_mapping(node)
    keys: set[object] = set()
    for key_node, _ in node.value:
        key = loader.construct_object(key_node)
        try:
            if key in keys:
                raise yaml.constructor.ConstructorError(
                    None, None, "duplicate retirement mapping key", key_node.start_mark
                )
            keys.add(key)
        except TypeError as error:
            raise yaml.constructor.ConstructorError(
                None, None, "invalid retirement mapping key", key_node.start_mark
            ) from error
    return loader.construct_mapping(node)


RetirementSafeLoader.add_constructor(
    yaml.resolver.BaseResolver.DEFAULT_MAPPING_TAG, _unique_retirement_mapping
)


def _host_reference(value: str) -> bool:
    """Recognize qualified host or IPv4 syntax rather than version directories."""
    prefix = value.split("/", 1)[0].removesuffix(".")
    return (
        re.fullmatch(r"(?:[A-Za-z0-9-]+\.)+[A-Za-z]{2,}", prefix) is not None
        or re.fullmatch(r"(?:[0-9]{1,3}\.){3}[0-9]{1,3}", prefix) is not None
    )


def _relative_path_valid(value: object) -> bool:
    """Require an unambiguous relative path without machine-specific syntax."""
    if not isinstance(value, str) or not value or value != value.strip():
        return False
    if any(character in value for character in ("\\", ":", "\x00", "\n", "\r")):
        return False
    return all(part not in ("", ".", "..") for part in value.split("/"))


def _substantive_string(value: object, minimum: int = 1) -> bool:
    """Reject missing, placeholder and insufficient metadata strings."""
    return (
        isinstance(value, str)
        and not _is_placeholder(value)
        and len(value.strip()) >= minimum
    )


def _retirement_disposition_valid(value: object) -> bool:
    """Validate disposition form without accessing a store or running commands."""
    if not isinstance(value, dict):
        return False
    if value.get("kind") == "reproducible":
        return set(value) == {"kind", "command"} and _substantive_string(
            value["command"]
        )
    if value.get("kind") != "cold-store":
        return False
    if set(value) != {"kind", "dataset_path", "digest_list"}:
        return False
    return all(
        _relative_path_valid(value[key]) and _substantive_string(value[key])
        for key in ("dataset_path", "digest_list")
    ) and not _host_reference(value["dataset_path"])


def _retirement_entry_valid(entry: object, config: dict) -> bool:
    """Validate all required fields and reject unknown schema fields."""
    required = {"path", "sha256", "date", "reason", "disposition", "follow_up"}
    if not isinstance(entry, dict) or set(entry) != required:
        return False
    if not _relative_path_valid(entry["path"]):
        return False
    if not _generated(entry["path"], config):
        return False
    if not isinstance(entry["sha256"], str) or not re.fullmatch(
        r"[0-9a-f]{64}", entry["sha256"]
    ):
        return False
    entry_date = entry["date"]
    if type(entry_date) is date:
        entry_date = entry_date.isoformat()
    if not isinstance(entry_date, str) or not _DATE_RE.fullmatch(entry_date):
        return False
    try:
        date.fromisoformat(entry_date)
    except ValueError:
        return False
    follow_up_re = _URL_RE.pattern.replace("issues/", "(?:issues|pull)/")
    return (
        _substantive_string(entry["reason"], 30)
        and _retirement_disposition_valid(entry["disposition"])
        and isinstance(entry["follow_up"], str)
        and re.fullmatch(follow_up_re, entry["follow_up"]) is not None
    )


def load_retirements(
    path: Path | str, config: dict
) -> tuple[dict[str, dict], list[str]]:
    """Load valid retirements; malformed entries never grant path exemptions."""
    register_path = Path(path)
    if not register_path.exists():
        return {}, []
    invalid = "invalid-retirement-entry: " + register_path.as_posix()
    try:
        loader = RetirementSafeLoader(register_path.read_text(encoding="utf-8"))
        try:
            payload = loader.get_single_data()
        finally:
            loader.dispose()
    except (OSError, UnicodeError, ValueError, yaml.YAMLError):
        return {}, [invalid]
    if not isinstance(payload, dict) or set(payload) != {"retirements"}:
        return {}, [invalid]
    if not isinstance(payload["retirements"], list):
        return {}, [invalid]
    entries: dict[str, dict] = {}
    violations: list[str] = []
    seen: set[str] = set()
    for entry in payload["retirements"]:
        if not _retirement_entry_valid(entry, config):
            violations.append(invalid)
            continue
        entry_path = entry["path"]
        if entry_path in seen:
            violations.append(
                "duplicate-retirement-entry: " + entry_path
            )
        else:
            entries[entry_path] = entry
            seen.add(entry_path)
    return entries, violations


def _git(repo: Path, *args: str) -> bytes:
    """Resolve the supplied checkout without inherited Git root bindings."""
    env = {key: value for key, value in os.environ.items()
           if key not in {"GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR", "GIT_INDEX_FILE"}}
    return subprocess.run(["git", *args], cwd=repo, env=env, check=True,
                          capture_output=True).stdout


def _deletion_sources(repo: Path) -> dict[str, str]:
    """Bind committed deletions to the comparison base and pending ones to HEAD."""
    try:
        _git(repo, "rev-parse", "--verify", "HEAD")
    except subprocess.CalledProcessError:
        return {}
    base_ref = os.environ.get("GITHUB_BASE_REF")
    if base_ref:
        # Hosted checkouts retain remote branches; fixtures can use local refs.
        for ref in ("refs/remotes/origin/" + base_ref, base_ref):
            try:
                base = _git(repo, "merge-base", ref, "HEAD").decode().strip()
                break
            except subprocess.CalledProcessError:
                continue
        else:
            raise ValueError("retirement-base-unavailable")
    else:
        try:
            base = _git(repo, "merge-base", "origin/main", "HEAD").decode().strip()
        except subprocess.CalledProcessError:
            base = "HEAD"
    sources = {}
    for ref, revisions in ((base, (base, "HEAD")), ("HEAD", ("HEAD",))):
        paths = _git(repo, "diff", *revisions, "--no-renames", "--diff-filter=D",
                     "--name-only", "-z").decode("utf-8").split("\0")
        sources.update({path: ref for path in paths if path})
    return sources


def check_retirements(repo: Path, config: dict) -> list[str]:
    """Authenticate current deletions and check historical register consistency."""
    retired, errors = load_retirements(repo / "config/generated_retirements.yml", config)
    tracked = set(_git(repo, "ls-files", "-z").decode().strip("\0").split("\0"))
    try:
        sources = _deletion_sources(repo)
    except ValueError as error:
        errors.append(str(error))
        sources = {}
    deleted = set(sources)
    for path, entry in retired.items():
        if path in tracked - deleted or os.path.lexists(repo / path):
            errors.append("retirement-entry-for-tracked-path: " + path)
        if any(fnmatch.fnmatchcase(path, row["pattern"])
               for row in config.get("durable_exceptions", [])):
            errors.append("retired-path-still-allowed: " + path)
    for path in sorted(deleted):
        if not _generated(path, config):
            continue
        if path not in retired:
            errors.append("generated-artifact-deletion-or-relocation: " + path)
            continue
        try:
            content = _git(repo, "cat-file", "blob", sources[path] + ":" + path)
        except subprocess.CalledProcessError:
            errors.append("generated-artifact-deletion-or-relocation: " + path)
            continue
        if hashlib.sha256(content).hexdigest() != retired[path]["sha256"]:
            errors.append("retirement-digest-mismatch: " + path)
    return sorted(set(errors))
