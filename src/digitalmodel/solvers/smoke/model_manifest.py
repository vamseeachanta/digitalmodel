"""Fail-closed, local-only input contract for the approved mooring case."""
from __future__ import annotations

import hashlib
import json
import os
from pathlib import Path, PurePosixPath
import re

import yaml

CONTRACT_PATH = Path(__file__).resolve().parents[4] / "docs/benchmarks/mooring_buoy/qualification.yml"
FIELDS = {"schema_version", "case_id", "run_id", "source_revision", "source",
          "master", "files", "contract", "python_hash_seed", "outputs"}
OUTPUTS = {"simulation": "solve/model.sim", "solve_results": "solve/results.json",
           "readback_results": "readback/results.json"}


def _pairs(pairs):
    result = {}
    for key, value in pairs:
        if not isinstance(key, str) or key in result:
            raise ValueError("duplicate or non-string mapping key")
        result[key] = value
    return result


class _UniqueLoader(yaml.SafeLoader):
    pass


def _mapping(loader, node):
    return _pairs((loader.construct_object(k), loader.construct_object(v))
                  for k, v in node.value)


_UniqueLoader.add_constructor(yaml.resolver.BaseResolver.DEFAULT_MAPPING_TAG, _mapping)


def read_yaml(path):
    text = Path(path).read_text(encoding="utf-8-sig")
    try:
        if any(isinstance(token, (yaml.tokens.AnchorToken, yaml.tokens.AliasToken))
               for token in yaml.scan(text)):
            raise ValueError("YAML aliases unsupported in audited bundle")
        return yaml.load(text, Loader=_UniqueLoader)
    except (yaml.YAMLError, RecursionError) as error:
        raise ValueError("invalid or unsupported YAML") from error


def load_contract():
    return read_yaml(CONTRACT_PATH)


def digest(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def _relative(root, name):
    if not isinstance(name, str) or not name or "\\" in name or ":" in name:
        raise ValueError("invalid relative path")
    path = PurePosixPath(name)
    if path.is_absolute() or ".." in path.parts or path.as_posix() != name:
        raise ValueError("noncanonical or escaping path")
    resolved = (root / name).resolve()
    if not resolved.is_relative_to(root) or not resolved.is_file():
        raise ValueError("missing or escaping file")
    return resolved


def _hash(value, length=64):
    if not isinstance(value, str) or not re.fullmatch(r"[0-9a-f]{" + str(length) + "}", value):
        raise ValueError("invalid digest or revision")


def _file_manifest(root, records):
    if not isinstance(records, list) or not records:
        raise ValueError("file manifest is empty")
    files, identities = {}, set()
    for record in records:
        if not isinstance(record, dict) or set(record) != {"path", "sha256"}:
            raise ValueError("invalid file record")
        name = record["path"]
        path = _relative(root, name)
        identity = str(path).casefold()
        if identity in identities:
            raise ValueError("duplicate file identity")
        identities.add(identity)
        _hash(record["sha256"])
        if digest(path) != record["sha256"]:
            raise ValueError("bundle hash mismatch")
        files[name] = path
    return files


def _tree(data):
    """Inspect data keys without evaluating selectors, scripts or expressions."""
    stack = [data]
    while stack:
        item = stack.pop()
        if isinstance(item, list):
            stack.extend(item)
        elif isinstance(item, dict):
            for key, value in item.items():
                if key == "RestartStateRecordingTest":
                    if type(value) is not str or value != "":
                        raise ValueError("restart script must be explicitly empty")
                elif key == "includefile":
                    yield value
                elif key == "BaseFile":
                    raise ValueError("BaseFile is not supported by audited mode")
                elif re.search(r"script|python|externalfunction|dll|filename", key, re.I):
                    if value not in (None, "", "(none)", "None"):
                        raise ValueError("external dependency or execution hook")
                stack.append(value)


def _closure(root, master, files):
    visited, active = set(), set()
    stack = [(master, False)]
    while stack:
        name, leaving = stack.pop()
        if leaving:
            active.remove(name)
            continue
        if name in active:
            raise ValueError("cyclic include graph")
        if name in visited:
            continue
        if name not in files:
            raise ValueError("unlisted reachable include")
        active.add(name)
        visited.add(name)
        stack.append((name, True))
        data = read_yaml(files[name])
        for include in _tree(data):
            if not isinstance(include, str):
                raise ValueError("include must be a relative filename")
            target = PurePosixPath(name).parent / include
            _relative(root, target.as_posix())
            stack.append((target.as_posix(), False))
    return visited


def _header(data):
    if not isinstance(data, dict) or set(data) != FIELDS:
        raise ValueError("manifest has missing or unknown fields")
    if type(data["schema_version"]) is not int or data["schema_version"] != 1:
        raise ValueError("unsupported manifest version")
    if data["python_hash_seed"] != "0" or os.environ.get("PYTHONHASHSEED") != "0":
        raise ValueError("explicit Python hash seed 0 required")
    if not isinstance(data["run_id"], str) or not re.fullmatch(r"[a-zA-Z0-9_-]{1,80}", data["run_id"]):
        raise ValueError("invalid run identity")
    _hash(data["source_revision"], 40)
    contract = load_contract()
    if (json.dumps(data["contract"], sort_keys=True, allow_nan=False)
            != json.dumps(contract, sort_keys=True, allow_nan=False)
            or data["case_id"] != contract["case_id"]):
        raise ValueError("contract differs from reviewed case")
    if data["outputs"] != OUTPUTS:
        raise ValueError("output contract changed")


def verify_manifest(path):
    path = Path(path).resolve()
    try:
        data = json.loads(path.read_text(encoding="utf-8-sig"), object_pairs_hook=_pairs,
                          parse_constant=lambda value: (_ for _ in ()).throw(ValueError(value)))
        _header(data)
        root = path.parent
        files = _file_manifest(root, data["files"])
        source = data["source"]
        if not isinstance(source, dict) or set(source) != {"path", "sha256"}:
            raise ValueError("invalid source identity")
        if source["path"] not in files or digest(files[source["path"]]) != source["sha256"]:
            raise ValueError("source hash mismatch")
        _relative(root, data["master"])
        _closure(root, data["master"], files)
        return {**data, "_root": root}
    except (OSError, TypeError, KeyError, json.JSONDecodeError) as error:
        raise ValueError("invalid audited manifest") from error
