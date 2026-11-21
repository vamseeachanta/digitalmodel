# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Offline checks of a bounded source review, never a solver qualification."""

import builtins
import copy
import hashlib
import json
from pathlib import Path
import re
import socket

import pytest

ROOT = Path(__file__).resolve().parents[3]
BASE = "8c09f59a0530baa8a29177b6adaf4e7454cee675"
INVENTORY = ROOT / "docs/reports/solver-proof-inventory.json"
REPORT = ROOT / "docs/reports/solver-proof-completeness.html"
EXPECTED = frozenset({
    "src/digitalmodel/solvers/smoke/model_manifest.py",
    "src/digitalmodel/solvers/smoke/model_proof.py",
    "docs/plans/evidence/issue-2082-native.ps1",
    "src/digitalmodel/solvers/ghs/_windows_job.py",
    "src/digitalmodel/solvers/orcaflex/modular_generator/input_manifest.py",
})
TEXT_FIELDS = {
    "source_role", "required_inputs", "optional_outputs", "consumer",
    "child_success", "cleanup_request", "cleanup_observation", "test_execution",
}
ROW_FIELDS = TEXT_FIELDS | {"path", "sha256", "status", "existing_tests", "limitations"}


NATIVE_MODULES = frozenset({"OrcFxAPI", "_winapi"})


def _refuse_native(delegate):
    """Wrap an import function so that NATIVE_MODULES are refused before delegating."""

    def import_offline(name, *args, **kwargs):
        if name.split(".")[0] in NATIVE_MODULES:
            raise AssertionError("native import forbidden")
        return delegate(name, *args, **kwargs)

    return import_offline


@pytest.fixture(autouse=True)
def offline_only(monkeypatch):
    def no_network(*args, **kwargs):
        raise AssertionError("network forbidden")

    monkeypatch.setattr(builtins, "__import__", _refuse_native(builtins.__import__))
    monkeypatch.setattr(socket, "create_connection", no_network)
    monkeypatch.setattr(socket.socket, "connect", no_network)
    monkeypatch.setattr(socket.socket, "connect_ex", no_network)


def _digest(path):
    return hashlib.sha256(path.read_bytes().replace(b"\r\n", b"\n")).hexdigest()


def _load():
    def unique(pairs):
        result = {}
        for key, value in pairs:
            if key in result:
                raise ValueError("duplicate inventory key")
            result[key] = value
        return result

    return json.loads(INVENTORY.read_text(encoding="utf-8"), object_pairs_hook=unique)


def _validate(data, root):
    if set(data) != {"schema_version", "base_revision", "digest_scope", "sources"}:
        raise ValueError("inventory fields differ")
    if type(data["schema_version"]) is not int or data["schema_version"] != 1:
        raise ValueError("inventory version differs")
    if data["base_revision"] != BASE or data["digest_scope"] != "SHA-256 of CRLF-normalized-to-LF source bytes":
        raise ValueError("review basis differs")
    rows = data["sources"]
    if not isinstance(rows, list) or len(rows) != len(EXPECTED):
        raise ValueError("required source coverage differs")
    for row in rows:
        if not isinstance(row, dict) or set(row) != ROW_FIELDS:
            raise ValueError("review fields differ")
    paths = [row["path"] for row in rows]
    if len(set(paths)) != len(paths) or set(paths) != EXPECTED:
        raise ValueError("required source identities differ")
    root = root.resolve()
    for row in rows:
        name = row["path"]
        if row["status"] not in {"reviewed-with-evidence", "NOT_EVALUATED"}:
            raise ValueError(f"unqualified review verdict: {name}")
        for key in TEXT_FIELDS:
            if not isinstance(row[key], str) or not row[key].strip():
                raise ValueError(f"missing {key}: {name}")
        for key in ("existing_tests", "limitations"):
            if not isinstance(row[key], list) or not row[key] or any(
                not isinstance(value, str) or not value.strip() for value in row[key]
            ):
                raise ValueError(f"missing {key}: {name}")
        path = root / name
        if not path.resolve().is_relative_to(root):
            raise ValueError(f"source escapes checkout: {name}")
        try:
            observed = _digest(path)
        except OSError as error:
            raise ValueError(f"unreadable reviewed source: {name}") from error
        if observed != row["sha256"]:
            raise ValueError(f"source changed; reinspect: {name}")


def test_inventory_exact_sources_and_current_bytes():
    _validate(_load(), ROOT)


@pytest.mark.parametrize("mutation", [
    "missing", "duplicate", "digest", "locator", "limitation", "blank_limitation",
    "unknown_row", "unknown_header", "missing_field", "blanket_pass", "base",
])
def test_incomplete_or_substituted_inventory_rejected(mutation):
    data = _load()
    row = data["sources"][0]
    if mutation == "missing":
        data["sources"].pop()
    elif mutation == "duplicate":
        data["sources"][1] = copy.deepcopy(row)
    elif mutation == "digest":
        row["sha256"] = "0" * 64
    elif mutation == "locator":
        row["path"] = "../outside.py"
    elif mutation == "limitation":
        del row["limitations"]
    elif mutation == "blank_limitation":
        row["limitations"] = [""]
    elif mutation == "unknown_row":
        row["unsupported"] = True
    elif mutation == "unknown_header":
        data["unsupported"] = True
    elif mutation == "missing_field":
        del row["required_inputs"]
    elif mutation == "blanket_pass":
        row["status"] = "PASS"
    else:
        data["base_revision"] = "0" * 40
    with pytest.raises(ValueError):
        _validate(data, ROOT)


@pytest.fixture
def synthetic(tmp_path):
    data = _load()
    for row in data["sources"]:
        path = tmp_path / row["path"]
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_bytes(b"identical synthetic source\n")
        row["sha256"] = _digest(path)
    return data, tmp_path


def test_identical_bytes_retain_distinct_paths_and_optional_outputs(synthetic):
    data, root = synthetic
    _validate(data, root)  # No outputs exist; only the five required sources do.
    data["sources"][1]["path"] = data["sources"][0]["path"]
    with pytest.raises(ValueError, match="identities"):
        _validate(data, root)


def test_unreadable_source_names_the_file(synthetic, monkeypatch):
    data, root = synthetic
    name = data["sources"][0]["path"]
    original = Path.read_bytes

    def unreadable(path):
        if path == root / name:
            raise PermissionError("synthetic read denial")
        return original(path)

    monkeypatch.setattr(Path, "read_bytes", unreadable)
    with pytest.raises(ValueError, match=re.escape(name)):
        _validate(data, root)


@pytest.mark.parametrize("mutation", ["missing", "changed", "escape", "crlf"])
def test_source_binding_and_lf_normalization(synthetic, mutation):
    data, root = synthetic
    row = data["sources"][0]
    path = root / row["path"]
    if mutation == "missing":
        path.rename(path.with_suffix(".absent"))
    elif mutation == "changed":
        path.write_bytes(b"changed source\n")
    elif mutation == "escape":
        path.rename(path.with_suffix(".saved"))
        path.symlink_to(root.parent / "unread-external-source")
    else:
        path.write_bytes(b"identical synthetic source\r\n")
        _validate(data, root)
        return
    with pytest.raises(ValueError, match=re.escape(row["path"])):
        _validate(data, root)


def test_report_has_bounded_contract_and_local_evidence():
    report = REPORT.read_text(encoding="utf-8")
    for marker in ("Summary", "Revision history", "First slice", "NOT_EVALUATED",
                   "hard links", "authentication", "engineering acceptance",
                   "optional licensed API", "diagnostics", "before and after"):
        assert marker in report
    assert report.index("Summary") < report.index("Revision history") < report.index("First slice")
    for target in re.findall(r'href="([^"#]+)(?:#[^"]*)?"', report):
        assert (REPORT.parent / target).resolve().is_relative_to(ROOT)
        assert (REPORT.parent / target).is_file(), target
    for row in _load()["sources"]:
        assert row["path"] in report
        for reference in row["existing_tests"]:
            name, function = reference.split("::")
            assert name.startswith("tests/") and ".." not in Path(name).parts
            assert f"def {function}(" in (ROOT / name).read_text(encoding="utf-8")


def test_offline_guards_reject_network_and_native_import():
    with pytest.raises(AssertionError, match="network forbidden"):
        socket.create_connection(("invalid", 1))
    # The autouse fixture installed this module's guard, and an import
    # statement goes through it.
    assert builtins.__import__.__qualname__ == "_refuse_native.<locals>.import_offline"
    with pytest.raises(AssertionError, match="native import forbidden"):
        import _winapi  # noqa: F401
    # Every refused name, the licensed module included, is proven on the same
    # guard with a delegate that never imports: a broken guard fails here and
    # cannot load the native module.
    delegated = []
    guard = _refuse_native(lambda name, *args, **kwargs: delegated.append(name))
    for root in ("OrcFxAPI", "_winapi"):
        for name in (root, f"{root}.sub"):
            with pytest.raises(AssertionError, match="native import forbidden"):
                guard(name)
    guard("json")
    assert delegated == ["json"]


@pytest.mark.parametrize("key", sorted(ROW_FIELDS))
def test_each_missing_review_field_rejected(key):
    data = _load()
    del data["sources"][0][key]
    with pytest.raises(ValueError, match="review fields"):
        _validate(data, ROOT)
