# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Exact-source JSON reader contracts and report integration (#1019)."""

import hashlib
from contextlib import contextmanager
from dataclasses import replace
from importlib import import_module
from pathlib import Path
from types import SimpleNamespace

import pytest

from digitalmodel.common.assumption_ledger import (
    AssumptionLedger,
    AssumptionSource,
    Confidence,
)
from digitalmodel.reporting import (
    ContractError,
    DataSource,
    Provenance,
    ProvenanceError,
    ReportSection,
    SectionMode,
    assemble_report,
)


def read(*args, **kwargs):
    return import_module("digitalmodel.reporting.contract_reader").read_declared_json(
        *args, **kwargs
    )


@pytest.fixture
def declared(tmp_path):
    root = tmp_path / "store"
    path = root / "results" / "values.json"
    path.parent.mkdir(parents=True)
    raw = b'{"force": 12.5, "label": "synthetic"}'
    path.write_bytes(raw)
    source = DataSource(
        kind="workflow_results",
        identifier="results/values.json",
        digest="sha256:" + hashlib.sha256(raw).hexdigest(),
    )
    provenance = Provenance(
        sources=[source],
        code_edition="test",
        units_system="SI",
        validated_path="screening",
    )
    section = ReportSection(
        "results",
        "Results",
        SectionMode.ALWAYS,
        lambda data, **kw: f"<p>{data['force']}</p>",
        reads=[source.identifier],
    )
    return root, section, source, provenance


def test_valid_source_is_read_once_relative_to_root(declared, monkeypatch, tmp_path):
    root, section, source, provenance = declared
    original_open = Path.open
    opened, reads = [], []

    @contextmanager
    def recording_open(path, mode):
        assert mode == "rb"
        opened.append(path)
        with original_open(path, mode) as stream:

            def bounded_read(size):
                reads.append(size)
                return stream.read(size)

            yield SimpleNamespace(read=bounded_read)

    monkeypatch.chdir(tmp_path)
    monkeypatch.setattr(Path, "open", recording_open)
    # A value-equal declaration is accepted; object identity is not required.
    result = read(
        Path("store"), section, source.model_copy(), provenance, assumptions=[]
    )
    assert result == {"force": 12.5, "label": "synthetic"}
    assert opened == [root / source.identifier]
    module = import_module("digitalmodel.reporting.contract_reader")
    assert reads == [module.MAX_JSON_BYTES + 1]


@pytest.mark.parametrize("reads", [None, [], ["workflow_results"], ["results/*.json"]])
def test_exact_identifier_required_before_open(declared, monkeypatch, reads):
    root, section, source, provenance = declared
    section = replace(section, reads=reads)

    def forbidden_open(*args, **kwargs):
        pytest.fail("An unauthorized source was opened")

    monkeypatch.setattr(Path, "open", forbidden_open)
    with pytest.raises(ContractError, match="declared"):
        read(root, section, source, provenance, assumptions=[])


@pytest.mark.parametrize(
    "field,value",
    [
        ("digest", "sha256:" + "0" * 64),
        ("kind", "file"),
        ("description", "different declaration"),
        ("identifier", "results/other.json"),
    ],
)
def test_full_source_must_belong_to_provenance(declared, field, value):
    root, section, source, provenance = declared
    other = source.model_copy(update={field: value})
    section.reads = [other.identifier]
    with pytest.raises(ContractError, match="provenance"):
        read(root, section, other, provenance, assumptions=[])


@pytest.mark.parametrize("units", [None, "", " \t"])
def test_units_required_only_by_strict_reader(declared, units):
    root, section, source, provenance = declared
    provenance.units_system = units
    assert provenance.is_complete()
    with pytest.raises(ContractError, match="units_system"):
        read(root, section, source, provenance, assumptions=[])


@pytest.mark.parametrize(
    "field,value",
    [
        ("sources", []),
        ("code_edition", None),
        ("validated_path", None),
    ],
)
def test_existing_metadata_gate_is_preserved(declared, field, value):
    root, section, source, provenance = declared
    setattr(provenance, field, value)
    with pytest.raises(ProvenanceError):
        read(root, section, source, provenance, assumptions=[])


def test_assumptions_must_be_explicit(declared):
    with pytest.raises(TypeError, match="assumptions"):
        read(*declared)


@pytest.mark.parametrize("assumptions", [None, "none", b"none", [""], ["  "], [0], {}])
def test_assumptions_are_a_sequence_of_nonblank_strings(declared, assumptions):
    with pytest.raises(ContractError, match="assumptions"):
        read(*declared, assumptions=assumptions)


@pytest.fixture
def ledger():
    ledger = AssumptionLedger()
    ledger.record(
        "force.units",
        "N",
        AssumptionSource.ASSUMED_DEFAULT,
        "Assumed units for the synthetic force",
        Confidence.LOW,
    )
    return ledger


def test_named_assumption_is_in_the_rendered_ledger(declared, ledger):
    _, section, _, provenance = declared
    data = read(*declared, assumptions=("force.units",), ledger=ledger)
    html = assemble_report(
        "Results", sections=[section], provenance=provenance, data=data, ledger=ledger
    )
    assert "<p>12.5</p>" in html
    assert "<td>force.units</td><td>N</td>" in html
    assert "<td>Assumed units for the synthetic force</td>" in html


def test_named_assumption_requires_a_ledger_before_open(declared, monkeypatch):
    def forbidden_open(*args, **kwargs):
        pytest.fail("Source opened before its assumptions were checked")

    monkeypatch.setattr(Path, "open", forbidden_open)
    with pytest.raises(ContractError, match="ledger"):
        read(*declared, assumptions=["force.units"])


def test_every_named_assumption_requires_an_exact_ledger_field(declared, ledger):
    with pytest.raises(ContractError, match="ledger"):
        read(*declared, assumptions=["force.units", "Force.units"], ledger=ledger)


@pytest.mark.parametrize(
    "digest",
    [
        None,
        "",
        "0" * 64,
        "sha256:abc",
        "sha256:" + "A" * 64,
        "md5:" + "0" * 64,
        "sha256:" + "0" * 64 + "\n",
    ],
)
def test_digest_format_is_required(declared, digest):
    root, section, source, provenance = declared
    source.digest = digest
    with pytest.raises(ContractError, match="digest"):
        read(root, section, source, provenance, assumptions=[])


@pytest.mark.parametrize(
    "identifier",
    [
        "../outside.json",
        "results/../values.json",
        "./values.json",
        "",
        "/values.json",
        "C:/values.json",
        "C:values.json",
        "\\\\server\\share\\values.json",
        "results\\values.json",
        "//server/share/values.json",
        "results//values.json",
        "results/values.json/",
        "results/\x00values.json",
    ],
)
def test_unsafe_identifiers_fail_before_open(declared, monkeypatch, identifier):
    root, section, source, provenance = declared
    source.identifier = identifier
    section.reads = [identifier]

    def forbidden_open(*args, **kwargs):
        pytest.fail("An unsafe identifier was opened")

    monkeypatch.setattr(Path, "open", forbidden_open)
    with pytest.raises(ContractError, match="portable relative path"):
        read(root, section, source, provenance, assumptions=[])


@pytest.mark.parametrize("directory_link", [False, True])
def test_symlink_escape_is_rejected(declared, tmp_path, directory_link):
    root, section, source, provenance = declared
    outside = tmp_path / "outside"
    outside.mkdir()
    target = outside / "values.json"
    target.write_bytes((root / source.identifier).read_bytes())
    link = root / "link"
    try:
        link.symlink_to(
            outside if directory_link else target, target_is_directory=directory_link
        )
    except OSError as error:
        if getattr(error, "winerror", None) == 1314:  # ERROR_PRIVILEGE_NOT_HELD
            pytest.skip("Windows symlink creation requires additional privileges")
        raise
    source.identifier = "link/values.json" if directory_link else "link"
    section.reads = [source.identifier]
    before = target.read_bytes()
    with pytest.raises(ContractError, match="escape"):
        read(root, section, source, provenance, assumptions=[])
    assert target.read_bytes() == before


@pytest.mark.parametrize("missing_root", [False, True])
def test_missing_file_raises_named_error(declared, missing_root):
    root, section, source, provenance = declared
    if missing_root:
        root = root / "missing"
    else:
        source.identifier = "missing.json"
        section.reads = [source.identifier]
    with pytest.raises(ContractError, match="resolve"):
        read(root, section, source, provenance, assumptions=[])


def test_changed_digest_preserves_source(declared):
    root, section, source, provenance = declared
    path = root / source.identifier
    raw = b'{"force": 13}'
    path.write_bytes(raw)
    with pytest.raises(ContractError, match="digest"):
        read(root, section, source, provenance, assumptions=[])
    assert path.read_bytes() == raw


@pytest.mark.parametrize(
    "raw,expected",
    [
        (b"not JSON", "UTF-8 JSON"),
        (b'{"force": 1, "force": 2}', "duplicate"),
        (b'{"nested": {"x": 1, "x": 2}}', "duplicate"),
        (b'{"x": NaN}', "non-finite"),
        (b'{"x": Infinity}', "non-finite"),
        (b'{"x": -Infinity}', "non-finite"),
        (b'{"x": 1e999}', "finite float range"),
        (b'{"x": -1e999}', "finite float range"),
        (b'"\xff"', "UTF-8 JSON"),
        pytest.param(b"[" * 10000 + b"]" * 10000, "UTF-8 JSON", id="deep-arrays"),
    ],
)
def test_invalid_json_preserves_verified_source(declared, raw, expected):
    root, section, source, provenance = declared
    path = root / source.identifier
    path.write_bytes(raw)
    source.digest = "sha256:" + hashlib.sha256(raw).hexdigest()
    with pytest.raises(ContractError, match=expected):
        read(root, section, source, provenance, assumptions=[])
    assert path.read_bytes() == raw


def test_bounded_read_and_exact_limit(declared):
    root, section, source, provenance = declared
    limit = import_module("digitalmodel.reporting.contract_reader").MAX_JSON_BYTES
    path = root / source.identifier
    for size in (limit, limit + 1):
        raw = b'"' + b"x" * (size - 2) + b'"'
        path.write_bytes(raw)
        source.digest = "sha256:" + hashlib.sha256(raw).hexdigest()
        if size == limit:
            assert read(*declared, assumptions=[]) == "x" * (size - 2)
        else:
            with pytest.raises(ContractError, match="limit"):
                read(*declared, assumptions=[])
        assert path.read_bytes() == raw


def test_assembler_uses_verified_data_and_never_builds_on_digest_change(declared):
    root, section, source, provenance = declared
    called = []

    def build(data, **kwargs):
        called.append(True)
        return f"<p>{data['force']}</p>"

    section.builder = build

    def render():
        data = read(*declared, assumptions=[])
        return assemble_report(
            "Synthetic results", provenance=provenance, sections=[section], data=data
        )

    assert "<p>12.5</p>" in render()
    assert called == [True]
    called.clear()
    path = root / source.identifier
    raw = b'{"force": 99}'
    path.write_bytes(raw)
    with pytest.raises(ContractError, match="digest"):
        render()
    assert not called
    assert path.read_bytes() == raw
