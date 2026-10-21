# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""The rendered examples are reproducible views of declared synthetic inputs."""

import hashlib
import importlib.util
import json
from pathlib import Path

import pytest


ROOT = Path(__file__).resolve().parents[2]


def load_example():
    spec = importlib.util.spec_from_file_location(
        "shell_example",
        ROOT / "examples/code_checks/shell_thickness_reports.py",
    )
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


@pytest.mark.parametrize("checkout_newlines", [b"\n", b"\r\n"])
def test_compact_examples_match_committed_pages_and_manifests(tmp_path, checkout_newlines):
    module = load_example()
    outputs = module.generate(tmp_path, compact=True)
    expected = tmp_path / "checkout"
    expected.mkdir()
    for source in (ROOT / "docs/domains/storage-tanks").iterdir():
        (expected / source.name).write_bytes(
            source.read_text(encoding="utf-8").encode("utf-8").replace(b"\n", checkout_newlines)
        )
    assert len(outputs) == 3
    for output in outputs:
        assert output.is_file()
        assert output.read_bytes() == (expected / output.name).read_text(
            encoding="utf-8"
        ).encode("utf-8")
    manifest = json.loads((tmp_path / "shell-thickness-examples.json").read_text(encoding="utf-8"))
    assert any("5.6.3.1" in gap for gap in manifest["not_evaluated"])
    assert any("5.6.1.1" in gap for gap in manifest["not_evaluated"])
    for filename, digest in manifest["sha256"].items():
        assert hashlib.sha256((tmp_path / filename).read_bytes()).hexdigest() == digest
    tank = (tmp_path / "one-foot-worked-example.html").read_text(encoding="utf-8")
    assert "18.846" in tank and "5.616" in tank
    assert "3 significant figures" in tank
    assert "assumed, based on previous experience" in tank
    assert "NOT_EVALUATED" in tank
    assert "Plotly.newPlot" in tank
    assert "cdn.plot.ly/plotly-" in tank
    assert len(tank.encode()) < 1_000_000


def test_default_example_delivery_is_self_contained(tmp_path):
    outputs = load_example().generate(tmp_path)
    for path in outputs:
        if path.suffix == ".html":
            content = path.read_text(encoding="utf-8")
            assert "<script src=" not in content
            assert "Plotly.newPlot" in content


def test_examples_decode_utf8_on_legacy_locale(tmp_path, monkeypatch):
    original = Path.read_text

    def legacy_read_text(self, encoding=None, errors=None, **kwargs):
        return original(self, encoding=encoding or "cp1252", errors=errors, **kwargs)

    monkeypatch.setattr(Path, "read_text", legacy_read_text)
    test_compact_examples_match_committed_pages_and_manifests(tmp_path, b"\n")
    test_default_example_delivery_is_self_contained(tmp_path)


def test_manifest_uses_lf_when_platform_default_is_crlf(tmp_path, monkeypatch):
    original = Path.write_text

    def windows_write_text(self, data, **kwargs):
        kwargs.setdefault("newline", "\r\n")
        return original(self, data, **kwargs)

    monkeypatch.setattr(Path, "write_text", windows_write_text)
    outputs = load_example().generate(tmp_path, compact=True)
    assert all(b"\r\n" not in path.read_bytes() for path in outputs)


def test_examples_retain_revision_rows_and_machine_readable_evidence_gaps():
    specs, result = load_example().example_specs()
    tank = specs["one-foot-worked-example.html"].document
    vessel = specs["pressure-vessel-example.html"].document
    assert tank.revision == "02"
    assert vessel.revision == "01"
    assert [row.rev for row in tank.revision_history] == ["00", "01", "02"]
    assert [row.rev for row in vessel.revision_history] == ["00", "01"]
    for document in (tank, vessel):
        assert document.revision_history[0].date == "2026-10-19"
        assert document.revision_history[-1].date == "2026-10-21"
    assert tank.revision_history[1].description == (
        "Use design liquid level in both sizing cases and distinguish "
        "pressure-governed utilisation from the nominal floor."
    )
    assert any("5.6.3.1" in gap for gap in result.not_evaluated)
    assert any("5.6.1.1" in gap for gap in result.not_evaluated)
