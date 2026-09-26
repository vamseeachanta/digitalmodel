"""Fourth review of PR #2167 (C13), finding 5: valid header forms were rejected.

Model discovery read the first 4,096 characters as plain UTF-8, so a BOM
before an initial ``General:`` defeated the anchored match, and a comment
preamble longer than 4,096 characters hid the header. With no ``.dat``
fallback the model disappeared. Discovery now reads ``utf-8-sig`` and skips
any comment or blank preamble line by line, with no size limit.
"""

from __future__ import annotations

import importlib.util
import sys
import types
from pathlib import Path

import pytest

REPO = Path(__file__).resolve().parents[2]
GEN = REPO / "scripts" / "generate_all_specs.py"
BENCH = REPO / "scripts" / "benchmark_model_library.py"


def _load(path: Path, name: str, monkeypatch):
    if name == "_bench_hdr_under_test":
        try:
            import OrcFxAPI  # noqa: F401
        except Exception:  # noqa: BLE001
            monkeypatch.setitem(sys.modules, "OrcFxAPI", types.ModuleType("OrcFxAPI"))
    spec = importlib.util.spec_from_file_location(name, path)
    mod = importlib.util.module_from_spec(spec)
    sys.modules[name] = mod
    spec.loader.exec_module(mod)
    return mod


@pytest.fixture(params=["gen", "bench"])
def is_model(request, monkeypatch):
    if request.param == "gen":
        return _load(GEN, "_gen_hdr_under_test", monkeypatch)._is_model_yml
    return _load(BENCH, "_bench_hdr_under_test", monkeypatch)._is_model_yml


def _write(tmp_path, data: bytes, name="m.yml") -> Path:
    p = tmp_path / name
    p.write_bytes(data)
    return p


def test_a_bom_before_general(is_model, tmp_path):
    assert is_model(_write(tmp_path, b"\xef\xbb\xbfGeneral:\n  x: 1\n"))


def test_a_bom_before_the_type_comment(is_model, tmp_path):
    data = b"\xef\xbb\xbf# Type: Model\n---\nGeneral:\n  x: 1\n"
    assert is_model(_write(tmp_path, data))


def test_a_long_comment_preamble(is_model, tmp_path):
    preamble = "".join(f"# note {i:05d} " + "x" * 60 + "\n" for i in range(400))
    assert len(preamble) > 20000
    data = (preamble + "\n%YAML 1.1\n---\nGeneral:\n  x: 1\n").encode()
    assert is_model(_write(tmp_path, data))


def test_a_long_blank_preamble_with_a_bom(is_model, tmp_path):
    data = b"\xef\xbb\xbf" + b"\n" * 10000 + b"General:\n  x: 1\n"
    assert is_model(_write(tmp_path, data))


def test_a_batch_config_is_still_not_a_model(is_model, tmp_path):
    preamble = "# config\n" * 1000
    data = (preamble + "basecaseName: x.dat\nloadcases: []\n").encode()
    assert not is_model(_write(tmp_path, data))


def test_an_empty_or_comment_only_file_is_not_a_model(is_model, tmp_path):
    assert not is_model(_write(tmp_path, b""))
    assert not is_model(_write(tmp_path, b"# only a comment\n\n", "c.yml"))


def test_a_model_with_a_bom_is_discovered(monkeypatch, tmp_path):
    for path, name in ((GEN, "_gen_hdr_d"), (BENCH, "_bench_hdr_under_test")):
        mod = _load(path, name, monkeypatch)
        root = tmp_path / name
        root.mkdir()
        _write(root, b"\xef\xbb\xbfGeneral:\n  x: 1\n", "Bom.yml")
        found = mod.discover_model_files(root)
        assert [p.name for p in found] == ["Bom.yml"], name
