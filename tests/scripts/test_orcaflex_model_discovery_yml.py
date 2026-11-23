"""Third review of PR #2167 (C13): models whose .dat was removed stay discovered.

C13 deleted the binary .dat of models that have a text .yml twin. The spec
generator and the model-library benchmark discovered ``*.dat`` only, so the
removed models -- all of A01 among them -- dropped out of both. Discovery now
takes OrcaFlex model .yml files as well, one entry per model, preferring the
.yml when both exist. A .yml that is not a model (a batch config, a preserved
record of a lost .sim) is not a model.
"""

from __future__ import annotations

import importlib.util
import sys
import types
from pathlib import Path

import pytest

REPO = Path(__file__).resolve().parents[2]
RAW = REPO / "docs" / "domains" / "orcaflex" / "examples" / "raw"
GEN = REPO / "scripts" / "generate_all_specs.py"
BENCH = REPO / "scripts" / "benchmark_model_library.py"

MODEL_HEAD = (
    "%YAML 1.1\n# Type: Model\n# Program: OrcaFlex 11.6b\n---\nGeneral:\n  x: 1\n"
)
A01 = {
    "A01 Catenary riser",
    "A01 Lazy wave riser",
    "A01 Pliant wave riser",
    "A01 Steep wave riser",
}


def _load(path: Path, name: str, monkeypatch):
    if name == "_bench_under_test":
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
def discover(request, monkeypatch):
    """A function root -> list of discovered model paths, for either script."""
    if request.param == "gen":
        mod = _load(GEN, "_gen_under_test", monkeypatch)
        return lambda root: [e.dat_path for e in mod.discover_models(root)]
    mod = _load(BENCH, "_bench_under_test", monkeypatch)
    return lambda root: mod.discover_models(root)


def test_the_a01_models_are_discovered_again(discover):
    found = discover(RAW)
    stems = {p.stem for p in found}
    assert A01 <= stems
    assert all(p.suffix.lower() == ".yml" for p in found if p.stem in A01)


def test_each_repository_model_is_discovered_once(discover):
    found = discover(RAW)
    keys = [(p.parent, p.stem.lower()) for p in found]
    assert len(keys) == len(set(keys))
    # Every .dat that remains has a .yml twin, and the twin wins.
    for dat in RAW.rglob("*.dat"):
        assert dat not in found
        assert dat.with_suffix(".yml") in found


def test_non_model_yml_is_not_discovered(discover):
    stems = {p.stem for p in discover(RAW)}
    assert "MultipleStatics" not in stems
    assert "PipelayConfig" not in stems
    assert not any(s.endswith("_preserved") for s in stems)


def test_yml_is_preferred_and_a_lone_dat_is_kept(discover, tmp_path):
    (tmp_path / "a").mkdir()
    (tmp_path / "a" / "Twin.dat").write_bytes(b"\x00dat")
    (tmp_path / "a" / "Twin.yml").write_text(MODEL_HEAD, encoding="utf-8")
    (tmp_path / "a" / "Lone.dat").write_bytes(b"\x00dat")
    (tmp_path / "a" / "OnlyText.yml").write_text(MODEL_HEAD, encoding="utf-8")
    (tmp_path / "a" / "config.yml").write_text(
        "basecaseName: x.dat\n", encoding="utf-8"
    )
    found = sorted(p.name for p in discover(tmp_path))
    assert found == ["Lone.dat", "OnlyText.yml", "Twin.yml"]
