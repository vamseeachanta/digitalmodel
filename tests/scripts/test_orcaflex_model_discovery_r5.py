"""Fifth review of PR #2167 (C13), finding 6: YAML model discovery.

* ``--- # comment`` and ``... # comment`` are document markers; only a bare
  ``---`` was skipped, so a commented marker ended the preamble and a header
  more than 4,096 characters later was missed.
* A quoted ``"General":`` or ``'General':`` key was not recognised.
* A comment-only file holding ``# Type: Model`` was accepted, and as a
  ``.yml`` it displaced a valid same-stem ``.dat``. The marker now counts only
  in the leading comment block of a file that also has a content line.
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
    if path == BENCH:
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
def mod(request, monkeypatch):
    if request.param == "gen":
        return _load(GEN, "_gen_r5_under_test", monkeypatch)
    return _load(BENCH, "_bench_r5_under_test", monkeypatch)


def _write(root: Path, text: str, name: str = "m.yml") -> Path:
    p = root / name
    p.write_text(text, encoding="utf-8")
    return p


_LONG = "".join(f"# note {i:04d} " + "y" * 60 + "\n" for i in range(100))


# -- document markers -------------------------------------------------------------


@pytest.mark.parametrize(
    "marker", ["--- # document", "---   #x", "... # end", "---\t# tab"], ids=repr
)
def test_a_commented_document_marker_then_a_long_comment_block(mod, tmp_path, marker):
    assert len(_LONG) > 4096
    text = f"%YAML 1.1\n{marker}\n{_LONG}General:\n  x: 1\n"
    assert mod._is_model_yml(_write(tmp_path, text))


def test_a_marker_with_trailing_content_is_not_a_bare_marker(mod, tmp_path):
    # "--- General" is not a marker line followed by a comment; it is content
    # that is not a model header.
    text = "--- basecaseName: x\n" + _LONG + "General:\n  x: 1\n"
    assert not mod._is_model_yml(_write(tmp_path, text))


# -- quoted keys --------------------------------------------------------------------


@pytest.mark.parametrize("key", ['"General":', "'General':", '"General" :'])
def test_a_quoted_general_key(mod, tmp_path, key):
    assert mod._is_model_yml(_write(tmp_path, f"---\n{key}\n  x: 1\n"))


def test_a_quoted_general_after_other_content(mod, tmp_path):
    text = "# Program: OrcaFlex\n---\nUnitsSystem: SI\n'General':\n  x: 1\n"
    assert mod._is_model_yml(_write(tmp_path, text))


def test_a_different_quoted_key_is_not_a_model(mod, tmp_path):
    assert not mod._is_model_yml(_write(tmp_path, '---\n"Generals":\n  x: 1\n'))


# -- the type comment --------------------------------------------------------------


def test_a_comment_only_file_with_the_type_marker_is_not_a_model(mod, tmp_path):
    assert not mod._is_model_yml(_write(tmp_path, "# Type: Model\n"))
    assert not mod._is_model_yml(
        _write(tmp_path, "%YAML 1.1\n# Type: Model\n---\n# nothing\n...\n", "b.yml")
    )


def test_the_type_marker_in_the_leading_block_with_content(mod, tmp_path):
    text = "%YAML 1.1\n# Type: Model\n# Program: OrcaFlex\n---\nUnitsSystem: SI\n"
    assert mod._is_model_yml(_write(tmp_path, text))


def test_the_type_marker_after_content_does_not_count(mod, tmp_path):
    text = "basecaseName: x.dat\n# Type: Model\nloadcases: []\n"
    assert not mod._is_model_yml(_write(tmp_path, text))


def test_a_comment_only_yml_does_not_displace_its_dat(mod, tmp_path):
    root = tmp_path / "models"
    root.mkdir()
    _write(root, "# Type: Model\n", "Riser.yml")
    (root / "Riser.dat").write_bytes(b"\x00binary model\x00")
    found = mod.discover_model_files(root)
    assert [p.name for p in found] == ["Riser.dat"], found
