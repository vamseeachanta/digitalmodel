"""Third review of PR #2167: the sanitizer audit is the private map again.

The audit records every replacement as ``'<real>' -> '<neutral>'`` and every
source path, so writing it to ``docs/domains/orcaflex/sanitization_audit.json``
published the de-identification key the map migration had just moved private.
The audit now goes to private storage -- ``--audit``, else
``DIGITALMODEL_S7_SANITIZE_AUDIT``, else next to the private map -- and the
script refuses a location inside the repository or the public output tree.

Every map here is synthetic.
"""

from __future__ import annotations

import argparse
import importlib.util
import json
import sys
from pathlib import Path

import pytest

REPO = Path(__file__).resolve().parents[2]
SCRIPT = REPO / "scripts" / "sanitize_s7_models.py"
MAP_ENV = "DIGITALMODEL_S7_SANITIZE_MAP"
AUDIT_ENV = "DIGITALMODEL_S7_SANITIZE_AUDIT"
REAL = "Acmefield"

SYNTHETIC = {
    "default_s7_root": "",
    "sanitization_map": {REAL: "deepwater_field_x"},
    "category_map": {"acme": "jumper/generic"},
    "exclusions": [],
}


def _load():
    name = "_s7_sanitize_audit_under_test"
    spec = importlib.util.spec_from_file_location(name, SCRIPT)
    mod = importlib.util.module_from_spec(spec)
    sys.modules[name] = mod
    spec.loader.exec_module(mod)
    return mod


@pytest.fixture
def setup(monkeypatch, tmp_path):
    private = tmp_path / "private"
    private.mkdir()
    map_path = private / "s7-map.json"
    map_path.write_text(json.dumps(SYNTHETIC), encoding="utf-8")
    monkeypatch.setenv(MAP_ENV, str(map_path))
    monkeypatch.delenv(AUDIT_ENV, raising=False)
    home = tmp_path / "home"
    home.mkdir()
    monkeypatch.setenv("HOME", str(home))
    monkeypatch.setenv("USERPROFILE", str(home))
    src = tmp_path / "s7" / "acme"
    src.mkdir(parents=True)
    (src / f"{REAL} riser.yml").write_text(
        f"General:\n  Comment: {REAL} riser\n", encoding="utf-8"
    )
    out = tmp_path / "out"
    return {"map": map_path, "src": tmp_path / "s7", "out": out, "tmp": tmp_path}


def _args(s, **kw):
    ns = argparse.Namespace(
        s7_root=str(s["src"]),
        output_root=str(s["out"]),
        dry_run=False,
        skip_dat=True,
        verbose=False,
        map=None,
    )
    for k, v in kw.items():
        setattr(ns, k, v)
    return ns


def _public_text(root: Path) -> str:
    return "\n".join(
        p.read_text(encoding="utf-8", errors="replace")
        for p in root.rglob("*")
        if p.is_file()
    )


def test_the_audit_is_written_next_to_the_private_map(setup):
    mod = _load()
    assert mod.run(_args(setup)) == 0
    audit = setup["map"].parent / "s7-sanitize-audit.json"
    assert audit.is_file()
    # The private audit keeps its evidence.
    assert REAL in audit.read_text(encoding="utf-8")


def test_the_public_output_carries_no_audit_and_no_real_name(setup):
    mod = _load()
    assert mod.run(_args(setup)) == 0
    out = setup["out"]
    assert not list(out.rglob("sanitization_audit.json"))
    text = _public_text(out)
    assert REAL not in text
    assert str(setup["src"]) not in text
    assert "->" not in text


def test_the_console_summary_carries_counts_only(setup, capsys):
    mod = _load()
    assert mod.run(_args(setup)) == 0
    printed = capsys.readouterr().out
    assert REAL not in printed and str(setup["src"]) not in printed


def test_the_audit_variable_is_honoured(setup, monkeypatch):
    mod = _load()
    target = setup["tmp"] / "elsewhere" / "audit.json"
    monkeypatch.setenv(AUDIT_ENV, str(target))
    assert mod.run(_args(setup)) == 0
    assert target.is_file()


def test_an_explicit_audit_argument_wins(setup, monkeypatch):
    mod = _load()
    monkeypatch.setenv(AUDIT_ENV, str(setup["tmp"] / "from-env.json"))
    target = setup["tmp"] / "explicit.json"
    assert mod.run(_args(setup, audit=str(target))) == 0
    assert target.is_file()
    assert not (setup["tmp"] / "from-env.json").exists()


def test_an_audit_inside_the_repository_is_refused(setup):
    mod = _load()
    target = REPO / "docs" / "domains" / "orcaflex" / "zz_r3_audit_probe.json"
    try:
        assert mod.run(_args(setup, audit=str(target))) != 0
        assert not target.exists()
        assert not setup["out"].exists()
    finally:
        if target.exists():
            target.unlink()


def test_an_audit_inside_the_output_tree_is_refused(setup):
    mod = _load()
    target = setup["out"] / "sanitization_audit.json"
    assert mod.run(_args(setup, audit=str(target))) != 0
    assert not setup["out"].exists()


def test_the_cli_default_audit_is_not_in_the_repository():
    mod = _load()
    args = mod.parse_args([])
    assert getattr(args, "audit", None) is None
