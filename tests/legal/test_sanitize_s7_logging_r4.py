"""Fourth review of PR #2167 (C13/C16): the sanitizer's log is public output.

The audit moved to private storage, but the log still carried the source root
(at INFO), duplicate and unmapped source paths, raw exceptions and map-loading
errors verbatim. The console test read ``capsys.out`` only, so logging was
never checked.

Now every record passes a ``logging.Filter`` that runs the formatted message
through the identifier gate's redactor, and the identifying diagnostics go to
the private audit, with counts or opaque ids in the log.

Every map and path here is synthetic.
"""

from __future__ import annotations

import argparse
import importlib.util
import json
import logging
import sys
from pathlib import Path

import pytest

REPO = Path(__file__).resolve().parents[2]
SCRIPT = REPO / "scripts" / "sanitize_s7_models.py"
MAP_ENV = "DIGITALMODEL_S7_SANITIZE_MAP"
AUDIT_ENV = "DIGITALMODEL_S7_SANITIZE_AUDIT"
REAL = "Zzacmefield"
PROJECT = "zzprojectfolder"
SRC_DIR = "zzsourceroot"

SYNTHETIC = {
    "default_s7_root": "",
    "sanitization_map": {REAL: "deepwater_field_x", f"{REAL} North": "field_y"},
    "category_map": {f"{PROJECT}/mapped": "jumper/generic"},
    "exclusions": [],
}


def _load():
    name = "_s7_sanitize_logging_under_test"
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
    monkeypatch.delenv("DIGITALMODEL_DENY_LIST", raising=False)
    home = tmp_path / "home"
    home.mkdir()
    monkeypatch.setenv("HOME", str(home))
    monkeypatch.setenv("USERPROFILE", str(home))
    root = tmp_path / SRC_DIR
    body = f"General:\n  Comment: {REAL} riser\n"
    mapped = root / PROJECT / "mapped"
    mapped.mkdir(parents=True)
    (mapped / f"{REAL} riser.yml").write_text(body, encoding="utf-8")
    # A duplicate (same content) and an unmapped folder named after the client.
    unmapped = root / PROJECT / f"{REAL} unmapped"
    unmapped.mkdir(parents=True)
    (unmapped / f"{REAL} copy.yml").write_text(body, encoding="utf-8")
    (unmapped / f"{REAL} other.yml").write_text(
        f"General:\n  Comment: {REAL} other\n", encoding="utf-8"
    )
    return {"map": map_path, "src": root, "out": tmp_path / "out", "tmp": tmp_path}


def _args(s, **kw):
    ns = argparse.Namespace(
        s7_root=str(s["src"]),
        output_root=str(s["out"]),
        dry_run=False,
        skip_dat=True,
        verbose=True,
        map=None,
        audit=None,
    )
    for k, v in kw.items():
        setattr(ns, k, v)
    return ns


def _logged(caplog) -> str:
    return "\n".join([caplog.text] + [r.getMessage() for r in caplog.records])


def _assert_clean(text: str, s) -> None:
    assert REAL.lower() not in text.lower(), text
    assert PROJECT not in text, text
    assert SRC_DIR not in text, text
    assert str(s["tmp"]) not in text, text


def test_a_full_run_logs_no_name_and_no_source_path(setup, caplog, capsys):
    mod = _load()
    caplog.set_level(logging.DEBUG)
    rc = mod.run(_args(setup))
    assert rc == 0, _logged(caplog)
    cap = capsys.readouterr()
    _assert_clean(_logged(caplog) + cap.out + cap.err, setup)
    # The debug and warning paths were exercised.
    levels = {r.levelno for r in caplog.records}
    assert logging.DEBUG in levels and logging.WARNING in levels


def test_the_identifying_diagnostics_go_to_the_private_audit(setup, caplog):
    mod = _load()
    caplog.set_level(logging.DEBUG)
    assert mod.run(_args(setup)) == 0
    audit = json.loads(
        (setup["map"].parent / "s7-sanitize-audit.json").read_text(encoding="utf-8")
    )
    text = json.dumps(audit)
    assert json.dumps(str(setup["src"]))[1:-1] in text
    kinds = {d["kind"] for d in audit["diagnostics"]}
    assert {"source_root", "duplicate", "unmapped"} <= kinds


def test_a_processing_error_logs_no_exception_text(setup, caplog, capsys, monkeypatch):
    mod = _load()

    def boom(text, config):
        raise ValueError(f"cannot handle {REAL} at {setup['src']}")

    monkeypatch.setattr(mod, "sanitize_text", boom)
    caplog.set_level(logging.DEBUG)
    assert mod.run(_args(setup)) != 0
    cap = capsys.readouterr()
    _assert_clean(_logged(caplog) + cap.out + cap.err, setup)
    assert "ValueError" in caplog.text


@pytest.mark.parametrize("content", ["{not json", json.dumps({"x": 1}), "[]"])
def test_a_map_loading_error_logs_no_path(setup, caplog, monkeypatch, content):
    mod = _load()
    bad = setup["tmp"] / PROJECT / f"{REAL}-map.json"
    bad.parent.mkdir(parents=True, exist_ok=True)
    bad.write_text(content, encoding="utf-8")
    monkeypatch.setenv(MAP_ENV, str(bad))
    caplog.set_level(logging.DEBUG)
    assert mod.run(_args(setup)) == 2
    _assert_clean(_logged(caplog), setup)


def test_a_missing_map_logs_no_path(setup, caplog, monkeypatch):
    mod = _load()
    monkeypatch.setenv(MAP_ENV, str(setup["tmp"] / PROJECT / f"{REAL}.json"))
    caplog.set_level(logging.DEBUG)
    assert mod.run(_args(setup)) == 2
    _assert_clean(_logged(caplog), setup)


def test_a_missing_source_root_logs_no_path(setup, caplog):
    mod = _load()
    caplog.set_level(logging.DEBUG)
    missing = setup["tmp"] / SRC_DIR / f"{REAL}-absent"
    assert mod.run(_args(setup, s7_root=str(missing))) != 0
    _assert_clean(_logged(caplog), setup)


def test_the_filter_redacts_any_record_on_the_logger(setup, caplog):
    """A future call site that logs a name or a path is redacted anyway."""
    mod = _load()
    cfg = mod.load_sanitize_config()
    mod.install_log_redaction(cfg)
    caplog.set_level(logging.DEBUG)
    user = "C:" + "\\" + "Users" + "\\" + "jdoe" + "321" + "\\" + "x.yml"
    mod.logger.debug("raw %s at %s", REAL, user)
    mod.logger.error("mapped %s", f"{PROJECT}/mapped")
    text = _logged(caplog)
    assert REAL not in text and "jdoe321" not in text and PROJECT not in text, text
    assert "<redacted:" in text


def test_the_root_handler_configured_by_main_carries_the_filter(setup):
    mod = _load()
    handler = logging.StreamHandler()
    mod.install_log_redaction(mod.load_sanitize_config(), handlers=[handler])
    assert any(isinstance(f, mod.RedactingFilter) for f in handler.filters)
