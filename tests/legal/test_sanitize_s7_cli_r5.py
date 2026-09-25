"""Fifth review of PR #2167 (C13/C16), finding 1: the sanitizer's CLI.

argparse ran before the redacting filter was installed, so an unknown
argument carrying a name was echoed verbatim; and ``run()`` had no exception
boundary, so a filesystem error while hashing a source or writing the audit
printed a traceback quoting the path. Now:

* the filter is installed before parsing, and argparse's usage and errors are
  written through the redactor, with every argument the caller typed treated
  as a phrase to redact;
* ``main()`` reports any exception by its type and the stage it happened in,
  exit 4, no traceback -- unless ``--debug-traceback`` is given on a local
  run; the flag is refused in CI.

Every map, name and path here is synthetic.
"""

from __future__ import annotations

import importlib.util
import json
import os
import subprocess
import sys
from pathlib import Path

import pytest

REPO = Path(__file__).resolve().parents[2]
SCRIPT = REPO / "scripts" / "sanitize_s7_models.py"
MAP_ENV = "DIGITALMODEL_S7_SANITIZE_MAP"
AUDIT_ENV = "DIGITALMODEL_S7_SANITIZE_AUDIT"
REAL = "Zzcli" + "fieldname"
PROJECT = "zzcli" + "projectdir"
USER = "jdoe" + "852"
BS = "\\"

SYNTHETIC = {
    "default_s7_root": "",
    "sanitization_map": {REAL: "field_x"},
    "category_map": {f"{PROJECT}/mapped": "jumper/generic"},
    "exclusions": [],
}

_COUNTER = [0]


def _load():
    _COUNTER[0] += 1
    name = f"_s7_cli_r5_under_test_{_COUNTER[0]}"
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
    for k in (AUDIT_ENV, "DIGITALMODEL_DENY_LIST", "CI", "GITHUB_ACTIONS"):
        monkeypatch.delenv(k, raising=False)
    home = tmp_path / "home"
    home.mkdir()
    monkeypatch.setenv("HOME", str(home))
    monkeypatch.setenv("USERPROFILE", str(home))
    root = tmp_path / PROJECT
    mapped = root / PROJECT / "mapped"
    mapped.mkdir(parents=True)
    (mapped / f"{REAL} riser.yml").write_text(
        f"General:\n  Comment: {REAL}\n", encoding="utf-8"
    )
    return {"map": map_path, "src": root, "out": tmp_path / "out", "tmp": tmp_path}


def _cli(*args, env=None):
    e = {k: v for k, v in os.environ.items() if k not in ("CI", "GITHUB_ACTIONS")}
    if env:
        e.update(env)
    return subprocess.run(
        [sys.executable, "-X", "utf8", str(SCRIPT), *args],
        cwd=REPO,
        capture_output=True,
        text=True,
        encoding="utf-8",
        errors="replace",
        env=e,
    )


def _assert_clean(text: str, s=None) -> None:
    assert REAL.lower() not in text.lower(), text
    assert PROJECT not in text, text
    assert USER not in text, text
    assert "Traceback" not in text, text
    if s is not None:
        assert str(s["tmp"]) not in text, text


# -- argparse ---------------------------------------------------------------------


def test_an_unknown_option_carrying_a_name_is_not_echoed(setup):
    r = _cli(f"--{REAL}-root", "x", "--dry-run")
    out = r.stdout + r.stderr
    assert r.returncode == 2, out
    assert "usage" in out.lower(), out
    _assert_clean(out)


def test_an_unknown_option_value_form_is_not_echoed(setup):
    r = _cli(f"--zzunknown={PROJECT}")
    out = r.stdout + r.stderr
    assert r.returncode == 2, out
    _assert_clean(out)


def test_an_unexpected_positional_path_is_not_echoed(setup):
    user_path = BS.join(["C:", "Users", USER, PROJECT, "model.yml"])
    r = _cli(user_path)
    out = r.stdout + r.stderr
    assert r.returncode == 2, out
    _assert_clean(out)


def test_a_bad_value_error_names_no_argument(setup):
    # "expected one argument" style errors keep the option name only.
    r = _cli("--s7-root")
    out = r.stdout + r.stderr
    assert r.returncode == 2 and "--s7-root" in out, out


def test_parser_errors_are_redacted_in_process(setup, capsys):
    mod = _load()
    with pytest.raises(SystemExit) as exc:
        mod.main([f"--{REAL}", f"/home/{USER}/x.yml"])
    cap = capsys.readouterr()
    assert exc.value.code == 2
    _assert_clean(cap.out + cap.err)
    assert "[redacted]" in cap.out + cap.err


def test_help_still_works(setup):
    r = _cli("--help")
    assert r.returncode == 0 and "--s7-root" in r.stdout, r.stdout + r.stderr


# -- the exception boundary ----------------------------------------------------------


def _main(mod, setup, capsys, *extra):
    rc = mod.main(
        [
            "--s7-root",
            str(setup["src"]),
            "--output-root",
            str(setup["out"]),
            "--skip-dat",
            *extra,
        ]
    )
    cap = capsys.readouterr()
    return rc, cap.out + cap.err


def test_a_hashing_error_prints_no_path(setup, capsys, monkeypatch):
    mod = _load()

    def denied(path):
        raise PermissionError(13, "Permission denied", str(path))

    monkeypatch.setattr(mod, "sha256_of_file", denied)
    rc, out = _main(mod, setup, capsys)
    assert rc == 4, out
    assert "PermissionError" in out and "hashing" in out, out
    _assert_clean(out, setup)


def test_an_audit_creation_error_prints_no_path(setup, capsys):
    mod = _load()
    blocker = setup["tmp"] / f"{PROJECT}-{REAL}"
    blocker.write_text("a file where a folder is needed\n", encoding="utf-8")
    audit = blocker / "sub" / "audit.json"
    rc, out = _main(mod, setup, capsys, "--audit", str(audit))
    assert rc == 4, out
    assert "writing the private audit" in out, out
    assert "Error" in out, out
    _assert_clean(out, setup)
    assert not audit.exists()


def test_an_unexpected_exception_prints_its_type_only(setup, capsys, monkeypatch):
    mod = _load()

    def boom(*a, **k):
        raise RuntimeError(f"detail {REAL} at {setup['src']}")

    monkeypatch.setattr(mod, "discover_model_files", boom)
    rc, out = _main(mod, setup, capsys)
    assert rc == 4 and "RuntimeError" in out, out
    _assert_clean(out, setup)


def test_the_debug_flag_shows_the_traceback_locally(setup, capsys, monkeypatch):
    mod = _load()

    def boom(*a, **k):
        raise RuntimeError(f"detail {REAL}")

    monkeypatch.setattr(mod, "discover_model_files", boom)
    rc, out = _main(mod, setup, capsys, "--debug-traceback")
    assert rc == 4
    assert "Traceback" in out and REAL in out, out


@pytest.mark.parametrize("var", ["CI", "GITHUB_ACTIONS"])
def test_the_debug_flag_is_refused_in_ci(setup, capsys, monkeypatch, var):
    mod = _load()
    monkeypatch.setenv(var, "true")
    called = []
    monkeypatch.setattr(mod, "run", lambda args: called.append(args) or 0)
    rc, out = _main(mod, setup, capsys, "--debug-traceback")
    assert rc == 2, out
    assert "refused in CI" in out, out
    assert not called


def test_a_clean_run_still_succeeds(setup, capsys):
    mod = _load()
    audit = setup["tmp"] / "private" / "audit.json"
    rc, out = _main(mod, setup, capsys, "--audit", str(audit))
    assert rc == 0, out
    assert audit.is_file()
    _assert_clean(out, setup)


def test_known_limits_are_documented():
    doc = SCRIPT.read_text(encoding="utf-8").split('"""')[1]
    assert "Known limits" in doc
    assert "handler" in doc.lower()
