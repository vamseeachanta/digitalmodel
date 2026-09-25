"""Fourth review of PR #2167 (C11/C15): one redacting output sink.

Round 3 redacted diagnostics one call site at a time, and round 4 found the
sites it missed: raw git stderr, exception text and tracebacks, and the
``--update-baseline`` acknowledgement. The fix is structural:

* every line the checker writes goes through ``emit()``, which replaces any
  deny-listed name (private, hashed or legacy-hashed) and any public pattern
  class (user-profile path, UNC share, host name ...) with
  ``<redacted:digest12>``; ``--show-lines`` (local only) disables it;
* git failures print the command and exit status, never git's stderr;
* ``main()`` catches every exception and prints its type and the stage only;
* a static test fails if a bare ``print(`` returns to the module.

Every identifier here is synthetic and assembled at run time.
"""

from __future__ import annotations

import ast
import hashlib
import importlib.util
import os
import shutil
import subprocess
import sys
from pathlib import Path

import pytest
import yaml

REPO = Path(__file__).resolve().parents[2]
CHECKER = REPO / "scripts" / "legal" / "check_identifiers.py"
RULES_NAME = ".legal-deny-list.yaml"
RULES = REPO / RULES_NAME
ENV = "DIGITALMODEL_DENY_LIST"
TOKEN = "zzsynthetic" + "fourthname"
LEGACY_TOKEN = "zzsynthetic" + "legacyname"
LEGACY_SALT = "zz-legacy-" + "salt"
PRIVATE_NAME = "zzprivate" + "alphaname"
PRIVATE_PHRASE = "Zzmulti " + "Zzwordcorp"
USER = "jdoe" + "789"
BS = "\\"
_GIT_BINDINGS = ("GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR", "GIT_INDEX_FILE")

pytestmark = pytest.mark.skipif(
    not CHECKER.exists() or not RULES.exists(), reason="identifier gate not installed"
)


def _rules() -> dict:
    rules = yaml.safe_load(RULES.read_text(encoding="utf-8"))
    salt = str(rules.get("salt", ""))
    rules["hashed_names"] = list(rules.get("hashed_names") or []) + [
        hashlib.sha256(f"{salt}:{TOKEN}".encode()).hexdigest()
    ]
    if "legacy_hashed_names" in rules:
        rules["legacy_hashed_names"] = {
            "salt": LEGACY_SALT,
            "hashes": [
                hashlib.sha256(f"{LEGACY_SALT}:{LEGACY_TOKEN}".encode()).hexdigest()
            ],
        }
    return rules


def _user_path() -> str:
    return BS.join(["C:", "Users", USER, "Documents", "model.yml"])


def _unc() -> str:
    return BS * 2 + BS.join(["fileserver" + "07", "projects", "model.yml"])


@pytest.fixture()
def gate(tmp_path):
    root = tmp_path / "repo"
    (root / "scripts" / "legal").mkdir(parents=True)
    shutil.copy(CHECKER, root / "scripts" / "legal" / "check_identifiers.py")
    (root / RULES_NAME).write_text(yaml.safe_dump(_rules()), encoding="utf-8")
    home = tmp_path / "home"
    home.mkdir()

    def run(*args, env=None):
        e = {k: v for k, v in os.environ.items() if k not in _GIT_BINDINGS}
        for k in (ENV, "CI", "GITHUB_ACTIONS"):
            e.pop(k, None)
        e["HOME"] = str(home)
        e["USERPROFILE"] = str(home)
        if env:
            e.update(env)
        return subprocess.run(
            [sys.executable, str(root / "scripts" / "legal" / "check_identifiers.py")]
            + list(args),
            cwd=root,
            capture_output=True,
            text=True,
            env=e,
            encoding="utf-8",
            errors="replace",
        )

    run.root = root
    run.home = home
    return run


_COUNTER = [0]


def _module(gate):
    _COUNTER[0] += 1
    name = f"_ci_r4_under_test_{_COUNTER[0]}"
    spec = importlib.util.spec_from_file_location(
        name, gate.root / "scripts" / "legal" / "check_identifiers.py"
    )
    mod = importlib.util.module_from_spec(spec)
    sys.modules[name] = mod
    spec.loader.exec_module(mod)
    return mod


@pytest.fixture()
def mod(gate, monkeypatch):
    for k in (ENV, "CI", "GITHUB_ACTIONS"):
        monkeypatch.delenv(k, raising=False)
    monkeypatch.setenv("HOME", str(gate.home))
    monkeypatch.setenv("USERPROFILE", str(gate.home))
    return _module(gate)


def _main(mod, monkeypatch, capsys, *args):
    monkeypatch.setattr(sys, "argv", ["check_identifiers.py", *args])
    rc = mod.main()
    cap = capsys.readouterr()
    return rc, cap.out + cap.err


# -- 1. the sink ---------------------------------------------------------------


def _calls(tree: ast.AST):
    for node in ast.walk(tree):
        if isinstance(node, ast.Call):
            yield node


def _enclosing_functions(tree: ast.AST) -> dict[int, str]:
    owner: dict[int, str] = {}
    for fn in ast.walk(tree):
        if isinstance(fn, (ast.FunctionDef, ast.AsyncFunctionDef)):
            for node in ast.walk(fn):
                owner.setdefault(id(node), fn.name)
    return owner


class TestOneOutputSink:
    tree = ast.parse(CHECKER.read_text(encoding="utf-8")) if CHECKER.exists() else None

    def test_no_bare_print_in_the_module(self):
        bad = [
            n.lineno
            for n in _calls(self.tree)
            if isinstance(n.func, ast.Name) and n.func.id == "print"
        ]
        assert not bad, f"print() bypasses emit() at lines {bad}"

    def test_only_emit_writes_to_a_standard_stream(self):
        owner = _enclosing_functions(self.tree)
        bad = []
        for n in _calls(self.tree):
            f = n.func
            if (
                isinstance(f, ast.Attribute)
                and f.attr in ("write", "writelines")
                and isinstance(f.value, ast.Attribute)
                and f.value.attr in ("stdout", "stderr")
                and owner.get(id(n)) != "emit"
            ):
                bad.append(n.lineno)
        assert not bad, f"stream writes outside emit() at lines {bad}"

    def test_exit_carries_a_status_never_a_message(self):
        # sys.exit("text") prints the text to stderr, outside emit().
        bad = []
        for n in _calls(self.tree):
            f = n.func
            is_exit = (isinstance(f, ast.Name) and f.id in ("SystemExit", "exit")) or (
                isinstance(f, ast.Attribute)
                and f.attr == "exit"
                and isinstance(f.value, ast.Name)
                and f.value.id == "sys"
            )
            if not is_exit or not n.args:
                continue
            arg = n.args[0]
            if isinstance(arg, ast.Constant) and isinstance(arg.value, int):
                continue
            if isinstance(arg, (ast.Name, ast.Call)):
                continue
            bad.append(n.lineno)
        assert not bad, f"exit with a message at lines {bad}"

    def test_no_traceback_printer(self):
        bad = [
            n.lineno
            for n in _calls(self.tree)
            if isinstance(n.func, ast.Attribute)
            and n.func.attr in ("print_exc", "print_exception", "print_stack")
        ]
        assert not bad, bad


class TestEmitRedacts:
    def _emit(self, mod, capsys, text, rules=None, show=False):
        mod.set_output_policy(rules, show=show)
        try:
            mod.emit(text)
            mod.emit(text, err=True)
        finally:
            mod.set_output_policy(None)
        cap = capsys.readouterr()
        return cap.out + cap.err

    def test_a_hashed_name(self, mod, capsys):
        out = self._emit(mod, capsys, f"x {TOKEN}-results y", mod.load_rules())
        assert TOKEN not in out and "<redacted:" in out, out

    def test_a_legacy_hashed_name(self, mod, capsys):
        rules = mod.load_rules()
        if "legacy_hashed_names" not in rules:
            pytest.skip("this repository carries no legacy block")
        out = self._emit(mod, capsys, f"x {LEGACY_TOKEN} y", rules)
        assert LEGACY_TOKEN not in out and "<redacted:" in out, out

    def test_private_names_and_phrases(self, gate, mod, capsys, monkeypatch):
        private = gate.home / "private.txt"
        private.write_text(f"{PRIVATE_NAME}\n{PRIVATE_PHRASE}\n", encoding="utf-8")
        monkeypatch.setenv(ENV, str(private))
        text = f"a {PRIVATE_NAME.upper()} b {PRIVATE_PHRASE.lower()} c"
        out = self._emit(mod, capsys, text, mod.load_rules())
        assert PRIVATE_NAME not in out.lower(), out
        assert PRIVATE_PHRASE.lower() not in out.lower(), out

    def test_a_private_pattern(self, gate, mod, capsys, monkeypatch):
        private = gate.home / "private.txt"
        private.write_text("re:zzpat[0-9]{4}\n", encoding="utf-8")
        monkeypatch.setenv(ENV, str(private))
        out = self._emit(mod, capsys, "see zzpat1234 here", mod.load_rules())
        assert "zzpat1234" not in out, out

    @pytest.mark.parametrize("rules_loaded", [False, True])
    def test_public_pattern_classes(self, mod, capsys, rules_loaded):
        rules = mod.load_rules() if rules_loaded else None
        host = "acme" + "-rds" + "07"
        text = f"a {_user_path()} b {_unc()} c {host} d /home/{USER}/x"
        out = self._emit(mod, capsys, text, rules)
        for leaked in (USER, "fileserver07", host):
            assert leaked not in out, out
        assert "<redacted:" in out

    def test_the_redaction_is_a_stable_digest(self, mod, capsys):
        out = self._emit(mod, capsys, f"{TOKEN}", mod.load_rules())
        tags = [w for w in out.split() if w.startswith("<redacted:")]
        assert len(tags) == 2 and tags[0] == tags[1]
        assert len(tags[0]) == len("<redacted:") + 12 + 1

    def test_clean_text_is_unchanged(self, mod, capsys):
        text = "check_identifiers: scanned 3 file(s); docs/readme.md:4"
        out = self._emit(mod, capsys, text, mod.load_rules())
        assert out == text + "\n" + text + "\n"

    def test_show_disables_redaction(self, mod, capsys):
        out = self._emit(mod, capsys, f"x {TOKEN} y", mod.load_rules(), show=True)
        assert TOKEN in out


def test_show_lines_does_not_persist_in_the_sink(gate, mod, monkeypatch, capsys):
    f = gate.root / "sample.md"
    f.write_text(f"model at {_user_path()}\n", encoding="utf-8")
    _main(mod, monkeypatch, capsys, "--show-lines", str(f))
    mod.emit(f"later {TOKEN} {_user_path()}")
    out = capsys.readouterr().out
    assert USER not in out, out


def test_an_unknown_argument_is_not_echoed(gate):
    r = gate(f"--{TOKEN}")
    assert r.returncode == 2
    assert TOKEN not in r.stdout + r.stderr


# -- 2. git diagnostics ----------------------------------------------------------


class _Done:
    def __init__(self, rc: int, err: bytes):
        self.returncode, self.stdout, self.stderr = rc, b"", err


def _git_stderr() -> bytes:
    return (
        f"fatal: pathspec {TOKEN}/x.md did not match any files\n"
        f'error: \'{TOKEN}\n/y.md\' {_user_path()} "{TOKEN} \\"z\\""\n'
    ).encode()


def test_git_stderr_is_never_printed(gate, mod, monkeypatch, capsys):
    monkeypatch.setattr(
        mod.subprocess, "run", lambda *a, **k: _Done(128, _git_stderr())
    )
    with pytest.raises(SystemExit) as exc:
        _main(mod, monkeypatch, capsys)  # staged mode: git diff
    out = capsys.readouterr()
    text = out.out + out.err
    assert exc.value.code not in (0, None)
    assert "git diff" in text and "128" in text, text
    assert TOKEN not in text and USER not in text and "pathspec" not in text, text


def test_git_stderr_is_shown_only_under_show_lines(gate, mod, monkeypatch, capsys):
    monkeypatch.setattr(
        mod.subprocess, "run", lambda *a, **k: _Done(128, _git_stderr())
    )
    with pytest.raises(SystemExit):
        _main(mod, monkeypatch, capsys, "--show-lines")
    out = capsys.readouterr()
    assert "pathspec" in out.out + out.err


# -- 3. exceptions ----------------------------------------------------------------


def _deny_open(mod, monkeypatch):
    real = open

    def fake(path, *a, **k):
        if TOKEN in os.fspath(path):
            raise PermissionError(13, "Permission denied", os.fspath(path))
        return real(path, *a, **k)

    monkeypatch.setattr(mod, "open", fake, raising=False)


def _assert_sanitized(rc, out, stage):
    assert rc == 4, out
    assert stage in out, out
    assert "PermissionError" in out, out
    assert TOKEN not in out and "Traceback" not in out, out


def test_a_private_list_that_cannot_be_read(gate, mod, monkeypatch, capsys):
    private = gate.home / f"{TOKEN}-list.txt"
    private.write_text("x\n", encoding="utf-8")
    monkeypatch.setenv(ENV, str(private))
    _deny_open(mod, monkeypatch)
    clean = gate.root / "clean.md"
    clean.write_text("nothing\n", encoding="utf-8")
    rc, out = _main(mod, monkeypatch, capsys, str(clean))
    _assert_sanitized(rc, out, "reading private list")


def test_a_baseline_that_cannot_be_read(gate, mod, monkeypatch, capsys):
    baseline = gate.root / f"{TOKEN}-baseline.txt"
    baseline.write_text("", encoding="utf-8")
    _deny_open(mod, monkeypatch)
    clean = gate.root / "clean.md"
    clean.write_text("nothing\n", encoding="utf-8")
    rc, out = _main(mod, monkeypatch, capsys, "--baseline", str(baseline), str(clean))
    _assert_sanitized(rc, out, "reading baseline")


def test_a_scanned_file_that_cannot_be_read(gate, mod, monkeypatch, capsys):
    p = gate.root / f"{TOKEN}.md"
    p.write_text("nothing\n", encoding="utf-8")
    _deny_open(mod, monkeypatch)
    rc, out = _main(mod, monkeypatch, capsys, str(p))
    _assert_sanitized(rc, out, "scanning a file")


def test_a_traceback_is_shown_only_under_local_show_lines(
    gate, mod, monkeypatch, capsys
):
    p = gate.root / f"{TOKEN}.md"
    p.write_text("nothing\n", encoding="utf-8")
    _deny_open(mod, monkeypatch)
    rc, out = _main(mod, monkeypatch, capsys, "--show-lines", str(p))
    assert rc == 4
    assert "Traceback" in out and TOKEN in out


def test_an_unexpected_exception_is_reported_by_type_only(
    gate, mod, monkeypatch, capsys
):
    def boom(*a, **k):
        raise RuntimeError(f"internal detail {TOKEN} {_user_path()}")

    monkeypatch.setattr(mod, "check", boom)
    clean = gate.root / "clean.md"
    clean.write_text("nothing\n", encoding="utf-8")
    rc, out = _main(mod, monkeypatch, capsys, str(clean))
    assert rc == 4 and "RuntimeError" in out, out
    assert TOKEN not in out and USER not in out and "Traceback" not in out


def test_an_unreadable_rules_file_does_not_quote_its_content(gate):
    (gate.root / RULES_NAME).write_text(
        f"structural:\n  - id: {TOKEN}\n    pattern: [unclosed\n  : {{\n",
        encoding="utf-8",
    )
    clean = gate.root / "clean.md"
    clean.write_text("nothing\n", encoding="utf-8")
    r = gate(str(clean))
    out = r.stdout + r.stderr
    assert r.returncode != 0
    assert "rules file" in out and TOKEN not in out and "Traceback" not in out, out


def test_an_invalid_rule_pattern_does_not_quote_it(gate):
    rules = _rules()
    rules["structural"].append({"id": "zz-bad", "pattern": f"({TOKEN}"})
    (gate.root / RULES_NAME).write_text(yaml.safe_dump(rules), encoding="utf-8")
    clean = gate.root / "clean.md"
    clean.write_text("nothing\n", encoding="utf-8")
    r = gate(str(clean))
    out = r.stdout + r.stderr
    assert r.returncode != 0 and "zz-bad" in out
    assert TOKEN not in out, out


# -- 4. the baseline acknowledgement -----------------------------------------------


def test_the_baseline_acknowledgement_does_not_name_a_denied_file(gate):
    p = gate.root / "plain-results.bin"
    p.write_bytes(b"\x00\x01binary\x00payload")
    baseline = gate.root / f"{TOKEN}.txt"
    r = gate("--update-baseline", "--baseline", str(baseline), str(p))
    out = r.stdout + r.stderr
    assert baseline.is_file(), out
    assert "now lists" in out, out
    assert TOKEN not in out, out
