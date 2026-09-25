"""Third review of PR #2167 (C11/C13/C16): disclosure through side channels.

1. Diagnostics for files the gate cannot read printed the path verbatim, so a
   denied name in a binary's file name reached the log through the
   uninspectable list; an office archive disclosed one through the name of a
   member it could not inspect, or through an exception message.
4. ``legacy_hashed_names: null`` became an empty mapping and silently dropped
   the legacy hashes. An absent block is allowed; a present null is not.
5. ``--show-lines`` set a module global, so a later in-process call without
   the flag -- even in CI -- still quoted lines.

Every identifier here is synthetic and assembled so this file carries nothing
the gate rejects.
"""

from __future__ import annotations

import hashlib
import importlib.util
import io
import os
import shutil
import subprocess
import sys
import zipfile
from pathlib import Path

import pytest
import yaml

REPO = Path(__file__).resolve().parents[2]
CHECKER = REPO / "scripts" / "legal" / "check_identifiers.py"
RULES = REPO / ".legal-deny-list.yaml"
TOKEN = "zzsynthetic" + "reviewname"
BS = "\\"
_GIT_BINDINGS = ("GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR", "GIT_INDEX_FILE")

pytestmark = pytest.mark.skipif(
    not CHECKER.exists() or not RULES.exists(), reason="identifier gate not installed"
)


def _rules_with_token(extra: dict | None = None) -> dict:
    rules = yaml.safe_load(RULES.read_text(encoding="utf-8"))
    salt = str(rules.get("salt", ""))
    rules["hashed_names"] = list(rules.get("hashed_names") or []) + [
        hashlib.sha256(f"{salt}:{TOKEN}".encode()).hexdigest()
    ]
    if extra:
        rules.update(extra)
    return rules


@pytest.fixture()
def gate(tmp_path):
    root = tmp_path / "repo"
    (root / "scripts" / "legal").mkdir(parents=True)
    shutil.copy(CHECKER, root / "scripts" / "legal" / "check_identifiers.py")
    home = tmp_path / "home"
    home.mkdir()

    def write_rules(rules: dict) -> None:
        (root / ".legal-deny-list.yaml").write_text(
            yaml.safe_dump(rules), encoding="utf-8"
        )

    write_rules(_rules_with_token())

    def run(*args, env=None):
        e = {k: v for k, v in os.environ.items() if k not in _GIT_BINDINGS}
        for k in ("DIGITALMODEL_DENY_LIST", "CI", "GITHUB_ACTIONS"):
            e.pop(k, None)
        e["HOME"] = str(home)
        e["USERPROFILE"] = str(home)
        if env:
            e.update(env)
        return subprocess.run(
            [
                sys.executable,
                str(root / "scripts" / "legal" / "check_identifiers.py"),
                *args,
            ],
            cwd=root,
            capture_output=True,
            text=True,
            env=e,
            encoding="utf-8",
            errors="replace",
        )

    run.root = root
    run.home = home
    run.write_rules = write_rules
    return run


def _bin(gate, name: str, data: bytes = b"\x00\x01binary\x00payload") -> Path:
    p = gate.root / name
    p.write_bytes(data)
    return p


# -- 1. uninspectable diagnostics are redacted ---------------------------------


class TestUninspectableDiagnosticsAreRedacted:
    def test_an_unlisted_binary_named_with_a_denied_name(self, gate):
        p = _bin(gate, f"{TOKEN}-results.bin")
        r = gate(str(p))
        out = r.stdout + r.stderr
        assert r.returncode != 0
        assert "not in the baseline" in out
        assert TOKEN not in out, out
        assert "<path " in out

    def test_a_binary_named_with_a_denied_name_changed_since_the_baseline(self, gate):
        name = f"{TOKEN}-results.bin"
        p = _bin(gate, name)
        baseline = gate.root / "baseline.txt"
        baseline.write_text(f"{'0' * 64}  {name}\n", encoding="utf-8")
        r = gate("--baseline", str(baseline), str(p))
        out = r.stdout + r.stderr
        assert r.returncode != 0
        assert "changed since the baseline" in out
        assert TOKEN not in out, out

    def test_a_clean_binary_name_is_still_shown(self, gate):
        p = _bin(gate, "plain-results.bin")
        r = gate(str(p))
        assert r.returncode != 0
        assert "plain-results.bin" in r.stdout

    def test_show_lines_shows_the_name_locally(self, gate):
        p = _bin(gate, f"{TOKEN}-results.bin")
        r = gate("--show-lines", str(p))
        assert TOKEN in r.stdout

    def _docx(self, gate, members: dict[str, bytes], name="report.docx") -> Path:
        buf = io.BytesIO()
        with zipfile.ZipFile(buf, "w", zipfile.ZIP_STORED) as z:
            for n, data in members.items():
                z.writestr(n, data)
        p = gate.root / name
        p.write_bytes(buf.getvalue())
        return p

    def test_an_office_member_name_that_cannot_be_inspected(self, gate):
        p = self._docx(
            gate,
            {
                "word/document.xml": b"<w><t>plain text</t></w>",
                f"word/embeddings/{TOKEN}.bin": b"\x00\x01\x02opaque",
            },
        )
        r = gate(str(p))
        out = r.stdout + r.stderr
        assert r.returncode != 0
        assert "report.docx" in out
        assert TOKEN not in out, out

    def test_an_office_member_that_is_malformed_xml(self, gate):
        p = self._docx(gate, {f"word/{TOKEN}.xml": b"<w><t>broken</w>"})
        r = gate(str(p))
        out = r.stdout + r.stderr
        assert r.returncode != 0
        assert TOKEN not in out, out

    def test_an_office_archive_whose_error_names_a_member(self, gate):
        # A stored member with a flipped data byte fails its CRC check, and
        # zipfile's message names the member.
        p = self._docx(gate, {f"word/{TOKEN}.xml": b"<w><t>abcdefgh</t></w>"})
        raw = bytearray(p.read_bytes())
        i = raw.index(b"abcdefgh")
        raw[i] ^= 0x01
        p.write_bytes(bytes(raw))
        r = gate(str(p))
        out = r.stdout + r.stderr
        assert r.returncode != 0
        assert TOKEN not in out, out

    def test_the_uninspectable_list_returned_by_check_is_redacted(self, gate):
        mod = _module_from(gate)
        p = _bin(gate, f"{TOKEN}-results.bin")
        rules = mod.load_rules()
        digests: dict = {}
        _, _, _, unins = mod.check([str(p)], rules, digests=digests)
        assert unins and not any(TOKEN in u for u in unins), unins

    def test_a_missing_private_list_path_is_not_printed(self, gate):
        missing = gate.home / f"{TOKEN}-list.txt"
        p = gate.root / "clean.md"
        p.write_text("nothing\n", encoding="utf-8")
        r = gate(str(p), env={"DIGITALMODEL_DENY_LIST": str(missing)})
        out = r.stdout + r.stderr
        assert r.returncode != 0
        assert TOKEN not in out, out


# -- 4. an explicit null legacy block fails closed ------------------------------


def _clean(gate) -> str:
    p = gate.root / "clean.md"
    p.write_text("nothing to see\n", encoding="utf-8")
    return str(p)


def test_an_absent_legacy_block_is_allowed(gate):
    rules = _rules_with_token()
    rules.pop("legacy_hashed_names", None)
    gate.write_rules(rules)
    r = gate(_clean(gate))
    assert r.returncode == 0, r.stdout + r.stderr


@pytest.mark.parametrize("value", [None, {}, [], "", 0])
def test_a_present_but_empty_or_null_legacy_block_fails_closed(gate, value):
    gate.write_rules(_rules_with_token({"legacy_hashed_names": value}))
    r = gate(_clean(gate))
    assert r.returncode != 0, r.stdout + r.stderr
    assert "legacy_hashed_names" in r.stdout + r.stderr


def test_the_null_file_really_carries_an_explicit_null(gate):
    gate.write_rules(_rules_with_token({"legacy_hashed_names": None}))
    text = (gate.root / ".legal-deny-list.yaml").read_text(encoding="utf-8")
    assert "legacy_hashed_names: null" in text


def test_check_called_directly_refuses_an_explicit_null(gate):
    mod = _module_from(gate)
    rules = mod.load_rules()
    rules["legacy_hashed_names"] = None
    with pytest.raises(SystemExit):
        mod.check([_clean(gate)], rules)


# -- 5. disclosure policy does not persist across in-process calls --------------


def _module_from(gate, name="_ci_r3_under_test"):
    spec = importlib.util.spec_from_file_location(
        name, gate.root / "scripts" / "legal" / "check_identifiers.py"
    )
    mod = importlib.util.module_from_spec(spec)
    sys.modules[name] = mod
    spec.loader.exec_module(mod)
    return mod


def _user_path() -> str:
    return BS.join(["C:", "Users", "jdoe" + "456", "Documents", "model.yml"])


class TestShowLinesDoesNotPersist:
    @pytest.fixture()
    def mod(self, gate, monkeypatch):
        for k in ("DIGITALMODEL_DENY_LIST", "CI", "GITHUB_ACTIONS"):
            monkeypatch.delenv(k, raising=False)
        monkeypatch.setenv("HOME", str(gate.home))
        monkeypatch.setenv("USERPROFILE", str(gate.home))
        return _module_from(gate)

    def _main(self, mod, monkeypatch, capsys, *args):
        monkeypatch.setattr(sys, "argv", ["check_identifiers.py", *args])
        rc = mod.main()
        cap = capsys.readouterr()
        return rc, cap.out + cap.err

    def test_a_second_call_without_the_flag_does_not_quote(
        self, gate, mod, monkeypatch, capsys
    ):
        f = gate.root / "sample.md"
        f.write_text(f"model at {_user_path()}\n", encoding="utf-8")
        rc1, out1 = self._main(mod, monkeypatch, capsys, "--show-lines", str(f))
        assert rc1 == 1 and "jdoe456" in out1
        rc2, out2 = self._main(mod, monkeypatch, capsys, str(f))
        assert rc2 == 1 and "jdoe456" not in out2
        assert not getattr(mod, "SHOW_LINES", False)

    def test_a_second_call_in_ci_does_not_quote(self, gate, mod, monkeypatch, capsys):
        f = gate.root / "sample.md"
        f.write_text(f"model at {_user_path()}\n", encoding="utf-8")
        self._main(mod, monkeypatch, capsys, "--show-lines", str(f))
        monkeypatch.setenv("CI", "true")
        rc, out = self._main(mod, monkeypatch, capsys, str(f))
        assert rc == 1 and "jdoe456" not in out
        rc, out = self._main(mod, monkeypatch, capsys, "--show-lines", str(f))
        assert rc == 3 and "jdoe456" not in out

    def test_a_second_call_does_not_show_a_path(self, gate, mod, monkeypatch, capsys):
        p = _bin(gate, f"{TOKEN}-results.bin")
        self._main(mod, monkeypatch, capsys, "--show-lines", str(p))
        _, out = self._main(mod, monkeypatch, capsys, str(p))
        assert TOKEN not in out, out
