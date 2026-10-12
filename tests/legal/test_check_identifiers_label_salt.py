"""Path and office-member labels are keyed by a per-run random salt (C11/C15).

``path_label()`` printed ``sha256(path)[:12]``. Anyone can hash candidate
paths offline and compare them with a published CI log, so the digest named
the path. The label is now keyed by a salt from ``secrets.token_hex(16)``,
generated once per ``main()`` call: within one run the same path keeps the
same label, so repeat findings can still be correlated; across runs and
offline the label reveals nothing. ``_member()`` had the same defect and gets
the same fix.

Every path and name here is synthetic.
"""

from __future__ import annotations

import hashlib
import importlib.util
import re
import sys
from pathlib import Path

import pytest

REPO = Path(__file__).resolve().parents[2]
CHECKER = REPO / "scripts" / "legal" / "check_identifiers.py"
ENV = "DIGITALMODEL_DENY_LIST"
PATH = "docs/zzsynthetic-" + "labelsalt/summary.md"
OTHER = "docs/zzsynthetic-" + "labelsalt/other.md"

pytestmark = pytest.mark.skipif(not CHECKER.exists(), reason="gate not installed")

_COUNTER = [0]


@pytest.fixture()
def mod(monkeypatch, tmp_path):
    for k in (ENV, "CI", "GITHUB_ACTIONS"):
        monkeypatch.delenv(k, raising=False)
    monkeypatch.setenv("HOME", str(tmp_path))
    monkeypatch.setenv("USERPROFILE", str(tmp_path))
    _COUNTER[0] += 1
    name = f"_ci_label_salt_under_test_{_COUNTER[0]}"
    spec = importlib.util.spec_from_file_location(name, CHECKER)
    m = importlib.util.module_from_spec(spec)
    sys.modules[name] = m
    spec.loader.exec_module(m)
    return m


def _run_main(mod, monkeypatch, fn):
    """Run main() with _run replaced by ``fn``; return what fn recorded."""
    seen: list[str] = []

    def fake_run() -> int:
        seen.extend(fn())
        return 0

    monkeypatch.setattr(mod, "_run", fake_run)
    assert mod.main() == 0
    return seen


def _digest(label: str) -> str:
    m = re.fullmatch(r"<path ([0-9a-f]{12})>", label)
    assert m, label
    return m.group(1)


def _salt_free(text: str) -> set[str]:
    b = text.encode("utf-8")
    variants = {b, text.lower().encode("utf-8"), text.strip().encode("utf-8")}
    out: set[str] = set()
    for v in variants:
        for algo in ("sha256", "sha1", "md5", "sha512", "blake2b", "sha3_256"):
            out.add(hashlib.new(algo, v).hexdigest()[:12])
    return out


class TestPathLabel:
    def test_same_path_same_label_within_one_run(self, mod, monkeypatch):
        a, b, c = _run_main(
            mod,
            monkeypatch,
            lambda: [mod.path_label(PATH), mod.path_label(PATH), mod.path_label(OTHER)],
        )
        assert a == b
        assert a != c

    def test_labels_differ_between_two_runs(self, mod, monkeypatch):
        (first,) = _run_main(mod, monkeypatch, lambda: [mod.path_label(PATH)])
        (second,) = _run_main(mod, monkeypatch, lambda: [mod.path_label(PATH)])
        assert first != second

    def test_label_is_not_a_salt_free_digest(self, mod, monkeypatch):
        (label,) = _run_main(mod, monkeypatch, lambda: [mod.path_label(PATH)])
        digest = _digest(label)
        assert digest != hashlib.sha256(PATH.encode("utf-8")).hexdigest()[:12]
        assert digest not in _salt_free(PATH)

    def test_label_outside_main_is_not_a_salt_free_digest(self, mod):
        # A caller that never enters main() (check() used as a library) still
        # gets a keyed label, never the bare digest.
        label = mod.path_label(PATH)
        assert label == mod.path_label(PATH)
        assert _digest(label) not in _salt_free(PATH)

    def test_the_salt_comes_from_secrets(self, mod, monkeypatch):
        calls: list[int] = []
        real = mod.secrets.token_hex

        def spy(n=None):
            calls.append(n)
            return real(n)

        monkeypatch.setattr(mod.secrets, "token_hex", spy)
        _run_main(mod, monkeypatch, lambda: [mod.path_label(PATH), mod.path_label(OTHER)])
        assert calls == [16]


class TestOfficeMemberLabel:
    def test_member_label_is_keyed_by_the_run_salt(self, mod, monkeypatch):
        name = "word/zzsynthetic-" + "member.xml"
        a, b = _run_main(mod, monkeypatch, lambda: [mod._member(name), mod._member(name)])
        (c,) = _run_main(mod, monkeypatch, lambda: [mod._member(name)])
        assert a == b
        assert a != c
        digest = a.split(" ", 1)[1]
        assert digest not in _salt_free(name)
