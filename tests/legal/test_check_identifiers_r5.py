"""Fifth review of PR #2167 (C11/C15): phrase matching and the redaction marker.

* Phrase offsets were computed in ``text.lower()`` and applied to the
  original text. Lowercasing U+0130 lengthens it, so enough of them before a
  phrase shifted the replacement past the phrase and left it visible. Phrases
  are now matched with ``re.IGNORECASE`` on the original text.
* A phrase split by a newline or a no-break space was not matched. Any run of
  whitespace now matches between the words of a phrase.
* The marker was ``<redacted:digest12>``, a digest under the public salt, so a
  published tag could be tested offline against candidate names. It is now the
  constant ``[redacted]``.

Every identifier here is synthetic and assembled at run time.
"""

from __future__ import annotations

import importlib.util
import sys
from pathlib import Path

import pytest

REPO = Path(__file__).resolve().parents[2]
CHECKER = REPO / "scripts" / "legal" / "check_identifiers.py"
RULES_NAME = ".legal-deny-list.yaml"
ENV = "DIGITALMODEL_DENY_LIST"
PHRASE_WORDS = ("Zzfifth" + "alpha", "Zzfifth" + "omega")
PHRASE = " ".join(PHRASE_WORDS)
NAME = "zzsynthetic" + "fifthname"
MARKER = "[redacted]"

pytestmark = pytest.mark.skipif(
    not CHECKER.exists() or not (REPO / RULES_NAME).exists(),
    reason="identifier gate not installed",
)

_COUNTER = [0]


@pytest.fixture()
def mod(monkeypatch, tmp_path):
    for k in (ENV, "CI", "GITHUB_ACTIONS"):
        monkeypatch.delenv(k, raising=False)
    monkeypatch.setenv("HOME", str(tmp_path))
    monkeypatch.setenv("USERPROFILE", str(tmp_path))
    _COUNTER[0] += 1
    name = f"_ci_r5_under_test_{_COUNTER[0]}"
    spec = importlib.util.spec_from_file_location(name, CHECKER)
    m = importlib.util.module_from_spec(spec)
    sys.modules[name] = m
    spec.loader.exec_module(m)
    return m


def _redactor(mod):
    # The phrase's words are not separately denied: only the phrase is.
    return mod.Redactor({"salt": "zz-test-salt"}, names=[PHRASE])


def _visible(out: str) -> bool:
    low = out.lower()
    return any(w.lower() in low for w in PHRASE_WORDS)


class TestPhraseMatching:
    def test_dotted_capital_i_before_the_phrase_does_not_shift_it(self, mod):
        text = "\u0130" * 50 + " " + PHRASE + " tail"
        out = _redactor(mod).redact(text)
        assert not _visible(out), out
        assert out.startswith("\u0130" * 50), out
        assert out.endswith(" tail"), out

    def test_mixed_case_after_expanding_characters(self, mod):
        text = "\u0130x\u0130 " + PHRASE.upper() + " and " + PHRASE.lower()
        out = _redactor(mod).redact(text)
        assert not _visible(out), out

    @pytest.mark.parametrize("sep", ["\n", "\u00a0", "  ", "\t", " \r\n "], ids=repr)
    def test_the_phrase_split_by_other_whitespace(self, mod, sep):
        text = "before " + sep.join(PHRASE_WORDS) + " after"
        out = _redactor(mod).redact(text)
        assert not _visible(out), repr(out)
        assert out.startswith("before ") and out.endswith(" after"), repr(out)

    def test_a_phrase_with_irregular_whitespace_in_the_list(self, mod):
        red = mod.Redactor({}, names=["  " + "\u00a0\n".join(PHRASE_WORDS) + " "])
        out = red.redact("x " + PHRASE + " y")
        assert not _visible(out), out

    def test_the_words_alone_are_not_redacted(self, mod):
        text = PHRASE_WORDS[0] + " is not the phrase"
        assert _redactor(mod).redact(text) == text

    def test_a_private_list_phrase_is_matched_across_a_newline(
        self, mod, monkeypatch, tmp_path
    ):
        private = tmp_path / "private.txt"
        private.write_text(PHRASE + "\n", encoding="utf-8")
        monkeypatch.setenv(ENV, str(private))
        mod.set_output_policy(mod.load_rules())
        try:
            red = mod._SINK.redactor
            out = red.redact("a " + "\n".join(PHRASE_WORDS) + " b")
        finally:
            mod.set_output_policy(None)
        assert not _visible(out), out


class TestConstantMarker:
    def test_the_marker_is_constant(self, mod):
        red = mod.Redactor({"salt": "zz-test-salt"}, names=[NAME, PHRASE])
        a = red.redact(NAME)
        b = red.redact(PHRASE)
        c = red.redact("C:" + "\\" + "Users" + "\\" + "jdoe" + "555" + "\\x.yml")
        assert a == b == c == MARKER, (a, b, c)

    def test_no_digest_is_published(self, mod):
        red = mod.Redactor({"salt": "zz-test-salt"}, names=[NAME])
        out = red.redact(f"one {NAME} two {NAME.upper()}")
        assert out == f"one {MARKER} two {MARKER}", out
        assert "<redacted" not in out

    def test_the_marker_is_independent_of_the_salt(self, mod):
        one = mod.Redactor({"salt": "zz-a"}, names=[NAME]).redact(NAME)
        two = mod.Redactor({"salt": "zz-b"}, names=[NAME]).redact(NAME)
        assert one == two == MARKER

    def test_the_marker_does_not_depend_on_the_text(self, mod):
        red = mod.Redactor({}, names=[NAME, NAME + "x"])
        assert red.redact(NAME) == red.redact(NAME + "x") == MARKER


def test_the_redactor_is_documented_with_the_constant_marker():
    text = CHECKER.read_text(encoding="utf-8")
    assert "<redacted:digest12>" not in text
    assert MARKER in text


def test_known_limits_are_documented():
    doc = CHECKER.read_text(encoding="utf-8").split('"""')[1]
    assert "Known limits" in doc
    assert "whitespace" in doc.lower()
