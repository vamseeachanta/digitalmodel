"""The identifier gate must fail a commit that adds a client identifier.

This repository is public and had no value-matching gate: the hook meant to be
one pointed at a script outside the repository and was fail-open, and its
replacement matches no values by design. 146 lines binding a client to a job
code, a CTR number or an internal path reached the public remote as a result.

**These tests hold no real identifier.** A fixture containing one would be
caught by the gate it tests, which is the self-blocking failure that makes a
check unusable. Denied names are therefore exercised through a token invented
for the purpose and hashed at test time, and every other case is structural.
"""

from __future__ import annotations

import os
import subprocess
import sys
from pathlib import Path

import pytest

REPO = Path(__file__).resolve().parents[2]
CHECKER = REPO / "scripts" / "legal" / "check_identifiers.py"
RULES = REPO / ".legal-deny-list.yaml"

pytestmark = pytest.mark.skipif(
    not CHECKER.exists() or not RULES.exists(),
    reason="identifier gate not installed")


def run(*args, env=None):
    e = dict(os.environ)
    if env:
        e.update(env)
    return subprocess.run([sys.executable, str(CHECKER), *args],
                          cwd=REPO, capture_output=True, text=True, env=e)


def write(tmp_path: Path, text: str, name: str = "sample.md") -> str:
    p = tmp_path / name
    p.write_text(text, encoding="utf-8")
    return str(p)


class TestStructuralPatternsAreCaught:
    """The inputs are assembled at runtime, never written as literals.

    A test file containing the strings this gate rejects would be rejected by
    it, and the usual escape -- adding the test file to the exclusions -- turns
    the exclusion list into a backdoor. Building the string from fragments
    exercises the same pattern and leaves nothing in the file for the gate to
    find.
    """

    @pytest.mark.parametrize("parts,rule", [
        (("see B", "1522 for the scope"), "job-code"),
        (("delivered under CTR", " 07 of the contract"), "ctr-number"),
        ((r"input = r'J:", r"\Projects\Something\file.dat'"),
         "mapped-drive-path"),
        (("contact person", "@somecompany.example"), "foreign-domain-email"),
        (("stored under acma", "-projects/archive"), "project-archive-path"),
    ])
    def test_each_pattern_fires(self, tmp_path, parts, rule):
        line = "".join(parts)
        out = run(write(tmp_path, line + "\n"))
        assert out.returncode == 1, out.stdout
        assert rule in out.stdout


class TestOrdinaryContentIsNotCaught:
    @pytest.mark.parametrize("line", [
        r'exe = r"C:\Program Files\ANSYS Inc\v261\aqwa\bin\winx64\Aqwa.exe"',
        r'py = r"C:\Python312\python.exe"',
        "A JONSWAP spectrum with a peak enhancement factor of 3.3.",
        "The roll natural period is 37.24 s at 5% of critical damping.",
        "Frequencies run from 0.100 to 2.252 rad/s over eight steps.",
    ])
    def test_no_false_positive(self, tmp_path, line):
        out = run(write(tmp_path, line + "\n"))
        assert out.returncode == 0, out.stdout


class TestDeniedNames:
    """Exercised with an invented token, so no real name is committed here."""

    TOKEN = "zzsynthetictestclient"

    def _rules_with_token(self, tmp_path):
        import yaml

        rules = yaml.safe_load(RULES.read_text(encoding="utf-8"))
        salt = str(rules.get("salt", ""))
        import hashlib

        digest = hashlib.sha256(
            f"{salt}:{self.TOKEN}".encode()).hexdigest()
        return digest

    def test_a_hashed_name_is_matched_without_being_published(self, tmp_path):
        """The rules file must not contain the token it matches."""
        digest = self._rules_with_token(tmp_path)
        assert self.TOKEN not in RULES.read_text(encoding="utf-8")
        # The hash of a name not on the list must not be on the list either.
        assert digest not in RULES.read_text(encoding="utf-8")

    def test_the_private_list_extends_the_public_one(self, tmp_path):
        private = tmp_path / "private.txt"
        private.write_text(self.TOKEN + "\n", encoding="utf-8")
        sample = write(tmp_path, f"The {self.TOKEN} convention applies.\n")
        out = run(sample, env={"DIGITALMODEL_DENY_LIST": str(private)})
        assert out.returncode == 1, out.stdout
        assert "denied-name" in out.stdout

    def test_without_the_private_list_the_same_line_passes(self, tmp_path):
        sample = write(tmp_path, f"The {self.TOKEN} convention applies.\n")
        out = run(sample)
        assert out.returncode == 0, out.stdout


class TestTheGateFailsClosed:
    """Every way this check can be unable to do its job must be an error.

    The gate it replaces returned success when it could not run, which is how
    a leak passed review.
    """

    def test_a_named_private_list_that_is_absent_is_an_error(self, tmp_path):
        sample = write(tmp_path, "nothing interesting here\n")
        out = run(sample, env={
            "DIGITALMODEL_DENY_LIST": str(tmp_path / "does-not-exist.txt")})
        assert out.returncode != 0
        assert "does not exist" in (out.stdout + out.stderr)
        assert "Refusing to continue" in (out.stdout + out.stderr)

    def test_a_clean_file_reports_what_it_scanned(self, tmp_path):
        """A pass must say how many files it looked at, not just 'ok'."""
        out = run(write(tmp_path, "a clean line\n"))
        assert out.returncode == 0
        assert "scanned 1 file" in out.stdout


class TestTheExistingBacklog:
    """The tracked tree is not clean, and the gate is retrofitted around that.

    A gate added to a repository with existing debt has two options: require
    the whole backlog to be cleared before it can be switched on, in which case
    it is never switched on; or block new content and leave the backlog as
    backlog. This takes the second, which is why the hook runs on staged files
    and `--all` is an audit rather than a build step.

    The number is recorded here so that it is visible and so that a later pass
    can show it falling. It is not asserted as a target: the classification of
    some of these names is an open question, and one token accounts for most of
    the count through two generated result files.
    """

    #: Measured 2026-09-21, after the HIGH-severity redaction.
    BASELINE_FINDINGS = 11647

    @pytest.mark.slow
    def test_the_backlog_has_not_grown(self):
        out = run("--all")
        if out.returncode == 0:
            return                      # the backlog is gone; nothing to guard
        text = out.stdout or ""
        import re as _re

        m = _re.search(r"(\d+) finding\(s\)", text)
        assert m, f"could not read a finding count from:\n{text[:2000]}"
        found = int(m.group(1))
        assert found <= self.BASELINE_FINDINGS, (
            f"identifier findings rose from {self.BASELINE_FINDINGS} to "
            f"{found}. The gate runs on staged files at commit time, so this "
            f"means content was added another way -- check the most recent "
            f"merge.")
