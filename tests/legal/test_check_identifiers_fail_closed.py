"""The gate must not pass content it did not read.

A cross-provider review (#2145) found the checker returned success without
inspecting the content in several ways: UTF-16 and NUL-bearing files were
skipped, a failed ``git`` enumeration read as "nothing to scan", a hyphenated
suffix hid a denied name, one vendor path exempted a whole line, a UNC path
needed four backslashes to match, and staged mode read the working tree rather
than the index. Each is pinned here.

Every test runs an isolated copy of the checker whose rules carry one invented
token, hashed at test time, so no real name is written in this file.
"""

from __future__ import annotations

import hashlib
import os
import shutil
import subprocess
import sys
from pathlib import Path

import pytest
import yaml

REPO = Path(__file__).resolve().parents[2]
CHECKER = REPO / "scripts" / "legal" / "check_identifiers.py"
RULES = REPO / ".legal-deny-list.yaml"
TOKEN = "zzsynthetictestclient"

pytestmark = pytest.mark.skipif(
    not CHECKER.exists() or not RULES.exists(),
    reason="identifier gate not installed")

#: Git variables that would bind a child git to the caller's repository.
_GIT_BINDINGS = ("GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR", "GIT_INDEX_FILE")


def _env():
    e = {k: v for k, v in os.environ.items() if k not in _GIT_BINDINGS}
    e.pop("DIGITALMODEL_DENY_LIST", None)
    return e


@pytest.fixture()
def gate(tmp_path):
    """An isolated repository root with the checker and a rules file that
    denies TOKEN by hash."""
    root = tmp_path / "repo"
    (root / "scripts" / "legal").mkdir(parents=True)
    shutil.copy(CHECKER, root / "scripts" / "legal" / "check_identifiers.py")
    rules = yaml.safe_load(RULES.read_text(encoding="utf-8"))
    salt = str(rules.get("salt", ""))
    rules["hashed_names"] = list(rules.get("hashed_names") or []) + [
        hashlib.sha256(f"{salt}:{TOKEN}".encode()).hexdigest()]
    (root / ".legal-deny-list.yaml").write_text(
        yaml.safe_dump(rules), encoding="utf-8")

    def run(*args, cwd=None):
        return subprocess.run(
            [sys.executable, str(root / "scripts" / "legal" / "check_identifiers.py"),
             *args],
            cwd=cwd or root, capture_output=True, text=True, env=_env())

    run.root = root
    return run


def _file(gate, name, data: bytes) -> str:
    p = gate.root / name
    p.parent.mkdir(parents=True, exist_ok=True)
    p.write_bytes(data)
    return str(p)


class TestHashedNames:
    def test_a_hashed_name_is_detected(self, gate):
        """The earlier test of this never called the matcher."""
        out = gate(_file(gate, "a.md", f"The {TOKEN} scope.\n".encode()))
        assert out.returncode == 1, out.stdout
        assert "denied-name" in out.stdout

    @pytest.mark.parametrize("text", [
        f"{TOKEN}-archive", f"old-{TOKEN}", f"{TOKEN}2", f"x_{TOKEN}_y",
        TOKEN.upper(),
    ])
    def test_a_name_joined_to_other_text_is_still_detected(self, gate, text):
        out = gate(_file(gate, "a.md", f"see {text} here\n".encode()))
        assert out.returncode == 1, out.stdout


class TestContentThatWasSkipped:
    def test_utf16_text_is_read(self, gate):
        data = f"The {TOKEN} scope.\n".encode("utf-16")
        out = gate(_file(gate, "a.txt", data))
        assert out.returncode == 1, out.stdout
        assert "denied-name" in out.stdout

    def test_an_office_document_is_read(self, gate):
        import io
        import zipfile

        buf = io.BytesIO()
        with zipfile.ZipFile(buf, "w") as z:
            z.writestr("[Content_Types].xml", "<Types/>")
            z.writestr("word/document.xml",
                       f"<w:document><w:t>Prepared for {TOKEN}</w:t></w:document>")
        out = gate(_file(gate, "report.docx", buf.getvalue()))
        assert out.returncode == 1, out.stdout
        assert "denied-name" in out.stdout

    def test_uninspectable_binary_fails_closed(self, gate):
        """Content that cannot be read is not content that passed."""
        data = b"%PDF-1.7\n\x00\x01\x02 binary stream \x00\xff"
        out = gate(_file(gate, "a.pdf", data))
        assert out.returncode != 0, out.stdout
        assert "uninspect" in (out.stdout + out.stderr).lower()

    def test_declared_binary_media_is_reported_not_failed(self, gate):
        png = b"\x89PNG\r\n\x1a\n\x00\x00\x00\rIHDR" + b"\x00" * 32
        out = gate(_file(gate, "img.png", png))
        assert out.returncode == 0, out.stdout
        assert "binary media" in out.stdout

    def test_an_oversized_file_fails_closed(self, gate):
        big = gate.root / "big.txt"
        with open(big, "wb") as fh:
            fh.seek(64 * 1024 * 1024 + 1)
            fh.write(b"\n")
        out = gate(str(big))
        assert out.returncode != 0, out.stdout

    def test_a_named_file_that_does_not_exist_is_an_error(self, gate):
        out = gate(str(gate.root / "missing.md"))
        assert out.returncode != 0, out.stdout


class TestPatterns:
    def test_an_ordinary_unc_path_is_caught(self, gate):
        line = "\\" * 2 + "fileserver" + "\\" + "share" + "\\" + "file.dat\n"
        out = gate(_file(gate, "a.md", line.encode()))
        assert out.returncode == 1, out.stdout
        assert "unc-share" in out.stdout

    def test_a_vendor_path_exempts_only_itself(self, gate):
        vendor = "D:" + "\\" + "Python312" + "\\" + "python.exe"
        project = "E:" + "\\" + "Projects" + "\\" + "Something" + "\\" + "f.dat"
        out = gate(_file(gate, "a.md", f"{vendor} {project}\n".encode()))
        assert out.returncode == 1, out.stdout
        assert "mapped-drive-path" in out.stdout

    def test_a_vendor_path_alone_still_passes(self, gate):
        vendor = "D:" + "\\" + "Python312" + "\\" + "python.exe"
        out = gate(_file(gate, "a.md", f"run {vendor}\n".encode()))
        assert out.returncode == 0, out.stdout

    @pytest.mark.parametrize("text", ["b" + "1234_report", "B" + "1234-x",
                                      "job " + "b" + "1234."])
    def test_a_job_code_in_any_case_or_joined_is_caught(self, gate, text):
        out = gate(_file(gate, "a.md", f"{text}\n".encode()))
        assert out.returncode == 1, out.stdout
        assert "job-code" in out.stdout


class TestExemptFilesCarryNoValues:
    """The files the gate exempts are where a real value slips in unseen.

    A real job code was once written into the rules file as the example in a
    comment; the exemption meant nothing caught it. The exemption covers the
    PATTERNS these files must hold, not values, so values are checked here.
    """

    @pytest.mark.parametrize("rel", [".legal-deny-list.yaml",
                                     "scripts/legal/check_identifiers.py"])
    def test_no_denied_name_or_job_code_outside_pattern_lines(self, rel):
        import re

        rules = yaml.safe_load(RULES.read_text(encoding="utf-8"))
        salt = str(rules.get("salt", ""))
        hashed = set(rules.get("hashed_names") or [])
        job = next(r for r in rules["structural"] if r["id"] == "job-code")
        job_rx = re.compile(job["pattern"])
        word = re.compile(r"[A-Za-z][A-Za-z0-9-]{3,}")
        text = (REPO / rel).read_text(encoding="utf-8")
        for n, line in enumerate(text.splitlines(), start=1):
            if line.strip().startswith("pattern:"):
                continue
            m = job_rx.search(line)
            # Synthetic examples are allowed: B1 followed by 234 is the house example.
            if m and not re.fullmatch(r"(?i)b1234", m.group(0)):
                pytest.fail(f"{rel}:{n}: job code in an exempt file")
            for w in word.findall(line):
                for c in {w.lower(), *re.split(r"[-\d]+", w.lower())}:
                    if len(c) >= 4 and hashlib.sha256(
                            f"{salt}:{c}".encode()).hexdigest() in hashed:
                        pytest.fail(f"{rel}:{n}: denied name in an exempt file")


class TestGitEnumeration:
    def _init(self, root):
        env = _env()
        for cmd in (["git", "init", "-q"],
                    ["git", "config", "user.email", "t@example.test"],
                    ["git", "config", "user.name", "t"]):
            subprocess.run(cmd, cwd=root, check=True, env=env)
        return env

    def test_staged_mode_reads_the_index_not_the_working_tree(self, gate):
        root = gate.root
        env = self._init(root)
        p = root / "note.md"
        p.write_text(f"The {TOKEN} scope.\n", encoding="utf-8")
        subprocess.run(["git", "add", "note.md"], cwd=root, check=True, env=env)
        p.write_text("clean now\n", encoding="utf-8")     # unstaged cleanup
        out = gate()
        assert out.returncode == 1, out.stdout
        assert "denied-name" in out.stdout

    def test_a_failed_git_enumeration_is_an_error(self, gate, tmp_path):
        """Outside any repository git fails; that is not 'nothing to scan'."""
        bare = tmp_path / "not-a-repo"
        bare.mkdir()
        # Put the checker's root outside a repository by running --all there.
        out = gate("--all", cwd=bare)
        assert out.returncode != 0, out.stdout
        assert "nothing to scan" not in out.stdout
