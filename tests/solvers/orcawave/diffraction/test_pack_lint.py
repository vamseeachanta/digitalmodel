"""Executable content checks for the legacy diffraction knowledge pack."""

import json
import re
import subprocess
import sys
import tomllib
from functools import lru_cache
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[4]
DOCS = ROOT / "src/digitalmodel/solvers/orcawave/docs"
PACK = DOCS / "agent-pack.md"
CITATION = re.compile(r"\[([^\]\n]+):L(\d+)-L(\d+)\]")


@lru_cache(maxsize=None)
def source_at(revision, path):
    return subprocess.check_output(
        ["git", "show", f"{revision}:{path}"], cwd=ROOT, text=True,
    )


def read_pinned_source(pack, reference):
    pin = re.search(r"^\| Code revision \| `([0-9a-f]{40})`", pack, re.MULTILINE)
    assert pin, "pack needs a code revision"
    path, explicit, revision = reference.partition("@")
    return source_at(revision if explicit else pin[1], path)


def citation_text(text, read_source):
    """Validate every ranged citation and return its pinned source text."""
    cited = []
    for reference, start, end in CITATION.findall(text):
        lines = read_source(reference).splitlines()
        assert 1 <= int(start) <= int(end) <= len(lines), f"invalid citation: {reference}"
        cited.append((reference, "\n".join(lines[int(start) - 1:int(end)])))
    assert cited, "evidence needs a ranged citation"
    return cited


def check_evidence(pack, prefix, reference, tokens, read_source):
    """Require the expected claim tokens in the cited file, at its audit pin."""
    row, = [line for line in pack.splitlines() if line.startswith(prefix)]
    evidence = "\n".join(text for ref, text in citation_text(row, read_source)
                         if ref == reference)
    for token in tokens:
        assert token in evidence, f"uncited dependency or setting: {token}"


def check_dependency_citation(pack, pyproject):
    project = tomllib.loads(pyproject)["project"]
    tokens = [f'requires-python = "{project["requires-python"]}"']
    for name in ("pyyaml", "numpy", "pandas", "h5py"):
        dependency, = [item for item in project["dependencies"]
                       if re.split(r"[<>=!~\[]", item)[0].lower() == name]
        tokens.append(f'"{dependency}"')
    check_evidence(pack, "| Python, PyYAML,", "pyproject.toml", tokens,
                   lambda ref: pyproject if ref == "pyproject.toml" else read_pinned_source(pack, ref))


def test_dependency_citation_content():
    pack = PACK.read_text(encoding="utf-8")
    check_dependency_citation(pack, read_pinned_source(pack, "pyproject.toml"))


def test_all_citation_ranges_at_pins():
    pack = PACK.read_text(encoding="utf-8")
    citation_text(pack, lambda ref: read_pinned_source(pack, ref))


def test_reproduction_citation_content():
    pack = PACK.read_text(encoding="utf-8")
    reader = lambda ref: read_pinned_source(pack, ref)
    project = tomllib.loads(reader("pyproject.toml"))["project"]
    check_evidence(pack, "Reproduction form:", "pyproject.toml",
                   ["test = [", *[f'"{dep}"' for dep in project["optional-dependencies"]["test"]
                                   if dep.startswith("pytest")]], reader)
    check_evidence(pack, "Reproduction form:", "pytest.ini",
                   ["--strict-markers", "--import-mode=importlib"], reader)


@pytest.mark.parametrize("reference,replacement,token", [
    ("pyproject.toml", "L5-L9", "test = ["),
    ("pytest.ini", "L1-L4", "--strict-markers"),
])
def test_reproduction_wrong_citation_is_rejected(reference, replacement, token):
    pack = PACK.read_text(encoding="utf-8")
    broken = re.sub(rf"\[{re.escape(reference)}:L\d+-L\d+\]",
                    f"[{reference}:{replacement}]", pack)
    with pytest.raises(AssertionError, match="uncited"):
        check_evidence(broken, "Reproduction form:", reference, [token],
                       lambda ref: read_pinned_source(broken, ref))


@pytest.mark.parametrize("start,end", [(0, 1), (2, 1), (1, 3)])
def test_invalid_citation_range_is_rejected(start, end):
    with pytest.raises(AssertionError, match="invalid citation"):
        citation_text(f"[example.py:L{start}-L{end}]", lambda ref: "one\ntwo\n")


def test_citation_revision_selection(monkeypatch):
    pack = PACK.read_text(encoding="utf-8")
    code_pin = re.search(r"^\| Code revision \| `([0-9a-f]{40})`", pack, re.MULTILINE)[1]
    calls = []

    def record_source(revision, path):
        calls.append((revision, path))
        return "pinned evidence"

    monkeypatch.setattr(sys.modules[__name__], "source_at", record_source)
    assert read_pinned_source(pack, "example.py") == "pinned evidence"
    assert read_pinned_source(pack, "example.py@explicit") == "pinned evidence"
    assert calls == [(code_pin, "example.py"), ("explicit", "example.py")]


def test_metadata_only_citation_is_rejected():
    pack = PACK.read_text(encoding="utf-8")
    broken = re.sub(r"\[pyproject.toml:L\d+-L\d+\]", "[pyproject.toml:L5-L9]", pack)
    with pytest.raises(AssertionError):
        check_dependency_citation(broken, read_pinned_source(pack, "pyproject.toml"))


def test_python_requirement_alone_does_not_cite_dependencies():
    pack = PACK.read_text(encoding="utf-8")
    broken = re.sub(r"\[pyproject.toml:L\d+-L\d+\]", "[pyproject.toml:L10-L10]", pack)
    with pytest.raises(AssertionError, match="uncited dependency"):
        check_dependency_citation(broken, read_pinned_source(pack, "pyproject.toml"))


def check_dispositions(pack, dispositions):
    rows = re.findall(r"^\| (S\d{3}) \| (.+)$", pack, flags=re.MULTILINE)
    assert rows
    assert len(rows) == len({key for key, _ in rows}), "duplicate failure row"
    assert set(dispositions) == {key for key, _ in rows}, "undisposed or stale rows"
    for key, text in rows:
        record = dispositions[key]
        assert record["status"] in {"deferred", "fail-closed", "benign", "fixed", "test-only"}
        symptom = text.split(" | ")[0]
        reason = CITATION.sub("", symptom).strip()
        assert record["reason"] == reason, f"stale reason: {key}"
        assert re.fullmatch(r"#\d+", record["issue"])
        # Every disposition remains bound to the package function/output evidence.
        assert "src/digitalmodel/solvers/orcawave/" in text or key == "S095"


def test_g0_control_dispositions():
    check_dispositions(PACK.read_text(encoding="utf-8"),
                       json.loads((DOCS / "gate-dispositions.json").read_text(encoding="utf-8")))


def test_g0_control_missing_disposition_is_rejected():
    pack = PACK.read_text(encoding="utf-8")
    records = json.loads((DOCS / "gate-dispositions.json").read_text(encoding="utf-8"))
    del records["S001"]
    with pytest.raises(AssertionError, match="undisposed"):
        check_dispositions(pack, records)


def test_g0_control_stale_reason_is_rejected():
    pack = PACK.read_text(encoding="utf-8")
    records = json.loads((DOCS / "gate-dispositions.json").read_text(encoding="utf-8"))
    records["S001"]["reason"] = "Outdated explanation."
    with pytest.raises(AssertionError, match="stale reason: S001"):
        check_dispositions(pack, records)
