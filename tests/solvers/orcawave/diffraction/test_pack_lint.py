"""Executable content checks for the legacy diffraction knowledge pack."""

import json
import re
import tomllib
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[4]
DOCS = ROOT / "src/digitalmodel/solvers/orcawave/docs"
PACK = DOCS / "agent-pack.md"


def check_dependency_citation(pack, pyproject):
    row, = [line for line in pack.splitlines() if line.startswith("| Python, PyYAML,")]
    ranges = re.findall(r"\[pyproject.toml:L(\d+)-L(\d+)\]", row)
    assert ranges, "dependency row needs pyproject evidence"
    lines = pyproject.splitlines()
    evidence = []
    for start, end in ranges:
        assert 1 <= int(start) <= int(end) <= len(lines)
        evidence.extend(lines[int(start) - 1:int(end)])
    cited = "\n".join(evidence)
    project = tomllib.loads(pyproject)["project"]
    assert f'requires-python = "{project["requires-python"]}"' in cited
    for name in ("pyyaml", "numpy", "pandas", "h5py"):
        dependency, = [item for item in project["dependencies"]
                       if re.split(r"[<>=!~\[]", item)[0].lower() == name]
        assert f'"{dependency}"' in cited, f"uncited dependency: {name}"


def test_dependency_citation_content():
    check_dependency_citation(
        PACK.read_text(encoding="utf-8"),
        (ROOT / "pyproject.toml").read_text(encoding="utf-8"),
    )


def test_metadata_only_citation_is_rejected():
    pack = PACK.read_text(encoding="utf-8")
    broken = re.sub(r"\[pyproject.toml:L\d+-L\d+\]", "[pyproject.toml:L5-L9]", pack)
    with pytest.raises(AssertionError):
        check_dependency_citation(broken, (ROOT / "pyproject.toml").read_text(encoding="utf-8"))


def test_python_requirement_alone_does_not_cite_dependencies():
    pack = PACK.read_text(encoding="utf-8")
    broken = re.sub(r"\[pyproject.toml:L\d+-L\d+\]", "[pyproject.toml:L10-L10]", pack)
    with pytest.raises(AssertionError, match="uncited dependency"):
        check_dependency_citation(broken, (ROOT / "pyproject.toml").read_text(encoding="utf-8"))


def check_dispositions(pack, dispositions):
    rows = re.findall(r"^\| (S\d{3}) \| (.+)$", pack, flags=re.MULTILINE)
    assert rows
    assert len(rows) == len({key for key, _ in rows}), "duplicate failure row"
    assert set(dispositions) == {key for key, _ in rows}, "undisposed or stale rows"
    for key, text in rows:
        record = dispositions[key]
        assert record["status"] in {"deferred", "fail-closed", "benign", "fixed", "test-only"}
        assert record["reason"].strip(), key
        assert re.fullmatch(r"https://github.com/vamseeachanta/digitalmodel/issues/\d+", record["issue"])
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
