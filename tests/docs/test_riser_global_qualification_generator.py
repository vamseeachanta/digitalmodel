# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Regress snapshot reconciliation without collecting native test modules."""

import importlib.util
import json
import runpy
import subprocess
import sys
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "docs/domains/risers/reconcile_qualification.py"
REGISTER = ROOT / "docs/domains/risers/global-model-qualification.json"
REPORT = REGISTER.with_suffix(".html")


@pytest.fixture
def generator(tmp_path, monkeypatch):
    spec = importlib.util.spec_from_file_location("riser_reconcile", SCRIPT)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    for name, source in (("REGISTER", REGISTER), ("REPORT", REPORT)):
        target = tmp_path / source.name
        target.write_bytes(source.read_bytes())
        monkeypatch.setattr(module, name, target)
    return module


def test_generator_check_is_read_only():
    before = [path.read_bytes() for path in (REGISTER, REPORT)]
    result = subprocess.run(
        [sys.executable, str(SCRIPT), "--check"], capture_output=True, text=True,
    )
    assert result.returncode == 0, result.stdout + result.stderr
    assert [path.read_bytes() for path in (REGISTER, REPORT)] == before


def test_generator_changes_select_documentation_contracts():
    detector = runpy.run_path(str(ROOT / "scripts/ci/detect_touched_domains.py"))
    domains = detector["parse_domains"](ROOT / "tests/DOMAINS.md")
    selected = detector["touched_domains"]([SCRIPT.relative_to(ROOT).as_posix()], domains)
    assert "contracts" in {domain.name for domain in selected}


def test_generator_repairs_inventory_and_table_drift(generator):
    expected = [path.read_bytes() for path in (generator.REGISTER, generator.REPORT)]
    data = json.loads(expected[0])
    data["sources"]["pack"]["sha256"] = "0" * 64
    data["test_coverage"][0]["other_tests"] = []
    data["test_coverage"][0]["classification"] = "STALE"
    generator.REGISTER.write_text(json.dumps(data), encoding="utf-8")
    text = expected[1].decode().replace("<td>8</td>", "<td>999</td>", 1)
    generator.REPORT.write_text(text, encoding="utf-8")
    tampered = [path.read_bytes() for path in (generator.REGISTER, generator.REPORT)]
    assert generator.main(["--check"]) == 1
    assert [path.read_bytes() for path in (generator.REGISTER, generator.REPORT)] == tampered
    assert generator.main(["--write"]) == 0
    assert [path.read_bytes() for path in (generator.REGISTER, generator.REPORT)] == expected
    assert generator.main(["--write"]) == 0
    assert generator.main(["--check"]) == 0


def test_generator_rejects_unknown_revision_without_writing(generator):
    before = [path.read_bytes() for path in (generator.REGISTER, generator.REPORT)]
    assert generator.main(["--write", "--revision", "nonexistent-snapshot"]) == 2
    assert [path.read_bytes() for path in (generator.REGISTER, generator.REPORT)] == before


def test_generator_can_select_a_committed_snapshot(generator):
    assert generator.main(["--write", "--revision", "HEAD"]) == 0
    data = json.loads(generator.REGISTER.read_text(encoding="utf-8"))
    assert data["snapshot_revision"] == generator.git_bytes("rev-parse", "HEAD").decode().strip()
    assert generator.main(["--check"]) == 0


def test_generator_ignores_inherited_repository_bindings(generator, tmp_path, monkeypatch):
    # Resolve metadata through Git: .git is a directory in a normal checkout
    # and a pointer file in a worktree.
    metadata = [Path(generator.git_bytes(
        "rev-parse", "--path-format=absolute", "--git-path", name,
    ).decode().strip()) for name in ("HEAD", "index")]
    protected = [generator.REGISTER, generator.REPORT, *metadata]
    before = [path.read_bytes() for path in protected]
    for name in ("GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR", "GIT_INDEX_FILE"):
        monkeypatch.setenv(name, str(tmp_path / "unrelated-repository"))
    assert generator.main(["--check"]) == 0
    assert [path.read_bytes() for path in protected] == before


def test_generator_rejects_escaping_source_without_writing(generator):
    data = json.loads(generator.REGISTER.read_text(encoding="utf-8"))
    data["sources"]["pack"]["path"] = "../outside.py"
    generator.REGISTER.write_text(json.dumps(data), encoding="utf-8")
    before = [path.read_bytes() for path in (generator.REGISTER, generator.REPORT)]
    assert generator.main(["--write"]) == 2
    assert [path.read_bytes() for path in (generator.REGISTER, generator.REPORT)] == before
