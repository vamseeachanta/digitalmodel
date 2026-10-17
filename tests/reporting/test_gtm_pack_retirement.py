# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Internal-screening reports and tracked-tree retirement boundaries."""

import importlib.util
import os
import re
import subprocess
import sys
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[2]
GTM = ROOT / "examples/demos/gtm"
TOKENS = (b"VC" + b"-funded", b"funding" + b"-badge")


def test_no_promotional_block_anywhere_under_demo_tree():
    offenders = []
    for path in GTM.rglob("*"):
        if path.is_file():
            content = path.read_bytes().lower()
            if any(token.lower() in content for token in TOKENS):
                offenders.append(path.relative_to(GTM).as_posix())
    assert not offenders, offenders


def test_teaser_method_keeps_callers_and_emits_only_internal_notice(tmp_path):
    spec = importlib.util.spec_from_file_location("gtm_report", GTM / "report_template.py")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    builder = module.GTMReportBuilder("Screening", "Parametric cases", "demo_test", 1)
    builder.add_live_mode_teaser("installation")
    target = tmp_path / "report.html"
    builder.build(target)
    text = target.read_text(encoding="utf-8")
    assert "internal screening, not for clients" in text.lower()
    assert "Schedule a Technical Demo" not in text
    assert "mailto:" not in text
    assert not any(token.lower() in text.lower().encode() for token in TOKENS)


def test_ten_stored_reports_have_no_sales_call_to_action():
    reports = sorted((GTM / "output").glob("demo_*_report.html"))
    assert len(reports) == 10
    for path in reports:
        text = path.read_text(encoding="utf-8")
        assert "Schedule a Technical Demo" not in text
        assert "mailto:" not in text
        assert "internal screening, not for clients" in text.lower()


def test_wave_period_relationship_is_qualified_in_source_and_reports():
    sources = list(GTM.glob("demo_03_*.py")) + list(GTM.glob("demo_05_*.py"))
    reports = list((GTM / "output").glob("demo_03_*_report.html"))
    reports += list((GTM / "output").glob("demo_05_*_report.html"))
    assert len(sources) == len(reports) == 2
    for path in sources + reports:
        text = path.read_text(encoding="utf-8")
        lines = [line for line in text.splitlines() if re.search(r"Tp = .*sqrt\(Hs\)", line)]
        assert lines, path
        assert any("screening simplification" in line.lower() for line in lines), path


def test_retired_pack_links_only_in_register_or_historical_archive_maps():
    names = (b"client_pdf_pack_" + b"2026-05-07", b"pipeline_company_pack_" + b"2026-05-07")
    # Dated archive inventories are immutable snapshots. The acceptance exception
    # is restricted to these three files; the retirement record is authoritative.
    allowed = {
        "config/generated_retirements.yml",
        "docs/archive/archive-blob-map-2026-10-08.csv",
        "docs/archive/archive-candidates-2026-10-08.csv",
        "docs/archive/archive-candidates-2026-10-08.summary.json",
    }
    command = ["git", "grep", "-l", "-z"]
    for name in names:
        command += ["-e", name.decode("utf-8")]
    env = {key: value for key, value in os.environ.items()
           if key not in {"GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR", "GIT_INDEX_FILE"}}
    result = subprocess.run(command, cwd=ROOT, env=env, capture_output=True)
    assert result.returncode == 0 and not result.stderr, result.stderr
    matches = set(result.stdout.decode("utf-8").rstrip("\0").split("\0"))
    assert matches == allowed, sorted(matches ^ allowed)
    assert not any((GTM / "output").glob("*_pack_2026-05-07"))


def test_tracked_public_tree_has_no_retired_promotional_tokens():
    command = ["git", "grep", "-i", "-I", "-l"]
    for token in TOKENS:
        command += ["-e", token.decode()]
    result = subprocess.run(command, cwd=ROOT, capture_output=True, text=True)
    assert result.returncode == 1 and not result.stdout and not result.stderr, result.stdout


@pytest.mark.parametrize("path", ["docs/new-reference.md", "scripts/new-reference.py", "README.md"])
def test_reference_boundary_rejects_paths_outside_demo_tree(tmp_path, monkeypatch, path):
    module = sys.modules[__name__]
    monkeypatch.setattr(module, "ROOT", tmp_path)
    demo = tmp_path / "examples/demos/gtm"
    demo.mkdir(parents=True)
    monkeypatch.setattr(module, "GTM", demo)
    name = "pipeline_company_pack_" + "2026-05-07"
    allowed = (
        "config/generated_retirements.yml",
        "docs/archive/archive-blob-map-2026-10-08.csv",
        "docs/archive/archive-candidates-2026-10-08.csv",
        "docs/archive/archive-candidates-2026-10-08.summary.json",
    )
    for relative in (*allowed, path):
        target = tmp_path / relative
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_text(name, encoding="utf-8")
    env = {key: value for key, value in os.environ.items()
           if key not in {"GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR", "GIT_INDEX_FILE"}}
    for arguments in (("init", "-q"), ("add", ".")):
        subprocess.run(["git", *arguments], cwd=tmp_path, env=env, check=True,
                       capture_output=True)
    with pytest.raises(AssertionError):
        test_retired_pack_links_only_in_register_or_historical_archive_maps()
