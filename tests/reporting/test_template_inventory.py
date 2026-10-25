# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Inventory expectations follow literal fixture contents, without rendering.

The repository case is sourced from the template's CSS, the retirement
regression's stored demo set, and the four buckling copies identified in #2691.
No engineering constants or numerical acceptance criteria are involved.
"""

import json
import os
import subprocess
from pathlib import Path

import pytest

from scripts.maintenance import report_template_inventory as inventory


ROOT = Path(__file__).resolve().parents[2]


@pytest.fixture
def sample(tmp_path, monkeypatch):
    """Two matching tracked pages; one manual, one outside the page census.

    A third tracked page has only one marker. An untracked copy has every
    marker but must be excluded. Thus the expected consumer set has precisely
    the manual page and the copy outside the census, by direct enumeration.
    """
    template = tmp_path / inventory.TEMPLATE
    template.parent.mkdir(parents=True)
    template.write_text("\n".join(inventory.MARKERS), encoding="utf-8", newline="")
    paths = ["reference/manual.html", "examples/copy.HTML", "reference/unrelated.html"]
    for relative in paths + ["examples/untracked.html"]:
        target = tmp_path / relative
        target.parent.mkdir(parents=True, exist_ok=True)
        text = inventory.MARKERS[0] if "unrelated" in relative else template.read_text()
        target.write_text(text, encoding="utf-8", newline="")
    monkeypatch.setattr(inventory, "tracked_html_paths", lambda repo: list(reversed(paths)))
    monkeypatch.setattr(inventory, "MANUAL_PAGES", {paths[0]: "maintained manually"})
    return tmp_path


def test_discovers_manual_and_outside_census_copies(sample):
    result = inventory.build_inventory(sample)
    assert [row["path"] for row in result["consumers"]] == [
        "examples/copy.HTML", "reference/manual.html",
    ]
    assert [row["page_classification"] for row in result["consumers"]] == [
        "not_classified_as_manual", "manual",
    ]
    assert result["freshness"] == "NOT_EVALUATED"
    assert result["template"] == inventory.TEMPLATE
    assert result["evidence_markers"] == list(inventory.MARKERS)


def test_fixture_sources_have_exact_lf_bytes(sample):
    expected = "\n".join(inventory.MARKERS).encode("utf-8")
    for relative in [inventory.TEMPLATE, "reference/manual.html", "examples/copy.HTML"]:
        assert (sample / relative).read_bytes() == expected


def test_snapshot_detects_template_and_copy_edits(sample):
    before = inventory.build_inventory(sample)
    template = sample / inventory.TEMPLATE
    template.write_text(template.read_text() + "\n/* amended */", encoding="utf-8", newline="")
    after = inventory.build_inventory(sample)
    assert before["template_sha256"] != after["template_sha256"]
    assert before["consumers"] == after["consumers"]
    page = sample / "reference/manual.html"
    page.write_text(page.read_text() + "\n<p>amended</p>", encoding="utf-8", newline="")
    assert after["consumers"] != inventory.build_inventory(sample)["consumers"]


@pytest.mark.parametrize("relative", [inventory.TEMPLATE, "reference/manual.html"])
def test_snapshot_ignores_git_checkout_crlf(sample, relative, monkeypatch):
    """Git checkout conversion alone must not invalidate source or copy hashes."""
    attributes = sample / "external.attributes"
    attributes.write_text("* -text\n", encoding="utf-8", newline="")
    config = sample / "external.gitconfig"
    config.write_text(
        "[core]\n\tattributesFile = " + json.dumps(attributes.as_posix(), ensure_ascii=False) + "\n",
        encoding="utf-8", newline="",
    )
    monkeypatch.setenv("GIT_CONFIG_GLOBAL", str(config))
    monkeypatch.setenv("GIT_CONFIG_SYSTEM", str(config))
    monkeypatch.delenv("GIT_CONFIG_NOSYSTEM", raising=False)
    env = {key: value for key, value in os.environ.items()
           if key not in {"GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR", "GIT_INDEX_FILE"}}
    env.update(GIT_CONFIG_GLOBAL=os.devnull, GIT_CONFIG_NOSYSTEM="1")

    def git(*args):
        return subprocess.run(
            ["git", *args], cwd=sample, env=env, check=True, capture_output=True,
        ).stdout

    git("init")
    git("-c", "core.autocrlf=false", "add", "--", relative)
    before = inventory.build_inventory(sample)
    converted = git("-c", "core.autocrlf=true", "cat-file", "--filters", f":{relative}")
    original = (sample / relative).read_bytes()
    assert b"\r\n" in converted
    assert converted != original
    assert converted.replace(b"\r\n", b"\n") == original
    (sample / relative).write_bytes(converted)
    assert inventory.build_inventory(sample) == before


def test_marker_removed_from_template_requires_signature_review(sample):
    (sample / inventory.TEMPLATE).write_text("unrelated source", encoding="utf-8", newline="")
    with pytest.raises(ValueError, match="template evidence markers changed"):
        inventory.build_inventory(sample)


def test_deleted_tracked_copy_is_an_error(sample):
    (sample / "reference/manual.html").unlink()
    with pytest.raises(FileNotFoundError):
        inventory.build_inventory(sample)


def test_new_tracked_copy_outside_existing_directories_changes_census(sample, monkeypatch):
    before = inventory.build_inventory(sample)
    target = sample / "portal/nested/new copy.html"
    target.parent.mkdir(parents=True)
    target.write_text((sample / inventory.TEMPLATE).read_text(), encoding="utf-8", newline="")
    paths = [row["path"] for row in before["consumers"]]
    added = target.relative_to(sample).as_posix()
    monkeypatch.setattr(inventory, "tracked_html_paths", lambda repo: paths + [added])
    after = inventory.build_inventory(sample)
    assert {row["path"] for row in after["consumers"]} == set(paths) | {added}
    assert before != after


def test_tracked_path_query_is_nul_delimited_and_case_insensitive(monkeypatch):
    git_keys = {"GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR", "GIT_INDEX_FILE"}
    for key in git_keys:
        monkeypatch.setenv(key, "unused inherited value")

    class Result:
        stdout = b"reference/space name.HTML\0reference/data.json\0examples/copy.html\0"

    def run(command, **kwargs):
        assert command == ["git", "ls-files", "-z"]
        assert kwargs["check"] is True
        assert not (git_keys & kwargs["env"].keys())
        return Result()

    monkeypatch.setattr(inventory.subprocess, "run", run)
    assert inventory.tracked_html_paths(ROOT) == [
        "examples/copy.html", "reference/space name.HTML",
    ]


def test_cli_prints_inventory_and_checks_snapshot(sample, capsys):
    assert inventory.main(["--repo", str(sample)]) == 0
    expected = json.loads(capsys.readouterr().out)
    manifest = sample / inventory.MANIFEST
    manifest.parent.mkdir(parents=True)
    manifest.write_text(json.dumps(expected), encoding="utf-8", newline="")
    assert inventory.main(["--repo", str(sample), "--check"]) == 0
    assert "inventory matches" in capsys.readouterr().out
    page = sample / "reference/manual.html"
    page.write_text("no remaining markers", encoding="utf-8", newline="")
    assert inventory.main(["--repo", str(sample), "--check"]) == 1
    assert "inventory differs" in capsys.readouterr().err


def test_cli_writes_utf8_lf_snapshot_and_refreshes_it(sample, capsys):
    manifest = sample / inventory.MANIFEST
    expected = (json.dumps(inventory.build_inventory(sample), indent=2) + "\n").encode("utf-8")
    assert inventory.main(["--repo", str(sample), "--write"]) == 0
    assert manifest.read_bytes() == expected
    assert inventory.main(["--repo", str(sample), "--check"]) == 0
    capsys.readouterr()
    (sample / "reference/manual.html").write_bytes(b"no remaining markers")
    assert inventory.main(["--repo", str(sample), "--check"]) == 1
    assert "inventory differs" in capsys.readouterr().err
    assert inventory.main(["--repo", str(sample), "--write"]) == 0
    assert inventory.main(["--repo", str(sample), "--check"]) == 0


def test_cli_rejects_check_and_write_together(sample, capsys):
    with pytest.raises(SystemExit) as error:
        inventory.main(["--repo", str(sample), "--check", "--write"])
    assert error.value.code == 2
    assert "not allowed with argument --check" in capsys.readouterr().err
    assert not (sample / inventory.MANIFEST).exists()


@pytest.mark.parametrize("contents", [None, "invalid json"])
def test_cli_reports_missing_or_invalid_manifest(sample, capsys, contents):
    if contents is not None:
        manifest = sample / inventory.MANIFEST
        manifest.parent.mkdir(parents=True)
        manifest.write_text(contents, encoding="utf-8", newline="")
    assert inventory.main(["--repo", str(sample), "--check"]) == 1
    assert "inventory check failed" in capsys.readouterr().err


def test_repository_snapshot_includes_all_known_copies():
    """Hand-enumerated expectation: ten stored demos plus four buckling pages.

    Demo membership follows test_gtm_pack_retirement.py. Buckling membership
    follows the structural builders' output paths and the two manual entries
    in generated_html_ownership.py. Counts express file membership only.
    """
    expected = {
        "docs/api/buckling/ship-panel-buckling.html",
        "docs/api/buckling/ship-plate-buckling.html",
        "examples/demos/structural/output/ship_panel_buckling_report.html",
        "examples/demos/structural/output/ship_plate_buckling_report.html",
    }
    demos = ROOT / "examples/demos/gtm/output"
    stored = {path.relative_to(ROOT).as_posix() for path in demos.glob("demo_*_report.html")}
    assert len(stored) == 10  # Source: test_gtm_pack_retirement.py stored-report census.
    result = inventory.build_inventory(ROOT)
    assert {row["path"] for row in result["consumers"]} == expected | stored
    manual = {row["path"] for row in result["consumers"]
              if row["page_classification"] == "manual"}
    assert manual == {path for path in expected if path.startswith("docs/")}
    assert json.loads((ROOT / inventory.MANIFEST).read_text()) == result
