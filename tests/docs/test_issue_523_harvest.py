# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Keep harvested recommendations assigned to the existing approved work."""

import re
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[2]
HARVEST = ROOT / "docs/sessions/2026-05-05-issue-523-harvest-source.md"


@pytest.mark.parametrize(
    "recommendation,destinations",
    [
        ("Rec 1", [(517, 3)]),
        ("Rec 2", [(519, 2)]),
        ("Rec 3", [(517, 3), (519, 3)]),
    ],
)
def test_recommendations_have_existing_plan_destinations(recommendation, destinations):
    """No recommendation may be left without its approved issue/task routing."""
    text = HARVEST.read_text(encoding="utf-8")
    rows = [line for line in text.splitlines() if line.startswith(f"| {recommendation} |")]
    assert len(rows) == 1, f"Missing or duplicate mapping: {recommendation}"
    for issue, task in destinations:
        assert f"https://github.com/vamseeachanta/digitalmodel/issues/{issue}" in rows[0]
        assert f"{issue} task {task}" in rows[0]


def test_reconstructed_source_exposes_its_evidence_limit():
    """An unavailable review must not be presented as recovered verbatim evidence."""
    text = HARVEST.read_text(encoding="utf-8").lower()
    assert "## source and recovery" in text
    assert "reconstructed from the issue body" in text
    assert "original review was not recovered" in text
    assert "no stronger equivalence claim" in text


def test_publication_observations_are_dated():
    """Publication observations describe the recording date, not current state."""
    text = HARVEST.read_text(encoding="utf-8")
    recorded = re.search(r"^Recorded (\d{4}-\d{2}-\d{2})\.", text, re.MULTILINE)
    assert recorded, "Missing recording date"
    status = text.split("## Publication status and comment drafts\n", 1)[1]
    introduction = status.split("\n### ", 1)[0]
    assert f"At the time of recording ({recorded[1]})" in introduction


def test_readback_draft_keeps_recording_observations_outside_payload():
    """The readback can be posted without copying a dated publication state."""
    text = HARVEST.read_text(encoding="utf-8")
    match = re.search(
        r"### Draft for \[#523\]\([^)]+\)\n(.*?)(?=\n### |\Z)",
        text,
        re.DOTALL,
    )
    assert match, "Missing readback draft for issue 523"
    assert not match[1].lstrip().startswith("Local harvest")
    assert "At the time of recording" not in match[1]


@pytest.mark.parametrize(
    "issue,recommendations",
    [(515, [1, 2, 3]), (517, [1, 3]), (518, [1, 2, 3]),
     (519, [2, 3]), (523, [1, 2, 3])],
)
def test_retained_drafts_cover_each_destination(issue, recommendations):
    """The retained drafts must contain every required GitHub routing payload."""
    text = HARVEST.read_text(encoding="utf-8")
    match = re.search(
        rf"### Draft for \[#{issue}\]\([^)]+\)\n(.*?)(?=\n### |\Z)",
        text,
        re.DOTALL,
    )
    assert match, f"Missing comment draft for issue {issue}"
    assert re.search(r"#523\b", match[1])
    for number in recommendations:
        assert f"Rec {number}" in match[1]


def test_repository_evidence_links_resolve():
    """Routing must remain reviewable from the retained artifact itself."""
    text = HARVEST.read_text(encoding="utf-8")
    targets = re.findall(r"\[[^\]]+\]\(([^)]+)\)", text)
    local_targets = [target for target in targets if not target.startswith("https://")]
    assert local_targets, "No retained repository evidence links"
    for target in local_targets:
        resolved = (HARVEST.parent / target).resolve()
        assert resolved.is_relative_to(ROOT), f"Evidence escapes repository: {target}"
        assert resolved.is_file(), f"Unresolvable evidence: {target}"
