"""Offline replay of the report-only trial census."""
import json
import subprocess

import pytest

from scripts.ci import trust_report as tool


def test_fetch_uses_explicit_get_and_report_replays_offline(tmp_path, monkeypatch):
    calls = []

    def github(args, **kwargs):
        assert args[:4] == ["gh", "api", "--method", "GET"]
        calls.append(args[4])
        payload = {"full_name": "example/repo"} if args[4].endswith("{repo}") else []
        if args[4].startswith("search/issues?"):
            payload = {"items": [], "total_count": 0, "incomplete_results": False}
        return subprocess.CompletedProcess(args, 0, json.dumps(payload))

    monkeypatch.setattr(subprocess, "run", github)
    tool.main(["fetch", "--since", "2026-11-10", "--until", "2026-11-18", "--out", str(tmp_path)])
    assert calls
    calls.clear()
    argv = ["report", "--snapshots", str(tmp_path), "--out-md", str(tmp_path / "r.md"),
            "--out-json", str(tmp_path / "r.json")]
    tool.main(argv)
    data = json.loads((tmp_path / "r.json").read_text())
    assert data["since"] == "2026-11-10T00:00:00Z"
    assert data["until"] == "2026-11-18T23:59:59Z"
    assert data["reads"]["interim"]["opened"] == 0
    assert data["reads"]["final"]["eligible_merged"] is None
    assert "Unknown" in (tmp_path / "r.md").read_text()
    assert calls == []


def test_ambiguous_timestamp_rejected():
    with pytest.raises(ValueError):
        tool.bound("2026-11-18T01:00", False)


def test_merge_history_includes_old_pr_and_conflicting_provider_is_unknown():
    def api(path):
        if path == "":
            return {"full_name": "owner/repo"}
        if path.startswith("search/issues?"):
            ids = [{"number": 11}] if "merged%3A2026-11-08" in path else []
            return {"total_count": len(ids), "incomplete_results": False, "items": ids}
        if path.startswith("pulls?"):
            return [{"number": 11, "created_at": "2026-08-01T00:00:00Z",
                     "updated_at": "2026-11-11T00:00:00Z", "merged_at": "2026-11-11T00:00:00Z",
                     "user": {"login": "author"}, "head": {"ref": "codex/work", "sha": "a" * 40},
                     "base": {"ref": "main"}, "body": "Implemented by Claude"}]
        if "/commits?" in path:
            return [{"sha": "a" * 40, "commit": {"message": "Add tests\n\nCo-Authored-By: Codex <bot@example.org>"}}]
        if path.startswith("issues/comments?") or "/reviews?" in path:
            return []
        raise AssertionError(path)

    snapshot = tool.fetch_pulls(api, "2026-11-15T00:00:00Z", "2026-11-17T23:59:59Z", "2026-11-18T00:00:00Z")
    row, = tool.normalize_pulls(snapshot)
    assert (row["number"], row["author"], row["lane"], row["provider"]) == (11, "author", "codex", None)


def test_sha_pinned_postmerge_fail_and_connector_summary():
    sha = "0123456789abcdef" * 2 + "01234567"
    review = {"id": 7, "created_at": "2026-11-11T11:00:00Z", "body": f"Claude review (second provider) at {sha[:8]}: FAIL"}
    summary = dict(review, id=8, body="<!-- codex-pull-request-review-summary -->\nNo findings.")
    item = dict(number=1, author="author", ref="codex/work", base="main", body="", head=sha,
                opened="2026-11-09T00:00:00Z", merged="2026-11-11T10:00:00Z",
                commits=[["f" * 40, "Old"], [sha, "Work\n\nCo-Authored-By: Codex <bot@example.org>"]],
                comments=[review, summary, dict(review, id=11, body=review["body"].replace(sha[:8], "ffffffff")),
                          dict(review, id=10, body=review["body"].replace("second", "first")),
                          dict(review, id=9, created_at="2026-11-11T08:00:00Z",
                               updated_at="2026-11-11T12:00:00Z")], reviews=[])
    row, = tool.normalize_pulls({"pulls": [item], "as_of": "2026-11-12T00:00:00Z"})
    assert row["provider"] == "Codex"
    assert len(row["reviews"]) == 3
    assert [(r["id"], r["sha"], r["kind"]) for r in row["post_merge_failures"]] == [
        ("comments/7", sha, "FAIL"), ("comments/10", sha, "FAIL")]


def day(n):
    return f"2026-11-{n:02d}T00:00:00Z"


def history(days, **fields):
    return [dict(number=i, opened=day(1), merged=day(d), author="contributor",
                 provider="Codex", lane="ci", base="main", defects_complete=True, **fields)
            for i, d in enumerate(days, 1)]


def report(prs, incidents=(), at=20):
    return tool.build_report(prs, list(incidents), day(1), day(18), day(at))


@pytest.mark.parametrize(("days", "provider", "complete", "trusted"), [
    (range(1, 11), "Codex", True, True), (range(1, 10), "Codex", True, False),
    ([1] * 9 + [7], "Codex", True, False), ([1] * 9 + [8], "Codex", True, True),
    (range(1, 11), None, True, False), (range(1, 11), "Codex", None, False)])
def test_author_needs_ten_merges_week_known_provider_and_coverage(days, provider, complete, trusted):
    prs = [p | dict(provider=provider, defects_complete=complete) for p in history(days)]
    assert report(prs)["authors"][0]["trusted"] is trusted


def test_author_lane_incident_dedup_and_observation_boundary():
    prs = history(range(1, 11))
    prs[-1]["lane"] = "build"
    prs[0]["post_merge_failures"] = [dict(id="review:1", at=day(9), kind="FAIL")]
    incidents = [dict(id="review:1", at=day(9), pr=1), dict(id="future", at=day(19), pr=1)]
    result = report(prs, incidents, at=15)
    assert [a["merged_count"] for a in result["authors"]] == [1, 9]
    assert [d["id"] for d in result["defects"]] == ["review:1"]
    assert result["prs"][0]["defect_ids"] == ["review:1"]
    assert result["authors"][1]["defects"] == 1


def test_incident_id_cannot_link_different_merges():
    with pytest.raises(ValueError, match="multiple"):
        report(history([1, 2]), [dict(id="bug", at=day(9), pr=i) for i in (1, 2)])


def test_incident_aliases_reconcile_multiple_verdicts_and_require_a_known_merge():
    prs = history([1])
    prs[0]["post_merge_failures"] = [dict(id=f"reviews/{i}", kind="FAIL", at=day(4)) for i in (1, 2)]
    linked = dict(id="incident", aliases=["reviews/1", "reviews/2"], pr=1, at=day(5))
    assert report(prs, [linked])["prs"][0]["defect_ids"] == ["incident"]
    assert report(prs, [linked], at=4)["authors"][0]["defects"] == 2
    with pytest.raises(ValueError, match="unknown merge"):
        report(prs, [linked | dict(pr=999)])
    with pytest.raises(ValueError, match="ambiguous"):
        report(prs, [linked, linked | dict(id="another-incident")])


def test_edited_text_and_unpinned_commits_cannot_backdate_attribution():
    head = "a" * 40
    pr = dict(number=1, author="author", head=head, ref="ci/fix", base="main",
              body="Implemented by Claude", updated=day(12), opened=day(1), merged=None,
              commits=[["b" * 40, "Work\n\nCo-Authored-By: Codex <bot@example.org>"]],
              reviews=[], comments=[])
    cutoff = day(10)
    row, = tool.normalize_pulls(dict(as_of=cutoff, pulls=[pr]))
    assert row["provider"] is None
    pr["commits"] = [[head, "Work\n\nCo-Authored-By: Codex <bot@example.org>"]]
    row, = tool.normalize_pulls(dict(as_of=cutoff, pulls=[pr]))
    assert row["provider"] == "Codex"
    pr["reviews"] = [dict(id=1, body=f"Claude review (second provider) at {head}: FAIL",
                          created_at=day(6), sha=head)]
    pr["merged"] = day(5)
    row, = tool.normalize_pulls(dict(as_of=cutoff, pulls=[pr]))
    # Submission time cannot date the text of a mutable formal review.
    assert row["post_merge_failures"] == []


def test_incomplete_census_is_rejected():
    def incomplete(_):
        return {"items": [], "total_count": 0, "incomplete_results": True}
    with pytest.raises(ValueError, match="Incomplete"):
        tool.census(incomplete, "owner/repo", "created", day(1), day(2))


@pytest.mark.parametrize("qualifier", ["merged", "created"])
def test_independent_census_rejects_missing_or_extra_pr(qualifier):
    def api(path):
        if not path:
            return {"full_name": "owner/repo"}
        rows = [{"number": 3}] if qualifier in path else []
        return {"items": rows, "total_count": len(rows), "incomplete_results": False}
    with pytest.raises(ValueError, match="disagree"):
        tool.verify_census(api, [], day(1), day(2), day(2), day(1))


def test_commit_reuse_requires_same_repository_base_and_head():
    prior = dict(schema=tool.SCHEMA, repository="owner/repo", pulls=[dict(
        number=1, base_sha="a" * 40, head="b" * 40, commits=[["b" * 40, "message"]])])
    assert tool.commit_cache(prior, "owner/repo") == {(1, "a" * 40, "b" * 40): [["b" * 40, "message"]]}
    with pytest.raises(ValueError):
        tool.commit_cache(prior, "other/repo")
