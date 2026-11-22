"""Collect a read-only PR census and replay author evidence for the fast-tier trial.

Run fetch in the repository checkout. Each fetch uses a new snapshot directory.
Manual incidents are a JSON list of {id, pr, at} with an optional shards list.
Use at for the time an incident was recorded (UTC); optional aliases link verdict ids.
Author counts cover main merges from 14 days before the opening window.
"""

import argparse
from collections import defaultdict
from concurrent.futures import ThreadPoolExecutor
from datetime import datetime, timedelta, timezone
from hashlib import sha256
import json
from pathlib import Path
from tempfile import NamedTemporaryFile
import os
import re
import subprocess
from urllib.parse import urlencode

SCHEMA = "earned-trust-census/1"
READS = {"interim": "2026-12-05T23:59:59Z", "final": "2026-12-12T23:59:59Z"}
CRITERIA = ("At least 20 eligible merged PRs", "No P1 at a frozen head",
            "Blocking findings on ≤10% of frozen heads and ≤½ the comparison rate",
            "No attributable defect from an eligible merge aged ≥7 days",
            "At least 10 PR-hours saved per week")


PROVIDERS = r"Codex|Claude|Gemini|Agy|Hermes"
TRAILER = re.compile(rf"^Co-Authored-By:\s*({PROVIDERS})\b", re.I | re.M)
BYLINE = re.compile(rf"^(?:Implemented by\s+|Provider:\s*)({PROVIDERS})\b", re.I | re.M)
VERDICT = re.compile(r"^([\w-]+) review \((first provider|second provider|post-merge)\) at "
                     r"([a-f0-9]{7,40}):\s*(FAIL|CHANGES[- ]REQUESTED|PASS)\b", re.I)


def pages(api, endpoint, **params):
    for page in range(1, 100_001):
        rows = api(endpoint + "?" + urlencode(dict(params, per_page=100, page=page)))
        if not isinstance(rows, list):
            raise ValueError(f"Expected a list from {endpoint}")
        yield from rows
        if len(rows) < 100:
            return
    raise ValueError(f"Pagination limit reached on {endpoint}")


def census(api, repo, qualifier, start, end):
    """Cross-check the mutable updated-order list against independent dated search results."""
    q = f"repo:{repo} is:pr {qualifier}:{start}..{end}"
    if qualifier == "merged":
        q += " base:main"
    numbers = set()
    for page in range(1, 11):
        result = api("search/issues?" + urlencode(dict(q=q, per_page=100, page=page)))
        total = result["total_count"]
        if result["incomplete_results"] is not False or total > 1000:
            raise ValueError(f"Incomplete {qualifier} census for {start}..{end}")
        numbers.update(row["number"] for row in result["items"])
        if len(numbers) == total:
            return numbers
        if not result["items"]:
            break
    raise ValueError(f"Partial {qualifier} census for {start}..{end}")


def verify_census(api, pulls, since, until, as_of, history):
    repo = api("")["full_name"]
    for qualifier, start, end in (("created", since, min(until, as_of)), ("merged", history, as_of)):
        cursor = datetime.fromisoformat(start.replace("Z", "+00:00"))
        while cursor.strftime("%Y-%m-%dT%H:%M:%SZ") <= end:
            a = cursor.strftime("%Y-%m-%dT%H:%M:%SZ")
            cursor += timedelta(days=7)
            b = min(end, (cursor - timedelta(seconds=1)).strftime("%Y-%m-%dT%H:%M:%SZ"))
            actual = {p["number"] for p in pulls if (a <= p["opened"] <= b if qualifier == "created"
                      else p["base"] == "main" and p["merged"] and a <= p["merged"] <= b)}
            if actual != census(api, repo, qualifier, a, b):
                raise ValueError(f"PR listing and {qualifier} search disagree for {a}..{b}")


def commit_cache(prior, repository):
    if prior.get("repository") != repository or prior.get("schema") != SCHEMA:
        raise ValueError("prior commit evidence belongs to a different repository or schema")
    return {(p["number"], p["base_sha"], p["head"]): p["commits"] for p in prior["pulls"]
            if p.get("base_sha") and p.get("head") and p["commits"] and p["commits"][-1][0] == p["head"]}


def fetch_pulls(api, since, until, as_of, cache=None):
    """Keep 14 days before opening for merge history; include late merges of older PRs."""
    history = (datetime.fromisoformat(since.replace("Z", "+00:00")) - timedelta(days=14))
    start = history.strftime("%Y-%m-%dT%H:%M:%SZ")
    pulls = {}
    for pr in pages(api, "pulls", state="all", sort="updated", direction="desc"):
        if pr["updated_at"] < start:
            break
        merged = pr["merged_at"]
        if (since <= pr["created_at"] <= min(as_of, until)
                or (pr["base"]["ref"] == "main" and merged and start <= merged <= as_of)):
            pulls[pr["number"]] = dict(number=pr["number"], author=(pr["user"] or {}).get("login"),
                opened=pr["created_at"], merged=merged, updated=pr["updated_at"],
                ref=pr["head"]["ref"], head=pr["head"].get("sha"), base=pr["base"]["ref"], base_sha=pr["base"].get("sha"),
                merge_sha=pr.get("merge_commit_sha"), body=pr["body"] or "", comments=[])

    verify_census(api, pulls.values(), since, until, as_of, start)
    for row in pages(api, "issues/comments", since=start, sort="updated", direction="desc"):
        number = int(row["issue_url"].rsplit("/", 1)[1])
        if number in pulls and VERDICT.match(row["body"] or ""):
            record = {k: row[k] for k in ("id", "body", "created_at", "updated_at")}
            record.update(login=(row["user"] or {}).get("login"), association=row.get("author_association"))
            pulls[number]["comments"].append(record)

    def collect(pr):
        key = pr["number"], pr["base_sha"], pr["head"]
        commits = (cache or {}).get(key)
        if commits is None:
            commits = [[r["sha"], r["commit"]["message"]] for r in pages(api, f"pulls/{pr['number']}/commits")]
        pr["commits"] = commits
        return pr

    with ThreadPoolExecutor(max_workers=4) as pool:
        return dict(history_since=start, as_of=as_of, pulls=list(pool.map(collect, pulls.values())))


def judgments(pr, as_of):
    revisions = {sha for sha, _ in pr["commits"]} | {pr.get("head"), pr.get("merge_sha")}
    for row in pr["comments"]:
        match = VERDICT.match(row["body"])
        at = row.get("updated_at", row["created_at"])
        if not match or max(at, row["created_at"]) > as_of:
            continue
        shas = {s for s in revisions if s and s.startswith(match[3].lower())}
        if len(shas) == 1:
            yield dict(sha=shas.pop(), at=at, created_at=row["created_at"], id=f"comments/{row['id']}",
                       login=row.get("login"), association=row.get("association"),
                       kind=match[4].upper().replace(" ", "-"), role=match[2].lower())


def normalize_pulls(data):
    rows = []
    for pr in data["pulls"]:
        commits = pr["commits"]
        pinned = commits and len(commits) < 250 and commits[-1][0] == pr.get("head")
        names = {p.casefold() for _, msg in commits for p in TRAILER.findall(msg.rstrip().rsplit("\n\n", 1)[-1])}
        byline_cutoff = min(pr["merged"] or data["as_of"], data["as_of"])
        if pr.get("updated") and pr["updated"] <= byline_cutoff:
            names.update(p.casefold() for p in BYLINE.findall(pr["body"]) + TRAILER.findall(pr["body"]))
        lane, separator, _ = (pr["ref"] or "").partition("/")
        provider = names.pop().capitalize() if pinned and len(names) == 1 else None
        reviews = list(judgments(pr, data["as_of"]))
        rows.append(dict(number=pr["number"], author=pr["author"], provider=provider,
                         lane=lane if separator and lane else None, opened=pr["opened"], base=pr["base"],
                         merged=pr["merged"], lines=None, heads=[],
                         reviews=[r for r in reviews if r["role"] == "second provider"],
                         post_merge_failures=[r for r in reviews if r["kind"] == "FAIL"
                                              and r["sha"] in (pr.get("head"), pr.get("merge_sha"))
                                              and pr["merged"] and r["created_at"] > pr["merged"]]))
    return rows


def utc(value):
    stamp = datetime.fromisoformat(value.replace("Z", "+00:00"))
    if stamp.tzinfo is None:
        raise ValueError(f"timestamp needs a timezone: {value}")
    return stamp.astimezone(timezone.utc)


def incident_aliases(incidents):
    aliases = {}
    for row in incidents:
        linked = row.get("aliases", [])
        if not isinstance(linked, list):
            raise ValueError("incident aliases must be a list of evidence ids")
        for evidence_id in [row["id"], *linked]:
            if not isinstance(evidence_id, str) or not evidence_id.strip():
                raise ValueError("incident ids must be nonempty strings")
            if aliases.setdefault(evidence_id, row["id"]) != row["id"]:
                raise ValueError(f"ambiguous incident alias: {evidence_id}")
    return aliases


def observed_defects(pulls, incidents, cutoff):
    """Manual entries may alias multiple automatic verdict ids to one incident."""
    by_number = {p["number"]: p for p in pulls}
    evidence = [e for e in incidents if utc(e["at"]) <= cutoff]
    aliases = incident_aliases(evidence)
    for p in pulls:
        evidence.extend(dict(r, pr=p["number"])
                        for r in p.get("post_merge_failures", [])
                        if r["kind"] == "FAIL" and utc(r["at"]) <= cutoff)
    found = {}
    for event in sorted(evidence, key=lambda e: utc(e["at"])):
        key = aliases.get(event["id"], event["id"])
        number = event["pr"]
        if type(number) is not int or number < 1:
            raise ValueError("incident requires a positive integer PR")
        pr = by_number.get(number)
        if not pr or not pr.get("merged") or utc(event["at"]) <= utc(pr["merged"]):
            raise ValueError(f"incident {key} cites an unknown merge at the time of observation")
        if key in found and found[key]["pr"] != number:
            raise ValueError(f"incident {key} links multiple PRs")
        result = found.setdefault(key, dict(id=key, pr=number, at=event["at"], evidence_ids=[]))
        if event["id"] not in result["evidence_ids"]:
            result["evidence_ids"].append(event["id"])
            if "sha" in event:
                result.setdefault("verdicts", []).append(event)
    return list(found.values())


def author_row(identity, merges, by_pr):
    author, provider, lane = identity
    dates = sorted((p["merged"] for p in merges), key=utc)
    count = sum(len(by_pr[p["number"]]) for p in merges)
    complete = bool(merges) and all(p.get("defects_complete") is True for p in merges)
    checks = [(all(identity), "author, provider or lane unknown"), (not count, "attributable defect"),
              (len(dates) >= 10, "fewer than 10 merges"),
              (dates and utc(dates[-1]) - utc(dates[0]) >= timedelta(days=7), "span below 7 days"),
              (complete, "defect coverage unknown")]
    why = next((reason for ok, reason in checks if not ok), "")
    return dict(author=author, provider=provider, lane=lane, merged_count=len(dates),
                first_merge=dates[0] if dates else None, last_merge=dates[-1] if dates else None,
                defects=count, defect_coverage_complete=complete, trusted=not why, why=why or "criteria met")


def build_report(pulls, incidents, since, until, as_of):
    start, end, cutoff = map(utc, (since, until, as_of))
    if len({p["number"] for p in pulls}) != len(pulls):
        raise ValueError("duplicate PR in snapshot")
    defects = observed_defects(pulls, incidents, cutoff)
    by_pr = defaultdict(list)
    for event in defects:
        by_pr[event["pr"]].append(event["id"])
    groups, cohort = defaultdict(list), []
    for p in sorted(pulls, key=lambda p: p["number"]):
        opened = utc(p["opened"])
        if opened > cutoff:
            continue
        merged = p["merged"] if p.get("merged") and utc(p["merged"]) <= cutoff else None
        identity = tuple(p.get(k) for k in ("author", "provider", "lane"))
        if merged and p.get("base") == "main":
            groups[identity].append(p)
        if start <= opened <= end:
            groups.setdefault(identity, [])
            cohort.append(dict(number=p["number"], author=identity[0], provider=identity[1], lane=identity[2],
                               opened=p["opened"], base=p.get("base"), merged_at=merged,
                               lines=p.get("lines"), eligible=False, first_failure="head evidence unknown",
                               frozen_head=None, frozen_at=None, hours_to_merge=None, blocking_findings=None,
                               comparison=None, defect_ids=by_pr[p["number"]]))
    authors = [author_row(key, groups[key], by_pr)
               for key in sorted(groups, key=lambda k: tuple(v or "" for v in k))]
    return dict(since=since, until=until, as_of=as_of, authors=authors, prs=cohort, defects=defects)


def bound(value, end):
    if len(value) == 10:
        value += "T23:59:59Z" if end else "T00:00:00Z"
    return utc(value).strftime("%Y-%m-%dT%H:%M:%SZ")


def api(path):
    repo = "repos/{owner}/{repo}"
    endpoint = f"{repo}/{path}" if path else repo
    if path.startswith("search/issues?"):
        endpoint = path
    result = subprocess.run(["gh", "api", "--method", "GET", endpoint],
                            stdout=subprocess.PIPE, check=True, timeout=120)
    return json.loads(result.stdout)


def fetch(args):
    since, until = bound(args.since, False), bound(args.until, True)
    if since > until:
        raise ValueError("opening window ends before it starts")
    dest = args.out / "census.json"
    if dest.exists():
        raise FileExistsError(f"snapshot exists: {dest}")
    as_of = datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")
    repository = api("")["full_name"]
    prior = args.reuse_commits.read_bytes() if args.reuse_commits else None
    cache = commit_cache(json.loads(prior), repository) if prior is not None else {}
    data = fetch_pulls(api, since, until, as_of, cache)
    snapshot = dict(schema=SCHEMA, repository=repository, since=since, until=until, **data,
                    prior_sha256=sha256(prior).hexdigest() if prior is not None else None)
    args.out.mkdir(parents=True, exist_ok=True)
    payload = json.dumps(snapshot, ensure_ascii=False)
    with NamedTemporaryFile("w", dir=args.out, encoding="utf-8", delete=False) as output:
        temporary = Path(output.name)
    try:
        temporary.write_text(payload, encoding="utf-8")
        os.link(temporary, dest)
    finally:
        temporary.unlink()


def author_table(authors):
    lines = ["| Login | Provider | Lane | Merged | First merge (UTC) | Last merge (UTC) | Recorded defect observations | Trusted | Reason |",
             "|---|---|---|---:|---|---|---:|---|---|"]
    for row in authors:
        values = [row[k] for k in ("author", "provider", "lane", "merged_count", "first_merge",
                                    "last_merge", "defects")]
        values += ["yes" if row["trusted"] else "no", row["why"]]
        lines.append("| " + " | ".join(str(v).replace("|", r"\|") if v is not None else "unknown"
                                      for v in values) + " |")
    return "\n".join(lines)


def render(report):
    lines = ["# Earned-trust census", "", f"Opening window {report['since']} through {report['until']}. "
             f"Observed through {report['as_of']}. Merge history begins {report['history_since']}.",
             "Counts cover SHA-bound verdict comments and manual incidents. Formal review bodies lack historical edit times. "
             "Sweep defects and historical head evidence are unmeasured.",
             "", author_table(report["authors"]), "", "## Trial reads", ""]
    for name, row in report["reads"].items():
        lines.append(f"- {name.title()} ({row['scheduled']}), observed through {row['as_of']}. "
                     f"{row['opened']} opened, {row['merged']} merged ({row['merged_main']} to main), "
                     f"{row['observed_defects']} recorded defect observations.")
    lines.extend(["", "## Success criteria", ""] + [f"- {criterion} — Unknown" for criterion in CRITERIA])
    lines += ["", "Shard versions, head eligibility and the comparison group await historical head and sweep evidence.",
              "", "## Pull requests", "", "| PR | Opened (UTC) | Merged (UTC) | Eligibility |",
              "|---:|---|---|---|"]
    lines.extend(f"| {p['number']} | {p['opened']} | {p['merged_at'] or '—'} | {p['first_failure']} |"
                 for p in report["prs"])
    return "\n".join(lines) + "\n"


def replay(args):
    src = args.snapshots / "census.json"
    files = [src, args.out_json, args.out_md] + ([args.incidents] if args.incidents else [])
    if len({p.resolve() for p in files}) != len(files):
        raise ValueError("snapshot, incidents and outputs need distinct paths")
    raw = src.read_bytes()
    snap = json.loads(raw)
    if snap["schema"] != SCHEMA:
        raise ValueError("unsupported census schema")
    incident_bytes = args.incidents.read_bytes() if args.incidents else b"[]"
    incidents = json.loads(incident_bytes) if incident_bytes.strip() else []
    if not isinstance(incidents, list):
        raise ValueError("incidents must be a JSON list")
    pulls = normalize_pulls(snap)

    def read(at):
        return build_report(pulls, incidents, snap["since"], snap["until"], at)

    report = read(snap["as_of"])
    report.update(repository=snap["repository"], history_since=snap["history_since"], schema=SCHEMA,
                  snapshot_sha256=sha256(raw).hexdigest(), incidents_sha256=sha256(incident_bytes).hexdigest(),
                  shards=None, criteria=dict.fromkeys(CRITERIA), reads={})
    for name, scheduled in READS.items():
        at = min(scheduled, snap["as_of"])
        rows = read(at)["prs"]
        report["reads"][name] = dict(scheduled=scheduled, as_of=at, provisional=at < scheduled,
                                     opened=len(rows), merged=sum(bool(p["merged_at"]) for p in rows),
                                     merged_main=sum(p["base"] == "main" and bool(p["merged_at"]) for p in rows),
                                     observed_defects=sum(len(p["defect_ids"]) for p in rows),
                                     eligible_merged=None, comparison=None)
    args.out_json.write_text(json.dumps(report, indent=2) + "\n", encoding="utf-8")
    args.out_md.write_text(render(report), encoding="utf-8")


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    commands = parser.add_subparsers(dest="command", required=True)
    collect = commands.add_parser("fetch")
    for flag in ("since", "until"):
        collect.add_argument("--" + flag, required=True)
    collect.add_argument("--out", type=Path, required=True)
    collect.add_argument("--reuse-commits", type=Path, metavar="CENSUS_JSON",
                         help="reuse commit messages for identical repository, PR, base and head")
    report = commands.add_parser("report")
    for flag in ("snapshots", "out-json", "out-md", "incidents"):
        report.add_argument("--" + flag, type=Path, required=flag != "incidents")
    args = parser.parse_args(argv)
    (fetch if args.command == "fetch" else replay)(args)


if __name__ == "__main__":
    main()
