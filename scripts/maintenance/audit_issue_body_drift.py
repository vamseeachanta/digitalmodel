#!/usr/bin/env python3
"""Audit GitHub issues for migration body-overwrite drift (read-only).

Detects the failure mode from digitalmodel #690: a WRK->issue migration script
stamped a machine-generated `## WRK-NNN: ...` template over the human-authored
body of an unrelated legacy issue. The tell-tale signs are:

  1. The body STARTS WITH the migration template signature `## WRK-NNN: ...`
     followed by the generated `**Status:** ... | **Priority:** ...` line.
  2. The WRK id embedded in the body does not correspond to the issue's title
     (title/body mismatch), or the body references foreign repos
     (assethold, acma-projects, ...) that don't match the issue.

Read-only: only `gh api` GET calls. Exits non-zero if any drift is found, so it
can gate CI.

Usage:
    # audit an explicit list
    python scripts/maintenance/audit_issue_body_drift.py \
        --repo vamseeachanta/digitalmodel --issues 13,17,18,19,20,23,28,29

    # sweep every open issue below a number
    python scripts/maintenance/audit_issue_body_drift.py \
        --repo vamseeachanta/digitalmodel --below 130 --state open
"""
from __future__ import annotations

import argparse
import json
import re
import subprocess
import sys

# Signature 1: body opens with the migration template header.
WRK_HEADER = re.compile(r"^\s*##\s*(WRK-?\d+)\s*:", re.IGNORECASE)
# Signature 2: the generated metadata line the template always emits.
TEMPLATE_META = re.compile(r"\*\*Status:\*\*.*\|\s*\*\*Priority:\*\*", re.IGNORECASE)
# Foreign-repo leakage seen in #690 (WRK->issue mapping wrote to wrong target).
FOREIGN_REPOS = ("assethold", "acma-projects")


def gh_api(path: str) -> dict | list:
    r = subprocess.run(["gh", "api", path], capture_output=True, text=True)
    if r.returncode != 0:
        raise RuntimeError(f"gh api {path} failed: {r.stderr.strip()}")
    return json.loads(r.stdout)


def title_tokens(title: str) -> set[str]:
    return {t for t in re.split(r"[^a-z0-9]+", title.lower()) if len(t) > 2}


def classify(issue: dict) -> dict | None:
    """Return a drift record if the issue body looks machine-overwritten."""
    body = issue.get("body") or ""
    title = issue.get("title") or ""
    num = issue.get("number")

    m = WRK_HEADER.match(body.strip())
    if not m:
        return None  # body does not open with a WRK template -> not this drift

    wrk_id = m.group(1).upper()
    flags = ["template-header"]
    if TEMPLATE_META.search(body):
        flags.append("template-metadata")

    # title/body mismatch: the WRK template title vs the issue title.
    # Pull the template's own title (rest of the `## WRK-NNN: <title>` line).
    tmpl_title = body.strip().splitlines()[0].split(":", 1)[-1].strip()
    overlap = title_tokens(title) & title_tokens(tmpl_title)
    if not overlap:
        flags.append("title-body-mismatch")

    foreign = [r for r in FOREIGN_REPOS if r in body.lower()]
    if foreign:
        flags.append("foreign-repo:" + ",".join(foreign))

    # Confidence: header alone is weak; header+metadata+mismatch is strong.
    strong = "template-metadata" in flags and (
        "title-body-mismatch" in flags or foreign
    )
    return {
        "issue": num,
        "title": title,
        "stamped_wrk": wrk_id,
        "template_title": tmpl_title,
        "flags": flags,
        "confidence": "high" if strong else "medium",
    }


def iter_issues(repo: str, args) -> list[dict]:
    if args.issues:
        nums = [int(x) for x in args.issues.split(",") if x.strip()]
        return [gh_api(f"repos/{repo}/issues/{n}") for n in nums]
    # sweep mode
    out: list[dict] = []
    page = 1
    while True:
        batch = gh_api(
            f"repos/{repo}/issues?state={args.state}&per_page=100&page={page}"
        )
        if not batch:
            break
        for it in batch:
            if "pull_request" in it:
                continue  # skip PRs
            if args.below and it.get("number", 0) >= args.below:
                continue
            out.append(it)
        page += 1
    return out


def main(argv=None) -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--repo", required=True)
    g = ap.add_mutually_exclusive_group(required=True)
    g.add_argument("--issues", help="comma-separated issue numbers")
    g.add_argument("--below", type=int, help="sweep open issues below this number")
    ap.add_argument("--state", default="open", choices=["open", "closed", "all"])
    ap.add_argument("--json", action="store_true", help="emit JSON")
    args = ap.parse_args(argv)

    issues = iter_issues(args.repo, args)
    drift = [d for d in (classify(it) for it in issues) if d]

    if args.json:
        print(json.dumps(drift, indent=2))
    else:
        if not drift:
            print(f"OK: no body-overwrite drift across {len(issues)} issue(s).")
        else:
            print(f"DRIFT: {len(drift)} issue(s) with machine-overwritten bodies:\n")
            for d in drift:
                print(f"  #{d['issue']} [{d['confidence']}] {d['title']!r}")
                print(f"      stamped {d['stamped_wrk']} ('{d['template_title']}')")
                print(f"      flags: {', '.join(d['flags'])}")
    return 1 if drift else 0


if __name__ == "__main__":
    raise SystemExit(main())
