#!/usr/bin/env python3
"""Non-destructive guard for WRK->GitHub-issue body updates (fix for #690).

Background
----------
The original migration helper (workspace-hub
`scripts/knowledge/update-github-issue.py`) ran, unconditionally:

    gh issue edit <num> --body <wrk-template>

whenever a WRK file's `github_issue_ref` pointed at an existing issue number.
When that ref was wrong/stale (it pointed at unrelated legacy digitalmodel
issues #13/#18/#19/#20/#23/#28/#29), it overwrote the human-authored body with a
machine WRK template — silent data loss. See digitalmodel #690.

This module is the safety check that the migration MUST pass before it is
allowed to write a body. It refuses to clobber a human-authored body and is
dry-run by default. Import `assert_safe_to_overwrite` / `gh_edit_body_safely`
into the migration, or run this file standalone to vet a single edit.

Read-only unless `--apply` is passed AND the guard allows the write.
"""
from __future__ import annotations

import argparse
import json
import re
import subprocess
import sys

WRK_HEADER = re.compile(r"^\s*##\s*(WRK-?\d+)\s*:", re.IGNORECASE)


class UnsafeBodyOverwrite(Exception):
    """Raised when an edit would clobber a human-authored issue body."""


def _norm_wrk(s: str) -> str:
    return re.sub(r"[^0-9a-z]", "", s.lower())


def current_body(repo: str, number: int) -> str:
    r = subprocess.run(
        ["gh", "api", f"repos/{repo}/issues/{number}", "--jq", ".body"],
        capture_output=True, text=True,
    )
    if r.returncode != 0:
        raise RuntimeError(f"gh api read failed for #{number}: {r.stderr.strip()}")
    # gh api --jq emits a trailing newline; a null body comes back as "null".
    out = r.stdout.rstrip("\n")
    return "" if out == "null" else out


def assert_safe_to_overwrite(existing: str | None, wrk_id: str) -> None:
    """Allow the write only if it cannot lose human-authored content.

    Safe (allowed) cases:
      * existing body is empty/null            -> nothing to lose
      * existing body is already THIS wrk's template (idempotent re-stamp)
    Unsafe (blocked) cases:
      * existing body is human-authored        -> would clobber
      * existing body is a DIFFERENT wrk's template (wrong-target mapping,
        exactly the #690 failure) -> would clobber the wrong issue
    """
    existing = (existing or "").strip()
    if not existing:
        return  # empty body, safe to populate

    m = WRK_HEADER.match(existing)
    if not m:
        raise UnsafeBodyOverwrite(
            "existing body is human-authored (no WRK template header); "
            "refusing to overwrite"
        )
    existing_wrk = _norm_wrk(m.group(1))
    if existing_wrk != _norm_wrk(wrk_id):
        raise UnsafeBodyOverwrite(
            f"existing body is a DIFFERENT WRK template ({m.group(1)}) than "
            f"the one being written ({wrk_id}); wrong-target mapping — refusing "
            f"to overwrite (this is the #690 defect)"
        )
    # else: same wrk template already there -> idempotent, allow.


def gh_edit_body_safely(repo: str, number: int, wrk_id: str, body: str,
                        *, apply: bool = False) -> str:
    """Vet then (optionally) perform `gh issue edit --body`.

    Default is dry-run. Raises UnsafeBodyOverwrite if the write would clobber.
    Returns a status string.
    """
    existing = current_body(repo, number)
    assert_safe_to_overwrite(existing, wrk_id)
    if not apply:
        return f"DRY-RUN ok: would safely set body of {repo}#{number} for {wrk_id}"
    r = subprocess.run(
        ["gh", "issue", "edit", str(number), "--repo", repo, "--body", body],
        capture_output=True, text=True,
    )
    if r.returncode != 0:
        raise RuntimeError(f"gh issue edit failed: {r.stderr.strip()}")
    return f"APPLIED: set body of {repo}#{number} for {wrk_id}"


def main(argv=None) -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--repo", required=True)
    ap.add_argument("--issue", required=True, type=int)
    ap.add_argument("--wrk", required=True, help="WRK id of the body being written")
    ap.add_argument("--body-file", help="file with the new body (else stdin)")
    ap.add_argument("--apply", action="store_true",
                    help="actually write (default: dry-run)")
    ap.add_argument("--json", action="store_true")
    args = ap.parse_args(argv)

    body = (open(args.body_file).read() if args.body_file else sys.stdin.read())
    try:
        msg = gh_edit_body_safely(args.repo, args.issue, args.wrk, body,
                                  apply=args.apply)
    except UnsafeBodyOverwrite as e:
        out = {"status": "BLOCKED", "reason": str(e)}
        print(json.dumps(out) if args.json else f"BLOCKED: {e}", file=sys.stderr)
        return 2
    print(json.dumps({"status": "OK", "message": msg}) if args.json else msg)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
