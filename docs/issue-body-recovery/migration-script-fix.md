# Migration-script fix for #690 (body-overwrite data loss)

## Root cause

The offending script is **`workspace-hub/scripts/knowledge/update-github-issue.py`**
(it lives in the `workspace-hub` repo, not `digitalmodel`). Its `--create` and
`--update` paths run, unconditionally:

```python
# --create branch (lines ~257-264)
existing_ref = fm.get("github_issue_ref", "")
if existing_ref and existing_ref.startswith("http"):
    num = existing_ref.rstrip("/").split("/")[-1]
    _gh_run(["gh", "issue", "edit", num, "--body", body] + repo_flag)   # <-- clobbers
...
# --update branch (lines ~271-273)
num = _get_issue_number(fm)
_gh_run(["gh", "issue", "edit", num, "--body", body] + repo_flag)        # <-- clobbers
```

When a WRK file's `github_issue_ref` pointed at the **wrong** issue (a stale or
mis-mapped legacy digitalmodel issue), the script overwrote that issue's
human-authored body with the generated `## WRK-NNN` template. There was:

- **no check** that the target issue's existing body belonged to this WRK,
- **no dry-run default** (it wrote immediately),
- **no body-preservation guard**.

Result: 7 legacy bodies (#13, #18, #19, #20, #23, #28, #29) were replaced with
unrelated WRK content; several even referenced foreign repos (assethold,
acma-projects) confirming the WRK→issue mapping wrote to wrong targets.

## The fix (non-destructive guard)

The guard module **`scripts/maintenance/safe_issue_body_update.py`** (added in this
repo, with tests in `tests/maintenance/test_safe_issue_body_update.py`) implements
the rule the migration must obey BEFORE any body write:

- Read the issue's **current** body first.
- **Allow** the write only when it cannot lose human content:
  - current body is empty/null (nothing to lose), or
  - current body is already *this* WRK's template (idempotent re-stamp).
- **Block** (`UnsafeBodyOverwrite`) when:
  - current body is human-authored (no WRK header), or
  - current body is a *different* WRK's template (the wrong-target #690 case).
- **Dry-run by default**; only writes when `apply=True` AND the guard passes.

### Upstream patch to apply (workspace-hub — user action)

In `workspace-hub/scripts/knowledge/update-github-issue.py`, replace each bare
`_gh_run(["gh", "issue", "edit", num, "--body", body] + repo_flag)` body write
with a guarded call. Minimal diff:

```diff
+from pathlib import Path
+import re
+
+_WRK_HEADER = re.compile(r"^\s*##\s*(WRK-?\d+)\s*:", re.IGNORECASE)
+
+def _assert_safe_body_overwrite(num, repo_flag, wrk_id):
+    """Refuse to overwrite a human-authored or wrong-WRK issue body (#690)."""
+    r = subprocess.run(["gh", "issue", "view", num, "--json", "body",
+                        "-q", ".body"] + repo_flag, capture_output=True, text=True)
+    existing = (r.stdout if r.returncode == 0 else "").strip()
+    if not existing:
+        return  # empty body, safe
+    m = _WRK_HEADER.match(existing)
+    norm = lambda s: re.sub(r"[^0-9a-z]", "", s.lower())
+    if not m:
+        sys.exit(f"REFUSED: #{num} has a human-authored body; would clobber (#690).")
+    if norm(m.group(1)) != norm(wrk_id):
+        sys.exit(f"REFUSED: #{num} body is {m.group(1)} not {wrk_id}; "
+                 f"wrong-target mapping (#690).")
+
@@ --create branch
         if existing_ref and existing_ref.startswith("http"):
             num = existing_ref.rstrip("/").split("/")[-1]
+            _assert_safe_body_overwrite(num, repo_flag, args.wrk_id)
             _gh_run(["gh", "issue", "edit", num, "--body", body] + repo_flag)
@@ --update branch
         num = _get_issue_number(fm)
+        _assert_safe_body_overwrite(num, repo_flag, args.wrk_id)
         _gh_run(["gh", "issue", "edit", num, "--body", body] + repo_flag)
```

Alternatively, retire the body-write entirely (have the migration only create
NEW issues / post comments, never edit an existing body). Either approach
prevents recurrence; the guard above is the minimal, behavior-preserving fix.

## Verification (read-only, against live issues)

```
$ echo "x" | python scripts/maintenance/safe_issue_body_update.py \
      --repo vamseeachanta/digitalmodel --issue 18 --wrk WRK-1149 --json
{"status": "BLOCKED", "reason": "existing body is a DIFFERENT WRK template (WRK-023)
 than the one being written (WRK-1149); wrong-target mapping — refusing to overwrite
 (this is the #690 defect)"}   # exit 2
```

Unit tests: `6 passed` (see `tests/maintenance/test_safe_issue_body_update.py`).
