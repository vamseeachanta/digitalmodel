# Issue body recovery (#690)

A WRK→GitHub-issue migration script overwrote 8 legacy issue bodies with
unrelated WRK template content. This directory holds the **recovered original
bodies** pulled READ-ONLY from GitHub's edit history, for the user to restore
manually. Nothing here has been written back to GitHub.

## Contents

- `issue-<n>.md` — the recovered original (pre-overwrite) body for each issue.
- `RECOVERY_SUMMARY.json` — per-issue recovered/not + source revision timestamp.
- `RESTORE_COMMANDS.sh` — human-gated `gh issue edit --body-file ...` per issue
  (review each file first; NOT run automatically).
- `migration-script-fix.md` — root cause + the non-destructive guard + the exact
  upstream patch to apply to `workspace-hub/scripts/knowledge/update-github-issue.py`.

## Recovery status (7 of 8)

| Issue | Recovered | Source revision |
|-------|-----------|-----------------|
| #13 | yes | 2024-12-29 |
| #17 | **no** — never had a body (0 edits, body always null; nothing to recover) |
| #18 | yes | 2024-12-29 |
| #19 | yes | 2024-12-29 |
| #20 | yes | 2024-12-29 |
| #23 | yes | 2023-11-05 |
| #28 | yes | 2023-12-08 |
| #29 | yes | 2023-12-08 |

## How to restore (user action — outward write)

```bash
# Review the recovered files first, then:
bash docs/issue-body-recovery/RESTORE_COMMANDS.sh
# or run individual lines as preferred.
```

## Reproduce the recovery / re-audit (read-only)

```bash
python scripts/maintenance/recover_issue_bodies.py \
    --repo vamseeachanta/digitalmodel --issues 13,17,18,19,20,23,28,29

python scripts/maintenance/audit_issue_body_drift.py \
    --repo vamseeachanta/digitalmodel --issues 13,17,18,19,20,23,28,29
# sweep mode: --below 130 --state open
```
