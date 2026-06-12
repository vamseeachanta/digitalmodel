# Exit handoff — issue #2760 SIROCCO current/rudder force package

Date: 2026-05-21T14:28:44-05:00

## Status

**PARTIAL / NOT CLOSEABLE.**

This work consumed too much time without a reliable closeout. Do **not** treat the current artifacts as final client-ready deliverables. The implementation has tests passing for the current draft state, but the central engineering/source requirement is still not satisfied: the report path still uses hardcoded OCIMF-style coefficient formulas in `_row()` rather than resolving/interpolating coefficients from the approved licensed workbook route at calculation time.

## Active task state

- `produce-artifacts`: in progress, partial artifacts exist but are not trustworthy/final.
- `validate-review-close`: not started/completed; do not close #2760.

## GitHub issue state verified

- Issue: https://github.com/vamseeachanta/workspace-hub/issues/2760
- State: open.
- Label: `status:plan-approved` is present, so implementation is authorized.
- Required closeout remains: TDD completion, artifact regeneration, code-stage adversarial review, commit/push, issue comments, parent issue update, close only if verified.

## Current digitalmodel working tree state

Repo: `/mnt/local-analysis/digitalmodel`
Branch: `main`
HEAD observed: `212dfd26 #617 cross-review fixes — resolver consolidation + symmetric env-var error + test tightening`

Dirty/untracked #2760-related files observed:

```text
M src/digitalmodel/naval_architecture/b1528_sirocco_current_heading_rudder_report.py
M src/digitalmodel/naval_architecture/data/b1528_sirocco_current_heading_rudder.yml
M tests/naval_architecture/test_b1528_sirocco_current_heading_rudder.py
?? docs/domains/marine-engineering/b1528-sirocco-current-rudder-force-manifest.json
?? docs/domains/marine-engineering/b1528-sirocco-current-rudder-force-report.html
?? docs/domains/marine-engineering/b1528-sirocco-current-rudder-force-report.md
?? docs/plans/2026-05-21-issue-2760-claude-completion-handoff.md
?? docs/session-handoffs/2026-05-21-issue-2760-sirocco-exit-handoff.md
?? tests/naval_architecture/test_issue_2760_sirocco_current_rudder_revision.py
```

Unrelated/pre-existing dirty state also observed in digitalmodel:

```text
T .codex/skills
T .gemini/skills
M AGENTS.md
```

Do not stage those unless a fresh inspection proves they belong to another active task.

## Artifacts currently present

Digitalmodel generated outputs exist under:

```text
/mnt/local-analysis/digitalmodel/outputs/b1528_sirocco/current_rudder_force/b1528_sirocco_current_rudder_force_results.csv
/mnt/local-analysis/digitalmodel/outputs/b1528_sirocco/current_rudder_force/b1528_sirocco_current_rudder_force_results.json
/mnt/local-analysis/digitalmodel/outputs/b1528_sirocco/current_rudder_force/b1528_sirocco_current_rudder_force_provenance.json
/mnt/local-analysis/digitalmodel/outputs/b1528_sirocco/current_rudder_force/b1528_sirocco_current_rudder_force_report.md
/mnt/local-analysis/digitalmodel/outputs/b1528_sirocco/current_rudder_force/b1528_sirocco_current_rudder_force_report.html
/mnt/local-analysis/digitalmodel/outputs/b1528_sirocco/current_rudder_force/b1528_sirocco_current_rudder_force_report.docx
/mnt/local-analysis/digitalmodel/outputs/b1528_sirocco/current_rudder_force/b1528_sirocco_current_rudder_force_report.pdf
/mnt/local-analysis/digitalmodel/outputs/b1528_sirocco/current_rudder_force/b1528_sirocco_current_rudder_force_manifest.json
```

File sizes verified:

```text
.../current_rudder_force_report.docx size=38729
.../current_rudder_force_report.pdf  size=94841
```

ACMA output copies also exist but were not shown as tracked changes in workspace-hub:

```text
/mnt/local-analysis/workspace-hub/acma-projects/B1528/output/b1528_sirocco_current_rudder_force_report.docx size=38729
/mnt/local-analysis/workspace-hub/acma-projects/B1528/output/b1528_sirocco_current_rudder_force_report.pdf  size=94841
```

Treat all of these artifacts as draft/stale until the workbook-backed coefficient adapter is implemented and artifacts are regenerated.

## Tests run

Command run from `/mnt/local-analysis/digitalmodel`:

```bash
uv run pytest tests/naval_architecture/test_issue_2760_sirocco_current_rudder_revision.py tests/naval_architecture/test_b1528_sirocco_current_heading_rudder.py -q
```

Observed result:

```text
21 passed in 15.45s
```

Important: passing tests are not sufficient because they currently do not force the main implementation to use workbook-derived OCIMF coefficients. Strengthen the tests first before further implementation.

## Known correctness blocker

In `/mnt/local-analysis/digitalmodel/src/digitalmodel/naval_architecture/b1528_sirocco_current_heading_rudder_report.py`, `_row()` still contains hardcoded coefficient formulas:

```python
ocimf_cx = 1.05 * heading_abs_cos
ocimf_cy = 1.0 * heading_sin
ocimf_cm = 0.55 * heading_sin
```

This violates the approved #2760 plan and issue comment decision ledger. The correct source route is:

```text
/mnt/ace/acma-codes/OCIMF/OCIMF Coef.xlsx
```

The next agent must implement a minimal workbook adapter/interpolator and fail closed when the workbook/provenance/citation route is unavailable. Do not invent, embed, or serialize a reusable OCIMF coefficient corpus in the repo.

## Existing handoff prompt

A more detailed execution prompt already exists at:

```text
/mnt/local-analysis/digitalmodel/docs/plans/2026-05-21-issue-2760-claude-completion-handoff.md
```

It contains allowed paths, forbidden paths, TDD assertions, validation commands, and closeout rules. Use it as a starting checklist, but re-ground from live issue state and current git state before acting.

## Recommended fresh-start sequence

1. **Stop trusting current artifacts.** Keep them only as draft output examples.
2. Re-read the approved plan in workspace-hub:
   ```text
   /mnt/local-analysis/workspace-hub/docs/plans/2026-05-20-issue-2760-b1528-sirocco-force-review-revision.md
   ```
3. Re-read #2760 comments, especially the OCIMF source update and plan-approved state.
4. Start with failing tests that prove `_row()`/report generation does not use hardcoded trigonometric coefficient placeholders.
5. Implement `OCIMFExcelAdapter` or equivalent local helper against `/mnt/ace/acma-codes/OCIMF/OCIMF Coef.xlsx`.
6. Regenerate MD/HTML/DOCX/PDF/manifest after tests pass.
7. Parse/inspect DOCX and PDF text enough to verify required sections and absence of heatmap/resultant main presentation.
8. Run code-stage adversarial review. Do not close with unresolved MAJOR findings.
9. Commit only intended #2760 files. Do not stage `.codex/skills`, `.gemini/skills`, or `AGENTS.md`.
10. Push, comment on #2760 and parent #2642, close #2760 only after pushed artifacts are verified.

## Workspace-hub control repo note

`/mnt/local-analysis/workspace-hub` has substantial unrelated dirty state in `.claude/skills`, `.claude/state`, config, logs, plans, reports, and review artifacts. Do not mix #2760 closeout with that repo-wide state. If ACMA output files must be committed there, inspect and stage only the exact ACMA output paths.
