# Exit handoff — SIROCCO 30° HTML report location and client export follow-up

Date: 2026-05-19T14:41:39-05:00
Host: `ace-linux-1`
Repository: `vamseeachanta/digitalmodel`
Branch: `main`
Related issue: https://github.com/vamseeachanta/digitalmodel/issues/598

## Current state

The SIROCCO 30° current-heading-rudder HTML report is located in the `digitalmodel` repository under the generated outputs tree. The HTML report, companion PDF, and native DOCX client-review export are tracked in git and present on `main`.

Canonical local artifact directory:

```text
/mnt/local-analysis/digitalmodel/outputs/b1528_sirocco/current_heading_rudder_30deg_limit/
```

Canonical local files:

```text
outputs/b1528_sirocco/current_heading_rudder_30deg_limit/b1528_sirocco_current_heading_rudder_30deg_limit_report.html
outputs/b1528_sirocco/current_heading_rudder_30deg_limit/b1528_sirocco_current_heading_rudder_30deg_limit_client_review.pdf
outputs/b1528_sirocco/current_heading_rudder_30deg_limit/b1528_sirocco_current_heading_rudder_30deg_limit_client_review.docx
```

Rendered HTML link for review:

```text
https://rawcdn.githack.com/vamseeachanta/digitalmodel/cc5ebd1eb3f647ba535d54ab27a53b888369820b/outputs/b1528_sirocco/current_heading_rudder_30deg_limit/b1528_sirocco_current_heading_rudder_30deg_limit_report.html
```

GitHub source link:

```text
https://github.com/vamseeachanta/digitalmodel/blob/main/outputs/b1528_sirocco/current_heading_rudder_30deg_limit/b1528_sirocco_current_heading_rudder_30deg_limit_report.html
```

## Verification evidence captured before exit

`digitalmodel` state before this handoff was added:

```text
HEAD=cc5ebd1eb3f647ba535d54ab27a53b888369820b
origin/main=cc5ebd1eb3f647ba535d54ab27a53b888369820b
branch=main
status=clean
```

Tracked artifact check:

```text
outputs/b1528_sirocco/current_heading_rudder_30deg_limit/b1528_sirocco_current_heading_rudder_30deg_limit_client_review.docx
outputs/b1528_sirocco/current_heading_rudder_30deg_limit/b1528_sirocco_current_heading_rudder_30deg_limit_client_review.pdf
outputs/b1528_sirocco/current_heading_rudder_30deg_limit/b1528_sirocco_current_heading_rudder_30deg_limit_report.html
```

Artifact sizes at handoff time:

```text
DOCX: 312280 bytes
PDF: 204999 bytes
HTML: 24268287 bytes
```

Remote/render verification:

```text
raw.githubusercontent.com HTML: 200 text/plain; charset=utf-8
rawcdn.githack.com rendered HTML: 200 text/html; charset=utf-8
GitHub blob page: 200 text/html; charset=utf-8
```

## Recent artifact history

The current `digitalmodel` `main` includes three follow-up commits that improved the native client DOCX export while leaving the canonical HTML/PDF report path stable:

```text
cc5ebd1e Add SIROCCO schematic image to client DOCX
a741f9e5 Add SIROCCO chart images to client DOCX
4ae5fd72 Replace SIROCCO client DOCX with native Word document
08691ad7 docs: add SIROCCO client review copies
```

Original HTML report publication was documented in the prior handoff:

```text
docs/session-handoffs/2026-05-16-issue-598-sirocco-force-review-exit-handoff.md
```

That prior handoff records the original 30° report publication commit as:

```text
b00163f9d48057d3eb1188efacc1c24f27fdc3ee feat: publish SIROCCO rudder limit reports
```

## Workspace-hub follow-up captured this session

A targeted skill-library update was committed and pushed in `workspace-hub` to preserve the report-location and native DOCX parity workflow lessons:

```text
3ce8b06bc docs(skills): capture HTML report DOCX parity workflow
```

Updated skill areas:

- `data/office/office-docs` — native DOCX parity from HTML reports.
- `development/html-report-verify` — historical HTML report location and client export closeout.
- `development/targeted-artifact-commit-verification` — binary/report artifact verification.
- `coordination/pre-completion-cleanup-audit` — cleanup audit SIGPIPE pitfall.

`workspace-hub` still has unrelated pre-existing/generated/provider/report dirty state. Those files were intentionally not staged or swept as part of this SIROCCO handoff.

## Restart / follow-up steps

1. Open the rendered HTML link above for visual review.
2. If editing/report follow-up is needed, work from the canonical HTML path and generator:
   - `outputs/b1528_sirocco/current_heading_rudder_30deg_limit/b1528_sirocco_current_heading_rudder_30deg_limit_report.html`
   - `src/digitalmodel/naval_architecture/b1528_sirocco_current_heading_rudder_report.py`
   - `src/digitalmodel/naval_architecture/data/b1528_sirocco_current_heading_rudder.yml`
3. If changing engineering calculations or report generation, follow the plan-gated issue workflow and TDD requirement before implementation.
4. If only doing client-review export formatting, preserve the HTML/PDF as source artifacts and patch the native DOCX in place, then verify `word/media/*` and document relationships before handoff.

## External-action status

No email/message/client send was performed. Links and local paths are ready for manual review or the next follow-up session.

## Closeout proof note

This handoff file should be committed and pushed before final session exit. Final chat closeout should report the post-push `HEAD == origin/main` proof and any remaining expected/unexpected residue from the cleanup audit.
