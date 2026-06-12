# Claude handoff prompt — complete issue #2760 SIROCCO current/rudder force package

You are Claude Code operating in `/mnt/local-analysis/digitalmodel` as a single implementation agent with an internal 4-role loop:

1. **Planner** — re-ground in the approved issue/plan and current worktree state.
2. **TDD Implementer** — write/strengthen failing tests first, then implement the minimum correct code.
3. **Adversarial Reviewer** — challenge the implementation for engineering, licensing, citation, and output-contract failures.
4. **Integrator/Closer** — regenerate artifacts, verify, commit/push only intended files, and update/close GitHub issues only if gates are satisfied.

Do not ask the user questions. If an approved source/citation/output dependency is missing, stop with a clear blocker comment/deliverable instead of inventing data.

## Primary issue

- Workspace issue: https://github.com/vamseeachanta/workspace-hub/issues/2760
- Title: `revise(naval-arch): B1528 SIROCCO force calculation review updates`
- Current label state verified 2026-05-21: `status:plan-approved` is present, so implementation is authorized.
- Parent/source issue: https://github.com/vamseeachanta/workspace-hub/issues/2642
- Related upstream visualization capability shipped in digitalmodel#616: `src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py`

## Repos and working directories

Implementation repo:

```bash
cd /mnt/local-analysis/digitalmodel
```

Control/issue repo:

```bash
/mnt/local-analysis/workspace-hub
```

Approved plan artifact:

```text
/mnt/local-analysis/workspace-hub/docs/plans/2026-05-20-issue-2760-b1528-sirocco-force-review-revision.md
```

## Current known state from Hermes handoff

Focused tests already passed once:

```bash
uv run pytest tests/naval_architecture/test_issue_2760_sirocco_current_rudder_revision.py tests/naval_architecture/test_b1528_sirocco_current_heading_rudder.py -q
# observed: 21 passed in 15.98s
```

But the implementation is **not complete**. The major correctness gap is that the current report module still uses simple hardcoded OCIMF-style trigonometric formulas for current coefficients instead of reading/interpolating coefficients from the approved licensed workbook route at calculation time.

Known current dirty state in `/mnt/local-analysis/digitalmodel` before this handoff:

```text
T .codex/skills
T .gemini/skills
M AGENTS.md
M src/digitalmodel/naval_architecture/b1528_sirocco_current_heading_rudder_report.py
M src/digitalmodel/naval_architecture/data/b1528_sirocco_current_heading_rudder.yml
M tests/naval_architecture/test_b1528_sirocco_current_heading_rudder.py
?? docs/domains/marine-engineering/b1528-sirocco-current-rudder-force-manifest.json
?? docs/domains/marine-engineering/b1528-sirocco-current-rudder-force-report.html
?? docs/domains/marine-engineering/b1528-sirocco-current-rudder-force-report.md
?? tests/naval_architecture/test_issue_2760_sirocco_current_rudder_revision.py
```

Treat `.codex/skills`, `.gemini/skills`, and `AGENTS.md` as unrelated/pre-existing unless your fresh inspection proves they are part of #2760. Do not modify or stage unrelated dirty files.

## Authoritative decisions and requirements

Consume the issue thread and approved plan as authoritative. Key requirements:

- Use licensed off-repo OCIMF workbook route:
  - `/mnt/ace/acma-codes/OCIMF/OCIMF Coef.xlsx`
- Do **not** commit the licensed workbook, source PDFs, reusable extracted coefficient CSV/JSON/YAML corpora, or embedded coefficient tables.
- Calculations must resolve coefficients from the off-repo workbook or an off-repo derived cache at calculation time.
- Fail closed if workbook/provenance/citation cannot be resolved. No placeholder/trig fallback.
- Report basis: generic/reference OCIMF tanker-current basis only; explicitly state it is **not SIROCCO-specific** and is an off-class/screening limitation if applicable.
- Default current speed: `3.08 kn`.
- Current-speed practical range where useful: `0..4 kn`; `4 kn` is the upper range bound, not default.
- Current heading default: `+5°` off bow, port positive.
- Heading plot sweep: `-5°..+5°` at `1°` increments.
- Rudder sweep: `0°..28°` port at `2°` increments.
- Propeller rpm = `0`, neutral `Cr=1.0`.
- Axes/signs: `+X` forward, `+Y` port, `+Z` up, `+N` bow-to-port.
- CoG: longitudinal datum midship; vertical CoG `6.1 m` above keel.
- Remove resultant force calculations from main presentation.
- Remove heatmap.
- Charts should be simple, one variable per chart.
- Use readable angle labels: `Current heading θ (deg)`, `Rudder angle α (deg)`, etc.; avoid dense subscript notation.
- Results in kN and kN·m rounded to 0 decimals for normal/large values; angles may use 1 decimal.
- Required final outputs: Markdown, HTML, Word `.docx`, PDF, manifest/provenance/data artifacts.

## OCIMF workbook intelligence already gathered

The workbook exists and was readable via `openpyxl`. Important sheets/figures:

- `Data 5a-9a`: loaded tanker longitudinal current drag `Cxc` by heading and `WD/T`/bow-shape variants.
- `Data 10a-14a`: loaded tanker lateral current drag `Cyc`; loaded tanker current yaw moment coefficient `Cxyc`; ballast 40%T current coefficients.
- `Data 16a-19a`: current velocity correction `K` and related families.

Next implementation should add a minimal workbook adapter/interpolator for the selected generic/reference tanker basis rather than keeping hardcoded formulas.

## Allowed write paths

In `/mnt/local-analysis/digitalmodel`, you may write only:

```text
src/digitalmodel/naval_architecture/b1528_sirocco_current_heading_rudder_report.py
src/digitalmodel/naval_architecture/data/b1528_sirocco_current_heading_rudder.yml
tests/naval_architecture/test_b1528_sirocco_current_heading_rudder.py
tests/naval_architecture/test_issue_2760_sirocco_current_rudder_revision.py
docs/domains/marine-engineering/b1528-sirocco-current-rudder-force-report.md
docs/domains/marine-engineering/b1528-sirocco-current-rudder-force-report.html
docs/domains/marine-engineering/b1528-sirocco-current-rudder-force-manifest.json
outputs/b1528_sirocco/current_rudder_force/**
```

In `/mnt/local-analysis/workspace-hub`, you may write only if needed for final packaged deliverables and issue closeout:

```text
acma-projects/B1528/output/b1528_sirocco_current_rudder_force_report.docx
acma-projects/B1528/output/b1528_sirocco_current_rudder_force_report.pdf
acma-projects/B1528/output/b1528_sirocco_current_rudder_force_manifest.json
```

If the live implementation discovers a different canonical ACMA output path, document it before writing.

## Read-only paths

```text
/mnt/local-analysis/workspace-hub/docs/plans/2026-05-20-issue-2760-b1528-sirocco-force-review-revision.md
/mnt/local-analysis/workspace-hub/scripts/review/results/2026-05-20-plan-2760-*.md
/mnt/local-analysis/digitalmodel/docs/data/OCIMF_CORPUS_README.md
/mnt/local-analysis/digitalmodel/scripts/python/digitalmodel/ocimf/build_coefficient_explorer.py
/mnt/local-analysis/digitalmodel/docs/domains/charts/phase2/ocimf/ocimf_coefficient_explorer.html
/mnt/local-analysis/digitalmodel/src/digitalmodel/marine_ops/marine_engineering/environmental_loading/ocimf.py
/mnt/local-analysis/digitalmodel/src/digitalmodel/marine_ops/marine_engineering/visualization/polar_force_overlay.py
/mnt/local-analysis/digitalmodel/src/digitalmodel/naval_architecture/b1528_sirocco_moored_current_report.py
/mnt/local-analysis/digitalmodel/src/digitalmodel/naval_architecture/data/b1528_sirocco_moored_current.yml
```

## Forbidden paths and content

Do not modify or stage:

```text
/mnt/ace/acma-codes/OCIMF/OCIMF Coef.xlsx
/mnt/ace/acma-codes/OCIMF/**
.codex/skills
.gemini/skills
AGENTS.md
```

Do not commit:

- licensed workbook files,
- licensed PDFs,
- reusable OCIMF coefficient corpora/tables,
- unrelated repo hygiene/session/governance artifacts,
- broad changes outside the #2760 files listed above.

## Required TDD work before implementation

First strengthen/add tests so the current hardcoded coefficient behavior fails. Required assertions:

1. Default/current report coefficients are read/interpolated from `/mnt/ace/acma-codes/OCIMF/OCIMF Coef.xlsx`, not generated by hardcoded trig formulas.
2. Missing workbook/provenance raises explicit fail-closed error.
3. No reusable OCIMF coefficient corpus is serialized into repo-bound CSV/JSON/YAML/HTML/manifest artifacts.
4. Report text identifies the exact generic/reference basis selected, including relevant figure/sheet IDs and `WD/T`/curve selection basis.
5. Default `+5°` case signs satisfy `+Y` port and `+N` bow-to-port unless the cited OCIMF sign convention explicitly requires documented conversion.
6. Heading domain is exactly `-5..+5°` by `1°`; rudder domain is exactly `0..28°` by `2°`; default speed is exactly `3.08 kn`.
7. Main report/DOCX/HTML/PDF contain no `heatmap`, `resultant`, or `total horizontal force` main-presentation terms.
8. DOCX opens with `python-docx`; PDF/HTML are generated and contain required sections.

## Implementation target

Add a minimal `OCIMFExcelAdapter` or equivalent local helper. It may be local to `b1528_sirocco_current_heading_rudder_report.py` unless a cleaner existing module path is obvious and still within allowed scope.

Minimum behavior:

- Read approved workbook via `openpyxl`.
- Resolve current coefficient families for generic/reference tanker current basis:
  - loaded tanker `Cxc` from A5-A9,
  - loaded tanker `Cyc` from A10,
  - loaded tanker `Cxyc`/yaw from A11,
  - optionally velocity correction `K` from A16 if the selected formula requires it.
- Interpolate by absolute heading angle within workbook heading domain.
- Apply approved port-positive sign convention for signed heading: longitudinal force sign/magnitude should be documented; lateral/yaw signs must follow `+Y` port and `+N` bow-to-port after any OCIMF convention conversion.
- Select and document one generic/reference basis curve (`WD/T`, bow shape, loaded/ballast family, etc.). If basis cannot be selected from available B1528 geometry, stop with blocker rather than guessing.
- Emit provenance/citation sidecar metadata containing pointer path, workbook checksum or mtime/size, sheet/figure IDs, selected basis labels, code_id(s), and license boundary. Do not serialize full coefficient tables.
- Preserve existing report writer behavior for CSV/JSON/MD/HTML/DOCX/PDF/manifest, but update content to accurately describe workbook-derived coefficients and limitations.

## Validation commands

Run from `/mnt/local-analysis/digitalmodel` using `uv run` only:

```bash
uv run pytest tests/naval_architecture/test_issue_2760_sirocco_current_rudder_revision.py tests/naval_architecture/test_b1528_sirocco_current_heading_rudder.py -q
```

If time permits and the focused suite passes:

```bash
uv run pytest tests/naval_architecture/test_b1528_sirocco_moored_current.py tests/naval_architecture/test_issue_2760_sirocco_current_rudder_revision.py tests/naval_architecture/test_b1528_sirocco_current_heading_rudder.py -q
```

Regenerate artifacts with the existing module function/CLI route. If no CLI exists, use a short `uv run python - <<'PY'` snippet that imports `run_b1528_current_heading_rudder_report` and `write_b1528_current_heading_rudder_report` and writes to `outputs/b1528_sirocco/current_rudder_force`, then copies/updates durable docs artifacts.

Also verify:

```bash
git status --short
```

Only intended #2760 files may remain modified/untracked for commit.

## Code-stage adversarial review requirement

Before closeout, run an adversarial review of the final diff. Preferred: Claude + Codex + Gemini if available. If provider outages/quotas block full 3-agent review, document the fallback honestly and at minimum perform one independent read-only review plus your own adversarial review.

Do not close #2760 if the final review has unresolved MAJOR findings.

## Commit/push/issue closeout rules

If and only if all tests, artifact checks, and code review pass:

1. Stage only intended #2760 paths with explicit `git add <path> ...`.
2. Commit in `digitalmodel` with a message like:
   - `feat: complete SIROCCO current rudder force report`
3. Push `digitalmodel` main/active branch.
4. If ACMA output files under `workspace-hub/acma-projects/B1528/output` changed, commit/push only those output files in `workspace-hub` separately.
5. Post a final comment on https://github.com/vamseeachanta/workspace-hub/issues/2760 with:
   - changed files,
   - output artifact paths/links,
   - test commands/results,
   - review verdicts/artifacts,
   - commits pushed,
   - limitations/source/citation summary.
6. Also comment on parent #2642 linking the revised deliverables.
7. Close #2760 only if no blocker remains and pushed commits/artifacts are verified.

If anything blocks closeout, do not close. Post a concise blocker comment or leave a handoff artifact with exact next action.

## Final response format

Return exactly:

1. **Status:** complete / blocked / partial.
2. **Files changed:** exact paths.
3. **Artifacts generated:** exact paths.
4. **Tests run:** commands and pass/fail.
5. **Review:** verdicts and any residual findings.
6. **GitHub updates:** comments/labels/closeout performed, with URLs if available.
7. **Residual blockers:** exact blocker or `None`.
