# Plan: digitalmodel #284 — WRK-133 Update OrcaFlex license agreement

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/284
**Status:** plan-review
**Tier:** T1 (document-only edit, no source/code changes)

## Context

Issue #284 (WRK-133) is a contractual document update: the draft OrcaFlex sublicensing agreement between ACMA and Achanta Aceengineer Inc. needs (1) both party street addresses inserted, (2) a clause-by-clause compliance pass against the upstream `ACMA-OrcaFlex-agreement.pdf`, and (3) explicit definition + scoping of the "3rd party" term. The reference materials live in **another repo**: `aceengineer-admin/admin/orcaflex/` already contains `acma-aceengineer-orcaflex.docx`, `acma-aceengineer-orcaflex.md`, `agreement-update-notes.md`, and `user_prompt.md` (verified via `ls /mnt/local-analysis/workspace-hub/aceengineer-admin/admin/orcaflex/`). The upstream `ACMA-OrcaFlex-agreement.pdf` is the binding instrument cited in the issue's "Reference Documents" section.

**Stale-flag:** This issue is misfiled in `digitalmodel`. No deliverable in the acceptance criteria modifies code or docs in the digitalmodel tree — every output lands in `aceengineer-admin/admin/orcaflex/`. Recommend transferring the issue to `vamseeachanta/aceengineer-admin` (or whichever repo hosts the contracts surface) before execution. The plan below assumes the executor will work in `aceengineer-admin` regardless of where the issue is finally hosted.

## Plan

### Task 1 — Locate and read both source docs
In `aceengineer-admin/admin/orcaflex/`, open `acma-aceengineer-orcaflex.md` (current draft, plain-text editable) and `ACMA-OrcaFlex-agreement.pdf` (upstream binding terms). Read `agreement-update-notes.md` and `user_prompt.md` for prior change requests already captured.

### Task 2 — Insert both party addresses
Edit the parties block at the top of `acma-aceengineer-orcaflex.md` to insert ACMA at `565 S. Mason Road #395, Katy, Texas 77450` and Achanta Aceengineer Inc. at `11511 Piping Rock Dr., Houston, TX 77077`. Keep entity-name spellings exactly as they appear in the upstream PDF; do not re-format or re-wrap unrelated paragraphs.

### Task 3 — Compliance pass against the upstream ACMA PDF
Walk every numbered clause of `ACMA-OrcaFlex-agreement.pdf` and confirm the sublicense draft does not grant rights ACMA does not hold. Capture each check in `agreement-update-notes.md` as a bulleted ledger (clause-N → status: complies / gap / TBC). Flag — do not silently fix — any clause where the sublicense exceeds upstream scope.

### Task 4 — Clarify the "3rd party" definition
Add a definition block to the sublicense draft naming who counts as a 3rd party (subcontractors of Achanta Aceengineer? client engineers using output? auditors?). Add a usage clause stating which uses are permitted (read-only audit access vs. operational use vs. sublicensing). Cross-check that the new clauses do not contradict ACMA's upstream prohibition on sub-sublicensing.

### Task 5 — Regenerate `.docx` and stage for human review
Re-export `acma-aceengineer-orcaflex.md` to `acma-aceengineer-orcaflex.docx` via the project's existing pandoc or Word workflow (record the command used in `agreement-update-notes.md`). Do **not** sign, send, or commit this to a public repo — it is a draft for legal review.

## Acceptance Criteria

- [ ] ACMA address (565 S. Mason Road #395, Katy, TX 77450) appears in the parties block.
- [ ] Achanta Aceengineer Inc. address (11511 Piping Rock Dr., Houston, TX 77077) appears in the parties block.
- [ ] `agreement-update-notes.md` contains a clause-by-clause compliance ledger covering every numbered clause of `ACMA-OrcaFlex-agreement.pdf`, with status entries.
- [ ] A "3rd Party" definition and usage clause is present in the draft, scoped consistently with upstream ACMA terms.
- [ ] `acma-aceengineer-orcaflex.docx` regenerated from the markdown source; both committed in the contracts repo.
- [ ] No commits land in `digitalmodel` under this plan.

## Open questions

- **Repo location:** confirm with the issue owner whether to transfer the issue to `aceengineer-admin` and close `digitalmodel#284` with a redirect, or keep the tracking issue here and work in the other repo.
- **Legal review gate:** does this draft go to outside counsel before the human signature stage, or does the in-house workflow stop at this plan's deliverables?
- **Scope of "3rd party":** the issue does not name candidate parties — flag for owner to enumerate (subcontractors / client engineers / auditors) before the executor commits to definition wording.
