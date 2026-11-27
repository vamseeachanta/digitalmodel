# Plan: digitalmodel #284 — WRK-133 Update OrcaFlex license agreement

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/284
**Status:** plan-review
**Tier:** T1 (document-only edit, no source/code changes)

## Context

Issue [284](https://github.com/vamseeachanta/digitalmodel/issues/284) (WRK-133) covers the draft OrcaFlex sublicensing agreement. The draft needs both parties’ street addresses, a clause-by-clause check against the governing PDF, and a definition of “third party” that specifies who is covered. The issue’s “Reference Documents” section identifies the governing PDF. The agreement repository holds the Word and Markdown drafts, update notes, and drafting prompt under `admin/orcaflex/`.

**Stale-flag:** This issue is misfiled in `digitalmodel`. No deliverable in the acceptance criteria modifies code or docs in the digitalmodel tree — every output lands in `aceengineer-admin/admin/orcaflex/`. Recommend transferring the issue to `vamseeachanta/aceengineer-admin` (or whichever repo hosts the contracts surface) before execution. The plan below assumes the executor will work in `aceengineer-admin` regardless of where the issue is finally hosted.

## Plan

### Task 1 — Locate and read both source docs
In `aceengineer-admin/admin/orcaflex/`, open `client-4e09-aceengineer-orcaflex.md` (current draft, plain-text editable) and `client-4e09-OrcaFlex-agreement.pdf` (upstream binding terms). Read `agreement-update-notes.md` and `user_prompt.md` for prior change requests already captured.

### Task 2 — Insert both party addresses
Enter the street addresses of the sublicensor and sublicensee in the parties block at the top of the Markdown draft. Match both legal names to the governing PDF and preserve the formatting of unrelated paragraphs.

### Task 3 — Compliance pass against the upstream client-4e09 PDF
Walk every numbered clause of `client-4e09-OrcaFlex-agreement.pdf` and confirm the sublicense draft does not grant rights client-4e09 does not hold. Capture each check in `agreement-update-notes.md` as a bulleted ledger (clause-N → status: complies / gap / TBC). Flag — do not silently fix — any clause where the sublicense exceeds upstream scope.

### Task 4 — Clarify the "3rd party" definition
Add a definition block to the sublicense draft naming who counts as a 3rd party (subcontractors of Achanta Aceengineer? client engineers using output? auditors?). Add a usage clause stating which uses are permitted (read-only audit access vs. operational use vs. sublicensing). Cross-check that the new clauses do not contradict client-4e09's upstream prohibition on sub-sublicensing.

### Task 5 — Regenerate `.docx` and stage for human review
Re-export `client-4e09-aceengineer-orcaflex.md` to `client-4e09-aceengineer-orcaflex.docx` via the project's existing pandoc or Word workflow (record the command used in `agreement-update-notes.md`). Do **not** sign, send, or commit this to a public repo — it is a draft for legal review.

## Acceptance Criteria

- [ ] client-4e09 address (`<client address>`) appears in the parties block.
- [ ] Achanta Aceengineer Inc. address (11511 Piping Rock Dr., Houston, TX 77077) appears in the parties block.
- [ ] `agreement-update-notes.md` contains a clause-by-clause compliance ledger covering every numbered clause of `client-4e09-OrcaFlex-agreement.pdf`, with status entries.
- [ ] A "3rd Party" definition and usage clause is present in the draft, scoped consistently with upstream client-4e09 terms.
- [ ] `client-4e09-aceengineer-orcaflex.docx` regenerated from the markdown source; both committed in the contracts repo.
- [ ] No commits land in `digitalmodel` under this plan.

## Open questions

- **Repo location:** confirm with the issue owner whether to transfer the issue to `aceengineer-admin` and close `digitalmodel#284` with a redirect, or keep the tracking issue here and work in the other repo.
- **Legal review gate:** does this draft go to outside counsel before the human signature stage, or does the in-house workflow stop at this plan's deliverables?
- **Scope of "3rd party":** the issue does not name candidate parties — flag for owner to enumerate (subcontractors / client engineers / auditors) before the executor commits to definition wording.
