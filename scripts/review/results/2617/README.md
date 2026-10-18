# Issue 2617 verification and review receipt

Date: 2026-10-16. Destination: public `vamseeachanta/digitalmodel`, one draft PR.
Authority: the originating issue-specific user request, including draft-PR publication.

The plan was reviewed before the production edit by Claude and main-session Codex (MINOR); the independently dispatched Agy response also returned MINOR. The revised plan and final code were reviewed together: Claude MINOR, Agy APPROVE, main-session Codex MINOR. No MAJOR finding remains. The final packet manifest was verified unchanged before accepting the verdicts; the same verification is repeated against committed bytes before publication. Historical plan-r1 hashes intentionally describe the pre-change implementation, not the current source.

Plan MINOR dispositions: the exact sparse equality tolerance, mean-force normalization check, positive-std fixture, oscillator coefficients/sampling/transient removal, and behavior-change notes are implemented. The plan and report identify the unsupported input classes rather than introducing a new validation policy in this summation correction.

Code MINOR dispositions:
- Hermitian matrices and uniform integration grids are explicit input prerequisites. Triangular-only, malformed and non-Hermitian validation is unchanged and outside the documented case. No validity claim is made for those inputs.
- The oscillator acceptance is explicitly bichromatic: one resonant beat. Four phase records verify complete-cycle phase invariance; they are not presented as a broadband statistical qualification. The narrow-band force ensemble separately exercises many pairs and shared difference-frequency bins.
- The reference is fixed-amplitude/random-phase, as specified in the full-QTF docstring, plan and report. Random Rayleigh-amplitude/DC ensemble variance is not claimed.
- Newman/full equality is asserted only on matching integration grids. Spectrum interpolation retains existing behavior. External callers are not claimed to be inventoried.
- `pytest.ini` registers `slow` and does not deselect it. The complete OrcaWave invocation collected and passed 357 tests including the Monte Carlo test. `orcawave-green.txt` and `tdd-red.txt` preserve actual runner evidence.
- Repository text search plus local AST readback finds one old and five new direct call sites; `src/digitalmodel/orcawave/__init__.py` re-exports the function at its import and `__all__`. The source SHA in the metrics record was recomputed from `git show c5e4ac33:src/digitalmodel/orcawave/drift_forces.py`.
- The mean-force convention test has zero endpoint spectral values to remove trapezoid/rectangle quadrature differences; it tests normalization, not a change to mean-drift quadrature.

Reproduction: baseline-existing-tests.txt records 38 passes; tdd-red.txt records 7 failures and 1 pass against the old function. The new regression file subsequently adds two signed/varying-Newman parameter cases. The final domain run records 357 passes. The initial broader collection needed the existing `loguru` dependency added to the isolated runner environment; no repository dependency metadata was changed.

Outgoing bundle review: code/tests, synthetic numeric metrics, HTML plan/report, and these frozen review records. No client data, licensed originals or credentials are included. Independent detect-secrets scanning found 17 hex-entropy candidates: SHA-256 source/review digests and the 40-hex baseline Git revision in two manifests. Those values were checked against their named source bytes or Git revision and are not credentials. No retired identifier scanner was invoked. Provider availability and the successful stdin fallback are recorded separately.

Retained artifacts: compact metrics, generation code and review/test evidence in Git. Time histories and full QTF arrays are generated in memory. The issue worktree and job-owned scratch evidence remain expected residue for draft-PR follow-up; no merge, deployment, force-push or hook bypass is performed.

Pre-completion cleanup audit: CLEAN — canonical digitalmodel and hub checkouts unchanged, no stash, task-local partial/tmp files, session-doc drift or cleanup lock; the existing cleanup-trash directory is empty. EXPECTED — issue worktree, job-owned evidence, launcher logs and runtime-managed temporary mounts. UNEXPECTED — none attributable to this task. The task branch is retained for the draft PR.

Formatting check: production code, tests, plan and report pass `git diff --check`. The immutable packet builder emits a space after the line-number separator on blank lines; the raw pytest failure transcript also contains trailing spaces. Those transport/log bytes are retained exactly, so an unfiltered whitespace check reports those evidence files. They are not modified after review to manufacture a different packet digest.
