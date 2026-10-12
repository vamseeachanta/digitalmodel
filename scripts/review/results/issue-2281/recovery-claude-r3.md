# Final bounded re-review: CP-2281 provenance

I reviewed only the source and diffs you pasted. I ran no tools or tests, so the red-then-green history of the new regressions is your claim, not something I checked.

## PLAN verdict: **APPROVE**

- **Scope:** provenance covers repository source in a fresh, non-malicious CLI process. Environment injection, ignored non-Python resources and the wiki revision are named as out of scope. The explicit `source_revision` pin is reserved for the internal CLI loop.
- **Released artifacts:** the historical baseline `bf638ed1…` stays attached to the old released packs. New builds record the clean commit they actually ran from. Nothing is certified, no private data or calculation output is generated, and no benchmark is counted as complete.
- **Benchmarks:** the guide's A/C/E wording matches the stated limits. A's correction is pending, E's infill is unresolved and acceptance was not evaluated, and B/D/C/E remain open.

## CODE verdict: **APPROVE**, no blockers

**Resolved findings, checked in the source**
- **R1 B1–B4 (each one has a test):**
  - A dirty checkout (unstaged, staged or untracked) is rejected before any output, both in `build_case` and in the CLI.
  - A non-Git or nested root is rejected.
  - `assume-unchanged` and `skip-worktree` flags are rejected, and committed bytes are compared with the file actually imported.
  - Mixing revisions across cases is rejected, and so is a new commit made after imports are loaded.
- **B5:** `build_case` re-checks import origins and the pinned revision after `verify_pack`. Imports that happen lazily during render or write are now covered before the case returns.
- **B6:** `main` re-checks both after `write_index` and before printing. The S11 foreign-import test proves the error is raised and stdout stays empty.
- **Fresh-process test:** it uses the real builder in a `-B` subprocess with `src` first on `sys.path`. It proves `implementation_loaded()` is false after the builder's top-level `cp_portfolio_contract` import, so the CLI cannot reject itself. It then loads the real routes and checks them without running solvers.
- **Lazy offshore route:** the test shows it is loaded and rejected before the destination is created.
- **JS merge:** deep-cloning the archived rounds fixes the alias bug. The test proves an archived pending round stays pending after a later accept/reject and after a local edit.

**Advisories (none block merge)**
1. **A failed final guard leaves a publishable candidate.** When `main`'s final guard fails, `coverage.json`, `index.html` and verified pack receipts are already on disk. `validate_candidate` would accept them, because it checks neither a success marker nor the revision. Today only the release register and agent review stand in the way. Cheap fixes: write a `build-complete.json` marker last and require it in `validate_candidate`, or build in a temporary directory and rename it on success. A failure inside `build_case` is already safe, since there is no `coverage.json` yet.
2. **`validate_candidate` doesn't check revision consistency.** It doesn't confirm that every row has the same `source_revision`. Worth adding next to item 1.
3. **Guide wording overstates two things:**
   - "Direct calls with preloaded implementation code are rejected" holds only when `source_revision` is omitted. Add "without an explicit revision pin."
   - "Tracked non-Python resources are covered by … hidden-index-flag checks": the flag check only looks at `src` and `scripts/reporting`. Fixtures are protected separately by their sha256 in `cp_portfolio_inputs.json`. Narrow the sentence to match.
4. **Stale or tampered `__pycache__` bytecode.** Python can run cached `.pyc` files whose header matches the source while the source bytes are what get validated. This falls under the excluded "ignored resources / environment" class. Consider naming it explicitly in the guide.
5. **The builder module isn't checked by origin.** `build_cp_review_portfolio` itself is never origin-validated; it relies on the clean-checkout check plus `ROOT` being derived from its own path. That is sound for the CLI, and worth one sentence in the guide.
6. **The fresh-process test depends on the working tree.** It fails whenever an imported `src/digitalmodel` file has uncommitted edits. That is the intended fail-closed behaviour, but it can confuse local runs. A skip-or-explain message would help.
7. **Possible encoding damage in the guide.** The `ΓÇö` in the benchmark rows is in the context lines, so it predates this change. Check that the file is really UTF-8 em-dashes and not baked-in mojibake (UTF-8 text decoded with the wrong code page), so it doesn't show up in the published guide.

**Net:** all six earlier blockers (R1 B1–B4, R2 B5–B6) are resolved and I found no new ones. Item 1 is the only advisory I'd fix before the next publication run.
