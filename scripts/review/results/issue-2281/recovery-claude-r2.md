## PLAN: needs minor revision

Pinning one revision in `main()`, refusing unpinned direct calls when implementation code is already loaded, rejecting hidden index flags, and comparing imported bytes with `HEAD` together close B1–B4 for a fresh, non-malicious CLI build. One gap remains in the plan itself. "Pin the whole build revision" needs a final guard after output is written. At the moment every guard runs before output, so imports and resource reads that happen while reports are written are never re-checked for the last case.

## CODE: changes requested (2 blocking defects, 1 check you must run)

### B5 (blocking): no guard after the last case's output
In `build_case`, the last `validate_import_origins` and `checkout_revision` run before `write_report` and `render_review`. Two things happen after that point and are never checked:
- **Lazy imports during output.** Modules that `digitalmodel.reporting.engine` or `cp_review_document` import inside their functions while rendering are not checked before output.
- **Non-Python files read during output.** Templates and `cp_review_ui.js` are read at render time, after the last clean-checkout check.

For S01–S10, the next case's opening guard catches the problem, so `main()` raises and no index is written. That fails closed, though with a partial candidate left behind. For S11 nothing runs afterwards, so `index.html` and `coverage.json` are produced. This is exactly the Windows editable-install shadowing case the guide describes.

**Fix:**
1. After `verify_pack(folder)` in `build_case`, call `validate_import_origins(ROOT)` again and compare `checkout_revision(ROOT)` with the pin.
2. Run the same pair in `main()` after `write_index` and before `print`.
3. Optionally, add the reporting engine and renderer submodules to `prepare_execution_source`.
4. Add a test where a stubbed `write_report` imports a module from outside the checkout and the build is rejected.

### B6 (blocking): the guide still overclaims (left over from B3)
- "All imported module origins … are checked" is wrong. `validate_import_origins` only checks names under `digitalmodel.*`, `cp_portfolio_*` and `cp_review_document`. Other modules are not checked: other top-level packages under `src`, other helpers in `scripts/reporting`, and all third-party packages.
- "Before report-file output" is untrue for imports made during output (see B5).
- **Suggested wording:** "Imported `digitalmodel` and CP reporting-helper modules are checked against the committed source before and after report output. Non-Python resources are covered only by the clean-checkout check." Or check every module whose file lies under `src/` or `scripts/reporting/`, whatever its name, and keep the broader claim.

### V1 (must verify): a real fresh CLI run may refuse to start
Every test that reaches `main()` past the dirty-checkout check stubs `implementation_loaded` to return `False`. Since you did no engine runs, nothing shows that a real fresh process passes this check. If `cp_portfolio_contract`, imported at the builder's top level, imports `digitalmodel` at all, every CLI build refuses.

You can check this without running any engine: in a fresh process, put `src` and `scripts/reporting` on the path, import the builder, and assert `implementation_loaded()` is `False`. Better still, add that as a subprocess test.

### Non-blocking
- **N1:** `build_case(..., source_revision=...)` accepts any string the caller passes, so a pinned direct call skips the preloaded-implementation refusal. That is out of the stated CLI scope, but say so in the docstring or make the pin private (for example, a token only `main()` can create).
- **N2:** The "live reload" JavaScript test only asserts `comments.length == 1`. It should also assert that `prior_rounds` and `decision_conflicts` don't grow when the same file is loaded twice; that is the de-duplication the comment refers to. Note too that the reload now starts from a fresh UI instance, not the session that saved the file. That is acceptable, but it is a weaker test than before.
- **N3:** The guide shows `ΓÇö` in the benchmark rows. If the file really contains the bytes `CE 93 C3 87 C3 B6` and this isn't just console display, the published guide is visibly corrupted and should get a proper em dash.
- **N4:** Missing tests:
  - hidden index flags under `scripts/reporting`
  - the in-case "before execution" and "during execution" revision checks
  - the real `implementation_loaded` refusal in `main()`
- **N5:** Gitignored non-Python resources under `src` pass the clean-status check. That's minor, and you could mention it alongside the guide's existing limits.

### Cleared
- The B1 ordering in `main()` (capture `HEAD`, refuse preloaded code, preload, compare again) is correct.
- Each case compares against the pinned revision.
- Hidden-flag parsing correctly catches both lowercase (assume-unchanged) and `S` (skip-worktree) entries.
- Comparing with `hash-object --path` applies Git's filters, as intended.
- Untracked or staged-only modules fail closed.
- Deep-cloning the archived rounds in `merge` removes the aliasing behind the archive mutation.

**Release gate:** fix B5 and B6 and run the V1 check, then a further re-review can approve. The published130 receipts and the old embedded UI are unaffected by any of this.
