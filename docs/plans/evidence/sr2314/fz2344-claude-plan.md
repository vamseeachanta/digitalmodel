VERDICT: MAJOR

The packet contains only the plan. No source, test, or diff is included, so every claim about "main" behavior is unverifiable from the frozen bytes. Defects below are cited as `fz2344-plan.md:line`.

1. **`cite` parameter validation is not specified (line 3).**
   - `cite="strict"` is a truthy string. If the existing code uses `if cite:`, any other string (`"Strict"`, `"false"`, `"off"`) silently enables the non-strict path. A typo for strict would then silently get lenient behavior.
   - The plan names no allowed-value set (`False`/`True`/`"strict"`) and no `ValueError` for anything else.
   - No test covers invalid values.
   - This is the central opt-in contract, and it is undefined.

2. **Main-compatible defaults are asserted, not tested (lines 3–4).**
   - The tests listed are behavioral: transfer directions, warning cardinality, strict cases. None is a golden parity check that `cite=True` and `cite=False` outputs match current `origin/main`.
   - The compared outputs would be numeric results, the sidecar structure, the warning text, and the unresolved-ITTC metadata fields.
   - "Retain unresolved ITTC metadata in default mode even when a fixture has both pages" is stated as intent only. The plan does not say that main actually emits this exact metadata.
   - Line 4 plans no merge of main unless GitHub reports a conflict. A textually clean but stale base can still drift semantically. The plan should require a comparison against the current main tip.

3. **Strict-mode semantics are underspecified (line 3).**
   - Not stated: does strict add or replace behavior for EN400? In particular, does strict still raise on EN400 configured-missing, as main does?
   - Not stated: does strict emit the standalone wrapper warning, and how many times?
   - Not stated: what is the sidecar shape in strict mode?
   - The tests cover "strict resolution, missing pages, revision mismatch" but not strict combined with an EN400 error, and not strict in standalone mode.
   - "Strict mode will not change numeric results" has no assertion.

4. **Warning-cardinality test isolation is missing (lines 3–4).**
   - Reusing `ittc57_cf_cited` means the warning depends on module or global state. If it is once-only or cached, tests that count warnings will be order-dependent unless the state is reset.
   - The plan names no reset or isolation mechanism.

5. **Owner constraints are not enforced by any check.**
   - "No registry or wiki will change" is a sentence, not a gate. There is no path-allowlist assertion, such as a `git diff --name-only` check against the four files plus evidence.
   - "Draft preserved" appears nowhere in the plan. There is no step to keep the PR in draft and no prohibition on marking it ready. Line 2 only says the coordinator owns readiness.

6. **Scope is unverifiable (line 4).**
   - "Four source/test files plus evidence" does not name the files.
   - Docs or changelog for the new public `cite="strict"` option are neither included nor explicitly excluded.

7. **The review disposition is self-authored (line 4).**
   - "Plan adversarial disposition (Codex inline)" is the author's own note, not an independent review artifact.
   - It must not stand in for the review gate, and it carries no verdict or hash.

Required before APPROVE:
- Name the four files.
- Define the exact `cite` domain and its validation.
- Specify strict-mode behavior for EN400 errors, the warning, and the sidecar.
- Add a parity test against the main-tip outputs and an invalid-`cite` test.
- Add a warning-state reset.
- Add a diff path-allowlist check and an explicit draft-preservation step.
