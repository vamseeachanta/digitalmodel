# Issue 2082 — Claude Haiku adversarial plan review

Explicit routed fallback: --model haiku. Existing CLI authentication; no billing/settings change.
CLI version: 2.1.267. Deadline: 240 seconds. Tools disabled.
Exit: 0
Elapsed seconds: 103.65
Timed out: False
Private temporary directory removed: True

## Actual stdout

# ADVERSARIAL PLAN REVIEW — Issue 2082: Trustworthy OrcaFlex Smoke Proof

**VERDICT: MAJOR** — Load-bearing gaps between plan design and current evidence. Do not approve pending resolution.

---

## Concrete Blockers

### 1. **Simulation Reload Proof Missing from Fake-API Evidence**
**Plan §Proposed flow & §Regression**: Claims the probe will "fresh model(threadCount=1) → LoadSimulation → find expected line → validate restored history and sample count" and lists "Reader lacks expected line, incomplete state, nonfinite values or different sample count → ok=false" as a required regression outcome.

**Evidence gap**: All 8 fake-API scenarios show `sim_reload_attempted: false`. The `finite_control` scenario—which should demonstrate successful save/reload—does NOT show a reload phase. This is the core proof requirement.

**Fix required**: Either:
- Update fake-API `finite_control` scenario to show `sim_reload_attempted: true` with reloaded sample count and validation results, OR
- Clarify in plan that saved-simulation round-trip is deferred to a follow-on issue and remove it from the regression table.

---

### 2. **threadCount Parameter Undocumented and Not Shown in Current Source**
**Plan §Bounded design**: "Every OrcaFlex model constructed by the probe...will receive threadCount=1. The success report will record the observed count; a mismatched count will fail."

**Current source line 60**: `model = OrcFxAPI.Model()` — passes NO kwargs.

**Fake-API evidence**: All `constructor_calls` entries show `kwargs: {}`.

**Risk**: If `threadCount=1` is not a valid parameter for the OrcaFlex Model constructor in the versions in scope, the probe will fail immediately on model construction, before any of the validation logic runs.

**Fix required**:
- Cite the OrcaFlex API version and documentation showing `Model(threadCount=int)` is valid, OR
- Revise the design to extract/validate threadCount via a post-construction property (e.g., `model.ThreadCount` read after construction), OR
- Defer threadCount validation to a separate issue and remove it from this plan's scope.

---

### 3. **LoadSimulation API Not Specified**
**Plan §Proposed flow**: "fresh model(threadCount=1) → LoadSimulation → find expected line → require complete/stopped → validate restored history and sample count."

**Issue**: The plan does NOT specify:
- Is `LoadSimulation(path)` a method call on an existing model, or does the constructor accept a path argument (e.g., `Model(sim_path)`)?
- How is "the expected line" located by name? Is it still named "SmokeTestLine"?
- Does LoadSimulation raise an exception if the .sim file is corrupt, or does it return a partially-loaded state?

**Fix required**: Cite the OrcFxAPI documentation for LoadSimulation or document the exact call pattern in the plan.

---

### 4. **Fake-API Coverage Gaps vs. Regression Test List**
**Plan §Regression** lists these as required failure modes:
- "Empty/singleton/mixed-nonfinite dynamic history → ok=false"
- "Reader lacks expected line, incomplete state, nonfinite values or different sample count → ok=false"
- "Missing/empty/corrupt simulation; loader exception → ok=false"

**Fake-API scenarios provided**: `finite_control`, `static_nan`, `static_inf`, `dynamic_nan`, `dynamic_inf`, `single_sample`, `wrong_state`, `corrupt_sim`.

**Missing coverage**:
- Empty dynamic history (0 samples) — not tested.
- Loader exception (e.g., corrupt .sim file read via LoadSimulation) — `corrupt_sim` exists but doesn't show a reload attempt.
- Missing expected line on reload — not tested.
- Different sample counts after reload — not tested.
- Model import failure (e.g., OrcFxAPI import raises) — not shown in fake-API structure.

**Fix required**: Extend fake-API with test scenarios for each missing case, or revise the regression list to match what is tested offline.

---

## Significant Issues

### 5. **Proof Field Names Not Specified**
**Plan §Proposed flow & §Regression**: Lists proof requirements (finite, completion, thread, saved-simulation) but does NOT name the dict keys that will appear in the report.

Current source report keys: `solver`, `ok`, `module`, `dll_version`, `statics_state`, `static_tension_kN`, `simulation_state`, `dynamic_samples`, `dynamic_tension_kN`, `sim_bytes`, `elapsed_s`, `error`, `stage`, `traceback`.

**Plan should add new keys like**:
- `threadCount_requested` / `threadCount_observed` (or similar)
- `static_finite` (boolean: static tension passed finiteness check)
- `dynamic_finite` (boolean: all dynamic samples are finite)
- `simulation_complete` (boolean: state was SimulationStopped)
- `saved_sim_bytes_valid` (boolean: > 0 and file exists)
- `saved_sim_reloaded` (boolean: LoadSimulation succeeded) — **only if reload is in scope**
- `reloaded_dynamic_samples` (int) — **only if reload is in scope**

**Fix required**: Specify exact field names and whether they are numeric, boolean, or string.

---

### 6. **Regression Claim: "All model-construction paths → Explicit threadCount=1"**
**Plan §Regression**: "All model-construction paths: Explicit threadCount=1, observed count=1, including exception paths."

**Ambiguity**:
- Does "observed count=1" mean a new field `threadCount_observed` will appear in the report for every result (success and failure)?
- If model construction fails before calling `OrcFxAPI.Model()`, how is threadCount validated?
- The plan says "including exception paths" — does this mean if import fails, we still report `threadCount_observed`?

**Fix required**: Clarify whether threadCount validation is part of every result or only success paths. If every path, explain how exception-before-construction cases report the observed count.

---

### 7. **Fake-API `raw_report_is_strict_json` Field**
**Plan §Proposed flow**: "Failure reports will contain no numeric NaN or Infinity. Values will be validated before insertion; error details will remain text. Strict JSON serialization will be a test oracle for the OrcaFlex-only report."

**Fake-API observation**: The field `raw_report_is_strict_json` tracks whether the report dict can be serialized to JSON without error. For scenarios like `static_nan`, `dynamic_nan`, `dynamic_inf`, this is `false`, meaning the unrepaired code produces invalid JSON.

**After repair, this must be `true` for ALL scenarios**, including failures. The plan intends this correctly (validate before insertion), but:
- The plan doesn't specify what happens if a float is very large but finite (e.g., 1e308). Should it be allowed? Rejected as a plausibility check?
- Does the fake-API mock use `math.isnan()` / `math.isinf()` or some other mechanism to detect problematic values?

**Fix required** (minor): Clarify the finiteness check: is it `not (math.isnan(v) or math.isinf(v))`, or does it also reject values above/below a threshold?

---

### 8. **Workflow Behavior with require_all=false Not Clarified**
**Plan §Bounded design**: "Explicit `require_all=false` will remain diagnostic-only and may return normally with `ok=false`; it will not become production acceptance."

**Current workflow code**:
```python
if settings.get("require_all", True) and not report["ok"]:
    raise RuntimeError(...)
return cfg
```

If `require_all=false` and `report["ok"]=false`, the workflow does NOT raise and returns normally.

**Issue**: The plan says "may return normally" (present tense) but this already happens in the current code. Clarify:
- Is this behavior changing (it's not), or is the plan just documenting current behavior?
- Should the plan warn that `require_all=false` is for offline diagnostics only, and relying on it to suppress a failure is unsafe?

---

## Minor Issues

### 9. **Hostname Disclosure "Will Remain Opt-in"**
**Plan §Bounded design**: "Hostname disclosure will remain opt-in."

**Current source**: Does NOT include hostname anywhere. The plan appears defensive but doesn't explain what the risk is or how opt-in works. Skip this or clarify the intent.

---

### 10. **Exception Handling for OrcFxAPI Objects**
**Plan §Proposed flow**: "on any error → release references → return ok=false with safe diagnostics."

**Current source**: Uses try/except but does NOT explicitly `del model` or `del line`. Python GC will release them, but:
- OrcaFlex may hold a licence lock while objects are in scope.
- A context manager or explicit release would be cleaner.
- Plan should specify: will the repair add explicit release, or rely on GC?

---

### 11. **Test Discovery Verification Method Unspecified**
**Plan §Artifact map**: "Test discovery will be verified explicitly because the legacy similarly named smoke tests exercise different code."

**How**: The plan doesn't say how this verification happens. Will there be:
- A manual code review comparing test function names?
- A CI check grepping for conflicting test IDs?
- A documentation note in the test file?

---

### 12. **Native Acceptance Harness Owner Unspecified**
**Plan §Native acceptance**: "A 90-second harness deadline will bound the process; timeout cleanup will target only its owned process tree."

**Question**: Who implements and runs this harness?
- The operator (manual)?
- A CI workflow (automated)?
- A script in the repo (which script)?

The plan says "a manually opted-in local subprocess," suggesting operator-driven, but doesn't name the entry point or script.

---

## Recommendations for Owner Approval

**Before proceeding to implementation**, resolve these MAJOR gaps:

1. **Confirm threadCount parameter**: Cite OrcaFlex API docs showing `Model(threadCount=1)` is valid, OR revise design to use post-construction property inspection.
2. **Clarify LoadSimulation scope**: Either include save/reload in fake-API test evidence, OR move this to a follow-on issue and remove it from regression table and proof fields.
3. **Extend fake-API coverage**: Add test scenarios for empty history, loader exception, missing line, sample-count mismatch, import failure.
4. **Specify proof field names**: Document exact dict keys for all new proof fields.
5. **Verify fake-API test harness**: The fake-API structure is clear, but clarify: will it use monkeypatch at import, a fixture module, or inline mocks in test functions?

Once these are resolved, the bounded design is sound: fail-closed, explicit, JSON-safe, and reversible by commit revert. The plan correctly preserves AQWA behavior and workflow contracts.

---

**⛔ Do not approve pending resolution of items 1–3 above.** Item 4–5 are clarifications that can be resolved during implementation if the owner explicitly signs off on the field-naming process (e.g., "reviewer will approve field names during code review").


## Actual stderr

(empty)
