# Plan: digitalmodel #554 — Catenary solver bracketing + sinh overflow (cluster fix R2)

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/554
**Status:** plan-review
**Tier:** T2 (bug fix with multiple sites in solver + adapter, ~21 sibling tests resolved)

## Root cause (verified at digitalmodel HEAD `08e9c333`)
- `src/digitalmodel/marine_ops/marine_analysis/catenary/solver.py:260` calls `brentq(...)` without a sign-change guard on `f(bracket_low)`/`f(bracket_high)` — raises `ValueError: f(a) and f(b) must have different signs` for marginal geometry.
- `src/digitalmodel/marine_ops/marine_analysis/catenary/solver.py:153-154, 250, 285, 305` evaluate `np.sinh(x/a)` where `a = H/w` becomes very small (shallow lines / low horizontal tension), causing `RuntimeWarning: overflow encountered in sinh` and Newton fallback non-convergence (e.g., `H_solution = 793941.6` after 100 iterations).
- `src/digitalmodel/marine_ops/marine_analysis/catenary/adapter.py:153-157` rejects valid force-method inputs with `ValueError: F/w (20.00) must be > d (100.00)` — over-strict precondition vs. what `TestCatenaryEquationForceMethod` exercises.
- Two test files (`tests/marine_ops/marine_engineering/test_mooring_catenary.py` and `tests/marine_ops/marine_engineering/legacy/test_mooring_catenary.py`) inline a private `CatenarySolver` carrying the same buggy math — duplicated, not regressed.

## Cluster impact
This single fix clears ~21 of the 77 marine_ops failures across:
- `legacy/test_mooring_catenary.py::TestCatenarySolver::*` (8) + `TestCatenaryPerformance::*` (2)
- `test_mooring_catenary.py::TestCatenarySolver::*` (8) + `TestCatenaryPerformance::*` (2)
- `test_catenary_adapter.py::TestCatenaryEquationForceMethod::*` (4)
- `test_marine_eng_perf.py::TestCatenaryPerformance::*` (3)
- `catenary/test_simplified.py` (3) + `test_catenary_perf.py` (2) + `test_catenary_integration.py` (2)

**Implementation order — do this BEFORE single-test sibling fixes:** This cluster fix is upstream of any test that exercises `CatenarySolver` or `CatenaryAdapter`. Do NOT cherry-pick #555 or any R6 sibling that touches the catenary path until #554 lands.

## Plan
### Task 1 — Reproduce
```
uv run pytest tests/marine_ops/marine_engineering/legacy/test_mooring_catenary.py::TestCatenarySolver::test_excel_reference_case_1 -xvs
uv run pytest tests/marine_ops/marine_engineering/test_catenary_adapter.py::TestCatenaryEquationForceMethod -xvs
```

### Task 2 — Fix (3 sites)

**Site A — `solver.py:260` bracketing guard:**
Before calling `brentq(f, low, high)`, validate `f(low) * f(high) < 0`. If not, expand the bracket geometrically (e.g., halve `low`, double `high`) up to a max of 6 iterations. If still no sign change, raise a domain error with the input parameters in the message rather than a generic brentq failure.

**Site B — `solver.py:153, 250, 285, 305` sinh overflow:**
Replace direct `np.sinh(x/a)` with a guarded path:
- For `x/a > 700` (overflow threshold of `np.sinh`): use the asymptotic `0.5 * np.exp(x/a)` (or fall back to a deep-water catenary approximation).
- Alternatively: switch the Newton iteration's parametrization to operate on `log(a)` so small-`a` doesn't blow up the argument.

**Site C — `adapter.py:153-157` precondition:**
Re-derive the force-method validity condition. The current `F/w > d` rejects a regime that `TestCatenaryEquationForceMethod` treats as physical. Either:
- (a) Loosen to `F/w >= d_min(geom)` where `d_min` is computed from the geometry, OR
- (b) Remove the precondition and let the solver's domain validation (Site A) catch genuinely impossible inputs.

Add a regression-test fixture inside the four test files exercising the previously-non-converging input (e.g., `H=793941.6` regime from `test_excel_reference_case_1`) so this divergence-point is locked in.

### Task 3 — Verify
```
uv run pytest tests/marine_ops/marine_engineering/legacy/test_mooring_catenary.py tests/marine_ops/marine_engineering/test_mooring_catenary.py tests/marine_ops/marine_engineering/test_catenary_adapter.py tests/marine_ops/marine_engineering/test_marine_eng_perf.py tests/marine_ops/marine_engineering/catenary/ -x
```

### Task 4 — Confirm no regressions
```
uv run pytest tests/marine_ops/ -x
```
Currently-passing `test_catenary_integration.py` tests (11 green) must stay green.

## Acceptance Criteria
- All ~21 listed catenary tests pass at digitalmodel HEAD.
- `test_catenary_integration.py`'s 11 currently-passing tests remain green.
- A regression-test fixture exercising the previously-non-converging bracket case is added.
- One PR. Future-tense PR body.

## Open questions
- Test-file dedup decision (`tests/marine_ops/marine_engineering/test_mooring_catenary.py` vs `legacy/`): per user guidance, defer to fix-time judgment. If the source-side fix folds the inlined solver math, dedup the test files in the same PR; otherwise leave separate and open a follow-up.
