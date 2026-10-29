# Decline-rate NPV sensitivity

## Plan-lite

**Objective:** evaluate NPV sensitivity to exponential, hyperbolic and harmonic
production decline through the existing economics evaluator, as scoped in #1777.

**Bounded scope:** `DeclineSensitivityResult`, `decline_sensitivity()`, package
exports and synthetic regression tests. Each evaluation uses an independent input
copy. The base evaluation retains the original decline curve and discount rate;
discount overrides apply only to sweep points. Dependency versions, fiscal
policies and decline equations remain those of the existing evaluator.

**Acceptance checks:** run `tests/field_development/test_decline_sensitivity.py`
and `tests/field_development/test_economics.py`. Cover independent cashflow
expectations, input preservation, supported models, sorted duplicate rates, empty
grids, zero decline, invalid inputs and the first zero crossing. Run repository
structure, whitespace, syntax, file hygiene, engineering-register, public
identifier and secret checks. Inspect hosted failures and distinguish failures
caused by this change from failures already present on main.

**Known limits:** breakeven uses the first crossing in ascending grid order,
with linear interpolation and no extrapolation. The optional `worldenergydata`
DCF integration is unavailable in standalone acceptance; its 28 existing skips
leave that integration unverified. Synthetic cashflows exercise the NumPy
fallback and do not establish field estimates or fiscal-policy validity.
