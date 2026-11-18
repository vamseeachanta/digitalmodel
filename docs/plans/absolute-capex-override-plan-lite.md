# Absolute CAPEX override validation

Selected absolute CAPEX overrides will be checked before they enter concept
screening. This plan records the validation scope and acceptance checks for
`src/digitalmodel/field_development/concept_screening.py` and
`tests/field_development/test_concept_screening.py`.

## Plan-lite

### Invariant and entry points

A selected override will be converted to `float` and accepted only when finite
and strictly positive. NaN, positive infinity, negative infinity, zero and
negative values will raise `ValueError` before rounding.

The invariant will apply through both `capex_line_items()` and
`screen_concepts()`, with maps keyed by either a `HostType` member or its string
value. Validation will remain in `capex_line_items()`; `screen_concepts()` will
use that entry point rather than duplicate the check.

### Test matrix

| Input or behavior | Coverage | Acceptance |
| --- | --- | --- |
| NaN and both infinities | Three values, two key forms, two entry points: 12 cases | `ValueError` naming `capex_override` |
| Finite positive total | Two key forms, both entry points: two cases | Synthetic hand sum of 5.0 + 1.25 = 6.25 USD bn, provenance and deterministic results |
| Zero and negative totals | Existing three cases | `ValueError` |
| Missing or partial override | Existing fallback cases | Generic estimation for hosts without a selected override |

The complete touched-module suite will be run before and after this
documentation change. Acceptance will require all 45 tests to pass, with no
failures or skips. File checks will cover the engineering register, repository
structure, whitespace, encoding, conflict markers, publication restrictions,
ignore status and LFS attributes.

### Retained behavior and limits

Accepted totals will retain the existing four-decimal rounding. An override will
continue to bypass fraction scaling and uplifts. Only the selected host's value
will be validated; unused map entries will remain outside this check. The
parameter defaults and generic estimation path will remain unchanged. The full
repository suite will remain outside the local validation scope; hosted failures
will be assessed separately against main.
