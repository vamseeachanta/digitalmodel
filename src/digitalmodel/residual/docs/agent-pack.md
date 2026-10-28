# Residual module knowledge pack

## Summary

The package fits a bounded affine correction and exposes error metrics on generic
float arrays. This pack describes existing software behavior and synthetic
consistency checks. Forecast validity, physical bounds and operational suitability
are NOT_EVALUATED. The empirical fit is not a standards-derived value.
[src/digitalmodel/residual/model.py:L1-L12]
[src/digitalmodel/residual/skill.py:L1-L7]

## Revision history

| Revision | Date       | What changed and why                                                                                                       |
| -------- | ---------- | -------------------------------------------------------------------------------------------------------------------------- |
| Initial  | 2026-10-28 | Document the residual API and executable metric example so inventory drift and arithmetic inconsistencies can be detected. |

| Header       | Value                                                                                       |
| ------------ | ------------------------------------------------------------------------------------------- |
| Module       | `residual`                                                                                  |
| Repository   | `digitalmodel`                                                                              |
| Package      | `src/digitalmodel/residual/`                                                                |
| Source basis | `09089a121a16069c4b657d1a320a630a23ab1dd5`; implementation files are unchanged by this pack |
| Status       | DRAFT; software consistency only; independent engineering clearance pending                 |

## 1. Purpose and scope

`ResidualModel.fit` estimates scale and bias from measured/predicted pairs;
`apply` returns a `Correction` containing a tracking estimate and status.
The metrics report RMSE, correlation, RMSE reduction and a strict comparison
against a supplied noise floor. No timing, sensor alignment, coordinate transform
or unit conversion is supplied by this package.
[src/digitalmodel/residual/model.py:L93-L165]
[src/digitalmodel/residual/skill.py:L13-L47]

The registry points to the Python package and its residual tests. There is no
package-specific command or configuration router. The demonstrated route is
`from digitalmodel.residual import ...`; direct submodule imports are also used
by the response-correction adapter.
[src/digitalmodel/residual/**init**.py:L20-L42]
[src/digitalmodel/drilling_riser/response_correction.py:L27-L29]

This pack covers only `residual`. The seismic and uncertainty packs in the
[parent issue](https://github.com/vamseeachanta/digitalmodel/issues/3338) remain
follow-up work.

## 2. Inputs and outputs

The following table inventories definitions across every package Python file,
including public class members. The initializer re-exports these definitions;
it has no additional definitions. Generated dataclass methods, imported helpers
and constants do not require duplicate definition rows under the existing AST
checker contract.
[src/digitalmodel/residual/**init**.py:L20-L42]
[scripts/docs/check_agent_pack_functions.py:L1-L10]

| Public symbol              | Inputs and units                                                                                                                             | Output and convention                                                                                                | Evidence                                       |
| -------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------- | ---------------------------------------------- |
| `Correction`               | `value`: caller's channel unit; `status`, `reason`: strings                                                                                  | Frozen dataclass; default status `active`; escalated values may be raw predictions                                   | [src/digitalmodel/residual/model.py:L39-L53]   |
| `Correction.escalated`     | Stored status                                                                                                                                | Boolean; status differs from `active`                                                                                | [src/digitalmodel/residual/model.py:L52-L53]   |
| `ResidualModel`            | Coefficients, bounds, domain, pair count, fit errors and status                                                                              | Frozen dataclass; direct construction does not perform fit gates                                                     | [src/digitalmodel/residual/model.py:L57-L189]  |
| `ResidualModel.active`     | Stored status                                                                                                                                | Boolean; status equals `active`                                                                                      | [src/digitalmodel/residual/model.py:L83-L84]   |
| `ResidualModel.identity`   | Reason string; optional output bounds in channel units                                                                                       | Inactive identity fallback with `escalate:<reason>` status                                                           | [src/digitalmodel/residual/model.py:L88-L90]   |
| `ResidualModel.fit`        | Equal-shaped measured/predicted arrays; output bounds and bias bound in channel units; dimensionless scale bounds; minimum finite pair count | Affine model with training domain and pre/post RMSE, or an escalated fallback; unequal shapes raise `ValueError`     | [src/digitalmodel/residual/model.py:L93-L149]  |
| `ResidualModel.apply`      | Scalar prediction in the training channel unit                                                                                               | `Correction`; active in-domain output is clipped; escalated output is the raw scalar                                 | [src/digitalmodel/residual/model.py:L152-L165] |
| `ResidualModel.provenance` | Model state                                                                                                                                  | Plain dictionary containing empirical markers, coefficients, bounds, domain, fit errors, pair count, status and note | [src/digitalmodel/residual/model.py:L168-L189] |
| `rmse`                     | Equal-shaped float-convertible arrays in the same unit                                                                                       | Float RMSE in that unit; empty arrays give NaN; shapes are checked before subtraction                                | [src/digitalmodel/residual/skill.py:L13-L21]   |
| `correlation`              | Paired float-convertible series                                                                                                              | Dimensionless Pearson coefficient; insufficient or constant series give NaN; no explicit equal-shape guard           | [src/digitalmodel/residual/skill.py:L24-L30]   |
| `skill_gain`               | RMSE before and after in the same unit                                                                                                       | Float `before - after`; positive means error reduction                                                               | [src/digitalmodel/residual/skill.py:L33-L35]   |
| `is_significant`           | RMSE before/after and noise sigma in the same unit                                                                                           | Boolean; gain must strictly exceed noise; invalid noise returns `False`                                              | [src/digitalmodel/residual/skill.py:L38-L47]   |

Table: Public definitions and source-relative definition locators.

| Setting or stored field    | Existing default or meaning                                                                           | Source                                         |
| -------------------------- | ----------------------------------------------------------------------------------------------------- | ---------------------------------------------- |
| `SCALE_MIN`, `SCALE_MAX`   | Dimensionless software bounds, 0.5 and 2.0; engineering suitability is NOT_EVALUATED                  | [src/digitalmodel/residual/model.py:L24-L26]   |
| `BIAS_MAX`                 | 1.0 in the caller's channel unit; not a universal physical limit                                      | [src/digitalmodel/residual/model.py:L27-L27]   |
| `MIN_PAIRS`                | 8 finite pairs; a software gate, not a statistical sample-size qualification                          | [src/digitalmodel/residual/model.py:L28-L29]   |
| `lo`, `hi`                 | Output bounds default to 0.0 and positive infinity; scale/bias defaults are 1.0/0.0                   | [src/digitalmodel/residual/model.py:L64-L67]   |
| Domain and fit errors      | Default domain is unbounded; default errors are NaN and pair count is zero; unfitted status escalates | [src/digitalmodel/residual/model.py:L68-L76]   |
| Bound comparison tolerance | 1e-9 in the existing coefficient comparison; not an engineering allowance                             | [src/digitalmodel/residual/model.py:L131-L134] |

Table: Software defaults; each numeric value is attributed to its implementation.

Measured and predicted channels must use compatible units, signs and aligned
samples. Scale is dimensionless when their units match; bias, domain, clipping
bounds, RMSE, gain and noise sigma use the channel unit. The generic arrays carry
no unit, clock, frame or phase metadata. Fit filters nonfinite pairs; the standalone
metrics do not share that filtering behavior.
[src/digitalmodel/residual/model.py:L113-L129]
[src/digitalmodel/residual/skill.py:L13-L47]

## 3. Method and governing standards

The fit uses NumPy least squares for `measured ≈ scale * predicted + bias`.
Finite pairs are retained, insufficient pairs and constant predictions escalate,
and fitted scale/bias are compared with caller bounds. The fit is rejected when
coefficients exceed those bounds; coefficients are not silently clamped into an
active fit. The active output alone is clipped to the output interval.
[src/digitalmodel/residual/model.py:L113-L149]
[src/digitalmodel/residual/model.py:L152-L165]

RMSE is the square root of the mean squared elementwise error. Correlation
delegates to NumPy's Pearson calculation. Gain is RMSE before minus RMSE after;
`is_significant` implements `gain > noise_sigma` after validating the noise value.
This name denotes the implemented threshold rule, not a statistical hypothesis
test or a calibrated confidence level.
[src/digitalmodel/residual/skill.py:L13-L47]

No governing engineering standard or calibrated physical limit is established
by the package. Provenance explicitly marks the fit as `data_driven`,
`not_a_standard` and `not_cited`; these are metadata statements, not qualification.
[src/digitalmodel/residual/model.py:L168-L189]

## 4. Validated worked examples

Only software arithmetic is checked here. The inputs below are synthetic,
dimensionless fixtures selected for independent hand calculation; they are not
measured observations or engineering acceptance data. Their basis is the
following calculation, with executable checks in
`tests/docs/test_residual_agent_pack.py::test_documented_example_matches_independent_calculation`.

For observed `[0, 0]` and predicted `[3, 4]`, the squared errors are 9 and 16;
their mean is `(9 + 16) / 2 = 12.5`, giving `RMSE = sqrt(12.5)`.
The second prediction `[0, 0]` gives zero error, hence `RMSE = 0` and
`gain = sqrt(12.5) - 0 = sqrt(12.5)`. These are hand-derived expectations,
independent of the implementation's output.

For separate synthetic before/after errors, hand subtraction gives `5 - 3 = 2`,
`5 - 2 = 3` and `5 - 4 = 1`. Against noise sigma 2 the required results are,
respectively, `False`, `True`, `False`: equality does not clear a strict threshold.
All literals in this example come from these synthetic hand calculations.

```python
from digitalmodel.residual import is_significant, rmse, skill_gain

observed = [0, 0]
predicted = [3, 4]
improved = [0, 0]
before = rmse(observed, predicted)
after = rmse(observed, improved)
gain = skill_gain(before, after)
boundary_cases = [(5, 3, 2), (5, 2, 2), (5, 4, 2)]
boundary_gains = [skill_gain(pre, post) for pre, post, noise in boundary_cases]
significant = [is_significant(pre, post, noise) for pre, post, noise in boundary_cases]
```

The executable check uses relative tolerance 1e-12 and zero absolute tolerance
for the square-root arithmetic, as specified for this example. It checks exact
Booleans for each threshold case. Neither the zero-error example nor these
threshold comparisons establish forecast validity or measured-data performance.

## 5. QA gates

The package-specific tests run the unchanged AST checker against the real pack,
resolve the registry pointer and execute the fenced example above against the
public interface. The drift test first checks copied packages, then adds a new
public class/member in a nested source file and separately renames a documented
definition. Nonzero checker exits and directional diagnostics are required for
both copies; the real package's file digests are checked for preservation.
[tests/docs/test_residual_agent_pack.py]
[scripts/docs/check_agent_pack_functions.py:L76-L193]

The existing residual tests check synthetic fit recovery, held-out error
reduction, escalation, clipping, plain-value provenance and generic-array reuse.
Passing these checks supports software behavior for those fixtures; it does not
qualify arbitrary input data or the output interval.
[tests/residual/test_residual_model.py]

Required verification routes:

```text
PYTHONPATH=src uv run --no-project --with pytest python -m pytest tests/residual tests/docs/test_residual_agent_pack.py tests/docs/test_agent_pack_functions.py tests/test_main_module_routing.py -q -rA -p no:cacheprovider
python scripts/docs/check_agent_pack_functions.py --pack src/digitalmodel/residual/docs/agent-pack.md
```

Basetemp and log output are directed to scratch when executing these commands.
Routing-test results are reported separately from residual/checker results;
distribution metadata and optional dependency availability affect that root.
[src/digitalmodel/**main**.py:L51-L68]
[src/digitalmodel/**main**.py:L117-L120]

## 6. Known failure modes and pitfalls

| Condition                                             | Existing behavior and limitation                                                                                                                               | Evidence                                                                                      |
| ----------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------- |
| Empty RMSE inputs                                     | Returns NaN, not zero error; equal-shape validation precedes the empty branch                                                                                  | [src/digitalmodel/residual/skill.py:L13-L21]                                                  |
| Empty, singleton or constant correlation series       | Returns NaN for the documented paired cases; constant observed values in the example cannot establish correlation skill                                        | [src/digitalmodel/residual/skill.py:L24-L30]                                                  |
| Mismatched metric shapes                              | RMSE raises `ValueError`; correlation relies on NumPy behavior and provides no explicit equal-shape check                                                      | [src/digitalmodel/residual/skill.py:L13-L30]                                                  |
| Nonfinite metric inputs                               | Standalone RMSE/correlation do not filter missing data; unavailable or infinite results must not become qualified evidence                                     | [src/digitalmodel/residual/skill.py:L15-L30]                                                  |
| Invalid noise                                         | Negative or nonfinite sigma returns `False`; before/after errors have no separate finite-value guard                                                           | [src/digitalmodel/residual/skill.py:L38-L47]                                                  |
| Empty/short fit or constant predicted training series | Returns inactive identity; caller receives an escalation reason, not fitted skill                                                                              | [src/digitalmodel/residual/model.py:L113-L123]                                                |
| Discarded nonfinite training pairs                    | Pair count covers only retained finite pairs; alignment, missing-data bias and representativeness are not established                                          | [src/digitalmodel/residual/model.py:L113-L129]                                                |
| Coefficient bounds exceeded                           | Stores the rejected fit's details but escalates; application returns the raw prediction                                                                        | [src/digitalmodel/residual/model.py:L131-L140] [src/digitalmodel/residual/model.py:L152-L158] |
| Out-of-domain/nonfinite active query                  | Escalates and returns raw input, which may itself be nonfinite or outside output bounds                                                                        | [src/digitalmodel/residual/model.py:L152-L165]                                                |
| Invalid caller controls                               | Ordering/finiteness of output or coefficient bounds and suitability of minimum-pair settings are not comprehensively validated; NumPy exceptions may propagate | [src/digitalmodel/residual/model.py:L93-L149]                                                 |
| Direct dataclass construction                         | Can bypass the `fit` gates; a stored `active` status alone is not proof that training or bounds were qualified                                                 | [src/digitalmodel/residual/model.py:L57-L90]                                                  |
| Provenance mistaken for retained evidence             | The dictionary records model state but contains no dataset digest, source identity or held-out validation record                                               | [src/digitalmodel/residual/model.py:L168-L189]                                                |

Table: Software limits; missing results are distinct from true zero error.

Empty/constant behavior is executed in
`tests/docs/test_residual_agent_pack.py::test_empty_and_constant_series_limits`.
These are software limits, with no forecast-validity implication.

## 7. Lessons log

- Gain equal to the noise floor is not significant under the implemented rule;
  preserve exact Boolean checks around that boundary.
  [src/digitalmodel/residual/skill.py:L38-L47]
- Check the correction status before using its value: escalation returns raw
  input and does not promise output clipping.
  [src/digitalmodel/residual/model.py:L152-L165]
- An empirical fit's self-described provenance does not supply independent
  measured evidence or a standards basis.
  [src/digitalmodel/residual/model.py:L168-L189]

## 8. Maintenance and qualification responsibility

The maintainer will keep the pack inventory and registry pointer consistent with
the package. Engineering acceptance requires qualified measured data, units,
physical bounds, a governing use case and an independent review criterion;
those inputs are NOT_EVALUATED by this documentation change.

Consumer policy remains outside the generic spine. The inspected adapter uses
`max(corrected, raw)` for its decision quantity, so a lower tracking estimate
does not reduce that adapter's raw decision demand. This route was inspected as
source only; adapter integration and physical suitability were not tested here.
[src/digitalmodel/drilling_riser/response_correction.py:L83-L112]

## 9. Dependencies and implementation inventory

| Source file   | Role and dependencies                                                                                      | Inspection boundary                                                                                  |
| ------------- | ---------------------------------------------------------------------------------------------------------- | ---------------------------------------------------------------------------------------------------- |
| `__init__.py` | Public re-exports from `model` and `skill`                                                                 | Complete file; no additional function/class definitions; export list includes the software constants | [src/digitalmodel/residual/**init**.py:L1-L42] |
| `model.py`    | NumPy, standard-library dataclasses/math/typing and local RMSE; private `_escalate` formats status strings | Complete file; public definitions appear in section 2                                                | [src/digitalmodel/residual/model.py:L1-L189]   |
| `skill.py`    | NumPy array conversion, mean, correlation and finite checks                                                | Complete file; metric definitions appear in section 2                                                | [src/digitalmodel/residual/skill.py:L1-L47]    |

Table: Complete package Python source inventory at the source basis above.

The generic package performs no file I/O or solver invocation. The initializer's
reference to future cross-domain reuse is not proof of an implemented consumer.
The inspected response adapter imports and fits `ResidualModel` directly.
[src/digitalmodel/residual/**init**.py:L1-L42]
[src/digitalmodel/drilling_riser/response_correction.py:L61-L80]

## 10. Coverage and open items

| Test root/file                            | Observations supported by its assertions                                                                                                                    | Limit                                                                                           |
| ----------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------- |
| `tests/residual/test_residual_model.py`   | Fit recovery, synthetic interior holdout gain, bounds/short-fit/domain escalation, clipping, provenance, no citation imports and generic-array correction   | Synthetic regression fixtures; no measured-data qualification                                   |
| `tests/docs/test_residual_agent_pack.py`  | Real inventory, registry resolution, added/stale copied-source rejection, independent example arithmetic, empty/constant behavior and numbered-section form | Software/documentation consistency                                                              |
| `tests/docs/test_agent_pack_functions.py` | Existing checker calibration, class-member inventory, exports, source locators, nested definitions and CLI status                                           | AST inventory does not check engineering correctness                                            |
| `tests/test_main_module_routing.py`       | General module/console-script routing, file/unknown-argument dispatch and help behavior                                                                     | Not a residual workflow or forecast test; environment-dependent results are reported separately |

Table: Inspected test files and supported observations; execution counts accompany the PR.

| Item                                   | Status and evidence needed                                                                                           |
| -------------------------------------- | -------------------------------------------------------------------------------------------------------------------- |
| Forecast/field validity                | NOT_EVALUATED; independent measured data and case-specific error criteria are absent                                 |
| Physical limits and sample sufficiency | NOT_EVALUATED; channel-specific bounds and a justified sample/coverage criterion are required                        |
| Statistical significance/confidence    | NOT_EVALUATED; the noise threshold is a software comparison, not a confidence model                                  |
| Consumer integration                   | NOT_EVALUATED; only the cited response-adapter import, fit and decision sites were inspected; its tests were not run |
| Wider routing/dependencies             | NOT_EVALUATED beyond the named routing tests; no repository-wide or licensed-solver execution is claimed             |
| Sibling modules                        | NOT_EVALUATED; seismic and uncertainty remain in the open parent issue                                               |

Table: Open qualification and scope boundaries.
