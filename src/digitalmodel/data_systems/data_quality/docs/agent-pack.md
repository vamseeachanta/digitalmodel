# Data quality module knowledge pack

| Header | Value |
| --- | --- |
| Module | data_systems/data_quality |
| Repository | digitalmodel |
| Package path | src/digitalmodel/data_systems/data_quality |
| Pack revision | 0 |
| Code revision | 444d36f174ed7b2b97f9da7ef3d6ca44ed6baa55 |
| Analysis family | EC; data pipeline; Python calculations gate set |
| Quality role | Calculations quality manager |
| Status | DRAFT |

## 1. Purpose and scope

The module classifies date-indexed observations by reporting lag and removes incomplete periods before a caller computes a trend; it does not fit the trend itself. [src/digitalmodel/data_systems/data_quality/completeness_guardrail.py:L204-L289] [src/digitalmodel/data_systems/data_quality/completeness_guardrail.py:L292-L370]

The public API exports configuration, report and warning types, two source presets, `classify_completeness` and `guard_trend`; the containing domain is marked beta. [src/digitalmodel/data_systems/data_quality/__init__.py:L10-L30] [docs/registry/module-routing.yaml:L113-L127]

Classification uses timestamps rather than observation magnitudes; a COMPLETE flag therefore does not establish record-count adequacy or source finalisation. [src/digitalmodel/data_systems/data_quality/completeness_guardrail.py:L241-L281]

## 2. Inputs and outputs

| Input | Unit / convention | Allowed range and default | Source / evidence |
| --- | --- | --- | --- |
| `series` | Values retain caller units; calendar timestamps identify observation periods | Series, DataFrame, or iterable in classifier; DataFrame selects `value_column` or first column; non-datetime index is coerced and sorted | [src/digitalmodel/data_systems/data_quality/completeness_guardrail.py:L204-L253] |
| `SourceConfig.name` | Text | Required; inserted into warnings and errors | [src/digitalmodel/data_systems/data_quality/completeness_guardrail.py:L119-L122] [src/digitalmodel/data_systems/data_quality/completeness_guardrail.py:L347-L368] |
| `granularity` | Calendar period code | Required; W, M, Q, A or Y; A is normalised to Y | [src/digitalmodel/data_systems/data_quality/completeness_guardrail.py:L124-L132] [src/digitalmodel/data_systems/data_quality/completeness_guardrail.py:L194-L201] |
| `lag_days` | Days after period end | Required; nonnegative; runtime check rejects negative values | [src/digitalmodel/data_systems/data_quality/completeness_guardrail.py:L119-L132] |
| `provisional_days` | Days relative to period end | Default 0; nonnegative; upper bound relative to lag is documented but not enforced | [src/digitalmodel/data_systems/data_quality/completeness_guardrail.py:L113-L132] |
| `as_of` | Timestamp or timestamp string | Default current normalised timestamp; explicit fixed value recommended for reproducibility | [src/digitalmodel/data_systems/data_quality/completeness_guardrail.py:L235-L239] [tests/data_systems/data_quality/test_completeness_guardrail.py:L1-L5] |
| `on_incomplete` | Mode string | warn by default; raise or ignore also accepted; other strings raise ValueError | [src/digitalmodel/data_systems/data_quality/completeness_guardrail.py:L292-L330] |

| Output | Unit / convention | Behaviour | Evidence |
| --- | --- | --- | --- |
| `CompletenessReport.flags` | One enum flag per sorted input observation | complete, partial or provisional; original observation timestamps, despite the period-end claim in its docstring | [src/digitalmodel/data_systems/data_quality/completeness_guardrail.py:L88-L90] [src/digitalmodel/data_systems/data_quality/completeness_guardrail.py:L165-L167] [src/digitalmodel/data_systems/data_quality/completeness_guardrail.py:L255-L273] |
| `safe_window` | Pair of observation timestamps, or None | Minimum and maximum COMPLETE timestamps; does not verify continuity | [src/digitalmodel/data_systems/data_quality/completeness_guardrail.py:L275-L279] |
| `n_incomplete`, `has_incomplete_tail` | Observation count, Boolean | Count all non-COMPLETE observations; property compares count with 0 | [src/digitalmodel/data_systems/data_quality/completeness_guardrail.py:L185-L191] [src/digitalmodel/data_systems/data_quality/completeness_guardrail.py:L281-L288] |
| `safe_mask()` | Boolean per observation | Equality to COMPLETE | [src/digitalmodel/data_systems/data_quality/completeness_guardrail.py:L189-L191] |
| `guard_trend` return | Value Series in caller units, report | Sorted values selected by COMPLETE mask; raises when no complete period exists | [src/digitalmodel/data_systems/data_quality/completeness_guardrail.py:L336-L370] |

There is no spatial coordinate frame, origin, sign or phase transform and no file-format reader or writer in the calculation path; calendar period ends supply the temporal reference. [src/digitalmodel/data_systems/data_quality/completeness_guardrail.py:L194-L370]

## 3. Method and governing standards

| Method | Implementing definition and rule | Basis |
| --- | --- | --- |
| Configuration validation | `SourceConfig.__post_init__` checks frequency membership and negative day values | Code-defined checks, not a cited engineering standard. [src/digitalmodel/data_systems/data_quality/completeness_guardrail.py:L124-L132] |
| Period boundary | `_period_end` returns `pd.Period(ts, freq).end_time` | Calendar convention implemented by pandas. [src/digitalmodel/data_systems/data_quality/completeness_guardrail.py:L194-L201] |
| Completeness | `d = (as_of - period_end).days`; COMPLETE when `d >= lag_days` | Elapsed whole-day comparison. [src/digitalmodel/data_systems/data_quality/completeness_guardrail.py:L255-L260] |
| Incomplete subdivision | When not complete, PROVISIONAL if provisional days is truthy and `(period_end - as_of).days >= -provisional_days`; otherwise PARTIAL | The future-period side has no upper bound. [src/digitalmodel/data_systems/data_quality/completeness_guardrail.py:L261-L269] |
| Safe range and filtering | Min/max of COMPLETE observation indices; `guard_trend` uses the mask rather than slicing between endpoints | No record-count or gap criterion. [src/digitalmodel/data_systems/data_quality/completeness_guardrail.py:L275-L289] [src/digitalmodel/data_systems/data_quality/completeness_guardrail.py:L354-L370] |
| Presets | Monthly lag 548 days / provisional 183 days; annual lag 120 days / provisional 0 days | Configuration values in code, not independently calibrated values. [src/digitalmodel/data_systems/data_quality/completeness_guardrail.py:L137-L156] |

No governing standard, edition or clause is cited for these comparisons or presets, and no standards-derived `Citation` sidecar is emitted by these definitions. [src/digitalmodel/data_systems/data_quality/completeness_guardrail.py:L97-L156] [src/digitalmodel/data_systems/data_quality/completeness_guardrail.py:L194-L370]

UNVERIFIED: the presets establish a reliable source-specific completeness cutoff; independent calibration remains open under #903. [src/digitalmodel/data_systems/data_quality/completeness_guardrail.py:L137-L156]

## 4. Validated worked examples

Validated examples: 0; regression examples: 14; the fixtures are explicitly synthetic and fixed-date, so no case below is credited as independently sourced validation. [tests/data_systems/data_quality/test_completeness_guardrail.py:L1-L5] [tests/data_systems/data_quality/test_completeness_guardrail.py:L23-L195]

Offline record: all listed nodes PASS before and after documentation edits on the header code revision; 14 passed, 0 failed, 0 skipped, 0 NOT_RUN. The full command also covers `tests/contracts` and `tests/docs/test_digitalmodel_routing_contract.py`: 604 passed before and after. Execution form: `PYTHONPATH=src uv run --no-sources --extra test python -m pytest <paths> -q -rA -p no:cacheprovider --basetemp=<scratch>`, without an addopts override.

Each row uses its test function's synthetic inputs, not a live-source input file; exact enum and timestamp assertions have zero mismatch tolerance, and inequality checks retain their stated bounds. [tests/data_systems/data_quality/test_completeness_guardrail.py:L23-L195]

| Regression / expected result and unit | Test node ID | Source and last result |
| --- | --- | --- |
| Recent months incomplete; window start January and window end no later than October, date stamps | `tests/data_systems/data_quality/test_completeness_guardrail.py::test_recent_periods_within_lag_are_partial_and_excluded` | PASS; [tests/data_systems/data_quality/test_completeness_guardrail.py:L37-L60] |
| December PROVISIONAL, 0 enum mismatches | `tests/data_systems/data_quality/test_completeness_guardrail.py::test_freshest_period_is_provisional_not_just_partial` | PASS; [tests/data_systems/data_quality/test_completeness_guardrail.py:L63-L67] |
| Full 12-month window; 0 incomplete observations | `tests/data_systems/data_quality/test_completeness_guardrail.py::test_fully_complete_series_yields_full_window` | PASS; [tests/data_systems/data_quality/test_completeness_guardrail.py:L73-L84] |
| Current year excluded; window end previous year, date stamps | `tests/data_systems/data_quality/test_completeness_guardrail.py::test_ogor_partial_current_year_excluded` | PASS; [tests/data_systems/data_quality/test_completeness_guardrail.py:L90-L105] |
| Annual period complete after lag; 0 incomplete observations | `tests/data_systems/data_quality/test_completeness_guardrail.py::test_ogor_year_becomes_complete_after_finalisation_lag` | PASS; [tests/data_systems/data_quality/test_completeness_guardrail.py:L108-L114] |
| Warning emitted; filtered observations all COMPLETE, 0 enum mismatches | `tests/data_systems/data_quality/test_completeness_guardrail.py::test_guard_trend_warns_and_truncates` | PASS; [tests/data_systems/data_quality/test_completeness_guardrail.py:L120-L128] |
| Incomplete input raises ValueError, 1 exception required | `tests/data_systems/data_quality/test_completeness_guardrail.py::test_guard_trend_raise_mode` | PASS; [tests/data_systems/data_quality/test_completeness_guardrail.py:L131-L134] |
| Ignore mode truncates with 0 warnings | `tests/data_systems/data_quality/test_completeness_guardrail.py::test_guard_trend_ignore_mode_no_warning` | PASS; [tests/data_systems/data_quality/test_completeness_guardrail.py:L137-L144] |
| Complete input passes unchanged; 0 Series mismatches and warnings | `tests/data_systems/data_quality/test_completeness_guardrail.py::test_guard_trend_complete_series_no_warning_full_passthrough` | PASS; [tests/data_systems/data_quality/test_completeness_guardrail.py:L147-L153] |
| Nothing complete raises ValueError, 1 exception required | `tests/data_systems/data_quality/test_completeness_guardrail.py::test_guard_trend_raises_when_nothing_complete` | PASS; [tests/data_systems/data_quality/test_completeness_guardrail.py:L156-L160] |
| Selected DataFrame column, 0 non-COMPLETE observations | `tests/data_systems/data_quality/test_completeness_guardrail.py::test_dataframe_input_with_value_column` | PASS; [tests/data_systems/data_quality/test_completeness_guardrail.py:L166-L170] |
| Date-string indices coerce, 0 non-COMPLETE observations | `tests/data_systems/data_quality/test_completeness_guardrail.py::test_string_index_is_coerced` | PASS; [tests/data_systems/data_quality/test_completeness_guardrail.py:L173-L177] |
| Invalid frequency and negative lag rejected, 2 exceptions required | `tests/data_systems/data_quality/test_completeness_guardrail.py::test_invalid_config_rejected` | PASS; [tests/data_systems/data_quality/test_completeness_guardrail.py:L180-L184] |
| Recent preset observations incomplete, earlier period complete, 0 enum mismatches | `tests/data_systems/data_quality/test_completeness_guardrail.py::test_curated_war_config_flags_recent_years` | PASS; [tests/data_systems/data_quality/test_completeness_guardrail.py:L187-L195] |

## 5. QA gates

Gate applicability follows AQG r0, section 10, Python calculations, with G5 from section 9; the check criteria below are documentation acceptance criteria, not additional source-finalisation claims.

| Gate | Applicability | Module-specific checks / numeric pass criterion / evidence |
| --- | --- | --- |
| G0 | Applicable: dates, configuration and units | C01: invalid-config node in section 4 produces 2 expected ValueErrors, 0 assertion failures. C04: DataFrame and string-index nodes produce 0 non-COMPLETE observations for old dates. C07: reproduce 3 input-risk observations below, 0 mismatches; this records risks rather than accepting them. [tests/data_systems/data_quality/test_completeness_guardrail.py:L166-L184] [src/digitalmodel/data_systems/data_quality/completeness_guardrail.py:L124-L132] [src/digitalmodel/data_systems/data_quality/completeness_guardrail.py:L246-L269] |
| G1 | Applicable: Python import and tests | C02: section-4 command imports the public API and passes all 14 module nodes, 0 failures. C03: five guard-mode nodes (warn, raise, ignore, complete passthrough, nothing complete) have 0 assertion failures; evidence is their individual pytest results. [tests/data_systems/data_quality/test_completeness_guardrail.py:L12-L20] [tests/data_systems/data_quality/test_completeness_guardrail.py:L120-L160] |
| G2 | Applicable: hand calendar comparison | C05: the 12-month synthetic case has 9 COMPLETE, 2 PARTIAL and 1 PROVISIONAL observation, window end September 1, 0 mismatches. C06: January period end plus exactly 90 days is complete, 1 ns earlier is incomplete; 2 assertions and 0 mismatches. Evidence: executable hand checks below compared with the elapsed-day rule. [tests/data_systems/data_quality/test_completeness_guardrail.py:L23-L42] [src/digitalmodel/data_systems/data_quality/completeness_guardrail.py:L255-L279] |
| G3 | NOT_APPLICABLE | No dynamic integration or solver launch occurs in the classifier or filter. [src/digitalmodel/data_systems/data_quality/completeness_guardrail.py:L204-L370] |
| G4 | NOT_APPLICABLE | No diffraction body, mesh or hydrostatics output is produced. [src/digitalmodel/data_systems/data_quality/completeness_guardrail.py:L204-L370] |
| G5 | Applicable: evidence traceability | C08: resolve every repository citation against the header revision, 0 missing files or out-of-range lines. C09: all 14 section-4 nodes have a result at the pin, 0 missing records; before/after aggregate results differ by 0 test outcomes. Evidence: citation resolution and pytest records. [tests/data_systems/data_quality/test_completeness_guardrail.py:L37-L195] |

C05-C07 reproduction (run with `PYTHONPATH=src` in the offline Python environment); the boundary expectations are derived from elapsed-time arithmetic independently of the classifier's returned flags. [src/digitalmodel/data_systems/data_quality/completeness_guardrail.py:L194-L201] [src/digitalmodel/data_systems/data_quality/completeness_guardrail.py:L255-L269]

```python
import pandas as pd
from digitalmodel.data_systems.data_quality import SourceConfig, classify_completeness
c = SourceConfig('synthetic', 'M', 90, 31)
s = pd.Series(range(12), index=pd.date_range('2025-01-01', periods=12, freq='MS'))
r = classify_completeness(s, c, '2026-01-15')
assert r.n_incomplete == 3
assert r.safe_window == (pd.Timestamp('2025-01-01'), pd.Timestamp('2025-09-01'))
assert r.flags.index.equals(s.index)
assert r.flags.value_counts().to_dict() == {'complete': 9, 'partial': 2, 'provisional': 1}
e = pd.Timestamp('2025-01-31 23:59:59.999999999')
a = pd.Series([1], index=pd.to_datetime(['2025-01-01']))
assert classify_completeness(a, c, e + pd.Timedelta(days=90)).n_incomplete == 0
assert classify_completeness(a, c, e + pd.Timedelta(days=90) - pd.Timedelta(nanoseconds=1)).n_incomplete == 1
assert SourceConfig('synthetic', 'M', 1, 2).provisional_days == 2
assert classify_completeness([1, 2], c, '2026-01-15').flags.index.min().year == 1970
future = pd.Series([1], index=pd.to_datetime(['2030-01-01']))
assert classify_completeness(future, c, '2026-01-15').flags.iloc[0].value == 'provisional'
```

## 6. Known failure modes and pitfalls

sweep: 20 found

Sweep scope: both package Python files, including defaults, date coercion, integer conversion, implicit units, empty outcomes and silent warning suppression; the rows distinguish intended defaults from unchecked assumptions. [src/digitalmodel/data_systems/data_quality/__init__.py:L1-L30] [src/digitalmodel/data_systems/data_quality/completeness_guardrail.py:L97-L370]

Exact discovery command: `rg -n 'except|continue|return None|return \[\]|return \{\}|= 0(\.0)?\b|1970|0\.05|int\(|float\(|days|to_datetime|Timestamp.now|columns\[0\]|flags\[' src/digitalmodel/data_systems/data_quality`. Manual review of every definition also covered implicit defaults and empty writes; there are no exception handlers, record-skipping continues, external-text numeric conversions or cache writes in these files. [src/digitalmodel/data_systems/data_quality/__init__.py:L1-L30] [src/digitalmodel/data_systems/data_quality/completeness_guardrail.py:L58-L370]

| ID / site at code pin | Symptom / cause | Detection check / disposition |
| --- | --- | --- |
| S01: provisional default 0 | Disables provisional labelling by default | C02 regression coverage; source adequacy: no detection check exists; #903. [src/digitalmodel/data_systems/data_quality/completeness_guardrail.py:L113-L122] |
| S02: monthly lag 548 days | Assumed cutoff can misclassify an actual source's completeness | C02 tests preset behaviour; independent calibration: no detection check exists; #903. [src/digitalmodel/data_systems/data_quality/completeness_guardrail.py:L137-L145] |
| S03: monthly provisional 183 days | Fixed near-tail duration is not measured from input values | C02; empirical adequacy: no detection check exists; #903. [src/digitalmodel/data_systems/data_quality/completeness_guardrail.py:L140-L146] |
| S04: annual lag 120 days | Fixed finalisation duration can diverge from actual source | C02; calibration: no detection check exists; #903. [src/digitalmodel/data_systems/data_quality/completeness_guardrail.py:L149-L154] |
| S05: annual provisional 0 days | Annual incomplete periods are PARTIAL, never PROVISIONAL | C02; intended preset, not a distinct defect; #903. [src/digitalmodel/data_systems/data_quality/completeness_guardrail.py:L151-L156] |
| S06: current clock fallback | Omitted as_of makes repeated runs time-dependent | Existing fixtures explicitly fix time; missing-time detection: no detection check exists; issue pending (1). [src/digitalmodel/data_systems/data_quality/completeness_guardrail.py:L235-L239] [tests/data_systems/data_quality/test_completeness_guardrail.py:L1-L5] |
| S07: first-column fallback in classifier | Omitted column selects by position; no columns raises IndexError | C04 covers explicit column only; fallback adequacy: no detection check exists; issue pending (1). [src/digitalmodel/data_systems/data_quality/completeness_guardrail.py:L241-L243] |
| S08: iterable conversion | Implicit RangeIndex becomes timestamps near the epoch and appears old | C07 reproduces year 1970; rejection: no detection check exists; issue pending (1). [src/digitalmodel/data_systems/data_quality/completeness_guardrail.py:L246-L251] |
| S09: classifier datetime coercion | Input strings or integers use default parsing; no explicit date format or unit | C04 covers ISO-like strings; ambiguous text: no detection check exists; issue pending (1). [src/digitalmodel/data_systems/data_quality/completeness_guardrail.py:L249-L253] |
| S10: calendar unit assumption | Period-end conversion assumes calendar period semantics; timezone policy is unspecified | no detection check exists; issue pending (1). [src/digitalmodel/data_systems/data_quality/completeness_guardrail.py:L194-L201] |
| S11: whole-day truncation | `.days` compares whole elapsed days at nanosecond period ends | C06 checks exact threshold and 1 ns below it; issue pending (1) for broader boundary coverage. [src/digitalmodel/data_systems/data_quality/completeness_guardrail.py:L257-L260] |
| S12: provisional comparison | Truthy threshold plus one-sided comparison can flag arbitrarily distant future periods | C07 reproduces future-period flag; input rejection: no detection check exists; issue pending (1). [src/digitalmodel/data_systems/data_quality/completeness_guardrail.py:L263-L269] |
| S13: keyed flags | Duplicate observation timestamps overwrite dictionary entries before re-expansion; uniqueness is not checked | no detection check exists; issue pending (1). [src/digitalmodel/data_systems/data_quality/completeness_guardrail.py:L255-L273] |
| S14: empty complete set | Returns safe_window None; empty input and wholly incomplete input are not distinguished by this field | C03 checks all-incomplete raise; empty-input contract: no detection check exists; issue pending (1). [src/digitalmodel/data_systems/data_quality/completeness_guardrail.py:L275-L279] [src/digitalmodel/data_systems/data_quality/completeness_guardrail.py:L347-L352] |
| S15: range endpoints | Min/max safe_window does not detect missing intervening periods | C05 checks ordinary window only; gap detection: no detection check exists; issue pending (1). [src/digitalmodel/data_systems/data_quality/completeness_guardrail.py:L275-L279] |
| S16: integer count conversion | `int()` converts an internal Boolean sum, not external numeric text; count includes every non-COMPLETE row | C05 checks 3 incomplete observations; no external-separator parsing occurs here. [src/digitalmodel/data_systems/data_quality/completeness_guardrail.py:L281-L288] |
| S17: first-column fallback in filter | Selected column is reconstructed independently of classification | C04 tests classifier only; filter fallback: no detection check exists; issue pending (1). [src/digitalmodel/data_systems/data_quality/completeness_guardrail.py:L337-L339] |
| S18: filter datetime coercion | Same implicit parsing repeated before positional mask application | C03 covers datetime-index fixtures; coercion alignment: no detection check exists; issue pending (1). [src/digitalmodel/data_systems/data_quality/completeness_guardrail.py:L342-L355] |
| S19: ignore warning policy | Truncation becomes silent when ignore mode is requested | C03 asserts 0 warnings and shorter output; intended opt-in policy; #903. [src/digitalmodel/data_systems/data_quality/completeness_guardrail.py:L365-L370] [tests/data_systems/data_quality/test_completeness_guardrail.py:L137-L144] |
| S20: value independence | Zero, missing or implausibly low observation magnitudes cannot affect completeness | C02 covers time logic; record-count adequacy: no detection check exists; #903. [src/digitalmodel/data_systems/data_quality/completeness_guardrail.py:L255-L281] |

| Other confirmed pitfall / incident | Detection / disposition |
| --- | --- |
| Report docstring says period-end timestamps and contiguous safe_window; implementation preserves input stamps and uses min/max | C05 exposes actual month-start stamps; issue pending (1). [src/digitalmodel/data_systems/data_quality/completeness_guardrail.py:L165-L172] [src/digitalmodel/data_systems/data_quality/completeness_guardrail.py:L271-L279] |
| Documented provisional <= lag condition is not enforced; nonnegative numeric types are not checked as integers | C07 reproduces provisional > lag acceptance; other type rejection: no detection check exists; issue pending (1). [src/digitalmodel/data_systems/data_quality/completeness_guardrail.py:L113-L132] |
| Reporting-lag and partial-year issue #903, within #890 | C02/C03 detect incomplete tails in synthetic input; no detection check exists for live filing magnitudes or downstream dashboard adoption. [src/digitalmodel/data_systems/data_quality/completeness_guardrail.py:L1-L1] [tests/data_systems/data_quality/test_completeness_guardrail.py:L37-L160] |

## 7. Lessons log

| Date | Entry | Lesson / pack change | Evidence | Shared-log entry |
| --- | --- | --- | --- | --- |
| 2026-11-08 | Initial documentation baseline | Separate calendar completeness from magnitude adequacy; retain synthetic cases as regression evidence | [src/digitalmodel/data_systems/data_quality/completeness_guardrail.py:L255-L281] [tests/data_systems/data_quality/test_completeness_guardrail.py:L1-L5] | Not assigned; issue pending (2) |

## 8. Ownership

Review contract: calculations quality manager, with a second provider for independent review; ambiguous acceptance criteria go to that quality role, and cross-family scope decisions go to the maintainer (AQG r0, section 11).

UNVERIFIED: a deputy quality manager and durable shared-log registration have been assigned; issue pending (2).

## 9. Dependencies and implementation inventory

Runtime dependencies used directly here are pandas and the Python standard library's warnings, dataclasses, enum and typing; public exports delegate to the completeness module. [src/digitalmodel/data_systems/data_quality/completeness_guardrail.py:L58-L65] [src/digitalmodel/data_systems/data_quality/__init__.py:L10-L30]

The upstream input is caller-supplied observations; the downstream interface is a filtered Series and report, without a solver version or external dataset being loaded by this package. [src/digitalmodel/data_systems/data_quality/completeness_guardrail.py:L204-L253] [src/digitalmodel/data_systems/data_quality/completeness_guardrail.py:L292-L370]

Implementation-map source: `codex/implmap-work` at `3256bab95f84da3d9fadce8d5dcf8458d66d1eb4`, map data revision `270c8759c374b67a7f032953e7fcdb14e0224461`; inventory results below use this separate map pin and its named data revision. [docs/registry/implementation-map.yaml:L1-L3]

A8 inventory: 0 module map families and 0 implementation rows; no map implementation path lies in this package, and there are no excluded subpackages. Counted families: 0. A8 inventory passes empty; the counted-family search is `NOT_APPLICABLE` because no family has an in-package row. [docs/registry/implementation-map.yaml:L1-L53690]

Map follow-up: period-end completeness classification and COMPLETE-mask trend filtering, the section-3 method families, need map rows and search signals before a later revision counts them; issue pending (3). [src/digitalmodel/data_systems/data_quality/completeness_guardrail.py:L194-L289] [src/digitalmodel/data_systems/data_quality/completeness_guardrail.py:L292-L370]

Additional discovery command: `rg -n 'completeness|guard_trend|classify_completeness|reporting.lag|partial.year' src scripts docs tests --glob '*.py' --glob '*.md'`; this searches repository code including scripts, beyond the A8 counted-family scope. Direct classifier/filter method hits lead to the package definitions and the dedicated tests; other generic completeness matches are not evidence of independent implementations of the lag criterion. [src/digitalmodel/data_systems/data_quality/completeness_guardrail.py:L204-L370] [tests/data_systems/data_quality/test_completeness_guardrail.py:L12-L195]

## 10. Coverage and open items

Files read: both package Python files in full and the dedicated test file in full; no package-local README or docs existed at the code pin. Domain context read: `src/digitalmodel/data_systems/README.md`, `docs/README.md`, `docs/maps/digitalmodel-operator-map.md` and the domain registry entry; the targeted search `rg -l 'data_quality|reporting.lag|partial.year' docs --glob '*.md'` found 0 module-specific Markdown files. [src/digitalmodel/data_systems/data_quality/__init__.py:L1-L30] [src/digitalmodel/data_systems/data_quality/completeness_guardrail.py:L1-L370] [tests/data_systems/data_quality/test_completeness_guardrail.py:L1-L195] [docs/registry/module-routing.yaml:L113-L127]

The inventory's approximate test count is replaced by 14 collected module tests, all listed in section 4; no live-source dataset values from the module's introductory docstring are reproduced. [tests/data_systems/data_quality/test_completeness_guardrail.py:L37-L195] [src/digitalmodel/data_systems/data_quality/completeness_guardrail.py:L1-L24]

| Open item / coverage limit | Responsible role | Issue |
| --- | --- | --- |
| UNVERIFIED claim from section 3: preset cutoff adequacy; live count-magnitude and downstream integration checks absent | Data maintainer | #903; context #890. [src/digitalmodel/data_systems/data_quality/completeness_guardrail.py:L1-L1] [src/digitalmodel/data_systems/data_quality/completeness_guardrail.py:L137-L156] |
| Input contracts, defaults, calendar boundaries, gaps, duplicates, missing timestamps, timezone policy and report-docstring corrections in section 6 | Calculation maintainer | issue pending (1). [src/digitalmodel/data_systems/data_quality/completeness_guardrail.py:L113-L132] [src/digitalmodel/data_systems/data_quality/completeness_guardrail.py:L165-L172] [src/digitalmodel/data_systems/data_quality/completeness_guardrail.py:L194-L370] |
| No independently sourced durable worked example; hand boundary probe is supplemental, not a section-4 validated case | Calculation maintainer | issue pending (2). [tests/data_systems/data_quality/test_completeness_guardrail.py:L1-L5] [tests/data_systems/data_quality/test_completeness_guardrail.py:L23-L195] |
| UNVERIFIED assignment claim from section 8: deputy and shared-log registration; independent clearance remains pending | Maintainer | issue pending (2); review contract: AQG r0, section 11. |
| Missing map families for period completeness and trend filtering | Map maintainer | issue pending (3). [src/digitalmodel/data_systems/data_quality/completeness_guardrail.py:L194-L370] |

The calculation-output citation contract was read; these methods provide code-defined calendar criteria and presets rather than a named standards-derived formula, so no licensed clause or standard value is asserted here. [src/digitalmodel/data_systems/data_quality/completeness_guardrail.py:L97-L156] [src/digitalmodel/data_systems/data_quality/completeness_guardrail.py:L194-L289]
