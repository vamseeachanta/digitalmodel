# Marine operations method inventory

This discovery supplement enumerates current direct-package definitions and
reconciles the envelope calculation with the historical implementation map.
Inventory is not numerical qualification. Sub-packages retain separate packs.

## Revision history

| Revision | Date | What changed and why |
|---|---|---|
| 1 | 2026-11-05 | Enumerate direct definitions, classify relations and check neutral documentation references to expose discovery gaps. |

## Evidence and search scope

Inventory revision: `aa96c303e1971317995b0aed3727847942fd5d5a`.
The Python sources and implementation map are unchanged by this supplement.
Line numbers below identify `def` statements at this revision; decorators and
automatically generated dataclass methods are excluded. Private functions,
properties, class methods and nested definitions are included. No discovered
calculation is imported or executed for discovery.

The [original pack](agent-pack.md), sections 6 and 9–10, retains its exact
failure sweep, three map search signals, documentation discovery pattern,
original discovery revision, map revision and embedded data revision. Its
141-row wave-spectra inventory and two direct-package rows are historical
evidence, not a count of today's files or a completeness claim for today's code.
The map's embedded revision remains `270c8759c374b67a7f032953e7fcdb14e0224461`.

Additional discovery commands, without changing those historical expressions:

```bash
rg --files src/digitalmodel/marine_ops -g '*.py'
rg -n '^ *(async )?def |^ *class ' src/digitalmodel/marine_ops/*.py
rg -n 'compute_response_spectrum|spectral_moments|significant_amplitude|jonswap|crane_tip_raos|heading_column|np\.interp' src/digitalmodel/marine_ops/*.py
```

The first command includes sub-packages for boundary inspection; only the
non-recursive `*.py` selection contributes to the tables. The acceptance test
uses Python AST traversal to include definitions nested inside control flow,
which a line search alone cannot reliably associate with enclosing scopes.

## Relation definitions

Relations describe implementation structure, not validated numerical equivalence:
`independent` identifies a local calculation or decision algorithm; `wrapper`
identifies an adapter or delegating interface; `helper` identifies validation,
lookup, interpolation, serialization or presentation support. Local quadrature,
coverage and heading rules are helpers in the envelope method, not additional
independent envelope solvers. A wrapper may add validation or aggregation.

The last column records the historical map's relation separately from this
structural classification. `not mapped` means no direct definition row in that
map; it does not mean the method is absent or qualified. For example,
`_significant_response` delegates to seakeeping functions although the historical
wave-spectra map labels it independent. No copy relationship is asserted without
source comparison. Formal registration of missing families remains open.

## Direct files

| File | Definition count |
|---|---|
| `__init__.py` | 0 |
| `batch_envelope.py` | 3 |
| `envelope_numerics.py` | 8 |
| `failure_sequence.py` | 5 |
| `jackup_failure_sequence.py` | 16 |
| `jackup_inputs.py` | 6 |
| `jackup_screening.py` | 6 |
| `operation_envelope.py` | 15 |
| `preload_sequence.py` | 7 |
| `response_benchmark.py` | 3 |
| `self_elevating_monitoring.py` | 3 |

Total: 11 direct files and 72 explicit definitions.

## Direct definitions

File names are relative to `src/digitalmodel/marine_ops/`. Each symbol and line
is checked against the source AST, including nested local functions and classes.

| File | Symbol | Line | Method family / role | Structural relation | Historical map disposition |
|---|---|---|---|---|---|
| `batch_envelope.py` | `allowable_hs_table` | 59 | batch spectral envelope | independent | not mapped |
| `batch_envelope.py` | `_criterion_component` | 141 | batch spectral envelope | helper | not mapped |
| `batch_envelope.py` | `_cell` | 182 | batch spectral envelope | helper | not mapped |
| `envelope_numerics.py` | `positive` | 10 | envelope validation and quadrature | helper | not mapped |
| `envelope_numerics.py` | `finite_grid` | 15 | envelope validation and quadrature | helper | not mapped |
| `envelope_numerics.py` | `validate_operation` | 24 | envelope validation and quadrature | helper | not mapped |
| `envelope_numerics.py` | `validate_component` | 38 | envelope validation and quadrature | helper | not mapped |
| `envelope_numerics.py` | `integrate` | 60 | envelope validation and quadrature | wrapper | not mapped |
| `envelope_numerics.py` | `spectrum` | 64 | envelope validation and quadrature | helper | not mapped |
| `envelope_numerics.py` | `extrapolated_fraction` | 86 | envelope validation and quadrature | helper | not mapped |
| `envelope_numerics.py` | `heading_column` | 117 | envelope validation and quadrature | helper | not mapped |
| `failure_sequence.py` | `FailureMode.utilization` | 28 | failure threshold ranking | helper | not mapped |
| `failure_sequence.py` | `FailureSequence.ordered` | 59 | failure threshold ranking | helper | not mapped |
| `failure_sequence.py` | `FailureSequence.governing` | 65 | failure threshold ranking | helper | not mapped |
| `failure_sequence.py` | `_number` | 78 | failure threshold ranking | helper | not mapped |
| `failure_sequence.py` | `rank_failures` | 84 | failure threshold ranking | independent | not mapped |
| `jackup_failure_sequence.py` | `_assumed` | 29 | jack-up failure screening | helper | not mapped |
| `jackup_failure_sequence.py` | `JackupInputs.__post_init__` | 80 | jack-up failure screening | helper | not mapped |
| `jackup_failure_sequence.py` | `JackupInputs.assumptions` | 97 | jack-up failure screening | helper | not mapped |
| `jackup_failure_sequence.py` | `member_capacity` | 112 | jack-up failure screening | independent | not mapped |
| `jackup_failure_sequence.py` | `_check_state` | 146 | jack-up failure screening | helper | not mapped |
| `jackup_failure_sequence.py` | `punch_consequence` | 155 | jack-up failure screening | independent | not mapped |
| `jackup_failure_sequence.py` | `screen_jackup` | 216 | jack-up failure screening | independent | not mapped |
| `jackup_failure_sequence.py` | `screen_jackup.ballast` | 257 | jack-up failure screening | helper | not mapped |
| `jackup_failure_sequence.py` | `screen_jackup.path` | 260 | jack-up failure screening | helper | not mapped |
| `jackup_failure_sequence.py` | `screen_jackup.compression` | 273 | jack-up failure screening | helper | not mapped |
| `jackup_failure_sequence.py` | `screen_jackup.holding` | 276 | jack-up failure screening | helper | not mapped |
| `jackup_failure_sequence.py` | `screen_jackup.leg_moment` | 279 | jack-up failure screening | helper | not mapped |
| `jackup_failure_sequence.py` | `screen_jackup.chord_demand` | 283 | jack-up failure screening | helper | not mapped |
| `jackup_failure_sequence.py` | `screen_jackup.brace_demand` | 290 | jack-up failure screening | helper | not mapped |
| `jackup_failure_sequence.py` | `screen_jackup.add` | 298 | jack-up failure screening | helper | not mapped |
| `jackup_failure_sequence.py` | `screen_jackup.triggered` | 343 | jack-up failure screening | helper | not mapped |
| `jackup_inputs.py` | `frozen_inputs` | 19 | input evidence and extremes | helper | not mapped |
| `jackup_inputs.py` | `observation_summary` | 30 | input evidence and extremes | helper | not mapped |
| `jackup_inputs.py` | `query_public_inputs` | 63 | input evidence and extremes | helper | not mapped |
| `jackup_inputs.py` | `gumbel_return_levels` | 95 | input evidence and extremes | independent | not mapped |
| `jackup_inputs.py` | `frozen_environment` | 130 | input evidence and extremes | helper | not mapped |
| `jackup_inputs.py` | `frozen_environment.ExtractClient.stream_lines` | 152 | input evidence and extremes | helper | not mapped |
| `jackup_screening.py` | `_signed_finite` | 22 | static jack-up equilibrium | helper | not mapped |
| `jackup_screening.py` | `StaticScreen.admissible` | 39 | static jack-up equilibrium | helper | not mapped |
| `jackup_screening.py` | `StaticScreen.failing_checks` | 44 | static jack-up equilibrium | helper | not mapped |
| `jackup_screening.py` | `screen_three_leg_unit` | 51 | static jack-up equilibrium | independent | not mapped |
| `jackup_screening.py` | `environmental_actions` | 120 | static jack-up equilibrium | independent | not mapped |
| `jackup_screening.py` | `static_operating_envelope` | 183 | static jack-up equilibrium | independent | not mapped |
| `operation_envelope.py` | `jackup_operating_envelope` | 50 | static jack-up envelope adapter | wrapper | not mapped |
| `operation_envelope.py` | `available_operations` | 133 | vessel spectral envelope | helper | not mapped |
| `operation_envelope.py` | `resolve_operation` | 137 | vessel spectral envelope | helper | not mapped |
| `operation_envelope.py` | `EnvelopePoint.hs_forecast_limit_m` | 160 | result lookup and serialization | wrapper | not mapped |
| `operation_envelope.py` | `EnvelopeResult.require_valid_points` | 175 | result lookup and serialization | helper | not mapped |
| `operation_envelope.py` | `EnvelopeResult.require_evaluated` | 191 | result lookup and serialization | helper | not mapped |
| `operation_envelope.py` | `EnvelopeResult.hs_limit_at` | 196 | result lookup and serialization | helper | not mapped |
| `operation_envelope.py` | `EnvelopeResult.to_dict` | 203 | result lookup and serialization | helper | not mapped |
| `operation_envelope.py` | `operation_envelope` | 229 | vessel spectral envelope | independent | wave-spectra / independent |
| `operation_envelope.py` | `_evaluate_point` | 304 | vessel spectral envelope | helper | not mapped |
| `operation_envelope.py` | `envelope_for_operations` | 326 | operation selection and aggregation | wrapper | not mapped |
| `operation_envelope.py` | `plot_envelopes` | 355 | envelope presentation | helper | not mapped |
| `operation_envelope.py` | `_component_for` | 400 | RAO selection and interpolation | helper | not mapped |
| `operation_envelope.py` | `_rao_by_heading` | 404 | RAO selection and interpolation | helper | not mapped |
| `operation_envelope.py` | `_significant_response` | 418 | vessel spectral envelope | wrapper | wave-spectra / independent |
| `preload_sequence.py` | `_decimal_input` | 28 | preload history screening | helper | not mapped |
| `preload_sequence.py` | `PreloadCriteria.validate` | 83 | preload history screening | helper | not mapped |
| `preload_sequence.py` | `_validate_history` | 134 | preload history screening | helper | not mapped |
| `preload_sequence.py` | `_planned_depth` | 160 | preload history screening | helper | not mapped |
| `preload_sequence.py` | `_settlement_rate` | 172 | preload history screening | helper | not mapped |
| `preload_sequence.py` | `_hold_evidence` | 191 | preload history screening | helper | not mapped |
| `preload_sequence.py` | `screen_preload_sequence` | 215 | preload history screening | independent | not mapped |
| `response_benchmark.py` | `ReferenceIntake.intake_complete` | 68 | benchmark evidence intake | helper | not mapped |
| `response_benchmark.py` | `inspect_response_reference` | 73 | benchmark evidence intake | helper | not mapped |
| `response_benchmark.py` | `_present` | 121 | benchmark evidence intake | helper | not mapped |
| `self_elevating_monitoring.py` | `AlertRule.__post_init__` | 192 | monitoring alerts | helper | not mapped |
| `self_elevating_monitoring.py` | `evaluate` | 216 | monitoring alerts | independent | not mapped |
| `self_elevating_monitoring.py` | `synthetic_cases` | 274 | monitoring alerts | helper | not mapped |

## Upstream and adjacent relations

Consumers use `file:symbol` relative to the direct package. Targets use
repository-relative paths and qualified Python symbols. `delegates` records a
call route (indirect where stated), `independent` records a distinct adjacent
algorithm, and `wrapper` records an adapter. All target definitions resolve in
the inventory revision. These rows cover the envelope route; they are not a
repository-wide dependency graph for the other direct modules.

| Consumer | Target file | Target symbol | Relation | Evidence / consequence |
|---|---|---|---|---|
| `envelope_numerics.py:spectrum` | `src/digitalmodel/hydrodynamics/wave_spectra.py` | `WaveSpectra.jonswap` | delegates | Calls spectra.jonswap with a peak-scaled grid; adds captured-variance screening. |
| `envelope_numerics.py:spectrum` | `src/digitalmodel/hydrodynamics/wave_spectra.py` | `WaveSpectra._normalize_to_significant_height` | delegates | Indirect through jonswap; normalization is upstream, not a separate local implementation. |
| `envelope_numerics.py:spectrum` | `src/digitalmodel/hydrodynamics/wave_spectra.py` | `WaveSpectra.spectral_moment` | delegates | Indirect through normalization; upstream trapezoidal moment calculation. |
| `operation_envelope.py:_significant_response` | `src/digitalmodel/hydrodynamics/seakeeping.py` | `compute_response_spectrum` | delegates | Calls the shared magnitude-squared response product. |
| `operation_envelope.py:_significant_response` | `src/digitalmodel/hydrodynamics/seakeeping.py` | `spectral_moments` | delegates | Calls the shared response integration at order zero. |
| `operation_envelope.py:_significant_response` | `src/digitalmodel/hydrodynamics/seakeeping.py` | `significant_amplitude` | delegates | Calls the shared significant single-amplitude conversion. |
| `batch_envelope.py:_criterion_component` | `src/digitalmodel/marine_ops/installation/crane_tip_motion.py` | `crane_tip_raos` | wrapper | Adapts phase-aware point transfer and derivative response magnitudes for table criteria. |
| `operation_envelope.py:operation_envelope` | `src/digitalmodel/marine_ops/installation/operability.py` | `hs_limit_for_criterion` | independent | Adjacent discrete Hs search; the vessel envelope uses linear closed-form scaling and does not call this search. |
| `operation_envelope.py:operation_envelope` | `src/digitalmodel/drilling_riser/operability.py` | `significant_wave_height_limit` | independent | Adjacent scalar scaling; response conventions and interchangeability are not established. |
| `operation_envelope.py:_evaluate_point` | `src/digitalmodel/marine_ops/batch_envelope.py` | `_cell` | independent | Separate criterion loops reuse coverage and response helpers; batch output retains each requested heading. |
| `operation_envelope.py:jackup_operating_envelope` | `src/digitalmodel/marine_ops/jackup_screening.py` | `static_operating_envelope` | wrapper | Delegates sampled static admissibility; no vessel RAO or spectral criteria are applied. |

`_rao_by_heading` sorts frequency samples and calls `numpy.interp` with held
edges; `heading_column` selects a bounded circular nearest heading, without
heading interpolation. `extrapolated_fraction` adds a held-edge response-moment
coverage check. `EnvelopeResult.hs_limit_at` is result lookup, not spectral
interpolation; `to_dict` and `plot_envelopes` are presentation consumers. These
relations distinguish the previously unclassified helper families from the two
historical wave-spectra rows.

## Public documentation locators

These neutral repository references were checked for existence on 2026-11-05;
the acceptance test repeats that check without network access. They document
the current calculation, API selection and adjacent methods. They are
not a reconstruction of the withheld source, nor proof of its provenance.
No restricted title, original locator or derived content is published.

| Purpose | Repository-relative locator | Locator status |
|---|---|---|
| `Envelope equations and numerical contracts` | `src/digitalmodel/marine_ops/docs/envelope-basis.md` | checked in repository |
| `Current envelope API and historical discovery` | `src/digitalmodel/marine_ops/docs/agent-pack.md` | checked in repository |
| `Batch criteria, point transfer and table output` | `src/digitalmodel/marine_ops/docs/batch-allowable-hs.md` | checked in repository |
| `Independent response evidence intake limits` | `src/digitalmodel/marine_ops/docs/response-benchmark-basis.md` | checked in repository |
| `Installation scope and qualification boundary` | `src/digitalmodel/marine_ops/installation/docs/agent-pack.md` | checked in repository |
| `Historical method-map discovery contract` | `docs/registry/implementation-map.md` | checked in repository |

## Remaining mapping gaps and limits

Withheld discovery source: unresolved. A public-neutral equivalent with matching
content and provenance has not been established. The references above support
navigation for current public code only; the original discovery row remains
withheld. No restricted source is renamed or guessed.

Only two direct definitions occur in the historical wave-spectra map. Missing
registry entries include envelope validation, criterion aggregation, RAO
selection/interpolation, coverage, result lookup/presentation and the newer
batch, failure-ranking, jack-up, preload, monitoring and input-evidence methods.
Their structural relations are enumerated here; formal registry family IDs,
repository-wide candidate reconciliation and numerical equivalence are not
established. The historical registry is not regenerated in this bounded update.
The maintainers record remaining mapping decisions under issue #2770.

No numerical API, screening criterion, source entitlement, standards compliance,
physical benchmark qualification or engineering approval is changed by this
inventory. Source existence, an AST match and a mapped relation establish
discoverability only.
