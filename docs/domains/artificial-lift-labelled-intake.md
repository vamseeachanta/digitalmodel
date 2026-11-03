# Labelled dynamometer-card intake

The collection workflow for #1865 accepts numeric cards, retains independent
label evidence under private custody, and prepares an explicit selection for
`BenchmarkRunner`. It does not include a real-card dataset or establish field
accuracy. Public output is a pseudonymised projection requiring a separate
permission and disclosure review.

## Revision history

| Revision | Date | Change and reason |
|---|---|---|
| 1 | 2026-10-19 | Add intake, collection accounting and a benchmark adapter so independently labelled cards can be collected without exporting raw identities. |
| 2 | 2026-10-19 | Require alias keys of at least 32 bytes, accept CSV byte order marks, reject future review dates and clarify the numeric contract. Remove contributor process guidance from the engineering method. |

## Collection and custody

Keep original files, identity mappings, reports and permission records in the
custodian's private repository. Record their digests there. A file path or an
assertion that data is retained does not prove retention: the custodian verifies
it before setting `original_retained`. Do not put received originals, private
references, controller headers or workover notes in this public repository.

Agree analysis and publication rights separately before use, including whether
anonymised geometry and derived results may be published. An opaque reference is
an audit link into the custodian's records, not proof that permission was granted.
Field accuracy remains **NOT_EVALUATED** until an authorised held-out real set
has been evaluated.

## Input contract

Import `prepare_intake` and `read_card_csv` from
`digitalmodel.marine_ops.artificial_lift.dynacard.intake`.
`prepare_intake(records, alias_key=private_key_bytes)` validates the entire batch
before returning an `IntakeBatch`. The caller supplies a private, unpredictable
key of at least 32 bytes and keeps it and the original identity mapping outside
releases. Length alone does not establish unpredictability. Use the same key and
stable private identifiers across batches for repeat linkage.

Each record is a dictionary with these required fields:

| Field | Meaning |
|---|---|
| `well_id`, `card_id` | Stable private identifiers; never copied into the projection. |
| `kind` | `real`, `synthetic`, or `plate`; a custodian assertion requiring review. |
| `acquisition` | `recorded` or `generated`; real cards require `recorded`. |
| `card_space` | `surface` or `pump`; surface intake does not perform inversion. |
| `position`, `load` | Lists of finite built-in Python `int` or `float` values, paired in traversal order, in inches and lbf. Booleans and NumPy scalars are rejected; convert NumPy arrays with `tolist()` first. The existing card validator requires at least three samples. |
| `relative_day` | Nonnegative elapsed day from a private well-specific origin; no calendar capture date is exported. |
| `context` | Numeric fields listed below; no automatic conversion or assumed inputs. |
| `label` | Current diagnostic mode or null, independent basis and private evidence references. |
| `rights` | Explicit Boolean `analysis` and `publication`, plus private `evidence_ref`. |
| `original_retained` | Boolean assertion that required original retention was verified. |

`context` requires `rod_string` as a nonempty list of objects containing
`diameter_in` and `length_ft`, plus `pump_diameter_in`,
`pump_setting_depth_ft`, `stroke_in`, `spm`, and `production_bpd`.
Dimensions and speed must be positive; approximate production may be zero.
These are schema domains, not performance acceptance limits.

`label` requires `mode` to be a current `ALL_GENERATORS` mode or null. Its
`basis` is `pulling_report`, `workover_report`, `independent_inspection`, or
`unconfirmed`. Confirmed bases require nonempty `evidence_ref`, `reviewer_ref`
and ISO `reviewed_on`, no later than the local calendar date at intake.
Use opaque private references; names are unnecessary.
The inspection must establish the condition independently of this classifier.
Unconfirmed labels must use a null mode and cannot enter a benchmark.
External taxonomies require a documented human crosswalk before assigning a
current mode; ambiguous outcomes remain unconfirmed.

Optional `time_s` values must be nonnegative and strictly increasing, with one
value per position/load pair. `read_card_csv(text)` requires unique column names
`time_s,position_in,load_lbf`; a single leading UTF-8 byte order mark is accepted
and other columns are discarded. It returns the three numeric arrays suitable
for merging into a record. No unit inference occurs. Numeric context and
`time_s` values also require built-in Python `int` or `float` values.
Negative loads are permitted; timing is retained without resampling.
Proprietary binary exports and digitised graphics require a separate authorised
conversion and provenance workflow; this adapter does not support them.

Optional `stage` is `standalone` (default), `before` or `after`. Before/after
records require a common private `event_id` on the same well and increasing
`relative_day` (fractional elapsed days are permitted). Duplicate event stages
are rejected. A complete pair records chronology; review the rod string,
intervention and operating context before attributing a condition change to the
intervention.

## Projection and collection accounting

`publication_document()` returns copies of numeric samples and context,
controlled mode/basis values, pseudonymous identifiers and evidence references.
It rejects a batch without publication permission or with unretained real
originals. All unrecognised input keys are discarded, including nested metadata.
Errors omit input values. Wells, cards, events, reviewers and evidence use
separate HMAC namespaces; references remain traceable within private custody.

The HMAC construction is specified by [RFC 2104, section 2 (1997)](https://www.rfc-editor.org/rfc/rfc2104#section-2),
using the standard library's HMAC-SHA256 implementation. This is pseudonymisation,
not guaranteed anonymity: geometry, dimensions, review dates and linked repeats
can still identify a well indirectly. Review all output before any release.

[RFC 2104, section 3 (1997)](https://www.rfc-editor.org/rfc/rfc2104#section-3)
discourages keys shorter than the hash output length; intake enforces the
SHA-256 output length of 32 bytes as the minimum key length.

`summary()` counts real cards, distinct real wells, repeat wells, complete real
pairs, eligible cards/wells, and eligible healthy wells. It reports synthetic and
plate cards separately. Repeat counts have no invented monthly cutoff. Exact
position/load duplicates are rejected even under different well or card IDs.
The duplicate guard is batch-local; custodians must deduplicate across batches.
A summary always says `NOT_EVALUATED`; no study sample size is inferred.

## Benchmark selection

`card_ids()` returns pseudonymous selection identifiers even for an
analysis-only collection. Call `benchmark_set(selected_ids)` with an explicit
selection of one eligible card per well. Eligibility requires a real, recorded
pump card, a confirmed independent label, analysis permission and retained
originals. Synthetic, plate, surface, unconfirmed and repeated-well selections
are rejected. Analysis-only data does not thereby acquire publication rights.

Pass the returned `LabelledSet` to the existing runner:

```python
dataset = prepared.benchmark_set(selected_ids)
result = BenchmarkRunner(
    real_set=dataset,
    minimum_count=study_minimum_per_class,
    confidence=study_confidence,
).run()
real_result = result.sections["real_card_accuracy"]
```

`BenchmarkRunner` is imported from the dynacard `benchmark` package. The study
must supply and justify `study_minimum_per_class` and `study_confidence`.
The existing runner reports both numerical ML and legacy heuristic counts,
per-class accuracy and Wilson intervals. No outside model service is needed.

The existing interval equation comes from the
[NIST e-Handbook, section 7.2.4.1, Wilson method](https://www.itl.nist.gov/div898/handbook/prc/section2/prc241.htm).
For successes `k`, count `n`, `p = k/n`, and the study's normal quantile `z`,
the centre and radius are:

```text
centre = (p + z²/(2n)) / (1 + z²/n)
radius = z sqrt(p(1-p)/n + z²/(4n²)) / (1 + z²/n)
```

No new physical equation, empirical coefficient or adequacy threshold is added.
One card per well prevents within-batch repeated strokes from increasing sample
counts; it does not establish independence across wells. The study must exclude
training wells, account for shared operating conditions and operators, predeclare
selection, and retain healthy cards. The benchmark cannot authenticate label
truth, permissions, retention or training/test separation.

## Worked example and checks

Run `PYTHONPATH=src python examples/domains/artificial_lift/labelled_card_intake.py`
in an environment with the package dependencies. Inputs are a unit-square loop,
a second loop with twice its height, and a third with three times its height,
plus explicit illustrative context. These inputs are chosen examples, with no
engineering acceptance meaning. The method parses numeric CSV, projects all
records, counts collection evidence and attempts benchmark selection.

The answer is three projected synthetic cards, zero real cards, zero eligible
wells, `NOT_EVALUATED` field accuracy, and a rejected benchmark selection. The
example deliberately cannot masquerade as a collected real set.

`test_intake.py` also exercises the existing runner using two in-memory records
asserted real for the test only. One has label `NORMAL`, the other
`FLUID_POUND`; a constant `NORMAL` predictor gets one out of two correct. The
test's hand calculation uses `z = 1`: for one success out of one, the interval is
`[0.5, 1]`; for no successes out of one it is `[0, 0.5]`. The threshold of one is
a chosen test input, not a recommended acceptance limit. Neither this predictor
nor these fixtures measures classifier field performance.

## Remaining collection work

No real cards have been received by this change. Acceptance of #1865 still
requires authorised originals, independent labels and a confirmed healthy
baseline; a study sufficient for the requested confidence; running the real
benchmark. Permissions and independent evidence cannot be replaced by generated
examples. Contributor contact and delivery records belong in private custody.
