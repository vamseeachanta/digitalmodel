# Dynacard classifier score on the public diagnostic plate

## Summary

The separate shape-only model matches **5/8 literal labels (62.5 %; Wilson
95 % interval 30.6–86.3 %)** and **6/10 labels including two ASSUMED crosswalks
(60.0 %; 31.3–83.2 %)**. The two ASSUMED rows alone match 1/2 (50.0 %;
9.5–90.5 %). These are consistency counts against a dependent diagnostic
plate, not field accuracy. The shipped 19-feature model matches between
0/10 and 5/10 under the four specified ASSUMED scales. The default
classification path and its model artefact are unchanged.

The new shape artefact uses 300 samples per eligible mode: 5,400 synthetic
cards, 18 labels and 16 normalised projections. Five-fold synthetic CV accuracy
is **96.185 %**. This measures separation of the generator population only.

## Revision history

| Revision | Date | Change |
| --- | --- | --- |
| 1 | 2026-11-12 | Recorded the public plate score, assumed-scale sweep and separate shape-only artefact. |
| 2 | 2026-11-12 | Corrected the interval precision description to match the recorded score. |
| 3 | 2026-11-14 | Compressed the shape artefact losslessly to satisfy the existing file-size limit; model bytes and scores are unchanged. |

Table 1. Revision record.

## Source and licence

The original [Mendeley Data version 1, DOI 10.17632/hnfwm9mjs6.1](https://data.mendeley.com/datasets/hnfwm9mjs6/1)
is retained under [CC0 1.0](https://creativecommons.org/publicdomain/zero/1.0/).
The [fixture manifest](../../../tests/marine_ops/artificial_lift/dynacard/testdata/mendeley_plate/manifest.json)
records the download URL, licence evidence, retrieval date and coordinate inventory.
The digest-pinned reader consumes the original workbook. Ten load curves are
used; thirteen power curves are excluded. Source axes are normalised, without
engineering units or established surface/pump-card identity. Pump-card shape
interpretation is **ASSUMED throughout this report**, including literal rows.

| Input | SHA-256 of retained bytes |
| --- | --- |
| Workbook, version 1 | `69723091c93269761a2fa79fc774a6ae7bf0eff786af439fb94c40171018058e` |
| Shape-only model | `4190590bd965267518f9e936ac3a315d3be9a68eac72e92b60f5b1e6bb38c94f` |
| Shipped model | `c3612e1f31aad740b4ebc5f81e34bf09ae7d21e42f1f18915c2a76c94337a40c` |

Table 2. Exact source and model identities used for the recorded calculation.

The shape artefact is stored as deterministic gzip JSON (768,441 bytes), below
the existing 1,000 KiB file-size limit. Its decompressed JSON is byte-identical
to the original 4,842,128-byte artefact; model digests identify those decompressed
bytes. Inference, scoring and training export support this storage format, and
the wheel includes the compressed artefact.

The complete [machine-readable score](../../../tests/marine_ops/artificial_lift/dynacard/testdata/plate_classifier_score.json)
contains per-card predictions, three denominator groups, confusion matrices,
Wilson intervals recorded to 12 decimal places and model training metadata.

## Method and reproduction

1. Each load-curve axis is independently min-max normalised. The loop is closed;
   repeated vertices are removed. Positive signed area is reversed to give
   clockwise traversal, matching the generators’ loaded upstroke.
2. The closed loop is interpolated at 100 uniformly spaced Euclidean arc-length
   samples in normalised coordinates, with no duplicated closing endpoint.
   Resampled extrema are normalised again; the first sampled minimum position
   in traversal order is rotated to index zero. This specifies the tie-break.
   The sampling grid begins at the first vertex of the oriented source polyline;
   cyclic shifts of source vertices can therefore leave a discretisation residual.
3. Resampled axes are scaled to the stated **ASSUMED** stroke and load range.
   Minimum load is **ASSUMED 0 lbf** for every recorded score. The shape result
   uses **ASSUMED 100 in and 6,000 lbf**; scale tests also compare all ten cards
   at 60 in/4,000 lbf and 140 in/8,000 lbf with a load offset. Magnitude is
   removed by the 16 Bezerra projections before shape classification.
4. Eight expected modes reuse the reader’s literal label crosswalk. Insufficient
   supply → FLUID_POUND and sand production → SAND_ABRASION are **ASSUMED**
   mechanisms. Neither mechanism is established by those source labels.
5. Correct/total is reported separately for literal, ASSUMED-only and combined
   groups. The existing benchmark Wilson method supplies two-sided 95 %
   intervals. Each confusion-matrix row is expected mode; columns are predictions,
   including unexpected output labels. One exemplar per class is too thin to
   establish a class-specific field rate. These intervals are descriptive under
   a binomial assumption, not evidence of independent field sampling.
6. The four shipped-model scales are fixed by the requested experiment. All
   four are reported; no scale is selected to tune or improve the headline score.
   The 80 in/12,000 lbf case is one of the prescribed comparisons; it provides
   no source dimensional evidence.

```sh
PYTHONPATH=src uv run --no-sync python -m digitalmodel.marine_ops.artificial_lift.dynacard.benchmark.plate_scoring \
  --workbook 'tests/marine_ops/artificial_lift/dynacard/testdata/mendeley_plate/Working condition diagnosis plate.xls' \
  > tests/marine_ops/artificial_lift/dynacard/testdata/plate_classifier_score.json
```

The CLI reads exact model bytes independently of process caches, performs no
network calls or training, and emits deterministic JSON without timestamps or
execution paths. Tests compare the whole record with a fresh calculation and
CLI output. The shipped artefact digest is also independently pinned in tests.

## Shape-only results

| Group | Correct / total | Accuracy (%) | Wilson 95 % interval (%) |
| --- | ---: | ---: | ---: |
| Literal crosswalk | 5/8 | 62.5 | 30.6–86.3 |
| ASSUMED crosswalk only | 1/2 | 50.0 | 9.5–90.5 |
| Including ASSUMED crosswalk | 6/10 | 60.0 | 31.3–83.2 |

Table 3. Dependent plate consistency; pump-card interpretation and physical scale are ASSUMED.

| ID | Source label | English gloss | Expected mode | Crosswalk | Predicted mode | Correct |
| --- | --- | --- | --- | --- | --- | --- |
| N | 工况正常 | Normal operation | NORMAL | LITERAL | NORMAL | yes |
| FP | 供液不足 | Insufficient supply | FLUID_POUND | ASSUMED | FLUID_POUND | yes |
| GI | 气体影响 | Gas interference | GAS_INTERFERENCE | LITERAL | GAS_INTERFERENCE | yes |
| RP | 抽油杆断脱 | Rod parting | ROD_PARTING | LITERAL | ROD_PARTING | yes |
| TU | 上碰泵 | Tagging up | PUMP_TAGGING_UP | LITERAL | VALVE_LEAK_TV | no |
| TD | 下碰泵 | Tagging down | PUMP_TAGGING_DOWN | LITERAL | VALVE_LEAK_TV | no |
| TV | 游动阀漏失 | Traveling valve leakage | VALVE_LEAK_TV | LITERAL | VALVE_LEAK_TV | yes |
| SV | 固定阀漏失 | Standing valve leakage | VALVE_LEAK_SV | LITERAL | VALVE_LEAK_SV | yes |
| PO | 活塞脱出工作筒 | Plunger out of barrel | PLUNGER_OUT_OF_BARREL | LITERAL | VALVE_LEAK_TV | no |
| SA | 出砂 | Sand production | SAND_ABRASION | ASSUMED | VALVE_LEAK_TV | no |

Table 4. One load-curve exemplar per label; the two ASSUMED mechanism mappings remain separate from literal rows.

| Expected ↓ / predicted → | N | FP (ASSUMED) | GI | RP | TU | TD | TV | SV | PO | SA (ASSUMED) |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| N | 1 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| FP (ASSUMED) | 0 | 1 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| GI | 0 | 0 | 1 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| RP | 0 | 0 | 0 | 1 | 0 | 0 | 0 | 0 | 0 | 0 |
| TU | 0 | 0 | 0 | 0 | 0 | 0 | 1 | 0 | 0 | 0 |
| TD | 0 | 0 | 0 | 0 | 0 | 0 | 1 | 0 | 0 | 0 |
| TV | 0 | 0 | 0 | 0 | 0 | 0 | 1 | 0 | 0 | 0 |
| SV | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 1 | 0 | 0 |
| PO | 0 | 0 | 0 | 0 | 0 | 0 | 1 | 0 | 0 | 0 |
| SA (ASSUMED) | 0 | 0 | 0 | 0 | 0 | 0 | 1 | 0 | 0 | 0 |

Table 5. Shape-only confusion matrix, expected × predicted; IDs follow Table 4. FP and SA mappings are ASSUMED.

Four mismatches are observed: tagging up, tagging down, plunger out of barrel
and sand production (ASSUMED abrasion) are predicted as VALVE_LEAK_TV.
The retained result is the measured 6/10, without model or scale selection.

## Shipped-model scale sweep

| ASSUMED stroke (in) | ASSUMED load range (lbf) | Literal correct / 8 | Literal Wilson 95 % (%) | Including ASSUMED correct / 10 | Including ASSUMED Wilson 95 % (%) |
| ---: | ---: | ---: | ---: | ---: | ---: |
| 60 | 4000 | 3/8 | 13.7–69.4 | 4/10 | 16.8–68.7 |
| 80 | 12000 | 4/8 | 21.5–78.5 | 5/10 | 23.7–76.3 |
| 100 | 6000 | 4/8 | 21.5–78.5 | 5/10 | 23.7–76.3 |
| 140 | 8000 | 0/8 | 0.0–32.4 | 0/10 | 0.0–27.8 |

Table 6. All four prescribed ASSUMED scales; minimum load is ASSUMED 0 lbf and card space is ASSUMED pump.

Combined correct counts span **0–5 of 10**; literal correct counts span **0–4
of 8**. At ASSUMED 140 in/8,000 lbf, every card is predicted as
TUBING_MOVEMENT. The arbitrary scale changes three raw-unit features: stroke
length, load range and enclosed card area. Source dimensional evidence is
absent, so no scale-specific shipped-model score establishes field accuracy.
The unchanged shipped model predates the current exclusions and can still
predict TUBING_MOVEMENT and PLUNGER_UNDERTRAVEL.

## Shape-only training and inference

`FeatureExtractor.extract_shape_vector` returns the 16 normalised Bezerra
projections; `shape_feature_names` records their order. The separate
`training.train_and_export_shape()` uses seeds 0–299 for each eligible label
and the existing GradientBoosting settings: 100 estimators, maximum depth 4,
learning rate 0.15, subsample 0.8 and random state 42. Cross-validation uses
five stratified, unshuffled folds. Fixed [0,1] projection bounds prevent
preprocessing from learning bounds from held-out folds. Synthetic CV remains
an upper indication of separability under these generator conventions.

Training metadata retains fold accuracies, their mean and standard deviation,
sample count per mode, feature names, hyperparameters, excluded labels and
scikit-learn version `1.3.2`. PLUNGER_UNDERTRAVEL and TUBING_MOVEMENT are excluded
by SYNTHETIC_LABEL_EXCLUSIONS because the defaults lack a distinguishing
matched-travel basis above NORMAL’s noise. Exclusion reasons are retained.

The shape artefact is regenerated with the declared training environment:

```sh
PYTHONPATH=src uv run --no-sync python -c \
  'from digitalmodel.marine_ops.artificial_lift.dynacard.training import train_and_export_shape; print(train_and_export_shape())'
```

The initial training environment uses Python 3.11.15, NumPy 1.26.4 and
scikit-learn 1.3.2. The shape-model SHA-256 in Table 2 is a regeneration target
conditional on matching this environment, generator source and exporter code;
a second full fit is not run by the tests. The generator source SHA-256 is
`7a20b3645286621b0c964faebab54315dd56833cf01c4a56ae7b217929e5f558`.
A different training environment requires new scores and metadata; byte identity
across environments is not established.

`diagnostics.classify_shape(card)` is opt-in and uses the existing JSON tree
evaluator without scikit-learn at inference. Missing artefacts raise an error.
Nonfinite, unpaired, too-short or degenerate shapes raise an error. The opt-in
function reads the artefact on each call; the benchmark reads it once per run.
Export checks train a five-sample-per-mode model through the same exporter and
require exact label agreement with scikit-learn on held-out synthetic cards and
probability agreement within 0.000051 after four-decimal reporting. These tests
do not rerun the retained 300-sample model's training or its full CV. The default
magnitude classifier and its shipped JSON remain unchanged.

## Limits

1. **n = 10 (8 literal + 2 ASSUMED crosswalk), one exemplar per class.** Wilson
   intervals are wide. This is not a field accuracy estimate.
2. **The plate is NOT independent of training.** The synthetic generators are
   held to these plate shapes by `test_card_generator_shapes.py`. The score
   is a consistency check on public labelled shapes, an upper indication at best.
3. **Card space is ASSUMED.** The source supplies normalised axes and does not
   state whether the curves are surface or pump cards. They are treated as
   pump-card shapes. Every physical scale is also ASSUMED.
4. **Field accuracy remains NOT_EVALUATED** under
   [#1864](https://github.com/vamseeachanta/digitalmodel/issues/1864) and
   [#1872](https://github.com/vamseeachanta/digitalmodel/issues/1872). Independent
   diagnostic labels, engineering units and acquisition groups are missing.
5. **Modes without a plate exemplar are unscored:** BENT_BARREL, DELAYED_TV_CLOSURE, EXCESSIVE_FRICTION, EXCESSIVE_VIBRATION, GAS_LOCK, PARAFFIN_RESTRICTION, PLUNGER_UNDERTRAVEL, STUCK_PUMP, TUBING_MOVEMENT, WORN_BARREL.
   Under literal-only scoring, FLUID_POUND and SAND_ABRASION also lack literal
   exemplars; their only plate crosswalks are ASSUMED.
6. **Shape alone cannot establish magnitude-defined conditions.** Rod parting,
   stuck pump and gas lock may depend on low load range or short stroke. Load
   range compared with expected fluid load, and an expected stroke basis, are
   outside this model. A matching outline does not establish these mechanisms.
   Model confidence describes synthetic separation, not calibrated field risk.
7. **Source phase can change a prediction.** A probe of all 485 cyclic source-vertex
   orderings across the ten load curves gives NORMAL for 22/43 normal-curve
   orderings and EXCESSIVE_VIBRATION for 21/43. For plunger out of barrel,
   7/30 orderings give PLUNGER_OUT_OF_BARREL and 23/30 give VALVE_LEAK_TV.
   Predictions remain unchanged across the other eight curves, although feature
   values vary. The largest absolute projection change is 0.143. These are repeated
   orderings of the same ten curves, not additional exemplars. The score uses the
   original workbook order and the stated first-minimum-position convention;
   exact source-phase invariance is not established. No phase is selected to
   improve the score.

Tracking: [#1874](https://github.com/vamseeachanta/digitalmodel/issues/1874).
