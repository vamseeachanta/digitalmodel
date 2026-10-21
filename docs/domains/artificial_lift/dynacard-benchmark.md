# Dynacard program benchmarking analysis

## Summary

The benchmark can score an authorised, independently labelled pump-card corpus
through the shipped numerical ML classifier and the legacy threshold rules.
No independently labelled field corpus is included in this change. Field
accuracy remains **Not Evaluated**. Synthetic generator labels and diagnostic
plate shapes do not establish field performance.

## Revision history

| Revision | Date | What changed and why |
|---|---|---|
| Initial | 2026-10-19 | Added overall scores, an offline command and a worked example to make the missing field evidence explicit. |

## Evidence available

The production `benchmark.test_set_builder` module, provenance-aware loader and
independent classifier paths already exist. The vision benchmark tests collect
without a missing-module fallback. The vision stub is a synthetic comparison;
it is not a field-scoring method.

The retained field-card captures in the test fixtures have reference downhole
curves for solver checks but no independently confirmed diagnostic labels.
The [diagnostic plate fixture](../../../tests/marine_ops/artificial_lift/dynacard/testdata/mendeley_plate/README.md)
has normalised axes and does not establish measured-field status. Neither is
promoted to a labelled real-card test set. Acquisition and label provenance,
rights to use the data, and engineering units remain required evidence.

The corpus acquisition route remains tracked in #1872. An attempted read of
the linked open paper returned a challenge page; the publisher page denied
access. No dataset counts or terms are inferred from those responses.

## Run the offline benchmark

The command does not train, invoke a vision service, or download a corpus.
It writes JSON to standard output; choose a destination outside the repository
for local data and results.

```sh
PYTHONPATH=src python -m digitalmodel.marine_ops.artificial_lift.dynacard.benchmark \
  --minimum-count 4 --confidence 0.95
```

The command above supplies no corpus: `sections.real_card_accuracy.status`
is `Not Evaluated`. The count threshold and interval coverage are illustrative
caller inputs, not engineering acceptance limits or a sample-size recommendation.
To score an authorised corpus, add `--real-set labelled-pump-cards.json`.

The input uses the existing labelled-set schema:

| Field | Required content |
|---|---|
| `card_space` | `pump`; invert surface cards before loading |
| `units` | `position: in`, `load: lbf`; no implicit conversion |
| `crosswalk` | Rows with `external_label`, current `mode` or explicit null, and `reason` |
| `cards` | Ordered paired `position` and `load` samples, external `label`, and `provenance` |
| `provenance` | `source`, `labeller`, ISO `date`, `licence`, `kind: real`, and `acquisition` |
| Digitised graphics | `acquisition: digitised_graphic` also requires `publication`, `figure_or_page`, and `digitising_method` |

Recorded cards use `acquisition: recorded`. A label is evidence only when its
source supports that diagnosis; the loader cannot authenticate declarations.
Unmapped labels are counted and excluded from the accuracy denominator. Check
their frequency before interpreting an apparent score. Synthetic or plate
cards mapped into the real-card section raise an error.

The output includes the following:

- `input_sha256` identifies the exact supplied JSON bytes.
- `classifier_artifact.sha256` identifies the exact model bytes used for
  numerical inference, independently of any cached classifier.
- `sections.real_card_accuracy.methods.ml.overall` and the corresponding
  `heuristic.overall` contain counts and, at the supplied threshold, accuracy
  and a Wilson interval.
- `per_class` contains counts and estimates subject to the same threshold.
  `confusion_matrix` retains abstentions and unexpected prediction columns.
- `predictions` pairs each expected label with both method outputs, its
  external label, and its index in the retained provenance list.
- Recorded and digitised acquisitions are counted separately. Synthetic
  cross-validation, solver round trips and plate distances remain distinct
  sections, each `Not Evaluated` when its inputs were not supplied.

An empty set produces no numerical estimate. A nonempty set below the supplied
threshold reports `too thin to score` with counts and no accuracy or interval.
The overall estimate can be available while every individual class is too thin;
this does not establish performance on those classes.

## Method and worked example

Let `C[i,j]` count cards labelled `i` and predicted as `j`. Then
`n = sum(C[i,j])`, `k = sum(C[i,i])`, and accuracy `p = k/n`.
This is the unweighted classification accuracy definition in the
[scikit-learn model evaluation guide, Accuracy score](https://scikit-learn.org/stable/modules/model_evaluation.html#accuracy-score)
(accessed 2026-10-19). Unmapped input labels are outside `n`.

For the requested two-sided coverage, let `z` be the corresponding standard
normal quantile. The
[NIST/SEMATECH e-Handbook, section 7.2.4.1](https://www.itl.nist.gov/div898/handbook/prc/section2/prc241.htm)
(Wilson score method, accessed 2026-10-19) gives:

```text
d = 1 + z²/n
centre = (p + z²/(2n))/d
radius = z sqrt(p(1-p)/n + z²/(4n²))/d
interval = [centre - radius, centre + radius], bounded by [0, 1]
```

The [executable example](../../../examples/domains/artificial_lift/dynacard_benchmark_demo.py)
uses invented labels and predictions, not field observations:

| Card | Expected | Predicted | Correct |
|---|---|---|---|
| First | NORMAL | NORMAL | yes |
| Second | NORMAL | GAS_LOCK | no |
| Third | GAS_LOCK | GAS_LOCK | yes |
| Fourth | GAS_LOCK | NORMAL | no |

There are two correct of four: **50.0 %** accuracy. Choose `z = 2` as an
arithmetic input, with coverage computed from the normal CDF. Then `d = 2`,
`centre = 1/2`, and `radius = sqrt(1/8)`. The interval is **14.6–85.4 %**.
The four-card reporting threshold permits the overall estimate but suppresses
both class estimates, since each class has only two observations.

```sh
PYTHONPATH=src python examples/domains/artificial_lift/dynacard_benchmark_demo.py
```

Independent test docstrings derive the expected values from these counts and
the published formula. Transport fixtures that simulate a recorded input are
explicitly invented and are not a committed field corpus.

## Limits and way forward

Wilson intervals describe independent binomial observations. Repeated cards
from the same well can violate that assumption. Review acquisition groups and
use a well-separated evaluation design before interpreting a field interval.
The caller's count threshold is a reporting setting, not a power analysis.

No classifier confidence is reported by this command. For operational reports,
present downhole cards and geometric metrics; diagnostic labels remain
hypotheses pending independently confirmed outcomes. Synthetic cross-validation
metadata describes the generator distribution only.

To complete field validation, obtain a corpus with agreed reuse rights,
independent diagnostic labels, pump-card units, acquisition groups and an
explicit taxonomy crosswalk. Run both paths unchanged, retain their digests,
and report exclusions, counts, intervals and the unvalidated classes.
