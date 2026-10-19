# Dynacard diagnostics: methods and evidence

## Summary

The numerical classifier identifies patterns in downhole dynamometer cards.
Its archived cross-validation result measures synthetic generator data.
**Field diagnostic accuracy is Not Evaluated:** no independently labelled real
test corpus is included. Classifier confidence is not an operational accuracy
claim. Report downhole cards and geometric metrics while diagnostic outcomes
remain unconfirmed.

## Revision history

| Revision | Date | What changed and why |
|---|---|---|
| Initial | 2026-10-19 | Replaced the missing brochure with an evidence-qualified description and a reproducible benchmark route. |

## What the implementation supports

The benchmark compares the shipped numerical ML classifier with independent
legacy threshold rules. A supplied labelled corpus retains label provenance,
explicit units, exclusions, paired predictions, sample counts and intervals.
The [benchmark analysis and worked example](../domains/artificial_lift/dynacard-benchmark.md)
describe the method and the limits on interpreting its results.

The shipped model's `training_metadata.cv_accuracy` is available in
`src/digitalmodel/marine_ops/artificial_lift/dynacard/data/dynacard_classifier.json`.
The [training pipeline](../../src/digitalmodel/marine_ops/artificial_lift/dynacard/training.py)
documents that this is cross-validation on synthetic cards. It is not a
measurement on independently labelled field cards, and its value changes when
the model is retrained. No field accuracy percentage is advertised here.

## Published precedent

*Pattern Recognition for Downhole Dynamometer Card in Oil Rod Pump System
using Artificial Neural Networks*, ICEIS 2009, pages 351–355,
[publisher full text](https://www.scitepress.org/papers/2009/20004/pdf/index.html),
describes artificial-card training and a separate human-labelled field test.
Section 4 describes eight classes with 300 artificial cards per class in each
training and validation set. Sections 4–5 report a total classification error
of **1.3 %**, rounded to one decimal place from the paper, on **6,101** real
cards. The error discussion includes unclassified cards as well as wrong
classifications. These are results for the paper's network and evaluation
protocol; they are not evidence of this implementation's field performance.

That paper demonstrates a synthetic-trained, real-tested approach. This
implementation still requires the real-scoring evidence before its diagnostic
accuracy can be stated. Source text was read on 2026-10-19.
