# Subprocess review harvest for [#523](https://github.com/vamseeachanta/digitalmodel/issues/523)

Recorded 2026-11-13. The filename follows the 2026-05-05
[harvest plan](../plans/2026-05-05-issue-523-harvest-517-subprocess-review.md).

## Source and recovery

This is a summary reconstructed from the issue body, not a verbatim subprocess
transcript. The original review was not recovered: the referenced temporary
artifact is unavailable, and no prior version of this harvest document was
found in repository history. Plan step 1 explicitly permits reconstruction from
the three recommendations in
[#523](https://github.com/vamseeachanta/digitalmodel/issues/523).
That issue has zero comments at the time of inspection; no later approved
revision is present there.

The source issue also mentions a taxonomy-document candidate and open questions,
but does not reproduce their full contents. Those details cannot be reconstructed
as review findings. Current repository evidence below is a separate observation;
it is not attributed to the missing review. No stronger equivalence claim is
established by this harvest.

## Recommendation routing

| Recommendation | Captured recommendation as summarized in the issue | Existing destination and plan task |
|---|---|---|
| Rec 1 | Create a shared taxonomy/classification Python module. | [#517](https://github.com/vamseeachanta/digitalmodel/issues/517), [517 task 3](../plans/2026-05-05-issue-517-semantic-diff-taxonomy.md): shared taxonomy module. |
| Rec 2 | Normalize Bool ↔ Yes/No in semantic comparison. | [#519](https://github.com/vamseeachanta/digitalmodel/issues/519), [519 task 2](../plans/2026-05-05-issue-519-general-environment-groups-fidelity.md): Environment normalization classification. |
| Rec 3 | Refactor comparison logic to use a structured registry instead of ad hoc sets. | [#517](https://github.com/vamseeachanta/digitalmodel/issues/517), [517 task 3](../plans/2026-05-05-issue-517-semantic-diff-taxonomy.md): structured registry; [#519](https://github.com/vamseeachanta/digitalmodel/issues/519), [519 task 3](../plans/2026-05-05-issue-519-general-environment-groups-fidelity.md): named General view-key policy. |

Table 1. All three recommendations have existing destinations; no new issue is required.

[#518](https://github.com/vamseeachanta/digitalmodel/issues/518) is the regression
consumer of these recommendations through
[518 tasks 2–4](../plans/2026-05-05-issue-518-model-library-regression-tests.md).
[#520](https://github.com/vamseeachanta/digitalmodel/issues/520) retains reverse
extraction scope; none of these three recommendations creates a separate task
there. [#522](https://github.com/vamseeachanta/digitalmodel/issues/522) remains
the sibling subprocess-pattern task.

## Current repository evidence

The inspected base is `b84092701c225a07cbc9113b21f990445c4922b8`.
The historical plan task numbers above are routing references, not a statement
that every proposed implementation or claim has been completed.

- **Rec 1:** [taxonomy.py](../../src/digitalmodel/solvers/orcaflex/semantic_diff/taxonomy.py)
  exports `DiffCategory`, `classify_diff`, and `is_semantically_equivalent`;
  [taxonomy tests](../../tests/solvers/orcaflex/semantic_diff/test_taxonomy.py)
  exercise the bounded policy.
- **Rec 2:** [semantic_validate.py](../../scripts/semantic_validate.py)
  normalizes boolean and exact Yes/No representations at `values_equal`;
  [boolean normalization tests](../../tests/solvers/orcaflex/test_values_equal_bool_normalization.py)
  retain wrong-polarity and non-Yes/No checks. No YAML dumper change is needed
  for this harvest.
- **Rec 3:** `taxonomy.py` contains an immutable per-section `REGISTRY`.
  [Comparison policy](../domains/orcaflex/SEMANTIC_DIFF_COMPARISON_POLICY.md)
  explicitly leaves the legacy `ALLOWED_DIFF_PROPS` consumer unmigrated.
  The existing General omission constant is `_SKIP_GENERAL_KEYS` in
  [generic_builder.py](../../src/digitalmodel/solvers/orcaflex/modular_generator/builders/generic_builder.py),
  rather than the historically proposed `_GENERAL_VIEW_SKIP_KEYS` name.
  [Reconciliation tests](../../tests/solvers/orcaflex/test_skip_list_reconciliation.py)
  connect these omissions to the documented taxonomy.

The registry is a synthetic, context-free comparison subset. Its presence does
not establish universal semantic equivalence or a completed consumer migration.
Regression integration remains routed to
[#518](https://github.com/vamseeachanta/digitalmodel/issues/518), and fidelity
gaps remain with [#519](https://github.com/vamseeachanta/digitalmodel/issues/519).
Reverse-extraction findings on
[#517](https://github.com/vamseeachanta/digitalmodel/issues/517) and
[#520](https://github.com/vamseeachanta/digitalmodel/issues/520) are separate from
the three harvested recommendations.

## Publication status and comment drafts

At the time of recording (2026-11-13), these drafts had not been posted;
plan steps 3–5 remained open on #523. These drafts preserve the requested
payloads; their relative document links require a reachable blob permalink
at the merge commit before posting.

No change to the [#515 plan](https://github.com/vamseeachanta/digitalmodel/issues/515)
is proposed. Routing stays with the existing children. The retained May plan's
`plan-review` descriptions are historical, not current status assertions.

### Draft for [#517](https://github.com/vamseeachanta/digitalmodel/issues/517)

Subprocess review from the semantic-equivalence program, harvested via
[#523](https://github.com/vamseeachanta/digitalmodel/issues/523):

- Rec 1: shared taxonomy/classification module → 517 task 3.
- Rec 3: structured registry rather than ad hoc sets → 517 task 3.
- [Retained reconstruction and mapping](2026-05-05-issue-523-harvest-source.md).
  `semantic_diff/taxonomy.py` and its `REGISTRY` now exist; this is a bounded
  synthetic policy, not a claim of complete model-library migration.

### Draft for [#518](https://github.com/vamseeachanta/digitalmodel/issues/518)

Subprocess review harvested via
[#523](https://github.com/vamseeachanta/digitalmodel/issues/523):

- Rec 1: consume the shared taxonomy module in 518 task 2.
- Rec 2: assert allowed Bool ↔ Yes/No normalization in 518 task 3.
- Rec 3: encode per-case expected differences as registry data in 518 task 3.
- [Retained reconstruction and mapping](2026-05-05-issue-523-harvest-source.md).
  These are regression-consumer mappings, not additional independent issues
  or evidence that every proposed regression case has passed.

### Draft for [#519](https://github.com/vamseeachanta/digitalmodel/issues/519)

Subprocess review harvested via
[#523](https://github.com/vamseeachanta/digitalmodel/issues/523):

- Rec 2: Bool ↔ Yes/No normalization → 519 task 2. The existing compare-site
  implementation and boolean regression tests provide current evidence.
- Rec 3: explicit General view-key policy → 519 task 3. Current code uses
  `_SKIP_GENERAL_KEYS`; the proposed `_GENERAL_VIEW_SKIP_KEYS` spelling is
  historical. A named skip constant alone does not migrate the legacy comparator.
- [Retained reconstruction and mapping](2026-05-05-issue-523-harvest-source.md).
  This routing does not resolve outstanding fidelity or licensed-validation work.

### Draft for [#515](https://github.com/vamseeachanta/digitalmodel/issues/515)

Harvest summary via [#523](https://github.com/vamseeachanta/digitalmodel/issues/523):

- Rec 1: shared taxonomy/classification module → 517 task 3.
- Rec 2: Bool ↔ Yes/No comparison normalization → 519 task 2.
- Rec 3: structured comparison registry → 517 task 3 and 519 task 3.
- 518 tasks 2–4 remain the regression-consumer home.
- [Retained reconstruction and mapping](2026-05-05-issue-523-harvest-source.md).

No 515 plan changes; routing is into the existing children described as
`plan-review` in the historical harvest plan. All three recommendations already
have a home, so no new issue is required. The original review was unavailable;
the retained summary is reconstructed from the issue, not quoted from the review.

### Draft for [#523](https://github.com/vamseeachanta/digitalmodel/issues/523)

Local harvest for [#523](https://github.com/vamseeachanta/digitalmodel/issues/523):

- Rec 1 → 517 task 3; Rec 2 → 519 task 2; Rec 3 → 517 task 3 and 519 task 3.
- [Retained reconstruction and mapping](2026-05-05-issue-523-harvest-source.md)
  records source limitations, current code evidence, and destination drafts.
- No new issue is required and no production code or 515 plan is changed.
- At the time of recording (2026-11-13), publication and readback remained
  pending; the retained drafts alone did not satisfy the publication criteria.
