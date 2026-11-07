# Executable YAML semantic-diff comparison policy

## Summary

The `semantic_diff` package implements a bounded, synthetic subset of the
[semantic-diff taxonomy](SEMANTIC_DIFF_TAXONOMY.md) for #517. It compares parsed
mappings offline and retains all differences. A policy match does not establish
physical equivalence, solver loadability, or reference validity. YAML equality
alone cannot establish those claims.

## Revision history

| Revision | Date | What changed and why |
|---|---|---|
| Initial | 2026-10-26 | Added a bounded executable policy to separate explicit omissions from accidental losses. |

## Categories and examples

The following inputs are synthetic, not vendor defaults or measurements. The
view examples reuse the display values in the taxonomy's cosmetic examples.
Depth inputs use the arbitrary round number `100`; the changed value follows
the independent hand calculation `100 + 1 = 101`. The normalization pair
`100` and `100.0` denotes exactly the same number. `1` in the boolean/type test
is the integer unit, distinct from the boolean `True`. Nonfinite test inputs
are mathematical infinity and not-a-number, not physical values.

| Category label | Definition | Synthetic example |
|---|---|---|
| `physics-significant` | Potentially changes physical inputs or is unknown to this policy. | `Environment.WaterDepth`: `100` to `101`. |
| `loadability-significant` | Known scalar type or section shape changes pose a loading risk. | `Environment.WaterDepth`: `100` to `"deep"`. |
| `reference-resolution-significant` | A registered reference mapping changes or disappears. | `Groups.Structure`: `Child: Parent` to `Child: Absent`. |
| `normalization-only` | A registered scalar changes representation with exactly equal normalized values. | `Environment.WaterDepth`: `100` to `100.0`; `MultipleCurrentDataCanBeDefined`: `"No"` to `False`. |
| `ui-cosmetic-only` | A registered display value changes while present and of the expected scalar kind. | `General.DefaultViewAzimuth`: `270` to `0`. |
| `intentional-omission` | A valid source value disappears under an explicit, directional entry with a reason. | `General.DefaultViewSize`: `400` to `MISSING`. |

The labels map respectively to the existing taxonomy's physics, loadability,
reference resolution, normalization, cosmetic and omission categories. A
significant label describes a comparison risk; it is not a measured result or
proof that the solver rejects the data.

## Decision rules

- Identical, type-preserving values produce no diff. Calling `classify_diff`
  directly on identical values raises `ValueError`.
- Whole-section additions, losses and shape changes remain significant. Groups
  section changes of this kind are reference risks; other sections are loading
  risks. A section cannot disappear through property-level omission rules.
- The registry matches the section and complete property key exactly. Unknown
  changes are `physics-significant`, including equal-looking numeric type
  changes on unknown keys. No wildcard or property-name heuristic is used.
- Registered reference changes remain significant before any normalization or
  omission rule. `Groups.Structure` is compared atomically; no reference graph
  is resolved, repaired or inferred.
- Missing values are represented by `MISSING`, distinct from explicit null
  (`None`). Only source-present to generated-missing changes with an explicit
  `omission_reason` qualify as intentional omissions. The source must satisfy
  the registered scalar kind. Additions and nulls do not qualify.
- Registered numeric scalars accept finite floats and integers, excluding
  booleans. Int/float normalization requires exact equality; there is no numeric
  tolerance, string-to-number coercion, whitespace trimming or case folding.
  Registered boolean scalars accept native booleans and exact `"Yes"`/`"No"`
  spellings. Changed normalized values remain significant.
- Cosmetic classification applies only to registered display values. A changed
  depth, missing group reference or unregistered view-like key is never cosmetic.

`REGISTRY` is an immutable per-section table of `Rule` entries. Numeric
normalization is enabled only by `scalar_kind="number"`; boolean normalization
only by `scalar_kind="boolean"`. Extending these existing rule forms requires a
reviewed registry entry and direct evidence tests, without changing classifier
control flow. New context-dependent rules require separate implementation.

## Executable mapping example

Run from the repository root in an environment containing the ordinary package
import dependencies:

```sh
PYTHONPATH=src python -m digitalmodel.solvers.orcaflex.semantic_diff.example
```

The module uses only synthetic in-memory mappings; it reads no model file and
imports no licensed solver. It prints `policy_match=True` with categorized
normalization, cosmetic and explicit omission differences. The corresponding
executable test is `test_offline_mapping_example` in
`tests/solvers/orcaflex/semantic_diff/test_taxonomy.py`.

`is_semantically_equivalent` returns `(policy_match, categorized_diffs)`. Its
name preserves the planned API; `True` means only that no changed value blocks
this bounded policy. All accepted differences remain available to callers,
including the reason for each intentional omission. Identical invalid models
can also return `True`: this is not a schema or reference validator. Mapping
key order is ignored, including group-definition ordering; list order and
nested value types are preserved. Loading order is outside the comparison.

## Compatibility evidence and limits

The plan's proposed package was absent at the inspected base. Existing code
uses `scripts/semantic_validate.py`: `values_equal` applies tolerance and boolean
normalization, while `_downgrade_allowed_diffs` uses `ALLOWED_DIFF_PROPS` without
section scope. The round-trip file's `TestSemanticExclusionList` checks that
legacy behavior, including a broad `Structure` allowance. Those assertions stay
intact; an added compatibility test demonstrates that the new scoped registry
keeps a changed group reference significant. The legacy script is not migrated
in this change.

`generic_builder.py` has `_SKIP_GENERAL_KEYS`, including view state and a
conditional `ImplicitVariableMaxTimeStep` exclusion. The latter depends on
solver settings and is deliberately absent from this context-free registry.
No dormant solver property or whole Groups omission is automatically accepted.
Only `DefaultViewSize` has an omission entry, as a synthetic example rather than
a general generator-policy claim.

TODO for #518: integrate the standalone taxonomy into regression comparison
consumers with model-specific evidence. #519 remains responsible for fidelity
gaps; broader model-library migration remains open. This change does not raise
any model's claim level under the
[existing claim boundary](SEMANTIC_EQUIVALENCE_CLAIM_BOUNDARY.md).
