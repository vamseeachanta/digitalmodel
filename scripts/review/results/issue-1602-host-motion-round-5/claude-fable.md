# #1602 lineage-corrected plan review — Claude Fable 5

**Date:** 2026-07-18  
**Reviewed commit:** `e1004ad3`  
**Verdict:** MAJOR

The actual `claude -p --model fable` review verified all six manifest hashes,
the 17-config surface, chain-field consistency, DAG ownership/acyclicity,
monitoring exclusion, explicit 13-to-17-config supersession, and the hard
no-implementation gate. It then found one MAJOR and five MINOR defects.

## MAJOR

The root keyed handoff equality merging only by `from`, `to`, and condition,
while the base and extension intentionally contain several conjunctive
requirements for the same route. A literal implementation could reject the
composition or retain only the first record, dropping wave-case or release-set
propagation. The merge contract must preserve and conjoin every distinct
constraint while deduplicating only exact records.

## MINOR

1. Child issues #1609 and #1611 lacked an explicit owner crosswalk in the
   approval surface.
2. Base acceptance obligations used role keys while extensions used issue-URL
   keys without a machine-readable crosswalk.
3. The values-table metadata join required a response-envelope hash that the
   row contract did not explicitly require.
4. Base commitment-rule wording could be read as excluding v3-added effective
   bindings from the signed/hashed preimage.
5. The plan lacked an explicit disclosure-safe disposition for repo-relative
   resource-intelligence metadata while forbidding private paths.

This verdict is not approval evidence. Every finding requires correction and
fresh review on an immutable pushed revision.
