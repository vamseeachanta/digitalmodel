# #1602 host/motion plan review — Claude Fable 5

**Date:** 2026-07-18  
**Reviewed commit:** `44e6b0becafb6aba7aa4b6735946d08a53382890`  
**Verdict:** MAJOR

This is the persisted defect summary from the actual `claude -p --model fable`
provider review. The reviewed revision is not approval-ready.

## MAJOR findings

1. The retained base obligation `exact_13_config_schemas` conflicted with the
   v3 exact 17-config release while the root declared no explicit removal.
2. The root's effective authority silently omitted the base-owned
   `global_coordinates` surface.

## MINOR findings

1. `wave_case_sha256` was not anchored and propagated through the complete
   normalized-case-to-result chain.
2. `analysis_contract`, both analysis requests, and `deterministic_bundle`
   lacked complete-envelope commitment rules.
3. The legacy base component identity was ambiguous because its filename and
   in-file version differed and it had no component ID.
4. The human-facing ownership table omitted source repair, website, and parent
   closeout; the assurance owner list also omitted source repair.
5. Release membership could be derived from all eligible producer envelopes
   instead of exactly the envelope references used by released engineering
   results, permitting missing or orphan members.
6. The long-form hydrodynamic value contract incorrectly imposed heading and
   imaginary dimensions on matrix and restoring rows.
7. Root keys named `approved_plan` and `approved_design` overstated the current
   approval scope.

## Required disposition

All findings will require correction and a fresh adversarial provider review.
This MAJOR verdict cannot support `status:plan-review` or user approval.
