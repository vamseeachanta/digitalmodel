# #1602 corrected-contract review — release and monitoring

**Date:** 2026-07-18  
**Reviewed base commit:** `e1004ad3` plus the then-current working-tree correction  
**Verdict:** MAJOR

1. `normalized_route_handoff_expansion.always_equalities` was not composed
   into the completion/workover not-applicable handoff, so its request could
   change `wave_case_sha256` after normalization.
2. The new analysis-contract and request-envelope commitments stopped at
   their producer envelopes and were not carried through bundle, execution,
   receipt, and engineering result.

Release-set derivation, HF/site/closeout preservation, monitoring exclusion,
and approval wording otherwise passed review.
