# #1602 corrected-contract review — solver and engineering

**Date:** 2026-07-18  
**Reviewed base commit:** `e1004ad3` plus the then-current working-tree correction  
**Verdict:** MAJOR

1. Complete analysis-contract and selected-request commitments were not
   propagated downstream, allowing request fields such as the request-matrix
   hash to change without changing downstream lineage.
2. The normalized not-applicable route did not normatively bind its wave-case
   identity into the selected request.

The envelope commitment count, symbolic references, DAG, hydrodynamic
kind-specific schemas, domain/convention gates, and release membership checks
otherwise passed.
