# #1602 final-candidate review — solver and engineering

**Date:** 2026-07-18  
**Reviewed commit:** `b7a23ffe`  
**Verdict:** MAJOR

The long-form row contract required the hydrodynamic-response envelope hash
for its exact metadata join, but omitted that hash from every kind-specific
key. Two response revisions sharing an ID and ordinates could therefore
collide. Every kind key must include the envelope hash and a collision RED
fixture must enforce it.

Solver neutrality, effective commitment coverage, lineage, route semantics,
DAG ownership, and release membership otherwise passed.
