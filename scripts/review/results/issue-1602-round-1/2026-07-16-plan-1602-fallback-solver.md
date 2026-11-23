## Verdict

MAJOR

## Retrieval

- Reviewed the exact plan at commit
  `106763a8da6eec7755095da048509c07950db77b`.
- Verified cited digitalmodel code at
  `87d56cac637f971ca3ed57d8ca98c16a845ff0f7`.
- Used the stale local Deckhand checkout only to identify integration risks; it
  was not treated as verification of the cited remote SHA.

## Findings

1. **MAJOR:** “Map every canonical field to `ProjectInputSpec`” is impossible;
   the plan needs an exhaustive direct/transformed/metadata/execution/result/
   rejected field-disposition table.
2. **MAJOR:** Repeat-byte equality can mark byte-identical invalid YAML as
   `MODEL_VERIFIED`; zero validator errors and real semantic load are required.
3. **MAJOR:** The bundle hash omits transitive inputs, clean staging, archive
   rules, stale files, and solver-loaded/saved model identity.
4. **MAJOR:** Coordinate frames/sign conventions are deferred until after D1
   case identities depend on them.
5. **MAJOR:** No assigned secure multi-file input/output transport exists.
6. **MAJOR:** Licensed-host attestation lacks signer trust, freshness, canary
   identity, production-run binding, and private machine-to-public-role mapping.
7. **MAJOR:** `result_sha256` contradicts timestamp exclusions and lacks
   deterministic row keys/order.
8. **MAJOR:** Strength/operability requirements allow implementation-selected
   “relevant” outputs and “where modeled” omissions.
9. **MAJOR:** The criteria inventory omits DNV-OS-F201, DNV-RP-C203, API STD 2RD,
   and AMJIG and does not prohibit the fail-open `riser_citations()` wrapper.
10. **MAJOR:** Independent plausibility checks have no oracle families,
    tolerances, sampling rule, or coverage requirement.
11. **MAJOR:** E2/E3 sequencing is cyclic and basic host readiness is delayed too
    late.
12. **MINOR:** Viewer evidence is not bound to the immutable HF revision.

## Blockers

- Findings 1–11 must be resolved before approval.
