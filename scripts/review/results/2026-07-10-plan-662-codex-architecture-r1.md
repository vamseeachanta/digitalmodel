## Verdict

MAJOR

## Retrieval

- Reviewed the revision-1 plan against `origin/main` at `529c4ba1`.
- Inspected Gmsh modules, OpenFOAM runner and benchmark, provisioning, CI, tests,
  and live issue state for #153, #640, #662, #1433, and #1495.
- Re-ran the cited focused baseline: 32 tests passed.
- Found no related open Gmsh bridge PR. No files were edited by the reviewer.

## Findings

1. **MAJOR - boundary semantics:** the plan checked patch names but not patch types,
   complete face coverage, or non-empty `defaultFaces`; the VOF case requires real
   `wall` patches.
2. **MAJOR - transactionality:** conversion wrote directly to the destination and
   could attest a stale or partially written `polyMesh` after a retry.
3. **MAJOR - scope ownership:** #662 mixed bridge, production-like geometry, generic
   quality, provisioning, CI, dispatch, and MPI work without a contract against #153,
   #640, #1495, and #155.
4. **MAJOR - locked provisioning:** the plan called the install locked while current
   provisioning uses `uv pip install`; no exact frozen command or idempotence/import
   test was specified.
5. **MAJOR - MPI contract:** no test fixed rank/core validation, exact argv, stage
   failure propagation, no-oversubscribe behavior, or a measurable idle threshold.
6. **MAJOR - CI viability:** the existing Gmsh workflow points at stale paths; adding
   another workflow would leave canonical CI broken. No actionlint or fresh-install
   proof was required.
7. **MINOR - circular golden:** comparing a refactored benchmark to the writer it now
   imports would not preserve an independent pre-refactor oracle.

## Blockers

- Define exact patch/type/default-face behavior and transactional promotion.
- Reconcile issue ownership and cross-links.
- Specify frozen provisioning and executable MPI/CI tests.
- Capture an independent characterization golden before refactoring.
- Obtain usable re-review evidence before the user approval gate.
