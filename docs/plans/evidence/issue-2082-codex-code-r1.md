# OrcaFlex smoke code review — Codex r1

Issue: [digitalmodel #2082](https://github.com/vamseeachanta/digitalmodel/issues/2082)

Date: 2026-09-10. Verdict: **MINOR**, no blocking functional probe defect found. Native acceptance remains pending; this review did not import the native API or acquire a licence.

## Findings and disposition

1. **MINOR, repaired during review — incomplete provenance could pass.** The original native harness `hashes()` filtered absent input files, and its success fixture provided only the CLI. A successful proof could therefore omit the probe/workflow hashes. The current harness checks every declared source exists before launch (`issue-2082-native.ps1:174`), and the fixture now provides all three files, asserts the exact input hash keys, and includes missing-input refusal cases. Source inspection verifies the repair; the orchestrator owns execution of these new regressions.
2. **MINOR, open wording/contract discrepancy — timeout is a child wait budget.** The plan promises a 90-second harness deadline. `run_owned()` waits up to `TimeoutSeconds`, while revision discovery can additionally take 10 seconds and process cleanup another 5 seconds, plus setup/hashing overhead. This remains bounded for the intended small local artifacts but is not a strict 90-second total wall-clock deadline. Clarify the plan/proof wording or implement a shared remaining-time budget before claiming that stronger guarantee.

## Review evidence

- Inspected finite validation of every history sample, minimum two samples, completion state, explicit requested threads and observed thread counts, independent `.sim` load, nonempty artifact, expected-line lookup, readback completion/history/count checks, and `finally` reference release. No success path bypass was identified within the agreed contract.
- Read the installed OrcaFlex 11.6 Python API source without executing it. `Model(filename=None, threadCount=None, ...)`, `threadCount`, `simulationComplete`, `ModelState.SimulationStopped`, and `LoadSimulation(filename)` match the implementation; supported filename inputs include `PathLike`.
- Reviewed Windows suspended process creation, assignment to a kill-on-close Job Object before resume, owned handle cleanup, strict JSON parsing, retained private diagnostics, fresh output directory refusal, and fake descendant/outsider tests. No process ownership defect was identified.
- Reviewed workflow/CLI tests and CI registration assertions. Existing code preserves the report-before-failure contract and scratch ownership. Native acceptance and actual CI execution are separate orchestrator gates, not certified here.
- Independence disclosure: this agent authored the original fake probe contract tests. Production implementation, additional workflow/CI/harness tests, and native harness were reviewed adversarially here; this is not an independent authorship review of the original test doubles.

## Exact scope

The review began before implementation commit `025474005ebb728584ffc892a00f3d45451acf44`. The provenance repair was observed at `681635702d7b0155bbe969c92846d97ce180b5e9`. SHA-256 snapshots below identify the reviewed content, including unchanged workflow/CLI dependencies.

| File | SHA-256 |
|---|---|
| `.claude/quality-gates.yaml` | `2004CB1D15DE79E6E797DB8122C6AF2DB700636B394CA93CDF931E38EAF6A8F0` |
| `src/digitalmodel/solvers/smoke/probes.py` | `074C57C7608DB8162536A5DADB519C3CF9FAF3308E25121474B78623006A0856` |
| `src/digitalmodel/solvers/smoke/workflow.py` | `C4DC244B8FE32BCDB6A64494D781A77A53FBD039CDF14E062AAE3F9662CE21FA` |
| `scripts/solver_smoke_test.py` | `B9CB4A6EEE76B80479680494336397DF318EBA55FA2F6966F69A3BE64556F27C` |
| `tests/DOMAINS.md` | `A466C04997B9C8D655EE9DF1E8034C68524E9B08C4DD3564EBECCDEADE7DD0B7` |
| `docs/plans/evidence/issue-2082-native.ps1` | `864C05C561D6782022E61678748D1F2B7A6AA4510D39AAC30E2ECAF0F1638946` |
| `tests/solvers/smoke/conftest.py` | `50B01AC8EF601C278B436A45DD1F99592FE3F40BE52C6269B9BEC2B78C378186` |
| `tests/solvers/smoke/native_harness_child.py` | `2FD12871C659F42C5452590890B16BC7E5AA6F8C944E8C417E0FE171D259434E` |
| `tests/solvers/smoke/test_ci_registration.py` | `8E43BBD2609D8869EBBB36BDD89F966952EB2FBEA25CBC5DB1D66C96DEFB342B` |
| `tests/solvers/smoke/test_native_harness.py` | `573127377B2728D5FD8B4AF2F5FCB7A3E7EBC0B2196CD8FDBFA0F4DFEEA7A24F` |
| `tests/solvers/smoke/test_orcaflex_probe_contract.py` | `D6A9C66256EA723104DFCF66BE32AE59FAEA17F281102CCF6D65449A1DF9DD69` |
| `tests/solvers/smoke/test_workflow_contract.py` | `80A35531F59DFD8369C70869C19FA143A1B207A216DF0E59379E2542CA87F1C9` |

Residue: EXPECTED — this review artifact only, for orchestrator disposition and commit. No source/test edits, native jobs, or commits were performed by this review lane.
