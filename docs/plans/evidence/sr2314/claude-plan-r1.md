# Plan review: `docs/plans/2026-10-10-sr2314-r02.html`

**Verdict: MAJOR — REQUEST CHANGES.** The review covers the packet bytes only (sha256 `ae3e5917…fdc9`, HEAD `4f7bfc0c`). This session has no filesystem or shell tools, so no claim about existing code, thresholds or tests was checked against the repository.

`★ Insight ─────────────────────────────────────`
- "Refuse, don't delete" is the right rule for a degenerate *source* mesh, but clipping manufactures slivers from valid input. The same rule applied after clipping turns a data-quality gate into a draft-dependent availability failure.
- An interval index (`min ≤ x ≤ max`) and the section routine each need a tolerance. If they differ, the index silently changes results at vertex-aligned stations, which is where midship and endpoint stations usually sit.
`─────────────────────────────────────────────────`

## Major findings

| # | Location | Defect | Required change |
|---|---|---|---|
| M1 | `:8`, `:21` | A clipping plane passing near a vertex produces triangles of arbitrarily small area from a valid mesh. With refusal after clipping, a valid hull is rejected at particular drafts or trims, and the chance of at least one sub-threshold sliver grows with face count. | Separate source-face degeneracy (refuse) from clip-generated slivers (snap/merge with an area-conservation check). State the disposition of faces collapsed to zero area by snapping. Add a draft-sweep test through vertex z-values. |
| M2 | `:8`, `:21` | The "existing relative threshold" has no stated value. Scaling by `source_diagonal**2` on a slender hull lets length set the scale; at 10⁶ faces the mean face area may approach the threshold. | State the threshold value and the ratio of mean and minimum face area to the threshold for the million-face case. |
| M3 | `:3`, `:12`, `:24` | The plan claims to close findings 5, 7, 9 and 10, yet the Holtrop item is reproduction only, with correction and primary-source acquisition excluded. Reproducing a discrepancy does not close it. | Add a finding-to-step-to-test table. Record the Holtrop finding as not established, naming the missing evidence (primary-source coefficients), unless R02 asked only for reproduction. |
| M4 | `:10` | Expected outcomes for configured, missing and unconfigured dependencies are not given, nor is the default of the citation flag. If it defaults to enabled, fail-closed breaks existing callers without the dependency, contradicting the compatibility claim at `:5`. The registry's owning repository and the ITTC procedure number and revision are unnamed. | Add a truth table (dependency state × direction × opt-out → result or exception type), the default, the registry location and the procedure identifier. A registry change in a sibling repository falls outside the single draft PR. |
| M5 | `:12`, `:24` | Acceptance carries no numeric criteria: no Wigley tolerance, no wall-time or memory bound, and no pre-change baseline. "Measured, not extrapolated" records a number without a comparator. The method for demonstrating "repeated section evaluation" and "full-vertex copies" (`:7`) is unstated. | Declare versioned criteria before the run: evaluation count per unique station = 1, a peak-memory bound, a Wigley tolerance tied to an expected convergence order, and a baseline at `4f7bfc0c`. Measure peak memory in a fresh subprocess, since the high-water mark in a shared pytest process includes earlier tests. State where the evidence is retained. |
| M6 | `:9`, `:13`–`:19` | The cache key is unspecified. Raw-float keys miss on stations differing by 1 ulp; quantized keys return a section at a different x. The candidate filter at `:16` has no tolerance. No test compares indexed output against the un-indexed path. | Define the key. Add an indexed-versus-brute-force test on a non-analytic mesh at stations on vertex x-values, endpoints and ±tolerance. Explain the `midship` constructor argument. |
| M7 | `:5`, `:11`, `:24` | The six-module split lands with three behavior changes, so a numerical difference cannot be attributed. "Unchanged input hashes" is checkable only against pinned values, and relocating classes changes `__module__`, which may feed hashes or pickles. | Make the split a move-only commit with re-export shims, verified green before behavior commits. Pin literal hashes captured at `4f7bfc0c`. Add a pickle or serialization round-trip from the old import path. |

## Minor findings

- **m1 (`:9`, `:19`)** — Cached areas and segments are shared across midship, feature and grid outputs. No test is listed for aliasing between outputs, although commit `6e37b83d` protects public arrays.
- **m2 (`:22`)** — "Independent volumes" is undefined. The behavior of the signed fan on multi-loop waterplanes (catamaran, moonpool) is unstated; either refuse or test it.
- **m3 (`:7`, `:11`)** — "Touched modules" is not enumerated. A line-limit test needs an explicit file list and a rule for the re-export shim.
- **m4 (`:16`)** — Precomputed ranges remove recomputation, but the per-station scan remains O(faces). State whether that satisfies finding 7 or whether a sorted structure is required.
- **m5 (`:24`)** — "Reviews will attack…" is listed as risk handling. Each named risk needs a test or design control in the plan.
- **m6 (whole file)** — The plan names no files, test paths or commands, and does not appear to follow `docs/plans/_template-issue-plan.md`. The split, new refusal semantics and citation fail-closed are substantial scope.
- **m7 (`:12`)** — The plan says "a small script", while the worktree holds untracked `tests/naval_architecture/test_holtrop_reproduction.py`. A test asserting current discrepancies pins approximate values as passing; a report-only script or non-asserting diagnostic avoids that.

## Not established by this packet

- **Authority for the draft PR (`:3`).** The "originating job authorization" is asserted, not evidenced. Publication is a consequential action and its approval needs verification before the push.
- **Content of findings 5, 7, 9 and 10.** The packet does not quote them, so the mapping to steps 1–6 is inferred.
- **The two untracked test files and the third in the worktree.** They are outside the packet and unreviewed; a verdict on this plan does not extend to them.
- **Template conformance (m6).** The template was not read.

## Next checkpoint

Revise the plan for M1–M7 and rebuild the packet; the re-review should cover the delta plus the clipping/threshold and citation-default interfaces. M1 and M4 are design decisions that change the tests to be written, so they need resolving before the existing untracked regressions are extended.
