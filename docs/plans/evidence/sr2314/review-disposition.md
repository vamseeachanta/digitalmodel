# R02 plan review disposition

Issue: [2239](https://github.com/vamseeachanta/digitalmodel/issues/2239).
Originating authority: explicit sr2314 user dispatch authorizes bounded implementation,
branch publication and one draft PR; no merge is authorized.

Claude reviewed two frozen plan revisions and returned CHANGES-REQUIRED. The main
Codex session applies the final delta inline under the review-routing contract;
no Claude approval or consensus is claimed. Gemini authentication was re-probed
and returned `invalid_grant`, an authentication-layer failure, not a network outage.

| Finding | Disposition and evidence |
| --- | --- |
| Source threshold rejects generated slivers | Source 1e-12 diagonal-squared criterion is retained. Generated triangles receive an additional local floating-point-resolution check, not that source criterion. Small resolved cuts and translated cuts are exercised. |
| Area uncertainty under translation | The local check detects computed collapse, repeated indices and non-finite areas. It does not establish exact collinearity of pre-rounding geometry. No integration consumer divides by triangle area or normalizes its normal; vector areas are summed directly. This limited guard is sufficient for the reported missing physical-face check, with no stronger geometric qualification claimed. Artificial signed fans remain exempt. |
| Cache key and tolerance | Exact validated float keys, calculation-local lifetime, fixed midship keep side, one epsilon for bounds and clipping. One-ulp-distinct stations remain distinct; cached segments have immutable backing. |
| Reference equivalence | Test-only full-body cut path supplies an independent selection/compaction comparator. Asymmetric trimmed hull sections and endpoints agree within 1e-10 relative/1e-9 m² absolute; segment endpoints within 1e-9 m. Existing box and Wigley tolerances remain unchanged. |
| Split confounds behavioral changes | Move-only commit `5bd2cc99` preserves the numerical implementation and original public class module names; existing 62 tests passed. Subsequent changes add indexing/guard/helpers. Baseline box digests and old pickles generated from 4f7bfc0c are retained as analytic fixtures. |
| Performance acceptance | Absolute criteria v1 precede baseline/current runs. The comparison guard (time ratio <=1.10; RSS ratio <=1.05) was set after exploratory measurement and is a no-regression guard, not a preregistered performance hypothesis. Both benchmarks use one fresh Linux process and the same analytic 1,009,998-face Wigley hull with freeboard and ten stations. RSS includes generation and diagnostics; timed region is validation plus hydrostatics. Structural tests require one evaluation per distinct station and compact candidate vertices. No universal speedup is claimed. |
| Citation compatibility/dependency | Public imports and TransferResult type are preserved. cite=True was already the default; silently empty citations now raise. The configured production wiki target is absent in the inspected sibling checkout; its owner must provision it. cite=False remains explicit and records omission. Getter references official ITTC 2021 revision 05, sections 2.3/2.4.1, and labels the simplified scope in its note. No private wiki change is authorized or included. |
| Holtrop scope | Discrepancies are characterized, not resolved as accuracy defects. Tests validate decomposition and matched operating points, not pinned unqualified coefficients. Issue [2020](https://github.com/vamseeachanta/digitalmodel/issues/2020) retains source-acquisition/qualification scope. |
| Gate order/evidence | RED mesh regressions: 4 failed before split; revised 4 failed/2 passed before optimization. Citation track: 4 failed before implementation. Diagnostic script: 2 failed; benchmark script: 1 failed before implementation. Exact final checks and frozen code receipt will be retained. Parallel authoring in distinct files does not establish reviewer approval. |

Inline Codex plan assessment: the implementation will remain within findings 5/7/9/10
and the requested diagnostic reproduction. A sorted spatial accelerator, arbitrary
mesh self-intersection qualification and production wiki publication remain outside
scope. Default citation failures are an intentional fail-closed change, disclosed
as a production dependency. T2 artifact review will cover final source, callers,
tests and criteria; changed bytes void that packet's verdict.

## Artifact r1 dispositions

The frozen artifact-r1 packet was verified unchanged before corrections. Claude
found no numerical defect in station compaction/cache logic. The following delta
addresses its actionable findings; acceptance will bind to a rebuilt packet.

| Finding | Disposition |
| --- | --- |
| M1 omitted scale records | Both exact JSON records and HTML interpretation will enter the final packet, including commands, pinned dependencies, engine digests and baseline source revision. |
| M2 omitted pickle fixture | The final packet includes the fixture. Tests check its literal SHA-256 before deserialization. The generator requires the exact baseline source digest and NumPy version, clears inherited Git bindings, and writes only the owning fixture path. |
| M3 citation dependency/callers | Both transfer docstrings state that the production wiki page is unprovisioned, with owner action tracked on issue 2239. `git grep -n 'transfer_model_to_ship\|transfer_ship_to_model' -- '*.py' '*.md' '*.html'` inventories tracked source, scripts, docs, examples and tests. Only the friction module and its focused test file contain matches; new untracked regression tests are also inspected. No additional tracked production caller is found. Tests either supply the synthetic citation root, explicitly opt out, or exercise expected failures. |
| D1 wildcard exports | Fluid, Reynolds/Froude helpers and transfer constants are restored and wildcard identities tested. |
| D2 inconsistent constructor defaults | These defaults predate this task. Constructor compatibility is retained; the unresolved note no longer attributes an opt-out to every constructed result. The legacy status name is documented as result-local unresolved metadata. Transfer functions explicitly set their correct resolved/opt-out fields. |
| D3 exception/frontmatter behavior | Actual API raises CitationResolutionError for missing/unconfigured dependencies. The plan is corrected; environment isolation and wrong-revision tests cover both directions. Fixture presence is asserted before the negative test removes its disposable copy. |
| D4 criteria drift/counting | Benchmark verdict reads CRITERIA. Compare mode reads the frozen ratio limits and rejects mismatched dependencies, inputs or criteria. Ten requested grid stations and eleven distinct/evaluated sections are reported separately; live section calls are instrumented. |
| D5 legacy loading | Original source imports hull_fixtures by absolute package name and loads successfully. Its raw digest matches git show 4f7bfc0c. The retained records state that revision; no extra checkout or history rewrite is needed. |
| D6 selection coverage | Fine Wigley and disjoint twin boxes with trim and a non-representable large x-offset now compare indexed areas against full-body cuts, including midship and endpoints. Existing concave/tandem/twin cases remain unchanged. |
| D7 guard claim | Docstring names a computed-collapse guard and explicitly disclaims exact pre-rounding collinearity. Near-plane omission of repeated-index polygon triangles is inherited, not introduced. The source remains refuse-only; the clipping polygon triangulator omits triangles with fewer than three distinct vertices as a representation operation. |
| D8 positional Holtrop labels | Named IDs select both hulls. A reordered-input test checks the named ratio; JSON carries the coefficient normalization. No current coefficient is promoted to a qualified oracle. |
| Minor input order | Allowances are validated before citation lookup, with three fail-first regressions. ca_model is explicitly a caller extension in assumptions. |
| Minor platform | The benchmark imports on systems without resource; RSS is unavailable and qualification is false rather than crashing. |
| Minor bounds caching/cohesion | The existing bounds copy remains immutable and is outside the per-station loop. Further caching and relocating station validation are optional refactors and are deferred. |
| Private facade names | No tracked caller reaches the old private helper names through the facade. Declared public imports and original pickle names remain supported. |

Inline Codex code assessment checks geometry tagging, candidate-local edge pairing,
shared tolerance, finite validated stations, immutable cached segments, literal
input identities and default citation behavior. No unresolved calculation defect
is identified in the authorized scope. The production citation dependency and
limited computed-collapse criterion remain explicit limitations.


## Artifact r2 disposition and inline r3 scope

The second packet is verified unchanged (34 files) before acting on the verdict.
Claude returns CHANGES-REQUIRED for evidence/test defects, with no proven numerical
production defect. Final corrections are reviewed inline by Codex under the
cross-review routing contract; a third provider dispatch is not performed.

- The large benchmark uses off-row draft and off-grid stations, compare all
  result quantities and expose analytical midship area. Cache reuse is established
  by the dedicated repeated-station test, not by a benchmark with unique stations.
- Generator isolation receives a disposable decoy-repository test and a named,
  timestamped negative-probe record with hostile bindings and exact outputs.
- Both NumPy environments receive focused tests with resolved versions recorded.
- Fine-section equivalence adds off-grid stations and representable near-vertex
  offsets; compact-selection regression now cuts at 20.3 m rather than a vertex column.
- A fail-first wildcard test records the literal baseline public namespace. The
  compatibility facade restores incidental imports as well as intended API names.
- Guardrail operators are aligned to the repository maximum (≤400 lines/module,
  ≤50 lines/function). Generated-area cap exclusion receives an exact face-count test.
- The disclosure verdict is issued only by the final digest-bound receipt.
- Generic source-quality versus generated-triangle validation is promoted to
  [issue 2342](https://github.com/vamseeachanta/digitalmodel/issues/2342); no unrelated
  implementation is implied by that follow-on audit.

Inline r3 acceptance is conditional on actual output comparison, passing focused
regressions and a committed-byte hash match. It does not establish physical hull
accuracy, primary-paper Holtrop ranges or provision the owner-managed wiki page.


Final inline r3 verification: 977 passed/5 skipped/1 xfailed in the full naval
suite; 224 passed in the NumPy 2.4.4 focused suite. Both million-face records have
identical all-quantity digests and meet recorded bounds with off-row clipping and
off-grid sections. Resolved dual-environment versions and isolated generator probe
are retained. Production source digests in the benchmark are re-read and match.
The final packet/receipt will bind these dispositions to the committed bytes;
Claude's two CHANGES-REQUIRED verdicts remain visible and are not relabeled APPROVE.
