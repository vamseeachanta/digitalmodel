# Plan for [#2281](https://github.com/vamseeachanta/digitalmodel/issues/2281): CP review-report portfolio

> **Status:** implementation authorized by the owner's 2026-10-03 instruction to proceed with constructive work; critical decisions will receive agent review first
> **Complexity:** T2
> **Date:** 2026-10-03
> **Client:** N/A — public artifacts will contain de-identified module evidence only
> **Lane:** lane:codex
> **Parent:** [#1834](https://github.com/vamseeachanta/digitalmodel/issues/1834)
> **Benchmark work:** [#1852](https://github.com/vamseeachanta/digitalmodel/issues/1852)
> **Review artifacts:** `scripts/review/results/issue-2281/`

## Objective and authority

The implementation will make a complete, discoverable CP review portfolio part of
the digitalmodel repository. It will cover both supported structure/mode regression
cases and private benchmark cases A–E. It will treat “final for review” as a complete
review package, never as engineering approval, issue authorization, or a PASS result.

The owner will perform a comprehensive visual/UI/print review later. The portfolio
will record that review as deferred, not passed. The implementation will preserve
the proposed reporting standard's review status and will not adopt it globally.
It will not modify numerical kernels, the shared reporting engine, or its templates.
The agent will not apply `status:plan-approved`. The owner's subsequent instruction
will authorize constructive implementation within this reviewed scope without
repeated permission requests; critical engineering or private-data-release decisions
will receive agent review before presentation to the owner.

## Resource Intelligence Summary

The baseline will be `bf638ed1` from origin/main, with CP provenance anchored to ABS
merge `b3002c85420e629c17950bd714d2015e3eeb2ab0`.

| Source to be reused | Concrete capability or constraint |
|---|---|
| `src/digitalmodel/cathodic_protection/engine_adapter.py` | Execution will use the route registry's nine keys and distinguish deprecated aliases from structure modes. |
| `src/digitalmodel/cathodic_protection/report_adapters.py` | Generation will reuse seven anode-design builders; unsupported deprecated B401/F103 builders will remain excluded. |
| `docs/domains/reporting/standard-report-engine.md` | Packs will use the existing input/report contract, citation sidecar and report manifest. |
| `tests/fixtures/cathodic_protection/workflow_inputs/` | Nine YAML inputs will seed the supported structure inventory. |
| `tests/cathodic_protection/test_b401_zone_families.py` | A dedicated mixed seawater/buried/concrete fixture will derive from the existing family regression, with explicit area/environment basis. |
| `tests/cathodic_protection/test_b401_structures_phases.py` | A dedicated temporary-service fixture will derive from the exercised phase mode. |
| `docs/domains/cathodic_protection/_index.md` and `standards-inventory.md` | Report eligibility, provisional values and pending evidence will remain visible. |
| `docs/plans/2026-09-29-issue-2259-abs-ships-rebuild.md` | Ships reports will preserve the owner-accepted interpretations and required project current-density input. |
| [Benchmark issue](https://github.com/vamseeachanta/digitalmodel/issues/1852) | A–E reports will link to existing benchmark evidence without claiming completion of the first-real-job validation condition. |
| workspace-hub proposed conventions at `c955b86159acc4f9e9f5964690c24dd1411e5b48` | Each report will apply the ten-section structure, comment workflow and status distinctions to this requested portfolio only. |

Before execution, resource verification will resolve the existing licensed citation
pages against the configured knowledge checkout. It will not retrieve new standards
or copy licensed clause prose. Private benchmark inventory and source-rights checks
will run on the private analysis host; no drive-wide source search will be needed
for this bounded, already-identified archive. An absent reference will block the
affected row rather than trigger an unauthorized standards download.

Gaps to be filled will include two reproducible fixtures, a structure/benchmark
coverage manifest, per-case commentable report packs, and a repo-owned review index.
An independent review report will record exact file/route evidence; code changes
will not begin on the strength of a missing or stale plan-status marker.

## Coverage contract

The initial matrix will contain eleven structure/mode rows and five benchmark rows.
Each row will retain its own identifier even where several rows share a route.

| Row | Structure/mode | Input basis | Eligibility to be retained |
|---|---|---|---|
| S01 | Jacket | `jacket.yml` | EOR check; assessed 55-anode regression will remain FAIL |
| S02 | Monopile | `monopile.yml` | EOR check; failure will not be hidden |
| S03 | Subsea manifold | `manifold.yml` | EOR check; failure will not be hidden |
| S04 | Ship hull | `ships.yml` | ABS accepted interpretations and EOR check |
| S05 | Floating offshore structure | `fpso.yml` | ABS offshore legacy/uncited, independent check required |
| S06 | Bracelet pipeline | `pipeline.yml` | F103 edition and coating-compatibility limits |
| S07 | Terminal-bank flowline | `anode_bank.yml` | Engineering validation required |
| S08 | Multi-component riser | `multi_component_riser.yml` | Allocation/continuity checks; unevaluated compatibility |
| S09 | Riser base/foundation/mudmat/hatch; wet storage/operation/retrofit | `riser_base_phased.yml` | Per-component and per-phase results, mass ledger and shortfall disposition |
| S10 | Temporary service | New fixture from existing phase regression | Explicit duration and phase assumptions |
| S11 | Concrete reinforcement/mattress plus buried and seawater families | New fixture from existing family regression | Reinforcement area and independent electrolyte/anode-family checks |
| A | Hybrid-riser benchmark | Private inventory and comparison evidence | Actual route applicability will be verified |
| B | Temporary-riser benchmark | Private inventory and comparison evidence | F103:2003 numerical acceptance will remain unqualified unless separately evidenced |
| C | Benchmark C | Identity/mapping will be resolved from private inventory | No assumed structure or manufactured expected result |
| D | Mattress benchmark | Private inventory and comparison evidence | Reinforcement/sediment basis will be explicit |
| E | Benchmark E | Identity/mapping will be resolved from private inventory | No assumed structure or manufactured expected result |

The implementation will enumerate S09 subcomponents and phases in a child manifest
so a single combined HTML cannot conceal missing foundation, mudmat, hatch, wet-storage
or retrofit coverage. Each child will link to its own stable row/anchor and matching
input/result keys, including every phase-by-family mass/output and retrofit disposition.
A link to the shared phase section alone will not satisfy child coverage. If the
supplied fixture omits a required child, that child will need a new
reproducible fixture and report under this plan before coverage can be complete.

F103:2010 and supported ABS legacy aliases will be recorded as compatibility variants,
not silently counted as new structures. Deprecated B401/F103 legacy keys without
builders will be explicit exclusions. Experimental ICCP, stray-current and galvanic
models and survey/depletion assessment will be listed as outside this structure-report
portfolio, with rationale and current eligibility; they will not be called complete
CP designs. New solver capability will require a separate issue/plan.

## Artifact map and destination

| Artifact | Proposed repository path |
|---|---|
| Owner roadmap | `docs/domains/cathodic_protection/reporting-roadmap.html` |
| Review landing page | `docs/domains/cathodic_protection/reports/index.html` |
| Coverage and child manifest | `docs/domains/cathodic_protection/reports/coverage.json` |
| Each report pack | `docs/domains/cathodic_protection/reports/cases/<row>/` |
| Input/output/provenance | Each pack: `input.yml`, `results.json`, citations and manifest JSON |
| Human review artifacts | Each pack: `report.html`, `report.comments.json`, checksums and change record |
| Benchmark comparisons | `docs/domains/cathodic_protection/reports/benchmarks/<letter>/` |
| Generation entry point | `scripts/reporting/build_cp_review_portfolio.py` |
| Portable review postprocessor | `scripts/reporting/cp_review_document.py` |
| Contract tests | `tests/reporting/test_cp_review_portfolio.py` |
| Added fixtures | Existing CP workflow-input directory, scoped to S10/S11 and missing S09 children |
| Plan index/changelog | `docs/plans/README.md`, `CHANGELOG.md` Unreleased |

The old jacket draft will migrate only after de-identification and provenance checks;
its preserved source numerical output will be compared with a fresh pinned run.
Any engineering difference will be disclosed and blocked from silent replacement.
Private originals, identifying filenames, document numbers, machine paths, PDF
watermarks and client-specific input echoes will not enter Git, issues or PRs.

For each benchmark, the private host will preserve originals and a confidential
mapping. Only a rights-cleared, de-identified derivative and its comparison evidence
will move into digitalmodel. Unknown rights or insufficient de-identification will
leave that benchmark blocked. A placeholder/exclusion will not count as a completed
benchmark report; the owner will decide any revised publication boundary.

The release record will allowlist fields and values before generation: case letter,
generic structure/mode, cited standard identifiers, evidence class, qualified
comparison metrics, limitations and specifically approved numerical inputs/results.
Every other field will default to private. Exact geometry, dimensions, area/mass
vectors, service history, document metadata, plot payloads, comments and source-file
hashes will be treated as potentially identifying or confidential even without names.
The public pack will not contain private source hashes or confidential mappings.
Its checksums will bind only the released derivative files.

Unapproved numerical vectors will remain private. A synthetic public counterpart
will have an explicitly documented transformation and independent calculation; it
will be labelled synthetic and will not claim to reproduce the confidential design.
A private-to-public mapping and any actual-design comparison will stay on the private
host unless their specific fields receive owner release. Each generated HTML, input
echo, JSON, plot, citation note, manifest, sidecar and build log will be checked against
the release allowlist before staging. A separate privacy review will check design
fingerprints and metadata, in addition to automated identifier/number/path scans.

Each pack will set explicit module document control, date/revision and a non-empty
revision history with purpose “Internal review draft”; reviewer/approver decisions
will start pending. It will not rely on DocumentMeta's default Issued history or
the adapter's placeholder document. M1 will establish a repo-internal
review identity register with a valid JOB-DOCTYPE-SEQ-REV number, revision and nonempty
neutral project/client fields for each pack under the owner's constructive-work
authorization. These identities will not represent a client job or issuance.
No existing CP document register will
be assumed. Missing register entries will block generation, not be passed as empty
values to DocumentMeta or replaced by invented client/job identities. The register
will be versioned at `docs/domains/cathodic_protection/reports/document-register.json`.
Tests will inspect issuance
and approval fields and rendered labels, allowing honest “not issued” caveats while
rejecting automatic issued/approved claims in all report artifacts.

## Execution milestones

1. M1 will freeze coverage, source permissions and the benchmark A–E mapping. It will
   prepare negative contract tests and migrate the jacket report as a bounded pilot.
2. M2 will generate all eleven supported structure/mode packs and S09 child coverage,
   using one registry and the existing calculators/report builders.
3. M3 will reproduce and compare A–E privately, produce allowed repo derivatives,
   and record applicability, units, tolerances and unresolved differences. It will
   reuse existing benchmark issue evidence rather than close that issue prematurely.
4. M4 will generate the index, run portfolio QA and adversarial review, reconcile
   outstanding comments, and publish a review-ready commit/PR. It will explicitly
   retain the owner's deferred comprehensive visual review and all EOR requirements.

## Pseudocode

```text
load approved case registry and pinned source revision
validate unique IDs, required rows and S09 children
for row in registry:
    verify source class, rights and de-identification before materialization
    validate input or mark blocked with reason (never substitute defaults silently)
    run existing route and report adapter; capture raw numerical outputs
    apply portfolio-only presentation/review layer; retain FAIL and use_status
    write pack; bind JSON to exact saved HTML; record input/output/file digests
    verify numerical/citation/link/portable-asset and comment-round contracts
build index only from verified pack receipts, with blocked/deferred states explicit
fail portfolio completion if any required row/child lacks its complete review pack
```

## TDD test list

Tests will be written before the pack builder or presentation postprocessor.

| Test | Required behavior |
|---|---|
| Missing/duplicate row or S09 child | Build/completeness check will fail closed |
| Missing input, citation or rights record | Row will block without emitting misleading report-ready status |
| Jacket source fidelity | Six tables and plotted arrays will match route output; 55 FAIL/173 unassessed distinction will remain |
| Route eligibility | PASS will not upgrade legacy/experimental/validation-required status |
| Source de-identification | Identifier, absolute-path and secret scans will reject unsafe derivative artifacts |
| Numerical/metadata privacy | A forbidden private vector, plot datum, source hash or metadata field will fail the public release allowlist check before staging |
| Document control | Every pack will supply review-only revision history and pending signoffs; no default Issued/approval field will survive in HTML/manifest/sidecar |
| Exact report binding | Changed HTML or mismatched revision/digest JSON will be rejected; no silent decision reset |
| Comment round-trip | Save/Load/Copy guards, conflicting decisions and repeat import deduplication will be exercised |
| Portability and print fallback | No remote script/style assets; linked evidence and static plot data will resolve |
| Benchmark comparisons | Units, denominator, comparator class, tolerance basis and source-rights decision will be explicit |
| Deterministic rebuild | Fixed inputs/revisions will reproduce results and receipts; runtime metadata will not create silent drift |
| Coverage index | Ready/blocked/deferred counts will derive from receipts, not filenames or folder presence |

## Acceptance criteria and validation

- All sixteen required rows and S09 children will have complete repo-owned review
  packs, valid references, reproducible sources and checksums, or the goal will remain
  incomplete. An approved scope revision alone will remove a required row.
- Results will show standard/edition, route/mode, calculated PASS/FAIL, use_status,
  limitations and independent reviewer status separately. No final-for-review label
  will imply an issued report or EOR approval.
- Scoped CP/specialized-CP/reporting tests will pass; ruff and mypy will cover touched
  Python files. Legal, identifier, numeric-text, absolute-path and diff checks will
  cover the exact added artifact set, including HTML/JSON sidecars and plan files.
- Local browser policy will not be bypassed. Desktop/mobile/print and actual file
  controls will be checked only through permitted mechanisms; the owner's deferred
  comprehensive review will remain an explicit review-stage gate in every index.
- A review-ready portfolio will require passing automated fidelity/traceability and
  adversarial checks; owner/EOR acceptance and comprehensive visual checks may remain
  pending only with the recorded owner deferral. Failed checks will never become PASS.
- The PR will contain the final coverage and evidence; task worktrees and scratch will
  receive a scoped cleanup audit before handoff. Publication/merge will require the
  applicable authority; this plan will not transfer the old Claude merge permission.

## Risks and boundaries

The main risks will be accidental disclosure through input echoes, overclaiming a
PASS calculation as approval, missing composite/phase coverage, stale draft provenance,
and coupling presentation changes to a numerical correction. Source rights and private
case mapping will stay fail-closed. Large self-contained HTML packs will be measured
before commit; asset reuse will not sacrifice portable/offline behavior. Any shared
engine change, new model, client issuance or unapproved publication will return to
the owner as a separate decision. The first-real-job benchmark condition will remain
open independently of this portfolio.
