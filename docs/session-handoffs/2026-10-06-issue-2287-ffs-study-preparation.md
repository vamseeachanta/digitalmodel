# Uniform local-loss study preparation handoff

Issue: https://github.com/vamseeachanta/digitalmodel/issues/2287 .
Branch: `study/ffs-uniform-loss-screening`.
Base: `2a52374d401e2f445d4baf435322f2cf18346c98`.
Session: `codex-acmaws014-ffs8`.

Goal: qualified API 579-1/ASME FFS-1 2021 Part 5 Level 1/2 remaining-wall
screening versus axial length, with width slices for four representative
geometries, followed by a gated numerical Level 3 comparison.

Completed preparation: comprehensive issue/specification, existing-method and
source audit, bounded synthetic diagnostic through the canonical coordinator,
13 diagnostic tests and one navigation regression, and report/validation links
in the existing asset_integrity routing row. No production physics changed.
No private measurements, client reports or licensed originals copied.

Execution: orchestration and numerical diagnostic both ran on a Windows workstation.
784 cases, 768 UNQUALIFIED, 16 INAPPLICABLE, zero allowable-wall ordinates.
Runtime Python 3.11.15, NumPy 2.4.6, pandas 2.3.3. The runtime's optional
pyarrow extension printed a NumPy ABI mismatch; pandas numerical operations
completed and all selected checks passed. No shared dependency changes made.
Diagnostic JSON remains task-local, with source SHA-256, executing revision,
dirty status and UTC run time. SHA-256 of final precommit diagnostic JSON:
`37f75d874218d1b1df6182426471d11ba16e683f63de6be1be9c18d29978da35`.
This pre-review file is superseded by the final committed-code run receipt below.

Validation: tests written first and observed red (missing diagnostic module;
missing routing metadata); final selected suite 130 passed in 3.16 s. The
initial broader run had a pytest temporary-directory permission error; rerun
used a unique task-local basetemp and passed. Diff whitespace check passed.

Independent Codex plan review permitted characterization only. Artifact review
approved preparation only and verified result counts, null ordinates, source
hashes and private/source boundaries. Low-severity suggestions were addressed:
sound-wall normalized length added, runtime commit/dirty/time retained, diagnostic
navigation link validated. Claude returned REQUEST CHANGES for preparation:
whole-sweep/JSON tests, complete source-byte digests, source/routing limitations,
clean-revision run evidence and missing navigation diff review. Whole-sweep null
checks and strict JSON/runtime output test were added; all digitalmodel Python
source bytes now hashed; forced routing, absent sound region, candidate floor
and intact controls explicitly marked. Specification clarified applicability,
UTC dates, pressure roles, monotonicity and future Level 3 criterion gates.
Navigation and actual run were independently verified by Codex. Agy was unavailable because the
local invocation reported unauthenticated session and inaccessible runtime logs;
shutdown completed. No provider authentication, trust, config or security changes
were attempted. Neither review nor unavailable provider lanes approve engineering
qualification. Parent retains the broader cross-provider integration review.

Blockers: exact 2021 clauses/errata and permitted source-use evidence unresolved;
existing Folias formulas disagree; L1 omits extent and L2 omits width and uses
code-minimum reference wall; shallow-loss segmentation changes inferred length;
closed-end axial stress and full Part 5 applicability remain unassessed. No
source-backed production-physics correction is established. Requested allowable
curves remain unfinished; no B31G/DNV substitution or certification is issued.

Next checkpoint: source-backed edition audit, independent worked-example tests,
then supported physics corrections and qualified inversion. Level 3 plan caps
four pilot runs with explicit resource/criterion/license gates. Existing private
cylinder benchmark metadata is retained as a candidate reuse reference only;
its receipt excludes engineering acceptance. CalculiX plate-with-hole support
does not establish a nonlinear damaged-pipe solver. Prefer established Linux
workers for sustained generic computation; live remote routing/readiness was
not verified and no remote computation occurred.

Discovery: common module routing amended only in asset_integrity row. Private
wiki query_sources remains unchanged; hub report-artifact index is undeployed.
Structural owner issue 2288 has disjoint structural lane; no shared helper or
CP/Elliott edits. Elliott-thread coordination message was rejected by automatic
approval review for missing recognized explicit recipient authorization; it
was not sent and no indirect workaround used.

Cleanup: task-local diagnostic JSON/log, review output logs, two bounded pytest
temporary trees and uv cache are expected execution evidence. Isolated worktree
is preserved for draft review. Canonical digitalmodel finance change was
pre-existing and untouched. No worktree deletion or source migration occurs.
Release ASCP issue-2287 claim `ffs/uniform-loss-study` before exit.

## Continuation: bounded preliminary pressure methods

The parent authorized useful preliminary B31G/Modified B31G/RSTRENG curves
within internal-pressure hoop-controlled longitudinal blunt-loss scope. The
existing curve module gained a bounded wall screen that calls raw engines
with explicit safety factor 1/0.72 and retains applicability, sample pressures,
brackets, margins and censoring. Existing curve/lookup APIs remain unchanged.
The existing diagnostic CLI gained `--preliminary-pressure` and optional
`--plot`; API 579 outputs and asset acceptance remain null in this mode too.

Four original synthetic geometries, seven axial lengths and three methods
produce 84 cells: 42 solved model pressure thresholds and 42 lower-bound
censored cells in the review run. Width response, axial stress, bending,
torsion, external pressure and instability are not assessed. Raw engines have
historical arithmetic/table evidence, not a new full code/edition compliance
audit. Unsupported direction/load/depth bounds return INAPPLICABLE/null.

Tests were observed red for the absent helper, then passed after implementation.
Full selected regression set: 148 passed, including 26 new pressure-study tests,
the 784-case API 579 diagnostic and existing curves/raw methods/routing tests.
Strict JSON uses null with reason for original B31G infinite-length Folias,
not Infinity. Plot visually checked for axes/units/caption and censored markers;
sample connecting lines explicitly carry no qualified interpolation claim.
Independent Codex plan and code reviews approved this bounded scope; direct
raw RSTRENG re-evaluation and endpoint regression suggestions were incorporated.
Gemini CLI was unavailable because no authentication method is configured;
no trust, credential or auth settings were changed. Claude plan/code review
returned REQUEST CHANGES. Boundary rounding, runtime host removal, branch
coverage, grade/SMYS provenance, demand labeling, unsupported plot outcomes
and CLI provenance findings were addressed. Independent final Codex review
disposition found no remaining blocking findings; this is not Claude approval.
Documentation head `763892b6` reached 31 successful CI checks before pushing
the preliminary numerical continuation.

Task-local JSON/PNG, execution logs, provider-review logs and pytest temporary
trees are intentional evidence; no numeric result or private source migration.
Exact clean-run receipt and latest-head CI disposition are recorded in issue
2287 at closeout. Preserve the load/code matrix and the source-qualified API
579/Level 3 dependencies for subsequent engineering review.

## Final receipt and review disposition

Clean executing revision: `f22659abd28bbf5ad2a11184f5c19d5481c8bbf8`.
Diagnostic run UTC: `2026-10-06T01:12:36.983200+00:00`.
Working tree dirty at run: false. Final output SHA-256:
`0d1471109e40e547702975d9ad5b20eb31723df1b6ee766cf25ea2229128b1d9`.
Final integrated checks: 131 passed in 4.70 s. Full strict JSON serialization
and runtime receipt are tested. 784 cases: 768 UNQUALIFIED, 16 INAPPLICABLE;
224 have inferred-length mismatch. Raw L1 counts: ACCEPT 392, FAIL_LEVEL_1 392.
Raw L2 counts: ACCEPT 572, FAIL_LEVEL_2 212. These raw counts do not establish
normative PASS/FAIL; all 784 allowable-wall fields remain null.

Claude review B1 navigation evidence was covered by passing routing tests and
independent Codex verification. B2 receipt is above. B3 all source bytes and
explicit checkout/config scope are retained. B4 serialization and all-case
null/status checks pass. P1/P2 applicability/routing limitations, P3 pressure
roles, P4 monotonicity rule, P5 width-convention source audit, P6 proposed Annex
basis, P7 discovery roles and P8 UTC date are explicit in the revised spec.
Fixture-anchor provenance is not upgraded to normative status. Independent
Codex final disposition reviewed f22659ab and found no remaining blocking
preparation findings; qualified curves and Level 3 remain blocked.

## Continuation: source qualification investigation

The parent requested continued exact-source discovery after the preparation
PR. The existing issue claim was checked and reacquired before writes. The
write lane was extended only to qualification annotations in the two existing
FFS validation records; no production physics or catalog-conversion edits.

Exact source coverage is now recorded in the source audit: api-579-1 combines
2021 header metadata with 2007 extracts; api-579-1-asme-ffs-1 contains one
metadata page; api-std-579-asme-ffs-1 reports the 2016 original as encrypted,
metadata-only and provisional-unverified. Ledger 2016-API579-PART5 has empty
source paths/modules despite done processing status. No 2021 clause record,
errata set, source digest or permitted implementation-use evidence was located
in these surfaces. Local Linux index artifact is absent; ACE_SHARE_ROOT unset.

Strict read-only Linux SSH probe failed host-key verification; no security,
trust or credential change followed. Parent relay to the established standards
coordinator requests exact owned 2021 source and the Folias/Tc/FCA/L1/L2
applicability/profile/rerating clauses. The 2016 source-owner parser failure is
not bypassed, and historical fragments are not substituted for 2021.

Existing-code arithmetic verified lambda 1/2 helper discrepancies, and the
source audit now contains conditional correction/test candidates and exact
unverified implementation clause claims. Original validation records now
explicitly qualify the scope of their historical arithmetic goldens. Independent
Codex documentation review approved this continuation without blocking findings.
Documentation routing checks: 9 passed. Whitespace checks passed. No additional
study simulation or allowable-wall computation was performed.

PR 2289 CI observed after preparation head 5332123b: 26 successful checks and
four running, no reported failures at that observation. New documentation-only
continuation updates the PR; retain latest-head CI status separately from this
prior-head observation. Issue remains open awaiting source qualification.

## Durable preliminary results (2026-10-06)

The exact reviewed synthetic PNG/CSV/JSON are now in private digitalmodel-data,
dataset `uniform-local-loss-screening`, archive run
`preliminary-pressure-20261006T030939Z-3c238c26`. Dataset commit
`795b35099bf29cdfc59e6e2184cbdc0da76131be` and manifest SHA256
`428196dd032899e91bb823dbd46e3a09715767778a85eadd43e6168d4e65f807`
pin one canonical copy. All eight files were freshly read through the
authenticated GitHub tree/blob API and matched exact bytes, sizes and hashes.
The external publication receipt is pinned at
`2b91fcfb9a6412ae17ec640f2f0f009953df4d6f`, with committed-byte SHA256
`ba13d25485030f4dd3a3e38697467b047abcce3aab79313dc6150ee5bbf5c4d2`.
Receipt remote bytes were separately verified; checkout line endings differ
from this tracked LF receipt. Dataset payloads are protected by `data/** -text`.

Existing asset_integrity registry metadata links manifest, PNG, CSV, JSON and
receipt. Private results [draft PR 46](https://github.com/vamseeachanta/digitalmodel-data/pull/46)
records retention; source [draft PR 2289](https://github.com/vamseeachanta/digitalmodel/pull/2289)
records methods/spec/discovery. No rerun or FEA occurred in retention, no client
or licensed originals were copied, and no parallel catalog was introduced.
Engineering qualification remains false and API 579/asset acceptance remain
null. Earlier source revision `3c238c26` had 31 successful CI checks; the
new metadata routing contract has 10 passing tests.
