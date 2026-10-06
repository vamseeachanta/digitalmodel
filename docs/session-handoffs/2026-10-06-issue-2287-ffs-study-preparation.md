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

Execution: orchestration and numerical diagnostic both ran on ACMA-WS014.
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
