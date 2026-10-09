# Plan for [2089](https://github.com/vamseeachanta/digitalmodel/issues/2089): neutral GHS hydrostatics canary

> Status: PLAN REVIEW — explicit user approval will precede implementation; the live packet and native-format addendum will retain their separate gates.
> Complexity: T2 for the bounded adapter/canary; new remote dispatch or systemic isolation will require a separate scope review.
> Date: 2026-09-11
> Client: N/A — public source-neutral code and synthetic data only.
> Lane: lane:codex
> Execution mode: parallel-readonly review; single-lane implementation in an isolated worktree.
> Baseline: 90bcff859220cc473c790831a2ed9f81a603cc80
> Reviews: scripts/review/results/issue-2089/; draft authorship will not count as independent review.

## Resource Intelligence Summary

### Existing code and integration boundaries

- `src/digitalmodel/vessel_stability_screening/workflow.py::router` will remain the existing screening calculation lane. Its loading/equilibrium/GZ/criteria inputs will not be fabricated from a hydrostatics table. This canary will supply a separate hydrostatics comparison result; connecting a complete stability case will require additional reviewed input coverage.
- `src/digitalmodel/ansys/runner.py::ANSYSRunner` and diffraction `aqwa_runner.py`/`orcawave_runner.py` will supply process-status/result-contract prior art. MAPDL/AQWA-specific flags, license detection and error markers will not be reused as GHS facts.
- `src/digitalmodel/workflow_api/runner.py` and `docs/registry/workflows.yaml` will supply existing integration conventions; no new job queue, remote worker or registry service will be created. Registry activation will await demonstrated live completion.
- `src/digitalmodel/citations/schema.py::Citation` will supply the citation contract if standards-derived calculation inputs are later introduced. Existing fleet curves-of-form estimates/clamped interpolation will not serve as the independent box-geometry oracle.
- `docs/domains/ghs/api/api.md` and `Install manual.txt` will be treated as historical API notes, not verified process invocation or entitlement.

### Documents and source routing

The external Creative Systems GHS User's Reference Manual, 936-page copy with version-control checklist 12/25, SHA-256 `6b5b4b023ae53efa83c6593255996b389267e5a29fdb0d2f0cc89f33ee623634`, will anchor command semantics. A folder name or packaged executable version will not prove manual/runtime compatibility. Original manuals, machine identity and license material will remain external; no private paths or source data will enter this repository.

| Manual PDF page / section | Proposed consequence |
|---|---|
| 59–61 / Main Program 10-4 Rev H | Invocation will use an explicit executable and documented run/geometry/work/temp parameters. `/L` and `/S` will suppress default library/save loading. |
| 59 / extension handler | Preflight will inventory the exact applicable `OPEN-ext.RF` startup handler locations; a handler that could replace the reviewed run will block the canary until disposition. `/L` alone will not be assumed to suppress these handlers. |
| 161 / END Rev D | An explicit `END` will terminate an OS-launched run; EOF alone will not be treated as process completion. |
| 229–230 / HS Rev E | Plain HS arguments will mean origin depths; `/G` and `/KM` will not be substituted. |
| 357–358 / REPORT Rev J | Fresh explicit report output will be closed using `REPORT OFF`; `REPORT CLOSE` and implicit termination printing will be excluded. |
| 371–372 / RUN Rev G | Nested run/dependency search will not be allowed in the neutral canary. |
| 157 / ECHO | Echo will be treated as screen display, not proof of persistent diagnostic capture. |

The plan will not implement standards compliance. Synthetic dimensions/density/tolerances will be declared test parameters, not statutory constants. The analytical derivation below will be reviewed independently and retained with result provenance. Any later standards-derived parameter will require an applicable edition/section and a resolvable Citation sidecar before calculation; synthetic defaults will not bypass that requirement.

General concepts in the existing naval-architecture wiki will be discovery aids, not native-format evidence. Native-format qualification will use an authorized actual capture and matching vendor documentation, never guessed parser examples. The local `data/document-index/{standards-transfer-ledger,code-registry,online-resource-registry}.yaml` paths were absent in the inspected checkout; future standards scope will resolve the canonical registries before making compliance claims.

### Evidence and gaps

Resource observations on 2026-09-11 will provide bounded starting evidence:

```text
GitHub issue 2089: OPEN; lane:codex; no plan-approved label
Open PR search GHS: []
Fresh fetched origin/main: 90bcff859220cc473c790831a2ed9f81a603cc80
Existing: ansys/runner.py; vessel_stability_screening/workflow.py; workflow_api/runner.py
Existing GHS search: historical docs/domains/ghs/api only; no GHS process adapter surfaced
Repository-local issue template: absent; workspace issue-plan template will govern
```

Reproduction proofs will be N/A: this will be a new integration capability, not an alleged numerical regression. Bounded discovery will be repeated before implementation to avoid duplicating concurrent work. The drive-index query `GHS hydrostatics manual neutral canary` will supply discovery context only: its 2026-09-11T15:49:38Z response included GHS-related project-document hits, which will be excluded from the public neutral fixture. Reported freshness limitations will remain explicit: one knowledge index last refresh failed; standards/CAD/master indexes were257/77/147 days old. Returned search hits will not establish licensed runtime, native-format qualification or complete corpus coverage.

Host identity, installed executable version/hash, manual compatibility, entitlement, process completion behavior, output encoding/grammar and complete diagnostic capture will remain unresolved live evidence. No runnable or licensed host will be named by assumption.

## Deliverable and staged authorization

The issue will deliver one source-neutral path from explicit neutral input geometry through a licensed GHS run to native evidence, parsed hydrostatics and a bounded analytical comparison. Two milestones will prevent mocked process success from being mistaken for the finished capability.

### Milestone 1: test-first contract and reviewed capture preparation

After plan approval, implementation will provide closed request/result records, explicit allowlisted canary command construction, analytical oracle, subprocess seam tested with synthetic process doubles, and a reviewed capture procedure. It will not implement a guessed native parser. Process doubles will be labeled synthetic; they will not establish GHS format, license state or execution success.

The request will identify geometry/runfile hashes, units/frame/depth/density assumptions, configured executable and resource limits. It will accept no arbitrary command string, shell flag, extra argument list, nested RUN, macros, SHELL operation or user-provided executable script. The canary runfile will be generated from fixed reviewed commands only. Exact geometry-generation/units/density commands will require version-matched manual sections and independent geometry review before a runnable packet is approved.

A separate concrete capture packet will name command arguments, version/hash evidence, staged neutral files, working/temp/output residency, cleanup policy, capture method and timeout. User authorization for that packet will precede any solver launch. If diagnostic capture cannot be established, the procedure will stop before attempting to claim a valid analysis.

### Milestone 2: qualified native format, parser, and licensed completion

An authorized diagnostic capture will first preserve native report bytes, startup/completion messages, warnings/errors and actual program version. Capture completion will mean evidence acquisition only, not parsed or accepted engineering output. Print suppression and startup-hook behavior will be checked. No default library/save or unknown handler will execute.

A format-qualification addendum will then identify the exact supported version, encoding, headers, columns, units, pagination, number formatting, warning/error/completion markers and source-line mapping. It will include a legally permitted neutral golden and malformed derivatives. Independent review and approval of this concrete addendum will precede parser implementation; the parser will be developed RED first against that capture. No speculative field regex or assumed stdout grammar will be implemented beforehand.

A final authorized run or replay of the retained authorized native capture will exercise the parser and comparison. Reusing a capture will preserve its actual execution timestamp/version and will not claim a new licensed run. The issue will remain incomplete until one genuine licensed run and its independently reviewed parsed comparison are present.

## Neutral case and independent analytical oracle

The proposed case will be an ideal rectangular prismatic closed hull, length20m, beam10m, depth4m; origin will be at aft end/centreplane/baseline, positive longitudinal coordinate forward, positive transverse coordinate port, vertical coordinate upward. There will be no appendages, shell allowance, trim, heel, wave, tank free-surface correction or damage. Reference length and geometry closure will be verified in the approved native model.

The canary will evaluate origin depths T=1,2,3m at density1000kg/m3 as an explicit artificial fluid parameter. This will not claim a measured freshwater property. A geometry review will confirm that actual native stations and surfaces describe this exact box.

For L=20m, B=10m and each T, independent volume/waterplane integrals will give:

- volume V=L*B*T; mass m=rho*V/1000 tonnes;
- waterplane area A=L*B; LCB=LCF=L/2; TCB=TCF=0; KB=T/2;
- transverse BM=(L*B^3/12)/V=B^2/(12*T);
- longitudinal BM=(B*L^3/12)/V=L^2/(12*T).

Expected rows will therefore include mass200/400/600 tonnes, area200m2, KB0.5/1/1.5m, BMT25/3,25/6,25/9m and BML100/3,50/3,100/9m. Exact rational oracle arithmetic will be independent of the GHS parser and existing screening implementation.

Proposed acceptance tolerance will be abs(actual-expected)<=max(0.001*abs(expected),absolute_floor), with absolute_floor0.005m for coordinates/BM,0.01m2 for waterplane area and0.1tonne for mass. Zero transverse coordinates will use the absolute floor. These will be proposed canary engineering tolerances requiring reviewer/user acceptance, not vendor accuracy or regulatory limits. Native displayed resolution will have to support this tolerance; a coarser print setting will require a reviewed higher-precision output or an explicit scope/tolerance review, never silent widening. Requested depth values will match the approved list and units exactly after qualified normalization; duplicated or missing rows will reject.

Conclusions will say only whether the captured neutral hydrostatics agree with the independently specified ideal-box oracle under this model/version/configuration and tolerance. They will not infer equilibrium, GM/GZ criteria, damage stability, loading suitability, certification or applicability to a real vessel. HS results will not be padded with fabricated screening inputs.

## Invocation, diagnostics and fail-closed runtime contract

The reviewed argument-vector template will be:

```text
<configured-executable> /R:canary.rf /L /S /G:canary.gf /D:<isolated-work> /T:<isolated-temp>
```

This template will be version-qualified before live use. It will not be executed through a shell. Paths with spaces/Unicode will have platform tests; placeholders will be resolved privately at execution time, not committed as machine paths. The executable will not be discovered from untrusted PATH.

Preflight will verify regular explicit staged files and hashes, exclusive fresh work/output directories, no existing report target, configured executable identity and version, approved startup behavior, and authorized license-use route without reading/copying key files. Installation metadata alone will not establish entitlement. A trial/demo mode or unproven entitlement will block accepted completion.

Execution will have a proposed120-second wall timeout, one process instance and no automatic retries. Termination will affect only the process tree started for the run; failure to establish bounded termination will block live approval. Capture will bound each retained output/log at8MiB and total retained artifacts at32MiB, reject overflow and retain truthful failure metadata; the implementation will additionally prevent unbounded pipe accumulation. The approved live packet will define safe process/output handling before starting.

Completion will require more than rc=0: report closure, qualified completion/diagnostic evidence, current captured native artifacts, complete expected rows, matching assumptions and independent comparison. Unknown diagnostics, unexpected file types, nonfinite fields, unsupported versions/encoding, truncated tables, stale artifacts or incomplete capture will return an explicit failed/incomplete state and no accepted numerical conclusion. No guessed universal GHS error strings will be used.

## Review revision: exact contracts and live controls

Milestone1 will freeze schema_version=1 records. The closed prepared packet will contain case_id, geometry_sha256, runfile_sha256, runtime_profile_id, runtime_profile_sha256, argv_sha256, output_policy_sha256, units_frame, depths_m, density_kg_m3, oracle_version, oracle_sha256, tolerance_profile, command_template_version, resource_limits and packet_sha256. Paths and host identity will remain in a separate local runtime profile with its own hash; the packet will bind that hash before live approval. Packet identity will hash canonical UTF-8 JSON with sorted keys and no nonfinite numbers, excluding packet_sha256 itself. The same versioned encoder will serve preparation and launch: sorted keys, separators comma/colon without spaces, ensure_ascii=False and no NaN. Physical parameters and tolerances will use canonical decimal strings (no exponent, leading plus/zeros or negative zero; fractional trailing zeros will be removed); Python/JSON floating inputs will reject. Schema/count/limit integers will remain bounded JSON integers. Equivalent accepted textual decimals will normalize once before hashing, while persisted noncanonical packets will reject. Unknown fields and nonfinite or out-of-range values will reject. The fixed neutral profile will allow only the proposed box and depth set; changes will require a new packet.

Result records will distinguish prepared, capture_failed, capture_unqualified, parsed_unreviewed, comparison_failed and comparison_passed_unreviewed. They will contain packet/runtime/profile hashes, execution receipt reference, native artifact hashes, qualification profile reference, normalized row references, comparison evidence and failure reasons as applicable. No state will mean licensed, authorized or engineering-approved merely because a caller supplied a boolean or rc=0. License observations will remain separately recorded evidence.

### Exact-packet authorization boundary

The first runner will be a local operator-invoked CLI, not an unattended authenticated service. The trusted operator will supply an approval receipt outside the input directory, containing the reviewed packet digest, runtime-profile digest, approval reference, expiry and single-run nonce. Approval provenance will be checked by the operator against the actual session/issue authorization; this local file will not be advertised as a cryptographic identity credential. Directory permissions will separate the receipt/consumed-nonce store from untrusted inputs. The threat model will cover swapped input/packet files and accidental or replayed approvals; a malicious actor controlling the operating-system account will remain outside this local adapter's boundary and will require separate authenticated-service infrastructure.

Before launch, the runner will reconstruct canonical packet/profile digests from the final staged geometry, fixed generated runfile, resolved executable identity, exact argv, limits and output policy, and will require equality with the receipt and explicit operator-supplied approved digest. It will reject expiry, reused nonce, writable-by-untrusted-party approval storage and changed inputs/runtime/arguments. It will atomically reserve the nonce before launching and preserve consumed status on failure; no automatic retry will occur. Tests will swap each bound component after preparation and assert no process start. Code will not manufacture or self-approve an approval receipt. The operator will manually copy the displayed digests into the documented receipt JSON template after explicit approval and will record that approval reference; no agent or prepare API will issue approval. A parse/preview-only check will validate the receipt without launching a process, so transcription errors will reject safely. The concrete live packet will specify OS account/ACL and receipt inspection evidence before authorization.

### Process containment and recovery qualification

The live target will be Windows only for this slice; Linux will run pure contract/oracle/parser and process-double tests. Before live GHS approval, a Windows test will start a harmless owned parent/child sentinel under a Job Object with kill-on-close semantics. The process will start suspended, join the job before resume and deny breakaway; an incompatible nesting/job policy will block launch. Tests will verify both owned process identities and creation times disappear after timeout and that an unrelated sentinel survives. This will establish containment behavior on the actual OS, not assume it from mocked subprocess tests.

A persistent local run guard will remain unresolved after timeout, failed termination or unclear license release. The operator will verify child-process absence and license-seat availability through the supported vendor/admin route named in the live packet before clearing the guard. No vendor key/lock will be copied, deleted or altered. If there will be no supported way to verify recovery, the live packet will remain blocked; retrying the solver will not be used as a license probe. Claims about actual helper processes or leases will require observed/vendor evidence rather than assumptions from executable lineage.

Every command token, including geometry construction, units/density, HS origin-depth semantics, report closure/print behavior and END, will receive a version-matched manual citation and independent review in the capture packet. Milestone1 command doubles will remain provisional non-executable examples until that packet is approved. The installed runtime/version comparison will gate the entire command profile, not only extension handlers.

### Narrow format qualification and public-fixture control

Initial parser support will be limited to the exact observed version, encoding, locale, single-page three-depth table and documented diagnostic/completion profile. It will not claim general pagination, warning variants or alternate number formats from a single successful sample. Unobserved layouts or diagnostics will fail closed. Any expanded format support will require independently captured evidence for that variant and a reviewed addendum. Synthetic mutations will test rejection only and will not count as native-format qualification.

Original capture bytes and host/license metadata will stay in private operator-controlled evidence storage. Before any public fixture is proposed, the operator will inspect all decoded text, headers, paths and any binary/container metadata for license-holder names, company identifiers, account/machine details and other restricted content. A designated human reviewer will approve a promotion record with original and derivative hashes, exact redactions and source/redistribution permission. No unreviewed raw capture will be committed. A redacted permitted fixture will be explicitly labeled a derivative; tests will retain line mappings back to the privately retained original and will not claim derivative bytes are native originals. If clearance will be unavailable, real captured fixtures/acceptance will stay external; public synthetic parser tests will be labeled synthetic and cannot alone satisfy licensed completion.

### Frame and quantity qualification

The normalized oracle frame will use x forward from aft end, y port and z upward from baseline. The format addendum will explicitly map the native GHS origin, longitudinal/transverse signs, depth and units into this frame. Sign/offset/scale tests will precede any numerical comparison; direct comparison assuming native positive-forward coordinates will reject.

Required normalized quantities per depth will be volume, displacement mass, waterplane area, LCB, LCF, TCB, TCF, KB, BMT and BML. Volume expected values will be200/400/600m3, with proposed absolute tolerance floor0.1m3 in the same max(0.1percent relative, absolute floor) rule. If a required native quantity will be absent, unsupported or insufficiently precise, the qualification addendum will remain blocked unless an explicitly reviewed derivation will establish it from documented native fields. For example, KM-KB will be allowed for BM only after native KM/KB conventions, axes and units are qualified; transformation lineage and source locators for both operands will be retained. Milestone1's oracle will consume this normalized physical contract, never guessed native column names. The native mapping will remain deferred until actual evidence.

## Artifact Map / Files to Change

| Proposed artifact | Path |
|---|---|
| Canonical plan and preview | `docs/plans/2026-09-11-issue-2089-ghs-canary.md`, companion `.html` |
| Request/result contract, command construction | `src/digitalmodel/solvers/ghs/contracts.py` |
| Controlled runner | `src/digitalmodel/solvers/ghs/runner.py` |
| Independent box oracle/comparison | `src/digitalmodel/solvers/ghs/comparison.py` |
| Parser AFTER format qualification | `src/digitalmodel/solvers/ghs/parser.py` |
| Synthetic tests and later permitted native fixtures | `tests/solvers/ghs/` |
| Neutral canary description/derivation | `docs/domains/ghs/neutral-hydrostatics-canary.md` |
| Format qualification and reviews | `scripts/review/results/issue-2089/` |

Package initializer files will be included as required. Each code/test file will stay below400lines and function below50lines. No vendor manual, executable, private path, key material or proprietary project input will be added. Existing report/result conventions will be composed after interface verification; no new renderer service will be built.

## Pseudocode

```text
prepare_canary(request):
    validate closed neutral parameters and explicitly approved source identities
    derive fixed reviewed commands; bind geometry/runfile/oracle/tolerance hashes
    return pending-live-authorization packet, never execute
run_approved_capture(packet, local_execution_authority):
    verify exact packet, runtime/version/license/startup prerequisites
    stage fresh neutral files; start argument vector with bounded capture/termination
    retain native bytes and diagnostics with lineage; return observed run status
qualify_capture(capture):
    require independent version/format/diagnostic review before defining parser
parse_and_compare(qualified_capture):
    require qualified native format; parse complete rows with byte/line locators
    reject incomplete evidence; compare independent exact box oracle within approved tolerance
    return bounded hydrostatics findings, limitations and separate review status
```

## TDD Test List

- Contract tests will first fail for absent APIs; closed fields, invalid/nonfinite parameters and altered hashes will reject before process start.
- Argument tests will prove shell-free construction, startup suppression, fixed command set, explicit termination and print suppression; command injection/unknown arguments will reject.
- Process-double tests will cover launch errors, nonzero/zero-but-incomplete outcomes, timeout, termination failure, missing diagnostics, bounded output overflow and no automatic retry.
- Staging tests will reject existing output, substitutions, path escapes, symlinks/reparse points and stale captures. No tests will access a real solver/license in generic CI.
- Oracle tests will verify all three exact rows, axis-specific BM formulas, density scaling, units and tolerance boundaries with independently specified expected values.
- Native parser tests will be written only after approved format capture: exact golden, pagination/header changes, encoding, comma/decimal conventions, malformed/nonfinite/duplicate/missing rows, warnings/errors, version mismatch, unsupported precision and exact source locators.
- Conclusion tests will prohibit equilibrium/stability/class claims when only HS evidence exists, and prevent a failed comparison from yielding accepted findings.
- Licensed acceptance will record one actual authorized execution, native hashes/version/diagnostics, independent comparison and review disposition. Mock success will never satisfy this test.

## Acceptance Criteria

- [ ] Milestone1 contract/runner seam/oracle tests will pass with synthetic inputs and no real license in CI.
- [ ] A concrete runtime/capture packet will receive independent review and explicit live authorization.
- [ ] Native-format evidence will be captured and qualified before parser implementation; unknown grammar will remain blocked, not guessed.
- [ ] The permitted native golden and parser regression suite will pass after RED-first implementation.
- [ ] One genuine licensed neutral run will have complete provenance, diagnostic disposition and independent analytical comparison under approved tolerances.
- [ ] Conclusions will remain limited to this canary; broader screening/engineering approval will remain separate.
- [ ] Legal/security checks and applicable completeness/cleanup gates will pass before closeout; the owner will decide approval labels.
- [ ] Public artifacts will contain only neutral original code, permitted synthetic/native fixtures and source metadata; private execution evidence will remain external.

## Risks, decisions and review routing

Independent reviewers will challenge startup interception, false completion, timeout/diagnostic capture, oracle independence, semantic units/frame errors, format generalization, privacy and tolerance selection. Authoring this draft will not count as a Codex adversarial verdict.

The user/reviewer will decide whether the proposed ideal box, three depths and numerical tolerances are suitable. The operator will supply configured runtime/version/entitlement evidence and the concrete capture route. These will block only licensed capture/acceptance, not synthetic contract planning; no full-fleet/all-solver rollout will be a prerequisite.

If capture needs remote worker deployment, new authentication or process-isolation infrastructure beyond local controlled execution, that work will receive a separate owner/plan and T3 reassessment as appropriate. Otherwise2089 will retain both milestones; it will not close after mocks alone. No plans or scripts will activate every solver.
