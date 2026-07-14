# Plan for #1575: Preserve the OpenFOAM batch case-definition contract

> **Status:** draft
> **Complexity:** T3
> **Date:** 2026-07-13
> **Issue:** https://github.com/vamseeachanta/digitalmodel/issues/1575
> **Client:** N/A
> **Lane:** lane:codex
> **Execution mode:** single-lane after user approval
> **Review artifacts:** pending mandatory T3 adversarial review

---

## Resource Intelligence Summary

### Existing repo code

- `src/digitalmodel/workflows/openfoam_run_batch.py` will remain responsible for
  matrix expansion, per-case isolation, pool/MPI execution, checkpoints, and
  rollups. Its `_render_cases()` currently retains arbitrary settings, but
  `_build_case()` forwards them to the generic workflow without proving that
  every setting is consumed.
- `src/digitalmodel/solvers/openfoam/workflow.py` currently creates
  `OpenFOAMCase.for_case_type()` and copies only `case_type`, `name`, and an
  optional `solver`. This is the drop boundary the implementation will replace.
- `src/digitalmodel/solvers/openfoam/models.py` and `case_builder.py` already
  provide `DomainConfig`, `SolverConfig`, `OpenFOAMCase.motion`,
  `OpenFOAMCase.fill_level`, `dynamicMeshDict`, `setFieldsDict`, and pressure-tap
  function-object rendering. The workflow will construct these existing types
  instead of creating parallel renderers.
- `src/digitalmodel/solvers/openfoam/runner.py` and `prebuilt_mesh.py` already
  validate a closed version-1 prebuilt manifest, its source/case/polyMesh hashes,
  its boundary/cell-zone contract, and its case-local path before execution.
  The batch path will pass the attestation to this API rather than weakening it.
- `src/digitalmodel/base_configs/modules/openfoam_run_batch/openfoam_run_batch.yml`
  and `examples/workflows/openfoam-run-batch/input.yml` define the existing
  simple `base: {case_type, solver, mesh_utility, to_vtk}` compatibility surface.

### Related plans and issues

- Issue #1575 is open and requires a versioned, fail-closed generic contract plus
  source-neutral tests.
- Closed issue #1560 owns the saturating batch runner and its pool/MPI behavior;
  this issue will preserve that execution model.
- Closed issue #662 and
  `docs/plans/2026-07-10-issue-662-gmsh-openfoam-bridge.md` own the prebuilt
  polyMesh attestation and runner contract. This issue will consume that API and
  will not redesign conversion, hashing, or `checkMesh` validation.
- Issues #658, #659, and #661 own prescribed motion, partial fill, and pressure
  taps respectively. Their model and renderer contracts will remain the
  authoritative implementation surfaces.

### Intelligence and drive search

- `docs/document-intelligence/README.md` identifies the workspace intelligence
  entry points; this unlabeled issue will use the General retrieval bundle.
- The mandatory drive-index query `OpenFOAM batch case definition domain motion
  partial fill function objects prebuilt mesh` ran with
  `--caller plan-resource-intel`. It reported no coverage gaps, but its hits were
  unrelated private legacy engineering documents. No drive file will be opened,
  named, or reused. The query warned that the standards inventory, CAD
  readability index, and master document index were stale.
- No standards-derived constant or engineering calculation will be introduced;
  the calc Citation sidecar contract will therefore not apply.

### Reproduction proof

At `origin/main` commit `2ff0f72c9c5ce9022bfca763a6bb24ae4fb768d4`, a
synthetic `sloshing` build requested a custom domain, roll motion, 40% fill,
time controls, and one pressure tap. The license-free build returned:

```text
{"dynamicMeshDict": false, "functions_present": false,
 "requested_domain_present": false, "requested_time_present": false,
 "setFieldsDict": false}
```

The result matches the issue: the workflow adapter will silently render a
default static case even though the batch layer retains the requested mappings.

### Gaps identified

- No closed, versioned case-definition parser owns the generic YAML-to-model
  boundary.
- No single normalization step reports unknown, dropped, or incompatible fields.
- No workflow path propagates domain, motion, fill, time, or function objects.
- Neither serial nor MPI batch execution passes a prebuilt attestation to the
  existing runner contract.
- No end-to-end synthetic test proves semantic settings survive batch expansion.

---

## Artifact Map

| Artifact | Path |
|---|---|
| This plan | `docs/plans/2026-07-13-issue-1575-openfoam-case-definition-contract.md` |
| Case-definition parser | `src/digitalmodel/solvers/openfoam/case_definition.py` |
| Models and builder | `src/digitalmodel/solvers/openfoam/models.py`; `src/digitalmodel/solvers/openfoam/case_builder.py` |
| Generic workflow | `src/digitalmodel/solvers/openfoam/workflow.py` |
| Batch workflow | `src/digitalmodel/workflows/openfoam_run_batch.py` |
| Config and example | `src/digitalmodel/base_configs/modules/openfoam_run_batch/openfoam_run_batch.yml`; `examples/workflows/openfoam-run-batch/input.yml` |
| Focused tests | `tests/solvers/openfoam/test_case_definition.py`; `tests/solvers/openfoam/test_workflow_router.py`; `tests/workflows/test_openfoam_run_batch.py` |
| Existing renderer/runner regressions | `tests/solvers/openfoam/test_case_builder.py`; `tests/solvers/openfoam/test_runner_prebuilt.py` |
| Plan reviews | `scripts/review/results/2026-07-13-plan-1575-{claude,codex,gemini}.md` |

The cross-repository control-plane row in workspace-hub `docs/plans/README.md`
will require a separately authorized workspace-hub change; this digitalmodel-only
branch will not invent a local index.

## Deliverable

A versioned, closed OpenFOAM case-definition contract will carry every supported
semantic field from batch input through typed models, rendered dictionaries, and
the runner while preserving legacy simple batches and rejecting silent loss.

## Frozen Version-1 Schema

The new canonical form will live under `base.case_definition` for batch inputs
and under `openfoam.case_definition` for the generic workflow:

```yaml
case_definition:
  schema_version: 1
  case_type: sloshing
  name: synthetic_sloshing
  solver: interFoam
  domain:
    min_coords: [0.0, 0.0, 0.0]
    max_coords: [2.0, 1.0, 1.0]
    n_cells: [20, 10, 10]
  motion:
    type: roll
    amplitude: 3.0
    period: 1.5
    origin: [1.0, 0.5, 0.0]
  fill:
    level: 0.4
  time:
    start_time: 0.0
    end_time: 2.0
    delta_t: 0.002
    write_interval: 25
    adjustable_time_step: true
    max_co: 0.5
    purge_write: 0
    n_subdomains: 4
  function_objects:
    pressure_taps:
      - name: synthetic_tap
        location: [1.0, 0.5, 0.8]
        fields: [p, p_rgh]
    write_control: timeStep
    write_interval: 1
  prebuilt_mesh:
    manifest: constant/polyMesh.manifest.json
```

Every mapping level will use an exact allowlist. `schema_version` will accept
only integer `1`; booleans will not pass as integers. Vectors will contain
exactly three finite numbers. Domain extents and cell counts will be positive;
time values, motion values, fill range, tap names/fields, and manifest path will
delegate to or strengthen the existing domain validations.

Legacy simple inputs containing only the existing flat `case_type`, `name`,
`solver`, `mesh_utility`, `run_snappy`, `run_set_fields`, and `to_vtk` keys will
normalize once to version 1 with the same `for_case_type()` defaults. A mapping
will not mix `case_definition` with legacy semantic keys. Batch execution keys
will remain outside the semantic contract and will have their own exact
allowlist. Variant mappings will target canonical `case_definition.*` paths;
legacy mappings already exercised by current tests will continue to normalize.

Incompatibilities will fail before any directory is cleaned or written:

- `fill` will require a multiphase case; motion will require a transient solver.
- `run_set_fields: false` will be incompatible with `fill`; normalized fill will
  set the runner stage explicitly rather than relying on an implicit default.
- `prebuilt_mesh` will require `interFoam`, the exact case-local manifest path,
  and the existing attested case layout. It will prohibit mesh-generation,
  snappy, merge, topo/subset, and MPI mesh/decomposition paths that bypass the
  existing prebuilt runner validation. Version 1 will therefore support
  prebuilt execution in pool/serial mode only; MPI support will require a later
  schema version and its own attested parallel contract.
- A prebuilt case will remain bound to its manifest's `case_inputs` and
  `poly_mesh` digests. The adapter will pass the manifest path unchanged to
  `OpenFOAMRunner.run()`; it will not copy, rewrite, or re-attest mesh evidence.

## Proposed Design and Pseudocode

```text
normalize_case_definition(settings):
    select canonical v1 or the explicitly recognized legacy-simple shape
    reject mixed forms and every unknown key at every mapping level
    validate primitive types, finite values, enums, and cross-field rules
    create DomainConfig, PrescribedMotion, SolverConfig, and PressureTap values
    return immutable ParsedCaseDefinition(case, taps, runner, prebuilt_manifest)

build_case(cfg, settings):
    parsed = normalize_case_definition(settings)
    if parsed.prebuilt_manifest exists:
        require its case-local parent is the selected existing case directory
        return that case without cleaning, authoring, or mutating it
    build parsed.case with OpenFOAMCaseBuilder(parsed.case, parsed.taps,
        tap_write_interval=parsed.function_write_interval)
    return the authored case plus parsed runner settings

run_serial(parsed, case_dir):
    construct OpenFOAMRunConfig only from validated runner settings
    call runner.run(case_dir, prebuilt_manifest=parsed.prebuilt_manifest)
    preserve fail-closed status and attested snapshot result reporting

render_batch(base, variants):
    normalize the base before destructive work
    apply each mapped variant to a deep copy of canonical data
    normalize every resulting case again so a mapped typo cannot be dropped
    carry the parsed definition through pool execution without reparsing drift
```

## Files to Change

| Action | Path | Reason |
|---|---|---|
| Create | `src/digitalmodel/solvers/openfoam/case_definition.py` | Own versioning, exact-key validation, compatibility normalization, and typed construction |
| Create | `tests/solvers/openfoam/test_case_definition.py` | Drive the closed schema and incompatibility rules TDD-first |
| Modify | `src/digitalmodel/solvers/openfoam/models.py` | Store generic pressure-tap/function-object inputs only if review confirms builder arguments cannot remain the boundary |
| Modify | `src/digitalmodel/solvers/openfoam/case_builder.py` | Consume normalized function-object settings without changing no-object output |
| Modify | `src/digitalmodel/solvers/openfoam/workflow.py` | Replace the lossy adapter and pass prebuilt attestations to the runner |
| Modify | `src/digitalmodel/workflows/openfoam_run_batch.py` | Validate before cleanup, retain parsed definitions, and support serial prebuilt execution |
| Modify | `src/digitalmodel/base_configs/modules/openfoam_run_batch/openfoam_run_batch.yml` | Document canonical v1 without injecting semantic defaults during merge |
| Modify | `examples/workflows/openfoam-run-batch/input.yml` | Add a source-neutral canonical example and retain the simple compatibility example |
| Modify | `tests/solvers/openfoam/test_workflow_router.py` | Prove generic workflow rendering and runner propagation |
| Modify | `tests/workflows/test_openfoam_run_batch.py` | Prove batch/variant propagation, compatibility, early failure, and prebuilt forwarding |
| Modify | `tests/solvers/openfoam/test_case_builder.py` | Lock exact dictionary output and no-function-object compatibility |

No private geometry, project code, client identifier, native result, or
standards-derived engineering value will enter fixtures, docs, commits, or
review prompts.

## TDD Test List

| Test | Verification | Input | Expected output |
|---|---|---|---|
| `test_v1_rejects_unknown_key_at_every_level` | No typo can disappear | one extra key per schema mapping | path-specific `ValueError` before filesystem mutation |
| `test_v1_rejects_unknown_schema_version_and_bool` | Version check is exact | `2`, `true`, missing | fail closed |
| `test_v1_builds_all_typed_values` | Every field reaches existing models | full synthetic v1 mapping | exact model/tap/runner values |
| `test_v1_rejects_invalid_domain_motion_fill_time_and_taps` | Primitive/domain validation is closed | parameterized invalid values | descriptive failure |
| `test_v1_rejects_incompatible_combinations` | Semantic and execution conflicts fail early | fill/single-phase, motion/steady, prebuilt/MPI or mesh stages | no case deletion or write |
| `test_legacy_simple_definition_matches_current_case` | Compatibility remains byte-stable | current simple batch base | same rendered dictionaries and command plan |
| `test_canonical_and_legacy_semantics_cannot_mix` | No precedence ambiguity | both forms | fail closed |
| `test_workflow_renders_full_case_definition` | Generic adapter is lossless | full synthetic v1 | requested `blockMeshDict`, `dynamicMeshDict`, `setFieldsDict`, `controlDict`, and `functions` |
| `test_batch_variant_revalidates_canonical_definition` | Dotted mapping cannot bypass validation | valid sweep plus typo/invalid case | exact variants or pre-write failure |
| `test_batch_mock_proves_requested_artifacts` | Mock cannot mask a default case | full canonical batch, `mock: true` | exact rendered artifacts |
| `test_serial_prebuilt_passes_case_local_manifest` | Attestation reaches runner unchanged | synthetic v1 attested case | runner receives exact manifest and skips mesh generation |
| `test_tampered_or_mismatched_prebuilt_remains_rejected` | Existing hashes/contracts remain authoritative | source-neutral mutated fixture | failure before solver |
| `test_mpi_prebuilt_is_rejected_before_cleanup` | Unsupported v1 combination cannot bypass attestation | prebuilt plus MPI | explicit error and source unchanged |

Tests will be committed RED before production changes, then GREEN with the
minimum implementation, followed by refactoring with the focused suite green.

## Acceptance Criteria

- [ ] `schema_version: 1` and every nested mapping reject missing, extra,
  mistyped, nonfinite, and incompatible values before any destructive work.
- [ ] Every accepted field is represented in a typed model or explicit runner
  value; a coverage test compares accepted schema paths with consumed paths so
  adding an allowlisted-but-unconsumed field fails.
- [ ] A source-neutral mock batch renders the requested domain,
  `dynamicMeshDict`, `setFieldsDict`, time controls, and pressure-tap functions.
- [ ] A source-neutral prebuilt fixture passes its exact manifest to the serial
  runner, while hash, input, patch/cell-zone, toolchain, and tamper failures stay
  fail-closed.
- [ ] Existing simple batch YAML produces the same case dictionaries, manifest
  rows, command plan, and checkpoint behavior.
- [ ] `PYTHONPATH=src uv run python -m pytest tests/solvers/openfoam/test_case_definition.py tests/solvers/openfoam/test_workflow_router.py tests/workflows/test_openfoam_run_batch.py tests/solvers/openfoam/test_case_builder.py tests/solvers/openfoam/test_runner_prebuilt.py -q` passes.
- [ ] `PYTHONPATH=src uv run python -m pytest tests/solvers/openfoam tests/workflows/test_openfoam_run_batch.py -q` passes.
- [ ] `uv run ruff check src/digitalmodel/solvers/openfoam src/digitalmodel/workflows/openfoam_run_batch.py tests/solvers/openfoam tests/workflows/test_openfoam_run_batch.py` passes.
- [ ] `bash scripts/legal/legal-sanity-scan.sh --diff-only` passes from the
  digitalmodel repository, or the repository's documented equivalent command
  passes with the exact invocation recorded.
- [ ] T3 plan review and post-implementation cross-review complete before their
  respective approval/close gates; the user, not an agent, applies plan approval.

## Adversarial Review Summary

Mandatory T3 adversarial review has not run. The draft will remain `draft`; no
review verdict, GitHub label, issue comment, or user approval will be inferred.

## Risks and Open Questions

- **Prebuilt ownership:** the v1 design will treat the attested case as immutable
  and will prohibit MPI rather than copy or rewrite evidence. Review must reject
  any implementation that weakens the existing case-local manifest binding.
- **Compatibility boundary:** only the enumerated simple legacy keys will remain
  permissive. Existing configs containing undocumented semantic keys may have
  relied on silent drops; they will fail and require canonical migration.
- **Model ownership:** pressure taps may remain a builder argument or become an
  `OpenFOAMCase` field. Review will prefer the smallest single source of truth
  that still makes accepted/consumed coverage mechanically testable.
- **Control-plane index:** workspace-hub owns the plan index. Updating it will
  need separate repository authorization and will not be hidden in this branch.

## Complexity: T3

The change will cross batch expansion, a public schema boundary, typed CFD
models, dictionary authoring, runner attestation, compatibility, and destructive
retry behavior. It will therefore require T3 plan and code-stage review.
