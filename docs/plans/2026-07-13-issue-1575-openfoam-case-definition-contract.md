# Plan for #1575: Preserve the OpenFOAM batch case-definition contract

> **Status:** draft — r2 MAJOR findings resolved inline in r3; user approval required
> **Complexity:** T3
> **Date:** 2026-07-13
> **Issue:** https://github.com/vamseeachanta/digitalmodel/issues/1575
> **Client:** N/A
> **Lane:** lane:codex
> **Execution mode:** single-lane after dependency gates and user approval
> **Review artifacts:** `scripts/review/results/2026-07-13-plan-1575-r{1,2}-consolidated.md`

---

## Resource Intelligence Summary

### Existing code and reproduced loss

- `src/digitalmodel/workflows/openfoam_run_batch.py` retains arbitrary per-case
  mappings, but `src/digitalmodel/solvers/openfoam/workflow.py` creates a fresh
  `OpenFOAMCase` and copies only `case_type`, `name`, and `solver`.
- `models.py`, `case_builder.py`, `motion.py`, `partial_fill.py`, and
  `pressure_taps.py` already provide typed domain, time, prescribed-motion,
  partial-fill, and pressure-tap rendering surfaces.
- `runner.py` and `prebuilt_mesh.py` own a closed case-local prebuilt-manifest
  contract. That contract hashes the whole case and cannot safely consume an
  arbitrary external path or a newly authored semantic definition.
- A synthetic `sloshing` request at base
  `2ff0f72c9c5ce9022bfca763a6bb24ae4fb768d4` requested domain, roll motion,
  fill, time, and a pressure tap. The exact license-free probe returned:

```text
{"dynamicMeshDict": false, "functions_present": false,
 "requested_domain_present": false, "requested_time_present": false,
 "setFieldsDict": false}
```

### Dependencies and ownership order

The implementation chain will be **#1565 → #1575 → #1576**:

1. **#1565** will merge first. #1575 will consume its `WorkLayout`,
   `RunIdentity`, safe case-root confinement, and checkpoint identity at the
   exact merged SHA; it will not create a second layout or identity algorithm.
2. As a hard API prerequisite to #1575 (not a separate implementation lane),
   **#1574** will merge its neutral pressure-tap API. Before any #1575 code
   change, the implementation evidence will record the exact 40-hex #1574 merge
   SHA and prove it is an ancestor of the implementation branch. #1575 will not
   import or preserve the project-coded compatibility surface.
3. **#1575** will land the authored-case schema and propagation contract.
4. **#1576** will then consume #1575's typed `SelectedExecutionPlan` and
   accepted-leaf ledger for MPI/VTK planning. It will not change the serial-only
   prebuilt boundary or create a second execution/rank contract.

If either upstream issue changes a named interface, this plan will be amended
and re-reviewed before implementation. Parallel edits to the shared workflow,
configuration, or tests will not proceed.

### Retrieval and privacy boundary

- Issues #1560, #662, #658, #659, #661, #1565, #1574, and #1576 define the
  current batch, attestation, motion, fill, function-object, layout, privacy,
  and downstream execution boundaries.
- No standard-derived constant, private geometry, project code, native result,
  host path, or client identifier will enter code, fixtures, review artifacts,
  commit text, or documentation.

### Gaps

- No closed schema distinguishes authored input from attested prebuilt input.
- No exact generic/batch root allowlist or accepted-leaf-to-consumer ledger
  makes silent loss mechanically impossible.
- Case name, coordinate frame, motion units, MPI rank authority, and
  function-object ownership are not frozen at the YAML boundary.
- Three touched files and three functions already exceed universal limits.

---

## Artifact Map

| Artifact | Path |
|---|---|
| Plan | `docs/plans/2026-07-13-issue-1575-openfoam-case-definition-contract.md` |
| r1 consolidated review | `scripts/review/results/2026-07-13-plan-1575-r1-consolidated.md` |
| Schema/parser | `src/digitalmodel/solvers/openfoam/case_definition.py` |
| Typed models | `src/digitalmodel/solvers/openfoam/models.py`; neutral tap API from #1574 |
| Builder/mesh renderer | `src/digitalmodel/solvers/openfoam/case_builder.py`; `src/digitalmodel/solvers/openfoam/block_mesh.py` |
| Generic adapter | `src/digitalmodel/solvers/openfoam/workflow.py` |
| Batch adapter | `src/digitalmodel/workflows/openfoam_run_batch.py` plus split execution/IO modules named below |
| Configuration/example | `src/digitalmodel/base_configs/modules/openfoam_run_batch/openfoam_run_batch.yml`; `examples/workflows/openfoam-run-batch/input.yml` |
| Focused tests | `tests/solvers/openfoam/test_case_definition.py`; `test_workflow_router.py`; split batch tests named below |

## Deliverable

A closed authored/prebuilt v1 contract will carry every accepted semantic and
execution leaf through batch normalization, typed construction or attested
staging, rendering, and runner planning, while legacy inputs retain documented
behavior and no prebuilt or unknown field can be silently ignored.

## Case-Source Discriminated Union

The type boundary will be explicit:

```text
CaseSource = AuthoredCaseV1 | PrebuiltCaseV1
discriminator = case_definition.kind
```

`kind: authored` will accept only the schema below. `kind: prebuilt` will accept
only `schema_version: 1` and `prebuilt: {case_id: <portable component>}`. It will
prohibit domain, motion, fill, time, function objects, mesh-generation controls,
MPI, manifest paths, directories, URIs, hostnames, and arbitrary locators.

`case_id` will resolve beneath the current input bundle's fixed
`prebuilt_cases/` directory by descriptor-relative/no-follow traversal. The only
manifest location is fixed at `constant/polyMesh.manifest.json`. The adapter will
validate the existing #662 case-input/polyMesh attestation, copy the complete
case by descriptors into a fresh #1565-owned run directory, then validate the
same manifest and bytes again. Every source byte and manifest digest enters
#1565 RunIdentity. Source and staged trees are immutable during the transaction;
drift rejects. V1 prebuilt is serial/pool only and runs only solver/postprocess
stages allowed by the attestation. Future prebuilt MPI requires a separate issue;
#1576 explicitly rejects it.

## Canonical Authored v1 Schema

```yaml
case_definition:
  schema_version: 1
  kind: authored
  authored:
    case_type: sloshing
    name: synthetic_sloshing
    solver: interFoam
    domain:
      min_coords_m: [0.0, 0.0, 0.0]
      max_coords_m: [2.0, 1.0, 1.0]
      n_cells: [20, 10, 10]
    motion:
      type: roll
      amplitude: 3.0
      amplitude_unit: deg
      period_s: 1.5
      origin_m: [1.0, 0.5, 0.0]
    fill:
      level: 0.4
    time:
      start_time_s: 0.0
      end_time_s: 2.0
      delta_t_s: 0.002
      write_interval_steps: 25
      adjustable_time_step: true
      max_co: 0.5
      purge_write: 0
    function_objects:
      pressure_taps:
        - name: synthetic_tap
          location_m: [1.0, 0.5, 0.8]
          fields: [p, p_rgh]
      write_control: timeStep
      write_interval: 1
execution:
  mesh_utility: blockMesh
  run_snappy: false
  run_set_fields: true
  to_vtk: false
  timeout_seconds: 43200
  dry_run: false
```

Every mapping will use exact keys. Integers will reject booleans; numeric leaves
will be finite; extents, counts, time steps, periods, intervals, and timeouts will
be positive; `0 <= fill.level <= 1`; and `start_time_s < end_time_s`.

Nested authored keys will be exactly:

```text
authored: case_type,name,solver,domain,motion,fill,time,function_objects
domain: min_coords_m,max_coords_m,n_cells
motion: type,amplitude,amplitude_unit,period_s,origin_m,phase_shift_s
fill: level; time: the seven shown leaves
function_objects: pressure_taps,write_control,write_interval
pressure_tap: name,location_m,fields
```

Authored v1 will require `case_type,name,solver,domain,time` and all six
`execution` leaves. Optional capability mappings, when present, will require all
applicable shown leaves; only `phase_shift_s` will default under the rules below.

### Coordinate, motion, and name contract

- Coordinates will be SI metres in one right-handed global Cartesian frame:
  `x` longitudinal, `y` transverse, `z` vertical/up. Gravity and partial fill
  will use `-z`/`z`; `blockMeshDict`, motion origin, and tap locations will use
  this same frame with no implicit transform.
- `roll`, `pitch`, and `yaw` will rotate about global `x`, `y`, and `z` and will
  require `amplitude_unit: deg` plus `origin_m`. `surge`, `sway`, and `heave`
  will translate along `x`, `y`, and `z` and will require
  `amplitude_unit: m`; translation will reject `origin_m`. `phase_shift_s` will
  be allowed only for translation and will default to zero.
- `name` will match `[A-Za-z0-9][A-Za-z0-9_-]{0,63}`. Separators, dots, spaces,
  controls, NULs, absolute paths, `.`/`..`, and case-insensitive Windows device
  names will fail. The target will be resolved beneath the #1565 case root;
  every existing component and target will be checked for symlinks immediately
  before create/clean/build.

### Function-object ownership

`case_definition.py` will own an immutable `FunctionObjectsConfig` containing
the #1574 neutral `PressureTap` values, `write_control`, and `write_interval`.
Version 1 will allow only pressure taps and `write_control` values `timeStep`,
`runTime`, or `adjustableRunTime`. `ParsedAuthoredCaseV1` will be the sole owner
of `OpenFOAMCase`, `FunctionObjectsConfig`, and `SelectedExecutionPlan`.

`OpenFOAMCaseBuilder` will receive the complete typed function-object config and
will pass both control leaves to the renderer. No parallel dict, model field,
default, or conditional ownership decision will remain. Empty taps will reject
non-default write controls because those values would otherwise be unconsumed.

## Exact Root and Execution Allowlists

### Generic `openfoam` adapter

Canonical root keys will be exactly:

```text
operation, output_directory, case_definition, execution
```

`operation` will be `build_case|run_openfoam`. `execution` keys will be exactly:

```text
mesh_utility, run_snappy, run_set_fields, to_vtk, timeout_seconds, dry_run
```

The legacy generic root will accept exactly the current keys:

```text
operation, case_type, name, output_directory, solver,
mesh_utility, run_snappy, to_vtk, dry_run, timeout_seconds
```

It will normalize once to authored v1. Canonical and legacy forms cannot mix.
The frozen generic legacy mapping will be `case_type|name|solver →
case_definition.authored.<same>`, each execution-named legacy key →
`execution.<same>`, while `operation` and `output_directory` remain adapter
controls. No accepted generic input key will lack a named destination.

### Batch `openfoam_run_batch` adapter

The batch root will accept exactly:

```text
base, cases, variants, mapping, run_batch
```

Canonical `base` keys will be exactly `case_definition, execution`. The legacy
base will accept exactly:

```text
case_type, name, solver, mesh_utility, run_snappy, run_set_fields, to_vtk
```

`run_batch` will consume #1565's exact typed configuration and keys:
`mode, workers, mock, reconstruct, resume, timeout_seconds, output_dir,
work_dir, execution_context, work_root, work_root_namespace`. It will not reparse
or duplicate their validation. Canonical `execution.timeout_seconds`
will be the sole per-utility timeout. Legacy `run_batch.timeout_seconds` will
normalize to it; specifying both with different values will reject.

The packaged YAML's `base` default will become `{}` so deep merge cannot inject
legacy semantic leaves into canonical input. Normalization will supply legacy
defaults, while `run_batch` will retain only adapter defaults. Equal legacy and
canonical timeout values will collapse to one typed leaf; differing values will
reject.

Each explicit case row will contain `name` plus mapped knobs. In canonical mode,
every non-name knob must appear once in `mapping`; every mapping target must be
an accepted mutable leaf below `case_definition.authored` or `execution`.
Duplicate targets, container targets, discriminator/version targets, and
unmapped knobs will reject. The existing variants source shapes
`factorial|range|csv|yaml_matrix` will remain owned by `_load_cases`, but their
generated knob names will obey the same exact mapping rule before rendering.
Legacy direct knobs will be accepted only when their names equal a legacy base
leaf and will normalize through that same leaf.

### Accepted-leaf consumption proof

The parser will expose a frozen `accepted_leaf -> consumer` table. Tests will
enumerate every canonical and legacy leaf and require exactly one consumer:

```text
OpenFOAMCase | FunctionObjectsConfig | SelectedExecutionPlan |
WorkLayout/RunIdentity | batch matrix/dispatch | generic operation/output
```

An accepted leaf with zero or multiple consumers, or a consumed leaf absent from
the table, will fail tests. Round-trip tests will compare normalized typed values,
rendered dictionaries, selected runner argv/settings, public rows, and identity
inputs rather than merely checking that parsing succeeds.

## Rank, Fill, and Execution Compatibility

- In MPI mode, `run_batch.workers` will be the sole rank authority and will set
  `OpenFOAMCase.solver_config.n_subdomains`. Authored v1 will not accept
  `n_subdomains`. Any upstream/default/model value that differs after assignment
  will reject before `decomposeParDict` is written. Tests will prove workers,
  dictionary count, `mpirun -np`, and #1565 RunIdentity agree.
- Pool cases will remain single-rank; their unused decomposition count will not
  enter public identity or claim MPI execution.
- `fill` will require a multiphase case and `execution.run_set_fields: true`.
  Supplying false will reject; absence of fill plus true will require an explicit
  caller-authored `setFieldsDict` capability, unsupported in v1, and will reject.
- Motion will require a transient solver. Mesh/snappy/setFields combinations
  unsupported by the current runner will reject before #1565 cleanup.
- `dry_run` and batch `mock` will remain distinct: dry-run will exercise runner
  planning; mock will build and record without claiming a solver result.

## Pre-Edit Size Split

After rebasing on merged #1565, RED characterization tests will prove its
`openfoam_batch_execution.py`, results, facade, and focused tests already satisfy
400/50. #1575 will modify those modules rather than re-create them. The remaining
builder split will land before feature behavior:

| Current excess | Required split |
|---|---|
| `case_builder.py` — 420 lines; `_write_block_mesh_dict` 81 | move deterministic block-mesh rendering to `block_mesh.py`; leave builder orchestration only |

The split commit will change no rendered bytes, argv, rows, checkpoints, or
exceptions. A syntax-tree test will enforce every touched Python file at no more
than 400 lines and every function at no more than 50 lines before feature edits.

## Files to Change

| Action | Path | Reason |
|---|---|---|
| Create | `case_definition.py`; `test_case_definition.py` | closed v1 models, normalization, leaf ledger, TDD |
| Create | `block_mesh.py` | mandatory builder split |
| Modify | `models.py`; `case_builder.py`; `workflow.py` | typed authored construction and rendering |
| Modify | #1565 facade/config/execution/results modules | adapter, typed selection, layout/identity consumption without duplicate modules |
| Modify | packaged YAML and synthetic example | canonical contract plus legacy compatibility documentation |

## TDD and Acceptance

- [ ] Dependency proof will record exact merged #1565/#1574 SHAs and ancestor
  checks before the first RED implementation commit.
- [ ] RED tests will accept only the fixed input-bundle `PrebuiltCaseV1`, reject
      caller paths/locators/MPI/mixed semantics, and prove attestation/copy/drift failures.
- [ ] Source-neutral tests will prove exact domain, frame/motion units,
  `dynamicMeshDict`, `setFieldsDict`, time, tap control/interval, and runner plan.
- [ ] Name/device/path/symlink tests will prove portable root confinement before
  clean/create/build.
- [ ] MPI tests will prove one workers-derived rank in model, dictionary, argv,
  and RunIdentity and will reject every mismatch before mutation.
- [ ] Legacy generic and batch examples will retain exact rendered bytes, argv,
  public rows, and checkpoint behavior after normalization.
- [ ] The #1565 split will be consumed at its exact merged SHA; the builder split
      will precede feature edits and preserve characterization goldens.
- [ ] `PYTHONPATH=src uv run python -m pytest tests/solvers/openfoam/test_case_definition.py tests/solvers/openfoam/test_workflow_router.py tests/solvers/openfoam/test_case_builder.py tests/workflows/test_openfoam_run_batch.py tests/workflows/test_openfoam_batch_execution.py tests/workflows/test_openfoam_run_batch_contract.py -q` will pass.
- [ ] `PYTHONPATH=src uv run python -m pytest tests/solvers/openfoam tests/workflows/test_openfoam_run_batch*.py -q` will pass.
- [ ] `uv run ruff check src/digitalmodel/solvers/openfoam src/digitalmodel/workflows/openfoam_run_batch.py src/digitalmodel/workflows/openfoam_batch_execution.py src/digitalmodel/workflows/openfoam_batch_results.py tests/solvers/openfoam tests/workflows/test_openfoam*.py` will pass.
- [ ] `PYTHONPATH=src uv run python -m compileall -q src/digitalmodel/solvers/openfoam src/digitalmodel/workflows/openfoam_run_batch.py src/digitalmodel/workflows/openfoam_batch_execution.py src/digitalmodel/workflows/openfoam_batch_results.py` will pass.
- [ ] `PYTHONPATH=src uv run python -m pytest tests/architecture/test_touched_python_size_limits.py -q` will prove the literal 400/50 limits.
- [ ] With `WORKSPACE_HUB_ROOT` and `DIGITALMODEL_REL_FROM_HUB` set, the candidate
      SHA check and legal scan will pass:
      `EXPECTED_SHA="$(git rev-parse HEAD)" && test "$(git -C "$WORKSPACE_HUB_ROOT/$DIGITALMODEL_REL_FROM_HUB" rev-parse HEAD)" = "$EXPECTED_SHA" && (cd "$WORKSPACE_HUB_ROOT" && bash scripts/legal/legal-sanity-scan.sh --repo="$DIGITALMODEL_REL_FROM_HUB" --diff-only)`.
- [ ] `git diff --check` and T3 code/artifact review will pass before closeout.
- [ ] The issue will remain implementation-blocked until r2 has no MAJOR and the
  user explicitly approves; no agent will create approval state.

## Adversarial Review Summary

R1 reached three-provider MAJOR consensus. The consolidated artifact records the
unsafe prebuilt ambiguity, incomplete allowlists/consumption, rank and path
authority gaps, unresolved ownership, oversized touched code, missing dependency
order, and non-executable validation commands.

R2 also reached three-provider MAJOR consensus. Its distinct findings required a
safe serial prebuilt variant, exact #1565 config composition, one
`SelectedExecutionPlan`, and modification rather than recreation of #1565 split
modules. Those are resolved inline in r3 without redispatch under the loop-break
rule. The plan remains draft pending explicit user approval.

## Risks and Open Questions

- #1565 and #1574 are hard gates whose exact merge SHAs do not yet exist. Their
  SHAs will be recorded and this plan revalidated before implementation.
- Prebuilt cases are intentionally unavailable in v1. Supporting them without an
  opaque staged locator and semantic digest would recreate the defect.
- #1576 must rebase on the final typed execution and identity contracts; it may
  not restore `--oversubscribe` or invent a second checkpoint schema.
- The worktree-specific legal invocation is intentionally pinned because the
  current scanner resolves repositories relative to workspace-hub.

## Complexity: T3

The change will cross schema versioning, batch mapping, CFD frames/units,
function objects, rank authority, filesystem confinement, layout identity,
compatibility, and three mandatory architecture splits.
