# Plan for #662: gmsh to OpenFOAM polyMesh bridge

> **Status:** adversarial-reviewed - user approval checkpoint pending
> **Complexity:** T3
> **Date:** 2026-07-10
> **Issue:** https://github.com/vamseeachanta/digitalmodel/issues/662
> **Client:** N/A
> **Lane:** lane:codex
> **Execution mode:** parallel-worktree
> **Review artifacts:** `scripts/review/results/2026-07-10-plan-662-codex-{architecture,engineering}-r{1,2}.md` | provider CLI UNAVAILABLE artifacts in the same directory

---

## Resource Intelligence Summary

### Existing repo code

- `src/digitalmodel/solvers/gmsh_meshing/mesh_generator.py` imports STEP, generates
  tetrahedra, and writes MSH, but it assigns no CFD physical groups and attaches no
  mesh to an OpenFOAM case.
- `src/digitalmodel/solvers/gmsh_meshing/quality_analyzer.py` calculates tetrahedral
  shape metrics. Its Jacobian path uses absolute volume, so it cannot prove element
  orientation and will remain only one non-authoritative shape check.
- `src/digitalmodel/solvers/openfoam/runner.py` always invokes one no-argument mesh
  utility before a solve. It has no attested prebuilt-`polyMesh` path.
- `scripts/cfd/run_sloshing_3d_benchmark.py` contains a working script-local 3D VOF
  case writer and MPI sequence. Any extraction will preserve an independent golden
  captured before the production refactor.
- `scripts/setup/provision-cfd-box.sh` uses `uv pip install -e .`, which does not
  enforce `uv.lock`, and the dedicated image lacks the Python Gmsh runtime.
- No production `GeometryProcessor` exists under `src/` or `tests/`; similarly named
  archived agent material will not become a runtime dependency.

### Standards and technical contracts

No standards-derived constant will enter this issue, so no calculation Citation
sidecar will be required. These executable technical contracts will govern instead:

| Contract | Required behavior | Evidence source |
|---|---|---|
| System toolchain | `openfoam2312-default=2312.260127-2` and OpenMPI packages `4.1.6-7ubuntu2` will be installed/held exactly; installer SHA-256 will equal `f7fa288327e936b5a85e3e4a0b29bf039c06d214916f39400b830b63a3310b5b` | live `dpkg-query`; provisioner |
| Gmsh MSH 2.2 ASCII | SI metres, first-order elements, `Mesh.SaveAll=0`, globally unique physical IDs, tagged triangles/tetrahedra, one thread, fixed seed, reproducible mode | Gmsh manual and locked Gmsh 4.x runtime |
| Converted boundary | exact patch names, types, positive sizes, full boundary-face coverage, and no `defaultFaces` | OpenFOAM v2312 `gmshToFoam` behavior plus parsed `polyMesh` |
| Converted volume | `fluid` cellZone will exist and cover every cell | parsed `cellZones` plus `checkMesh` cell count |
| Mesh verdict | return code zero is insufficient; output will require `Mesh OK` and zero `Failed N mesh checks` | OpenFOAM v2312 `checkMesh.C` |
| Public/private boundary | only synthetic inputs, code, tests, and reduced evidence will enter this repository | issues #637 and #640 |

### Coordinate-frame contract

All geometry, fields, motion, tests, and reportable values will use one frozen frame:

| Axis | Meaning | Coupled behavior |
|---|---|---|
| `x` | transverse breadth | L-shaped plan coordinate |
| `y` | vertical, positive upward | atmosphere at maximum `y`; fill measured from minimum `y`; gravity in `-y` |
| `z` | longitudinal/span | physical roll axis and prescribed rotation axis |

The public L-shaped plan will lie in `x-z`; every YAML length will be an SI metre
value and any other unit token will fail validation. Tests will reject unit or frame
mismatch before meshing or solving.

### Ownership and dependency boundary

| Issue | Ownership after this plan |
|---|---|
| #662 | MSH contract, transactional conversion, `polyMesh` contract parser, domain-specific synthetic fixture, attested prebuilt-mesh execution, and short MPI smoke |
| #640 | production geometry adapters, private geometry assumptions, final geometry simplifications, and production study promotion |
| #153 | generic OCC assemblies, defeaturing, boundary layers, convergence, and reusable quality-gate work; #662 will not introduce those abstractions |
| #1495 | owns provisioning, verification, and transport-neutral selection; #662 will extend those shared scripts only for locked Gmsh readiness and tested smoke handoff |
| #155 | full CAD-to-CFD orchestration; #662 will provide its narrow Gmsh/OpenFOAM handoff and will cross-link it |

The synthetic fixture will be test infrastructure, not a production geometry module.
It will include one longitudinal member, one transverse member, and one large
elliptical through-opening in each. Generic geometry import will remain out of scope.

### Documents, memory, and issues consulted

- Issues #662, #640, #1433, #153, #1495, and #155 define scope, production gates,
  adjacent ownership, routing, and the full-pipeline consumer.
- `docs/plans/2026-06-29-cfd-capability-assessment.md` identifies the disconnected
  Gmsh/OpenFOAM layers and recommends activation rather than replacement.
- `docs/session-handoffs/2026-07-09-cfd-connectivity-html-reporting-notes.md`
  keeps transport behind SSH targets and report implementation in another issue.
- `.claude/memory/topics/cfd-execution-box.md` defines the dedicated execution class,
  eight-rank ceiling, and shared fallback.
- The committed-data-leak memory rule requires synthetic names and de-identified
  fixtures, manifests, review prose, commits, and PR text.
- The drive-file query `OpenFOAM CFD tank geometry mesh STEP STL` returns no reusable
  public fixture. Private and unrelated legacy CAD hits will not be opened or named.

### Evidence and reproduction

**Live issue state** (GitHub connector, 2026-07-10): #662, #640, #1433, #153,
#1495, and #155 are open. #1433 explicitly keeps production 3D work paused. No
related open bridge/OCC PR appears on `origin/main`.

**Baseline** (2026-07-10):

```text
$ uv run --extra solvers pytest \
    tests/solvers/gmsh_meshing/test_gmsh_step_inp.py \
    tests/solvers/openfoam/test_runner.py -q
32 passed in 71.42s
```

**Gap proof** (2026-07-10):

```text
$ rg -n 'gmshToFoam' src tests scripts
# no matches
$ rg -n 'class GeometryProcessor|GeometryProcessor' src tests
# no matches
$ rg -n 'mesh_utility|run_mesh|skip_mesh' src/digitalmodel/solvers/openfoam/runner.py
93:    mesh_utility: str = "blockMesh"
193:            (OpenFOAMRunStatus.MESHING, [self._config.mesh_utility]),
```

**Execution image and routing probe** (labels de-identified, 2026-07-10):

```text
dedicated: gmshToFoam=yes, Python gmsh=no, eligible at 8 ranks, 0.5899 s/step
shared fallback: gmshToFoam=yes, Python gmsh=no, eligible at 8 ranks, 1.0582 s/step
dispatcher selection: dedicated execution class, 8 ranks
both nodes: OpenFOAM package 2312.260127-2; OpenMPI packages 4.1.6-7ubuntu2
```

**Reproduction proof:** the structural disconnection matches #662. The first RED
tests will fail to import the bridge/parser/fixture and will show that the current
runner cannot consume an attested prebuilt mesh.

### Review defects incorporated

Two native adversarial reviewers return MAJOR. Revision 2 will require zero failed
`checkMesh` checks despite exit code zero, normalize wall patch types, freeze the
coordinate frame, reject `defaultFaces`, separate cellZone and patch contracts, enforce
MSH2 serialization, detect signed orientation, cover the full internal structure,
stage conversion transactionally, test MPI argv/failures/motion, preserve an independent
benchmark golden, and create a versioned digest-linked manifest. Round 2 then finds
field/mesh contract drift, vacuous point hashes, mutable execution, incomplete
dispatcher binding, semantic completion, package/source provenance, units, ID collisions,
and Gmsh nondeterminism. Revision 3 resolves these inline per the review loop-break rule.
Claude, Codex CLI, and Gemini CLI remain UNAVAILABLE and do not count as consensus.

---

## Artifact Map

| Artifact | Path |
|---|---|
| Plan | `docs/plans/2026-07-10-issue-662-gmsh-openfoam-bridge.md` |
| Synthetic test fixture | `src/digitalmodel/solvers/gmsh_meshing/tank_fixture.py` |
| polyMesh contract parser | `src/digitalmodel/solvers/openfoam/poly_mesh_contract.py` |
| Transactional bridge | `src/digitalmodel/solvers/openfoam/gmsh_bridge.py` |
| 3D case writer | `src/digitalmodel/solvers/openfoam/validation/sloshing_3d.py` |
| MPI smoke driver | `scripts/cfd/run_synthetic_tank_3d_smoke.py` |
| Public fixture spec | `examples/cfd/synthetic_l_tank/input.yml` |
| Focused tests | `tests/solvers/gmsh_meshing/test_tank_fixture.py`; `tests/solvers/openfoam/test_gmsh_bridge.py`; `tests/solvers/openfoam/test_poly_mesh_contract.py` |
| Execution tests | `tests/solvers/openfoam/test_runner.py`; `tests/solvers/openfoam/validation/test_sloshing_3d.py`; `tests/scripts/test_cfd_python_install.py` |
| Dispatch tests | `tests/scripts/test_dispatch_cfd_run.py` |
| Independent golden | `tests/solvers/openfoam/validation/fixtures/sloshing_3d_case_sha256.json` |
| CI | `.github/workflows/gmsh-meshing-tests.yml` |
| Reduced live evidence | `docs/api/cfd/synthetic-l-tank-smoke.json` |
| Control-plane index | workspace-hub `docs/plans/README.md` |

---

## Deliverable

A public synthetic L-shaped tank will pass a fail-closed, transactional Gmsh-to-
OpenFOAM conversion and complete a short, non-oversubscribed eight-rank VOF machinery
smoke while a versioned reduced manifest links every input, mesh, command, and result
without making a production engineering claim.

---

## Proposed Design and Pseudocode

### 1. Fixture and serialized MSH contract

```text
build_fixture(spec, output):
    require length_unit=m; validate frame, dimensions, members, and openings
    fuse two boxes into one L-shaped fluid volume
    create longitudinal and transverse members
    cut one scale-bounded elliptical through-opening through each member
    subtract both perforated members from the fluid volume; synchronize once
    classify every exterior surface exactly once as walls or atmosphere
    assert no overlap, no unassigned surface, one connected fluid volume
    assign globally unique IDs for fluid, walls, atmosphere
    set MSH2 ASCII, SaveAll=0, order=1, Reproducible=1, RandomSeed=0, threads=1
    assert positive signed tetrahedral determinants; calculate existing shape metrics
    verify tagged triangles/tetrahedra and physical names by reopening serialized MSH
    write through a temporary sibling and atomically promote the source MSH
```

### 2. Reusable y-up 3D case writer and independent golden

```text
characterize_current_writer_before_refactor():
    render the current case at frozen inputs
    store fixed SHA-256 values for every generated dictionary in a committed fixture

write_sloshing_case(case_dir, config, mesh_mode, boundary_contract):
    enforce x-transverse/y-up/z-longitudinal frame
    render every 0/* entry from contract.wall_patches and atmosphere_patch
    assert rendered boundary keys exactly equal the mesh contract before writing
    render deterministic gravity, motion, numerics, and time controls
    write blockMeshDict only for blockMesh mode
    verify output hashes against the pre-refactor golden for benchmark inputs
```

The characterization fixture will land in the initial RED commit before the benchmark
imports the library writer, preventing a circular post-refactor oracle.

### 3. Transactional conversion and exact polyMesh contract

```text
prepare_mesh(fresh_case, source_msh, contract):
    reject symlinks, unsafe names, missing input, or any existing destination polyMesh
    create a same-filesystem staging case with copied source and required dictionaries
    run fixed argv gmshToFoam against the staged source with shell disabled
    generate changeDictionaryDict; run exact argv [changeDictionary, -constant]
        to set walls=wall and atmosphere=patch
    run checkMesh -allGeometry -allTopology
    reject nonzero exit, fatal markers, absent "Mesh OK", or any "Failed N" where N>0
    structurally parse staged boundary and cellZones files
    require exact patches, expected types, nFaces>0, no nonempty defaultFaces,
        complete boundary-face coverage, fluid zone present, fluid cells=all cells
    reject links; SHA-256 tree-hash sorted POSIX paths as
        path NUL size NUL content_sha newline; include regular files only
    atomically promote polyMesh into an absent destination
    atomically write the final manifest last as the sole success marker
    on interruption, leave no completed marker; a retry will reject orphan output
```

The parser will tokenize OpenFOAM lists/dictionaries; it will not use regex-only or
free-form replacement for structural verification.

### 4. Attested prebuilt-mesh runner

```text
run(case, prebuilt_manifest=None):
    if prebuilt_manifest is absent, retain the current mesh utility sequence
    otherwise require completed schema status, matching case/polyMesh tree digest,
        exact patch/cellZone contract, and pinned toolchain metadata
    acquire an exclusive run lock; copy attested inputs into a private unique snapshot
    verify canonical input digest in the snapshot; skip mesh generation only then
    execute there; re-hash protected inputs after the run and fail on mutation
```

### 5. Exact MPI smoke state machine

```text
plan_smoke(ranks, visible_cpus, selected_ranks):
    require 1 <= ranks <= visible_cpus, ranks == selected_ranks, threshold == 1.5
    return bridge, setFields, decomposePar -force,
        mpirun -np ranks interFoam -parallel, reconstructParMesh -latestTime
    forbid --oversubscribe and shell execution

execute_smoke(plan):
    stop on the first failed stage; persist bounded logs and digests
    reject fatal/divergence markers; require End and abs(finalTime-endTime)<=10^-timePrecision
    parse reconstructed coordinates; require max displacement >1e-8*length and
        max rigid-rotation coordinate error <=1e-6*length
    emit completed evidence only after mesh motion and all stages pass
```

### 6. Versioned evidence chain

`schema_version: 1` will carry generic execution class; solve/report status; UTC
timestamps; clean-worktree proof, commit and canonical tracked-source digest; lockfile,
input YAML, source MSH, case-dictionary tree,
`polyMesh` tree, and log digests; exact safe argv; solver/Gmsh/MPI versions; patch,
cellZone, quality, rank, load, step, motion, wall-time, and seconds/step evidence.
Paths will be case-relative. A temporary manifest will atomically replace the final
path only when every required stage and digest validates. Execution will require a clean
remote checkout and will compare the post-run protected-input digest with its snapshot.

The exact preflight will use `dispatch-cfd-run.py --ranks 8 --max-load-per-core 1.5
-- uv run --frozen --extra cfd python scripts/cfd/run_synthetic_tank_3d_smoke.py --ranks {ranks}`. Its existing assumed-probe
tests will pin threshold evaluation, exported rank variables, argv rendering, and the
refusal path before any SSH solve.

---

## Files to Change

| Action | Path | Reason |
|---|---|---|
| Create | `src/digitalmodel/solvers/gmsh_meshing/tank_fixture.py` | domain-specific test fixture and strict MSH2 serialization |
| Create | `src/digitalmodel/solvers/openfoam/poly_mesh_contract.py` | structured patch/cellZone verification |
| Create | `src/digitalmodel/solvers/openfoam/gmsh_bridge.py` | staged conversion, type normalization, QA, and attestation |
| Create | `src/digitalmodel/solvers/openfoam/validation/sloshing_3d.py` | reusable frame-locked case writing |
| Modify | `src/digitalmodel/solvers/openfoam/runner.py` | accept only digest-attested prebuilt meshes |
| Modify | `scripts/cfd/run_sloshing_3d_benchmark.py` | consume writer after independent golden lands; retain benchmark behavior |
| Create | `scripts/cfd/run_synthetic_tank_3d_smoke.py` | tested rank/core/load state machine and live entry point |
| Create | `examples/cfd/synthetic_l_tank/input.yml` | de-identified public fixture |
| Modify | `pyproject.toml`, `uv.lock` | add Linux-safe `cfd` extra without proprietary packages |
| Modify | `scripts/setup/provision-cfd-box.sh`, `scripts/setup/verify-cfd-box.sh` | verify installer SHA, install/hold exact OpenFOAM/OpenMPI packages, use frozen `cfd` extra, and require a clean source tree |
| Modify | `.github/workflows/gmsh-meshing-tests.yml` | replace stale path filters/commands and run current plus new tests |
| Create/modify | test and golden paths in the Artifact Map | TDD, characterization, security, and workflow coverage |
| Create | `docs/api/cfd/synthetic-l-tank-smoke.json` | reduced, digest-linked live evidence |
| Update | workspace-hub `docs/plans/README.md` | canonical control-plane plan index row |

No private geometry, generic CAD ingestion, detached execution, cloud sync, or HTML
report implementation will enter this issue.

---

## TDD Test List

Tests will be committed and observed RED before each implementation slice.

| Area | Required tests |
|---|---|
| Frame/config | require SI metres; reject nonpositive dimensions, unit/frame mismatch, bad openings, and wrong roll/gravity axes |
| OCC topology | one connected fluid volume; longitudinal plus transverse members; two elliptical through-openings; expected boolean volume |
| Surface coverage | every exterior surface belongs to exactly one group; no overlap/unassigned surfaces; groups remain stable at two scales |
| MSH2 | ASCII 2.2, SaveAll off, order one, globally unique IDs, signed determinants, fixed seed/thread/reproducible options, byte-identical repeat build |
| Conversion safety | reject missing/symlink source and existing/orphan polyMesh; fixed argv and shell disabled; failed retry cannot attest stale output |
| checkMesh | reject nonzero/fatal, return-zero `Failed 1 mesh checks`, missing `Mesh OK`, and inconsistent cell/face counts |
| polyMesh | exact patch names/types/sizes, no nonempty `defaultFaces`, boundary-face total match, `fluid` zone covers every cell |
| Contract/transaction | field boundary keys equal parsed mesh patches; canonical hash vectors; stale output/links fail; manifest is last |
| Runner | private locked snapshot, clean source proof, pre/post protected-input digest, tamper failure, unchanged legacy order |
| MPI planner | dispatcher threshold 1.5, exported ranks, exact decompose/MPI/reconstruct argv, visible-CPU ceiling, no oversubscribe |
| MPI execution | stage failure stops; fatal/early-end fails; final time matches; parsed reconstructed coordinates match prescribed motion |
| Golden parity | fixed pre-refactor dictionary hashes remain identical after extraction; benchmark timing/manifest naming tests remain independent |
| Manifest | schema/version, clean commit/tracked-tree and all input/lock/case/mesh/log digests; unsafe metadata and failed completion rejected |
| Provision | installer SHA, exact held apt versions, frozen uv argv, clean-tree gate, syntax/idempotence, fresh Gmsh import, no proprietary dependency |
| CI | current path triggers, current test paths, actionlint, and focused fresh-environment run |

---

## Acceptance Criteria

- [ ] Focused RED/GREEN tests and all existing Gmsh/OpenFOAM tests pass under
  `uv run --frozen --extra cfd pytest`.
- [ ] `bash -n` passes setup scripts; actionlint passes the repaired existing Gmsh
  workflow; a fresh locked environment imports Gmsh without proprietary packages.
- [ ] MSH and polyMesh contracts pass every source/converted topology, type, coverage,
  unit, global-ID, determinism, orientation, `defaultFaces`, and cellZone assertion.
- [ ] `checkMesh` requires `Mesh OK` and zero failed checks even when its exit code is
  zero; a synthetic return-code-zero failure test proves this behavior.
- [ ] Conversion from a fresh case is staged on the same filesystem; stale/orphan
  output cannot receive a completed manifest; promoted tree digests match evidence.
- [ ] Assumed-probe and live dispatcher checks enforce eight ranks and projected
  load/core <=1.5, then pass the exact selected rank to the smoke driver.
- [ ] The dedicated node completes the exact non-oversubscribed eight-rank sequence;
  fatal/early completion is absent, final time equals configured endTime, and parsed
  reconstructed coordinates match the prescribed rotation within scale tolerance.
- [ ] The reduced schema-v1 manifest links all required digests and versions without
  raw fields, absolute paths, addresses, infrastructure labels, or private identifiers.
- [ ] The artifact states methodology/bridge validation only and makes no production
  geometry, convergence, pressure, resonance, or design claim.
- [ ] `scripts/enforcement/check-no-abs-paths.sh --added origin/main`, the workspace legal sanity
  scan, focused tests, and adversarial code/artifact review all pass before merge.
- [ ] #662 receives implementation evidence and links #640, #153, #1495, and #155;
  the #153 stale production-dependency claim receives a corrective issue comment.
- [ ] The plan is indexed in the workspace-hub control-plane README before the issue
  moves to `status:plan-review`.

---

## Adversarial Review Summary

| Review | Verdict | Resolution |
|---|---|---|
| Codex native architecture r1/r2 | MAJOR | revisions 2-3 add transactionality, exact field/mesh contract, immutable snapshot/hash, dispatcher binding, ownership, provisioning, CI, and golden |
| Codex native engineering r1/r2 | MAJOR | revisions 2-3 add mesh-check/type/frame/group/MSH fixes plus numeric motion, semantic completion, exact packages/source, SI units, global IDs, and determinism |
| Claude CLI | UNAVAILABLE | two headless attempts time out; no signal counted |
| Codex CLI | UNAVAILABLE | installed CLI stdin regression; no signal counted |
| Gemini CLI | UNAVAILABLE | no noninteractive auth; no signal counted |

**Overall result:** revision 3 applies round-2 findings inline; no third automatic review
will run per the loop-break rule. Provider diversity remains degraded and will be named
at the user approval checkpoint. No implementation or approval label has moved.

---

## Risks and Open Questions

- `changeDictionary` behavior and the structural parser will require live v2312
  attestation; any patch-type mismatch will block promotion.
- Elliptical OCC cuts can fragment faces; coverage assertions will use final post-boolean
  entities rather than pre-boolean tags.
- Tetrahedral dynamic motion proves machinery only; production mesh strategy,
  convergence, and pressure fidelity remain #640/#1433 work.
- The Gmsh wheel may need host libraries; provisioning will not be accepted until a
  fresh locked environment and import check pass on the target execution image.
- Provider diversity remains degraded. If Claude/Gemini stay unavailable, the final
  approval preview will name that residual review risk rather than claim consensus.
- Standardized CFD HTML reporting will use the schema-v1 evidence in a separately
  planned issue; report code will not expand this bridge PR.

---

## Complexity: T3

The issue crosses geometry, conversion, runner security, provisioning, CI, MPI, and
evidence. After approval, two isolated lanes will converge for integration and review.
