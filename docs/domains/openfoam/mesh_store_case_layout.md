# Campaign layout and the master mesh store

Adopted 2026-09-04 for the calm-water resistance programme (lane-A and lane-B).
Mirrors the OrcaFlex modular-generator pattern: one master model, `includes/`
that variations reference rather than copy, a campaign file naming the
variations. Code: `scripts/cfd/mesh_store.sh` (solve-host runtime),
`digitalmodel.solvers.openfoam.mesh_store` (library twin, tested against the
runtime), `scripts/cfd/stage45_driver.sh` (mesh phase: find -> link, or
build -> promote), `scripts/cfd/prune_case.sh`, `scripts/cfd/deploy_lane.sh`.

## The problem this fixes

Every case carried its own copy of the mesh, and the mesh was copied again
for every sibling variant. On adoption day one campaign had 9 mesh families
duplicated byte for byte across 2-3 cases each, 8.0 GB of redundant mesh in
a 31 GB tree, and mesh reuse happened only by accident: copying a meshed case
with its logs made `runApplication` skip every mesh stage because the log
already existed. A snappyHexMesh build costs 12-14 min for a double-body
hull at 2 M cells, 31 min at 4.4 M, 75 min for a 6 M-cell free-surface mesh.

## Layout

```
<campaign root>/                     CONTROL PLANE, identical on every lane
  <registry>.yml                     case registry (lane-specific rank counts; never synced)
  db_job.sh, db_job_matrix.sh        lane entry points (canonical AT THE ROOT on every lane)
  cfd-status*.sh, status/            status pull read by the control surface
  <case>.log, <case>.marker,         RUN LEDGER: written by the job scripts and the solve chain,
  queue_*.log, queue_ledger.txt      read by the control surface's gate scripts. Stays at the root.
  geometry/                          hull STL (+.gz), hull_manifest.json: hashed into mesh identity
  meshes/                            MASTER MESH STORE, one entry per mesh identity
    <id12>-<tag>/
      polyMesh/                      serial mesh, chmod a-w
      inputs/system/*Dict            the dicts that built it (diff target for a mismatch)
      logs/log.snappyHexMesh ...     build logs; gates read these
      mesh_provenance.json           identity, per-input sha256, cells, checkMesh verdict,
                                     hull layer coverage, host, OpenFOAM version, source case
  cases/<case>/
      system/                        solve dicts + the meshing dicts (identity inputs)
      constant/polyMesh -> ../../../meshes/<id>-<tag>/polyMesh    (relative symlink)
      constant/{transportProperties,turbulenceProperties,triSurface/}
      0.orig/, 0/, processor*/       fields, decomposition, results: OWNED BY THE CASE
      log.*, postProcessing/, TIMING.csv, driver.log, case_provenance.json, PRUNED.md
  scripts/                           chain library + helpers, deployed from this repo
  stage/                             generator inputs and staged-but-unlaunched cases; never solved in place
  docs/, rejected/, runs/
```

Rule for the root: only registries, lane entry points, the ledger and status
live there. Helpers go to scripts/, geometry to geometry/, cases to cases/.
Lane-specific exceptions (a study whose remote root is hard-wired by the
control surface) are documented in the campaign's own docs, not moved.

## Mesh identity

`mesh_store.sh id <case>` and `mesh_identity(case)` are sha256 over, in
fixed order:

- `system/blockMeshDict`, `surfaceFeatureExtractDict`, `snappyHexMeshDict`,
  `meshQualityDict`, `refineMeshDict`, then `topoSetDict.N` in numeric order,
  with `//` and single-line `/* */` comments removed, runs of blanks
  collapsed, blank lines dropped;
- `constant/triSurface/*.stl`, raw bytes, sorted by name.

NOT inputs: `decomposeParDict`, `controlDict`, `fvSchemes`, `fvSolution`,
`0.orig`, transport and turbulence dicts. Those change the solve, not the
mesh. Validation on adoption day: the identity partitioned 26 meshed cases
into exactly the groups a byte-level checksum of the built meshes gave.

The bash runtime and the Python library implement the rule independently;
`tests/solvers/openfoam/test_mesh_store.py` runs both on one fixture case and
asserts equal hashes. Change the rule in both or in neither.

## What is shared and what is not

**Shared:** the serial `constant/polyMesh`, read-only in the store.

**Not shared:** `processor*/constant/polyMesh`. Measured on lane-A,
`redistributePar -decompose` is not reproducible run to run: sibling cases
with byte-identical serial meshes and identical hierarchical 2x2x2
decomposeParDicts came out with processor-0 cell counts of 264122, 264030
and 261928. A case's processor time directories are numbered in its own
processor cell order, so a decomposition belongs to the results it holds. It
is rebuilt per case in about 15 s (redistributePar 11 s + renumberMesh 4 s).
Disk cost about 300 MB per 2 M-cell case; correctness wins.

## Driver flow (stage45_driver.sh, mesh phase)

1. If `constant/polyMesh` is a link, remove the link (never the target).
2. `mesh_store.sh find <case>`: hash the inputs, look for `meshes/<id>-*`.
3. Found and `DM_CFD_MESH_REUSE` is not 0: link, copy the store's build logs
   into the case so the layer-coverage and checkMesh gates read the evidence
   the original build produced, record `mesh.reuse,0,0` in TIMING.csv, skip
   surfaceFeatureExtract through checkMesh.
4. Not found: build as before, then `mesh_store.sh promote <case> <name>`
   moves the serial mesh into the store and leaves the link.
   `DM_CFD_MESH_PROMOTE=0` keeps a mesh private.
5. restore0Dir, setFields, decomposition, renumberMesh: unchanged, per case.

## Rules

- Never write through the link. `snappyHexMesh -overwrite`, `refineMesh
  -overwrite`, `topoSet`, `setSet`, `createPatch`, serial `renumberMesh
  -overwrite` write into `constant/polyMesh`. The store is read-only so they
  fail loudly rather than rewriting every sibling. Run zone-building
  `topoSet` before promote, or on a case that has unlinked and copied.
- A meshing dict change forks the identity and builds a new master. A solve
  dict change does not; the driver links. `mesh_store.sh verify <case>`
  reports a case whose dicts drifted from the master it links to.
- A master with no linked cases is a candidate for `drop`. `status` shows
  the linkage. Tag = the case that first built the mesh; the 12-hex prefix
  is what `find` matches on.
- Running builds are never migrated. Promote only after the chain finished.

## Pruning assessed cases

`scripts/cfd/prune_case.sh [--apply] <case>...` removes the program
artefacts of a case whose result is harvested and whose condition is
superseded: `constant/polyMesh` (a link is removed as a link), feature-edge
meshes, `processor*`, numeric time directories, dynamicCode. It keeps
system/, constant dicts and STLs, 0.orig, every log, TIMING.csv and
postProcessing, and writes PRUNED.md. Dry run unless `--apply`. It refuses a
case in which an OpenFOAM or MPI process has its working directory, checked
by `/proc/<pid>/cwd`, never by `pgrep -f <path>`: that matches the command
line of whoever invoked the script with the path. On adoption day the prune
of 28 assessed double-body and KCS cases took a lane from 73 GB to 96 GB
free; a second lane's wave-tank studies went 5.9 GB to 1.6 GB after gzip
of the reducer's column files.

## Lanes and deployment

`scripts/cfd/deploy_lane.sh <user@host> [campaign]` rsyncs `scripts/cfd/`
to `<host>:~/cfd/<campaign>/scripts/` after checking, by exact process
name, that no solver or mesher is running there. Registries and cases are
never deployed by it. Every lane runs the same scripts; a lane-local edit is
a bug until it is committed here and redeployed.

## Disk practice

- `writeFormat binary` everywhere; `writeCompression off` (binary fields
  compress poorly and gzip costs write time).
- `purgeWrite 2` on new steady cases (Pawsey guidance is 1 for steady
  state); `purgeWrite 4` on LTS free-surface runs so the restart point is
  always among the kept writes.
- Finished parallel cases that have been reconstructed can drop
  `processor*`; unreconstructed ones keep it (the results live there).
- `decomposePar` parses and expands zero-directory fields; the driver uses
  restore0Dir before decomposition. Keep it that way.

## Sources consulted

- OpenFOAM v12 user guide, case management tools:
  https://doc.cfd.direct/openfoam/user-guide-v12/case-management
- OpenFOAM v12 user guide, time and data I/O control:
  https://doc.cfd.direct/openfoam/user-guide-v12/controldict
- Pawsey Supercomputing Centre, OpenFOAM best practices:
  https://pawsey.atlassian.net/wiki/spaces/US/pages/51925980/OpenFOAM:+Best+Practices
- CFD Engine, "Fun with zero directories": https://cfdengine.com/newsletter/067/
- CaseFOAM parametric-study layout: https://casefoam.readthedocs.io/
- snappyWiki, parallel meshing workflow: https://sites.google.com/site/snappywiki/snappyhexmesh
- OpenFOAM.com renumberMesh manual:
  https://www.openfoam.com/documentation/guides/latest/man/renumberMesh.html
