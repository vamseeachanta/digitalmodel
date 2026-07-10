## Verdict

MAJOR

## Retrieval

- Reviewed revision 1 at `529c4ba1`; plan SHA-256 began `8473dac`.
- Inspected current Gmsh, OpenFOAM, benchmark, provisioning, dispatcher, and tests.
- Checked official OpenFOAM v2312 converter/checker behavior and Gmsh MSH2 rules.
- No live local conversion was claimed. No files were edited by the reviewer.

## Findings

1. **MAJOR - checkMesh exit semantics:** v2312 can report failed mesh checks while
   returning zero; fatal-marker and return-code checks alone are not fail-closed.
2. **MAJOR - patch types:** `gmshToFoam` imports boundaries as generic patches; the
   plan neither normalized nor asserted wall/atmosphere types.
3. **MAJOR - coordinate frame:** geometry "top" was undefined while the benchmark
   uses x-transverse, y-up, z-longitudinal/roll and the generic case builder is z-up.
4. **MAJOR - surface coverage:** non-empty named groups did not prove every final OCC
   boundary face was assigned exactly once or that `defaultFaces` was empty.
5. **MAJOR - volume/patch conflation:** `fluid` is a physical volume/cellZone, while
   `walls` and `atmosphere` are surface patches; one flat expected-group API was wrong.
6. **MAJOR - MSH2 contract:** the plan omitted explicit ASCII, SaveAll, element-order,
   physical-ID, and triangle/tetrahedron tag checks.
7. **MAJOR - inverted tetrahedra:** the existing analyzer uses absolute tetrahedron
   volume and cannot establish positive orientation.
8. **MAJOR - issue scope:** one obstruction/opening did not satisfy the longitudinal
   floor, transverse frame, and large oval-opening acceptance surface.
9. **MAJOR - dynamic/MPI proof:** no tests fixed command order, core ceiling, stage
   failures, positive timestep count, no oversubscription, or actual point motion.
10. **MAJOR - characterization:** post-refactor parity would be tautological without
    a pre-refactor golden covering generated dictionaries and execution semantics.
11. **MAJOR - reproducibility:** the manifest omitted schema/version, code/lock/input/
    case/polyMesh/log digests, exact argv, and an atomic success-promotion rule.

## Blockers

- Parse zero-exit mesh-check failures and normalize/verify exact converted topology.
- Freeze one frame across OCC, fill, gravity, and motion.
- Add complete MSH2, signed-orientation, group/zone, dynamic/MPI, and golden tests.
- Define a versioned atomic evidence chain and truly frozen provisioning.
- Resolve the internal-structure scope explicitly.
