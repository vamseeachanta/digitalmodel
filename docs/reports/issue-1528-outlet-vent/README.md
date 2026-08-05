# Issue #1528 slice 7 — outlet/vent placement, measured result

Settles the open slice-7 checklist item *"Settle outlet/vent placement for clean
tank-volume balance"*, which gated the full 144-case coupled matrix.

Issue: https://github.com/vamseeachanta/digitalmodel/issues/1528

## The defect, confirmed in source

`src/digitalmodel/solvers/openfoam/block_mesh.py` emits a single hex block whose
`outlet` patch is the face `(1 2 6 5)` — the entire x=max face, from `z=min` to
`z=max`, i.e. full height. `case_coupling._exchange_boundary_conditions` then
puts `totalPressure` on `p_rgh` and `pressureInletOutletVelocity` on `U` at that
patch.

Submerged cells therefore carry a pressure opening, so a static tank drains
under its own hydrostatic head. This report measures that, rather than asserting
it.

The same mesh already emits a separate `top` patch at `z=max` of type `patch`,
which lies above the free surface for any fill fraction below 1.0. The remedy
needs no mesh-topology change: close `outlet` to a no-slip wall and move the
atmosphere opening to `top`.

## Run configuration

| Item | Value |
|---|---|
| Host | gpu-claw (Tailscale `undi@100.101.237.123`), Ubuntu 24.04.4, 8 ranks |
| OpenFOAM | ESI v2312, `FOAM_API=2312` |
| Interpreter | `/home/undi/ws/digitalmodel/.venv/bin/python`, CPython 3.12.3 |
| Branch SHA run | `04725bbfa58be2c64ee3ca055a6552cf7b4b6619` (detached worktree, clean) |
| Case | `dm1528-filldrain-2a44db9cf998`, fill 0.500, tank 20 x 6 x 10 m |
| Mesh | 40 x 8 x 40 = 12800 cells |
| Solver | `interFoam`, 8 MPI ranks, `endTime` 10.0 s, 1000 timesteps |
| Flow rate | 0.0 m3/s — static hold, so any volume change is boundary leakage |
| Tolerance | 0.1 % |

The run was only possible because #1959 (`2b4b2567`, on `main`) made the emitted
case `interFoam`-runnable. The previous attempt at `dbeb5cc5` died at solver
start-up with `Entry 'cAlpha' not found`.

## Measured result

| Variant | Initial liquid volume | Final liquid volume | Drift | Holds volume |
|---|---|---|---|---|
| `baseline_full_height_outlet` | 599.9037 m3 | 113.0211 m3 | **-81.160 %** | no |
| `vent_top` | 600.0000 m3 | 600.0000 m3 | **0.000 %** | yes |

Verdict: **`vent_top`**. The baseline loses over four fifths of the tank
inventory in ten seconds of a *static* hold with zero imposed flow. The vent
variant holds volume exactly at the printed precision of the `volFieldValue`
output for all 1000 timesteps.

Both variants ran 1000 advancing timesteps from `Time = 0.01` to `Time = 10`,
each ending with a clean `End`, with zero `FOAM FATAL` markers.

## Why the tolerance is not fitted to this data

`DEFAULT_TOLERANCE_PCT` in `scripts/cfd/run_outlet_vent_study.py` is
`100.0 * ExtractionConfig().mass_balance_rtol`, where
`ExtractionConfig.mass_balance_rtol = 1.0e-3` in
`src/digitalmodel/solvers/openfoam/time_history.py`. That is the relative
mass-balance tolerance the repo already applies to decide `mass_balance_ok` for
these same extracted histories, and it predates this study.

The separation between the two variants is roughly three orders of magnitude
either side of the threshold, so the verdict does not depend on the exact value.

## Proof the case tree was not patched

`~/ws/cfd_work/dm1528/patch_case.sh` on gpu-claw overwrites nine generated files
and deletes the RAS fields. All earlier slice-7 evidence came from a tree it had
patched, which is why the builder itself had never been validated. This run
proves independence three ways.

**1. Two independent roots.** The study was built twice: once with `--dry-run`
(builder output only, solver never invoked) into `emitted/`, and once for real
into `run/`. `sha256-emitted.txt` and `sha256-after.txt` hash the builder-emitted
inputs under both.

**2. All 28 non-alpha inputs are byte-identical.** `sha256-diff.txt` shows only
two classes of difference, both benign:

- `postProcessing/liquidVolume/0/volFieldValue.dat` appears only in `run/`. It is
  a *result* file that my path filter caught because its path contains `/0/`. It
  does not exist in the dry-run root because the solver never ran there.
- `0/alpha.water` differs, because `setFields` rewrites it in place. Everything
  else — including `system/fvSolution`, `system/fvSchemes`, `system/controlDict`
  and all of `constant/` — is identical.

**3. The `alpha.water` difference is reproducible from the emitted case.**
Running `blockMesh` then `setFields` on a pristine copy of the `emitted/` case
produced `0/alpha.water` with sha256
`2991019d066fba54c596266e087faf1ad7492121d7cec7b19717c4faea62f988`, which is
exactly the hash of the file `interFoam` consumed in `run/`. The builder emits
`internalField uniform 0`; `setFields` replaces it with the initialised
12800-cell VOF list. No third party wrote that file.

**Independent cross-check.** The consumed `system/fvSolution`
(`131c2c54fcefe51feddaa123697ce7c44c002524f1353bd7302a98067aaf5930`) carries
`cAlpha 1` and `nAlphaSubCycles 1`. `patch_case.sh` writes `cAlpha 1.5`. The
working evidence therefore cannot have come from the patch script. That script's
mtime is unchanged at 2026-07-11.

No value in this report was imported from `patch_case.sh`.

## Files

| File | What it is |
|---|---|
| `manifest-run.json` | Driver manifest with the measured drift and verdict |
| `manifest-dryrun.json` | Build-only manifest from the `emitted/` root |
| `sha256-emitted.txt` | Hashes of builder-emitted inputs, solver never invoked |
| `sha256-after.txt` | Hashes of the inputs `interFoam` actually consumed |
| `sha256-diff.txt` | The two benign differences, explained above |
| `liquidVolume.<variant>.dat` | Full 1000-sample liquid-volume series per variant |
| `log.interFoam.<variant>.excerpt` | Solver banner, first timesteps, clean `End` |
| `driver-run.log` | Full launcher transcript |

Full 4.4 MB solver logs are retained on gpu-claw under
`/home/undi/ws/cfd_work/vent2/run/<variant>/dm1528-filldrain-2a44db9cf998-<variant>/`.

## Confirmation run against the shipped default

The measurement above was taken at `04725bbf`, where the vent configuration was
built inside the study script. That proves the physics but not the shipped
library default, so `case_coupling._exchange_boundary_conditions` was changed to
emit the vent placement and the study was re-run at `90f3ae72`.

At `90f3ae72` the study's roles are inverted in a deliberate way:

- the **treatment** is now literally `_exchange_boundary_conditions`, so the run
  validates what the 144-case matrix will emit rather than a script-local copy;
- the **control** is a frozen literal of the old full-height pressure outlet, so
  it cannot silently track the library and turn the study into a comparison of
  the vent against itself.

Results are bit-identical to the first run, from a different worktree, a
different commit and a different directory:

| Variant | Drift, run 1 (`04725bbf`) | Drift, run 2 (`90f3ae72`) |
|---|---|---|
| `baseline_full_height_outlet` | -81.16012620025514 % | -81.16012620025514 % |
| `vent_top` | 0.0 % | 0.0 % |

Run 2 exited `rc=0`, 1000 timesteps per variant, zero `FOAM FATAL`, and the same
28 non-alpha builder inputs byte-identical between its emitted and consumed
roots. The emitted `0/p_rgh` for the shipped default carries `zeroGradient` on
`outlet` and `totalPressure` on `top`, committed here as `p_rgh.vent_top`.

Artefacts are under `confirmation-shipped-default/`.

## Scope

This settles placement only. The 144-case coupled matrix is the next checklist
item and was deliberately not started.
