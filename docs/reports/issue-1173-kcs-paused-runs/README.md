# KCS benchmark runs — paused 2026-08-20, resume record

Both KCS validation solves were **paused by operator request** to free the
compute hosts for client work. Neither converged. **Do not read either stop as
a result** — the ITTC criterion was never met on either case.

Nothing is lost: both stopped cleanly under `stopAt writeNow`, logged `End` and
`Finalising parallel run`, and wrote a complete field set to every rank. The
`processor*` decompositions are untouched, and each `controlDict` already
carries `startFrom latestTime`.

## State at pause

| Case | Host | Ranks | Cells | Latest written time |
|---|---|---|---|---|
| `kcs_fine` | gpu-claw | 8 | — | **5024** |
| `kcs_prod_yplus` | ace-linux-2 | 16 | 1,626,151 | **9913** |

## To resume

Replace `NRANKS` and the case path per the table above. The rank count **must**
match the existing `processor*` decomposition — 8 for `kcs_fine`, 16 for
`kcs_prod_yplus`. Decomposing again would discard the paused fields.

```bash
cd "$HOME/cfd/dm1173/kcs_cases/<case>"          # gpu-claw: kcs_fine
                                                 # ace-linux-2: ~/cfd/dm1173/kcs_prod_yplus
source /usr/lib/openfoam/openfoam2312/etc/bashrc   # no set -e / set -u; it trips both
setsid nohup mpirun -np NRANKS interFoam -parallel \
  < /dev/null > log.interFoam.resume 2>&1 &
```

Prefer `scripts/cfd/solve_case.sh`, which does the above plus the start
verification and supervisor arming, and takes ranks from the chain config
rather than a hand-substituted placeholder.

## Two defects in the on-host pause notes

The `PAUSED.md` files as found on each host are archived here verbatim
(`PAUSED-*.as-found.md`). Both carry defects, which is why this README exists
and supersedes them:

1. **`$LAST` was never expanded.** Both notes read "wrote its fields at
   iteration `$LAST`" literally, so neither states its own resume point — the
   value was recoverable only by listing `processor0/`. The real values are in
   the table above.
2. **`-np NRANKS` is a literal placeholder**, and the notes point at
   `~/cfd/dm1173/ittc_watch.sh` — the superseded first-generation watcher, not
   the repo copy. The stale copy is the one carrying the coefficient-file
   hazard described below.

## Known trap on resume: the watcher's coefficient file

A resumed run does **not** append to `coefficient.dat`; it writes a new
`coefficient_<startTime>.dat`. `scripts/cfd/ittc_watch.sh` resolves the file
with `ls -t | head -1`, i.e. newest by mtime, which is correct **once the
resumed run has written its first row**.

It is not correct in the window before that. During startup the newest file is
still the *previous* run's, and that file holds a full converged-looking
history — so a watcher armed too early can read the old run's data and issue a
verdict about a run that has barely started. `kcs_fine` is already exposed: it
holds both `coefficient_0.dat` and `coefficient.dat`, and a resume adds a
third.

Until that is fixed in the watcher, arm the watcher only after confirming the
resumed run has written a coefficient row. Follow-up tracked separately; the
live client cases are not exposed (each has a single coefficient file).

## `kcs_prod_yplus-mesh-logs/`

The mesh under solve on ace-linux-2 was **built on gpu-claw and transferred**,
so the ace-linux-2 case has no mesh logs at all. These are the sole build
record for it. Verified as that mesh, not assumed: `log.snappyHexMesh`
("Layer mesh : cells:1626151 faces:4987086 points:1736971") and `log.checkMesh`
match the live `constant/polyMesh/owner` header on ace-linux-2 field-for-field
(nPoints 1736971, nCells 1626151, nFaces 4987086).

Refs #1173, #2023.
