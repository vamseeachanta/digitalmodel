# Licensed-solver sparse atlas libraries (#801)

> Epic: digitalmodel#794 (Parametric Refresh) · slice #801

## Why a *library*, not a dense atlas

The offline workflows get dense grids because a run is effectively free. The
licensed solvers — AQWA / OrcaWave diffraction and OrcaFlex — cost a license
plus minutes per run, so they cannot be densely gridded. Instead an atlas is a
**sparse library** of canonical cases solved once on the licensed machine, with
an explicit **coverage map**. A query either matches a covered case or escalates.

## Coverage model: exact key + interpolate-within

- A categorical **key axis** (vessel/draft, or model/config) is matched
  **exactly**. A value not in the enumerated coverage map **escalates by
  default** — the library never interpolates *across* cases.
- The **continuous axes within a covered case** (frequency × heading for
  diffraction) interpolate between the solver's grid points.
- A continuous query outside a case's solved range also escalates.

This is exactly the regular `Atlas` behaviour (one categorical axis +
continuous axes), so `library.py` only adds the build-from-recorded-output path
and the licensed-run provenance — the query path is shared (`_handle_value`).

## What ships vs. what's gated

| Piece | Status |
|---|---|
| `parametric/library.py` — build an atlas from recorded solver output | shipped |
| Coverage-map + exact-key/interpolate-within query semantics | shipped, tested |
| `scripts/parametric/build_diffraction_library.py` — diffraction heave-RAO library | shipped **as a STUB** |
| `diffraction-library-query` registered workflow + bot-routable query | shipped |
| **Real values** — a licensed OrcaWave/AQWA run per canonical case | **operator step (gated)** |

The committed library carries `solver.licensed = false` and a `STUB` version in
its provenance; the stub heave-RAO is a transparent single-DOF placeholder, not
a hydrodynamic result.

## Operator runbook to populate a real library

1. On the licensed machine (dev-secondary, per the CFD/OrcaWave convention),
   run diffraction for each canonical case in `build_diffraction_library.py`
   `CASES` — one run yields the full frequency × heading response for that
   vessel/draft.
2. Replace the `_stub_heave_rao(...)` values with the solved RAO (and add other
   responses — added mass, damping, excitation — as columns if wanted).
3. Set the `solver` block to the real name/version/run-date/licensed=true.
4. Re-run the script to regenerate the library; commit the atlas.

## Coverage map (current canonical cases — for sign-off)

Diffraction (#801), key = vessel/draft, interpolate freq × heading within:

- `fpso-design-draft`
- `fpso-ballast-draft`
- `semisub-operating`

OrcaFlex (#832), key = load case, interpolate heading within; response = max
effective line tension (`scripts/parametric/build_orcaflex_library.py`):

- `operating-hs2`
- `storm-hs5`
- `extreme-hs8`

Extending a map = adding a case (one licensed run) to both the builder and the
matching `refresh.LIBRARY_EXPECTATIONS` entry.

## Staleness (#831)

Library atlases are not code-computable, so `refresh --apply` never regenerates
them. Instead their staleness is judged against an operator-declared
expectation in `refresh.LIBRARY_EXPECTATIONS` (solver name + version + covered
case set):

- `refresh --check` **reports** each library (`[ok]` / `[STALE]`) and exits 1 if
  any library is stale, but never auto-builds it ("licensed run required").
- A query against a stale library **escalates** (the staleness gate in
  `query._staleness`), exactly like a stale computed atlas.

The shipped library is a STUB; its expectation also says `version: STUB`, so it
reads current. When the operator runs real diffraction and bumps the expected
`solver_version`, the stub library immediately reads **stale** → queries
escalate → which prompts populating the real library. Extending coverage = add
the case to both the builder and the expectation.
