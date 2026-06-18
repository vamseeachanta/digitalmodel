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

Diffraction, scoped first per the #801 decision:

- `fpso-design-draft`
- `fpso-ballast-draft`
- `semisub-operating`

Extending the map = adding a case (one licensed run) here. OrcaFlex follows the
same model (canonical sea-state × heading cases for a reference model) once the
diffraction library is proven.

## Not wired into `refresh`

Library atlases are not in the `refresh` content-fingerprint iteration: they are
not code-computable, so `refresh --apply` cannot regenerate them. They refresh
when the operator re-runs the solver. (A follow-up could fingerprint a library
against its solver version + case set so a stale library is detectable.)
