# Wave beach damping coefficient guard

## Summary

The standalone limiter and C++ insertion fragment implement the per-cell
coefficient policy proposed in issue #2046. They constrain the rate used by a
directional momentum sink and expose clipping. They do not establish wave
absorption performance or stability of the coupled flow solution.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 1 | 2026-10-19 | Add a timestep-aware rate limiter and an insertion fragment so local Euler cells cannot silently use the global timestep. |

## Coefficient policy and sources

Let `s = f*b` be the raw spatially blended damping rate in inverse seconds.
The caller supplies a dimensionless cap `c`, strictly between zero and unity.
For each selected cell, apply

```text
r = rDeltaT[cell]              for localEuler
r = 1/deltaT                  for Euler
s_eff = min(s, c*r)
```

The reciprocal timestep definitions follow OpenFOAM v2312 (2023),
[`localEulerDdt.C`, `localRDeltaT`, lines 47–55](https://api.openfoam.com/2312/localEulerDdt_8C_source.html).
The clipping equation is the engineering policy proposed in issue #2046
(2026), Fix item 1. That issue proposes `c = 0.5`; this implementation makes
the cap a required input rather than treating that proposal as a calibrated
physical coefficient. No beach strength, spatial profile, wave frequency or
acceptance threshold is inferred from it.

For context, the scalar Euler test equation has amplification `1 + lambda*dt`:
[*Fundamentals of Numerical Computation*, online edition, §11.3, equations
11.3.3 and 11.3.5–7](https://fncbook.com/absstab-diffusion/) (undated source,
accessed 2026). Setting `lambda = -s` gives `1 - s*dt`. This explains why a
small global-step scalar test does not exercise a large local timestep.
It is not the amplification factor of the coupled momentum matrix or its
explicit tangential correction; no stability bound for that matrix is claimed.

## Python use

```python
from digitalmodel.solvers.openfoam.beach_damping import limit_beach_rates

# Rates and reciprocal steps must refer to the same selected cells, in order.
result = limit_beach_rates(
    raw_zone_rates,
    time_scheme="localEuler",
    r_delta_t=zone_reciprocal_steps,
    max_damping_fraction=chosen_cap,
)
effective_zone_rates = result.effective_rates
```

For Euler, supply `delta_t` in seconds and omit `r_delta_t`. Missing local
steps are an error under local Euler, even when a global step is available.
Rates must be finite and nonnegative, and reciprocal steps finite and positive.

`clip_fraction` is the number of clipped cells divided by the selected-cell
count. Equality with the cap is not clipping. Empty selections have undefined
fraction and maximum, represented by `None`. To combine partition results,
sum counts before dividing; do not average partition fractions. This is a
cell-count diagnostic, not a volume-weighted absorption metric. No threshold
classifying a beach as excessively clipped is supplied.

## Inserting the C++ fragment

`render_beach_limiter(max_damping_fraction=chosen_cap)` returns the contents
of the packaged `templates/beach/limitBeachRates.H` with the supplied cap.
This is an insertion fragment, not a complete `fvOptions` dictionary.

Within an existing v2312 `vectorCodedSource` momentum `codeAddSupRho` callback:

1. Add `fvCFD.H`, `PstreamReduceOps.H` and `<cmath>` to `codeInclude`.
2. Compute a mesh-sized `scalarField beachRate` containing raw `f*b` in
   inverse seconds. Retain the source's existing `cells_` zone selection.
3. Insert the rendered fragment after the raw rate is computed and before
   density weighting or source assembly. The callback must provide `rho`
   and `eqn` with their usual v2312 meanings.
4. Use `beachEffectiveRate` in **both** the implicit coefficient and explicit
   tangential add-back. Do not retain the raw rate in either term.

The fragment queries the density-weighted momentum scheme using the field
names, following v2312 (2023),
[`fvmDdt.C`, `ddt(rho, vf)`, lines 90–100](https://api.openfoam.com/2312/fvmDdt_8C_source.html).
Thus a field-specific momentum override takes precedence over `default`.
It accepts Euler and local Euler only. Euler uses the global timestep and
ignores a stale registered local field. Local Euler requires `rDeltaT` with
inverse-time dimensions. The v2312
[`objectRegistry` API](https://api.openfoam.com/2312/classFoam_1_1objectRegistry.html)
still supports `lookupObjectPtr`; the API marks that method deprecated.

Each callback reduces clipped and selected counts across all partitions,
then prints the global fraction and maximum effective `s_eff/r` on the master
process. Empty global selections print `NOT_EVALUATED`. Rates outside the
selection remain zero. This callback emits a diagnostic every time it runs;
the calling source controls how often it is invoked.

## Verification and remaining work

Policy tests calculate expected limits independently in their docstrings.
The C++ arithmetic harness compiles the rendered fragment and checks local
and global steps, noncontiguous cells, empty selections and invalid inputs.
Its incompatible default scheme also checks that the fragment queries the
momentum field scheme. Harness stubs do not implement OpenFOAM or MPI.

The original beach source referenced by issue #2046 is outside this repository
and is not modified. The following remain `NOT_EVALUATED`: compilation against
OpenFOAM, execution with decomposed meshes, a local Euler solver smoke test,
and wave absorption performance. The fragment targets the density-weighted
momentum callback outside alpha subcycling; other source signatures and time
schemes are outside its contract. The later slice is integration into the
actual beach source followed by Euler and local Euler solver runs and an
absorption assessment using caller-specified criteria.
