# Riser station geometry contract

Coupling expansion accepts an optional caller-supplied resource budget and
validates all section counts before allocating coupling positions. Station-map
coordinates must be finite and sampling distances finite and nonnegative.
These checks establish software input validity. Engineering qualification of
exclusion boundaries and sampling tolerances remains **NOT_EVALUATED**.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| Initial | 2026-10-19 | Define input and expansion contracts and assess unresolved topology to avoid treating rounding tolerance as engineering acceptance. |

## Arithmetic and supported inputs

The arithmetic basis is [OpenStax, *College Algebra 2e* (2021), section 9.2,
Arithmetic Sequences](https://openstax.org/books/college-algebra-2e/pages/9-2-arithmetic-sequences).
The source was opened successfully when preparing this contract. No licensed
standard values are introduced.

For section length `L`, start coordinate `s`, joint length `J` and supplied
whole-joint tolerance `t`, coupling positions inside a multi-joint section are
`s + k*J`. The section endpoint is always `s + L`. The existing expansion rule
expands when `L - J > t`, takes `n = round(L/J)`, and requires the length
residual `abs(L/J - n)*J <= t`. Such a section contributes `n` positions,
including its endpoint; every shorter section contributes its endpoint alone.
This is arithmetic bookkeeping, not a coupling design rule.

`sections_m` retains its existing nonempty ordered-real-input contract: positive
finite lengths and a finite strictly increasing cumulative axis. `joint_length_m`
must be positive and finite; `tol_m` must be nonnegative and finite. Booleans,
strings and non-real inputs are rejected. A non-finite computed joint ratio is
rejected before integer conversion. `joint_counts` returns per-section counts
without enumerating positions. Every section is checked before expansion begins.

`max_couplings`, when supplied to `coupling_positions` or `classify_stations`,
is a positive integer that caps the total returned positions, including section
endpoints and End B. It is a caller-selected resource input, with no default
numerical limit. A budget overrun raises `ValueError` before joint iteration;
the existing CR-10 adapter turns this into `NOT_EVALUATED` for regular and
irregular waves. Validation costs scale with the supplied section count;
expansion costs scale with the coupling count. Classification also scans result
points for each coupling, so this budget alone does not bound elapsed time.

For example, caller inputs `sections_m=[1,6,4]`, `J=2`, `t=0` give positions
`[1]`, `[3,5,7]`, `[9,11]`: counts `[1,3,2]`. A caller budget of `6` permits
these positions; `5` refuses them. This is an independent hand calculation,
not a published engineering example or a recommended resource limit.

`exclude_below_m`, supplied `exclude_above_m`, supplied `hot_spot_m` and each
`pup_side_couplings_m` entry must be finite real coordinates. Sampling distances
`hot_spot_tol_m`, `pup_side_tol_m` and supplied `coupling_tol_m` must be finite
and nonnegative, including when their associated optional feature is inactive.
Zero sampling distance is supported for exact-coordinate samples.

## Exclusion topology assessment

The existing `_ON_TOL` in
[src/digitalmodel/drilling_riser/postprocess/stress_range.py](../../../src/digitalmodel/drilling_riser/postprocess/stress_range.py)
is a stored-arc rounding convention, not a sourced engineering acceptance limit.
The present classification behavior is retained pending a separate topology
contract:

- The lower exclusion boundary exempts all couplings within `_ON_TOL`, while
  the upper exclusion selects a nearest geometry boundary. These rules can
  assign different exterior-side exemptions to geometrically similar spans.
- A sample can fall in the coincidence neighborhoods of multiple couplings.
  For spacing `d` and radius `r`, neighborhoods can overlap when `d <= 2*r`
  by the triangle inequality. A short final section can therefore allow a
  single rounded endpoint sample to represent an interior boundary as well.
- Lower and upper exclusion neighborhoods can overlap. Duplicate-sample
  classification then depends on both boundaries, without explicit side IDs.

The spacing inequality is a geometric derivation, not a new acceptance limit:
if a point is within `r` of both boundaries, their separation is at most `r+r`.
The implementation neither rejects these spans nor claims that they are
qualified. Existing near-End-B regressions intentionally retain that behavior.

## Later slices and limits

Topology qualification requires explicit boundary identities and sample-side
ownership, or a separately justified rejection rule for ambiguous coincidences.
The minimum supported spacing and engineering sampling distances remain required
design inputs; no values are inferred here. Exclusion interval ordering and
extent, ambiguous pup-side ownership and intermediate floating-point coordinate
collapse also require follow-up assessment.

Resource budgets are opt-in for compatibility: omitting `max_couplings` leaves
finite large expansions unbounded. Consumers handling untrusted or compressed
geometry must supply a budget. Adopting a mandatory budget across existing
callers is a later slice. The budget does not cap input iterable length or
sample count and does not establish numerical representability of every
expanded intermediate coordinate.

## Test evidence

`tests/drilling_riser/postprocess/test_geometry_contract.py` records hand-derived
positions, counts and residuals in test docstrings. It checks finite scalar
validation, exact and exceeded budgets, preflight before any joint iteration,
non-representable ratios, zero sampling distances, and CR-10 `NOT_EVALUATED`
propagation. The existing postprocess tests cover compatibility. No solver is
started or imported by these checks.
