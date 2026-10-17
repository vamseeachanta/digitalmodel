# Four-point single-hook skew screening

Experimental workflow for issue #2667, following the merged skew-factor screen
(PR #2678, squash `6bcacbc6`). The screen supplies the shared four-leg minimum;
this calculator supplies the indeterminate load split and CoG share.

Run from the repository root:

```sh
uv run --no-sources python -m digitalmodel.marine_ops.installation.four_point_skew examples/workflows/four-point-skew/input.yml
```

The standalone entry point writes `results/skew_summary.json` beside the input.
It avoids the general engine's unrelated solver adapters. The registry records
the invocation and the dedicated executable workflow test. `runtime: standalone`
means the dedicated test executes this entry point; the general engine registry
test does not execute it. No licensed solver is needed.

## Inputs and scope

All inputs use SI units: N, m, and N/m. Weight is a force, not a mass. The sample
uses 100,000 kg multiplied by 9.81 m/s², matching the issue's illustration.

- Supply four horizontal, coplanar lift-point coordinates in convex perimeter
  order. Opposite sling pairs are indices (0,2) and (1,3).
- `hook_height_m` is an absolute z coordinate in the lift-point coordinate frame.
  A hook point H* is fitted once in the payload frame from the reference sling
  lengths. At each `cog_envelope_xy_m` corner the rigid payload rotates so that
  H* minus CoG is vertical. CoG z is assumed to equal the nominal eye plane;
  other CoG heights are outside this input model.
- Each `sling_EA_N` and `sling_length_m` specifies its axial spring compliance
  L/EA. Gauss–Newton fits H* to the four hook-to-eye lengths; initial misfit is
  `L_i - |H* - P_i|`, reused at every CoG corner. Equal reference slings therefore
  retain zero initial misfit at diagonal offsets. A fitted height differing from
  `hook_height_m` by more than 0.01 times the shortest sling length is rejected.
  Reference lengths affect both stiffness and misfit.
- `general_length_m` is a scalar or four absolute ±length-error bounds, each
  strictly positive. The default is ±0.01 m on each leg; supply verified
  fabrication bounds for the lift.
  `padeye_elevation_m` is a scalar or four independent ±elevation-error bounds.
  Its default is ±0.001 m. Supply measured elevation uncertainty, or explicitly
  set zero for ideal elevation (as in the sample). Positive elevation is along world vertical and reduces sling
  extension at fixed pose.
- Optional `matched_pair_difference_m` limits the absolute difference of the
  two length errors **within each opposite pair**, in addition to the general
  bounds. Supply general bounds explicitly when using matching. Matching does
  not limit the difference **between** pairs. This is the narrower, explicit
  interpretation chosen here; other manufacturing matching conventions require
  converting the certificate into these bounds before use.
- For a symmetric matched set certified with a maximum **leg-to-leg** difference
  Δ, use `general_length_m = Δ/2`. The worst short-to-long difference is then
  `2 × general_length_m = Δ`. Using ±Δ instead would permit a 2Δ spread.
  Opposite-pair matching alone cannot reduce the symmetric diagonal worst case;
  its two pairs can still sit at opposite absolute bounds. This conversion
  assumes the common length is the midpoint; retain any separate mean-length
  uncertainty in the reference lengths.
- `code_minimum_skl` defaults to, and cannot fall below, the shared four-leg value
  **1.33** read from PR #2678's analytical `installation_analysis` module.
  Its citation is DNVGL-ST-E273, *2.7-3 Portable offshore units*, Edition April
  2016, §3.5.5 (skew effects §3.5.4), pp. 34–35. The cited minimum assumes a
  four-leg matched set without spreaders in that standard's scope; this floor
  does not establish applicability to every lift. Higher minima require an explicit
  `code_minimum_source`; a source supplied without a value is rejected.
  Plain and annotated literal assignments are read without importing the solver
  package initializer. Missing or invalid basis literals raise a named error.

The model uses finite rigid-body rotation for the nominal pose and linear axial
spring compatibility about that pose. Sling extension, tolerance displacements,
and the optional vertical warp mode do not update directions. Noncoplanar
nominal eyes, nonconvex ordering, horizontal applied loads and CoG outside the
support polygon are rejected. Material nonlinearity, friction, dynamics,
strength, payload bending/racking and lift stability require separate analysis.

Every case reports `tilt_deg = atan(|CoG_xy - H*_xy| / clearance)` in degrees
and `tilt_exceeds_3_deg`. The summary also flags any tilt above 3°. The previous
level-geometry calculation at CoG (0.7, 0) gave about 331.6 kN versus 338.0 kN
from finite-rotation body statics (about 2% low at approximately 4.7° tilt).
The rigid rotation now removes that discrepancy: the computed angle is 4.62°,
and the regression compares every leg to exact stretched-length body statics
within 0.3%. This is a checked example, not an error bound for arbitrary geometry;
the >3° flag still calls for a separate large-displacement assessment.

## Equilibrium, compatibility and slack

Let `u_i = (H* - P_i)/|H* - P_i|` in payload axes and let
`g = (H* - CoG)/|H* - CoG|` be world vertical expressed in those axes.
The equilibrium equations are `A T = W g`, where A has columns u_i, and
`c_i = u_i · g` gives vertical load share. Rigidly rotating this configuration
puts hook-to-CoG on world vertical while preserving all four geometric sling
lengths. Concurrent sling forces automatically satisfy moment equilibrium about
the CoG since `(H* - CoG) × (W g) = 0`. The single self-stress mode n is the
nullspace of A, normalized to maximum absolute component 1.

Every equilibrated tension state is `T = p + n alpha`, with p a particular
equilibrium solution. Nonnegative tensions give a closed feasible interval for
alpha. Minimize complementary energy `0.5 Tᵀ C T + eᵀ T` on that interval:

```text
C_rigid = diag(L_i / EA_i)
e_i = L_i - |H* - P_i| + length_error_i + c_i elevation_error_i
alpha_free = -nᵀ(C p + e) / (nᵀ C n)
alpha = clip(alpha_free, feasible_lower, feasible_upper)
```

This is the exact tension-only active-set solution for the single self-stress
mode. At an interval endpoint a leg becomes slack; the exact symmetric two-leg
diagonal limit remains solvable. There is no singular three-by-three solve after
dropping two legs, and negative tensions are never reported. A post-solve
equilibrium residual check rejects failed solutions. `slack` includes zero load
within 10⁻⁹ W, a numerical flag rather than a dynamic slack/snap criterion.

All vertices of the length-bound polytope, including the matched-pair constraints,
are crossed with the elevation-box vertices and each supplied CoG corner.
The JSON retains every case, its input errors, four tensions, vertical shares,
nominal tensions and slack flags. For fixed CoG each tension is an affine function
of one projected misfit, clipped to its feasible interval, so tolerance vertices
cover its extrema. CoG evaluation covers the **supplied corners only**; no claim
of a continuous CoG-envelope maximum or an optimized hook location is made.

Each leg is compared with its own zero-tolerance nominal tension at the same CoG.
`effective_skl = max(1, max_cases,i(T_i / T_nominal,i))`. This keeps the CoG share
separate from tolerance skew. An initially slack nominal leg makes that
multiplicative comparison undefined, so the envelope is rejected; `solve_case`
can still compute that individual static state. `design_skl` is always
`max(code_minimum_skl, effective_skl)` for an accepted envelope. `design_leg_tensions_N` gives, for each leg, the maximum nominal tension over
all supplied CoG corners multiplied by the global design factor.
`max_nominal_vertical_share` is the largest nominal vertical share over those
corners and feeds PR #2678's `most_loaded_share`. `governing_cog_xy_m` identifies
the corner governing that share; `governing_case_index` separately identifies
tolerance SKL. For the centred example these are 0.25 and 433190.307 N per design
leg. Multiplying the factor by an equal split would discard the CoG calculation.

## Payload warp stiffness and checked series derivation

Optional `payload_warp_stiffness_N_per_m` is a **normalized vertical warp
stiffness**, not torsional stiffness in Nm/rad. It is limited to a rectangular
layout and a centred CoG. Omitting it means rigid payload. A physical torsional
stiffness must be mapped to this displacement mode and independently checked
before it can be supplied here.

For a symmetric rectangle n = [1,-1,1,-1], write the vertical warp as `w_i=n_i q`.
Define its strain energy as `0.5 (4 K_w) q²`. The modal load is `sum(n_i c_i T_i)`,
so `q = sum(n_i c_i T_i)/(4 K_w)`. Projecting this displacement back into the
sling directions gives:

```text
C_payload = (c*n)(c*n)ᵀ / ((nᵀ n) K_w)
C = C_rigid + C_payload
```

For equal EA/L, equal c and diagonal errors ±tau, the nominal tension drops out
of the modal compatibility equation. With extra tension delta in the short pair:

```text
tau = delta L/EA + c q
q = c delta/K_w
delta = tau / (L/EA + c²/K_w)
```

This verifies the issue's expression **under this stated normalization**.
For a stiffness measured with one diagonal pinned, the displacement definition
differs and the scalar cannot be transferred without conversion. This simplified
mode does not reproduce a flexible frame's complete structural response.

## Verification register

The following independent checks are executable in `tests/marine_ops/installation/test_four_point_skew.py`.

| Check | Independent expected result | Acceptance |
| --- | --- | --- |
| Hand 1: rigid 100 t, 30° from vertical, EA=150 MN, L=10 m | Nominal 283190.307 N/leg; delta=15000 N/mm until the other pair is slack | Within 0.1%; ±5,10,15,19,25 mm |
| Hand 2: same lift, ±10 mm, K_w=15 MN/m | Series compliance 1.166666667×10⁻⁷ m/N; delta=85714.286 N | Within numerical roundoff |
| Zero tolerance | 25% vertical load per leg | Numerical roundoff |
| Offset CoG x=0.7 m | Full body-frame force and moment equilibrium; max tension ≈338.0 kN | Balance to roundoff; finite-rotation reference within 0.3% |
| Diagonal CoG (0.5,0.5) and (0.7,0.7) m | Independent exact stretched-length body statics (three force equations, no nullspace method) | Every leg within 0.3%; review reference at (0.5,0.5): [202.1,282.1,362.0,282.1] kN |
| Published two-supporting-leg limit, 10,000 lbf, 60° from horizontal | 5,775 lbf per supporting leg, as rounded in the catalog | Within 0.1%; other diagonal slack |
| Code floor | Default/floor equals the screen module value; supplied higher minimum governs | Exact max rule; source-only input and custom minimum without a source rejected |

The generated ±10 mm example has 16 length corners and effective/design SKL
1.529679; the short diagonal carries about 76.484% of the weight. The hand case
at ±19 mm reaches the two-leg limit (SKL 2). Additional regressions cover uneven
EA, nominal lengths, raised-eye signs, warp stiffness monotonicity and rigid
limit, translated/rotated coordinates, matched-pair polytope vertices, multiple
CoG corners and invalid/undefined inputs. An oblique quadrilateral with offset
CoG is checked against an independent displacement/stiffness solution and full
3D force/moment balance. A single-slack-leg case is checked against a determinate
three-leg balance and the inactive spring gap inequality.

The published example verifies inclined equilibrium in the two-supporting-leg
limit. It does **not** independently validate the indeterminate spring solution
or flexible-payload approximation; the two hand calculations address those.
No claim is made to reproduce the journal article's complete frame analyses.

## References

1. Sharrow Lifting Products, *SLP Sling Catalog*, undated online edition,
   printed p. 93 (web viewer page 99), “Sling Considerations — Effects of Sling
   Angles”, “Example of the effect of Sling Angle on Tension”. Published 10,000 lbf
   two-leg example: 60° from horizontal, multiplier 1.155, 5,775 lbf/leg.
   [Publisher's worked example](https://www.sharrowlifting.com/wp-content/themes/sharrowliftingproducts/assets/flipbook/files/simple/99.html),
   accessed 2026-10-17. Exact sine evaluation gives 5,773.503 lbf, within the
   stated rounding tolerance. Conversion: 1 lbf = 4.4482216152605 N.
2. AISC, “Variation of Lift Load Distribution Due to Sling Length Tolerance”,
   *Engineering Journal*, volume 26, number 3, third quarter 1989, pp. 96–101,
   “Computer Model and Analyses”, “Design Methods”, and Fig. 3.
   [Publisher's full article](https://ej.aisc.org/index.php/engj/article/download/524/523),
   accessed 2026-10-17. Context for length-error analysis and the distinction
   between structural flexibility and sling stiffness; no code values are
   imported from this historical discussion.

3. DNVGL-ST-E273, *2.7-3 Portable offshore units*, Edition April 2016,
   §3.5.5, pp. 34–35 (skew effects §3.5.4).
   [DNV standard page](https://www.dnv.com/energy/standards-guidelines/dnv-st-e273-2-7-3-portable-offshore-units/).

## Qualification and review handoff

Every summary has `qa_status: pending_family_clearance` and
`engineering_use_allowed: false`. Numerical verification does not supply
installation-family clearance. The
[family gate register](../../../src/digitalmodel/marine_ops/installation/docs/agent-pack.md)
section 5 distinguishes unit-test success from engineering clearance; independent
hook/per-sling balance receipts and traceable clearance receipts remain open.

[verification.json](verification.json) records numerical results, current artifact
hashes, and the dispositions of the second-provider reviews, including `b942066b`.
Maintainer review of these corrections and formal family clearance remain open.
No licensed solver execution is included. Issue acceptance criterion 5 remains
open despite numerical checks for criteria 1–4.
