# Four-point single-hook skew screening

Experimental workflow for issue #2667. It is independent of the unmerged #2666
change (PR #2678); no existing sling or padeye calculation is changed.

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
  For each supplied `cog_envelope_xy_m` corner, the single hook is vertically
  above that CoG. Horizontal sling forces then balance in the reference geometry.
- Each `sling_EA_N` and `sling_length_m` specifies its axial spring compliance
  L/EA. Reference lengths also set initial misfit: L minus geometric hook-to-eye
  distance. Thus changing reference length is not merely changing stiffness.
  The reference lengths remain unchanged when the hook moves between CoG cases.
- `general_length_m` is a scalar or four absolute ±length-error bounds.
  `padeye_elevation_m` is a scalar or four independent ±elevation-error bounds.
  Positive elevation raises the eye and reduces sling extension at fixed pose.
- Optional `matched_pair_difference_m` limits the absolute difference of the
  two length errors **within each opposite pair**, in addition to the general
  bounds. Supply general bounds explicitly when using matching. Matching does
  not limit the difference **between** pairs. This is the narrower, explicit
  interpretation chosen here; other manufacturing matching conventions require
  converting the certificate into these bounds before use.
- `code_minimum_skl` (at least 1) and a nonempty `code_minimum_source` are required.
  The example's 1.25 is an illustrative supplied minimum, not a qualified code
  default. No private standard values, diameter tables, or automatic leg-count
  defaults are embedded. For engineering use, supply the governing standard's
  number, title, edition and clause/table together with the verified value.

The model is a small-displacement axial-spring analysis, not finite-rotation
rigging analysis. It linearizes directions about the supplied horizontal lift
plane. It includes initial length misfit, heave/roll/pitch compatibility and
one optional vertical warp mode. Noncoplanar nominal eyes, nonconvex ordering,
unbalanced horizontal applied loads and CoG outside the support polygon are
rejected. Raised-eye tolerances enter compatibility to first order; they do not
recompute sling directions. Material nonlinearity, friction, dynamics, strength,
payload bending/racking and lift stability require separate analysis. Reference
geometry and misfits must remain appropriate for a small-displacement model.

## Equilibrium, compatibility and slack

Let c_i be the cosine of the sling angle from vertical. The equilibrium matrix
has columns `[c_i, (x_i-x_CoG)c_i/s, (y_i-y_CoG)c_i/s]`, where s is a conditioning
length. It enforces vertical force W and zero moments about the CoG. Horizontal
forces cancel because the hook is above the CoG and all nominal eyes share z.
For any convex four-point layout there is one self-stress mode n, obtained as
the nullspace of this matrix and normalized to maximum absolute component 1.

Every equilibrated tension state is `T = p + n alpha`, with p a particular
equilibrium solution. Nonnegative tensions give a closed feasible interval for
alpha. Minimize complementary energy `0.5 Tᵀ C T + eᵀ T` on that interval:

```text
C_rigid = diag(L_i / EA_i)
e_i = L_i - geometric_distance_i + length_error_i + c_i elevation_error_i
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
`max(code_minimum_skl, effective_skl)` for an accepted envelope. Use the per-leg
nominal loads and this design factor together; multiplying an equal split would
discard the CoG calculation.

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

Tests were written and run before implementation: initial collection failed
because the calculator module did not exist. The following independent checks
are executable in `tests/marine_ops/installation/test_four_point_skew.py`.

| Check | Independent expected result | Acceptance |
| --- | --- | --- |
| Hand 1: rigid 100 t, 30° from vertical, EA=150 MN, L=10 m | Nominal 283190.307 N/leg; delta=15000 N/mm until the other pair is slack | Within 0.1%; ±5,10,15,19,25 mm |
| Hand 2: same lift, ±10 mm, K_w=15 MN/m | Series compliance 1.166666667×10⁻⁷ m/N; delta=85714.286 N | Within numerical roundoff |
| Zero tolerance | 25% vertical load per leg | Numerical roundoff |
| Offset CoG x=0.7 m; half-width a=5/√2 m | Right-side fraction `(a+0.7)/(2a)` and `sum(x_i R_i)=W*0.7` | Numerical roundoff; equal-split planted defect fails |
| Published two-supporting-leg limit, 10,000 lbf, 60° from horizontal | 5,775 lbf per supporting leg, as rounded in the catalog | Within 0.1%; other diagonal slack |
| Code floor | Supplied minimum 3 governs the example tolerance envelope | Exact max rule |

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

## Qualification and review handoff

Every summary has `qa_status: pending_family_clearance` and
`engineering_use_allowed: false`. Numerical verification and an independent
agent's review cannot provide installation-family gate signatures. The module's
[family gate register](../../../src/digitalmodel/marine_ops/installation/docs/agent-pack.md)
section 5 explicitly distinguishes unit-test success from engineering clearance;
G2 includes independent hook/per-sling balance and G5 requires traceable receipts.

The numerical checks and hash-bound second-agent mechanics review are recorded
in [verification.json](verification.json). A second-provider
review requested by the issue comment and formal family QA clearance remain
maintainer actions before engineering use. Neither is marked passed by this
calculator. No licensed solver execution is included. Issue acceptance criterion
5 therefore remains open, despite numerical checks for criteria 1–4.
