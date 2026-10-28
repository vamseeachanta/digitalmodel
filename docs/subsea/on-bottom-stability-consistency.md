# On-bottom stability margin analysis

The absolute static interface now checks lateral demand and uplift separately
under clause 3.6.2, Eqs. (3.38)-(3.39), and reports the governing utilisation
and required weight. Factors, peak coefficients and reduction inputs are
explicit. The retained synthetic example requires 1853.7 N/m against
2200.0 N/m supplied, leaving 346.3 N/m reserve. These arithmetic regressions
establish neither a site design nor a qualified standard implementation.

## Revision history

| Revision | Date | What changed and why |
|---|---|---|
| 0 | 2026-10-27 | Compared the supplied page with the code, added weight margins and corrected signed sliding demand. |
| 1 | 2026-10-27 | Reclassified the example as synthetic, removed unsupported page comparisons and recorded independent arithmetic because the source cannot be retrieved. |
| 2 | 2026-10-27 | Aligned the absolute checks and shared inputs with the supplied clause map, added independent uplift and initial-state resistance, and distinguished peak loads from instantaneous arithmetic. |
| 3 | 2026-10-28 | Removed the unverified concept-page comparison, clarified the source basis and linked the synthetic example to independent expected-value arithmetic. |

## Evidence and scope

The [canonical module](../../src/digitalmodel/subsea/on_bottom_stability/dnv_rp_f109.py),
[compatibility wrappers](../../src/digitalmodel/subsea/on_bottom_stability/simplified.py)
and [clause regressions](../../tests/subsea/on_bottom_stability/test_f109_absolute_clauses.py)
use clause locators from a supplied map identified as the October 2010 issue
amended November 2011. No retrievable, versioned source or rights-cleared excerpt
for the concept page or that map is retained with this analysis. The locators
are unverified references, not evidence of conformity to that edition. Original
pages and numerical lookup datasets have not been independently qualified in
this change. No licensed sentences, tables or figure values are reproduced.

The former concept-page comparison is withdrawn because its source cannot be
independently checked. No page equivalence is claimed. All inputs and
coefficients of the retained instantaneous example are synthetic; their
original provenance remains unresolved. The table below records the local
implementation and the supplied map's unverified locators only.

## Plan-lite

- Source basis: local code and synthetic arithmetic; page provenance and
  edition-matched clause evidence remain unresolved.
- Scope: one static margin slice and its compatibility entry points; the
  separately maintained concept page is not changed.
- Verification: direct regression tests and independent arithmetic for loads,
  required weight, reserve and both static criteria.
- Qualification limits: no site acceptance, licensed lookup qualification,
  generalised displacement response or dynamic qualification is established.

## Implementation and evidence gaps

| Topic | Supplied map locator (unverified) | Current module and disposition |
|---|---|---|
| Horizontal force | Clause 3.6.4, Eqs. (3.40)-(3.41) | `absolute_peak_loads` accepts explicit Table 3-9 and Table 3-10 coefficients; instantaneous helpers are separate. Lookup qualification **OPEN**. |
| Current | Clause 3.4.2, Eqs. (3.2)-(3.3) | `transverse_current_at_height` and `diameter_mean_current` added; Table 3-1 roughness is user input, headings in radians. |
| Wave and current basis | Clauses 3.4.2-3.4.4 | Explicit extreme amplitude, associated period and associated transverse current; spectral generation and extreme mapping **OPEN**. |
| Reductions | Clause 3.4.5, Eq. (3.17) | `load_reduction_factor` multiplies supplied components; peak helper applies each direction once. Empirical fits **OPEN**. |
| Weight | Clauses 1.5 and 2.3 | Positive bore and material validation added; reference gravity aligned. Zero contents density is an empty idealisation, never a universal gas assumption. Phase/loss selection **OPEN**. |
| Lateral acceptance | Clause 3.6.2, Eq. (3.38) | Both horizontal and friction-weighted upward demand are factored in the shared absolute, deprecated static alias and lateral wrapper criteria. |
| Upward acceptance | Clause 3.6.2, Eq. (3.39) | Separate utilisation, weight boundary and combined acceptance; passive resistance cannot mask uplift failure. |
| Factors | Clause 3.6.3, Tables 3-5 through 3-8 | Explicit positive finite factor required; no automatic selection or licensed numeric default. |
| Soil credit | Clauses 3.6.1 and 3.6.5 | Optional passive resistance requires `initial_penetration` or `prior_loading` qualification; movement-generated resistance rejected. Soil evidence remains caller responsibility; rock passive credit must be zero. |
| Method identity | Clause 3.5, Eq. (3.32) | Historical generalised function warns and delegates to the static criteria; actual generalised and dynamic methods **OPEN**. |
| Example conclusion | No retained source | Retained example is synthetic arithmetic only; no coating or site acceptance conclusion. |

Table 1. Local implementation and evidence gaps; clause locators remain unverified.

## Implemented checks and explicit inputs

The absolute method uses nonnegative extreme-cycle peak demands `h` and `z`,
net downward weight `w`, friction `mu`, eligible passive resistance `p` (all
forces N/m), and dimensionless factor `gamma`. Passing requires positive
supported weight and **both** utilisations at most one:

| Implemented operation | Clause and equation | Relation or input contract |
|---|---|---|
| No-slip decision | Clause 3.6.1 | No lateral slip in the selected extreme cycle; not a displacement prediction. `no_lateral_slip` records combined static acceptance. |
| Lateral utilisation | Clause 3.6.2, Eq. (3.38) | `eta_h = gamma*(h + mu*z)/(mu*w + p)`. |
| Vertical utilisation | Clause 3.6.2, Eq. (3.39) | `eta_z = gamma*z/w`; independent of passive resistance. |
| Required weight | Clause 3.6.2, Eqs. (3.38)-(3.39) | `w_h = max(0, (gamma*(h+mu*z)-p)/mu)`, `w_z = gamma*z`, `w_required = max(w_h,w_z)`. |
| Factor selection | Clause 3.6.3, Tables 3-5 through 3-8 | User supplies the factor for environment, region, soil and consequence class, with edition-matched evidence. No selection is inferred. |
| Peak coordinates | Clause 3.6.4 | `KC_e = Ue*Te/D`, `M_e = Ve/Ue`. Positive amplitude and period; nonnegative associated current magnitude. |
| Peak force generation | Clause 3.6.4, Eqs. (3.40)-(3.41) | `q_e = rho*D*(Ue+Ve)^2/2`, `h = r_h*C_H*q_e`, `z = r_z*C_Z*q_e`. Table 3-9, Table 3-10 and Figures 3-13/3-14 values are user inputs. No additional inertia term. |
| Low-KC adjustment | Clause 3.6.4, Table 3-9 | `C_H = C_H_anchor*KC_anchor/KC_e`, restricted to positive KC at or below the supplied first-column coordinate. No zero-wave limit or other extrapolation. |
| Directional reduction | Clause 3.4.5, Eq. (3.17) | `r_i = r_perm_i*r_pen_i*r_tr_i`, separately for horizontal and upward demand. Explicit multipliers between zero and one; use unit multipliers when coefficients already include those reductions. |
| Current profile | Clause 3.4.2, Eq. (3.2) | `V(z) = Vref*sin(theta)*log(1+z/z0)/log(1+zref/z0)`. |
| Diameter-mean current | Clause 3.4.2, Eq. (3.3) | `V = Vref*sin(theta)*((1+z0/D)*log(1+D/z0)-1)/log(1+zref/z0)`. |
| Weight and geometry | Clauses 1.5 and 2.3 | Concentric steel, one coating annulus and bore contents minus displaced-water mass, multiplied by gravity. |
| Friction-only boundary | Clause 3.6.5, Eq. (3.42) | With no passive credit, no reductions and unit factor, `w_required/q_e = C_H/mu + C_Z`. This subset does not replace the two design criteria. |

Table 2. Implemented expressions and their clause locators; no licensed lookup data is embedded.

The module rejects nonfinite inputs, negative peak demands, nonpositive
friction or factor, invalid geometry and negative material densities. A real
coating annulus requires positive coating density. Nonpositive supplied
weight fails the canonical check; the margin can still show its deficit.
Frictionless cases are outside this interface's domain. Inputs are scalars,
with SI units and coated hydrodynamic diameter distinguished from steel OD.
Generic instantaneous coefficients are explicit too; legacy exported constants
remain unqualified compatibility symbols and are never chosen by a check.

Nonzero passive resistance requires the caller's initial-state qualification.
The string records that assertion; it does not prove penetration, loading
history, soil suitability or resistance magnitude. A proposed change of weight
requires requalification if passive resistance depends on weight or contact.
No credit may be taken for movement generated in the no-slip cycle, or for rock
passive resistance under this model.

Reduction inputs require source identity, geometry, angle units and eligibility
under clause 3.4.5, Eqs. (3.18)-(3.22), Figures 3-5 through 3-8. The code does not
calculate those empirical fits or infer whether coefficients were already
reduced. A unit multiplier is an algebraic identity, not an assumed figure value.
The same responsibility applies to Table 3-1 roughness and coefficient axes,
interpolation domains and provenance for Tables 3-9 and 3-10.

## Outputs and compatibility changes

`StabilityResult.utilisation` is `max(eta_h, eta_z)`. Its `details` include both
utilisations, both required weights, the supplied factor and passive basis,
clause references, load basis, no-slip result and the governing **utilisation**
at the supplied weight. Nonpositive weight is labelled `unsupported`.

`AbsoluteStabilityMargin` retains supplied and required weight, reserve,
supplied/required ratio and stability, and adds separate lateral/vertical
utilisations and required weights. Its governor identifies the larger
**required weight**; it may differ from the utilisation governor when passive
resistance is present. Equal nonzero boundaries use `uplift`; zero loading uses
`no_load`, zero required weight and ratio `None`. Passive credit may also
reduce required weight to zero with nonzero horizontal loading; that case
retains the `absolute_lateral` label and has ratio `None`. Stability always requires
positive supported weight. The weight ratio is distinct from a friction/drive
ratio when lift or passive resistance is present.

These contracts intentionally change previous behaviour:

- All static checks and wrappers require an explicit factor; generic load
  coefficients are explicit as well.
- Signed horizontal samples are rejected by static interfaces. Resolve the
  governing extreme cycle; taking the absolute value of one sample does not
  qualify a peak.
- `generalized_stability_check` emits `DeprecationWarning` and delegates to the
  absolute static criteria. It is retained only as a compatibility name; its
  previous horizontal-only factor placement and uplift masking are removed.
- The N/m and kN/m lateral wrappers use the same absolute criteria and required
  weight. The upward wrapper uses Eq. (3.39), distinct from flotation Eq. (3.1).
- Weight geometry and instantaneous load validation are shared with the direct
  wrappers. Gravity now follows the supplied clause 1.5 basis.

Existing assertions for signed static inputs, implicit factors and the former
compatibility formula were updated to the new contract. Negative peaks now have
explicit rejection tests; no failing check was skipped or weakened.

## Synthetic examples and reproducibility

The retained instantaneous arithmetic case uses D 0.5 m, U 1.5 m/s,
acceleration 0.6 m/s², density 1025 kg/m³, CD 0.7, CM 3.29, CL 0.9, friction
0.6, unit factor and supplied weight 2200 N/m. These are synthetic inputs;
they are not section 3.6.4 table data or a qualified design environment.
The complete case is retained in
[`test_synthetic_unfactored_worked_example`](../../tests/subsea/on_bottom_stability/test_dnv_rp_f109.py).
That test is the reproducible definition of this synthetic case, not an external
source citation. The coefficients do not establish calibrated design loads.

| Quantity | Synthetic result |
|---|---|
| Instantaneous horizontal force | 800.9 N/m |
| Instantaneous lift | 518.9 N/m |
| Available unfactored friction | 1008.7 N/m |
| Friction / horizontal force | 1.3 |
| Required weight under the supplied-load static arithmetic | 1853.7 N/m |
| Supplied / required weight | 1.2 |
| Weight reserve | 346.3 N/m |
| Lateral utilisation | 84.3 % |
| Vertical utilisation | 23.6 % |
| Governing weight boundary | `absolute_lateral` |

Table 3. Retained synthetic arithmetic only; no site acceptance or page equivalence.

Independent arithmetic uses `h = rho*CD*D*abs(U)*U/2 + rho*CM*pi*D²*a/4`,
`z = rho*CL*D*U²/2`, `w_required = h/mu + z`, and
`reserve = w_supplied - w_required`, retaining unrounded loads until display.
Substitution gives `h = 0.5*1025*0.7*0.5*1.5² +
1025*3.29*pi*0.5²*0.6/4`, `z = 0.5*1025*0.9*0.5*1.5²`, and
available friction `0.6*(2200-z)`. The test evaluates these expressions
independently of the load helpers before checking required weight and reserve.
This arithmetic test does not turn the instantaneous samples into design peaks.

A separate synthetic peak regression uses density 1000 kg/m³, coated diameter
0.5 m, extreme amplitude 2 m/s, period 5 s and associated current 1 m/s.
Arbitrary synthetic `C_H=2.5`, `C_Z=1.5` and independent reduction products
`r_h=0.4`, `r_z=0.6` give q 2250.0 N/m, h 2250.0 N/m and z 2025.0 N/m.
KC is 20.0 and current ratio is 0.5. These values exercise the equations;
none was obtained from a licensed table or figure.

```python
from digitalmodel.subsea.on_bottom_stability.dnv_rp_f109 import (
    absolute_peak_loads,
    absolute_stability_margin,
)

# Synthetic coefficients and factor for arithmetic regression only.
loads = absolute_peak_loads(1000, 0.5, 2, 5, 1, 2.5, 1.5, 0.4, 0.6)
margin = absolute_stability_margin(
    10000, loads.horizontal_N_m, loads.lift_N_m, 0.5, gamma_SC=1.25,
)
```

The regressions cover both inequality boundaries, positive-passive uplift
failure, initial-state qualification, equivalent static entry points, explicit
factors and coefficients, invalid inputs, small-diameter current averaging,
independent numerical integration of the current profile, weight geometry,
low-KC anchoring and direction-specific reduction arithmetic. They do not
supply independent authorised engineering benchmarks.

## OPEN register and way forward

| Area | OPEN capability or evidence | Required next step |
|---|---|---|
| Edition and mathematical evidence | Later-edition conformance and flagged extraction groupings. | Qualify legible authorised source pages before extending the implementation. |
| Environment and phases | Clause 2.2 combinations, heading selection, joint statistics, phase boundaries and clause 2.3 loss/contents selection. | Establish and run every governing phase/environment externally; this interface evaluates one supplied case. |
| Spectrum and transfer | Clause 3.4.3, Eqs. (3.4)-(3.14), including transfer, moments and spectral normalisation. | Supply qualified measured or analytical kinematics; do not assume an empirical fit. Figure 3-2/3-3 alternatives remain user inputs. |
| Extreme-cycle mapping and spreading | Clause 3.4.3, Eqs. (3.15)-(3.16), and clause 3.4.4. | Qualify amplitude/period mapping and directionality, including Figure 3-4 inputs; no undocumented multiplier or cosine-power normalisation. |
| Lookup and reduction eligibility | Clauses 3.4.5 and 3.6.3-3.6.4; every table/figure input and the zero-wave limit. | Obtain edition-matched values, axes, domains, physical applicability and review. No numerical lookup qualification is claimed. |
| Soil, penetration and trench models | Clause 3.4.6, Eqs. (3.23)-(3.31), Figures 3-9/3-10 and clause 3.6.5. | Qualify soil laws, initial/prior loading, penetration, trench geometry and piping before supplying resistance. |
| Other common checks | Clause 3.2 Eq. (3.1), clauses 3.3 and 4.4-4.5, and clause 2.5 structural/travel consequences. | Independently assess flotation, sinking, soil flotation, scour, stress, fatigue, buckling and accumulated displacement. |
| Generalised displacement | Clause 3.5, Eqs. (3.32)-(3.37), Tables 3-2 through 3-4, Figures 3-11/3-12 and Appendix A. | Implement a qualified response family with applicability, duration, travel limits, normalisation and interpolation; all lookup data remains user input. |
| Dynamic method | Time-dependent pipe/soil/structural response. | Establish qualified dynamic modelling and structural acceptance; static margins are not dynamic qualification. |
| Geometry and package integration | Multilayer materials, frictionless domain, further callers and wiki correction. | Extend supported geometry with tests, audit callers and correct the separately maintained concept page. |

Table 4. Remaining gaps are OPEN; passing static arithmetic does not close them.

Use the implemented margin only after the phase, peak-load, factor and soil
inputs have been qualified. The way forward is to resolve the listed evidence
and modelling gaps before making a design acceptance claim.
