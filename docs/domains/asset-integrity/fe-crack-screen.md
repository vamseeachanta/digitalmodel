# FE-supplied crack-like flaw analysis

The solver-free entry point screens supplied crack states against a supplied
failure assessment diagram (FAD). It combines primary, secondary and residual
stress-intensity factors, integrates a tabulated stress-intensity range history,
and compares automated growth histories with that same growth law.

This is a screening capability under [issue 2157](https://github.com/vamseeachanta/digitalmodel/issues/2157).
It does not establish API 579-1/ASME FFS-1 Part 9 or BS 7910 conformity.
`passes` remains false and evidence remains `INCOMPLETE`, including when every
supplied state lies inside the supplied envelope. No licensed solver is loaded.
The existing `crack_fad` API/BS curves remain separate; this entry point accepts
an explicitly sourced envelope rather than assuming an edition or reproducing
licensed-source constants.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| A | 2026-10-20 | Initial solver-free screening method and independent arithmetic example. |
| B | 2026-10-22 | Preserve supplied report criteria, report arrest cycles and share threshold integration with the fatigue API. |

Table 1. Document revisions.

## Three implementation slices

1. FE input screening: separate primary, secondary and residual K terms;
   explicit multiplicative secondary factor; user-supplied FAD envelope and
   collapse cutoff; required provenance strings and finite-number validation.
2. Growth and consistency: exact piecewise-linear DeltaK integration with a
   user-supplied Paris law; threshold arrest; explicit E-ratio sensitivity and
   threshold scaling rule; DeltaK multiplier sensitivities; optional automated
   history and nominal-opening/reference-stress diagnostics.
3. Report and executable workflow: standalone JSON-to-HTML command, exclusive
   output creation, input readback in the report, criteria, classified missing
   evidence, references and way forward.

## Method and sources

- **Secondary stress:** `Kr = (Kp + V*(Ks + Kres))/Kmat`.
  The multiplicative secondary-stress treatment is described in TWI (1999),
  [Quantification of Residual Stress Effects in the SINTAP Defect Assessment
  Procedure for Welded Components](https://www.twi-global.com/technical-knowledge/published-papers/quantification-of-residual-stress-effects-in-the-sintap-defect-assessment-procedure-for-welded-components-july-1999).
  Equation 22 defines this form. `V` varies with `Lr` and the secondary/primary
  stress ratio; the supplied global value must bound every supplied state.
  Establishing that bound is the caller's responsibility and is not checked.
  Compressive secondary and residual K are not supported: both inputs must be
  nonnegative. Signed-K cases require a qualified separate method.
  No API Phi or BS rho correction
  is inferred or mixed into this expression. Residual K is part of secondary K.
- **FAD coordinates:** `Lr = reference_stress/yield_stress`; `Kr <= f(Lr)` and
  `Lr <= lr_max` define inside the supplied envelope. The intended code context
  uses `reference_mpa` as primary-only collapse reference stress.
  API 579-1/ASME FFS-1 (2016), Part 9, Assessment of Crack-Like Flaws, provides context.
  Curve ordinates, cutoff and their basis are supplied inputs. Interpolation
  is linear and extrapolation is prohibited. A supplied envelope is not
  authenticated by a basis string; edition conformity is `NOT_EVALUATED`.
- **Growth:** AFGROW Handbook for Damage Tolerant Design (2002),
  [section 2.2.5, equation 2.2.7](https://afgrow.net/applications/DTDHandbook/sections/page2_2_5.aspx),
  describes the Paris power law and integration of crack-growth rate.
  `da/dN = C*DeltaK^m`; `N = integral da/(C*DeltaK(a)^m)`.
  For a linear segment with slope `b`, the analytical integral is
  `(K1^(1-m)-K0^(1-m))/(C*b*(1-m))`; for `m=1`, it is
  `log(K1/K0)/(C*b)`. Constant K gives `Delta_a/(C*K^m)`.
  The implementation uses logarithmic forms to limit cancellation.
- **Threshold:** equality or a crossing of the supplied threshold returns
  `ARRESTED`, with no finite life to the final depth. The crossing depth is
  calculated from the linear segment, including a crossing between samples.
  `cycles_to_arrest` gives the integral to that depth, including earlier segments;
  `cycles` remains `None` because the final depth is not reached. An integral
  diverging at zero DeltaK has `cycles_to_arrest=None`, displayed as `unbounded`.
  An absent arrest depth displays as `n/a`.
  This hard-cutoff model is an explicit approximation; material behaviour near
  threshold is not qualified by the Paris law. The reported minimum
  DeltaK/threshold ratio describes the full supplied table.
- **E ratio:** a required positive `e_ratio` multiplies DeltaK. The required
  `threshold_rule` either leaves the threshold `unchanged` or multiplies it by
  `e_ratio`. This is a declared sensitivity, not a sourced temperature law.
- **Consistency:** automated history intervals are compared using
  `observed_cycles/predicted_cycles`; the absolute difference from unity is
  compared with the supplied fractional tolerance. Every intermediate static
  table knot is retained in the integration. No extrapolation is permitted.
  A separate diagnostic uses `sigma_implied = Kp/(Y*sqrt(pi*a))`, from AFGROW
  (2002), [section 2.2.1](https://afgrow.net/applications/DTDHandbook/sections/page2_2_1.aspx).
  Its ratio to reference stress is compared with a user-supplied band. Nominal
  opening stress and collapse reference stress are different quantities: this
  diagnostic flags discrepancies for investigation and cannot validate either.

The existing `fatigue.crack_growth.paris_law_life` API delegates each sampled
interval to the same threshold-aware `growth_life` API. Its optional `threshold`
defaults to zero; callable DeltaK is approximated linearly over `da_step`.
An arrested history ends at the actual arrest depth, with infinite life to the
requested final depth. This replaces the earlier left-endpoint integration.
The separate implementation proposed in #3106 is not present on this branch;
cross-branch consolidation remains pending.

Reports preserve supplied criteria and multiplier labels. Computed results use
four significant figures; all dispositions use unrounded values, including
equality at the supplied envelope and cutoff.

No material constants, acceptance bands or code coefficients are supplied by the
workflow. Therefore there are no new standards-derived constant Citation
sidecars. Basis strings record supplied provenance; they do not resolve or
validate an engineering source. Derived results are not qualified engineering
input merely because a calculation completes.

## Worked example: inputs, method and answer

All numbers below are artificial arithmetic inputs, not material or FE data.
The basis is **artificial, for hand calculation only**.

```python
from digitalmodel.asset_integrity.assessment.fe_crack_screen import assess_fe_crack

inputs = {
    "basis": "Artificial hand calculation; not engineering data",
    "k_mat_mpa_sqrt_m": 100.0,
    "yield_mpa": 200.0,
    "lr_max": 1.5,
    "envelope": [[0.0, 1.0], [1.0, 0.5], [1.5, 0.0]],
    "envelope_basis": "Artificial piecewise-linear envelope; not API 579",
    "secondary_factor": 2.0,
    "secondary_basis": "Artificial V factor, not code-derived",
    "states": [
        {"a_m": 0.001, "k_primary": 20.0, "k_secondary": 5.0,
         "k_residual": 5.0, "reference_mpa": 100.0, "delta_k": 10.0},
        {"a_m": 0.002, "k_primary": 30.0, "k_secondary": 5.0,
         "k_residual": 5.0, "reference_mpa": 150.0, "delta_k": 10.0},
    ],
    "growth": {
        "c": 1e-10, "m": 2.0, "threshold": 2.0,
        "e_ratio": 1.0, "threshold_rule": "unchanged",
        "basis": "Artificial Paris parameters; not material data",
    },
    "delta_k_multipliers": [0.8, 1.2],
}
result = assess_fe_crack(inputs)
assert result["screen"] == "INSIDE_SUPPLIED_ENVELOPE"
assert result["passes"] is False
assert result["growth"]["cycles"] == 100000.0
```

Units: depth in m; stresses in MPa; all K terms in MPa sqrt(m); C in
m/cycle/(MPa sqrt(m))**m; cycle counts dimensionless. `delta_k` is supplied
separately from static K because the stress ratio and cyclic secondary loads
are not inferred. No implicit mm conversion is performed.

At the final state, `Lr = 150/200 = 0.75` and
`Kr = (30+2*(5+5))/100 = 0.5`. Linear interpolation gives
`f(Lr) = 1 - 0.5*0.75 = 0.625`: the point lies inside the artificial envelope.
The unrounded quantities govern the comparison; displayed comparisons may be
rounded. Increasing residual K to 15 gives `Kr = 0.7 > 0.625`, outside.
Constant DeltaK gives `N = (0.002-0.001)/(1e-10*10**2) = 100000.0` cycles.
The sensitivity factors give 156250.0 and 69444.4 cycles, respectively.
These cycles run to the last supplied depth; they are not allowable service life.

For CLI use, save `inputs` as JSON in a chosen working directory, then run:

```bash
PYTHONPATH=src python -m digitalmodel.asset_integrity.assessment.fe_crack_screen input.json report.html
```

The command refuses to overwrite any existing output. Duplicate JSON keys and
nonfinite engineering inputs are rejected. The report contains the input basis,
state comparisons, sensitivities, consistency checks, incomplete evidence and
way forward. Python consumers can call `report_html(result)` without file I/O.

Optional `history` entries are `[depth_m, cumulative_cycles]`, with strictly
increasing depth and cycles; `history_tolerance_fraction` and `history_basis`
are required. Optional `consistency` requires `geometry_factor`, `lower_ratio`,
`upper_ratio` and `basis`. Omitted diagnostic inputs return `NOT_EVALUATED`.
Each diagnostic `PASS` applies only to its supplied criterion, not the component.

## Later slices and limits

Licensed FE generation/execution, FE-result provenance qualification, code-edition
confirmation, partial safety factors, temperature/material applicability,
small-scale yielding, shakedown, plasticity-coupled growth, mixed-mode fracture,
critical-depth search, inspection intervals, engine routing and integration with
the separate report implementation remain outside this solver-free slice.
The lightweight HTML report is not a CalcReport integration. No benchmark FE
result or report from the separate feature branch is imported or republished.
The next engineering checkpoint is qualified data and explicit governing-depth
criteria; a positive service disposition cannot be calculated from this example.
