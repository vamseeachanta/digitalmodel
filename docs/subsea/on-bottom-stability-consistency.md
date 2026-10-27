# On-bottom stability margin analysis

The embedded pipeline on-bottom stability page dated 2026-10-27 and the
canonical module agree on SI loads, submerged weight and the unfactored static
balance. The worked example passes. Factored equations differ in how lift is
treated. Required weight and reserve are now exposed explicitly, and reversing
horizontal load no longer produces a false stability pass.

## Revision history

| Revision | Date | What changed and why |
|---|---|---|
| 0 | 2026-10-27 | Compared the supplied page with the code, added weight margins and corrected signed sliding demand. |

## Evidence and scope

This comparison uses the supplied page, [canonical module](../../src/digitalmodel/subsea/on_bottom_stability/dnv_rp_f109.py),
[simplified wrapper](../../src/digitalmodel/subsea/on_bottom_stability/simplified.py)
and [regression tests](../../tests/subsea/on_bottom_stability/test_dnv_rp_f109.py).
The existing knowledge pack is pinned to an earlier revision; its recorded
standard-edition and qualification limits still apply. No licensed standard was
opened or used to establish equivalence. Example coefficients come from the
supplied page; existing defaults are retained without qualifying their use at a
particular site. Other coefficients must be supplied by the caller.

## Input consistency

| Page input or symbol | Module input or treatment | Meaning and units |
|---|---|---|
| Seawater density, rho | `rho_w_kg_m3` in load and weight routines | Same, kg/m³. |
| Total outer diameter, D, including coatings | `D_outer_m` in load routines | Same, m. In the weight routine the same argument name means **steel** OD; total OD is steel OD plus twice coating thickness. |
| Combined near-bed velocity, U | `U_m_s` | Same signed scalar, m/s, at pipe centreline. The caller must combine wave/current components consistently. |
| Acceleration, dU/dt | `a_m_s2` | Same signed scalar, m/s², at pipe centreline. Drag and inertia are added before taking horizontal demand magnitude. |
| Drag, inertia and lift coefficients, CD, CM, CL | `C_D`, `C_M`, `C_L` | Same dimensionless meanings; explicit example values are required for reproduction. The drag default differs from the example. |
| Submerged weight, Ws | `W_s_N_m` | Same, N/m, positive downward; accepted directly by the check and margin helper. |
| Steel, coating and contents weights per length | Computed from annular geometry and densities | Same N/m result, but component weights are not direct inputs. |
| Gravity, g | Fixed in `submerged_weight_per_meter` | m/s²; not caller-selectable. The page leaves its numerical value unstated. |
| Buoyancy and total air weight | Internal mass/area arithmetic times gravity | Same buoyancy basis using total coated OD; no separate air-weight or buoyancy output. |
| Specific gravity, SG | No direct input/output | Page defines total air weight / buoyancy; the canonical module returns only net submerged weight. |
| Friction, mu | `mu_soil` | Same dimensionless Coulomb coefficient; site value is caller-supplied. The new margin helper requires positive friction. |
| Safety factor, gamma_SC | `gamma_SC` | Dimensionless; default retained. Pass `1.0` for the page's unfactored example. Placement differs for factored equations; see below. |
| Passive resistance, FR | `F_R_N_m` in `generalized_stability_check` only | Same N/m contribution; unavailable to the absolute check and its margin helper. |
| Undrained strength, su, and embedment | No direct input or soil model | The caller must derive any passive resistance externally. No automatic clay/sand selection. |
| Orbital velocity amplitude, Um, and period, T, for KC | No direct input in this module | Page uses m/s and s; KC is dimensionless. Coefficients are not selected from KC. |
| Reynolds number and seabed proximity | No direct input or calculation | Page names coefficient dependencies but supplies no worked-example values or coefficient-selection procedure. |
| Sea state, return period and kinematic transformation | Caller responsibility | The page requires their selection; this module starts with near-bed velocity and acceleration. No metocean transformation or displacement analysis. |
| Horizontal and lift forces, FH and FL | `F_H_N_m`, `F_L_N_m` | N/m. The load function returns signed FH; stability checks now use its magnitude. Lift is upward; the margin helper requires nonnegative FL. |

Additional inputs needed **only when computing weight from materials** are steel
OD, wall thickness, coating thickness (m), and steel, coating and contents
densities (kg/m³). The page names the material weights but supplies none of this
geometry or density detail. They are unnecessary when Ws is supplied directly.
The new helper introduces no additional engineering input. It requires finite
values and a positive safety factor, permits negative supplied weight to show a
buoyancy deficit, and does not validate the upstream pipe geometry or coefficients.

## Equation consistency and findings for wiki maintainers

The drag-plus-inertia and lift expressions match the page exactly. The weight
routine implements the same material-weight-minus-buoyancy expression using
concentric areas and fixed gravity. For simultaneous signed loads, FH below
denotes the magnitude of the net horizontal force.

| Balance | Page | Module |
|---|---|---|
| Unfactored, no passive resistance | `FH + mu*FL <= mu*Ws` | Absolute check is equivalent with `gamma_SC=1.0`. |
| Factored absolute | `gamma_SC*FH <= mu*(Ws-FL) + FR` | Absolute check uses `gamma_SC*(FH + mu*FL) <= mu*Ws`; no FR. Lift is also factored. |
| Passive-resistance balance | Included in the page's factored static equation | The function named generalized uses `gamma_SC*FH <= mu*(Ws-FL) + FR`. The simplified wrapper uses this balance with FR zero. |

The factor-placement discrepancy is a finding for wiki maintainers to reconcile;
the existing module convention is retained. With positive friction and no FR,
the absolute required weight is `gamma_SC*(FH/mu + FL)`, whereas the page's
factored equation requires `gamma_SC*FH/mu + FL`. Neither code nor this comparison
establishes which convention is required by a licensed standard. The function
named generalized evaluates a scalar force balance; it does not implement
calibrated allowable displacement or a dynamic time history.

The page's friction-to-drive margin and the new supplied-to-required weight
margin are different quantities when lift is present. They share the pass
boundary at unity for the unfactored no-passive-resistance case, but their
numerical values must not be interchanged. The page leaves the use of horizontal
load magnitude for sliding unstated.

## Worked example and API

Using D 0.5 m, U 1.5 m/s, acceleration 0.6 m/s², density 1025 kg/m³,
CD 0.7, CM 3.29, CL 0.9, friction 0.6 and supplied weight 2200 N/m:

| Quantity | Computed result |
|---|---|
| Horizontal force | 800.9 N/m |
| Lift | 518.9 N/m |
| Available friction | 1008.7 N/m |
| Friction / horizontal force | 1.3 |
| Required submerged weight | 1853.7 N/m |
| Supplied / required weight | 1.2 |
| Supplied minus required weight | 346.3 N/m |
| Governing check | `absolute_lateral`; stable |

Reported comparisons are rounded to one decimal place. The regression checks the
page's displayed values at their own rounding precision, using unrounded force
calculations for the marginal-weight boundary.

```python
from digitalmodel.subsea.on_bottom_stability.dnv_rp_f109 import (
    absolute_stability_margin,
    hydrodynamic_force_per_meter,
    lift_force_per_meter,
)

horizontal = hydrodynamic_force_per_meter(1025, 0.5, 1.5, 0.6, C_D=0.7, C_M=3.29)
lift = lift_force_per_meter(1025, 0.5, 1.5, C_L=0.9)
margin = absolute_stability_margin(2200, horizontal, lift, 0.6, gamma_SC=1.0)
```

`AbsoluteStabilityMargin` reports `required_weight_N_m`, `supplied_weight_N_m`,
`weight_margin_ratio`, `weight_margin_N_m`, `governing_check` and `is_stable`.
It reuses the absolute check's demand. Nonzero horizontal load governs as
`absolute_lateral`; lift alone gives `uplift`, the limiting case of the same
equation rather than an independently qualified vertical check. With no load,
the label is `no_load`, required weight is zero, and the ratio is `None` because
division by zero has no defined margin. The stability flag retains the existing
check's rejection of zero or negative supplied weight.

The direction correction applies to both canonical checks and the simplified
wrapper's required weight. Positive-load behaviour and safety-factor placement
are retained. No site-specific demonstration or coefficient qualification is
included; the example is an arithmetic regression, not a design acceptance.
