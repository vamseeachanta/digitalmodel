# On-bottom stability margin analysis

The synthetic unfactored example requires 1853.7 N/m of submerged weight against
2200.0 N/m supplied, leaving 346.3 N/m reserve. Its calculated utilisation is
0.843 against the implemented limit of 1.000. Required weight and reserve are
exposed explicitly, and both canonical checks use horizontal load magnitude.
Source provenance for the previously attributed page is unresolved; page
equivalence and licensed-standard qualification are not established.

## Revision history

| Revision | Date | What changed and why |
|---|---|---|
| 0 | 2026-10-27 | Compared the supplied page with the code, added weight margins and corrected signed sliding demand. |
| 1 | 2026-10-27 | Reclassified the example as synthetic, removed unsupported page comparisons and recorded independent arithmetic because the source cannot be retrieved. |

## Evidence and scope

This analysis describes the [canonical module](../../src/digitalmodel/subsea/on_bottom_stability/dnv_rp_f109.py),
[simplified wrapper](../../src/digitalmodel/subsea/on_bottom_stability/simplified.py)
and [regression tests](../../tests/subsea/on_bottom_stability/test_dnv_rp_f109.py).
No retrievable source, revision or retained excerpt supports the earlier page
attribution. The example is therefore **synthetic**: its stated inputs and
coefficients define an arithmetic regression and are not measured or qualified
design inputs. Revision 0 records the earlier attribution, which this revision
withdraws. The test and this document retain the complete synthetic input basis.

The module cites DNV-RP-F109, Edition October 2021, sections 3.2.1 (hydrodynamic
loads), 2.2 (submerged weight), 4.3.1 (absolute lateral stability) and 4.3.2
(generalized lateral stability). These inherited locators have not been checked
against the licensed text. No licensed standard was opened or used to establish
equivalence. Equations below describe the implemented arithmetic only; no
coefficient or safety factor is qualified for a particular site.

## Module input basis

| Quantity or symbol | Module input or treatment | Meaning and units |
|---|---|---|
| Seawater density, rho | `rho_w_kg_m3` in load and weight routines | kg/m³. |
| Total outer diameter, D, including coatings | `D_outer_m` in load routines | m. In the weight routine the same argument name means **steel** OD; total OD is steel OD plus twice coating thickness. |
| Combined near-bed velocity, U | `U_m_s` | Signed scalar, m/s, at pipe centreline. The caller must combine wave/current components consistently. |
| Acceleration, dU/dt | `a_m_s2` | Signed scalar, m/s², at pipe centreline. Drag and inertia are added before taking horizontal demand magnitude. |
| Drag, inertia and lift coefficients, CD, CM, CL | `C_D`, `C_M`, `C_L` | Dimensionless; explicit synthetic values are required for reproduction. The drag default differs from the example. |
| Submerged weight, Ws | `W_s_N_m` | N/m, positive downward; accepted directly by the check and margin helper. |
| Steel, coating and contents weights per length | Computed from annular geometry and densities | N/m; component weights are not direct inputs. |
| Gravity, g | Fixed in `submerged_weight_per_meter` | m/s²; not caller-selectable. |
| Buoyancy and total air weight | Internal mass/area arithmetic times gravity | Buoyancy uses total coated OD; no separate air-weight or buoyancy output. |
| Specific gravity, SG | No direct input/output | The canonical module returns net submerged weight. |
| Friction, mu | `mu_soil` | Dimensionless Coulomb coefficient; site value is caller-supplied. The margin helper requires positive friction. |
| Safety factor, gamma_SC | `gamma_SC` | Dimensionless; default retained. Pass `1.0` for the synthetic unfactored example. Placement differs between methods; see below. |
| Passive resistance, FR | `F_R_N_m` in `generalized_stability_check` only | N/m; unavailable to the absolute check and its margin helper. |
| Undrained strength, su, and embedment | No direct input or soil model | The caller must derive any passive resistance externally. No automatic clay/sand selection. |
| Orbital velocity amplitude, Um, and period, T, for KC | No direct input in this module | m/s and s; KC is dimensionless. Coefficients are not selected from KC. |
| Reynolds number and seabed proximity | No direct input or calculation | No coefficient-selection procedure is implemented. |
| Sea state, return period and kinematic transformation | Caller responsibility | This module starts with near-bed velocity and acceleration. No metocean transformation or displacement analysis. |
| Horizontal and lift forces, FH and FL | `F_H_N_m`, `F_L_N_m` | N/m. The load function returns signed FH; stability checks use its magnitude. Lift is upward; the margin helper requires nonnegative FL. |

Table 1. Module interface quantities and units; no page comparison is asserted.

Additional inputs needed **only when computing weight from materials** are steel
OD, wall thickness, coating thickness (m), and steel, coating and contents
densities (kg/m³). They are unnecessary when Ws is supplied directly.
The new helper introduces no additional engineering input. It requires finite
values and a positive safety factor, permits negative supplied weight to show a
buoyancy deficit, and does not validate the upstream pipe geometry or coefficients.

## Implemented balances and qualification limits

The load routines calculate drag plus inertia and lift. The weight routine
subtracts buoyancy from material weight using concentric areas and fixed gravity.
For simultaneous signed loads, FH below denotes the magnitude of the net
horizontal force. The inherited standard locators above remain unverified.

| Method | Implemented balance |
|---|---|
| Unfactored absolute, positive supplied weight | `FH + mu*FL <= mu*Ws` with `gamma_SC=1.0`. |
| Factored absolute | `gamma_SC*(FH + mu*FL) <= mu*Ws`; no FR. Lift is also factored. |
| Generalized scalar balance | `gamma_SC*FH <= mu*(Ws-FL) + FR`. The simplified wrapper uses this balance with FR zero. |

Table 2. Implemented force balances; licensed-standard equivalence is not established.

The existing methods differ in safety-factor placement and are retained. With
positive friction and no FR, the absolute required weight is
`gamma_SC*(FH/mu + FL)`, whereas the generalized balance requires
`gamma_SC*FH/mu + FL`. Neither this arithmetic nor the tests establish which
convention is required by a licensed standard. The function
named generalized evaluates a scalar force balance; it does not implement
calibrated allowable displacement or a dynamic time history.

The friction-to-drive margin and the supplied-to-required weight
margin are different quantities when lift is present. They share the pass
boundary at unity for the unfactored no-passive-resistance case, but their
numerical values must not be interchanged. Site use requires a retrieved source
and independent qualification of coefficients, factors, soil and metocean inputs.

## Synthetic worked example and API

The synthetic case uses D 0.5 m, U 1.5 m/s, acceleration 0.6 m/s², density 1025 kg/m³,
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
| Utilisation | 0.843 against limit 1.000 |
| Governing check | `absolute_lateral`; stable because utilisation is at most 1.000 |

Table 3. Synthetic unfactored results for the stated inputs and implemented criterion.

Independent hand arithmetic defines the expected values:

- Horizontal force: `0.5*1025*0.7*0.5*1.5² + 1025*3.29*(pi/4)*0.5²*0.6` gives 800.9 N/m.
- Lift: `0.5*1025*0.9*0.5*1.5²` gives 518.9 N/m.
- Available friction: `0.6*(2200 - FL)` gives 1008.7 N/m.
- Required weight: `FH/0.6 + FL` gives 1853.7 N/m.
- Weight reserve: `2200 - required weight` gives 346.3 N/m.

These expressions define the synthetic regression using the implemented model;
they are not quotations from a standard. Unrounded loads are used throughout,
with rounding only for presentation. Reported comparisons use one decimal place;
the decision utilisation uses three decimals beside its limit. The tests assert
the independently calculated literals with explicit rounding tolerances.

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
