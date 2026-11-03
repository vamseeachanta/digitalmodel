# Pump-down conveyance: steady core worked example

This is a screening calculation for plug-and-perf wireline conveyance. The
library calculates bypass loss, hydraulic drive, resistance, line speed,
survey-based cable tension and elastic stretch. It does not establish a
field operating window or certify that a tool will reach the toe. Pressure
limits, transients, activation loads, injectivity and a workflow router are
follow-up work.

All example inputs are representative, synthetic values specified for this
calculation, independently recomputed on 2 November 2026. No private technical
reference, measured dataset or vendor method is included. The test vectors
are in `tests/well/completion/pump_down_vectors.yaml`.

## Basis and units

Physical functions accept `digitalmodel.units.Q_` quantities. Bare inputs are
SI, except `bypass`, `api_drift`, `runnable` (inches), `ideal_line_speed`
(bbl/min and inches), and angles explicitly named in degrees. Density is
mass/volume; BHA weight, cable weight/length, weak-point load and effective
axial stiffness EA are forces. The shared registry defines a petroleum barrel
as 42 U.S. gallons. Field/SI parity is tested using the same physical inputs.
Conversions follow [NIST SP 811, Appendix B.8](https://www.nist.gov/pml/special-publication-811/nist-guide-si-appendix-b-conversion-factors/nist-guide-si-appendix-b8).

For casing ID Dc, ring OD Dp, tool speed vt and volume rate Q:

```text
Ac = pi Dc^2 / 4; Ap = pi Dp^2 / 4; Ab = Ac - Ap; beta = Ab / Ac
vb = (Q - vt Ac) / Ab
dp = K rho vb^2 / 2
Fh = dp Ac
```

Continuity and minor loss are classical fluid mechanics:
[DOE-HDBK-1012/3-92, Fluid Flow, Continuity Equation and Head Loss](https://www.energy.gov/ehss/articles/doe-hdbk-10123-92).
The moving-boundary balance and the effective casing area Ac are explicit
assumptions of this task's reduced model. Plug area Ap is **not** used as the
driving area. K = 2.0, with a representative bracket of 1.5 to 2.5, requires
calibration against measured restriction differential pressure:
`K = 2 dp_measured / (rho vb^2)`. Calibration is not clipped to the bracket.

The task's lumped-section resistance and solved line speed are:

```text
BF = 1 - rho / rho_material
Fres = mu (1+e) [Wbha' sin(theta) + wc' Llat]
       - (Wbha' + wc' Llat) cos(theta)
vt = Q/Ac - beta sqrt[2 Fres / (K rho Ac)]
sigma = 1 - vt / (Q/Ac) = sqrt(Fres/Fh_static)  (moving branch)
```

Theta is inclination from vertical. Weight resolution and Coulomb friction
follow [OpenStax University Physics 1, section 6.2](https://openstax.org/books/university-physics-volume-1/pages/6-2-friction);
the buoyancy ratio follows [section 14.4](https://openstax.org/books/university-physics-volume-1/pages/14-4-archimedes-principle-and-buoyancy).
The lumped cable normal-load term assumes a lateral. Mu = 0.30, e = 0.12 and
rho_material = 65.4 ppg are representative inputs from the task, not certified
properties. Use the cable certificate's submerged weight for composite cable.
Negative Fres indicates gravity assistance and is rejected by this forward
pump-down speed branch; it requires a braking/reverse-flow calculation.

At Fres >= Fh_static, vt is exactly zero and stall is true. Sigma is 1 during
positive-rate stall and is undefined (`None`) at zero rate. At zero resistance
and positive rate, vt equals Q/Ac. The field convenience shortcut
`1029.4 Q / ID^2` gives ft/min; the solved speed uses exact unit conversions
(the corresponding coefficient is 1029.4142).

## Restriction and line-speed result

The example uses ID = 4.778 in (5.5 in, 20 lb/ft casing), Q = 15 bbl/min,
rho = 8.34 ppg and K = 2.0. Ac = 17.93008 in2 and Q = 1.403646 ft3/s.

| Ring or plug OD (in) | Ab (in2) | beta (-) | Static vb (ft/s) | Static dp (psi) | Static Fh (lbf) |
|---:|---:|---:|---:|---:|---:|
| 4.50 | 2.02576 | 0.112981 | 99.7771 | 134.0576 | 2403.6640 |
| 3.54 | 8.08778 | 0.451073 | 24.9914 | 8.4103 | 150.7971 |

Table 1. Independently calculated static restriction loads; continuity and
minor-loss basis above. The force ratio follows beta^-2 at fixed casing and rate.

For 4,900 ft of lateral cable, buoyed BHA weight 873 lbf and buoyed cable
weight 229 lbf/1,000 ft:

```text
Fres = 0.30 * 1.12 * (873 + 0.229 * 4900) = 670.3536 lbf
sigma = sqrt(670.3536 / 2403.663955) = 0.528099
Q/Ac = 676.3774 ft/min
vt = 676.3774 * (1 - 0.528099) = 319.1834 ft/min
```

For this example, 4,900 ft is the midpoint of a representative 9,800 ft lateral.
The equation gives sigma = 0.5281, **not** the 0.08 design value mentioned in
the task's source critique. Adopting 0.08 would give 622.27 ft/min, about 1.95
times the solved speed. At the solved speed the ring's bypass velocity is
52.692 ft/s, dp = 37.3871 psi and Fh = Fres = 670.3536 lbf; the static dp
must not be reported as the moving differential pressure. The bare 3.54 in
plug stalls against that resistance at 15 bbl/min.

Constant-speed transit over 1,000 ft at 319.1834 ft/min takes 3.1330 min and
pumps 46.9949 bbl at 15 bbl/min. A varying-resistance stage needs piecewise
integration over positions; a single midpoint speed is only an approximation.

## Pipe friction and friction reducer

Swamee-Jain is reused through `digitalmodel.common.hydraulics`, with its old
`field_development.onshore_layout` import retained. CT hydraulics can import
the shared helper; its existing Haaland correlation is unchanged.

```text
Re = rho abs(v) D / dynamic_viscosity
fD = 0.25 / log10[eps/(3.7D) + 5.74/Re^0.9]^2
dp/dL = fD rho v^2 / (2D)
```

For water viscosity 1.0 cP, representative roughness eps = 0.0018 in and
full-bore mean velocity Q/Ac, Re = 416,726.7, fD = 0.0171502 and the gradient
is **0.0368536 psi/ft**. The units, worked through:

```text
v   = 1.403646 ft3/s / 0.124514 ft2          = 11.2730 ft/s
rho = 8.34 ppg * 7.480519 gal/ft3            = 62.3875 lbm/ft3
dp/dL = fD rho v^2 / (2 gc D)
      = 0.0171502 * 62.3875 * 11.2730^2 / (2 * 32.174 * 0.398167)
      = 5.30692 lbf/ft2 per ft               = 0.0368536 psi/ft
```

The issue's target of 0.147 psi/ft is four times this value and is not used.
It comes from the common field shortcut
`dp/dL [psi/ft] = f rho[ppg] v[ft/s]^2 / (25.81 D[in])`. The constant 25.81
is derived from `2 fF rho v^2 / D`, so the shortcut takes the **Fanning**
factor fF = fD/4 = 0.0042876. With fF it gives 0.03685 psi/ft. Feeding it
the Darcy factor 0.01715 gives 0.1474 psi/ft. An annulus uses Dh = Dc - Dt
and its own flow area; it must not use full-bore Q/Ac for annular velocity.

The correlation is [Swamee and Jain (1976), J. Hydraulics Div. 102(5), 657–664](https://doi.org/10.1061/JYCEAJ.0004542).
The public [INL OpenPronghorn theory implementation](https://openpronghorn.inl.gov/modules/thermal_hydraulics/theory_manual/path_integrated_incompressible_model/index.html)
also documents it. The helper uses 64/Re below 2,300 and labels the
2,300–5,000 band as a screening extrapolation. Roughness requires checking
against casing condition; the example is not a measured roughness estimate.

Friction-reducer loss is water loss times (1 - DR), with the task's empirical
fit DR(c) = DRmax c/(c+c50). DRmax and c50 have no defaults: measured values
are required, with c and c50 in the same caller-selected concentration units.
[Virk (1975), AIChE J. 21(4), 625–656](https://doi.org/10.1002/aic.690210402)
is the polymer-drag-reduction reference; the rational concentration fit is
**not** a Virk correlation or a maximum-drag-reduction law.

Optional smooth-pipe power-law flow uses the generalized Reynolds number of
[Metzner and Reed (1955)](https://doi.org/10.1002/aic.690010409) and the implicit
Fanning-factor resistance law of [Dodge and Metzner (1959)](https://doi.org/10.1002/aic.690050214),
converted to Darcy by multiplying by four.
Inputs are consistency K in Pa*s^n and 0.3 <= n <= 1. Laminar Darcy = 64/Re_g
below 2,100; the turbulent branch does not resolve transition, rough-pipe,
viscoelastic or eccentric-annulus effects. It is an alternative friction
factor calculation; combining it with polymer DR needs independent calibration.

## Tension, weak-point comparison and stretch

The body-only critical angle atan(1/mu) is 78.690, 73.301 and 68.199 degrees
at mu = 0.20, 0.30 and 0.40. With an eccentricity multiplier, use the effective
mu(1+e) instead. A 90-degree turn at mu = 0.30 has high/low tension ratio
exp(0.30*pi/2) = 1.60198, from the
[Euler-Eytelwein capstan equation](https://sciencedemonstrations.fas.harvard.edu/presentations/friction).

`tension_profile` consumes the existing `well.wellpath` survey station schema:
`measured_depth_m`, `inclination_deg`, `azimuth_deg`. MD starts at zero and
increases. Dogleg angle includes changes in azimuth as well as inclination.
The bottom tension and direction (`payout` or `pickup`) are explicit inputs.
Gravity/contact loads and capstan curvature are integrated along each
minimum-curvature arc. The scalar addition of gravity-normal and
curvature-normal loads is a screening approximation. Substeps use midpoint
tangents (20 m and 1 degree by default); exact exponential updates and their
integrals are used within each substep. Refine the controls for survey
convergence. A boundary requiring negative tension is rejected as slack.

Stretch is integral T(s)/(EA) ds, following
[OpenStax section 12.3, tensile strain](https://openstax.org/books/university-physics-volume-1/pages/12-3-stress-strain-and-elastic-modulus).
The certificate's effective EA is required. Surface tension times full cable
length is not used. For an ideal 9,500 ft vertical, zero heel tension, and
4,900 ft horizontal payout, the cable-head tension is 377.0256 lbf and
surface tension is 2,175.5 lbf. The full **9,500 ft** vertical length is used;
substituting 4,900 ft would give only 1,122.1 lbf of vertical cable weight.
At representative EA = 100,000 lbf, stretch is 112.5734 ft. This EA is an
illustration, not a cable certificate value.

The table helper uses this ideal vertical/horizontal shape with a massless
90-degree heel at zero tension; the capstan multiplier therefore acts on zero
at the heel. Use `tension_profile` with the actual build survey and a stated
bottom boundary to evaluate build-section effects. Table margin is the
supplied weak-point limit minus the **maximum** cable-profile tension, a
conservative cable-wide comparison; it is not a prediction of load at a
particular weak-point location. Stalled rows retain the incipient-payout
tension reference, not a unique static-friction equilibrium.

## Geometry clearance and use

API Spec 5CT sets the standard drift-mandrel diameter for casing smaller
than 9-5/8 in OD at ID - 1/8 in, which gives a 4.653 in nominal drift for this
4.778 in ID casing. Six small-casing strings (5 in 11.5 lb/ft and five 5-1/2 in
weights) are checked in the YAML against their standard drift values. The
live `CASING_SIZES` catalog has more than six entries.
The helper `api_drift(ID)` implements the requested ID - 0.125 in convenience;
it cannot infer product-specific rules from ID alone. The `casing()` factory
populates this nominal `drift_in` only for OD <= 8.625 in. Larger casing and
the tubing factory leave drift unknown unless supplied explicitly for casing.

For the 4.50 in ring, (4.653 - 4.50)/2 = 0.0765 in, below the representative
minimum radial clearance of 0.10 in. `runnable` therefore returns false; the
largest OD that passes at this drift is 4.653 - 2(0.10) = 4.453 in. The
hydraulic example keeps the 4.50 in ring because it is the specified base
case; it is not an overall runnability approval. The measured casing
tally and vendor pumpability chart are needed before geometry is qualified.

```python
from digitalmodel.units import Q_
from digitalmodel.well.completion.pump_down import bypass, rate_position_table

rows = rate_position_table(
    rates=[Q_(5, "bbl/min"), Q_(15, "bbl/min")],
    lateral_positions=[Q_(2450, "ft"), Q_(4900, "ft")],
    geometry=bypass(4.778, 4.50), density=Q_(8.34, "ppg"),
    buoyed_bha_weight=Q_(873, "lbf"),
    buoyed_cable_weight=Q_(229, "lbf/kft"),
    vertical_length=Q_(9500, "ft"),
    cable_axial_stiffness=Q_(100000, "lbf"), weak_point=Q_(4000, "lbf"),
)
for row in rows:
    print(row["rate"].to("bbl/min"), row["line_speed"].to("ft/min"), row["stall"])
```

Each row carries beta, vb, moving dp/Fh, static hydraulic capacity, Fres, vt,
sigma, stall, surface and cable-head tension, maximum tension, weak-point
margin, stretch and basis references. No files are written by the model.
The example's cable weights, stiffness, weak-point limit, geometry and loss
coefficients require field verification. Impact and pressure-transient
examples are deliberately excluded from this steady-core job.
