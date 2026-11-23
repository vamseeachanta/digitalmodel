# Analytical screening basis

These equations define the implemented, static, inextensible flexible-line
screen. They are derived below from equilibrium and geometry, not copied from a
standard. No standard allowable, plastic qualification, native solver result,
contents pressure, dynamic amplification or ovalisation result is asserted.
The inherited bibliographic references in the analytical module are background;
their editions and clause text have not been qualified by this change.

## Equations

- **E1 — circular annulus and buoyancy.** With steel diameter D, wall t and
  radial coating c, Di=D−2t and Dc=D+2c. Require 0<t<D/2.
  As=π(D²−Di²)/4, Ac=π(Dc²−D²)/4, Ad=πDc²/4.
  The empty pipe's submerged weight is w=g(ρs As+ρc Ac−ρwater Ad).
  The existing g=9.80665 m/s² and illustrative steel density 7850 kg/m³ are
  retained input conventions. Both templates now state the latter explicitly.
- **E2 — catenary force closure.** Horizontal force H is constant. At a
  horizontal touchdown tangent T0=H. Along the cable dT/ds=w dz/ds;
  integration from touchdown to height h gives T=H+w h. With top angle θ
  measured from vertical, H=T sin θ. Thus T=w h/(1−sin θ), H=T sin θ.
- **E3 — parameter.** a=H/w=h sin θ/(1−sin θ). A finite sagbend requires
  w>0 and θ>0. T>w h follows from H>0. A zero angle is a degenerate vertical
  line with zero sagbend radius; it cannot support this bending screen.
- **E4 — geometry.** The catenary is z=a(cosh(x/a)−1), whose arc length
  s=a sinh(x/a). At z=h: x=a acosh(1+h/a), s=√(h²+2ah).
- **E5 — bending screen.** At touchdown curvature is 1/a. Outer-fibre
  bending strain is ε=Dκ/2=D/(2a); the elastic stress proxy is Eε.
  Stinger radius is the rearrangement Rmin=D/(2εmax). This proxy does not
  establish elastic material response when the computed stress exceeds yield.
- **E6 — reel assumption.** κreel=1/R; assumed straightener removal fraction
  η gives κres=(1−η)/R. Then Rres=1/κres, εres=Dκres/2 and the small-slope
  span offset δ≈κres L²/8. At η=1, κres=εres=δ=0; Rres is unbounded,
  encoded as null with `residual_radius_status=unbounded_zero_curvature`.
- **E7 — operability assumption.** For J-lay, joining and laying must both
  be available: Hs_limit=min(Hs_lay,Hs_welding). Rate is zero above this
  limit, otherwise max(0,rate_calm−degradation*Hs). Reel screening retains
  the supplied lay-only limit. Equality to the limit is permitted.

## Validation criteria

These are screening input checks selected in
[audit 2745](https://github.com/vamseeachanta/digitalmodel/issues/2745),
not engineering design allowables.

- **V1:** The workflow requires depth, angle and tension. Depth and angle
  solve E2–E4; supplied tension is checked against derived tension with
  |Tsupplied−Tderived|/Tderived ≤0.001. The analytical class alone permits
  omitted tension to request a derived value. Angle remains at most 15°
  under the existing near-vertical applicability bound, now strictly positive.
- **V2:** Finite limits require 0<sagbend strain limit<0.01 and
  0<reeling strain limit<0.05. These plausibility bounds detect common percent
  versus fraction mistakes; they do not validate the selected criterion.
- **V3:** Finite overbend strain requires 0<ε<1; radius and measurement
  span require finite positive values; efficiency requires 0≤η≤1.
- **V4:** The pipe diameter, wall, coating thickness and all densities must
  be supplied to the workflow. Unknown keys are errors. Other applied
  numerical, method and output defaults are reported by their input paths.
  Direct analytical models retain their documented pipe-property defaults.

## Independently evaluated anchors

The J-lay template E1 arithmetic gives w=1658.331318 N/m. With h=1500 m,
θ=3°, E2–E5 give T=2624.872173 kN, a=82.839414 m,
ε=0.001954383687, x=301.745330 m and s=1580.670188 m.
The supplied rounded tension is 2624.9 kN. Tests use relative tolerance 0.001.

For the reel template D=0.2731 m, R=8 m, η=0.95 and L=12 m:
εreel=0.01706875, κres=0.00625 1/m, Rres=160 m,
εres=0.0008534375 and δ=0.1125 m (E5–E6).
The J-lay rate at Hs=2.5 m is 3−0.5×2.5=1.75 km/day;
at Hs=3 m it is zero because 3>2.5 m (E7).
