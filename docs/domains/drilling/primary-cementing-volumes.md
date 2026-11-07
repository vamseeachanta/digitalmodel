# Primary cementing volume calculations

## Summary

`digitalmodel.well.drilling.cementing.PrimaryCementing` calculates slurry,
spacer and displacement volumes for one uniform hole and casing string. It
also calculates top of cement from a placed slurry volume. The method is a
circular-area volume balance with caller-selected excess; it does not establish
cementing pressure limits, bond quality or barrier acceptance. Refs #3831.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| 1 | 2026-11-05 | Added uniform-geometry volume balances as the first cementing calculation slice. |

## Geometry and equations

Diameters are inches, volumes are barrels, and all depths and lengths are feet
measured along the casing from its entrance at zero. There is one hole
diameter, one casing OD and one casing ID throughout the interval. The float
collar lies between the entrance and the shoe. This datum is a volume reference;
it is not a pressure datum or an elevation reference.

One barrel is 42 U.S. gallons, and one gallon is 231 cubic inches. With
`k = 3*pi/(42*231)`, nominal annular capacity per foot is
`Ca = k*(hole_diameter_in^2 - casing_od_in^2)`, and casing bore capacity per foot
is `Ci = k*casing_id_in^2`. These follow directly from circular area times
12 inches per foot. No standard-specific factors or acceptance criteria enter
these equations.

Let `D` be shoe depth, `F` float-collar depth, `T` target cement top, `S` spacer
length and `e` annular excess fraction. Effective annular capacity is
`Ce = Ca*(1+e)`. The caller selects `e`; no excess recommendation is inferred.

| Quantity | Equation | Interpretation |
| --- | --- | --- |
| Nominal annular slurry | `Ca*(D-T)` | Nominal hole geometry below the target top |
| Annular excess | `e*Ca*(D-T)` | Additional annular volume budget |
| Shoe-track cement | `Ci*(D-F)` | Cement retained inside casing below the collar |
| Total slurry | `Ce*(D-T) + Ci*(D-F)` | Annular cement plus shoe-track cement |
| Spacer | `Ce*S` | Slug occupying the caller-specified annular length |
| Displacement to bump | `Ci*F` | Pumped volume after launching the top plug at the casing entrance |
| Top of cement | `D - (slurry_volume_bbl - Ci*(D-F))/Ce` | Cement top after complete placement, using the same excess |

Excess increases effective annular capacity in both sizing and inversion. It
does not enlarge the casing bore or improve the predicted cement top for a
fixed slurry quantity. Use zero excess when inverting on nominal geometry.

Nonfinite and negative quantities, Boolean inputs, nonpositive diameters and
shoe depth, invalid diameter ordering and a collar below the shoe are rejected.
The target cement top and spacer length must lie between zero and shoe depth.
Slurry below shoe-track capacity or above track plus full-annulus capacity is
rejected. Only floating-point roundoff at those volume boundaries is clamped.

## Hand calculation

A synthetic case uses a 10.0 in hole, 8.0 in casing OD, 6.0 in casing ID,
1000.0 ft shoe depth, 900.0 ft collar depth, 400.0 ft target cement top,
100.0 ft spacer length and 25.0% annular excess.

Both nominal annular area and casing bore area equal `9*pi` square inches.
Each nominal foot therefore contains `108*pi` cubic inches, or `6*pi/539`
barrels. The slurry budget contains 600.0 nominal annular feet, 150.0 equivalent
feet of excess and 100.0 casing bore feet. The spacer contains 125.0 equivalent
nominal annular feet; displacement contains 900.0 casing bore feet.

| Output | Volume, bbl |
| --- | ---: |
| Nominal annular slurry | 21.0 |
| Annular excess | 5.2 |
| Shoe-track cement | 3.5 |
| Total slurry | 29.7 |
| Spacer | 4.4 |
| Displacement to bump | 31.5 |

The tests use the unrounded rational multiples of `pi`. Inverting the total
slurry gives 400.0 ft using effective capacity and 250.0 ft using nominal
capacity. This checks the effect of the excess assumption on predicted height.

## Use

```python
from digitalmodel.well.drilling.cementing import PrimaryCementing

geometry = PrimaryCementing(
    hole_diameter_in=10.0,
    casing_od_in=8.0,
    casing_id_in=6.0,
    shoe_depth_ft=1000.0,
    float_collar_depth_ft=900.0,
    annular_excess_fraction=0.25,
)
volumes = geometry.plan(target_top_of_cement_ft=400.0, spacer_length_ft=100.0)
cement_top_ft = geometry.top_of_cement_ft(volumes.total_slurry_bbl)
```

## Limits and remaining work

Fluids are incompressible, placement is complete and no fluid is lost. This is
not a displacement-efficiency or spacer contact-time model. Surface lines,
plug dimensions, multiple annular sections, rat holes, returns, contamination,
compression and slurry yield from dry material are not modelled. Slurry volume
means mixed, placed fluid volume; spacer volume is separate. The predicted bump
volume excludes surface equipment and is not an operational bump confirmation.

Hydrostatic and displacement pressure against the existing drilling window,
bump pressure, two-barrier checks, pressure-test and cement-bond acceptance,
zone-specific abandonment criteria and kick tolerance remain open. The barrier
package proposed in #3822 is absent from this base; no parallel implementation
of it is introduced. No standards-based acceptance table or field validation
is established by these geometric tests.
