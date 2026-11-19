# Example well, stretch, stress and sensitivity

Rod string article ("Rod string mechanics: what stretch, load and damping do to the dynamometer card"), revision 1, 19 nov 2026. This page holds in full what Appendix A of the article summarises. `examples/domains/artificial_lift/build_rod_string_article.py` writes it from `results.json`; the results come from the library at [a26efaa4](https://github.com/vamseeachanta/digitalmodel/tree/a26efaa413c962105cdd49ce6b5122b5d0ab0b58/) (version of 19 Nov 2026).

Purpose:

- Give the inputs, assumptions and results of the example well behind Figures 1 to 3.
- Give the sensitivity of the key results to each input at ±1 % and ±5 %.

The well is one anonymised field well in this library: [cleansed_well_001.json](https://github.com/vamseeachanta/digitalmodel/blob/a26efaa413c962105cdd49ce6b5122b5d0ab0b58/tests/marine_ops/artificial_lift/test_data/cleansed_well_001.json), version of 19 Nov 2026.

## Inputs

| Input | Value | Unit |
| --- | --- | --- |
| Pumping speed | 6.42 | strokes/min |
| Pump depth | 4,844 | ft |
| Plunger diameter | 1.500 | in |
| Rod string, surface first | 0.875 in × 1,400 ft; 0.75 in × 3,350 ft | in; ft |
| Rod modulus of elasticity, from the record | 30,500,000 | psi |
| Rod weight per foot, from the record | 2.224; 1.634 | lb/ft |
| Fluid density | 63.237 | lb/ft³ |
| Survey, maximum inclination | 0.0 | deg |

Table 1. Example well inputs, from the well record.

## Assumed values

| Assumed value | Value | Why it is used |
| --- | --- | --- |
| Fluid viscosity | 10 cP, ASSUMED - to be confirmed | Library default for a light crude; the record gives no viscosity. It sets the damping only. |
| Tubing bore | 2.441 in, ASSUMED - to be confirmed | Drift bore of 2-7/8 in tubing, the library default; the record gives no tubing. It sets the damping only. |
| Coupling diameter | 1.5 × rod diameter, ASSUMED - to be confirmed | Solver default; the record gives no coupling size. It sets the damping only. |
| Rod end at the pump | 4,750 ft, ASSUMED - to be confirmed | The record lists 4,750 ft of rods and a pump at 4,844 ft; the end of the listed rods is taken as the pump. |
| Tubing | anchored, ASSUMED - to be confirmed | Unanchored tubing movement is not modelled. |

Table 2. Assumed values.

## Results

| Result | Value | Unit |
| --- | --- | --- |
| Surface stroke | 53.99 | in |
| Pump stroke | 39.57 | in |
| Stroke lost | 14.42 | in |
| Fluid load, from the pump card | 3,310 | lbf |
| Static stretch, 7/8 in; 3/4 in; total | 3.03; 9.88; 12.91 | in |
| Static stretch as a share of the stroke lost | 89.5 | % |
| Remainder: rod motion and damping | 1.51 | in |
| Stroke at the taper joint | 50.79 | in |
| Peak and minimum load, polished rod | 12,240; 7,296 | lbf |
| Peak and minimum load, taper joint | 9,531; 4,618 | lbf |
| Peak and minimum stress, top of 7/8 in rods | 20,360; 12,133 | psi |
| Peak and minimum stress, top of 3/4 in rods | 21,570; 10,453 | psi |
| Stress range, top of 7/8 in; top of 3/4 in rods | 8,222; 11,120 | psi |

Table 3. Example well results, finite-difference method, 200 grid points, estimated damping.

## How the stretch and stress are worked

- Static stretch of each taper: fluid load × 12 × length / (area × modulus), in inches, with the rod elastic constant of `rod_pump/rod_string.py` (`rod_elastic_constant`); the bar relation is Bower, Applied Mechanics of Solids, online edition, section 4.3.3.
- Hand check: 7/8 in rods, area 0.6013 in²; 3/4 in rods, area 0.4418 in²; modulus 30,500,000 psi. Stretch = 3,310 × (12 × 1,400 / (0.6013 × 30,500,000) + 12 × 3,350 / (0.4418 × 30,500,000)) = 12.91 in.
- Stress at the polished rod: 12,240 lbf / 0.6013 in² = 20,360 psi. Stress at the top of the 3/4 in rods: the peak rod load interpolated to 1,400 ft, 9,531 lbf, over 0.4418 in² = 21,570 psi.
- The solver averages the load at the grid point at the taper joint between its neighbours, because the strain-derived load steps where the area steps.
- No allowable stress is applied: the record gives no rod grade.

## Sensitivity

Each input is changed by ±1 % and ±5 %, one at a time, all rod sections together; a diameter change keeps the rod's density by scaling its weight with its area. The factor is the percent change of the result divided by the percent change of the input, from the central difference.

| Input | Step | Pump stroke | Static stretch | Peak pump load | Fillage | Peak stress, top of 3/4 in rods | Damping share |
| --- | --- | --- | --- | --- | --- | --- | --- |
| Rod modulus of elasticity | ±1 % | 0.32 | −0.97 | 0.03 | −0.47 | 0.00 | 0.17 |
| Rod modulus of elasticity | ±5 % | 0.33 | −1.25 | 0.03 | −0.46 | 0.00 | 0.17 |
| Rod diameter (weight with area) | ±1 % | 0.58 | −2.17 | −3.14 | −0.94 | −2.56 | 2.17 |
| Rod diameter (weight with area) | ±5 % | 0.60 | −2.20 | −3.14 | −0.93 | −2.57 | 2.17 |
| Rod weight per foot | ±1 % | −0.03 | −0.26 | −1.82 | 0.00 | −0.32 | −0.02 |
| Rod weight per foot | ±5 % | −0.03 | 0.20 | −1.82 | 0.00 | −0.32 | −0.02 |
| Fluid density | ±1 % | 0.00 | 0.00 | 0.22 | 0.00 | 0.04 | 0.00 |
| Fluid density | ±5 % | 0.00 | 0.00 | 0.22 | 0.00 | 0.04 | 0.00 |
| Pumping speed | ±1 % | −0.06 | −0.22 | 0.04 | −0.01 | 0.01 | 0.95 |
| Pumping speed | ±5 % | −0.06 | 0.31 | 0.04 | −0.01 | 0.01 | 0.95 |
| Fluid viscosity (damping) | ±1 % | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.99 |
| Fluid viscosity (damping) | ±5 % | 0.00 | 0.00 | 0.00 | 0.00 | 0.00 | 0.99 |
| Tubing bore (damping) | ±1 % | 0.00 | 0.01 | 0.00 | 0.00 | 0.00 | −1.76 |
| Tubing bore (damping) | ±5 % | 0.00 | 0.01 | 0.00 | 0.00 | 0.00 | −1.78 |
| Load-cell gain | ±1 % | −0.29 | 1.24 | 2.57 | 0.47 | 1.28 | −1.14 |
| Load-cell gain | ±5 % | −0.29 | 1.05 | 2.57 | 0.46 | 1.28 | −1.14 |
| Surface position scale | ±1 % | 1.29 | −0.23 | 0.05 | −0.47 | 0.01 | 1.14 |
| Surface position scale | ±5 % | 1.29 | −0.05 | 0.05 | −0.47 | 0.01 | 1.14 |

Table 4. Sensitivity factors (percent change of the result per percent change of the input).

- Fillage and fluid load step with the corner samples the library detects on the pump card, so their factors and the static stretch factor can differ between the 1 % and 5 % steps.
- Data: [sensitivity.csv](sensitivity.csv).
