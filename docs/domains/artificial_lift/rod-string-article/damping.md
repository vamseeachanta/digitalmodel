# Damping

Rod string article ("Dynamometer card: through the rod string"), revision 3, 20 Nov 2026. This page holds in full what Appendix B of the article summarises. `examples/domains/artificial_lift/build_rod_string_article.py` writes it from `results.json`; the results come from the library at [a26efaa4](https://github.com/vamseeachanta/digitalmodel/tree/a26efaa413c962105cdd49ce6b5122b5d0ab0b58/) (version of 19 Nov 2026).

Purpose:

- Show how the damping estimate changes the pump card of the example well.
- State the energy check and what the damping estimate rests on.

## Key conclusion

- With no damping the pump card's area equals the surface card's within 0.005 %: the energy put in at the polished rod reaches the pump, an energy check on the calculation.
- The estimated damping takes 0.7 % of the 7,960 ft-lbf of work per stroke.
- For this well, across a ten-fold range of damping, the maximum pump load changes by 0.34 %, the peak stress at the top of the 3/4 in rods by 0.06 % and the stroke by 0.03 in: the top of the card, which sets the rod stress, hardly moves, so the damping does not change the rod stress check. The damping changes the card's area and raises the bottom of the card.
- The gap between the wave-equation stroke and the static-stretch stroke at the pump is 1.45 in with no damping and 1.51 in with the estimate: the rods' motion makes it, not the damping.

## The estimate

- The library estimates a viscous damping coefficient for each taper and each stroke direction from the drag of the rod body and couplings in the tubing (`everitt_jennings/damping.py`).
- Inputs: fluid viscosity 10 cP, tubing inside diameter 2.441 in and coupling diameter 1.5 × rod diameter, all ASSUMED - to be confirmed.
- Coefficients (1/s), 7/8 in and 3/4 in rods: upstroke 0.0096 and 0.0110; downstroke 0.0374 and 0.0366. The well record stores 0.0095, 0.0109, 0.0376 and 0.0367, within 1.1 % of the estimate.
- The well produces 96 % water, so a viscosity near water's, about 1 cP or less, is more realistic than 10 cP. The library raises any value at or below 1 cP to 10 cP (issue 4176), so 10 cP is kept. The coefficient is proportional to the viscosity, so the scales below also stand for viscosities from 0 to 100 cP and cover a water-like fluid.
- The estimate has not been calibrated against a measured pump card.

## Damping sweep

| Damping scale | Maximum pump load (lbf) | Minimum pump load (lbf) | Fillage (%) | Fluid load (lbf) | Pump card area (ft-lbf) | Damping share (%) | Change of pump card (% of load range) | Stroke gap to static stretch (in) | Change of peak stress, top of 3/4 in rods (%) |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| 0 | 4,662 | 67 | 60.15 | 3,324 | 7,959 | 0.00 | 0.15 | 1.45 | 0.006 |
| 0.5 | 4,661 | 75 | 60.11 | 3,317 | 7,932 | 0.35 | 0.07 | 1.48 | 0.003 |
| 1 | 4,661 | 82 | 60.08 | 3,310 | 7,905 | 0.69 | 0.00 | 1.51 | 0.000 |
| 2 | 4,659 | 97 | 60.00 | 3,297 | 7,850 | 1.38 | 0.15 | 1.56 | 0.006 |
| 5 | 4,654 | 140 | 59.78 | 3,273 | 7,686 | 3.44 | 0.59 | 1.66 | 0.024 |
| 10 | 4,645 | 213 | 59.41 | 3,205 | 7,413 | 6.87 | 1.34 | 1.94 | 0.055 |

Table 1. Pump card against damping scale; 1 is the estimate. Change of pump card: root-mean-square load difference from the estimate's card, over its load range. Change of peak stress: against the estimate's 21,570 psi.

- Data: [figure-4-damping-cards.csv](figure-4-damping-cards.csv) (scales 0, 1 and 10) and [results.json](results.json).
