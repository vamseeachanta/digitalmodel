# Method and its limits

Rod string article ("Dynamometer card: through the rod string"), revision 3, 20 Nov 2026. This page holds in full what Appendix C of the article summarises. `examples/domains/artificial_lift/build_rod_string_article.py` writes it from `results.json`; the results come from the library at [a26efaa4](https://github.com/vamseeachanta/digitalmodel/tree/a26efaa413c962105cdd49ce6b5122b5d0ab0b58/) (version of 19 Nov 2026).

Purpose:

- Describe how the pump card is recovered from the surface card.
- State where the method breaks down and which items are open in the library.

## Key conclusion

- The march is stable up to 6,380 samples a stroke on this well's 200-point grid; the example card has 187.
- Above the limit the pump card grows without bound and the library raises no flag; the check that refuses such a card is in review, not merged.

## Method

- Finite-difference method, the default: the damped wave equation for axial rod motion is marched down the rods one grid point at a time, from the measured position and load at the polished rod, after Everitt and Jennings (1992), [SPE-18189-PA](https://doi.org/10.2118/18189-PA). The load at each depth is rebuilt from the strain, F = E A du/dx, and the buoyant weight of the rods below is added back.
- The pump card is the last grid point, the end of the listed rods (ASSUMED - to be confirmed as the pump); its load is smoothed with a Savitzky-Golay filter, as in the library.
- Frequency-domain method: the rod model of Gibbs (1963), [SPE-588-PA](https://doi.org/10.2118/588-PA), harmonic by harmonic. Its coupled load transfer was corrected in the library on 13 Nov 2026; this article uses the default method only.

## Stability limit

- Condition: a stress wave takes at least one time step to cross one grid step, Δt ≥ Δx / c (Courant, Friedrichs and Lewy, 1928, [doi:10.1007/BF01448839](https://doi.org/10.1007/BF01448839)), with depth as the marching direction. The wave speed of an elastic bar is c = (E/ρ)^0.5 (Bower, Applied Mechanics of Solids, online edition, section 4.3.3).
- This well: Δx = 4,750 ft / 199 = 23.87 ft (7.275 m); c = 4,965 m/s (about 16,300 ft/s), with the rod density of 8,531 kg/m³ from the record's weight per foot; stroke period 9.35 s. Limit: floor(9.35 × 4,965 / 7.275) = 6,380 samples a stroke.
- Demonstration: the example card resampled periodically to each count and run through the same solver.

| Samples a stroke | Courant number | Maximum pump load (lbf) | Minimum pump load (lbf) | Solver flags |
| --- | --- | --- | --- | --- |
| 187 | 0.02931 | 4,661 | 82 | none |
| 1,000 | 0.15673 | 4,666 | 34 | none |
| 6,380 | 0.99992 | 4,684 | 21 | none |
| 6,381 | 1.00008 | 4,840 | −2,411 | none |
| 6,400 | 1.00306 | 7.9 × 10^13 | −1.3 × 10^13 | none |

Table 1. Pump card against card sampling; a Courant number above 1 breaks the condition.

- Below the limit the minimum pump load also changes with the sampling, because the smoothing window is a fixed number of samples; the maximum load and the stroke change by less than 1 %.

## Open items in the library

- The Python march has no sampling check (digitalmodel issue 4159); the fix is in review in pull request 4162, not merged.
- Deviation and rod-on-tubing friction are off by default (issue 1894); this well's survey is vertical, so they do not change it.
- Damping is not calibrated against a measured pump card (pull request 3964, limits), and a viscosity at or below 1 cP is replaced by 10 cP in the damping estimate (issue 4176).
- The rod-string functions used for the hand checks have value tests in this change; issue 3355 asks for tests of the rest of `rod_string.py` and `kinematics.py`.
