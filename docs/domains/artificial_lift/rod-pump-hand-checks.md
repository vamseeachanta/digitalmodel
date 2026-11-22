# Rod-pump mechanics hand checks

The tests check the declared rod-string and sinusoidal crank models using
synthetic inputs, independent arithmetic and public mechanics references.
Production code is unchanged. These checks do not qualify an installation or
validate an empirical pumping-unit model.

## Revision history

| Revision | Date | Change and reason |
|---|---|---|
| 1 | 2026-10-22 | Added hand checks and a worked example to distinguish numeric coverage from name references. |

## Sources and method

The following public sources were opened successfully on 2026-10-22. Source
responses, addresses, retrieval times and HTTP status were retained as check
evidence outside the repository. No licensed publication was downloaded.

| Source | Basis used |
|---|---|
| [OpenStax, University Physics Volume 1 (2016), section 12.3](https://openstax.org/books/university-physics-volume-1/pages/12-3-stress-strain-and-elastic-modulus) | Stress, strain and axial extension: delta = F L / (E A); stiffness is the inverse compliance. |
| [Same text, section 14.1](https://openstax.org/books/university-physics-volume-1/pages/14-1-fluids-density-and-pressure) | Hydrostatic pressure rho g h, pressure force p A, and freshwater density at its stated temperature. |
| [Same text, section 14.4](https://openstax.org/books/university-physics-volume-1/pages/14-4-archimedes-principle-and-buoyancy) | Buoyancy equals the displaced fluid weight. |
| [OpenStax, College Physics 2e (2022), section 11.2, table 11.1](https://openstax.org/books/college-physics-2e/pages/11-2-density) | Generic iron/steel density of 7.8 g/cm³ and water density of 1.0 g/cm³; no alloy-specific qualification is implied. |
| [OpenStax, University Physics Volume 1 (2016), section 15.1, equations 15.3, 15.4 and 15.7](https://openstax.org/books/university-physics-volume-1/pages/15-1-simple-harmonic-motion) | Harmonic position, velocity and period; translate the origin to the bottom of stroke. |
| [NIST SP 811 (2008), appendix B](https://nvlpubs.nist.gov/nistpubs/Legacy/SP/nistspecialpublication811e2008.pdf) | International length units, pressure, standard gravity, time and petroleum barrel conversion. |

A circle has area A = pi d² / 4. Axial compliance per foot is 12 / (E A),
because a foot is 12 inches; total compliance multiplies by rod length in
feet. The modulus and rod weight in the worked example are **synthetic inputs**,
not inferred material or coupling properties.

The plunger load uses its full bore area times the column pressure plus tubing
minus casing pressure. The interface has no rod diameter for subtracting an
annular area. Whether another load definition needs an annulus is outside
these checks.

For buoyancy, Wb = Wa (1 - SGfluid / SGsteel). The density table rounds steel
to one decimal place. Propagating half the last tabulated digit bounds the
weight rounding by 0.9 lb for the synthetic 1000 lb air weight at fluid SG=1.
The tests use that bound, rather than deriving the expectation from the
implementation's buoyancy coefficient.

For water at 4°C, pressure is calculated in SI from the cited density and
standard gravity, then converted to psi and feet. A relative tolerance of
2e-3 bounds the module's approximate water gradient. This is a numerical
approximation allowance, not a physical acceptance criterion.

Displacement follows swept cylindrical volume per stroke. Appendix B gives a
petroleum barrel as 42 U.S. gallons; the gallon and cubic-inch conversions give
231 in³ per gallon to tabulated precision. There are 24 × 60 minutes per day.
Thus the conversion is (24 × 60) / (42 × 231), multiplying A × Sp × N.
The tests use a relative tolerance of 2e-4 for the module's rounded conversion.
They do not use the module constant to construct the expected answer.

Other numeric tolerances cover only the last retained decimal digit and binary
floating-point/trigonometric rounding. Exact small-integer cases use equality.
Neither tolerance is an engineering qualification limit.

## Worked single-diameter example

Use a synthetic 1.0 in diameter rod, 1000.0 ft length, 100.0 in surface stroke,
modulus 12.0 million psi and air weight 1.0 lb/ft. Use a 1.0 in plunger, zero
column height, zero fluid specific gravity and tubing/casing pressures of
125.0/25.0 psi. The zero-density fluid isolates the pressure-difference load
and removes any dependency on an empirical buoyancy coefficient.

1. A = pi/4 in²; per-foot elastic compliance = 12/(12000000 A).
2. Total length is 12000 in, so k = 12000000 A / 12000 = 250 pi lb/in.
3. Air and buoyed weights are both 1000.0 lb; Fo = (125 - 25) A = 25 pi lb.
4. Fo/k = 0.1 in, giving Sp = 99.9 in without overtravel.
5. At 10.0 strokes/min, displacement is A × 99.9 × 10 × (1440/9702).

The following example prints rounded answers. The tests retain more digits
and state the reason for each tolerance.

```python
from digitalmodel.marine_ops.artificial_lift.dynacard.rod_pump import (
    analyse_rod_string,
    plunger_stroke,
    pump_displacement,
)

rod = analyse_rod_string(
    diameter_in=1.0, length_ft=1000.0, surface_stroke_in=100.0,
    plunger_diameter_in=1.0, fluid_level_ft=0.0, specific_gravity=0.0,
    tubing_pressure_psi=125.0, casing_pressure_psi=25.0,
    weight_lb_per_ft=1.0, modulus_psi=12_000_000,
)
stroke = plunger_stroke(100.0, rod.fluid_load_lb, rod.spring_rate_lb_per_in)
rate = pump_displacement(1.0, stroke, 10.0)
print(f"Spring rate: {rod.spring_rate_lb_per_in:.1f} lb/in")
print(f"Fluid load: {rod.fluid_load_lb:.1f} lb")
print(f"Plunger stroke: {stroke:.1f} in")
print(f"Displacement: {rate:.1f} bbl/day")
```

Expected output:

```text
Spring rate: 785.4 lb/in
Fluid load: 78.5 lb
Plunger stroke: 99.9 in
Displacement: 116.4 bbl/day
```

The exact unit conversion gives 116.5 bbl/day at one decimal; the module
conversion gives the printed 116.4 bbl/day. The unrounded results differ by
less than 0.1%, consistent with the conversion-rounding allowance. An efficiency example uses a
synthetic 100.0 bbl/day displacement: 12.0 operating hours provide 50.0 bbl;
25.0 surface bbl at a formation volume factor of 1.2 represent 30.0 reservoir
bbl, giving 60.0% efficiency. Runtime and volume factor must be supplied.

## Kinematics and timing cases

A synthetic 20.0 in stroke at 60.0 strokes/min has a 1.0 s period and 10.0 in
amplitude. The model x = (S/2)(1 - cos omega t) gives bottom, midpoint, top,
midpoint and bottom at successive quarter-periods. The velocity changes sign
on the second half-cycle. A quarter-stroke position gives cos omega t = 1/2,
so the first-half-cycle inverse is one sixth of the period. The tests check
these independent hand values as well as forward/inverse consistency.

Time uncertainty uses the local inverse slope, dt = dx/abs(v); the digitizing
error is supplied explicitly. The interval gate adds endpoint uncertainties
and treats touching closed intervals as overlapping. Tests exercise below,
exactly at and above that boundary; no statistical confidence claim is made.

A synthetic natural frequency of 120.0 cycles/min means a 0.5 s peak interval.
At 30.0 strokes/min the half-stroke lasts 1.0 s, so two undulations fit.
The declared phase-origin model places upstroke peaks at 0.5, 1.0, 1.5 and
2.0 s and downstroke peaks at 1.5 and 2.0 s over one full cycle. Short horizons
and the exact horizon endpoint are checked. Divergence onset checks the
first-turnaround rule on synthetic speeds, including reordered duplicates.

## Later slices and NOT_EVALUATED checks

| Check | Status and next step |
|---|---|
| Absolute natural frequency, steel sonic speed, default modulus and coupled-rod weights | NOT_EVALUATED: source comments cite API RP 11L without edition-specific evidence in hand. Tests use inverse-length invariance, or explicit modulus and weight inputs. Obtain a public formula/property basis before certifying defaults. |
| Non-unit taper correction and inconsistent taper sections | NOT_EVALUATED: these functions accept a scalar correction and single diameter, with no taper-section representation to validate. Obtain the correlation source and define section consistency upstream. |
| Fixed-free exact wave-speed check | NOT_EVALUATED: the current function returns its fixed API coefficient even when agreement checking is disabled; a supplied sonic speed cannot select a different absolute frequency. |
| Overtravel, peak excitation phase and physical divergence | NOT_EVALUATED: provided overtravel is additive; peak times and divergence tests check declared timing identities, not empirical excitation or measured traces. |
| Efficiency outside zero to one | Validation gap: negative production and production exceeding displacement are not refused or bounded; runtime above a day and non-positive volume factor are also accepted. Existing documented None boundaries and valid ratios are tested. |
| Non-finite inputs | Validation gap: most positive checks accept NaN and positive infinity. Card-position bounds refuse NaN and infinities; negative infinity is refused by existing positive-value checks. No general finite-input policy exists. |
| Non-positive physical inputs | Partial guards exist for rod diameter, length, taper factor, undulation speed and resulting plunger stroke. Modulus, spring rate, pump stroke/speed, fluid column/SG and other crank/timing calls lack consistent guards. Fixes need a separate validation change. |
| Top-of-stroke timing uncertainty | The model velocity should vanish, but floating-point sine at pi can produce a very large finite uncertainty. Only the exact bottom-end infinity is asserted. Define a documented endpoint rule before changing behavior. |

## Unit suffix families

An untyped positive number cannot reliably identify a unit mistake. There is
no general magnitude detector; the following concrete probes were inspected
without adding tests that would enshrine their invalid use as supported input.
All conversions below come from NIST SP 811 appendix B.

| Family | Wrong-unit probe | Existing outcome or gap |
|---|---|---|
| `_in`, including stroke and position | Supply 0.0254 (metres for 1 inch) as rod diameter | Tested refusal by the standard-size weight lookup only. Supplying explicit weight would bypass it. Stroke and in-range position magnitudes have no unit guard. |
| `_ft` | Supply 304.8 (metres for 1000 feet) as rod length | Accepted as feet; no magnitude refusal. |
| `_psi` | Supply 82.7 (approximately GPa for the synthetic 12 million psi modulus) | Accepted as psi; no magnitude refusal. |
| `_lb`, `_lb_per_ft`, `_lb_per_in` | Supply 444.8 (approximately newtons for 100 pounds-force) as fluid load | Accepted as pounds-force; derived weights and stiffness have no magnitude refusal. |
| `_s`, `_ft_s`, strokes per minute and frequency `_spm` | Supply 1.0 (cycles per second for 60 cycles/min) as crank speed | Accepted as strokes/min. Time and sonic-speed unit errors have no general unit guard; the sonic agreement check is not a unit conversion. |
| `_bpd`, hours per day and dimensionless ratios | Supply 15.9 (approximately m³/day for 100 barrels/day) as measured rate | Accepted as barrels/day; no magnitude refusal. Missing runtime or volume factor returns None, but does not identify a unit error. |

## Test layout

The new tests live under
`tests/marine_ops/artificial_lift/dynacard/rod_pump/`, next to the existing
module-level regression file in the parent directory. The existing file is
unchanged. The inventory script remains external check evidence because the
repository has no `tests/_tools/` directory; its table is in the pull request.
