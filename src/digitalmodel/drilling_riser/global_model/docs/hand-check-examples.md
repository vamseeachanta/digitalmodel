# Drilling riser hand-check examples

The synthetic tests cover waterline geometry, submerged weight, joint effective
tension, lumped ring weight, constant-tension line stiffness, uniform string and
beam period limits, spec-based references and post-process channel selection.
They use independent hand calculations and hand-written result dictionaries.
They establish no qualification of a licensed solver or a field design.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 1 | 2026-10-22 | Added independent arithmetic examples and evidence limits for the reference calculations and readers. |

## Inputs and conventions

All example inputs are deliberately synthetic. Elevation is positive upward,
with the waterline at zero. Sections run from top to bottom. The actual
`LineSection` and `RiserGlobalModelSpec` constructors build the examples.

| Input or result | Unit | Example |
| --- | --- | --- |
| `length_m`, `top_z_m` | m | A 10 m section whose top is 4 m above water has 6 m wet length. |
| `mass_per_m_kg` | kg/m | Structural mass excludes bore contents. |
| `displaced_volume_per_m_m3` | m³/m | Only the wet length displaces water. |
| `rho_water`, `rho_contents` | kg/m³ | These arguments lack a suffix; they are mass densities. |
| `top_tension_n`, `submerged_weight_n` | N | Force uses mass times standard gravity, without a kN conversion. |
| `ei_nm2` | N.m² | Flexural rigidity, not a force or a spring stiffness. |
| `tensioner_lateral_stiffness_n_m` | N/m | The suffix denotes force per displacement. |
| Period result | s | First mode first, decreasing period. |

## Weight and effective-tension worked example

Archimedes' principle gives buoyancy equal to the weight of displaced water [1].
Use standard gravity, exactly 9.80665 m/s² [2]. A section's downward force is
the weight of its structure plus bore contents, less buoyancy of its wet part.

Section A is 10 m long, with structural mass 100 kg/m, bore area π m² and
contents density 10/π kg/m³. Its contents therefore weigh 10 kg/m before
multiplication by gravity. Its total mass is 1100 kg. Its top is at 4 m, so
6 m displaces 4 m³/m of water at a synthetic density of 25 kg/m³: 600 kg.
Its net submerged mass is 500 kg and its downward force is **4903.3 N**.

Section B is 5 m long, fully wet, with structural mass 200 kg/m, no bore,
and displaced volume 4 m³/m in the same water. Its net mass is also 500 kg.
Starting at **20000.0 N**, the effective tension falls to **15096.7 N** after A
and **10193.4 N** after B. The tests retain the unrounded arithmetic values.

The separate ring case uses mass 2000 kg and volume 1 m³ in water at
1000 kg/m³. Its force is **19613.3 N** above or exactly at the waterline and
**9806.7 N** below it. This records the implementation's lumped ring convention;
it does not model partial immersion of a finite ring.

## Natural periods and integrated worked example

For a pinned string, [3] gives wave speed √(Te/m) and frequency
`f_n = n sqrt(Te/m)/(2L)`. Thus `T_n = 2L/(n sqrt(Te/m))`.
For L = 100 m, Te = 10000 N and m = 100 kg/m, the first two periods are
**20.0 s** and **10.0 s**. The constant-tension WKB travel-time integral is
L/√(Te/m), so this limiting case must give the same answers to rounding error.
Variable-tension WKB accuracy is **NOT_EVALUATED**.

The uniform Euler–Bernoulli equation [4] is `EI w'''' + m w_tt = 0`.
Substitution of `sin(n π x/L)` satisfies pinned displacement and moment at
both ends, giving `T_n = 2L²/(n² π sqrt(EI/m))`. With L = 100 m,
EI = 1000000 N.m² and m = 100 kg/m, the first two periods are **63.7 s** and
**15.9 s**. The tests use the analytical expressions before rounding.

The finite-element comparisons use 20 and 40 elements: the second mode has
10 and 20 elements per half-wave. Each mesh must meet a **0.1 %** relative
period budget against the analytical answer. A separate refinement check
requires decreasing error for both beam modes. This is a synthetic numerical
test allowance, with two resolutions providing its evidence; it is not a
design acceptance criterion. WKB and arithmetic comparisons allow relative
`1e-12` for floating-point summation only.

The integrated spec has a 10 m inner barrel and three 10 m riser sections,
with a separate 1 m fixed stack. All vibrating sections have mass 1000 kg/m,
EI = 1000000000000 N.m², empty bores and no added mass. Each wet riser
displaces 1 m³/m in water at 1000 kg/m³, so every riser joint carries
**100000.0 N**. A stack with net mass 1000 kg leaves **90193.4 N** at its base.
The ring is massless. Four vertical constant-tension lines, 100 m long,
share the total force: lateral stiffness is `4 × 25000/100` = **1000.0 N/m**.

The full 40 m span is bending dominated. The pinned zero-tension periods
are **32.2 ms** and **8.1 ms**; `reference_periods` is checked within the
same **0.1 %** budget on 40 and 80 elements. Tension and ring-spring
perturbation bounds, and the nearly pinned end springs, are stated in the
test docstring. The riser-only constant-tension WKB reference instead uses
the 30 m riser length and gives **6.0 s** and **3.0 s**. These references
describe different physical limits; they are not compared to each other.

## Tensioner stiffness worked example

The constant-tension model is documented in `spec.py`, class `Tensioners`.
For a line with initial horizontal x separation a and other separation b,
`F_x = T(a-x)/sqrt((a-x)²+b²+dz²)`. Differentiating its force direction gives
restoring stiffness `T/l × (1-a²/l²)`, without an axial wire-stiffness term.

Four lines at cardinal azimuths have radial separation 3 m, vertical separation
4 m and length 5 m. Their total vertical force is 4000 N, so each carries
1250 N. The two lines aligned with x each contribute 160 N/m; the other two
each contribute 250 N/m. The hand answer is **820.0 N/m**. Failed lines,
finite displacement and elastic tensioner response are **NOT_EVALUATED**.

## Post-process example and limits

The companion reader tests build two points with three coincident load rows
each. Maximum tension and maximum moment occur at different times, so the
tests check both the selected driver and its companion loads. A three-station
graph has tied maxima and minima; selection keeps the first occurrence, which
records the current forward scan and Python `max`/`min` behaviour. The stroke
minimum is −2 m and maximum is 5 m, so the excursion is **7.0 m**. The accessor
itself returns minimum, maximum and mean, rather than excursion.

Static and dynamic values, optional hull rows, alternate and missing arc axes,
`None`, schema versions and exact missing-channel paths are covered. Empty
graphs have no extreme. Missing channels raise `MissingChannel`; malformed
numeric `None` values raise `TypeError` when converted to float.

Negative paths also cover invalid section lengths (negative, zero and NaN),
empty spec section lists, insufficient beam elements, zero or negative WKB
end tension, unit-bearing strings in SI fields, and the unsupported
`vertical_force` reference representation. Plain numeric unit mismatches,
comprehensive infinity/NaN rejection in the raw hand-check and reader functions,
and continuous ring immersion are **NOT_EVALUATED**. No production validation
is added or inferred from these tests.

## Sources and test locations

1. [OpenStax, University Physics Volume 1, first edition (2016), §14.4,
   Archimedes' Principle and Buoyancy](https://openstax.org/books/university-physics-volume-1/pages/14-4-archimedes-principle-and-buoyancy).
2. [NIST, standard acceleration of gravity, exact conventional value](https://physics.nist.gov/cgi-bin/cuu/Value?gn).
3. OpenStax, University Physics Volume 1, first edition (2016):
   [§16.3, Wave Speed on a Stretched String](https://openstax.org/books/university-physics-volume-1/pages/16-3-wave-speed-on-a-stretched-string),
   [§16.6, Standing Waves and Resonance, equation 16.16](https://openstax.org/books/university-physics-volume-1/pages/16-6-standing-waves-and-resonance).
4. University of Nebraska–Lincoln, *Vibrations of Beams*, 23 April 1999,
   “Determination of Equations”, equation (3). The institutional teaching note
   and its equation image were opened; pinned supports and the modal formula
   above are derived independently from that PDE, rather than taken from its
   cantilever example.

Tests: `tests/drilling_riser/global_model/test_hand_check_values.py` and
`tests/drilling_riser/postprocess/test_channel_values.py`. No licensed solver
is used by these examples.
