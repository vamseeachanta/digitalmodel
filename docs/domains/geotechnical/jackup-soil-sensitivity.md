# Soil sensitivity analysis for a generic jack-up

This analysis samples the inherited layered penetration envelope for an assumed
three-leg independent-leg jack-up. At the assumed 30.0 MN preload per leg,
conditional punch-through estimates range from 87.6% to 95.5% among the layered
cases. The uniform-clay control has no descending branch, although 53.3% of its
samples lack preload support within the evaluated depth. The way forward is to
replace assumed soil distributions with investigation data, qualify the chosen
mechanisms and assess installation dynamics before engineering use.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| 0 | 2026-10-28 | Added seeded soil sampling, probability intervals and reusable bound cases to quantify the effect of assumed inputs. |

## Scope and input basis

The [named JSON definitions](../../../examples/data/jackup_soil_sensitivity/)
retain the three generic profiles from the
[layered penetration analysis](jackup-layered-penetration.md). Surface sand is
also evaluated with the inherited punching-shear alternative. These profiles
illustrate possible offshore layered stratigraphies; no publicly verified
regional investigation is in hand. Every soil value, distribution, range, geometry
and preload is **assumed, based on previous experience**. The cases do not
reconstruct an event or describe a specific installation.

Each case declares the complete nominal input, uncertain parameter bounds,
assumption basis, other input ranges, equation sources and limits. The following
inputs are independently uniform within their inclusive bounds. Uniform laws and
independence are assumptions for exploring sensitivity, rather than fitted
population statistics. Nominal values preserve the inherited scenario and need
not equal distribution means.

| Profile | Sampled input | Nominal | Low | High |
| --- | --- | --- | --- | --- |
| Stiff-over-soft clay | Upper clay strength (kPa) | 80.0 | 60.0 | 120.0 |
| Stiff-over-soft clay | Middle clay strength (kPa) | 15.0 | 10.0 | 30.0 |
| Stiff-over-soft clay | Deeper clay strength (kPa) | 100.0 | 60.0 | 120.0 |
| Stiff-over-soft clay | Top-layer thickness (m) | 3.0 | 2.0 | 4.0 |
| Sand over soft clay, both methods | Clay strength (kPa) | 20.0 | 10.0 | 30.0 |
| Sand over soft clay, both methods | Sand friction angle (degrees) | 32.0 | 28.0 | 36.0 |
| Sand over soft clay, both methods | Sand thickness (m) | 4.0 | 3.0 | 5.0 |
| Uniform soft clay | Clay strength (kPa) | 15.0 | 10.0 | 30.0 |

Changing top thickness moves the bottom of the first layer and the top of the
second layer together. Deeper boundaries remain fixed. This preserves contiguous
layers without rejection sampling. The uniform-clay control has no top-layer
interface; its investigation extent stays fixed and thickness sensitivity is
inapplicable. All clay strengths are sampled separately, including the deepest
clay. Sand friction is sampled for both sand methods even where it does not
control the load-spread envelope.

Fixed nominal inputs include a 12.0 m equivalent diameter, a 1.5 m tip-to-bearing
plane offset, a 30.0 MN vertical preload per leg, and a tip interval of 1.5 to
24.0 m below mudline. Fixed submerged unit weights and deeper interfaces, with
their assumption ranges, remain in each JSON definition. The spread denominator
is fixed at 3.0 and the punching-shear coefficient at an assumed 1.5; their
uncertainty is excluded. Each leg is evaluated independently. No joint
three-leg probability or redistribution model is inferred.

## Equations, probability and margin

Every realization calls `penetration_curve()` with the full tip interval.
The inherited API inserts layer boundaries, mechanism switches and stationary
points, retaining descending branches even with only interval endpoints supplied.
No change is made to its bearing factors, equations, depth datum or preload flag.

The method basis is the open
[Spudcan Penetration Analyses for Three Case Histories in Sand Overlying Clay (2015), equations 1–3](https://jack-up.citystgeorges.ac.uk/wp-content/uploads/2025/09/Jackup-2015-11.pdf),
and
[Theoretical Study on the Effect of a Weak Layer on the Vertical Bearing Capacity of Spudcans in Clay (2015), equations 2 and 6](https://jack-up.citystgeorges.ac.uk/wp-content/uploads/2025/09/Jackup-2015-19.pdf).
Both public copies were retrieved with HTTP 200 on 2026-10-28. Their equations
supply mechanisms, not the assumed regional soil values. The inherited circular
clay reference uses 6.34 rather than the weak-layer paper's 6.05 surface factor;
this declared research combination remains unchanged.

The inherited references are SNAME T&RB 5-5A, Rev 3 (2008), clause **TO CONFIRM**,
and ISO 19905-1:2012, clause **TO CONFIRM**. The weak-layer paper's references
identify these editions but do not confirm their clause identifiers. No standard
copy was obtained, no standard values are reproduced, and no conformity is
claimed. Clause verification remains open before engineering use.

For each descending event, its access threshold is the largest reaction on the
curve up to that event's peak. The punch-through threshold is the smallest such
access threshold. This retains shielding by a higher earlier capacity. The
margin in kN is:

```text
margin = punch-through threshold - planned preload per leg
```

Positive margin means preload is below the first accessible descending peak;
zero reaches it. If no descending branch exists, the threshold and margin are
null. They are never replaced by the interval maximum or a zero margin. The
inherited flag records whether preload reaches a descending peak. Finite-depth
preload support is a separate output; a false punch-through flag does not
establish support or installation acceptance.

The probability estimator is the count of true flags divided by N. The study
uses **N = 10,000 per case**, **seed = 20261028**, and Python's local
`random.Random` generator. Inputs consume one `random()` draw each, in lexical
parameter order. A separate generator starts from the stated seed for each
case; the two sand methods use common soil draws. Changing input-file order
therefore does not change a case's result. Between-profile errors are correlated,
so these intervals do not establish uncertainty in differences between methods.

Two-sided 95% Wilson score intervals follow the
[NIST binomial proportion interval formulation](https://www.itl.nist.gov/div898/handbook/prc/section2/prc241.htm),
retrieved with HTTP 200 on 2026-10-28. They invert
`(k - N p)^2 <= z^2 N p (1 - p)` at the two-sided normal quantile. They quantify
Monte Carlo sampling uncertainty conditional on the specified distributions and
mechanisms, excluding uncertainty in the distributions or the model itself.

## Probability results

The compact [reference outputs](../../../examples/data/jackup_soil_sensitivity/reference/)
were generated on 2026-10-28. The manifest records the executing source and input
digests, precise output digests, sample CSV digests, N and seed. Human-readable
values below use one decimal place; machine-readable CSV and JSON retain
calculation precision. The control's nonzero upper confidence bound is shown as
less than 0.1% to avoid displaying a zero-width interval.

| Profile and method | Punch-through samples / N | Probability | 95% interval | Samples without preload support |
| --- | --- | --- | --- | --- |
| Stiff-over-soft clay | 9550 / 10000 | 95.5% | 95.1–95.9% | 0.0% |
| Sand over soft clay, load spread | 9047 / 10000 | 90.5% | 89.9–91.0% | 41.2% |
| Sand over soft clay, punching shear | 8763 / 10000 | 87.6% | 87.0–88.3% | 41.2% |
| Uniform soft clay | 0 / 10000 | 0.0% | 0.0% to less than 0.1% | 53.3% |

The sand punching-shear model produces no descending branch for 12.4% of draws.
These remain in N as false punch-through flags. None are discarded or resampled.
A high conditional probability here reflects the chosen preload, soil ranges
and static model; it is not a frequency estimate for a regional population.

## One-at-a-time tornado

For each input, low and high bounds are applied separately while every other
input remains at its nominal value. The tornado plots **change in margin from
nominal**, rather than change in probability. It ranks the largest absolute
bound change first and retains a null bar where a bound eliminates the descending
branch. Its annotations list inputs with at least one undefined bound margin.
Named cases retain both the realized input and result at every bound.

| Profile | Input | Low-bound margin change (MN) | High-bound margin change (MN) |
| --- | --- | --- | --- |
| Stiff-over-soft clay | Upper clay strength | -1.7 | +3.4 |
| Stiff-over-soft clay | Middle clay strength | -3.6 | +10.8 |
| Stiff-over-soft clay | Deeper clay strength | 0.0 | 0.0 |
| Stiff-over-soft clay | Top-layer thickness | -2.3 | +2.9 |
| Sand, load spread | Clay strength | -9.7 | +10.7 |
| Sand, load spread | Friction angle | 0.0 | 0.0 |
| Sand, load spread | Top-layer thickness | -1.9 | +2.0 |
| Sand, punching shear | Clay strength | -7.2 | +7.2 |
| Sand, punching shear | Friction angle | -0.1 | +0.2 |
| Sand, punching shear | Top-layer thickness | Undefined: no loss | +1.6 |
| Uniform soft clay | Clay strength | Undefined: no loss | Undefined: no loss |

Nominal margins are -12.5 MN for stiff-over-soft clay, -8.6 MN for sand load
spread and -11.0 MN for sand punching shear. The middle/underlying clay strength
has the largest nominal bound effect in each layered case. Zero effects in this
tornado are specific to the nominal mechanism: the deeper strong clay does not
control the first clay loss, and the sand homogeneous cap does not control the
nominal load-spread peak. They do not establish that these inputs are generally
irrelevant or that interactions are absent.

## Reproduction and reusable cases

```bash
PYTHONPATH=src uv run --no-project --with pytest python \
  examples/domains/geotechnical/jackup_soil_sensitivity.py OUTPUT \
  --samples 10000 --seed 20261028
```

Use a fresh output directory outside the source tree. The CLI rejects fewer
than 10,000 samples. The callable exporter accepts smaller runs for integration
tests. The example writes the following files together:

- `probabilities.csv`: counts, probabilities, Wilson bounds, unsupported counts,
  N, seed and preload for each case.
- `tornado.csv`: nominal and low/high values, margins and changes for every input.
- `probability.figure.json` and `tornado.figure.json`: offline Plotly figure
  specifications with assumption metadata; no browser, network or Plotly package
  is required to generate them.
- Per-profile `.samples.csv`: sampled inputs and individual outcomes.
- Per-profile `.cases.json`: a `nominal` case and named `<parameter>.low` and
  `<parameter>.high` cases, each with complete profile, geometry, tip interval,
  preload, parameter values and result. Subsequent calculations can consume
  `cases[name].input` without reconstructing moving interfaces.
- Per-profile `.summary.json`: completion record binding inputs, executing
  sources, N and seed to the digests of generated artifacts.

Completed cases are reused only when their input/source fingerprint and all
recorded artifact digests match. A mismatch raises an error. An interrupted case
without its final summary can be recomputed; other completed cases are retained.
There are no timestamps or absolute paths in generated case outputs. Reproduction
requires the same source/input bytes and a compatible Python random/math runtime;
reference results used Python 3.12.15. The compact reference omits the per-sample
CSVs and completion records; generate a fresh study to obtain them. Figure specs
can be embedded by a report using a locally bundled Plotly renderer.

## Evidence and limits

Before implementation, the inherited spudcan suites passed **94 tests**. The
integrated suites pass **117 tests**, including 23 new tests. Independent hand
calculations in the new test file cover clay punching margins, each bound effect,
sand-friction peak resistance, binomial score intervals and an analytically known
uniform-input probability. The expected seeded count is derived separately by
applying the hand inequality to random draws. Tests also exercise undefined
margins, unsupported preload, coupled interfaces, invalid distributions, stable
exports, completion reuse, digest tampering and the CLI sample minimum.

This study is a static, vertical, full circular-area sensitivity screen. It
inherits all layered-envelope limits: no partial engagement, remoulding, plug
transport, consolidation, cyclic changes, scour, combined loading, leg interaction,
buoyancy correction or installation dynamics. The soil assumptions are not
regional observations. Uniform independent laws do not represent spatial
correlation, soil-parameter correlation or epistemic uncertainty. Tornadoes do
not quantify interactions. Investigated soil distributions, qualified geometry,
load conventions, method coefficients and standards clauses, plus independent
engineering review and dynamic assessment, remain required before operational use.
