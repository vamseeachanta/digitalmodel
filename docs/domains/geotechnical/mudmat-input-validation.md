# Mudmat calculator input validation

The mudmat calculator rejects NaN, positive infinity, negative infinity, and
Boolean values for every supplied numeric argument. A `ValueError` names the
argument before bearing or sliding arithmetic starts. This applies to soil
arguments even when the selected drainage condition does not use them.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 1 | 2026-10-20 | Document the existing finite-input contract and add an executable geometry example to explain rejection before calculation. |
| 2 | 2026-10-20 | Validate horizontal load and safety factor before summary output, collect the examples in normal tests, and identify the example values as synthetic. |

## Input contract

Both `bearing_capacity_factors(phi_deg)` and `mudmat_bearing_capacity(...)`
validate finiteness. The mudmat numeric arguments are:

- `width_b_m`, `length_l_m`, `embedment_depth_m`;
- `submerged_unit_weight_kn_m3`, `undrained_shear_strength_kpa`,
  `friction_angle_deg`, `effective_cohesion_kpa`,
  `interface_friction_angle_deg`;
- `vertical_load_kn`, `moment_knm`.

The three optional arguments `undrained_shear_strength_kpa`, `friction_angle_deg`,
and `interface_friction_angle_deg` may be `None` where the selected condition
permits it. Undrained calculation requires shear strength; drained calculation
requires friction angle. A missing interface angle defaults to the soil friction
angle in the drained sliding calculation.

Python's [3.11 `math.isfinite` reference](https://docs.python.org/3.11/library/math.html#math.isfinite)
defines NaN and both infinities as non-finite, and zero as finite. Boolean rejection
is an additional calculator contract. The existing physical range checks still
apply after this validation. No additional engineering coefficient, friction-angle
limit, or unit-weight sign limit is introduced.

## Worked example

The synthetic inputs below reproduce the geometry in issue #2105 and the soil
inputs in issue #2330. These are synthetic values for this example; not site data
or calibrated parameters. The method is the existing effective-area construction in
`src/digitalmodel/geotechnical/mudmat.py`: equilibrium gives eccentricity
`e = abs(M)/V`; removing two eccentricity strips from the selected dimension
gives `B_eff = B - 2e` or `L_eff = L - 2e`. These dimension reductions and the
area product are given in [FHWA NHI-06-089, *Soils and Foundations, Volume II*
(December 2006)](https://www.fhwa.dot.gov/engineering/geotech/pubs/nhi06089.pdf),
Section 8.4.3.1, page 8-25, Equations 8-7 through 8-9; the same passage defines
eccentricity as moment divided by vertical load. The coefficient two is geometric,
not an empirical soil coefficient. This citation supports the effective-area
construction only. No bearing-capacity equation is changed or independently
qualified by this example.

For width 3.0 m, length 4.0 m, vertical load 800.0 kN and moment 400.0 kNm,
the hand calculation is `e = 400.0/800.0 = 0.5 m`. Width eccentricity gives
`B_eff = 3.0 - 2*0.5 = 2.0 m` and `A_eff = 2.0*4.0 = 8.0 m2`.
Length eccentricity gives `L_eff = 4.0 - 2*0.5 = 3.0 m` and
`A_eff = 3.0*3.0 = 9.0 m2`. These are geometry results, not design acceptance
limits. The following examples are exercised as doctests.

```pycon
>>> from digitalmodel.geotechnical.mudmat import mudmat_bearing_capacity
>>> inputs = dict(width_b_m=3.0, length_l_m=4.0, embedment_depth_m=0.0,
...               condition="undrained", submerged_unit_weight_kn_m3=8.0,
...               vertical_load_kn=800.0, moment_knm=400.0,
...               undrained_shear_strength_kpa=25.0)
>>> result = mudmat_bearing_capacity(**inputs)
>>> (result.effective_width_m, result.effective_length_m, result.effective_area_m2)
(2.0, 4.0, 8.0)
>>> result = mudmat_bearing_capacity(**inputs, eccentricity_axis="L")
>>> (result.effective_width_m, result.effective_length_m, result.effective_area_m2)
(3.0, 3.0, 9.0)

```

A NaN moment has no usable eccentricity. Replace the moment with NaN to obtain
the named input error instead of an area or capacity result:

```pycon
>>> mudmat_bearing_capacity(**(inputs | {"moment_knm": float("nan")}))
Traceback (most recent call last):
    ...
ValueError: moment_knm must be a finite number

```

Configuration-based calls through `MudmatBearingCapacityWorkflow` also reject
numeric strings representing NaN or infinities for the calculator arguments
listed above before writing a summary. Their error names use the calculator's
argument names. The workflow additionally requires a finite, nonnegative
`loads.horizontal_kN` and a finite, positive `design.factor_of_safety`, including
when supplied as numeric strings. Errors name `horizontal_kN` or
`factor_of_safety` and leave the configuration unchanged without writing a
summary. Omitted values retain the defaults of zero horizontal load and a safety
factor of 2.0. Invalid values must be corrected from an input source; they are
not replaced with zero or interpreted as missing values.

## Scope and remaining audit

The runtime guard and its original tests are already present from #2404.
The expanded tests cover every numeric argument under both drainage conditions
and eccentricity axes, both Boolean values, optional `None` defaults, and
configuration strings, including horizontal load and safety factor. Normal
pytest collection runs this document through `doctest.testfile`. The finite
eccentricity regressions are retained.

Other geotechnical calculators require a separate audit. Reading the current
source gives the following inventory; it does not establish exhaustive input
validation or change any of these modules.

| Module | Observation | Follow-up |
| --- | --- | --- |
| `pile_capacity.py` | Capacity wrappers perform geometry arithmetic and call stress and strength helpers without a shared finite-input guard. | Audit public scalar entry points. |
| `piles.py` | Range warnings use chained comparisons; axial-capacity dimensions use positivity comparisons. | Audit warnings, scalars and layer fields. |
| `anchors.py` | Weight, dimensions and strength use positivity comparisons. | Audit every numeric argument. |
| `scour.py` | Sediment-motion helpers check finiteness; pipeline, monopile and rock inputs include comparison-only guards. | Audit the remaining entry points. |
| `liquefaction.py` | Acceleration, magnitude, stress and required safety factor include positivity comparisons. | Audit scalar and layer inputs. |

Finite input alone does not guarantee finite output: extreme finite values may
overflow. Wider physical validity limits, overflow handling, and validation in
other calculators remain outside this change.
Atlas data and calculator source bytes are retained.
