# Potential against distance: screening derivation

## Summary

The point-source and uniform finite-line-source models calculate electrolyte
potential rise around one prescribed outward current source. They are screening
models, not structure potential predictions. Use the finite-line curve as the
primary distance plot because it retains anode length; show the point-source
curve alongside it to identify the far-field approximation. Neither curve
establishes cathodic protection, anode spacing or throwing power for a jacket.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| A | 2026-10-26 | Derive both fields and provide reproducible screening data; structure current distribution is outside this calculation. |

## First-principles basis

The primary basis is the following derivation, rather than an unseen textbook
page. Assume steady current in an isotropic, homogeneous, infinite electrolyte
of resistivity `rho` in ohm.m. There is no structure, polarisation, other anode,
interface or finite-distance return electrode. The reference is remote electrolyte:
`V(infinity) = 0`. Outward conventional source current `I` is positive.

### Point source: equation P

Current conservation on a sphere of radius `r` gives
`I = J(r) * 4*pi*r^2`, where `J` is radial current density in A/m².
Ohm's law in the electrolyte gives `E = rho*J`, with `E = -dV/dr`.
Consequently:

```text
J(r) = I / (4*pi*r^2)
V(r) = integral from r to infinity of rho*I/(4*pi*s^2) ds
     = rho*I / (4*pi*r)                           [V]       (P)
potential_shift_mv = 1000 * V(r)                  [mV]
```

The sphere area supplies the geometric factor; it is not an empirical constant.
Dimensional check: `(ohm.m)*A/m = V`; the SI milli prefix supplies the factor
`1000`. Distance is from the point centre and must be strictly positive. The
point-centre singularity is rejected, including for zero current.

### Uniform finite line: equation L

Put the source on the axis `z`, from `-L/2` to `L/2`, with uniform current per
unit length `I/L`. Evaluate in the perpendicular plane through its midpoint, at
axis distance `r > 0`. An element `dz` supplies `dI = (I/L)*dz`; its distance
to the observation point is `sqrt(r^2+z^2)`. Superposition of equation P gives:

```text
V(r) = rho*I/(4*pi*L) * integral[-L/2,L/2] dz/sqrt(r^2+z^2)
     = rho*I/(2*pi*L) * asinh(L/(2*r))            [V]       (L)
     = rho*I/(2*pi*L) * ln((L/2+sqrt((L/2)^2+r^2))/r)
potential_shift_mv = 1000 * V(r)                  [mV]
```

This is a uniform line-current approximation, not the exact field of an
equipotential finite cylinder. A real anode has nonuniform surface discharge,
including end effects. The cross-section radius only converts a surface gap
to an axis distance; it does not impose an equipotential surface boundary.
Axial offsets and end-on observations are not represented by this function.

For `r` large relative to `L`, `asinh(L/(2*r))` approaches `L/(2*r)`, so L
reduces to P. Both fields tend to zero as `r` tends to infinity. Doubling
`rho` or the prescribed `I` doubles either field. The functions also accept
positive infinite distance and return zero. Resistivity and length must be
positive and finite; current must be non-negative and finite.

### Sign and interpretation

The returned positive value is **electrolyte potential rise relative to remote
electrolyte**. It is not a steel potential versus Ag/AgCl and is not a predicted
polarisation shift. If a metal potential were externally held fixed, subtracting
the local electrolyte potential would contribute a negative metal-to-electrolyte
change of the same magnitude; this model does not solve that coupled problem.
There is no compliance threshold or passing distance in these data.

## Chart data and declared assumed basis

The generator reads the existing published-jacket fixture and calls the existing
B401 resistance and output-current functions. The assessed source revision is
recorded in [basis.json](data/potential-distance/basis.json). No published-jacket
report builder or section-based replacement is present in that revision. The
existing mass-equivalent-cylinder route is used, as follows:

| Input | Value and unit | Basis and locator |
| --- | --- | --- |
| Base seawater resistivity | 0.22 ohm.m | Xu (2023), p. 242, section 2: 22 ohm.cm; multiply by 0.01 m/cm. |
| Anode length | 2.020 m | Xu (2023), p. 242, section 2: 2020 mm; divide by 1000 mm/m. |
| Anode mass | 323.4 kg | Xu (2023), p. 242, section 2; interpreted as net alloy, assumed, based on previous experience. |
| Driving voltage | 0.25 V | Xu (2023), p. 242, section 2; also the existing B401 driving-voltage lookup. |
| Alloy density | 2750 kg/m³ | Existing fixture assumptions and `_kernels.ANODE_DENSITY_ALZNI`; assumed, based on previous experience, no public source established. |
| Standard edition | 2021 | Existing fixture; assumed, based on previous experience, not established by the paper. |
| Equivalent radius | `sqrt(mass/(pi*length*density))` m | Existing `_kernels.equivalent_radius_from_mass`, conservation of cylinder volume on the assumed net-alloy basis. |
| Initial individual output | 4.7 A (display rounding) | Existing `anode_sizing.calculate_anode_resistance` and `_kernels.anode_current_output`, using base resistivity and the fresh mass-equivalent cylinder. Unrounded value is in each CSV and in `basis.json`. |
| Resistivity curves | 0.20, 0.22, 0.25, 0.30 ohm.m | Only 0.22 has the published basis above. The others are illustrative requested samples, assumed, based on previous experience, until a cited sensitivity range replaces them; no environmental bound is claimed. |
| Surface-gap samples | 0 to 30 m inclusive, every 1 m | Illustrative extent and plotting resolution, assumed, based on previous experience. Member spacing is not established by the paper. The paper's 0–30 m depth band (p. 242, Table 1) is not a member-spacing measurement. |

The reproduced current is the **code's** fresh-anode output on that declared
assumed geometry basis, not the paper's printed individual output. It is held
fixed across resistivities to isolate `rho` in the potential field. A coupled
sacrificial-anode sweep would recompute `I = driving_voltage/R(rho)`; for fixed
geometry and driving voltage, `R` is proportional to `rho`, and `rho*I` would
be constant. These curves must not be described as that coupled sensitivity.
Design life, capacity and utilisation do not enter the fresh-anode field.

The horizontal coordinate in both CSVs is surface gap `d`, with
`r = equivalent_radius + d`. Thus the row `d=0` is outside the mathematical
source and has a finite value; it is an evaluation convention, not a computed
anode boundary potential. The core functions accept `r`, never `d`. The point
approximation close to the anode can be poor and has no near-surface accuracy
claim. The two charts share the same radial coordinate and prescribed current.

| Deliverable | Contents |
| --- | --- |
| [finite-line-source.csv](data/potential-distance/finite-line-source.csv) | Primary screening plot data, one curve per resistivity. |
| [point-source.csv](data/potential-distance/point-source.csv) | Point approximation for comparison. |
| [values.csv](data/potential-distance/values.csv) | One manifest row per chart value, with quantity, value, unit, basis, source and phrases. |
| [basis.json](data/potential-distance/basis.json) | Inputs, provenance, coordinate convention, assumptions and pending review status. |

Machine-readable data retain floating-point precision for reproduction. A report
should format displayed results to one decimal place, retain the screening label
and assumptions, and import these values only after technical review. This work
does not build or edit a report page or assert that review has passed.

## Independent checks and reproduction

`tests/cathodic_protection/test_potential_distance.py` gives three independently
hand-worked examples per form, with their arithmetic in the test docstrings.
The synthetic inputs cancel `pi` or form the triangle with half-length, radial
distance and hypotenuse in the ratio `3:4:5`. They are algebraic test cases,
not sourced environmental values. Tests check units, linear scaling, far-field
and short-line limits, invalid domains, the existing B401 current basis, and
every chart and manifest row. The generated bundle is compared byte for byte
with the committed bundle.

From the repository root, use an environment with the package's import
dependencies, including pydantic, numpy, scipy, PyYAML and loguru:

```sh
PYTHONPATH=src python scripts/cathodic_protection/generate_potential_distance.py \
  --output-dir docs/domains/cathodic_protection/data/potential-distance
PYTHONPATH=src python -m pytest tests/cathodic_protection/test_potential_distance.py \
  -q -rA -p no:cacheprovider
```

## References and remaining source confirmation

- Primary formulation: the first-principles derivation above, equations P and L.
  No empirical field coefficient or unseen source page is used.
- Xu, D. (2023), *Study on Implementation and Optimization of Cathodic Protection
  for Offshore Oil Platform Jacket*, *Engineering Advances*, 3(3), pp. 241–245,
  [DOI 10.26855/ea.2023.06.017](https://doi.org/10.26855/ea.2023.06.017).
  Inputs: p. 242, section 2; depth bands: p. 242, Table 1.
  The [publisher's open paper](https://www.hillpublisher.com/UpFile/202308/20230803150221.pdf)
  was read on 2026-10-26 (HTTP 200).
- See also: Telford, W. M., Geldart, L. P. and Sheriff, R. E. (1990),
  *Applied Geophysics*, second edition, Cambridge University Press, chapter 8,
  *Resistivity Methods*,
  [DOI 10.1017/CBO9781139167932](https://doi.org/10.1017/CBO9781139167932).
  The publisher's catalogue confirms bibliographic metadata and the chapter
  title (read 2026-10-26, HTTP 200). The formulation page was not read:
  **page to be confirmed by the maintainer**. This is a see-also reference;
  no cathodic-protection textbook application to spacing is claimed verified.
- Existing implementation references: `engine_adapter._b401_anode_geometry`,
  `anode_sizing.calculate_anode_resistance`, `_kernels.long_slender_standoff`
  and `_kernels.anode_current_output`. The existing code labels the resistance
  route DNV-RP-B401 (2021), *Cathodic protection design*, *Anode resistance*,
  and the voltage lookup *Design driving voltage*. No standard document was
  opened, read or quoted; no new standard table data are introduced.

Before applying these curves to a structure, establish member geometry,
electrical continuity, boundary and return-current effects, actual current
distribution and polarisation. Confirm the textbook page and replace the
illustrative resistivity samples with a cited range. A finite-line field alone
cannot establish an installed design's local protection.
