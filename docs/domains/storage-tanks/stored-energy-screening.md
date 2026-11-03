# Stored-energy screening

`digitalmodel.code_checks.stored_energy.stored_energy` estimates expansion
energy in joules and a TNT energy equivalent in kilograms. It records the
equation and inputs and compares energy with an optional review threshold.
This is an equipment screening calculation; it does not establish design
compliance or predict blast effects.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| 0 | 2026-10-27 | Added gas and liquid calculations with explicit units and an optional review criterion. |
| 1 | 2026-10-28 | Added the bounded calculation, validation and documentation plan to make the screening scope explicit. |

## Plan-lite

- API scope: one `stored_energy` helper and immutable input and result records
  in `digitalmodel.code_checks.stored_energy`, with pressure-unit conversion,
  input validation, energy, TNT equivalent and an optional strict threshold flag.
- Model assumptions: gas uses total reversible isentropic ideal-gas boundary
  work to ambient pressure with constant specific-heat ratio; liquid uses
  small compression with constant bulk modulus. Fluid properties and any
  review threshold require qualification for the equipment being assessed.
- Independent validation: hand-calculated gas cases at gamma 2 and 1.4 give
  2000.0 J and 24000.0 J; the water case gives 1000.0 J. Tests also cover unit
  conversion, monotonic behaviour, threshold boundaries, immutable provenance,
  invalid inputs and numerical stability, alongside existing code-check regressions.
- Documentation scope: equations and public basis, units, defaults, a usage
  example, reproducible input records and limits are documented on this page.
- Exclusions: no dependency or solver integration, design-compliance decision,
  universal acceptance threshold, container strain, trapped gas, phase change,
  chemical energy, real-gas model or prediction of blast effects is included.

## Equations and public basis

For gas, the total reversible isentropic expansion work is

```text
P1 = p_gauge + P0
V2 = V * (P1/P0)**(1/gamma)
E = (P1*V - P0*V2)/(gamma - 1)
  = P1*V/(gamma - 1) * [1 - (P0/P1)**((gamma - 1)/gamma)]
```

Pressures P1 and P0 are absolute Pa; V is the initial contained volume in m3.
The formula follows the ideal-gas relation `P*V**gamma = constant` and the
integral of `P dV` in [OpenStax, University Physics Volume 2, section 3.6,
equation 3.12 and example 3.7](https://openstax.org/books/university-physics-volume-2/pages/3-6-adiabatic-processes-for-an-ideal-gas).
This is total boundary work down to ambient pressure, including work
displacing the atmosphere. The atmosphere-displacement term `P0*(V2-V)`
has not been subtracted. The method assumes a constant ratio of specific
heats and reversible adiabatic expansion of an ideal gas. The default
`gamma=1.4` is an air screening assumption; supply the applicable fluid value.

For liquid with constant bulk modulus K in Pa:

```text
compression volume = p_gauge * V / K
E = integral from 0 to p_gauge of (p * V/K) dp
  = p_gauge**2 * V / (2*K)
```

This is the linear, small-compression approximation obtained by integrating
the bulk-strain relation in [OpenStax, University Physics Volume 1, section
12.3, equation 12.39](https://openstax.org/books/university-physics-volume-1/pages/12-3-stress-strain-and-elastic-modulus).
Table 12.1 gives the approximate water bulk modulus used as the default,
`K=2.2e9 Pa`. Confirm K for the fluid, temperature and pressure range.
V is the contained liquid volume, approximated as constant in this model.
Use the liquid result only where `p_gauge/K` is small and entrained gas is
negligible; the helper does not impose a numerical applicability cutoff.

TNT equivalent is `E / tnt_energy_j_per_kg`. The configurable default is
`4.184e6 J/kg`, obtained from the conventional metric-ton TNT energy
equivalent of `4.184e9 J` in [NIST Guide to the SI, appendix B.8](https://www.nist.gov/pml/special-publication-811/nist-guide-si-appendix-b-conversion-factors/nist-guide-si-appendix-b8).
These public textbook and conversion references were checked on 2026-10-27.
No standard document was opened or transcribed.

## Inputs and review flag

- `volume_m3` is positive, and `pressure_gauge` is nonnegative.
- `pressure_unit` is exactly `Pa` (default), `bar`, `MPa` or `psi`.
  Bar and MPa convert by 100000 and 1000000; psi means pound-force per
  square inch and converts by 6894.757293168 Pa.
- `fluid_type` is exactly `gas` or `liquid`.
- `ambient_pressure_pa` is absolute Pa, independent of `pressure_unit`;
  its default is 101325 Pa. It affects the gas calculation only.
- `specific_heat_ratio` must exceed 1, and `bulk_modulus_pa` and
  `tnt_energy_j_per_kg` must be positive. Both fluid properties are validated
  and recorded, including the one inactive for the selected fluid.
- `threshold_j` is an explicit nonnegative energy criterion in J. It defaults
  to `None` because no universal public review threshold is established by
  these sources. `exceeds_threshold` is then `None`, meaning unevaluated.
  With a criterion, the flag is True only when `energy_j > threshold_j`;
  equality is False. A False flag is not a safety determination.

All numeric inputs must be finite numbers; booleans and numeric strings are
rejected. Invalid inputs and nonfinite calculation results raise `ValueError`.

## Example and checks

```python
from digitalmodel.code_checks.stored_energy import stored_energy

result = stored_energy(
    volume_m3=0.001,
    pressure_gauge=300,
    pressure_unit="bar",
    fluid_type="gas",
    threshold_j=10000,  # Illustrative caller criterion, not a public limit.
)
assert result.exceeds_threshold is True
```

`StoredEnergyResult` and its `StoredEnergyInputs` are immutable dataclasses.
The result records the original pressure, its unit, converted gauge pressure
in Pa, all other inputs and the formula string. `dataclasses.asdict(result)`
provides a serializable record.

Tests in `tests/code_checks/test_stored_energy.py` include independent cases:
gas at P1/P0=4 and gamma=2 gives 2000.0 J for V=0.01 m3; gas at
P1/P0=128 and gamma=1.4 gives 24000.0 J for V=0.001 m3; water at
10.0 MPa gauge and V=0.044 m3 gives 1000.0 J. Tests also cover all supported
units, zero pressure, monotonic pressure and volume, explicit threshold
boundaries, input provenance, numerical stability and invalid inputs.

## Known limits and way forward

The helper excludes container strain, chemical energy, phase change and
two-phase or real-gas effects. A liquid containing gas requires a separate
assessment. A TNT energy equivalent does not specify blast efficiency,
fragmentation, hazard distance or injury risk. Before using the result for
equipment review, confirm fluid properties and model applicability and supply
the review threshold from the applicable engineering assessment.
