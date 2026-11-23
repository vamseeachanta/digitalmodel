# Stray-current re-model from open literature (provisional)

Issue #2247 | Epic #2206 | owner decisions 2026-09-27

Lane: stray current only (`stray_current.py`, new `_provisional.py`). The galvanic, ICCP-life and fuel-system parts of #2247 are another lane (`corrosion_rate.py`, `iccp_design.py`, `fuel_system_cp.py`, `__init__.py` are not touched here).

## Owner decisions applied

1. The governing standards (EN 50162, ISO 18086, ISO 15589-1, NACE SP0169) are not on file. Models are built from openly published literature.
2. Every constant is a `ProvisionalValue(value, units, source, note, provisional=True, pending_standard)` naming its literature source and the clause that must confirm it. No number is presented as read from a standard.
3. Where no open source gives a threshold, it is a required input.
4. Models stay behind `experimental=True` (`ExperimentalModelError` otherwise).

## Defects addressed (review 2026-09-24, blocker B10)

- DC: the remote-earth potential `rho I / (2 pi d)` was reported as the pipe-to-soil shift. Now the pipe is an infinite leaky transmission line in the imposed earth potential; the shift is `V_pipe - V_earth`.
- AC: unexplained `/1000`, negative voltage for d > 658 m, and `V/R x circumference` called a density. The induction model is removed (V_ac is a measured input, >= 0) and density is the coupon spread-resistance formula.
- Drainage bond: circular effectiveness figure removed; Ohm's-law sizing only.
- Units: AC criterion was 30 **mA**/m²; the literature value is 30 **A**/m².

## Models

| Route | Equation | Source |
|---|---|---|
| DC limit | 20 mV (rho < 15), 1.5·rho mV (15–200), 300 mV (>= 200) incl. IR; 20 mV excl. IR | Lynch 2016 (CEOCOR), reproducing EN 50162 Table 1 |
| DC source | `V_e = rho I/(2 pi r)`; `V_p'' = alpha² (V_p - V_e)`; `dE = V_p - V_e`; `alpha = sqrt(r' g')` | Sunde 1968; Peabody 2001 |
| AC density | `i_ac = 8 V_ac / (rho pi d)` | Brenna et al. 2020; Newman 1966 |
| AC criteria | V_ac < 15 V and (i_ac < 30 A/m² or i_dc < 1 A/m² or i_ac/i_dc < 3) | Brenna et al. 2020 (reporting ISO 18086) |
| Bond | `R = V/I - R_circuit`, `P = I² R` | Peabody 2001 |

## Tests

`tests/cathodic_protection/test_stray_current.py`: hand values (limit bands, alpha, Struve closed form at the closest point, coupon density, bond), experimental gate for every public function, validity-range errors, polarity, monotonicity (shift linear in leakage current, falling with distance), and a registry test that every default is a `ProvisionalValue` with a non-empty source.

## Follow-up when standards arrive

Work through `docs/domains/cathodic_protection/standards-inventory.md` §8 (standards verification checklist); lift the gate only when every row for a standard is confirmed.
