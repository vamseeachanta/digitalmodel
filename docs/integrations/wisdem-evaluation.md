# WISDEM plant finance evaluation

## Summary

WISDEM 4.2.9 exposes a bounded finance interface for the existing floating-wind economics model. The pinned backend installed, but its import failed under the repository warning policy. Backend LCOE comparisons are **Not Evaluated**; executable integration remains deferred under #1690. No adapter, dependency extra or lock change is included.

## Revision history

| Revision | Date | Change and reason |
|---|---|---|
| 1 | 2026-11-16 | Record the pinned API and failed backend gate; defer the adapter until real-package comparisons pass. |

## Overview and source

WISDEM combines wind-system component design and plant economics through OpenMDAO. This evaluation covers only `PlantFinance`, rather than rotor, drivetrain or tower sizing. Source pin: release `v4.2.9`, commit `54353f00fab3861919a58ca57e161f16acc247ac`.

The [component source](https://github.com/NLRWindSystems/WISDEM/blob/v4.2.9/wisdem/plant_financese/plant_finance.py#L74-L87), [package metadata](https://github.com/NLRWindSystems/WISDEM/blob/v4.2.9/pyproject.toml) and [Apache-2.0 licence](https://github.com/NLRWindSystems/WISDEM/blob/v4.2.9/LICENSE) were retrieved on 2026-11-16, each with HTTP 200. Python >=3.11 is required. WISDEM declares unconstrained NumPy and OpenMDAO >=3.45.1; the selected OpenMDAO requires NumPy >=2.0, conflicting with this repository's NumPy <2.0 runtime constraint.

## Installation and gate

An isolated Python 3.12.3 environment was created outside the repository. Installation with binary distributions succeeded using WISDEM 4.2.9 and OpenMDAO 3.45.2. No extra is added because the runtime NumPy decision remains unresolved.

The attempted gate used the following form, with `WISDEM_ENV` and `CHECK_TMP` pointing to disposable directories and the repository source on `PYTHONPATH`:

```bash
uv venv --python 3.12 "$WISDEM_ENV"
uv pip install --python "$WISDEM_ENV/bin/python" --only-binary=:all: \
  'wisdem==4.2.9' 'openmdao==3.45.2' pydantic pyyaml plotly \
  pytest pytest-asyncio jinja2
PYTHONPATH=src "$WISDEM_ENV/bin/python" -m pytest \
  tests/floating_wind/test_wisdem_finance.py -m 'not solver' \
  -q -rA -p no:cacheprovider --basetemp="$CHECK_TMP"
```

The provisional test file and adapter were excluded after the failure, so the final tree does not provide this test command. It records the attempted gate for later implementation.

Resolved versions: Python 3.12.3, WISDEM 4.2.9, OpenMDAO 3.45.2, NumPy 2.5.5, SciPy 1.18.1 and MoorPy 1.2.1.

Result: **2 failed, 10 passed**, zero skips. Both backend cases failed while importing WISDEM through its rotor code and MoorPy 1.2.1. At `moorpy/Catenary.py:845`, Python raised `SyntaxError: invalid escape sequence '\p'` under `pytest.ini`'s warnings-as-errors policy. No warning filter was relaxed. Both backend LCOE values and their relative differences remain **Not Evaluated**.

## Proposed API mapping

| PlantFinance input | Economics value | OpenMDAO unit supplied |
|---|---|---|
| `machine_rating` | `turbine_rating_mw` | `MW` |
| `turbine_number` | `turbine_count` | none, discrete |
| `tcc_per_kW` | `capex.turbine` | `USD/kW` |
| `offset_tcc_per_kW` | explicit zero | `USD/kW` |
| `bos_per_kW` | `capex.total_per_kw - capex.turbine` | `USD/kW` |
| `opex_per_kW` | `opex_per_kw_year` | `USD/kW/yr` |
| `plant_aep_in` | `annual_energy_production_mwh(econ)` | `MW*h` |
| `fixed_charge_rate` | capital recovery factor | none |

The proposed output is `lcoe` requested in `USD/(MW*h)` through OpenMDAO conversion. Upstream declares rating in kW, energy in kWh and LCOE in USD/kWh; manual scaling is unnecessary when units are supplied.

## Analytical fit and verification

Upstream lines 209-226 calculate `lcoe = C / E`, where `C = icc * fcr + c_opex` and `E` is annual plant energy divided by installed kW. This matches `compute_lcoe` only when escalation and decommissioning costs are zero and FCR is the capital recovery factor `d / (1 - (1 + d)**(-N))`.
At zero discount, the capital recovery factor takes its limiting value of `1/N` as `d` approaches zero.

Before implementation, the provisional tests failed on the absent adapter. Afterwards, 10 tests passed and two real-package cases skipped in the environment without WISDEM. These covered the annuity identity, exact mapping, rejected escalation/decommissioning, component calls and units, lazy imports and transitive import errors. They establish the proposed wiring only; the failed real-package gate prevents adoption.

The proposed backend comparisons use the existing base inputs with escalation and decommissioning removed, then a second case with discount 0.08, life 20 years and capacity factor 0.45. They require agreement with `compute_lcoe` at relative tolerance `1e-9`. The original base-case YAML and its existing LCOE test remain unchanged.

The named economics, dependency-contract and repository-structure acceptance set passed both before and after this documentation change: **161 passed** at each point. The engineering-register check reported **2 files scanned, 0 findings**.

## Known limits and later work

- Upstream line 190 switches to turbine AEP when `plant_aep_in` is zero; line 82 defaults wake loss to 0.15. A future adapter shall supply positive net plant AEP explicitly. No passing backend detection check exists yet (#1690).
- Upstream line 89 declares `plant_aep` with cost units. A future adapter shall read only `lcoe`; the unused energy output is not validated here.
- The proposed mapping sets the turbine cost offset to zero and treats all non-turbine CAPEX as BOS. It assumes constant annual net production, annual OPEX in USD/kW/yr and no escalation or decommissioning term. Those excluded terms shall raise field-specific errors.
- Resolve the upstream import failure without weakening repository checks, then restore tests and an adapter only after both pinned-backend cases pass with zero skips. Provisional mapping tests do not replace this gate.
- A NumPy compatibility decision, dependency extra, isolated CI, component sizing, ORBIT/LandBOSSE cost models and coupling to FLORIS (#1688) or WEIS (#1692) remain later work.
