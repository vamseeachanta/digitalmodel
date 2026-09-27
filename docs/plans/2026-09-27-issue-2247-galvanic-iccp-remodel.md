# Plan: re-model galvanic corrosion and ICCP anode life from literature; remove the fuel-system check (#2247)

> Issue #2247 | Epic #2206 | owner decisions 2026-09-27
> Scope of this lane: galvanic, ICCP life and fuel-system items. The stray-current item is a separate lane (`stray_current.py`, `_provisional.py`).
> Status: implemented in worktree `feat/cp-2247-galv`. Not committed.

## Owner direction
The governing standards (NACE SP0572, NACE SP0169, ISO 15589-1, BS PD 6484) are not on file. Models are built from openly published literature. Every constant is a `ProvisionalValue(value, units, source, note, provisional=True, pending_standard)` naming its literature source. Models stay behind `experimental=True`. No number is attributed to a standard that has not been read. A default with no open source becomes a required input.

## Changes
| Item | Blocker | Change | Tests |
|---|---|---|---|
| `fuel_system_cp.check_protection` | B11 | Deleted, with its tests. The module docstring points to `cp_survey` (`check_potential_criteria`, `analyze_cis_survey`) for survey-based verification. The only other caller, `test_test_vectors.py`, took `PROTECTION_POTENTIAL_CSE` through the fuel module; it now imports it from `api_rp_1632`. | `test_check_protection_removed` |
| `corrosion_rate.galvanic_corrosion` | B12 | Rewritten as a mixed-potential solver (CNWRA 97-010 eqs. 2-2, 2-3, 2-26). Anode: Tafel. Cathode: Tafel plus O2 diffusion limit. Inputs: area ratio and `R_s`. Uses a bracketed Brent root in ln I and returns a convergence flag. Rate uses `faraday_rate_factor`. All kinetic parameters are required inputs. The uncited `GALVANIC_POTENTIAL` table and the risk bands are removed. Unknown `anode_material` raises. `oxygen_limiting_current_density(D, C, δ)` helper added. | Exact symmetric-couple hand value, Tafel-intersection hand value, couple between the free potentials (36 cases), monotonic in area ratio, diffusion plateau, `R_s` drop, raises |
| `iccp_design.anode_bed_design` | B9 | Per-material `ICCP_ANODE_RECORDS` (HSCI, graphite, scrap steel, magnetite, MMO, Pt/Ti, Pt/Nb) by environment. Life: consumable `N·m·u/(C·I)`, coating `N·w·A/(C·I)` (`iccp_anode_life`, experimental only). Resistance: TP-16 multi-anode (Sunde-form) with spacing; deep well as an active-column electrode (×0.6 removed); TP-16 horizontal column; `DISTRIBUTED` raises. Spacing, column geometry, coating loading, and mass/limit where no source exists are required inputs. Anode count comes from the current-density limit only, so geometry is independent of the experimental flag. | TP-16 worked examples (3.26 Ω; 28.6/21.2/17.0/2.6 Ω; deep well 7.2 to 2.2 Ω; life 301 yr), resistance falls with N and S, life linear in m and 1/I, scrap-steel rate against Faraday, every default provisional and sourced |
| `examples/demos/gtm/demo_08_iccp_design.py` | — | Anode options changed to vertical arrays with 5 m spacing and the seawater environment. Methodology text updated. The smoke test now expects `anode_life_years is None`; before this change it failed on `None > 0`. | demo smoke 4/4 |
| `docs/domains/cathodic_protection/standards-inventory.md` | — | Appended "Galvanic and ICCP provisional values — standards verification checklist (#2247)" | — |

## Follow-ups (not done here)
- Unify `_provisional_galvanic.ProvisionalValue` with the stray-current lane's `_provisional.ProvisionalValue` at merge. The fields and order are the same.
- `cathodic_protection/__init__.py:305` still lists `fuel_system_cp.check_protection` in its comment on quarantined models. It was never exported. This lane did not edit the comment, to avoid colliding with the stray-current lane.
- Once the standards are on file, work through the checklist rows. The TP-16 horizontal-column form comes first, because it differs from other published Dwight forms.
