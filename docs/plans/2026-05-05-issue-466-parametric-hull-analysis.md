# Plan: digitalmodel #466 — Parametric hull analysis: steady speed, passing ship, narrow water

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/466
**Status:** plan-review
**Tier:** T3 (four feature blocks)
**Related:** #464 (Capytaine BEM)

## Context

The hydrodynamics module carries:
- `src/digitalmodel/hydrodynamics/hull_library/` — extensive parametric hull tooling already exists (`parametric_hull.py`, `panel_catalog.py`, `mesh_generator.py`, `mesh_refiner.py`, `panel_inventory.py`, `rao_database.py`, `rao_lookup_plots.py`, `rao_registry.py`) — Block 1 is largely implemented
- `src/digitalmodel/hydrodynamics/passing_ship/` — full module: `calculator.py`, `formulations.py`, `force_time_history.py`, `templates/{basic,offshore,tanker}.yml`, CLI, exporters — Block 3 is largely implemented
- `src/digitalmodel/hydrodynamics/capytaine/` — BEM solver from #464

Real gaps per the issue:
- **Block 1**: tie hull-library sweeps to a `vessel_operator_charts` deliverable — automated plotting comparing variants
- **Block 2 (Steady Speed)**: NO existing forward-speed correction in the BEM pipeline; speed-dependent added mass/damping not implemented
- **Block 3**: passing ship is built but lacks a `narrow_water` extension and a charts deliverable
- **Block 4 (Narrow/Shallow water)**: NO existing finite-depth corrections wired into Capytaine, no bank-effects implementation

The issue asks for DNV-RP-C205 §3.5 (shallow water) and §7.4 (forward speed) corrections — both are documented analytic correction families that wrap a BEM solve.

## Plan

1. **Inventory existing hull-library + passing-ship surface.** `grep -n "def " src/digitalmodel/hydrodynamics/hull_library/*.py src/digitalmodel/hydrodynamics/passing_ship/*.py | wc -l`. Confirm Block 1 is genuinely "deliverable plotting only" gap. If sweep machinery is already there, the Block 1 work is a 100-line plotting wrapper, not a new module.

2. **Block 1 — Vessel operator charts.** New `src/digitalmodel/hydrodynamics/hull_library/operator_charts.py`:
   - `plot_added_mass_sensitivity(panel_catalog, params=["beam","draft","cb"]) -> matplotlib.Figure`
   - `plot_operability_envelope(rao_database, criterion: dict) -> Figure`
   - `export_comparison_table(panel_catalog) -> pandas.DataFrame`
   Emit one chart per parameter sweep showing variant-vs-variant added mass and damping at design wave period.

3. **Block 2 — Forward-speed corrections.** New `src/digitalmodel/hydrodynamics/capytaine/forward_speed.py`:
   - `apply_forward_speed_correction(bem_result, U: float, dnv_clause="C205-7.4") -> BEMResult` — applies the Salvesen-Tuck-Faltinsen strip-theory forward-speed correction OR the DNV-RP-C205 §7.4 simplified frequency-shift; emit Citation per `.claude/rules/calc-citation-contract.md`
   - `wave_resistance_estimate(hull_geom, U, sea_state) -> float` — DNV-recommended polynomial fit; cite clause
   The function takes an existing zero-speed BEM result and produces a corrected one — does not re-mesh.

4. **Block 3 — Narrow-water passing-ship extension.** Edit `src/digitalmodel/hydrodynamics/passing_ship/formulations.py` to add `passing_ship_in_channel(...)` consuming channel width and depth, applying PIANC bank-effect corrections. New unit tests in `tests/hydrodynamics/passing_ship/test_narrow_water.py`. Reuse the existing CLI; add `--channel-width` and `--channel-depth` flags.

5. **Block 4 — Shallow-water hydrodynamic corrections.** New `src/digitalmodel/hydrodynamics/capytaine/shallow_water.py`:
   - `finite_depth_correction(bem_result, water_depth: float, dnv_clause="C205-3.5") -> BEMResult` — Bessel-function-based correction per DNV-RP-C205 §3.5
   - `squat_estimate(vessel: Vessel, U: float, depth: float) -> dict` — Tuck/Romisch squat formulae for restricted waterways
   Cite each function with the DNV clause via the citation contract.

6. **Update manifests.** Edit `src/digitalmodel/hydrodynamics/capytaine/manifest.yaml` to add new functions (`apply_forward_speed_correction`, `wave_resistance_estimate`, `finite_depth_correction`, `squat_estimate`) with their DNV clauses. Add a similar manifest update to `passing_ship/` if it has one.

7. **Tests.** New `tests/hydrodynamics/`:
   - `capytaine/test_forward_speed.py` — non-zero U produces non-zero shift, U=0 returns identity, citation present
   - `capytaine/test_shallow_water.py` — infinite depth → zero correction; specific depth produces published Bessel-eval value (one canonical case)
   - `passing_ship/test_narrow_water.py` — bank-effect amplification monotone with closer bank
   - `hull_library/test_operator_charts.py` — plot returns Figure object, table has expected columns

8. **Smoke check.** `uv run python -c "from digitalmodel.hydrodynamics.capytaine.shallow_water import finite_depth_correction; ..."` — module imports clean. `uv run pytest tests/hydrodynamics/ -v -k 'forward_speed or shallow_water or narrow_water or operator_charts'` — all new tests green.

## Acceptance Criteria

- [ ] `operator_charts.py` produces parameter-sweep plots from existing hull-library catalog
- [ ] `forward_speed.py` applies DNV §7.4 correction to BEM results with Citation
- [ ] `passing_ship` exposes narrow-water variant with channel-geometry inputs
- [ ] `shallow_water.py` applies DNV §3.5 finite-depth corrections to BEM results with Citation
- [ ] Capytaine manifest.yaml lists every new function with the correct clause
- [ ] All new tests pass; existing `test_capytaine_*` and `test_passing_ship_*` unaffected

## Open Questions

- Forward-speed correction theory choice: DNV simplified vs Salvesen-Tuck-Faltinsen full strip-theory. Default DNV (defensible, single-equation) and document Faltinsen as future work.
- Does the citation contract apply to passing-ship PIANC formulae? PIANC isn't in the existing wiki standards/ corpus — the executor must either skip citation per "Do NOT apply when constant is convention-only" clause, or open a wiki-page seed for PIANC. Default: skip citation, add a TODO referencing the rule's "do not apply when" branch.
