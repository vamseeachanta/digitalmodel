# Plan: CP engine adapter, PASS/FAIL adequacy, legacy behind `*_legacy`, demos retired (#2210)

> Issue: #2210 | Epic: #2206 | Owner decisions: D4 (engine adapter onto the new package, legacy behind `*_legacy` keys), D5 ("no more demos; end deliverables become HTML/PDF")
> Status: approved by owner instruction "continue with tasks" (2026-09-25); implementation in worktree `feat/cp-2210-engine-adapter`, stacked on the #2211 branch (`_kernels.py`, `dnv_rp_f103.py`, full B401 Sec. 7 loop in `marine_structure_cp` / `anode_sizing`).

## Why
The review (`docs/domains/cathodic_protection/technical-quality-review-2026-09-24.md`, section 3, B13 and B14) found:
- B13: `engine.py` routes `basename: cathodic_protection` to the legacy solver only; the new package is unreachable from any YAML.
- B14: the legacy ABS route returns a fractional anode count (184.257) and `adequate: false` / `initial_meets_demand: false` never fail the run; four of six demos exit 0 with failing designs.
- D5: the six `examples/workflows/cathodic-protection*` demos are retired; client deliverables come from the standard HTML/PDF reporting engine (#2212).

## Scope (deliverables)
1. **`src/digitalmodel/cathodic_protection/engine_adapter.py`** (new): `run_cathodic_protection(cfg) -> cfg` dispatching on `inputs.calculation_type`:
   - `DNV_RP_B401_offshore` -> the new package. Zones from `inputs.structure.zones` (`zone` / `base_zone` -> `marine_structure_cp.ExposureZone`, `area_m2`, `coating_category` I/II/III/bare -> Table 10-4 constants via `b401_tables.coating_breakdown_constants` and `_kernels.coating_breakdown_mean/final`, `depth_m` -> depth band) validated as `StructuralZone`s; densities from `marine_structure_cp.zone_current_density` (Tables 10-1/10-2, Sec. 6.3); climate from `environment.seawater_temperature_C`; anode from `inputs.anode` (`type`, `length_m` required, `radius_m` optional, `individual_anode_mass_kg`, `material`, `utilization_factor` optional -> Table 10-8, optional `count` = installed count to verify); design life and `edition` from `design_data`. The Sec. 7 loop: `N_mass` (Eq. 2), `N_initial` with the fresh anode (`anode_sizing.calculate_anode_resistance`, Table 10-6 driving voltage), `N_final` with the anode consumed to its utilisation limit, `governing_case` from `anode_sizing.governing_case`. Results keep the legacy top-level keys (`standard`, `edition`, `provenance`, `citations`, `design_life_years`, `surface_areas_m2`, `coating_breakdown`, `current_densities_A_m2`, `current_demand_A` with `total_initial_A/total_mean_A/total_final_A`, `anode_resistance_ohm`, `anode_requirements`, `current_output_verification` with `adequate/governing_case/recommended_anode_count`) plus `status`.
   - `DNV_RP_F103_2010` -> `dnv_rp_f103.design_bracelet_cp` mapped from the F103 YAML schema (`pipeline.outer_diameter_m/wall_thickness_m/length_m/burial_condition/internal_fluid_temperature_C/coating_type/resistivity_ohm_m`, optional field-joint keys, `environment.seawater_resistivity_ohm_m`, `anode.material/individual_anode_mass_kg/length_m/thickness_m|exposed_area_m2/utilization_factor`, `design_data.design_life/edition`). Results: `pipeline_geometry_m`, `coating_breakdown_factors`, `current_densities_A_m2`, `current_demand_A`, `anode_requirements`, `anode_spacing_m`, `attenuation_analysis` (protected length, `protection_adequate`), `citations`, `status`.
   - `ABS_gn_ships_2018`, `ABS_gn_offshore_2018` -> legacy implementation wrapped: anode counts become `ceil` ints (raw kept as `anode_count_raw`), `status` from `anode_performance.checks` (ships) or mass-only sizing (offshore, which has no current-output check); `cfg["results"]` aliases the ships' `cfg["cathodic_protection"]` block.
   - `DNV_RP_B401_offshore_legacy`, `DNV_RP_F103_2010_legacy` -> the old code paths unchanged, with a `DeprecationWarning`.
   - Unknown key -> `ValueError` listing the accepted keys.
   - `status = {"result": "PASS"|"FAIL", "governing_case", "reason", "checks"}`; a FAIL is logged through loguru `logger.warning` (the engine's logger) and never raises.
2. **`engine.py`**: the `cathodic_protection` arm calls `run_cathodic_protection`; the legacy `CathodicProtection` import is dropped. `tests/test_engine.py` routing row updated to patch the adapter function.
3. **Deletions**: `infrastructure/base_solvers/hydrodynamics/cp_DNV_RP_F103_2010.py` (no callers; its `__init__` export removed) and the `infrastructure/common/cp_DNV_RP_F103_2010.py` shim (no importers). The `common/{cathodic_protection,cp_DNV_RP_B401_2021,cp_sacrificial_anode_b401}.py` shims stay: they are imported by `tests/specialized/cathodic_protection/*`, `tests/marine_ops/marine_engineering/test_cathodic_protection_dnv.py` and the engine mock lists. `scripts/generate_cp_report.py` and `scripts/check_run_cp.py` (the task's `run_cp_test.py` does not exist) import `digitalmodel.common.cathodic_protection` and `digitalmodel.reporting.cp_html_report`, neither of which exists, and drive a coating-quality / wet-storage schema the legacy solver no longer reads; both are deleted (reporting is #2212).
4. **Demos retired**: the six `input.yml` files move to `tests/fixtures/cathodic_protection/workflow_inputs/<name>.yml` (the pipeline fixture gains the bracelet dimensions the F103 module needs); the example directories, READMEs, `docs/registry/workflows.yaml` rows and the `capabilities-sections.yml` href are removed; the CP arms leave `tests/workflows/test_durable_workflows.py`.
5. **`tests/cathodic_protection/test_engine_adapter.py`** (new): every fixture through the adapter; schema keys, int counts, `status.result` in {PASS, FAIL} with a governing case; pinned numbers re-derived from the tables (below); unknown key, legacy keys (DeprecationWarning), FAIL warning, `anode.count` override.
6. **Docs**: `docs/registry/module-routing.yaml` and `docs/maps/digitalmodel-operator-map.md` cathodic rows name the adapter as the engine entry, no demo workflows, legacy keys deprecated.

## Derivations pinned in the new test (DNV-RP-B401 2021 tables, Sec. 7 loop)
- Jacket (temperate 10 C, 0-30 m, Cat III a = 0.02, b = 0.012, T = 25 yr, 5000 m2 submerged; splash/atmospheric draw no CP current): f_ci/f_cm/f_cf = 0.02/0.17/0.32; I_initial = 5000 x 0.200 x 0.02 = 20.0 A; I_mean = 5000 x 0.100 x 0.17 = 85.0 A; I_final = 5000 x 0.130 x 0.32 = 208.0 A; M = 85 x 25 x 8760 / (2000 x 0.85) = 10 950 kg -> N_mass = 55 x 200 kg. Fresh stand-off (rho 0.30, L 1.0 m, r 0.05 m): R = 0.30/(2 pi) (ln 80 - 1) = 0.1615 ohm, I_a = 0.25/0.1615 = 1.548 A -> N_initial = 13. Depleted anode (u = 0.85, r_f = 0.05 sqrt(0.15) = 0.01936 m): R_f = 0.2068 ohm, I_a = 1.209 A -> N_final = 173. Final governs; recommended 173; status FAIL (55 mass-based anodes < 173).
- Manifold (arctic 6 C, 850 m2): I_mean = 17.34 A, M = 2233.8 kg -> 28 x 80 kg; R = 0.2606 ohm -> N_initial = 5; R_f = 0.3436 ohm -> N_final = 64; FAIL, final governs.
- Monopile (temperate 8 C, T = 30 yr, 1200 m2): I_mean = 24.0 A, M = 3710.12 kg -> 25 x 150 kg; R = 0.1532 ohm -> N_initial = 3; R_f = 0.2002 ohm -> N_final = 48; FAIL, final governs.
- Pipeline (F103 2010, non-buried, 60 C -> i_cm = 0.060 A/m2; FBE a = 0.010, b = 0.0003; T = 30 yr): A = pi x 0.3239 x 1500 = 1526.4 m2; f_cm = 0.0145, f_cf = 0.019; I_cm = 1.328 A, I_cf = 1.740 A; M = 218.1 kg -> 9 x 25 kg (u = 0.80 input); bracelet 0.2 m x 0.04 m: A_a = 0.254 m2, R = 0.1875 ohm, I_a = 1.333 A -> N_final = 2; spacing 166.7 m <= 2 x PL (Eq. 14, PL ~ 3.15 km); PASS, mass governs.
- Ships (ABS 2018 legacy): mean demand 196.667 A, mass 5067.07 kg -> 185 anodes (ceil of 184.257); status from the legacy initial/final checks.
- FPSO (ABS offshore 2018 legacy): mass 14 091.15 kg -> 79 x 180 kg; PASS by mass only (the route has no current-output check).

## Assumptions
- With `radius_m` given, the fresh equivalent radius is the input and the depleted radius scales as r sqrt(1 - u) (remaining cross-section at the same length, consistent with `anode_sizing.depleted_equivalent_radius` for the mass-based case). Without `radius_m` the mass-based radii are used.
- `anode.count` (optional) is the installed count that the adequacy check verifies; otherwise the mass-based count is verified, as the legacy route did.
- The ABS offshore route stays mass-only; its status is PASS with the reason stating that no current-output check exists in that route.
- Retired demo `results/` outputs were untracked and are not carried over.

## Out of scope (other children)
Reporting (#2212); test hygiene beyond the files touched (#2213); docs index/examples (#2214).

## Verification
- `tests/cathodic_protection tests/specialized/cathodic_protection tests/marine_ops/marine_engineering/test_cathodic_protection_dnv.py tests/benchmarks/test_cp_benchmarks.py tests/workflows tests/test_engine.py tests/docs/test_digitalmodel_routing_contract.py tests/capabilities` green with the worktree runner.
- `ruff check` and `mypy` clean on every file touched.
- One fixture (jacket) run end to end through `digitalmodel.engine.engine` from a scratch copy; the printed `status` block reported.
