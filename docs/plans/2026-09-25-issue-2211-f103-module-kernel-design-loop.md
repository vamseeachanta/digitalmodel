# Plan: DNV-RP-F103 module, single CP kernel, full B401 Sec. 7 design loop (#2211)

> Issue: #2211 | Epic: #2206 | Owner decisions: D1–D3 (keep editions/standards open), D6
> Status: approved by owner instruction "continue with tasks" (2026-09-25); implementation in worktree `feat/cp-2211-f103-kernel-design-loop`, stacked on the #2207 branch (`b401_tables.py`, `f103_tables.py`, wired package).

## Why
The review (`docs/domains/cathodic_protection/technical-quality-review-2026-09-24.md`, section 4 "B401 design loop", "Pipeline", "Duplication"; Appendix A2/A3) found:
- current demand implemented 7x, anode mass 7x, coating a + b·t 6x, Dwight/McCoy resistance in four variants with three different flush formulas;
- no initial/final anode current-output check (B401 Sec. 7.8): the A2 jacket needs 506 anodes by initial current, the package returns 487 by mass with no flag;
- bracelet/flush resistance not B401 Table 10-7 (`flush_anode_resistance` takes W/H then ignores them, inch/ohm-cm units, half-space slender-body expression mis-attributed to McCoy); no L >= 4r validity check; `number_of_anodes(x, 0)` raises a bare `ZeroDivisionError`;
- `anode_depletion` double-counts the utilisation factor (consumed = I·t·8760/(eps·u)), so `end_of_life_year` disagrees with the same profile's zero crossing;
- no DNV-RP-F103 bracelet module: F103 content is split across `dnv_rp_b401.protected_length`, `anode_sizing` (u = 0.80) and the uncalled legacy solver;
- `marine_cp.py` duplicates `marine_structure_cp.py` (two `MarineCPResult` classes).

## Scope (deliverables)
1. **`_kernels.py`** (new): the single implementation of `current_demand`, `anode_mass`, `coating_breakdown_linear` (+ `coating_breakdown_mean` = a + b·T/2, `coating_breakdown_final` = a + b·T, clamped at 1.0), `anode_current_output(delta_E, R_a)`, the B401 Table 10-7 resistance set — `long_slender_standoff` (L >= 4r), `short_slender_standoff` (L < 4r), `long_flush` (rho/(2S), S = mean of length and width, L >= 4·width and >= 4·thickness), `short_flush_or_bracelet` (0.315·rho/sqrt(A)) — with validity checks raising `ValueError` naming the parameter; `slender_standoff` dispatcher; `equivalent_radius_from_periphery` (c/(2·pi), Table 10-7 note 2); `equivalent_radius_from_mass`; `resistance_proximity_factor(distance_m)` (1.0 for >= 0.30 m, 1.3 for 0.15–0.30 m per Table 10-7 note 1, `ValueError` below 0.15 m); `anode_count` / `anodes_for_current` (ceil, positive-input validation).
2. **Thin wrappers**: `dnv_rp_b401.py`, `anode_sizing.py`, `iso_15589_2.py`, `marine_structure_cp.py`, `marine_cp.py`, `anode_depletion.py`, `pipeline_cp.py`, `coating.py` call the kernel where they re-implement demand / mass / breakdown / resistance. Every public name, signature and `edition` argument stays. `dnv_rp_b401.flush_anode_resistance` becomes a deprecated wrapper (inches / ohm-cm to SI, `short_flush_or_bracelet` with A = L·W; W is now used, H and r_eq accepted for signature compatibility). `anode_sizing.calculate_anode_resistance` dispatches by `AnodeType` to the Table 10-7 set. `iso_15589_2.anode_resistance` uses the Table 10-7 long-slender form (the review found its `ln(2L/r) - 0.5` form in neither standard).
3. **`anode_depletion` fix**: consumed = I·t·8760/eps; usable = M·u; remaining life = (M·u - consumed)·eps/(I·8760); `is_depleted` when consumed >= M·u; `end_of_life_year` = M·u·eps/(I·8760), which is the profile's zero crossing of usable mass (test added). `marine_structure_cp.retrofit_assessment` gets the same fix.
4. **`dnv_rp_f103.py`** (new; editions "2010"/"2016" via `normalize_f103_edition`): `BraceletDesignInput` (pydantic: OD, WT, length, linepipe + field-joint coating with joint area fraction or joint count, exposure, fluid temperature, design life, seawater resistivity, steel resistivity 2.0e-7 ohm-m, anode alloy from `b401_tables`, bracelet net mass / length / thickness or exposed area, delta_E_me 0.15 V) and `BraceletDesignResult` (f_cm / f_cf for linepipe and FJC, I_cm, I_cf, total net mass, anode count per case, spacing, protected length per Eq. 14, spacing <= 2·PL check, final current-output check with 0.315·rho/sqrt(A), governing case, citations, standard, provenance). `protected_length` moves here; `dnv_rp_b401.protected_length` stays as a wrapper.
5. **Full B401 Sec. 7 loop** in `marine_structure_cp.marine_structure_current_demand` (optional anode geometry: length, seawater resistivity, alloy density, driving voltage) and `anode_sizing.design_cp_system`: N = max(N_mass, N_initial, N_final); N_initial uses the fresh geometry, N_final the depleted anode (remaining mass (1 - u)·m_a, same length, radius from the mass-based equivalent radius — assumption documented; B401 7.8 asks for the final resistance with the anode consumed to its utilisation limit). Results carry per-case counts and `governing_case`.
6. **`marine_cp.py`** becomes a thin deprecated facade over `marine_structure_cp` (`DeprecationWarning` on import of `design_marine_cp`); tests converted to the facade behaviour.
7. **Exports** in `__init__.py`: kernel names under `kernel_`, F103 names under `f103_` / `F103…`.

## Assumptions
- Final-case depleted geometry for stand-off anodes: cylinder of the remaining mass (1 - u)·m_a at the original length; flush / bracelet types in `anode_sizing` map the equivalent radius to width = 2r (flush) and A = 2·pi·r·L (bracelet). The F103 module uses the bracelet exposed area directly.
- F103 Table A.1 FBE constants are a = 0.010, b = 0.0003 as printed (the review's A3 used a = 0.030 from memory); the A3 test is written from the table.
- `dnv_rp_b401.protected_length` maps the B401 edition to the companion F103 edition (2005/2010 -> 2010; 2017/2021 -> 2016), the inverse of the map `f103_tables` already uses.

## Out of scope (other children)
Engine adapter and legacy `*_legacy` routing (#2210); reporting (#2212); test hygiene beyond the files touched here (#2213); retiring `cp_DNV_RP_F103_2010.py`.

## Verification
- `tests/cathodic_protection/`, `tests/specialized/cathodic_protection/`, `tests/marine_ops/marine_engineering/test_cathodic_protection_dnv.py`, `tests/benchmarks/test_cp_benchmarks.py` and `tests/workflows/test_durable_workflows.py -k cathodic` green (baseline 779 + 6).
- New tests: `test_kernels.py` (formulas, validity errors, proximity factor), `test_dnv_rp_f103.py` (Appendix A3: A = 10 176 m2, i_cm = 0.050, f_cm = 0.01375, I_cm = 6.996 A, M = 957.7 kg, 24 x 40 kg bracelets, PL from Eq. 14 with f_cf = 0.0175), `test_cp_cross_module_consistency.py` (mass and resistance entry points agree), A2 regression extended with N_initial = 506 (R_a = 0.0790 ohm, I_a = 3.16 A, I_initial = 1600 A), N_final from the depleted geometry and the governing case, depletion end-of-life vs profile zero crossing, `number_of_anodes(x, 0)` raises `ValueError`.
- `ruff check` and `mypy` clean on every file touched.
- Callers of deprecated names in `examples/` and `scripts/` grepped and reported.
