# Plan: cathodic-protection test hygiene (#2213)

> Issue: #2213 | Epic: #2206 | Owner decision: D6
> Status: approved by owner instruction "continue with tasks" (2026-09-25); implementation in worktree `test/cp-2213-hygiene`, stacked on the #2211 branch.

## Why
Review `docs/domains/cathodic_protection/technical-quality-review-2026-09-24.md` section 5 (Tests):
- `test_dnv_rp_b401_doc_verified.py` claims "DNV worked examples" but is verified against `calc-008` (abstracted SLHR calculation under B401:2005); the flush-mount values (0.4209 / 0.3258 / 0.5114 ohm, "riser modification calc, Appendix 2") trace to no repo document; F103 Eq. 14 tests sit in a B401 file; several assertions are range-only.
- `test_anode_depletion_new.py` duplicates the module under a second file name.
- `tests/fixtures/test_vectors/cathodic_protection/*.yaml` (10 files) are read by nothing; some expected values were produced by the pre-#2211 formulas (Sunde half-space flush resistance, ISO `ln(2L/r) - 0.5`) and some constants are mislabelled as Table 10-4.
- No property-based tests (hypothesis is installed) and no negative-input tests for the kernel outside `test_kernels.py`.
- No published-value checks for ISO 15589-1 Table 1 potentials, NORSOK M-506 or de Waard-Milliams.
- `tests/DOMAINS.md` cathodic row omits the two files CI already routes to the domain.

## Scope (tests only, plus `tests/DOMAINS.md` and the YAML vectors)
1. `test_dnv_rp_b401_doc_verified.py`: honest module docstring (calc-008 / calc-009, not DNV worked examples); cite `calc-009` for the flush-mount inputs (24 in x 5 in x 2.5 in, 29 lb, 31 / 24 ohm-cm) and keep only Table 10-7 hand-derived expected values; move the 8 in gas-lift Eq. 14 case into `test_dnv_rp_f103.py` (12 in case already there) and delete the B401-file copies; replace range-only assertions with the calc-008 values (r = 0.0949 m, R_a = 0.0725 ohm, I_a = 3.45 A; x1.3 -> 0.0942 ohm, 2.65 A).
2. Merge `test_anode_depletion_new.py` into `test_anode_depletion.py` (no name collisions), delete the `_new` file.
3. `test_test_vectors.py`: load every YAML, map each `worked_examples[]` entry (schema is `description / inputs / outputs / use_as_test`, not `function / inputs / expected`) to the public API by output key, assert within the vector tolerance. Re-derive the vectors whose expected values came from pre-#2211 formulas; delete unmappable examples with a note.
4. `test_properties.py` (hypothesis, `@settings(deadline=None, max_examples=100)`, `derandomize=True`): mass proportional to I and T; R_long_slender proportional to rho; demand linear in area and f_c; breakdown monotone non-decreasing and clamped at 1; anode count >= mass / m_a; protected length decreasing in i_cm and f_cf; ValueError on negative / zero inputs for current_demand, anode_mass, protected_length, number_of_anodes and the four Table 10-7 resistance forms.
5. `test_published_values.py`: ISO 15589-1:2015 Table 1 potentials through `cp_reporting.compliance_check_potential` and `iso_15589_2.check_protection_potential`; de Waard-Milliams 1975 at 60 degC / 1 bar (derived from the published equation); NORSOK M-506 as `xfail(strict=False)` (published value pending source PDF).
6. `tests/DOMAINS.md`: add `tests/marine_ops/marine_engineering/test_cathodic_protection_dnv.py` and `tests/benchmarks/test_cp_benchmarks.py` to the cathodic row (matches `scripts/ci/detect_touched_domains.py`).
7. `ruff check tests/cathodic_protection` clean.

## Out of scope (owned elsewhere)
- `test_api_rp_1632.py`, `test_corrosion_rate.py`, `test_fuel_system_cp.py`, `test_iccp_design.py`, `test_pipeline_cp.py`, `test_stray_current.py` (PR #2215).
- `tests/specialized/cathodic_protection/conftest.py` (unused fixtures, Al capacity 2750 Ah/kg is the density) — #2210 owns `tests/specialized/`; reported, not edited.
- Anything under `src/digitalmodel/cathodic_protection/`.

## Verification
`PYTHONUTF8=1 <venv>/python.exe <scratch>/wt_pytest.py tests/cathodic_protection -q` green (573 before); `ruff check tests/cathodic_protection` clean.
