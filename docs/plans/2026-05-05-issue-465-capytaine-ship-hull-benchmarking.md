# Plan: digitalmodel #465 — Capytaine ship hull benchmarking: OC4 semi-sub + WAMIT validation

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/465
**Status:** plan-review
**Tier:** T2 (validation harness, no new physics)
**Related:** #464 (Capytaine module — DONE)

## Context

The Capytaine module (`src/digitalmodel/hydrodynamics/capytaine/`) is implemented and validated against the Hulme 1982 sphere analytical benchmark. Production-readiness requires hull validation against more representative geometries. The repo already carries the reference data:

- **OC4 semi-sub**: full case at `docs/domains/orcawave/examples/L02 OC4 Semi-sub/` — `.gdf` mesh, `.owr` results, `.yml` config (mass 13.473M kg, water depth 200 m). The Capytaine module's `mesh_adapter.py` already supports GDF (`MeshFormat.GDF`).
- **WAMIT cases**: 12 validation cases at `docs/domains/orcawave/L00_validation_wamit/{2.1, 2.2, 2.3, ...}` — each has `Wamit v7.3 files/` (`.dat`, `.out`, `.frc`, `.pot`, `.cfg`, `.gdf`) plus `OrcaWave v11.0 files/` for cross-solver baseline. Case 2.1 is the cylinder.
- **Unit-box benchmark** at `docs/benchmarks/unit_box/` — 3-way (AQWA, OrcaWave, BEMRosetta) reference for sanity.
- **WAMIT loader**: `src/digitalmodel/hydrodynamics/diffraction/wamit_reference_loader.py` — extracts added mass / damping arrays from WAMIT `.out` files.

Gap: there's no bridge running Capytaine on the OC4 mesh + comparing against WAMIT/OrcaWave reference, and no pass/fail test gating the comparison.

## Plan

1. **Confirm WAMIT-loader API.** Read `src/digitalmodel/hydrodynamics/diffraction/wamit_reference_loader.py` and identify: function signature for loading `.out` files, returned data shape (DataFrame? xarray?), units. Capture as a 10-line block in the new benchmark module's docstring so the executor doesn't re-derive.

2. **Author benchmark harness.** New module `src/digitalmodel/hydrodynamics/capytaine/benchmarks.py`:
   - `run_oc4_semi_sub_benchmark(out_dir: Path) -> BenchmarkResult` — load GDF, set up `BodyConfig` per the L02 spec.yml's mass + water depth, solve via `CapytaineSolver`, compare added mass / damping against WAMIT reference loaded by `wamit_reference_loader`
   - `run_wamit_cylinder_benchmark(case="2.1", out_dir: Path) -> BenchmarkResult` — same pattern for case 2.1
   - `BenchmarkResult` dataclass: `case_name`, `metric_name`, `capytaine_value`, `reference_value`, `relative_error`, `pass_threshold`, `passed`

3. **Pass/fail thresholds.** Add `src/digitalmodel/hydrodynamics/capytaine/benchmarks_config.yaml`:
   ```yaml
   thresholds:
     added_mass_relative_error: 0.05   # 5% per DNV-RP-C205 §7.1
     damping_relative_error: 0.10      # 10% (more sensitive numerically)
     rao_amplitude_relative_error: 0.05
   citations:
     - code_id: DNV-RP-C205
       clause: "§7.1 (potential flow validation)"
   ```
   Cite via citation contract.

4. **Cross-solver comparison plots.** New `src/digitalmodel/hydrodynamics/capytaine/plot_benchmarks.py`:
   - `plot_added_mass_comparison(results: list[BenchmarkResult]) -> Figure`
   - `plot_excitation_comparison(...)` — overlays Capytaine vs WAMIT vs OrcaWave on shared axes
   - Saves to `docs/benchmarks/capytaine/<case>/<metric>.png`

5. **CLI driver.** New entrypoint `python -m digitalmodel.hydrodynamics.capytaine.benchmarks --case oc4 --out docs/benchmarks/capytaine/oc4/` runs the full case, writes plots + `benchmark_report.json`.

6. **Tests.** New `tests/hydrodynamics/capytaine/test_benchmarks.py`:
   - `test_oc4_benchmark_passes_threshold` — full benchmark, slow-marked (`pytest.mark.slow`); assert `passed == True` for added mass and damping at 3 wave periods
   - `test_wamit_cylinder_case_2_1_passes` — same for case 2.1
   - `test_benchmark_result_serialization` — `BenchmarkResult.to_dict()` round-trips JSON
   The `slow` marker keeps these out of the default fast-CI lane; they run on a nightly job.

7. **Documentation.** New `docs/benchmarks/capytaine/README.md` — table of cases, pass/fail status, error percentages, plots inline.

8. **Smoke check.** `uv run python -m digitalmodel.hydrodynamics.capytaine.benchmarks --case wamit-2.1 --out /tmp/cap-bench/ -v` — completes in < 60 s, writes report.json with `passed: true`. Then `uv run python -m digitalmodel.hydrodynamics.capytaine.benchmarks --case oc4 --out /tmp/cap-bench-oc4/ -v` — slower, completes within 5 min, all metrics pass DNV thresholds.

## Acceptance Criteria

- [ ] `benchmarks.py` runs OC4 semi-sub against WAMIT/OrcaWave reference; produces BenchmarkResult per metric
- [ ] WAMIT case 2.1 (cylinder) passes thresholds
- [ ] Cross-solver plots emitted to `docs/benchmarks/capytaine/<case>/`
- [ ] Pass/fail thresholds documented and citation-bound to DNV-RP-C205 §7.1
- [ ] `tests/hydrodynamics/capytaine/test_benchmarks.py` has slow-marked benchmark tests, all passing
- [ ] `docs/benchmarks/capytaine/README.md` summarizes results

## Open Questions

- The Capytaine env at `/mnt/local-analysis/capytaine-env` may not be on every CI runner. The `slow` marker keeps these tests out of the fast lane; benchmark runs happen via dedicated nightly job. Confirm a nightly job slot exists.
- OC4 mesh size: if the GDF is too coarse for direct Capytaine ingestion, may require `mesh_refiner` from `hull_library/`. Spike it during step 1.
