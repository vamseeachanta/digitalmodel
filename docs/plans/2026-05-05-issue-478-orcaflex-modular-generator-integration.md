# Plan: digitalmodel #478 — OrcaFlex model generator integration: spec.yml → .dat pipeline

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/478
**Status:** plan-review
**Tier:** T3 (module-feature)
**Parent:** #471

## Context

`src/digitalmodel/marine_ops/installation/jumper_lift.py` exposes `generate_orcaflex_line_sections_yaml()` which emits a 27-section list (`name`, `line_type`, `length_m`) for the jumper geometry — but only the **line-types** block, not a full model spec. The modular generator at `src/digitalmodel/solvers/orcaflex/modular_generator/` consumes a wider schema (vessels, environment, analysis, line types) demonstrated in `docs/domains/orcaflex/installation/pipeline/route_a/spec.yml` and `docs/domains/orcaflex/library/templates/jumper_rigid_subsea/spec.yml`. Gap: the function output cannot be fed straight to `modular_generator.generate()` to produce a valid `.dat`.

Existing infrastructure already in place:
- 27-section breakdown is correct (`compute_orcaflex_sections` in `jumper_lift.py`)
- Both jumper specs (`ballymore_mf_plet/spec.yml`, `ballymore_plet_plem/spec.yml`) carry `pipe`, `environment`, `metadata`, `cranes` — what's missing for a full modular spec is `vessels`, `line_types` (with bend stiffness + axial stiffness), `analysis` (static + dynamic settings), and the modular generator's `model.yml` envelope.

Reference live SZ.yml: `rock-oil-field/s7/ballymore/sut_mm/SZ.yml` (40 scenario files) — the canonical shape of the .dat-bound model.yml the generator should produce.

## Plan

1. **Map the modular generator's required schema.** Read `src/digitalmodel/solvers/orcaflex/modular_generator/__init__.py` and the `generate()` entry point, plus one full reference spec at `docs/domains/orcaflex/library/templates/jumper_rigid_subsea/spec.yml`. Catalog the exact top-level keys and any `$ref`-style section composition. Write findings into a 30-line block at the head of the new conversion module so the executor doesn't re-derive the contract.

2. **Author `jumper_to_modular_spec.py`.** New file `src/digitalmodel/marine_ops/installation/jumper_to_modular_spec.py`:
   - `build_modular_spec(jumper_cfg: JumperConfig, env: dict, analysis: dict) -> dict` — composes the dict the modular generator expects (vessels, line_types, line_sections, environment, analysis)
   - Pulls line-type definitions (steel + insulation cross-section, bend stiffness from steel I) from `BarePipeProperties` already on `jumper_cfg`
   - Pulls vessel block from a new `vessels:` section the executor will add to both `ballymore_*/spec.yml`
   - `to_yaml(modular_spec: dict, out_path: Path) -> Path` — writes via `ruamel.yaml` to preserve key order (already a project dep)

3. **Extend the two jumper specs with vessel + analysis blocks.** Edit `docs/domains/orcaflex/subsea/jumper/installation/ballymore_mf_plet/spec.yml` and `ballymore_plet_plem/spec.yml`. Add:
   ```yaml
   vessels:
     SZ: { hull: saipem_sz, raos: rao_libraries/saipem_sz.yml, position: [0, 0, 0] }
     DZ: { hull: deepwater_dz, raos: rao_libraries/deepwater_dz.yml, position: [10, 0, 0] }
   analysis:
     static: { ramp_load: true, max_iterations: 200 }
     dynamic: { duration_s: 600, time_step_s: 0.1, wave_seeds: [1,2,3] }
   ```
   RAO library paths are pointers; if RAOs are absent on Linux, the modular generator already supports `placeholder: true` for missing assets (verify in modular_generator code; if not, raise issue and gate dynamic generation).

4. **Wire the pipeline.** Edit `marine_ops/installation/jumper_installation.py` to add a `generate_orcaflex_model()` step that calls `build_modular_spec()` then `modular_generator.generate(spec, out_path=<output_dir>/<jumper>_model.yml)`. Add a `run_dat_export: bool = False` flag on `PipelineConfig` (default off; .dat write requires Windows + OrcaFlex license). Resulting model.yml is unconditionally emitted on Linux.

5. **Integration test.** New `tests/solvers/orcaflex/modular_generator/test_jumper_modular_spec.py`:
   - `test_build_modular_spec_for_mf_plet_has_27_line_sections` — load spec, build modular dict, assert 27 sections
   - `test_build_modular_spec_for_plet_plem_distinct` — both configs round-trip to distinct line-section length sums
   - `test_modular_generate_writes_model_yaml` — full pipeline writes a parseable model.yml

6. **Document in README.** Append a "Conversion parameters" section to `src/digitalmodel/marine_ops/installation/__init__.py` module docstring (or `marine_ops/installation/README.md` if it exists) listing the spec.yml→model.yml field map.

7. **Smoke check.** `uv run python -c "from digitalmodel.marine_ops.installation.jumper_installation import run_pipeline; r = run_pipeline('docs/domains/orcaflex/subsea/jumper/installation/ballymore_mf_plet/spec.yml', output_dir='/tmp/j'); print(r.output_files)"` — confirm `ballymore_mf_plet_jumper_model.yml` exists and parses with `yaml.safe_load`.

## Acceptance Criteria

- [ ] Both `ballymore_*/spec.yml` files carry `vessels:` and `analysis:` blocks
- [ ] `jumper_to_modular_spec.build_modular_spec()` produces a dict the modular generator's `generate()` accepts without error
- [ ] Pipeline `run_pipeline()` writes `<jumper>_model.yml` for both configurations
- [ ] Integration tests pass, with the 27-section assertion gating regressions
- [ ] Conversion-parameter documentation lands next to the module

## Open Questions

- RAO assets: are saipem_sz/deepwater_dz RAO libraries already on disk, or do they require synthesis from `marine_ops/installation/crane_tip_motion.py`? Default: synthesize if absent, with a warning logged.
- .dat emission: scope confirms model.yml suffices; .dat export remains issue-level out-of-scope until a Windows runner is wired (track separately).
