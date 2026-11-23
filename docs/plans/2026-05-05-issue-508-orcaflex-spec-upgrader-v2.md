# Plan: digitalmodel #508 — OrcaFlex spec upgrader v2: rewrite generic specs to typed sections

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/508
**Status:** plan-review
**Tier:** T3
**Depends on:** #499 (closed — detection logic landed)

## Context

`src/digitalmodel/solvers/orcaflex/spec_upgrader.py` (454 lines) implements `detect_model_type()` returning `(DetectedModelType, confidence, evidence)` with regex-based heuristics for riser/mooring/pipeline/generic. The companion `upgrade_spec(spec, target_type)` is a stub that currently raises `NotImplementedError("upgrade_spec is planned for v2.")` (line ~423). v2 will turn this into a real rewrite that maps generic OrcaFlex sections (lines, line-types, vessels, end-connections) into the typed Pydantic schemas under `src/digitalmodel/solvers/orcaflex/modular_generator/schema/{riser,mooring,pipeline}.py`.

The target schemas already exist as Pydantic models. The challenge is bidirectional fidelity: an upgraded spec must round-trip through the modular generator and produce byte-equivalent OrcaFlex YAML to the original generic input. Three model_library specs (`a01_catenary_riser`, `c03_turret_moored_fpso`, `m01_pipeline_lateral_buckling`) provide ground truth.

## Plan

1. **Map generic → typed for each domain.** For each of riser/mooring/pipeline, read the schema file and the corresponding model_library spec. Document field-by-field mapping in module docstring at the top of `spec_upgrader.py`: e.g. for riser, `Lines[name=*flex*].LineType` → `riser.line_types[]`; `Lines[name=*flex*].Sections` → `riser.sections[]`; `EndA/EndB` → `riser.end_connections`. Capture unmapped keys into a `generic_fallback: dict` field on each typed model (or document if schemas need a new `extras` field — open question 1).

2. **Implement three private upgrade functions.** Add `_upgrade_to_riser(spec)`, `_upgrade_to_mooring(spec)`, `_upgrade_to_pipeline(spec)` in `spec_upgrader.py`. Each accepts the generic spec dict, extracts the relevant subset, instantiates the target Pydantic model, and returns a new spec dict with the typed section populated and unmapped properties retained under a `generic:` key. Replace the `NotImplementedError` body of `upgrade_spec` with a dispatch on `target_type`.

3. **Round-trip test on three reference specs.** New file `tests/solvers/orcaflex/test_spec_upgrader_v2.py`. For each model_library reference (`a01_catenary_riser`, `c03_turret_moored_fpso`, `m01_pipeline_lateral_buckling`): load original generic spec, call `detect_model_type` (assert correct type detected with confidence >0.9), call `upgrade_spec`, round-trip through the modular generator (`generate_model_yaml` or equivalent), compare output to original generic-generated YAML (use `yaml.safe_load` + dict comparison to ignore key ordering).

4. **Smoke and regression.** Run `uv run pytest tests/solvers/orcaflex/test_spec_upgrader_v2.py -xvs`. Run the full orcaflex test bucket `uv run pytest tests/solvers/orcaflex/ -x` to confirm no regression in existing detection tests.

5. **Document the upgrade workflow.** Add a "v2 upgrade flow" section to `src/digitalmodel/solvers/orcaflex/MODULE_README.md` showing detect → upgrade → regenerate. Note the round-trip invariant as the regression guarantee.

## Acceptance Criteria

- [ ] `upgrade_spec()` no longer raises `NotImplementedError`; dispatches on `DetectedModelType`
- [ ] Three new private functions `_upgrade_to_{riser,mooring,pipeline}` populate the corresponding Pydantic schema and emit a spec with both typed and `generic:` fallback sections
- [ ] Round-trip test passes for all three model_library reference specs (typed-regenerated YAML deep-equals generic-generated YAML)
- [ ] Existing `tests/solvers/orcaflex/` suite remains green (`uv run pytest tests/solvers/orcaflex/ -x` exits 0)
- [ ] MODULE_README.md documents the upgrade workflow and round-trip invariant

## Open questions

1. Do the typed schemas (`riser.py`, `mooring.py`, `pipeline.py`) currently expose an `extras: dict` or equivalent for unmapped generic keys? If not, decide: extend each schema or carry unmapped keys at spec-root level under `generic:`.
2. Confirm the three model_library spec paths actually exist on `main` (no parallel agent has renamed them) before test fixture wiring.
