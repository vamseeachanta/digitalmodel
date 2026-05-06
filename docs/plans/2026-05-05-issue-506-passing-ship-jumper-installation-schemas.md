# Plan: digitalmodel #506 — Add PassingShipSpec and JumperInstallationSpec schemas

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/506
**Status:** plan-review
**Tier:** T3

## Context

The spec audit identified two valid domain specs that don't conform to `ProjectInputSpec` (the OrcaFlex modular-generator schema) because they describe non-OrcaFlex workflows:

- `docs/domains/orcaflex/passing_ship/sample/spec.yml` — top-level `moored_vessel`, `passing_vessel`, scenario keys (consumed by `hydrodynamics/passing_ship/`).
- `docs/domains/orcaflex/subsea/jumper/installation/ballymore_mf_plet/spec.yml` — top-level `metadata`, `environment`, `pipe`, `jumper` keys (consumed by `marine_ops/installation/jumper_lift.py`).

Both currently lack Pydantic schema validation, so `scripts/audit_all_specs.py` cannot validate them and silently skips or warns. We need parallel Pydantic schemas that the audit script can dispatch to based on a `metadata.spec_type` discriminator.

## Plan

1. **Define `metadata.spec_type` discriminator.** Edit both target spec files to carry `metadata.spec_type: passing_ship` and `metadata.spec_type: jumper_installation` respectively (verify the ballymore spec already has `metadata.structure: jumper` — extend it without breaking `jumper_lift.py` consumption). This becomes the dispatch key.

2. **Author `PassingShipSpec`.** New file `src/digitalmodel/hydrodynamics/passing_ship/schema.py`. Pydantic `BaseModel` mirroring the YAML structure: `name`, `description`, `moored_vessel: VesselSpec` (hull + loading_condition + mooring_lines), `passing_vessel: VesselSpec`, `scenario: ScenarioSpec` (water_depth, separation, speed, heading). Use field aliases for any deviations. Export from `src/digitalmodel/hydrodynamics/passing_ship/__init__.py`.

3. **Author `JumperInstallationSpec`.** New file `src/digitalmodel/marine_ops/installation/jumper_schema.py` (sibling of `jumper_lift.py` — keeps the consumer co-located). Pydantic models for `metadata`, `environment.water/seabed/metocean`, `pipe`, `jumper.geometry/config/segments`, `cranes`. Co-locate with `KNOWN_JUMPER_CONFIGS` so callers don't import across packages.

4. **Wire schemas into audit script.** Edit `scripts/audit_all_specs.py` to read `metadata.spec_type` (default `project_input`) and dispatch to `ProjectInputSpec`, `PassingShipSpec`, or `JumperInstallationSpec`. Surface validation failures with the same `file:line:reason` format already used for `ProjectInputSpec`.

5. **Tests for both schemas.** New `tests/hydrodynamics/passing_ship/test_schema.py` and `tests/marine_ops/installation/test_jumper_schema.py`. Each: load the canonical spec → assert validation passes; mutate one required field → assert `ValidationError`. Smoke: `uv run pytest tests/hydrodynamics/passing_ship/test_schema.py tests/marine_ops/installation/test_jumper_schema.py -xvs` and `uv run python scripts/audit_all_specs.py` (must exit 0 with both new specs validated).

## Acceptance Criteria

- [ ] `PassingShipSpec` Pydantic model exists at `src/digitalmodel/hydrodynamics/passing_ship/schema.py` and is importable from the package
- [ ] `JumperInstallationSpec` Pydantic model exists at `src/digitalmodel/marine_ops/installation/jumper_schema.py` and is importable
- [ ] Both target spec.yml files carry `metadata.spec_type` and validate against their schemas
- [ ] `scripts/audit_all_specs.py` dispatches by `spec_type` and reports both new specs as validated (no `ProjectInputSpec` failure)
- [ ] Tests cover happy-path load and one negative case per schema; both pass

## Open questions

1. Is there a third unaccounted-for spec shape in `docs/domains/orcaflex/subsea/jumper/installation/ballymore_plet_plem/spec.yml`? Inspect — if structurally identical, share the schema; if different, file a follow-up.
