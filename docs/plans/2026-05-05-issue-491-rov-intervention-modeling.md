# Plan: digitalmodel #491 — ROV intervention modeling (API 17H)

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/491
**Status:** plan-review
**Tier:** T3 (new submodule)
**Priority:** low

## Context

Subsea production systems require ROVs (API 17H) for inspection, maintenance, and emergency intervention. Field-development planning needs to verify that subsea equipment is ROV-accessible, that tooling interfaces exist, and that intervention is feasible under site-specific sea states. No ROV capability code exists in digitalmodel today.

Adjacent code that we'll integrate with: `src/digitalmodel/hydrodynamics/` (sea state to operability conversion) and the future tree/manifold modules from #484 / #485 (interface surfaces this module checks).

## Plan

1. **Create submodule skeleton.** New `src/digitalmodel/subsea/rov/` with `__init__.py`, `interface_checks.py`, `tool_library.py`, `seastate_limits.py`, `intervention_sequence.py`, plus a fixture `fixtures/work_class_rov_specs.yml` (representative work-class ROV: 2500 m depth rating, 6 manipulator dof, 5 t bollard pull at LARS).

2. **API 17H interface checks (`interface_checks.py`).** Pydantic model `InterfacePoint` (location, hot-stab type, valve handle type, ISO 13628-8 class). `check_interfaces(component, rov_capabilities)` returns `[(interface_id, pass|fail, reason)]`. Capture the canonical 17H interface classes (Class 1-4) as enum values.

3. **Tool library (`tool_library.py`).** Static catalog of ROV tools (hot-stab connectors, torque tools, capstan pulls, valve operators) keyed by interface class. `compatible_tools(interface_point) -> list[Tool]`. Source: API 17H Annex.

4. **LARS seastate limits (`seastate_limits.py`).** `lars_operability(rov, vessel, hs_series, tp_series)` returns time fraction within launch-recovery limits. Reuse `hydrodynamics/` sea-state inputs. Include heave compensation derating (active vs passive vs none) as a parameter.

5. **Intervention sequence and tests (`intervention_sequence.py`).** Plan a multi-task sequence: `(task, duration_min, weather_window_required_hr)` → `dispatch_plan(tasks, hs_series, lookahead_days)` returns earliest feasible start using a rolling-window operability scan. Tests in `tests/subsea/rov/` covering interface match/mismatch, tool lookup, LARS Hs threshold, and sequence dispatch on a synthetic Hs series. Smoke: `uv run pytest tests/subsea/rov/ -x`.

## Acceptance Criteria

- [ ] `src/digitalmodel/subsea/rov/` exists with 4 core modules + fixtures
- [ ] `check_interfaces()` returns explicit pass/fail + reason on a fixture covering all 4 API 17H classes
- [ ] `compatible_tools()` returns deterministic results for each of the 4 interface classes
- [ ] `lars_operability()` returns a fraction in [0, 1] and matches a hand-checked windowed count within 1%
- [ ] `dispatch_plan()` finds the earliest weather window in a synthetic series with at least one feasible start and returns `None` when none exists
- [ ] All tests pass; citation emitted for API 17H per the citation contract

## Open questions

1. Does the user have access to API 17H Annex content to seed the canonical interface-class definitions, or should we source them from a public reference (e.g., NORSOK U-102) and flag the divergence?
2. Should the tool catalog be data-only (YAML) or class-based? Default: YAML to keep it open to non-engineer edits.
