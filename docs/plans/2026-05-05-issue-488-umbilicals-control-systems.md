# Plan: digitalmodel #488 — Subsea umbilicals + control systems (API 17E, 17F)

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/488
**Status:** plan-review
**Tier:** T3 (two new submodules)
**Priority:** low

## Context

Subsea systems use umbilicals (API 17E) for power/signal/chemicals and subsea control systems (API 17F) for remote valve and sensor operation. Existing `src/digitalmodel/subsea/cross_sections/` already models umbilical cross-sections via Pydantic schemas (fixtures include `power_optical_hybrid_umbilical.yml`, `steel_tube_electro_hydraulic_umbilical.yml`). What's missing: lay-configuration analysis, chemical-flow delivery calc, and an explicit control-system architecture model. This plan extends `cross_sections` rather than duplicating it, and adds a sibling `control_systems` submodule.

## Plan

1. **Extend `subsea/cross_sections/` for umbilical analysis (do not duplicate).** New file `src/digitalmodel/subsea/cross_sections/umbilical_lay_analysis.py`: lay configuration (parallel-to-flowline, bundled, suspended), separation distances, dynamic curvature limits. New `src/digitalmodel/subsea/cross_sections/umbilical_chemical_flow.py`: pressure drop and dosage residence-time calc through individual tubes (Hagen-Poiseuille for laminar, Colebrook for turbulent). Reuse the existing umbilical fixtures.

2. **New `subsea/control_systems/` submodule.** Files: `__init__.py`, `architecture.py` (electric, e-hydraulic, multiplexed, all-electric — class hierarchy with `topology()` returning a directed graph of vessel → umbilical → tree-control-pod → valves), `reliability.py` (FMEA: per-component MTBF, ripple effect on connected valves, system-availability calc).

3. **Multi-well example fixture.** New `src/digitalmodel/subsea/control_systems/fixtures/three_well_subsea_field.yml` describing 3 trees × 1 manifold × 1 master control station, multiplexed e-hydraulic. Used by both the architecture and reliability tests.

4. **Tests.** `tests/subsea/cross_sections/test_umbilical_lay_analysis.py`, `test_umbilical_chemical_flow.py`. `tests/subsea/control_systems/test_architecture.py`, `test_reliability.py`. Each at least one happy-path + one boundary case. Smoke: `uv run pytest tests/subsea/cross_sections/test_umbilical*.py tests/subsea/control_systems/ -x`.

5. **Documentation.** Add a section to `src/digitalmodel/subsea/cross_sections/README.md` (or create) summarizing the umbilical workflow. New short README at `src/digitalmodel/subsea/control_systems/README.md` with a one-page architecture diagram (mermaid) and pointer to the multi-well fixture.

## Acceptance Criteria

- [ ] `umbilical_lay_analysis.py` and `umbilical_chemical_flow.py` exist as siblings of `cross_sections/schema.py`; both consume the existing umbilical fixtures
- [ ] `subsea/control_systems/` submodule has architecture + reliability modules plus the 3-well fixture
- [ ] Hagen-Poiseuille pressure drop matches analytic value for one fixture tube within 1%
- [ ] FMEA `system_availability()` returns the analytic 2-of-3 result on a hand-checked redundancy case within 0.1%
- [ ] All tests pass; API 17E and 17F citations emitted in module docstrings

## Open questions

1. Reuse of `subsea/cross_sections/schema.py` for umbilicals: is there an existing test fixture for both an electrical and a chemical-tube umbilical that already passes schema validation? If yes, build on those; if no, escalate as a blocker.
2. Should the FMEA solver be Markov-chain based or simple series/parallel availability? Default: series/parallel v1, Markov as follow-up.
