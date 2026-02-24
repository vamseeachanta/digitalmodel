---
# =============================================================================
# PLAN METADATA
# =============================================================================

title: "Modular OrcaFlex Pipeline Installation Input with Parametric Campaign Support"
description: "Refactor OrcaFlex pipeline installation input into modular include-file sections with parametric campaign matrix support for both S-lay and floating tow methods."
version: "1.0"
module: "orcaflex/modular_generator"

session:
  id: "wrk-032-plan-20260203"
  agent: "claude-opus-4.5"
  started: "2026-02-03"
  last_active: "2026-02-03"
  conversation_id: ""

review:
  required_iterations: 3
  current_iteration: 2
  status: "in_progress"
  reviewers:
    openai_codex:
      status: "approved"
      iteration: 3
      last_reviewed: "2026-02-03"
      feedback: "APPROVED — 1 Low (roller contract v_angle), 2 Very Low cosmetic"
    google_gemini:
      status: "approved"
      iteration: 3
      last_reviewed: "2026-02-03"
      feedback: "APPROVED — 1 Low informational (roller contract v_angle, not blocking)"
  approval_gate:
    min_iterations_met: true
    codex_approved: true
    gemini_approved: true
    ready_for_next_step: true

status: "implemented"
progress: 100
phase: 1
blocked_by: []

created: "2026-02-03"
updated: "2026-02-03"
target_completion: ""

author: "claude-opus-4.5"
reviewers: []
assignees: []

technical:
  language: "python"
  python_version: ">=3.11"
  dependencies:
    - pydantic
    - pyyaml
    - itertools (stdlib)
  test_coverage: 80
  platforms: ["windows"]

priority: "medium"
complexity: "complex"
risk: "medium"
tags: ["orcaflex", "pipeline", "installation", "modular", "campaign", "parametric"]

links:
  spec: ".claude/work-queue/pending/WRK-032.md"
  branch: ""
  pr: ""
  issues: ["WRK-032", "WRK-033 (superseded)"]
  docs:
    - "specs/modules/orcaflex/pipeline-installation-modular/research.md"
    - "specs/modules/orcaflex/pipeline-installation-modular/data-model.md"
    - "specs/modules/orcaflex/pipeline-installation-modular/tasks.md"
    - "specs/modules/orcaflex/pipeline-installation-modular/quickstart.md"

history:
  - date: "2026-02-03"
    action: "created"
    by: "claude-opus-4.5"
    notes: "Initial plan from WRK-032 + WRK-033 merge. SME Q&A completed."
---

# Modular OrcaFlex Pipeline Installation Input with Parametric Campaign Support

> **Module**: orcaflex/modular_generator | **Status**: draft | **Priority**: medium
> **Created**: 2026-02-03 | **Work Item**: WRK-032 (supersedes WRK-033)

## Executive Summary

This plan extends the OrcaFlex modular model generator to support parametric pipeline installation campaigns. The current system generates a single OrcaFlex model from a single `ProjectInputSpec` YAML. The extension adds:

1. **Parametric roller arrangements** — multi-station rollers with configurable V-angle, diameter, friction, and spacing (replacing the minimal single-position roller model)
2. **Campaign matrix** — define a base configuration plus parameter variations (water depth, route length, environment, soil, tension) that expand into a full cartesian product of run configurations
3. **Section composition engine** — variable substitution in include-file templates, enabling reusable section libraries
4. **Both S-lay and floating tow** — campaign parameters relevant to each method (tension for S-lay, tugs for floating)

The implementation builds on the existing builder registry, Pydantic schema layer, and include-file convention. Legacy `OrcInstallation` functionality is absorbed.

---

## Technical Context

| Aspect | Details |
|--------|---------|
| Language | Python 3.11+ |
| Package Manager | uv |
| Schema Validation | Pydantic v2 |
| Test Framework | pytest |
| Test Coverage Target | 80% |
| Platform | Windows |

### Dependencies

- [x] Pydantic (existing)
- [x] PyYAML (existing)
- [x] itertools (stdlib — for cartesian product)
- [x] copy (stdlib — for deep copy)
- [x] pathlib (stdlib)

### Prerequisites

- [x] Existing `modular_generator` schema and builders functional
- [x] Existing include-file convention defined (CALM buoy spec)
- [x] Existing floating and S-lay spec examples available
- [ ] WRK-032 work item updated with merged scope (done 2026-02-03)

---

## Phases

### Phase 0: Generator Prerequisite (T00)

**Objective**: Add `ModularModelGenerator.from_spec()` classmethod to support in-memory spec objects.

**Tasks**:
- [ ] T00: Add `from_spec()` classmethod to `ModularModelGenerator`

**Deliverables**:
- Refactored `__init__.py` separating YAML loading from validation
- `from_spec()` classmethod accepting `ProjectInputSpec` directly

**Exit Criteria**:
- [ ] `from_spec(spec)` generates identical output to file-based path
- [ ] Existing CLI and file-based usage unaffected

---

### Phase 1: Schema Extension (T01-T03)

**Objective**: Add `RollerArrangement` model and extend `Equipment` with backward compatibility.

**Tasks**:
- [ ] T01: Add `RollerType` enum + `RollerStation` model with validation
- [ ] T02: Add `RollerArrangement` model with `uniform()` factory
- [ ] T03: Extend `Equipment` with `roller_arrangement` field + backward compat validator

**Deliverables**:
- Extended `equipment.py` with roller parametrisation
- Extended `_enums.py` with `RollerType`
- Updated `schema/__init__.py` exports
- Test suite for roller models and backward compatibility

**Exit Criteria**:
- [ ] All existing modular generator tests still pass
- [ ] Legacy `rollers` field auto-converts to `RollerArrangement`
- [ ] Both existing spec examples (floating + S-lay) parse without error

---

### Phase 2: Campaign Models + Builder Update (T04-T05, parallelisable)

**Objective**: Add campaign matrix data models and update BuoysBuilder for multi-station rollers.

**Tasks**:
- [ ] T04: Add `EnvironmentVariation`, `SoilVariation`, `CampaignMatrix`, `CampaignSpec` models
- [ ] T05: Update `BuoysBuilder` for `RollerArrangement` (multi-station, parametric geometry)

**Deliverables**:
- New `campaign.py` schema module
- Updated `buoys_builder.py` with `_build_roller_arrangement()`
- Test suites for campaign models and roller generation

**Exit Criteria**:
- [ ] `CampaignMatrix.combinations()` produces correct cartesian product
- [ ] Multi-station roller arrangement generates correct 6D buoys
- [ ] Support geometry calculated from v_angle and diameter

**Note**: T04 and T05 are independent — can be implemented in parallel.

---

### Phase 3: Variable Resolution + Generation Engine (T06-T07)

**Objective**: Build the variable resolver, extend the generator with section overrides, and implement the campaign generator.

**Tasks**:
- [ ] T06: Implement `VariableResolver` + `ModularModelGenerator.generate_with_overrides()`
- [ ] T07: Implement `CampaignGenerator` with unified CLI subcommand

**Deliverables**:
- New `sections.py` module (VariableResolver only)
- Extended `ModularModelGenerator` with `generate_with_overrides()`
- Campaign generation in `campaign.py`
- Unified `campaign` subcommand in `cli.py`
- Test suites for variable resolution, section overrides, and generation

**Exit Criteria**:
- [ ] Variable substitution resolves all `${...}` tokens with format specifier support
- [ ] Post-substitution YAML validation catches broken structure
- [ ] Master YAML generated with correct `includefile` entries
- [ ] Campaign generates correct directory structure with one run per combination
- [ ] `--preview` shows matrix without file generation
- [ ] `--force` and `--resume` flags work correctly
- [ ] Streaming generation (memory-efficient for large matrices)

---

### Phase 4: Legacy Absorption + Integration (T08-T09)

**Objective**: Absorb legacy `OrcInstallation` functionality and verify end-to-end.

**Tasks**:
- [ ] T08: Add `from_legacy_config()` on `CampaignSpec`, deprecate `OrcInstallation`
- [ ] T09: Integration tests + example campaign files

**Deliverables**:
- Legacy compatibility layer
- Integration test suite
- Example campaign YAML files (floating + S-lay)
- Deprecation warning on `OrcInstallation`

**Exit Criteria**:
- [ ] Legacy elevation-based generation reproduced by campaign mode
- [ ] 80% test coverage achieved for new code
- [ ] All acceptance criteria from WRK-032 met

---

## Risk Assessment

| Risk | Likelihood | Impact | Mitigation |
|------|------------|--------|------------|
| Backward compat breakage | Medium | High | Extensive existing test suite; auto-conversion validator; run all tests at each step |
| Large campaign matrix overwhelms disk | Low | Medium | `preview()` shows count first; add optional `max_runs` limit on `CampaignGenerator` |
| Roller geometry calculation errors | Medium | Medium | Unit tests with known geometries; compare with hardcoded positions in existing builder |
| Legacy OrcInstallation edge cases | Medium | Low | Side-by-side output comparison tests; keep legacy module available (deprecated, not deleted) |
| Variable substitution in YAML breaks structure | Low | High | Resolver operates on string content post-template-load; validate parsed YAML after substitution |

---

## Cross-Review Process (MANDATORY)

> **REQUIREMENT**: Plan must complete minimum **3 review iterations** with both OpenAI Codex and Google Gemini before proceeding to implementation.

### Review Status

| Gate | Requirement | Status |
|------|-------------|--------|
| Minimum Iterations | >= 3 iterations completed | **Met** |
| OpenAI Codex | Approved | **Approved (Iter 3)** |
| Google Gemini | Approved | **Approved (Iter 3)** |
| **Ready for Next Step** | All gates passed | **PASSED** |

### Review Iteration Log

| Iteration | Date | Reviewer | Status | Key Feedback | Changes Made |
|-----------|------|----------|--------|--------------|--------------|
| 1 | 2026-02-03 | OpenAI Codex | Changes Requested | 17 suggestions: override mapping, roller geometry formula, SectionRegistry removal, ParameterRange removal, model_copy, legacy scope | All critical items addressed in Rev 2 |
| 1 | 2026-02-03 | Google Gemini | Changes Requested | 18 suggestions: ModularModelGenerator.from_spec(), eliminate SectionComposer, streaming generation, tensions cross-validation, current profile scaling, schema versioning | All critical items addressed in Rev 2 |
| 2 | 2026-02-03 | OpenAI Codex | Approved | 8 minor suggestions (all Low/Very Low) | N/A — approved |
| 2 | 2026-02-03 | Google Gemini | Changes Requested | 2 MEDIUM: stale contracts (section_composition, campaign_generator); 4 LOW: quickstart CLI, plan.md SectionComposer ref | Contracts Rev 2, quickstart Rev 2, plan.md Phase 3 updated |
| 3 | 2026-02-03 | OpenAI Codex | **APPROVED** | 1 Low (roller contract v_angle type), 2 Very Low (cosmetic) | N/A — approved |
| 3 | 2026-02-03 | Google Gemini | **APPROVED** | 1 Low informational (roller contract v_angle type — not blocking) | N/A — approved |

### Approval Checklist

- [x] Iteration 1: Codex reviewed
- [x] Iteration 1: Gemini reviewed
- [x] Iteration 1: Feedback incorporated
- [x] Iteration 2: Codex reviewed (APPROVED)
- [x] Iteration 2: Gemini reviewed (Changes Requested)
- [x] Iteration 2: Feedback incorporated
- [x] Iteration 3: Codex reviewed (APPROVED)
- [x] Iteration 3: Gemini reviewed (APPROVED)
- [x] Iteration 3: Feedback incorporated (1 minor roller contract note — deferred to implementation)
- [x] **FINAL**: Both reviewers approved
- [x] **FINAL**: Ready for implementation

---

## Progress Tracking

### Milestones

| Milestone | Target Date | Status | Notes |
|-----------|-------------|--------|-------|
| Plan Draft Complete | 2026-02-03 | Complete | |
| SME Q&A Complete | 2026-02-03 | Complete | 6 questions answered |
| Review Iteration 1 | 2026-02-03 | Complete | 17 Codex + 18 Gemini suggestions incorporated |
| Review Iteration 2 | 2026-02-03 | Complete | Codex approved; Gemini: 2 MEDIUM + 4 LOW addressed |
| Review Iteration 3 | 2026-02-03 | Complete | Both APPROVED |
| Plan Approved | 2026-02-03 | **Complete** | All gates passed |
| Phase 0 Complete (Generator Prerequisite) | 2026-02-03 | **Complete** | T00 |
| Phase 1 Complete (Schema) | 2026-02-03 | **Complete** | T01-T03 |
| Phase 2 Complete (Campaign + Builder) | 2026-02-03 | **Complete** | T04-T05 |
| Phase 3 Complete (Engine) | 2026-02-03 | **Complete** | T06-T07 |
| Phase 4 Complete (Integration) | 2026-02-03 | **Complete** | T08-T09 |

### Session Log

| Date | Session ID | Agent | Actions | Notes |
|------|------------|-------|---------|-------|
| 2026-02-03 | wrk-032-plan-20260203 | claude-opus-4.5 | Plan created | WRK-032+033 merged, SME Q&A, all artifacts generated |
| 2026-02-03 | wrk-032-impl-20260203 | claude-opus-4.5 | Implementation complete | All 10 tasks (T00-T09) implemented, 209 tests passing |

---

## Artifacts

| Artifact | Path | Phase |
|----------|------|-------|
| Research | `specs/modules/orcaflex/pipeline-installation-modular/research.md` | 0 |
| Data Model | `specs/modules/orcaflex/pipeline-installation-modular/data-model.md` | 1 |
| Contract: CampaignGenerator | `specs/modules/orcaflex/pipeline-installation-modular/contracts/campaign_generator.md` | 1 |
| Contract: RollerArrangement | `specs/modules/orcaflex/pipeline-installation-modular/contracts/roller_arrangement.md` | 1 |
| Contract: SectionComposition | `specs/modules/orcaflex/pipeline-installation-modular/contracts/section_composition.md` | 1 |
| Quickstart Guide | `specs/modules/orcaflex/pipeline-installation-modular/quickstart.md` | 1 |
| Implementation Tasks | `specs/modules/orcaflex/pipeline-installation-modular/tasks.md` | 2 |
| Implementation Plan | `specs/modules/orcaflex/pipeline-installation-modular/plan.md` | Final |

---

## Appendix

### A. Glossary

| Term | Definition |
|------|-----------|
| Campaign | A set of parametric runs with varied water depths, environments, etc. |
| Include-file | OrcaFlex mechanism for composing a model from multiple YAML section files |
| Roller station | A single physical roller support point along the installation route |
| Roller arrangement | A collection of roller stations with shared parametric properties |
| Section | One modular include-file (e.g., `_03_environment.yml`) in the OrcaFlex input |
| Variable substitution | Replacing `${token}` placeholders in templates with concrete values |

### B. References

- Existing modular input spec: `specs/modules/orcaflex/modular-input-file/output/README.md`
- Existing floating spec example: `docs/modules/orcaflex/pipeline/installation/floating/30in_pipeline/spec.yml`
- Existing S-lay spec example: `docs/modules/orcaflex/pipeline/installation/s-lay/SB-SA/spec.yml`
- Work item: `.claude/work-queue/pending/WRK-032.md`
- Superseded work item: `.claude/work-queue/pending/WRK-033.md`

### C. Change History

| Version | Date | Author | Changes |
|---------|------|--------|---------|
| 1.0 | 2026-02-03 | claude-opus-4.5 | Initial plan (WRK-032 + WRK-033 merged) |

---

*Plan generated using workspace-hub plan template v1.0*
