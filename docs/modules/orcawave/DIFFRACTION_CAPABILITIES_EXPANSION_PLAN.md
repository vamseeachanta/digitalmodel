# Diffraction Capabilities Expansion Plan (AQWA + OrcaWave)

**Date:** 2026-01-03
**Status:** Phase 2 Complete - Output Standardization

## Overview

This plan expands diffraction analysis capabilities for AQWA and OrcaWave in a format aligned with the OrcaFlex agent implementation summary. The goal is to standardize workflows, add reliable automation, and expose repeatable examples that integrate into OrcaFlex workflows.

## Current Baseline

### OrcaWave
- Orchestrator and templates: `src/digitalmodel/modules/orcawave/diffraction/`
- Batch execution: `src/digitalmodel/modules/orcawave/diffraction/scripts/run_diffraction_analysis.bat`
- Specifications and sample configs: `specs/modules/orcawave/diffraction-analysis/`
- Examples: `docs/modules/orcawave/examples/` and `docs/modules/orcawave/L01_aqwa_benchmark/`

### AQWA
- Analysis utilities, RAO workflows, and parsers: `src/digitalmodel/modules/aqwa/`
- EF server integration: `src/digitalmodel/modules/aqwa/aqwa_analysis_ef_server.py`
- Example configs and scripts: `docs/modules/aqwa/scripts/wlng_diffraction_analysis/`
- Example runs and references: `docs/modules/aqwa/examples/`

## Target Capabilities

### Shared Goals
- Single entry points for diffraction analysis (AQWA and OrcaWave).
- Repeatable configuration formats (YAML-based) with clear templates.
- Automated results packaging for OrcaFlex import.
- QA checks: geometry validation, reciprocity, energy balance, and frequency coverage.

### OrcaWave Enhancements
- Config-driven batch runs (multi-vessel, multi-condition).
- Integrated result conversion to OrcaFlex vessel type templates.
- Standardized geometry validation report format.
- Automated report artifacts (CSV + XLSX + summary report).

### AQWA Enhancements
- One routing entry for RAO, damping, and external-force workflows.
- Standardized RAO extraction and damping computation outputs.
- Workbench/DAT run helpers and consistent log extraction.
- Results packaging to OrcaFlex-ready structures.

## Expansion Phases

### Phase 1: Workflow Hardening (Short Term) ✅ COMPLETE
- ✅ Fix execution blockers and path mismatches.
- ✅ Ensure AQWA analysis routing covers RAO, damping, and EF server flows.
- ✅ Normalize config locations for example runs.
- ✅ Add minimal CLI examples in docs for both tools.

**Deliverables**: 3 CLI tools, 1,220+ lines documentation
**Completion**: 2026-01-03
**Report**: `docs/modules/orcawave/PHASE_1_COMPLETION.md`

### Phase 2: Output Standardization (Mid Term) ✅ COMPLETE
- ✅ Define unified output schemas (RAO tables, added mass, damping).
- ✅ Implement converters to OrcaFlex-ready YAML/CSV outputs.
- ✅ Add validation scripts for results completeness and ranges.

**Deliverables**: 2,030+ lines code, 6 export formats, comprehensive validation
**Completion**: 2026-01-03
**Report**: `docs/modules/diffraction/PHASE_2_COMPLETION.md`

### Phase 3: Automation + QA (Mid Term)
- Batch execution support for multiple headings/drafts.
- Add geometry quality gates (watertight, normals, panel limits).
- Create automated comparison between AQWA and OrcaWave benchmarks.

### Phase 4: Templates + Examples (Long Term)
- Publish end-to-end example projects for both tools.
- Add reusable vessel templates and configuration generators.
- Provide starter packs for new diffraction studies.

## Working Examples and Assets

- OrcaWave Sea Cypress go-by: `specs/modules/orcawave/diffraction-analysis/`
- AQWA ship RAO examples: `docs/modules/aqwa/examples/001_ship_raos.md`
- AQWA restart example: `docs/modules/aqwa/examples/102_restart/`
- OrcaWave AQWA benchmark assets: `docs/modules/orcawave/L01_aqwa_benchmark/`

## Dependencies and Risks

- AQWA and OrcaWave licenses and Windows execution environment.
- Geometry quality and mesh conversion tooling.
- Consistent units and coordinate frames across tools.

## Validation Strategy

- Geometry validation reports saved per run.
- Frequency coverage and heading completeness checks.
- Cross-tool benchmark comparisons where data is available.
- OrcaFlex import checks using generated vessel type files.

## Next Actions

1. Align AQWA and OrcaWave configuration templates with existing examples.
2. Document end-to-end run commands for both tools in their module docs.
3. Add automated result checks and a small benchmark suite.
