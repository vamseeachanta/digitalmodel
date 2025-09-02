# Docs Reorganization Specification - Prompt Documentation

## Original User Request

**Request**: Create a comprehensive specification for reorganizing the docs folder. The spec needs to:

1. Analyze the current docs structure challenges:
   - docs/ has two main folders: domains/ (61 subcategories) and modules/ (40+ subcategories)
   - Total of 100+ subcategories making navigation difficult
   - Many empty folders (0 documents)
   - Duplicate naming patterns (ship-design vs ship_design)
   - Hidden folders with underscores (_tools, _config, _assets, etc.)
   - Mixed technical modules (software tools) and engineering domains

2. Create the spec in: specs/modules/documentation/docs-reorganization/

3. The spec.md should include:
   - Executive summary of current problems
   - Proposed new structure that's simpler
   - Clear entry points for humans and AI
   - Migration strategy
   - Benefits and impact analysis

4. The tasks.md should include:
   - Concrete tasks for reorganization
   - Time estimates
   - Dependencies

5. The prompt.md should document:
   - The user's original request
   - Analysis performed
   - Decisions made

## Analysis Performed

### Current State Assessment

**Directory Statistics:**
- `docs/domains/`: 109 total directories with 102 markdown files
- `docs/modules/`: 419 total directories with 195 markdown files
- **Total**: 528+ directories across both sections
- **Issues Identified:**
  - Massive directory sprawl (528+ dirs for ~300 MD files)
  - Complex nested structures making navigation difficult
  - Hidden utility folders mixed with content (_tools, _config, _assets, etc.)
  - Inconsistent naming (ship-design vs ship_design)
  - Legacy organization patterns not aligned with repository standards

**Key Problems:**
1. **Scale Overwhelm**: 100+ subcategories create choice paralysis
2. **Deep Nesting**: Some paths are 6+ levels deep
3. **Mixed Purpose**: Content docs mixed with configuration/tools
4. **Inconsistent Patterns**: Multiple naming conventions
5. **Discovery Issues**: Hard for humans and AI to find relevant content
6. **Maintenance Burden**: Too many empty/sparse directories to maintain

### Repository Pattern Analysis

**Current Repository Standard**: All organized content follows `<group>/modules/<module>/` pattern:
- ✅ `specs/modules/orcaflex/` (working examples)
- ✅ `src/modules/hydrodynamics/`
- ❌ `docs/domains/cathodic-protection/` (violates pattern)
- ❌ `docs/modules/ansys/` (correct pattern but overly complex)

## Key Decisions Made

### 1. Alignment with Repository Pattern
- **Decision**: Enforce `docs/modules/<module>/` pattern consistently
- **Rationale**: Aligns with existing repository organization standards

### 2. Simplification Strategy
- **Decision**: Consolidate 100+ categories into ~15-20 core modules
- **Rationale**: Human cognitive limits (7±2 items) and easier maintenance

### 3. Content vs. Infrastructure Separation
- **Decision**: Move utility folders (_tools, _config) out of main content areas
- **Rationale**: Content discovery should not be cluttered with infrastructure

### 4. Entry Point Optimization
- **Decision**: Create clear "gateway" documents at each level
- **Rationale**: Both humans and AI agents need clear navigation paths

### 5. Migration Approach
- **Decision**: Phased migration with validation at each step
- **Rationale**: Minimize disruption while ensuring no content loss

## Implementation Priorities

1. **Critical Path**: Establish new structure foundation
2. **High Impact**: Migrate most-used content first (OrcaFlex, AQWA, Ship Design)  
3. **Low Risk**: Handle legacy/archive content last
4. **Validation**: Comprehensive verification at each phase

## Success Metrics

- **Navigation Time**: &lt;30 seconds to find relevant content
- **Directory Count**: Reduce from 528+ to &lt;100 directories
- **Entry Points**: 3 or fewer clicks to reach any content
- **Maintenance**: Single person can maintain structure in &lt;2 hours/month

## Reusable Prompt for Similar Tasks

```
Analyze the current [content area] structure and create a comprehensive reorganization specification that:

1. **Audit Current State**: Count directories, files, and identify structural issues
2. **Apply Repository Pattern**: Ensure alignment with established `<group>/modules/<module>/` organization
3. **Optimize for Discovery**: Design for both human navigation and AI agent indexing  
4. **Plan Migration**: Create concrete, phased migration strategy with validation
5. **Measure Success**: Define clear metrics for improved organization

Focus on reducing cognitive load while maintaining all existing content value.
```

This prompt pattern can be reused for reorganizing any complex content hierarchy in technical repositories.